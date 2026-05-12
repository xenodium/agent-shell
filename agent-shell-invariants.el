;;; agent-shell-invariants.el --- Runtime buffer invariants and event tracing -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Alvaro Ramirez and contributors

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Runtime invariant checking and event tracing for agent-shell buffers.
;;
;; When enabled, every buffer mutation point logs a structured event to
;; a per-buffer ring buffer and then runs a set of cheap invariant
;; checks.  When an invariant fails, the system captures a debug
;; bundle (event log + buffer snapshot + ACP traffic) and presents it
;; in a pop-up buffer with a recommended agent prompt.
;;
;; Enable globally:
;;
;;   (setq agent-shell-invariants-enabled t)
;;
;; Or toggle in a running shell:
;;
;;   M-x agent-shell-toggle-invariants

;;; Code:

(require 'ring)
(require 'map)
(require 'cl-lib)
(require 'text-property-search)

(defvar agent-shell-ui--content-store)

;;; --- Configuration --------------------------------------------------------

(defvar agent-shell-invariants-enabled nil
  "When non-nil, check buffer invariants after every mutation.")

(defvar agent-shell-invariants-ring-size 5000
  "Number of events to retain in the per-buffer ring.
Each event is a small plist; 5000 entries uses roughly 200-400 KB.")

;;; --- Per-buffer state -----------------------------------------------------

(defvar-local agent-shell-invariants--ring nil
  "Ring buffer holding recent mutation events for this shell.")

(defvar-local agent-shell-invariants--seq 0
  "Monotonic event counter for this shell buffer.")

(defvar-local agent-shell-invariants--violation-reported nil
  "Non-nil when a violation has already been reported for this buffer.
Reset by `agent-shell-invariants--clear-violation-flag'.")

;;; --- Event ring -----------------------------------------------------------

(defun agent-shell-invariants--ensure-ring ()
  "Create the event ring for the current buffer if needed."
  (unless agent-shell-invariants--ring
    (setq agent-shell-invariants--ring
          (make-ring agent-shell-invariants-ring-size))))

(defun agent-shell-invariants--record (op &rest props)
  "Record a mutation event with operation type OP and PROPS.
PROPS is a plist of operation-specific data."
  (when agent-shell-invariants-enabled
    (agent-shell-invariants--ensure-ring)
    (let ((seq (cl-incf agent-shell-invariants--seq)))
      (ring-insert agent-shell-invariants--ring
                   (append (list :seq seq
                                 :time (float-time)
                                 :op op)
                           props)))))

(defun agent-shell-invariants--events ()
  "Return events from the ring as a list, oldest first."
  (when agent-shell-invariants--ring
    (let ((elts (ring-elements agent-shell-invariants--ring)))
      ;; ring-elements returns newest-first
      (nreverse elts))))

;;; --- Invariant checks -----------------------------------------------------
;;
;; Each check returns nil on success or a string describing the
;; violation.  Checks must be fast (marker comparisons, text property
;; lookups, no full-buffer scans).

(defun agent-shell-invariants--check-process-mark ()
  "Verify the process mark is at or after all fragment content.
The process mark should sit at the prompt line, which comes after
every fragment."
  (when-let ((proc (get-buffer-process (current-buffer)))
             (pmark (process-mark proc)))
    (let ((last-fragment-end nil))
      (save-excursion
        (goto-char (point-max))
        (when-let ((match (text-property-search-backward
                           'agent-shell-ui-state nil
                           (lambda (_ v) v) t)))
          (setq last-fragment-end (prop-match-end match))))
      (when (and last-fragment-end
                 (< (marker-position pmark) last-fragment-end))
        (format "process-mark (%d) is before last fragment end (%d)"
                (marker-position pmark) last-fragment-end)))))

(defun agent-shell-invariants--check-ui-state-contiguity ()
  "Verify that agent-shell-ui-state properties are contiguous per fragment.
Gaps in the text property within a single fragment indicate
corruption from insertion or deletion gone wrong."
  (let ((violations nil)
        (prev-end nil)
        (prev-qid nil))
    (save-excursion
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (let* ((state (get-text-property pos 'agent-shell-ui-state))
                 (qid (when state (map-elt state :qualified-id)))
                 (next (or (next-single-property-change
                            pos 'agent-shell-ui-state)
                           (point-max))))
            (when qid
              (when (and prev-qid (equal prev-qid qid)
                         prev-end (< prev-end pos))
                (push (format "fragment %s has gap: %d to %d"
                              qid prev-end pos)
                      violations))
              (setq prev-qid qid
                    prev-end next))
            ;; When qid is nil (no state at this position), just
            ;; advance.  The next span with a matching qid will
            ;; detect the gap.
            (setq pos next)))))
    (when violations
      (string-join violations "\n"))))

(defun agent-shell-invariants--body-length-in-block (block-start block-end)
  "Return length of the body section between BLOCK-START and BLOCK-END.
Finds the body by scanning for the `agent-shell-ui-section' text
property with value `body'.  Returns nil if no body section exists."
  (let ((pos block-start)
        (body-len nil))
    (while (< pos block-end)
      (when (eq (get-text-property pos 'agent-shell-ui-section) 'body)
        (let ((end (next-single-property-change
                    pos 'agent-shell-ui-section nil block-end)))
          (setq body-len (+ (or body-len 0) (- end pos)))
          (setq pos end)))
      (setq pos (or (next-single-property-change
                     pos 'agent-shell-ui-section nil block-end)
                    block-end)))
    body-len))

(defun agent-shell-invariants--check-content-store-consistency ()
  "Verify content-store body length is plausible vs buffer body length.
Large discrepancies indicate the content-store and buffer diverged.

Cost: O(N · buffer-size) per call — `maphash' over every entry in
the content store, and each entry walks the buffer from
`point-min' looking for its qualified-id property.  Acceptable
for the live-validate workflow this is gated behind, but keep
`agent-shell-invariants-enabled' off in normal sessions."
  (when agent-shell-ui--content-store
    (let ((violations nil))
      (maphash
       (lambda (key stored-body)
         (when (and (string-suffix-p "-body" key)
                    stored-body)
           (let* ((qid (string-remove-suffix "-body" key))
                  (buf-body-len
                   (save-excursion
                     (goto-char (point-min))
                     (let ((found nil))
                       (while (and (not found)
                                   (setq found
                                         (text-property-search-forward
                                          'agent-shell-ui-state nil
                                          (lambda (_ v)
                                            (equal (map-elt v :qualified-id) qid))
                                          t))))
                       (when found
                         (agent-shell-invariants--body-length-in-block
                          (prop-match-beginning found)
                          (prop-match-end found)))))))
             ;; Only flag if buffer body is dramatically shorter than
             ;; stored (indicating lost content, not just formatting).
             (when (and buf-body-len
                        (< 0 (length stored-body))
                        (< buf-body-len (/ (length stored-body) 2)))
               (push (format "fragment %s: buffer body %d chars, store %d chars"
                             qid buf-body-len (length stored-body))
                     violations)))))
       agent-shell-ui--content-store)
      (when violations
        (string-join violations "\n")))))

(defvar agent-shell-invariants--all-checks
  '(agent-shell-invariants--check-process-mark
    agent-shell-invariants--check-ui-state-contiguity
    agent-shell-invariants--check-content-store-consistency)
  "List of invariant check functions to run after each mutation.")

;;; --- Check runner ---------------------------------------------------------

(defun agent-shell-invariants--run-checks (trigger-op)
  "Run all invariant checks.  TRIGGER-OP is the operation that triggered them.
On failure, present the debug bundle.  Only reports the first violation
per buffer to avoid pop-up storms; reset with
`agent-shell-invariants--clear-violation-flag'."
  (when (and agent-shell-invariants-enabled
             (not agent-shell-invariants--violation-reported))
    (let ((violations nil))
      (dolist (check agent-shell-invariants--all-checks)
        (condition-case err
            (when-let ((v (funcall check)))
              (push (cons check v) violations))
          (error
           (push (cons check (format "check error: %s" (error-message-string err)))
                 violations))))
      (when violations
        (setq agent-shell-invariants--violation-reported t)
        (agent-shell-invariants--on-violation trigger-op violations)))))

(defun agent-shell-invariants--clear-violation-flag ()
  "Clear the violation-reported flag so future violations are reported again."
  (setq agent-shell-invariants--violation-reported nil))

;;; --- Violation handler ----------------------------------------------------

(defun agent-shell-invariants--snapshot-buffer ()
  "Capture the current buffer state as a string with properties."
  (buffer-substring (point-min) (point-max)))

(defun agent-shell-invariants--snapshot-markers ()
  "Capture key marker positions."
  (let ((result nil))
    (when-let ((proc (get-buffer-process (current-buffer))))
      (push (cons :process-mark (marker-position (process-mark proc))) result))
    (push (cons :point-max (point-max)) result)
    (push (cons :point-min (point-min)) result)
    result))

(defun agent-shell-invariants--format-events ()
  "Format the event ring as a readable string."
  (let ((events (agent-shell-invariants--events)))
    (if (not events)
        "(no events recorded)"
      (mapconcat
       (lambda (ev)
         (format "[%d] %s %s"
                 (plist-get ev :seq)
                 (plist-get ev :op)
                 (let ((rest (copy-sequence ev)))
                   ;; Remove standard keys for compact display
                   (cl-remf rest :seq)
                   (cl-remf rest :time)
                   (cl-remf rest :op)
                   (if rest
                       (prin1-to-string rest)
                     ""))))
       events "\n"))))

(defun agent-shell-invariants--on-violation (trigger-op violations)
  "Handle invariant violations from TRIGGER-OP.
VIOLATIONS is an alist of (check-fn . description)."
  (let* ((shell-buffer (current-buffer))
         (buffer-name (buffer-name shell-buffer))
         (markers (agent-shell-invariants--snapshot-markers))
         (buf-snapshot (agent-shell-invariants--snapshot-buffer))
         (events-str (agent-shell-invariants--format-events))
         (violation-str (mapconcat
                         (lambda (v)
                           (format "  %s: %s" (car v) (cdr v)))
                         violations "\n"))
         (bundle-buf (get-buffer-create
                      (format "*agent-shell invariant [%s]*" buffer-name))))
    ;; Build the debug bundle buffer
    (with-current-buffer bundle-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "━━━ AGENT-SHELL INVARIANT VIOLATION ━━━\n\n")
        (insert (format "Buffer: %s\n" buffer-name))
        (insert (format "Trigger: %s\n" trigger-op))
        (insert (format "Time: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
        (insert "── Violations ──\n\n")
        (insert violation-str)
        (insert "\n\n── Markers ──\n\n")
        (insert (format "%S\n" markers))
        (let* ((window 2000)
               (total (length buf-snapshot)))
          (cond
           ((<= total window)
            (insert (format "\n── Buffer Snapshot (%d chars) ──\n\n" total))
            (insert buf-snapshot))
           (t
            (insert (format "\n── Buffer Snapshot Head (first %d / %d chars) ──\n\n"
                            window total))
            (insert (substring buf-snapshot 0 window))
            (insert (format "\n\n── Buffer Snapshot Tail (last %d / %d chars) ──\n\n"
                            window total))
            (insert (substring buf-snapshot (- total window))))))
        (insert "\n\n── Event Log (last ")
        (insert (format "%d" (length (agent-shell-invariants--events))))
        (insert " events) ──\n\n")
        (insert events-str)
        (insert "\n\n── Recommended Prompt ──\n\n")
        (insert "Copy the full contents of this buffer and paste it as context ")
        (insert "for this prompt:\n\n")
        (let ((prompt-start (point)))
          (insert "An agent-shell buffer invariant was violated during a ")
          (insert (format "`%s` operation.\n\n" trigger-op))
          (insert "The debug bundle above contains:\n")
          (insert "- The specific invariant(s) that failed and why\n")
          (insert "- Marker positions at time of failure\n")
          (insert "- The last N mutation events leading up to the failure\n\n")
          (insert "Please analyze the event sequence to determine:\n")
          (insert "1. Which event(s) caused the violation\n")
          (insert "2. The root cause in the rendering pipeline\n")
          (insert "3. A proposed fix\n\n")
          (insert "The relevant source files are:\n")
          (insert "- agent-shell-ui.el (fragment rendering, insert/append/rebuild)\n")
          (insert "- agent-shell-streaming.el (tool call streaming, marker management)\n")
          (insert "- agent-shell.el (agent-shell--update-fragment, ")
          (insert "agent-shell--with-preserved-process-mark)\n")
          (add-text-properties prompt-start (point)
                               '(face font-lock-doc-face)))
        (insert "\n\n━━━ END ━━━\n")
        (goto-char (point-min))
        (special-mode)))
    ;; Show the bundle
    (display-buffer bundle-buf
                    '((display-buffer-pop-up-window)
                      (window-height . 0.5)))
    (message "agent-shell: invariant violation detected — see %s"
             (buffer-name bundle-buf))))

;;; --- Mutation point hooks --------------------------------------------------
;;
;; Call these from the 5 key mutation sites.  Each records an event
;; and then runs the invariant checks.

(defun agent-shell-invariants-on-update-fragment (op namespace-id block-id &optional append)
  "Record and check after a fragment update.
OP is a string like \"create\", \"append\", or \"rebuild\".
NAMESPACE-ID and BLOCK-ID identify the fragment.
APPEND is non-nil if this was an append operation."
  (when agent-shell-invariants-enabled
    (let ((pmark (when-let ((proc (get-buffer-process (current-buffer))))
                   (marker-position (process-mark proc)))))
      (agent-shell-invariants--record
       'update-fragment
       :detail op
       :fragment-id (format "%s-%s" namespace-id block-id)
       :append append
       :process-mark pmark
       :point-max (point-max)))
    (agent-shell-invariants--run-checks 'update-fragment)))

(defun agent-shell-invariants-on-append-output (tool-call-id marker-pos text-len)
  "Record and check after live tool output append.
TOOL-CALL-ID identifies the tool call.
MARKER-POS is the output marker position.
TEXT-LEN is the length of appended text."
  (when agent-shell-invariants-enabled
    (agent-shell-invariants--record
     'append-output
     :tool-call-id tool-call-id
     :marker-pos marker-pos
     :text-len text-len
     :point-max (point-max))
    (agent-shell-invariants--run-checks 'append-output)))

(defun agent-shell-invariants-on-process-mark-save (saved-pos)
  "Record process-mark save.  SAVED-POS is the position being saved."
  (when agent-shell-invariants-enabled
    (agent-shell-invariants--record
     'pmark-save
     :saved-pos saved-pos
     :point-max (point-max))))

(defun agent-shell-invariants-on-process-mark-restore (saved-pos restored-pos)
  "Record and check after process-mark restore.
SAVED-POS was the target; RESTORED-POS is where it actually ended up."
  (when agent-shell-invariants-enabled
    (agent-shell-invariants--record
     'pmark-restore
     :saved-pos saved-pos
     :restored-pos restored-pos
     :point-max (point-max))
    (agent-shell-invariants--run-checks 'pmark-restore)))

(defun agent-shell-invariants-on-collapse-toggle (namespace-id block-id collapsed-p)
  "Record and check after fragment collapse/expand.
NAMESPACE-ID and BLOCK-ID identify the fragment.
COLLAPSED-P is the new collapsed state."
  (when agent-shell-invariants-enabled
    (agent-shell-invariants--record
     'collapse-toggle
     :fragment-id (format "%s-%s" namespace-id block-id)
     :collapsed collapsed-p)
    (agent-shell-invariants--run-checks 'collapse-toggle)))

(defun agent-shell-invariants-on-notification (update-type &optional detail)
  "Record an ACP notification arrival.
UPDATE-TYPE is the sessionUpdate type string.
DETAIL is optional extra info (tool-call-id, etc.)."
  (when agent-shell-invariants-enabled
    (agent-shell-invariants--record
     'notification
     :update-type update-type
     :detail detail)))

;;; --- Interactive commands -------------------------------------------------

(defun agent-shell-toggle-invariants ()
  "Toggle invariant checking for the current buffer."
  (interactive)
  (setq agent-shell-invariants-enabled
        (not agent-shell-invariants-enabled))
  (when agent-shell-invariants-enabled
    (agent-shell-invariants--ensure-ring)
    (agent-shell-invariants--clear-violation-flag))
  (message "Invariant checking: %s"
           (if agent-shell-invariants-enabled "ON" "OFF")))

(defun agent-shell-view-invariant-events ()
  "Display the invariant event log for the current buffer."
  (interactive)
  (let ((events-str (agent-shell-invariants--format-events))
        (buf (get-buffer-create
              (format "*agent-shell events [%s]*" (buffer-name)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert events-str)
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun agent-shell-check-invariants-now ()
  "Run all invariant checks right now, regardless of the enabled flag.
Temporarily clears the violation-reported flag so the check always runs."
  (interactive)
  (let ((agent-shell-invariants-enabled t)
        (agent-shell-invariants--violation-reported nil))
    (agent-shell-invariants--run-checks 'manual-check)
    (unless (get-buffer (format "*agent-shell invariant [%s]*" (buffer-name)))
      (message "All invariants passed."))))

(provide 'agent-shell-invariants)

;;; agent-shell-invariants.el ends here
