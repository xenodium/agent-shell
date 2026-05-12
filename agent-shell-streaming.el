;;; agent-shell-streaming.el --- Streaming tool call handler for agent-shell -*- lexical-binding: t; -*-

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
;; Streaming tool call handler for agent-shell.  Accumulates incremental
;; tool output from _meta.*.toolResponse and renders it on final update,
;; avoiding duplicate output.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'seq)
(require 'agent-shell-invariants)
(require 'subr-x)
(require 'agent-shell-meta)

;; Functions that remain in agent-shell.el
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--delete-fragment "agent-shell")
(declare-function agent-shell--save-tool-call "agent-shell")
(declare-function agent-shell--make-diff-info "agent-shell")
(declare-function agent-shell--format-diff-as-text "agent-shell")
(declare-function agent-shell--append-transcript "agent-shell")
(declare-function agent-shell--make-transcript-tool-call-entry "agent-shell")
(declare-function agent-shell-make-tool-call-label "agent-shell")
(declare-function agent-shell--extract-tool-parameters "agent-shell")
(declare-function agent-shell-ui--nearest-range-matching-property "agent-shell-ui")

(defvar agent-shell-tool-use-expand-by-default)
(defvar agent-shell--transcript-file)
(defvar agent-shell-ui--content-store)

;;; Output normalization

(defun agent-shell--tool-call-normalize-output (text)
  "Normalize tool call output TEXT for streaming.
Strips backtick fences, formats <persisted-output> wrappers as
fontified notices, and ensures a trailing newline.

For example:

  (agent-shell--tool-call-normalize-output \"hello\")
    => \"hello\\n\"

  (agent-shell--tool-call-normalize-output
   \"<persisted-output>saved</persisted-output>\")
    => fontified string with tags stripped"
  (when (and text (stringp text))
    (let ((result (string-join (seq-remove (lambda (line)
                                             (string-match-p "\\`\\s-*```" line))
                                           (split-string text "\n"))
                               "\n")))
      (when (string-match-p "<persisted-output>" result)
        (setq result (replace-regexp-in-string
                      "</?persisted-output>" "" result))
        (setq result (string-trim result))
        (setq result (propertize (concat "\n" result)
                                 'font-lock-face 'font-lock-comment-face)))
      (when (and (not (string-empty-p result))
                 (not (string-suffix-p "\n" result)))
        (setq result (concat result "\n")))
      result)))

(defun agent-shell--tool-call-content-text (content)
  "Return concatenated text from tool call CONTENT items.

For example:

  (agent-shell--tool-call-content-text
   [((content . ((text . \"hello\"))))])
    => \"hello\""
  (let* ((items (cond
                 ((vectorp content) (append content nil))
                 ((listp content) content)
                 (content (list content))))
         (parts (delq nil
                      (mapcar (lambda (item)
                                (let-alist item
                                  (when (and (stringp .content.text)
                                             (not (string-empty-p .content.text)))
                                    .content.text)))
                              items))))
    (when parts
      (mapconcat #'identity parts "\n\n"))))

;;; Chunk accumulation

(defun agent-shell--tool-call-append-output-chunk (state tool-call-id chunk)
  "Append CHUNK to tool call output buffer for TOOL-CALL-ID in STATE."
  (let* ((tool-calls (map-elt state :tool-calls))
         (entry (or (map-elt tool-calls tool-call-id) (list)))
         (chunks (map-elt entry :output-chunks)))
    (setf (map-elt entry :output-chunks) (cons chunk chunks))
    (setf (map-elt tool-calls tool-call-id) entry)
    (map-put! state :tool-calls tool-calls)))

(defun agent-shell--tool-call-output-text (state tool-call-id)
  "Return aggregated output for TOOL-CALL-ID from STATE."
  (let ((chunks (map-nested-elt state `(:tool-calls ,tool-call-id :output-chunks))))
    (when (and chunks (listp chunks))
      (mapconcat #'identity (reverse chunks) ""))))

(defun agent-shell--tool-call-clear-output (state tool-call-id)
  "Clear aggregated output for TOOL-CALL-ID in STATE."
  (let* ((tool-calls (map-elt state :tool-calls))
         (entry (map-elt tool-calls tool-call-id)))
    (when entry
      (setf (map-elt entry :output-chunks) nil)
      (setf (map-elt entry :output-marker) nil)
      (setf (map-elt entry :output-ui-state) nil)
      (setf (map-elt tool-calls tool-call-id) entry)
      (map-put! state :tool-calls tool-calls))))

(defun agent-shell--tool-call-output-marker (state tool-call-id)
  "Return output marker for TOOL-CALL-ID in STATE."
  (map-nested-elt state `(:tool-calls ,tool-call-id :output-marker)))

(defun agent-shell--tool-call-set-output-marker (state tool-call-id marker)
  "Set output MARKER for TOOL-CALL-ID in STATE."
  (let* ((tool-calls (map-elt state :tool-calls))
         (entry (or (map-elt tool-calls tool-call-id) (list))))
    (setf (map-elt entry :output-marker) marker)
    (setf (map-elt tool-calls tool-call-id) entry)
    (map-put! state :tool-calls tool-calls)))

(defun agent-shell--tool-call-output-ui-state (state tool-call-id)
  "Return cached UI state for TOOL-CALL-ID in STATE."
  (map-nested-elt state `(:tool-calls ,tool-call-id :output-ui-state)))

(defun agent-shell--tool-call-set-output-ui-state (state tool-call-id ui-state)
  "Set cached UI-STATE for TOOL-CALL-ID in STATE."
  (let* ((tool-calls (map-elt state :tool-calls))
         (entry (or (map-elt tool-calls tool-call-id) (list))))
    (setf (map-elt entry :output-ui-state) ui-state)
    (setf (map-elt tool-calls tool-call-id) entry)
    (map-put! state :tool-calls tool-calls)))

(defun agent-shell--tool-call-body-range-info (state tool-call-id)
  "Return tool call body range info for TOOL-CALL-ID in STATE."
  (when-let ((buffer (map-elt state :buffer)))
    (with-current-buffer buffer
      (let* ((qualified-id (format "%s-%s" (map-elt state :request-count) tool-call-id))
             (match (save-excursion
                      (goto-char (point-max))
                      (text-property-search-backward
                       'agent-shell-ui-state nil
                       (lambda (_ state)
                         (equal (map-elt state :qualified-id) qualified-id))
                       t))))
        (when match
          (let* ((block-start (prop-match-beginning match))
                 (block-end (prop-match-end match))
                 (ui-state (get-text-property block-start 'agent-shell-ui-state))
                 (body-range (agent-shell-ui--nearest-range-matching-property
                              :property 'agent-shell-ui-section :value 'body
                              :from block-start :to block-end)))
            (list (cons :ui-state ui-state)
                  (cons :body-range body-range))))))))

(defun agent-shell--tool-call-ensure-output-marker (state tool-call-id)
  "Ensure an output marker exists for TOOL-CALL-ID in STATE."
  (let* ((buffer (map-elt state :buffer))
         (marker (agent-shell--tool-call-output-marker state tool-call-id)))
    (when (or (not (markerp marker))
              (not (eq (marker-buffer marker) buffer)))
      (setq marker nil))
    (unless marker
      (when-let ((info (agent-shell--tool-call-body-range-info state tool-call-id))
                 (body-range (map-elt info :body-range)))
        (setq marker (copy-marker (map-elt body-range :end) t))
        (agent-shell--tool-call-set-output-marker state tool-call-id marker)
        (agent-shell--tool-call-set-output-ui-state state tool-call-id (map-elt info :ui-state))))
    marker))

(defun agent-shell--store-tool-call-output (ui-state text)
  "Store TEXT in the content-store for UI-STATE's body key."
  (when-let ((qualified-id (map-elt ui-state :qualified-id))
             (key (concat qualified-id "-body")))
    (unless agent-shell-ui--content-store
      (setq agent-shell-ui--content-store (make-hash-table :test 'equal)))
    (puthash key
             (concat (or (gethash key agent-shell-ui--content-store) "") text)
             agent-shell-ui--content-store)))

(defun agent-shell--append-tool-call-output (state tool-call-id text)
  "Append TEXT to TOOL-CALL-ID output body in STATE without formatting.
Note: process-mark preservation is unnecessary here because the output
marker is inside the fragment body, which is always before the
process-mark.  Insertions at the output marker shift the process-mark
forward by the correct amount automatically."
  (when (and text (not (string-empty-p text)))
    (with-current-buffer (map-elt state :buffer)
      (let* ((inhibit-read-only t)
             (buffer-undo-list t)
             (was-at-end (eobp))
             (saved-point (copy-marker (point) t))
             (marker (agent-shell--tool-call-ensure-output-marker state tool-call-id))
             (ui-state (agent-shell--tool-call-output-ui-state state tool-call-id)))
        (if (not marker)
            (progn
              (agent-shell--update-fragment
               :state state
               :block-id tool-call-id
               :body text
               :append t
               :navigation 'always)
              (agent-shell--tool-call-ensure-output-marker state tool-call-id)
              (setq ui-state (agent-shell--tool-call-output-ui-state state tool-call-id))
              (agent-shell--store-tool-call-output ui-state text))
          (goto-char marker)
          (let ((start (point)))
            (insert text)
            (let ((end (point))
                  (collapsed (and ui-state (map-elt ui-state :collapsed)))
                  (qualified-id (and ui-state (map-elt ui-state :qualified-id))))
              (set-marker marker end)
              ;; Streamed appends bypass `agent-shell--update-fragment',
              ;; so the block-level `field' and body-level `help-echo'
              ;; properties that wrapper applies aren't extended.  Stamp
              ;; them here to keep comint field navigation and tooltips
              ;; consistent across the streamed region.
              (add-text-properties
               start end
               `(read-only t
                 front-sticky (read-only)
                 field output
                 ,@(when qualified-id (list 'help-echo qualified-id))
                 agent-shell-ui-state ,ui-state
                 agent-shell-ui-section body))
              (agent-shell--store-tool-call-output ui-state text)
              (when collapsed
                (add-text-properties start end '(invisible t))))))
        (if was-at-end
            (goto-char (point-max))
          (goto-char saved-point))
        (set-marker saved-point nil)
        (agent-shell-invariants-on-append-output
         tool-call-id
         (when marker (marker-position marker))
         (length text))))))

;;; Streaming handler

(defun agent-shell--tool-call-final-p (status)
  "Return non-nil when STATUS represents a final tool call state."
  (and status (member status '("completed" "failed" "cancelled"))))

(defun agent-shell--tool-call-update-overrides (state update &optional include-content include-diff)
  "Build tool call overrides for UPDATE in STATE.
INCLUDE-CONTENT and INCLUDE-DIFF control optional fields."
  (let ((diff (when include-diff
                (agent-shell--make-diff-info :acp-tool-call update))))
    (append (list (cons :status (map-elt update 'status)))
            (when include-content
              (list (cons :content (map-elt update 'content))))
            ;; The initial tool_call notification often carries a generic
            ;; title (eg. "Bash", "Read"); a later tool_call_update may
            ;; supply a more descriptive one (eg. 'grep -i -n "tool"
            ;; /path/to/file').  Upgrade whenever a non-empty title
            ;; arrives.  See https://github.com/xenodium/agent-shell/issues/182
            ;; and https://github.com/xenodium/agent-shell/issues/309.
            (when-let* ((new-title (map-elt update 'title))
                        ((not (string-empty-p new-title))))
              (list (cons :title new-title)))
            (when diff
              (list (cons :diff diff))))))

(defun agent-shell--handle-tool-call-update-streaming (state update)
  "Stream tool call UPDATE in STATE with dedup.
Three cond branches:
  1. Terminal output data: accumulate and stream to buffer live.
  2. Non-final meta-response: accumulate only, no buffer write.
  3. Final: render accumulated output or fallback to content-text."
  (let* ((tool-call-id (map-elt update 'toolCallId))
         (status (map-elt update 'status))
         (terminal-data (agent-shell--tool-call-terminal-output-data update))
         (meta-response (agent-shell--tool-call-meta-response-text update))
         (final (agent-shell--tool-call-final-p status)))
    (agent-shell--save-tool-call
     state
     tool-call-id
     (agent-shell--tool-call-update-overrides state update nil nil))
    ;; Accumulate meta-response before final rendering so output is
    ;; available even when stdout arrives only on the final update.
    ;; Skip when terminal-data is also present to avoid double-accumulation
    ;; (both sources carry the same underlying output).  Run the chunk
    ;; through the same delta dedup as thought chunks: agents that re-send
    ;; cumulative stdout across updates would otherwise concatenate every
    ;; revision into the final render.
    (when (and meta-response (not terminal-data))
      (let* ((accumulated (or (agent-shell--tool-call-output-text state tool-call-id) ""))
             (normalized (agent-shell--tool-call-normalize-output meta-response))
             (delta (and normalized
                         (agent-shell--thought-chunk-delta accumulated normalized))))
        (when (and delta (not (string-empty-p delta)))
          (agent-shell--tool-call-append-output-chunk state tool-call-id delta))))
    (cond
     ;; Terminal output data (e.g. codex-acp): accumulate and stream live.
     ((and terminal-data (stringp terminal-data))
      (let ((chunk (agent-shell--tool-call-normalize-output terminal-data)))
        (when (and chunk (not (string-empty-p chunk)))
          (agent-shell--tool-call-append-output-chunk state tool-call-id chunk)
          (unless final
            (agent-shell--append-tool-call-output state tool-call-id chunk))))
      (when final
        (agent-shell--handle-tool-call-final state update)
        (agent-shell--tool-call-clear-output state tool-call-id)))
     (final
      (agent-shell--handle-tool-call-final state update)))
    ;; Update labels for non-final updates (final gets labels via
    ;; handle-tool-call-final).  Only rebuild when labels actually
    ;; changed — the rebuild invalidates the output marker used by
    ;; live terminal streaming and is O(fragment-size), so skipping
    ;; unchanged labels avoids O(n²) total work during streaming.
    (unless final
      (let* ((tool-call-labels (agent-shell-make-tool-call-label
                                state tool-call-id))
             (new-left (map-elt tool-call-labels :status))
             (new-right (map-elt tool-call-labels :title))
             (prev-left (map-nested-elt state `(:tool-calls ,tool-call-id :prev-label-left)))
             (prev-right (map-nested-elt state `(:tool-calls ,tool-call-id :prev-label-right))))
        (unless (and (equal new-left prev-left)
                     (equal new-right prev-right))
          (agent-shell--update-fragment
           :state state
           :block-id tool-call-id
           :label-left new-left
           :label-right new-right
           :expanded agent-shell-tool-use-expand-by-default)
          (agent-shell--tool-call-set-output-marker state tool-call-id nil)
          ;; Cache labels to skip redundant rebuilds on next update.
          (let* ((tool-calls (map-elt state :tool-calls))
                 (entry (or (map-elt tool-calls tool-call-id) (list))))
            (setf (map-elt entry :prev-label-left) new-left)
            (setf (map-elt entry :prev-label-right) new-right)
            (setf (map-elt tool-calls tool-call-id) entry)
            (map-put! state :tool-calls tool-calls)))))))

(defun agent-shell--handle-tool-call-final (state update)
  "Render final tool call UPDATE in STATE.
Uses accumulated output-chunks when available, otherwise falls
back to content-text extraction."
  (let-alist update
    (let* ((accumulated (agent-shell--tool-call-output-text state .toolCallId))
           (content-text (or accumulated
                             (agent-shell--tool-call-content-text .content)))
           (diff (map-nested-elt state `(:tool-calls ,.toolCallId :diff)))
           (output (if (and content-text (not (string-empty-p content-text)))
                       (concat "\n\n" content-text "\n\n")
                     ""))
           (diff-text (agent-shell--format-diff-as-text diff))
           (body-text (if diff-text
                          (concat output
                                  "\n\n"
                                  "╭─────────╮\n"
                                  "│ changes │\n"
                                  "╰─────────╯\n\n" diff-text)
                        output)))
      (agent-shell--save-tool-call
       state
       .toolCallId
       (agent-shell--tool-call-update-overrides state update t t))
      (when (member .status '("completed" "failed"))
        (agent-shell--append-transcript
         :text (agent-shell--make-transcript-tool-call-entry
                :status .status
                :title (map-nested-elt state `(:tool-calls ,.toolCallId :title))
                :kind (map-nested-elt state `(:tool-calls ,.toolCallId :kind))
                :description (map-nested-elt state `(:tool-calls ,.toolCallId :description))
                :command (map-nested-elt state `(:tool-calls ,.toolCallId :command))
                :parameters (agent-shell--extract-tool-parameters
                             (map-nested-elt state `(:tool-calls ,.toolCallId :raw-input)))
                :output body-text)
         :file-path agent-shell--transcript-file))
      (when (and .status
                 (not (equal .status "pending")))
        (agent-shell--delete-fragment :state state :block-id (format "permission-%s" .toolCallId)))
      (let* ((tool-call-labels (agent-shell-make-tool-call-label
                                state .toolCallId))
             (saved-command (map-nested-elt state `(:tool-calls ,.toolCallId :command)))
             (command-block (when saved-command
                              (concat "```console\n" saved-command "\n```"))))
        (agent-shell--update-fragment
         :state state
         :block-id .toolCallId
         :label-left (map-elt tool-call-labels :status)
         :label-right (map-elt tool-call-labels :title)
         :body (if command-block
                   (concat command-block "\n\n" (string-trim body-text))
                 (string-trim body-text))
         :expanded agent-shell-tool-use-expand-by-default))
      ;; Clear the per-tool label cache too — the streaming dispatcher
      ;; uses it to skip redundant rebuilds during in-flight updates.
      ;; After final, no further updates fire, so the cached values
      ;; would just linger in state for the lifetime of the shell.
      (when-let* ((tool-calls (map-elt state :tool-calls))
                  (entry (map-elt tool-calls .toolCallId)))
        (setf (map-elt entry :prev-label-left) nil)
        (setf (map-elt entry :prev-label-right) nil)
        (setf (map-elt tool-calls .toolCallId) entry)
        (map-put! state :tool-calls tool-calls))
      (agent-shell--tool-call-clear-output state .toolCallId))))

;;; Thought chunk dedup

(defun agent-shell--thought-chunk-delta (accumulated chunk)
  "Return the portion of CHUNK not already present in ACCUMULATED.
When an agent re-delivers the full accumulated thought text (e.g.
codex-acp sending a cumulative summary after incremental tokens),
only the genuinely new tail is returned.

Four cases are handled:
  ;; Cumulative from start (prefix match)
  (agent-shell--thought-chunk-delta \"AB\" \"ABCD\")  => \"CD\"

  ;; Already present (suffix match, e.g. leading whitespace trimmed)
  (agent-shell--thought-chunk-delta \"\\n\\nABCD\" \"ABCD\")  => \"\"

  ;; Partial overlap (tail of accumulated matches head of chunk)
  (agent-shell--thought-chunk-delta \"ABCD\" \"CDEF\")  => \"EF\"

  ;; Incremental token (no overlap)
  (agent-shell--thought-chunk-delta \"AB\" \"CD\")  => \"CD\""
  (cond
   ((or (null accumulated) (string-empty-p accumulated))
    chunk)
   ;; Chunk starts with all accumulated text (cumulative from start).
   ((string-prefix-p accumulated chunk)
    (substring chunk (length accumulated)))
   ;; Chunk is already fully contained as a suffix of accumulated
   ;; (e.g. re-delivery omits leading whitespace tokens).
   ((string-suffix-p chunk accumulated)
    "")
   ;; Partial overlap: tail of accumulated matches head of chunk.
   ;; Try decreasing overlap lengths to find the longest match.
   (t
    (let ((max-overlap (min (length accumulated) (length chunk)))
          (overlap 0))
      (cl-loop for len from max-overlap downto 1
               when (string= (substring accumulated (- (length accumulated) len))
                             (substring chunk 0 len))
               do (setq overlap len) and return nil)
      (if (< 0 overlap)
          (substring chunk overlap)
        chunk)))))

;;; Cancellation

(defun agent-shell--mark-tool-calls-cancelled (state)
  "Mark in-flight tool-call entries in STATE as cancelled and update UI."
  (let ((tool-calls (map-elt state :tool-calls)))
    (when tool-calls
      (map-do
       (lambda (tool-call-id tool-call-data)
         (let ((status (map-elt tool-call-data :status)))
           (when (or (not status)
                     (member status '("pending" "in_progress")))
             (agent-shell--handle-tool-call-final
                state
                `((toolCallId . ,tool-call-id)
                  (status . "cancelled")
                  (content . ,(map-elt tool-call-data :content))))
             (agent-shell--tool-call-clear-output state tool-call-id))))
       tool-calls))))

(provide 'agent-shell-streaming)

;;; agent-shell-streaming.el ends here
