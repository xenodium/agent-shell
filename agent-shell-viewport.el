;;; agent-shell-viewport.el --- Agent shell viewport interaction  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alvaro Ramirez

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

;; Viewport provides an alternative interaction mode for agent-shell.
;; It enables crafting queries, navigating conversation history,
;; and viewing responses in a dedicated buffer.
;;
;; Support the work https://github.com/sponsors/xenodium

;;; Code:

(require 'cursor-sensor)
(require 'seq)
(require 'subr-x)
(require 'window)
(require 'flymake)
(require 'markdown-overlays)
(require 'shell-maker)
(require 'transient)

(eval-when-compile
  (require 'cl-lib))

(declare-function agent-shell--current-shell "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell--next-command-and-response "agent-shell")
(declare-function agent-shell--get-region "agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--make-header "agent-shell")
(declare-function agent-shell--context "agent-shell")
(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--filter-buffer-substring "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-goto-last-interaction "agent-shell")
(declare-function agent-shell-interaction-at-point "agent-shell")
(declare-function agent-shell-copy-session-id "agent-shell")
(declare-function agent-shell-cycle-session-mode "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-interrupt-confirmed-p "agent-shell")
(declare-function agent-shell-open-transcript "agent-shell")
(declare-function agent-shell-queue-request "agent-shell")
(declare-function agent-shell-remove-pending-request "agent-shell")
(declare-function agent-shell-resume-pending-requests "agent-shell")
(declare-function agent-shell-view-acp-logs "agent-shell")
(declare-function agent-shell-view-traffic "agent-shell")
(declare-function agent-shell-next-permission-button "agent-shell")
(declare-function agent-shell-other-buffer "agent-shell")
(declare-function agent-shell-previous-permission-button "agent-shell")
(declare-function agent-shell-project-buffers "agent-shell")
(declare-function agent-shell-select-config "agent-shell")
(declare-function agent-shell-set-session-mode "agent-shell")
(declare-function agent-shell-set-session-model "agent-shell")
(declare-function agent-shell-set-session-thought-level "agent-shell")
(declare-function agent-shell-ui-backward-block "agent-shell")
(declare-function agent-shell-ui-forward-block "agent-shell")
(declare-function agent-shell-ui-mode "agent-shell")
(declare-function agent-shell-completion-mode "agent-shell-completion")
(declare-function agent-shell-yank-dwim "agent-shell")

(defvar agent-shell-header-style)
(defvar agent-shell-prefer-viewport-interaction)
(defvar agent-shell-preferred-agent-config)
(defvar agent-shell-session-strategy)
(defvar agent-shell--state)
(defvar agent-shell-file-completion-enabled)

(defvar-local agent-shell-viewport--compose-snapshot nil
  "Alist with :content and :location from compose buffer before viewing history.")
;; The viewport buffer transitions between major modes which clears
;; buffer-local vars. Make snapshot permanent-local.
(put 'agent-shell-viewport--compose-snapshot 'permanent-local t)

(defvar-local agent-shell-viewport--ring-index nil
  "Current index into `comint-input-ring' for history navigation.")
;; Survives mode switches (edit <-> view) which clear buffer-local vars.
(put 'agent-shell-viewport--ring-index 'permanent-local t)

(cl-defun agent-shell-viewport--show-buffer (&key append override submit no-focus shell-buffer)
  "Show a viewport compose buffer for the agent shell.

APPEND is appended to the viewport compose buffer.
OVERRIDE, when non-nil, replaces content verbatim (no trimming).
SUBMIT, when non-nil, submits after insertion.
NO-FOCUS, when non-nil, avoids focusing the viewport compose buffer.
SHELL-BUFFER, when non-nil, prefer this shell buffer.
NEW-SHELL, create a new shell (no history).

Returns an alist with insertion details or nil otherwise:

  ((:buffer . BUFFER)
   (:start . START)
   (:end . END))"
  (when submit
    (error "Not yet supported"))
  (when no-focus
    (error "Not yet supported"))
  (when (and append override)
    (error "Use :append or :override but not both"))
  (when shell-buffer
    ;; Momentarily set buffer to same window, so it's recent in stack.
    (let ((current (current-buffer)))
      (pop-to-buffer-same-window shell-buffer)
      (pop-to-buffer-same-window current)))
  (when-let* ((shell-buffer (or shell-buffer (agent-shell--shell-buffer)))
              (viewport-buffer (agent-shell-viewport--buffer :shell-buffer shell-buffer))
              (text (or append (agent-shell--context :shell-buffer shell-buffer) "")))
    (when (and override (not (string-empty-p text)))
      (error "Cannot override"))
    (let ((insert-start nil)
          (insert-end nil))
      ;; Is there text to be inserted? Reject while busy.
      (when (and (agent-shell-viewport--busy-p
                  :viewport-buffer viewport-buffer)
                 (or (not (string-empty-p (string-trim text)))
                     (and override (not (string-empty-p (string-trim override))))))
        (user-error "Busy... please wait"))
      (agent-shell--display-buffer viewport-buffer)
      (when (and override
                 (with-current-buffer viewport-buffer
                   ;; viewport buffer empty?
                   (not (= (buffer-size) 0))))
        (unless (y-or-n-p "Compose buffer is not empty.  Override?")
          ;; User does not want to override.
          ;; Treat as regurlar text (typically appended).
          (setq text (concat text
                             (unless (string-empty-p text)
                               "\n\n")
                             override))
          (setq override nil)))
      ;; TODO: Do we need to get prompt and partial response,
      ;; in case viewport compose buffer is created for the
      ;; first time on an ongoing/busy shell session?
      (cond
       ((agent-shell-viewport--busy-p)
        (agent-shell-viewport-view-mode))
       (override
        (agent-shell-viewport-edit-mode)
        (agent-shell-viewport--initialize)
        (setq insert-start (point))
        (insert override)
        (setq insert-end (point)))
       ((derived-mode-p 'agent-shell-viewport-edit-mode)
        (unless (string-empty-p text)
          (save-excursion
            (goto-char (point-max))
            (setq insert-start (point))
            (insert "\n\n" text)
            (setq insert-end (point)))))
       (t
        (agent-shell-viewport-edit-mode)
        ;; Transitioned to edit mode. Wipe content.
        (agent-shell-viewport--initialize)
        ;; Restore snapshot if needed.
        (when-let* ((snapshot agent-shell-viewport--compose-snapshot))
          (insert (map-elt snapshot :content))
          (goto-char (map-elt snapshot :location))
          (setq agent-shell-viewport--compose-snapshot nil))
        (save-excursion
          (goto-char (point-max))
          (setq insert-start (point))
          (unless (string-empty-p text)
            (insert "\n\n" text))
          (setq insert-end (point)))))
      `((:buffer . ,viewport-buffer)
        (:start . ,insert-start)
        (:end . ,insert-end)))))

(defun agent-shell-viewport-compose-send ()
  "Send the viewport composed prompt to the agent shell."
  (declare (modes agent-shell-viewport-edit-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-edit-mode)
    (user-error "Not in a shell viewport buffer"))
  (when (and (not (eq agent-shell-session-strategy 'new-deferred))
             (not (with-current-buffer (agent-shell-viewport--shell-buffer)
                    (map-nested-elt agent-shell--state '(:session :id)))))
    (user-error "Session not ready... please wait"))
  (setq agent-shell-viewport--compose-snapshot nil)
  (setq agent-shell-viewport--ring-index nil)
  (if agent-shell-prefer-viewport-interaction
      (agent-shell-viewport-compose-send-and-wait-for-response)
    (agent-shell-viewport-compose-send-and-kill)))

(defun agent-shell-viewport-compose-send-and-kill ()
  "Send the viewport composed prompt to the agent shell and kill compose buffer."
  (declare (modes agent-shell-viewport-edit-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-edit-mode)
    (user-error "Not in a shell viewport buffer"))
  (let ((shell-buffer (agent-shell-viewport--shell-buffer))
        (viewport-buffer (current-buffer))
        (prompt (buffer-string)))
    (with-current-buffer shell-buffer
      (agent-shell--insert-to-shell-buffer
       :text prompt
       :submit t))
    (kill-buffer viewport-buffer)
    (pop-to-buffer shell-buffer)))

(defun agent-shell-viewport-compose-send-and-wait-for-response ()
  "Send the viewport composed prompt and display response in viewport."
  (declare (modes agent-shell-viewport-edit-mode))
  (interactive)
  (catch 'exit
    (unless (derived-mode-p 'agent-shell-viewport-edit-mode)
      (user-error "Not in a shell viewport buffer"))
    (let ((shell-buffer (agent-shell-viewport--shell-buffer))
          (viewport-buffer (current-buffer))
          (prompt (string-trim (buffer-string))))
      (when (agent-shell-viewport--busy-p)
        (unless (agent-shell-interrupt-confirmed-p)
          (throw 'exit nil))
        (with-current-buffer shell-buffer
          (agent-shell-interrupt t))
        (with-current-buffer viewport-buffer
          (agent-shell-viewport-view-mode)
          (agent-shell-viewport--initialize
           :prompt prompt))
        (user-error "Aborted"))
      (when (string-empty-p (string-trim prompt))
        (agent-shell-viewport--initialize)
        (user-error "Nothing to send"))
      (if (derived-mode-p 'agent-shell-viewport-view-mode)
          (progn
            (agent-shell-viewport-edit-mode)
            (agent-shell-viewport--initialize))
        (let ((inhibit-read-only t))
          (markdown-overlays-put))
        (agent-shell-viewport-view-mode)
        (agent-shell-viewport--initialize :prompt prompt)
        ;; (setq view-exit-action 'kill-buffer) TODO
        (when (string-equal prompt "clear")
          (agent-shell-viewport-edit-mode)
          (agent-shell-viewport--initialize))
        (agent-shell--insert-to-shell-buffer
         :shell-buffer (agent-shell-viewport--shell-buffer)
         :text prompt
         :submit t
         :no-focus t)
        ;; TODO: Point should go to beginning of response after submission.
        (let ((inhibit-read-only t))
          (markdown-overlays-put))))))

(defun agent-shell-viewport-interrupt ()
  "Interrupt active agent shell request."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (catch 'exit
    (let ((shell-buffer (agent-shell-viewport--shell-buffer)))
      (unless (agent-shell-viewport--busy-p)
        (user-error "No pending request"))
      (unless (agent-shell-interrupt-confirmed-p)
        (throw 'exit nil))
      (with-current-buffer shell-buffer
        (agent-shell-interrupt t))
      (user-error "Aborted"))))

(cl-defun agent-shell-viewport--initialize (&key prompt response)
  "Initialize viewport compose buffer.

Optionally set its PROMPT and RESPONSE."
  (agent-shell-viewport--ensure-buffer)

  ;; Recalculate and cache position
  (agent-shell-viewport--position :force-refresh t)
  (let ((inhibit-read-only t)
        (viewport-buffer (current-buffer)))
    (erase-buffer)
    (when-let* ((shell-buffer (agent-shell-viewport--shell-buffer)))
      (with-current-buffer shell-buffer
        (unless (eq agent-shell-header-style 'graphical)
          ;; Insert newline at point-min purely for
          ;; display/layout. Only needed for non-graphical header.
          (with-current-buffer viewport-buffer
            (insert (propertize "\n"
                                'cursor-intangible t
                                'front-sticky '(cursor-intangible)
                                'rear-nonsticky '(cursor-intangible)))))))
    (when prompt
      (insert
       (if (derived-mode-p 'agent-shell-viewport-view-mode)
           (propertize (concat prompt "\n\n")
                       'rear-nonsticky t
                       'agent-shell-viewport-prompt t
                       'face 'font-lock-doc-face)
         prompt)))
    (when response
      (insert response))
    (let ((inhibit-read-only t))
      (markdown-overlays-put))))

(defun agent-shell-viewport--ensure-buffer ()
  "Ensure current buffer is a viewport and err otherwise."
  (unless (or (derived-mode-p 'agent-shell-viewport-view-mode)
              (derived-mode-p 'agent-shell-viewport-edit-mode))
    (user-error "Not in a shell viewport buffer")))

(defun agent-shell-viewport--prompt ()
  "Return the buffer prompt."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((start (if (get-text-property (point-min) 'agent-shell-viewport-prompt)
                           (point-min)
                         (next-single-property-change (point-min) 'agent-shell-viewport-prompt)))
                (found (get-text-property start 'agent-shell-viewport-prompt)))
      (string-trim
       (buffer-substring-no-properties
        start
        (or (next-single-property-change
             start 'agent-shell-viewport-prompt)
            (point-max)))))))

(defun agent-shell-viewport--response ()
  "Return the buffer response."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((start (if (get-text-property (point-min) 'agent-shell-viewport-prompt)
                           (point-min)
                         (next-single-property-change (point-min) 'agent-shell-viewport-prompt)))
                (found (get-text-property start 'agent-shell-viewport-prompt))
                (end (next-single-property-change start 'agent-shell-viewport-prompt)))
      (buffer-substring end (point-max)))))

(defun agent-shell-viewport--prompt-start ()
  "Return the start position of the prompt, or nil if no prompt."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((start (if (get-text-property (point-min) 'agent-shell-viewport-prompt)
                          (point-min)
                        (next-single-property-change (point-min) 'agent-shell-viewport-prompt))))
      (when (get-text-property start 'agent-shell-viewport-prompt)
        start))))

(defun agent-shell-viewport--response-start ()
  "Return the start position of the response, or nil if no response."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((start (if (get-text-property (point-min) 'agent-shell-viewport-prompt)
                           (point-min)
                         (next-single-property-change (point-min) 'agent-shell-viewport-prompt)))
                (found (get-text-property start 'agent-shell-viewport-prompt))
                (end (next-single-property-change start 'agent-shell-viewport-prompt)))
      (when (< end (point-max))
        end))))

(defun agent-shell-viewport-compose-cancel ()
  "Cancel prompt composition."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (setq agent-shell-viewport--compose-snapshot nil)
  (let ((viewport-buffer (current-buffer))
        (shell-buffer (agent-shell-viewport--shell-buffer)))
    (cond
     ;; View mode
     ((derived-mode-p 'agent-shell-viewport-view-mode)
      (bury-buffer))
     ;; Edit mode, no history to go back to
     ((with-current-buffer shell-buffer
        (not (shell-maker-history-position)))
      (when (or (string-empty-p (string-trim (buffer-string)))
                (y-or-n-p "Discard composed prompt? "))
        (agent-shell-viewport--initialize)
        (bury-buffer)))
     ;; Edit mode, has history
     (t
      (when (or (string-empty-p (string-trim (buffer-string)))
                (y-or-n-p "Discard composed prompt? "))
        (if agent-shell-prefer-viewport-interaction
            (agent-shell-viewport-view-last)
          (agent-shell-other-buffer)
          (kill-buffer viewport-buffer)))))))

(defun agent-shell-viewport-previous-history ()
  "Insert previous prompt from history into compose buffer."
  (declare (modes agent-shell-viewport-edit-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-edit-mode)
    (user-error "Not in a shell viewport buffer"))
  (let* ((ring (with-current-buffer (agent-shell-viewport--shell-buffer)
                 (seq-filter
                  (lambda (item)
                    (not (string-empty-p item)))
                  (ring-elements comint-input-ring))))
         (next-index (unless (seq-empty-p ring)
                       (if agent-shell-viewport--ring-index
                           (1+ agent-shell-viewport--ring-index)
                         0))))
    ;; Save in-progress compose text before first history navigation.
    (when (and (not agent-shell-viewport--ring-index)
               (not agent-shell-viewport--compose-snapshot))
      (setq agent-shell-viewport--compose-snapshot
            `((:content . ,(buffer-string))
              (:location . ,(point)))))
    (cond
     ;; Empty ring.
     ((not next-index)
      (setq agent-shell-viewport--ring-index nil))
     ;; Already at oldest entry, clamp.
     ((>= next-index (seq-length ring))
      (setq agent-shell-viewport--ring-index (1- (seq-length ring))))
     (t
      (setq agent-shell-viewport--ring-index next-index)))
    (when agent-shell-viewport--ring-index
      (agent-shell-viewport--initialize
       :prompt (seq-elt ring agent-shell-viewport--ring-index)))))

(defun agent-shell-viewport-next-history ()
  "Insert next prompt from history into compose buffer."
  (declare (modes agent-shell-viewport-edit-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-edit-mode)
    (user-error "Not in a shell viewport buffer"))
  (unless agent-shell-viewport--ring-index
    (user-error "No more history"))
  (let* ((ring (with-current-buffer (agent-shell-viewport--shell-buffer)
                 (seq-filter
                  (lambda (item)
                    (not (string-empty-p item)))
                  (ring-elements comint-input-ring))))
         (next-index (1- agent-shell-viewport--ring-index)))
    (if (< next-index 0)
        ;; Past newest entry, restore in-progress compose text.
        (let ((snapshot agent-shell-viewport--compose-snapshot))
          (setq agent-shell-viewport--ring-index nil)
          (agent-shell-viewport--initialize)
          (when snapshot
            (insert (map-elt snapshot :content))
            (goto-char (map-elt snapshot :location))
            (setq agent-shell-viewport--compose-snapshot nil)))
      ;; Show older entry.
      (setq agent-shell-viewport--ring-index next-index)
      (agent-shell-viewport--initialize
       :prompt (seq-elt ring next-index)))))

(defun agent-shell-viewport-search-history ()
  "Search prompt history, select, and insert into compose buffer."
  (declare (modes agent-shell-viewport-edit-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-edit-mode)
    (user-error "Not in a shell viewport buffer"))
  (insert (with-current-buffer (agent-shell-viewport--shell-buffer)
            (completing-read
             "History: "
             (delete-dups
              (seq-filter
               (lambda (item)
                 (not (string-empty-p item)))
               (ring-elements comint-input-ring))) nil t))))

(defun agent-shell-viewport-compose-peek-last ()
  "Save compose buffer snapshot and peek at the last interaction."
  (declare (modes agent-shell-viewport-edit-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-edit-mode)
    (user-error "Not in a prompt compose buffer"))
  (unless (with-current-buffer (agent-shell-viewport--shell-buffer)
            (shell-maker-history-position))
    (user-error "No items in history"))
  (setq agent-shell-viewport--compose-snapshot
        `((:content . ,(buffer-string))
          (:location . ,(point))))
  (agent-shell-viewport-view-last))

(defun agent-shell-viewport-view-last ()
  "Display the last request/response interaction."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (agent-shell-goto-last-interaction)
  (when-let* ((current (agent-shell-interaction-at-point)))
    (agent-shell-viewport-view-mode)
    (agent-shell-viewport--initialize
     :prompt (map-elt current :prompt)
     :response (map-elt current :response))
    (goto-char (point-min))
    current))

(defun agent-shell-viewport-refresh ()
  "Refresh viewport buffer content with current item from shell."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (when-let* ((viewport-buffer (current-buffer))
              (current (agent-shell-interaction-at-point)))
    (agent-shell-viewport--initialize
     :prompt (map-elt current :prompt)
     :response (map-elt current :response))
    (goto-char (point-min))
    current))

(defun agent-shell-viewport-next-item ()
  "Go to next item.

If at point-max, attempt to switch to next interaction."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-view-mode)
    (error "Not in a viewport buffer"))
  (let* ((current-pos (point))
         (prompt-start (agent-shell-viewport--prompt-start))
         (response-start (agent-shell-viewport--response-start))
         (block-pos (save-mark-and-excursion
                      (agent-shell-ui-forward-block)))
         (button-pos (save-mark-and-excursion
                       (agent-shell-next-permission-button)))
         ;; Filter positions to only those after current position
         (candidates (delq nil (list
                                (when (and prompt-start (> prompt-start current-pos))
                                  prompt-start)
                                (when (and response-start (> response-start current-pos))
                                  response-start)
                                block-pos
                                button-pos)))
         (next-pos (if candidates
                       (apply #'min candidates)
                     ;; No more items, try point-max if not already there
                     (when (< current-pos (point-max))
                       (point-max)))))
    (if next-pos
        (progn
          (deactivate-mark)
          (goto-char next-pos))
      ;; At point-max with no more items, try next interaction
      (condition-case nil
          (agent-shell-viewport-next-page)
        (error
         ;; At the end of all interactions, stay at point-max
         nil)))))

(defun agent-shell-viewport-previous-item ()
  "Go to previous item.

If at the first item, attempt to switch to previous interaction."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-view-mode)
    (error "Not in a viewport buffer"))
  (let* ((current-pos (point))
         (prompt-start (agent-shell-viewport--prompt-start))
         (response-start (agent-shell-viewport--response-start))
         (block-pos (save-mark-and-excursion
                      (let ((pos (agent-shell-ui-backward-block)))
                        (when (and pos (< pos current-pos))
                          pos))))
         (button-pos (save-mark-and-excursion
                       (let ((pos (agent-shell-previous-permission-button)))
                         (when (and pos (< pos current-pos))
                           pos))))
         ;; Filter positions to only those before current position
         (candidates (delq nil (list
                                (when (and prompt-start (< prompt-start current-pos))
                                  prompt-start)
                                (when (and response-start (< response-start current-pos))
                                  response-start)
                                block-pos
                                button-pos)))
         (next-pos (when candidates
                     (apply #'max candidates))))
    (if next-pos
        (progn
          (deactivate-mark)
          (goto-char next-pos))
      ;; No more items before current position, try previous interaction
      (condition-case nil
          ;; Switch to previous page and stop at point-max (call next-interaction directly)
          (agent-shell-viewport-next-page :backwards t)
        (error
         ;; At the beginning of all interactions, stay at first item
         (when prompt-start
           (goto-char prompt-start)))))))

(defconst agent-shell-viewport--suffix " [viewport]"
  "Suffix appended to shell buffer name to create viewport buffer name.")

(cl-defun agent-shell-viewport--buffer (&key shell-buffer existing-only)
  "Get the viewport buffer associated with a SHELL-BUFFER.

With EXISTING-ONLY, only return existing buffers without creating."
  (when-let* ((shell-buffer (or shell-buffer
                                (agent-shell--shell-buffer))))
    (with-current-buffer shell-buffer
      (let* ((viewport-buffer-name (concat (buffer-name (get-buffer shell-buffer))
                                           agent-shell-viewport--suffix))
             (viewport-buffer (get-buffer viewport-buffer-name)))
        (if viewport-buffer
            viewport-buffer
          (if existing-only
              nil
            (with-current-buffer (get-buffer-create viewport-buffer-name)
              (agent-shell-viewport-edit-mode)
              (current-buffer))))))))

(defun agent-shell-viewport--block-quote (text)
  "Return TEXT with each line prefixed by \"> \", displayed as a bar.

Underlying text keeps the \"> \" so it remains valid markdown;
the bar is a display-only override.  Yanks strip both the bar
styling and the leading \"> \" so paste gives plain text."
  (let* ((bar      (propertize "▌" 'face 'font-lock-comment-face))
         (wrap     (propertize "▌ " 'face 'font-lock-comment-face))
         (quoted   (concat "> " (replace-regexp-in-string
                                 (rx "\n") "\n> " text)))
         (rendered (copy-sequence quoted))
         (pos      0))
    (add-text-properties
     0 (length rendered)
     (list 'wrap-prefix wrap
           'face 'font-lock-comment-face
           'yank-handler
           (list (lambda (s)
                   (insert
                    (replace-regexp-in-string
                     (rx line-start "> ") ""
                     (substring-no-properties s))))))
     rendered)
    (while (string-match (rx line-start ">") rendered pos)
      (put-text-property (match-beginning 0) (match-end 0)
                         'display bar rendered)
      (setq pos (match-end 0)))
    rendered))

(cl-defun agent-shell-viewport--setup-reply (&key quoted-text)
  "Set up the buffer to compose a reply.

QUOTED-TEXT is inserted as a block quote as part of the reply."
  (with-current-buffer (agent-shell-viewport--shell-buffer)
    (goto-char (point-max)))
  (let ((snapshot agent-shell-viewport--compose-snapshot))
    (agent-shell-viewport-edit-mode)
    (agent-shell-viewport--initialize)
    (when snapshot
      (insert (map-elt snapshot :content))
      (setq agent-shell-viewport--compose-snapshot nil))
    (when quoted-text
      (goto-char (point-max))
      (insert (if snapshot "\n\n" "")
              (agent-shell-viewport--block-quote quoted-text) "\n\n"))
    ;; Skip past any cursor-intangible layout text (e.g. the
    ;; newline inserted by `agent-shell-viewport--initialize')
    ;; so callers like `agent-shell-viewport-reply-1' can insert.
    (goto-char (if (or snapshot quoted-text)
                   (point-max)
                 (or (next-single-property-change (point-min) 'cursor-intangible)
                     (point-max))))))

(defun agent-shell-viewport-reply ()
  "Reply as a follow-up and compose another prompt/query."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-view-mode)
    (user-error "Not in a shell viewport buffer"))
  (when (agent-shell-viewport--busy-p)
    (user-error "Busy, please wait"))
  (let ((region (map-elt (agent-shell--get-region :deactivate t) :content)))
    (agent-shell-viewport--setup-reply
     :quoted-text (when region (string-trim region))))
  ;; Setting point isn't enough at times. Force scrolling.
  (set-window-start (selected-window) (point-min)))

(defun agent-shell-viewport-quote-reply ()
  "Reply with the entire response block-quoted."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-view-mode)
    (user-error "Not in a shell viewport buffer"))
  (when (agent-shell-viewport--busy-p)
    (user-error "Busy, please wait"))
  (let ((response (or (agent-shell-viewport--response) "")))
    (agent-shell-viewport--setup-reply
     :quoted-text (string-trim (substring-no-properties response)))))

(defun agent-shell-viewport-reply-yes ()
  "Reply with \"yes\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "yes")
  (agent-shell-viewport-compose-send))

(defun agent-shell-viewport-reply-1 ()
  "Reply with \"1\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "1")
  (agent-shell-viewport-compose-send))

(defun agent-shell-viewport-reply-2 ()
  "Reply with \"2\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "2")
  (agent-shell-viewport-compose-send))

(defun agent-shell-viewport-reply-3 ()
  "Reply with \"3\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "3")
  (agent-shell-viewport-compose-send))

(defun agent-shell-viewport-reply-4 ()
  "Reply with \"4\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "4")
  (agent-shell-viewport-compose-send))

(defun agent-shell-viewport-reply-5 ()
  "Reply with \"5\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "5")
  (agent-shell-viewport-compose-send))

(defun agent-shell-viewport-reply-6 ()
  "Reply with \"6\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "6")
  (agent-shell-viewport-compose-send))

(defun agent-shell-viewport-reply-7 ()
  "Reply with \"7\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "7")
  (agent-shell-viewport-compose-send))

(defun agent-shell-viewport-reply-8 ()
  "Reply with \"8\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "8")
  (agent-shell-viewport-compose-send))

(defun agent-shell-viewport-reply-9 ()
  "Reply with \"9\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "9")
  (agent-shell-viewport-compose-send))

(defun agent-shell-viewport-reply-more ()
  "Reply with \"more\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "more")
  (agent-shell-viewport-compose-send))

(defun agent-shell-viewport-reply-again ()
  "Reply with \"again\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "again")
  (agent-shell-viewport-compose-send))

(defun agent-shell-viewport-reply-continue ()
  "Reply with \"continue\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "continue")
  (agent-shell-viewport-compose-send))

(defun agent-shell-viewport-previous-page ()
  "Show previous interaction (request / response)."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-next-page :backwards t :start-at-top t))

(cl-defun agent-shell-viewport-next-page (&key backwards start-at-top)
  "Show next interaction (request / response).

If BACKWARDS is non-nil, go to previous interaction.
If START-AT-TOP is non-nil, position at point-min regardless of direction.

If there are no more next items and a compose snapshot exists, restore the
buffer from the snapshot and switch to edit mode."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-view-mode)
    (error "Not in a viewport buffer"))
  (when (agent-shell-viewport--busy-p)
    (user-error "Busy... please wait"))
  (let ((shell-buffer (agent-shell-viewport--shell-buffer))
        (snapshot agent-shell-viewport--compose-snapshot)
        (pos (agent-shell-viewport--position :force-refresh t)))
    ;; Check if at last position going forward with a snapshot to restore
    (if (and (not backwards) snapshot pos
             (= (map-elt pos :current) (map-elt pos :total)))
        (progn
          (agent-shell-viewport-edit-mode)
          (agent-shell-viewport--initialize)
          (insert (map-elt snapshot :content))
          (goto-char (map-elt snapshot :location))
          (setq agent-shell-viewport--compose-snapshot nil)
          (cl-return-from agent-shell-viewport-next-page))
      (when-let* ((next (with-current-buffer shell-buffer
                          (if backwards
                              (when (save-excursion
                                      (let ((orig-line (line-number-at-pos)))
                                        (comint-previous-prompt 1)
                                        (= orig-line (line-number-at-pos))))
                                (error "No previous page"))
                            (when (save-excursion
                                    (let ((orig-line (point)))
                                      (comint-next-prompt 1)
                                      (= orig-line (point))))
                              (error "No next page")))
                          (agent-shell--next-command-and-response backwards))))
        (agent-shell-viewport--initialize
         :prompt (car next) :response (cdr next))
        (goto-char (if start-at-top
                       (point-min)
                     (if backwards (point-max) (point-min))))
        (agent-shell-viewport--update-header)
        next))))

(defun agent-shell-viewport-set-session-model ()
  "Set session model."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (let* ((shell-buffer (or (agent-shell--current-shell)
                           (user-error "Not in an agent-shell buffer")))
         (viewport-buffer (agent-shell-viewport--buffer
                          :shell-buffer shell-buffer
                          :existing-only t)))
    (with-current-buffer shell-buffer
      (agent-shell-set-session-model
       (lambda ()
         (with-current-buffer viewport-buffer
           (agent-shell-viewport--update-header)))))))

(defun agent-shell-viewport-set-session-mode ()
  "Set session mode."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (let* ((shell-buffer (or (agent-shell--current-shell)
                           (user-error "Not in an agent-shell buffer")))
         (viewport-buffer (agent-shell-viewport--buffer
                          :shell-buffer shell-buffer
                          :existing-only t)))
    (with-current-buffer shell-buffer
      (agent-shell-set-session-mode
       (lambda ()
         (when viewport-buffer
           (with-current-buffer viewport-buffer
             (agent-shell-viewport--update-header))))))))

(defun agent-shell-viewport-set-session-thought-level ()
  "Set thought level (reasoning effort) for the current session."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (let* ((shell-buffer (or (agent-shell--current-shell)
                           (user-error "Not in an agent-shell buffer")))
         (viewport-buffer (agent-shell-viewport--buffer
                          :shell-buffer shell-buffer
                          :existing-only t)))
    (with-current-buffer shell-buffer
      (agent-shell-set-session-thought-level
       (lambda ()
         (when viewport-buffer
           (with-current-buffer viewport-buffer
             (agent-shell-viewport--update-header))))))))

(defun agent-shell-viewport-cycle-session-mode ()
  "Cycle through available session modes."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (let* ((shell-buffer (or (agent-shell--current-shell)
                           (user-error "Not in an agent-shell buffer")))
         (viewport-buffer (agent-shell-viewport--buffer
                          :shell-buffer shell-buffer
                          :existing-only t)))
    (with-current-buffer shell-buffer
      (agent-shell-cycle-session-mode
       (lambda ()
         (when viewport-buffer
           (with-current-buffer viewport-buffer
             (agent-shell-viewport--update-header))))))))

(defun agent-shell-viewport-view-traffic ()
  "View agent shell traffic buffer."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (let ((shell-buffer (or (agent-shell--current-shell)
                          (user-error "Not in an agent-shell buffer"))))
    (with-current-buffer shell-buffer
      (agent-shell-view-traffic))))

(defun agent-shell-viewport-view-acp-logs ()
  "View agent shell ACP logs buffer."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (let ((shell-buffer (or (agent-shell--current-shell)
                          (user-error "Not in an agent-shell buffer"))))
    (with-current-buffer shell-buffer
      (agent-shell-view-acp-logs))))

(defun agent-shell-viewport-queue-request ()
  "Queue or immediately send a request depending on shell busy state."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (let ((shell-buffer (or (agent-shell--current-shell)
                          (user-error "Not in an agent-shell buffer"))))
    (with-current-buffer shell-buffer
      (call-interactively #'agent-shell-queue-request))))

(defun agent-shell-viewport-resume-pending-requests ()
  "Resume processing pending requests in the queue."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (let ((shell-buffer (or (agent-shell--current-shell)
                          (user-error "Not in an agent-shell buffer"))))
    (with-current-buffer shell-buffer
      (agent-shell-resume-pending-requests))))

(defun agent-shell-viewport-remove-pending-request ()
  "Remove pending requests."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (let ((shell-buffer (or (agent-shell--current-shell)
                          (user-error "Not in an agent-shell buffer"))))
    (with-current-buffer shell-buffer
      (call-interactively #'agent-shell-remove-pending-request))))

(defun agent-shell-viewport-copy-session-id ()
  "Copy the current session ID to the kill ring."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (let ((shell-buffer (or (agent-shell--current-shell)
                          (user-error "Not in an agent-shell buffer"))))
    (with-current-buffer shell-buffer
      (agent-shell-copy-session-id))))

(defun agent-shell-viewport-open-transcript ()
  "Open the transcript file for the current `agent-shell' session."
  (declare (modes agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell-viewport--ensure-buffer)
  (let ((shell-buffer (or (agent-shell--current-shell)
                          (user-error "Not in an agent-shell buffer"))))
    (with-current-buffer shell-buffer
      (agent-shell-open-transcript))))

;; Continuously fetching position can get expensive. Cache it.
(defvar-local agent-shell-viewport--position-cache nil
  "Cached position alist with :current and :total.")

(cl-defun agent-shell-viewport--position (&key force-refresh)
  "Return the position in history of the shell buffer.

When FORCE-REFRESH is non-nil, recalculate and update cache."
  (agent-shell-viewport--ensure-buffer)
  (if (and (not force-refresh) agent-shell-viewport--position-cache)
      agent-shell-viewport--position-cache
    (let ((position (with-current-buffer (agent-shell-viewport--shell-buffer)
                      (shell-maker-history-position))))
      (setq agent-shell-viewport--position-cache position)
      position)))

(cl-defun agent-shell-viewport--busy-p (&key viewport-buffer)
  "Return non-nil if the associated shell buffer is busy.

VIEWPORT-BUFFER is the viewport buffer to check."
  (when-let* ((shell-buffer (agent-shell--shell-buffer
                             :viewport-buffer viewport-buffer
                             :no-error t)))
    (with-current-buffer shell-buffer
      shell-maker--busy)))

(defvar agent-shell-viewport-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-shell-viewport-compose-send)
    (define-key map (kbd "C-c C-p") #'agent-shell-viewport-compose-peek-last)
    (define-key map (kbd "C-c C-k") #'agent-shell-viewport-compose-cancel)
    (define-key map (kbd "C-c C-h") #'agent-shell-viewport-compose-help-menu)
    (define-key map (kbd "C-<tab>") #'agent-shell-viewport-cycle-session-mode)
    (define-key map (kbd "C-c C-m") #'agent-shell-viewport-set-session-mode)
    (define-key map (kbd "C-c C-v") #'agent-shell-viewport-set-session-model)
    (define-key map (kbd "C-c C-t") #'agent-shell-viewport-set-session-thought-level)
    (define-key map (kbd "C-c C-o") #'agent-shell-other-buffer)
    (define-key map (kbd "M-p") #'agent-shell-viewport-previous-history)
    (define-key map (kbd "M-n") #'agent-shell-viewport-next-history)
    (define-key map (kbd "M-r") #'agent-shell-viewport-search-history)
    (define-key map [remap yank] #'agent-shell-yank-dwim)
    map)
  "Keymap for `agent-shell-viewport-edit-mode'.")

(defvar agent-shell-viewport-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-shell-viewport-interrupt)
    (define-key map (kbd "TAB") #'agent-shell-viewport-next-item)
    (define-key map (kbd "<backtab>") #'agent-shell-viewport-previous-item)
    (define-key map (kbd "n") #'agent-shell-viewport-next-item)
    (define-key map (kbd "p") #'agent-shell-viewport-previous-item)
    (define-key map (kbd "f") #'agent-shell-viewport-next-page)
    (define-key map (kbd "b") #'agent-shell-viewport-previous-page)
    (define-key map (kbd "r") #'agent-shell-viewport-reply)
    (define-key map (kbd "R") #'agent-shell-viewport-quote-reply)
    (define-key map (kbd "y") #'agent-shell-viewport-reply-yes)
    (define-key map (kbd "1") #'agent-shell-viewport-reply-1)
    (define-key map (kbd "2") #'agent-shell-viewport-reply-2)
    (define-key map (kbd "3") #'agent-shell-viewport-reply-3)
    (define-key map (kbd "4") #'agent-shell-viewport-reply-4)
    (define-key map (kbd "5") #'agent-shell-viewport-reply-5)
    (define-key map (kbd "6") #'agent-shell-viewport-reply-6)
    (define-key map (kbd "7") #'agent-shell-viewport-reply-7)
    (define-key map (kbd "8") #'agent-shell-viewport-reply-8)
    (define-key map (kbd "9") #'agent-shell-viewport-reply-9)
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "C-<tab>") #'agent-shell-viewport-cycle-session-mode)
    (define-key map (kbd "v") #'agent-shell-viewport-set-session-model)
    (define-key map (kbd "m") #'agent-shell-viewport-reply-more)
    (define-key map (kbd "a") #'agent-shell-viewport-reply-again)
    (define-key map (kbd "c") #'agent-shell-viewport-reply-continue)
    (define-key map (kbd "s") #'agent-shell-viewport-set-session-mode)
    (define-key map (kbd "t") #'agent-shell-viewport-set-session-thought-level)
    (define-key map (kbd "o") #'agent-shell-other-buffer)
    (define-key map (kbd "C-c C-o") #'agent-shell-other-buffer)
    (define-key map (kbd "?") #'agent-shell-viewport-help-menu)
    map)
  "Keymap for `agent-shell-viewport-view-mode'.")

(transient-define-prefix agent-shell-viewport--help-menu ()
  "`agent-shell' viewport help menu"
  [:class transient-columns
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'agent-shell-viewport-help-menu
             (list
              (apply #'vector "Viewport Help"
                     (agent-shell-viewport--make-transient-group
                      agent-shell-viewport-view-mode-map
                      '(((:function . agent-shell-viewport-next-item)
                         (:description . "Next item"))
                        ((:function . agent-shell-viewport-previous-item)
                         (:description . "Previous item"))
                        ((:function . agent-shell-viewport-next-page)
                         (:description . "Next page")
                         (:if-not . agent-shell-viewport--busy-p))
                        ((:function . agent-shell-viewport-previous-page)
                         (:description . "Previous Page")
                         (:if-not . agent-shell-viewport--busy-p))
                        ((:function . agent-shell-other-buffer)
                         (:description . "Switch to shell")
                         (:transient . nil))
                        ((:function . bury-buffer)
                         (:description . "Close")
                         (:transient . nil)))))
              (apply #'vector ""
                     (agent-shell-viewport--make-transient-group
                      agent-shell-viewport-view-mode-map
                      '(((:function . agent-shell-viewport-reply)
                         (:description . "Reply…")
                         (:if-not . agent-shell-viewport--busy-p))
                        ((:function . agent-shell-viewport-quote-reply)
                         (:description . "Quote reply…")
                         (:if-not . agent-shell-viewport--busy-p))
                        ((:function . agent-shell-viewport-reply-yes)
                         (:description . "Reply \"yes\"")
                         (:if-not . agent-shell-viewport--busy-p))
                        ((:function . agent-shell-viewport-reply-more)
                         (:description . "Reply \"more\"")
                         (:if-not . agent-shell-viewport--busy-p))
                        ((:function . agent-shell-viewport-reply-again)
                         (:description . "Reply \"again\"")
                         (:if-not . agent-shell-viewport--busy-p))
                        ((:function . agent-shell-viewport-reply-continue)
                         (:description . "Reply \"continue\"")
                         (:if-not . agent-shell-viewport--busy-p))
                        ((:function . agent-shell-viewport-reply-1)
                         (:description . "Reply \"1\"")
                         (:if-not . agent-shell-viewport--busy-p)))))
              (apply #'vector ""
                     (agent-shell-viewport--make-transient-group
                      agent-shell-viewport-view-mode-map
                      '(((:function . agent-shell-viewport-reply-2)
                         (:description . "Reply \"2\"")
                         (:if-not . agent-shell-viewport--busy-p))
                        ((:function . agent-shell-viewport-reply-3)
                         (:description . "Reply \"3\"")
                         (:if-not . agent-shell-viewport--busy-p))
                        ((:function . agent-shell-viewport-set-session-model)
                         (:description . "Set model"))
                        ((:function . agent-shell-viewport-set-session-mode)
                         (:description . "Set mode"))
                        ((:function . agent-shell-viewport-set-session-thought-level)
                         (:description . "Set thought level"))
                        ((:function . agent-shell-viewport-cycle-session-mode)
                         (:description . "Cycle mode"))
                        ((:function . agent-shell-viewport-interrupt)
                         (:description . "Interrupt")))))
              (apply #'vector ""
                     (agent-shell-viewport--make-transient-group
                      agent-shell-viewport-view-mode-map
                      '(((:function . agent-shell-viewport-view-traffic)
                         (:description . "View traffic"))
                        ((:function . agent-shell-viewport-view-acp-logs)
                         (:description . "View logs"))
                        ((:function . agent-shell-viewport-copy-session-id)
                         (:description . "Copy session ID"))
                        ((:function . agent-shell-viewport-open-transcript)
                         (:description . "Open transcript")))))
              )))])

(defun agent-shell-viewport-help-menu ()
  "Show viewport and display the transient help menu (bound to ? in view mode)."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-view-mode)
    (error "Not in a viewport buffer"))
  (call-interactively #'agent-shell-viewport--help-menu))

(transient-define-prefix agent-shell-viewport--compose-help-menu ()
  "`agent-shell' viewport compose help menu"
  [:class transient-columns
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'agent-shell-viewport-compose-help-menu
             (list
              (apply #'vector "Compose Help"
                     (agent-shell-viewport--make-transient-group
                      agent-shell-viewport-edit-mode-map
                      '(((:function . agent-shell-viewport-compose-send)
                         (:description . "Submit"))
                        ((:function . agent-shell-viewport-compose-cancel)
                         (:description . "Cancel"))
                        ((:function . agent-shell-viewport-compose-peek-last)
                         (:description . "Previous Page")))))
              (apply #'vector ""
                     (agent-shell-viewport--make-transient-group
                      agent-shell-viewport-edit-mode-map
                      '(((:function . agent-shell-viewport-previous-history)
                         (:description . "Previous prompt"))
                        ((:function . agent-shell-viewport-next-history)
                         (:description . "Next prompt"))
                        ((:function . agent-shell-viewport-search-history)
                         (:description . "Search prompts")))))
              (apply #'vector ""
                     (agent-shell-viewport--make-transient-group
                      agent-shell-viewport-edit-mode-map
                      '(((:function . agent-shell-viewport-set-session-model)
                         (:description . "Set model"))
                        ((:function . agent-shell-viewport-set-session-mode)
                         (:description . "Set mode"))
                        ((:function . agent-shell-viewport-set-session-thought-level)
                         (:description . "Set thought level"))
                        ((:function . agent-shell-viewport-cycle-session-mode)
                         (:description . "Cycle mode"))
                        ((:function . agent-shell-other-buffer)
                         (:description . "Switch to shell")
                         (:transient . nil))))))))])

(defun agent-shell-viewport-compose-help-menu ()
  "Show the transient help menu for compose (edit) mode."
  (declare (modes agent-shell-viewport-edit-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-edit-mode)
    (error "Not in a compose buffer"))
  (call-interactively #'agent-shell-viewport--compose-help-menu))

(defun agent-shell-viewport--make-transient-group (keymap commands)
  "Build a list of transient suffix specs from COMMANDS using KEYMAP.
Each element of COMMANDS is an alist with keys :function, :description,
and optionally :if, :if-not, or :transient (defaults to t).
Returns only suffixes whose function has a binding in KEYMAP."
  (seq-filter
   #'identity
   (seq-map (lambda (command)
              (when-let* ((keys (where-is-internal (map-elt command :function)
                                                   keymap t))
                          ((not (keymapp keys)))
                          (description (map-elt command :description)))
                (append (list (key-description keys) description (map-elt command :function)
                              :transient (map-elt command :transient t))
                        (when-let* ((pred (map-elt command :if)))
                          (list :if pred))
                        (when-let* ((pred (map-elt command :if-not)))
                          (list :if-not pred)))))
            commands)))

(defun agent-shell-viewport--update-header ()
  "Update header and mode line based on `agent-shell-header-style'.

Automatically determines position, status and bindings based on current
major mode."
  (agent-shell-viewport--ensure-buffer)
  (let* ((pos (or (agent-shell-viewport--position)
                  (list (cons :current 1) (cons :total 1))))
         (position-label (format "%d/%d" (map-elt pos :current) (map-elt pos :total)))
         (status (cond
                  ((agent-shell-viewport--busy-p) (propertize "Busy" 'face 'warning))
                  ((derived-mode-p 'agent-shell-viewport-edit-mode)
                   (propertize "Edit" 'face 'success))
                  ((derived-mode-p 'agent-shell-viewport-view-mode)
                   (propertize "View" 'face 'default))))
         (bindings (cond
                    ((derived-mode-p 'agent-shell-viewport-edit-mode)
                     (list
                      `((:key . ,(key-description (where-is-internal
                                                   'agent-shell-viewport-compose-send
                                                   agent-shell-viewport-edit-mode-map t)))
                        (:description . "Submit"))
                      `((:key . ,(key-description (where-is-internal
                                                   'agent-shell-viewport-compose-cancel
                                                   agent-shell-viewport-edit-mode-map t)))
                        (:description . "Cancel"))
                      `((:key . ,(key-description (where-is-internal
                                                   'agent-shell-viewport-compose-peek-last
                                                   agent-shell-viewport-edit-mode-map t)))
                        (:description . "Previous Page"))
                      `((:key . ,(key-description (where-is-internal
                                                   'agent-shell-viewport-compose-help-menu
                                                   agent-shell-viewport-edit-mode-map t)))
                        (:description . "Help"))))
                    ((derived-mode-p 'agent-shell-viewport-view-mode)
                     (append
                      (list
                       `((:key . ,(key-description (where-is-internal
                                                    'agent-shell-viewport-next-item
                                                    agent-shell-viewport-view-mode-map t)))
                         (:description . "Next"))
                       `((:key . ,(key-description (where-is-internal
                                                    'agent-shell-viewport-previous-item
                                                    agent-shell-viewport-view-mode-map t)))
                         (:description . "Previous")))
                      (unless (agent-shell-viewport--busy-p)
                        (list
                         `((:key . ,(key-description (where-is-internal
                                                      'agent-shell-viewport-reply
                                                      agent-shell-viewport-view-mode-map t)))
                           (:description . "Reply…"))))
                      (when (agent-shell-viewport--busy-p)
                        (list
                         `((:key . ,(key-description (where-is-internal
                                                      'agent-shell-viewport-interrupt
                                                      agent-shell-viewport-view-mode-map t)))
                           (:description . "Interrupt"))))
                      (list
                       `((:key . ,(key-description (where-is-internal
                                                    'agent-shell-viewport-help-menu
                                                    agent-shell-viewport-view-mode-map t)))
                         (:description . "Help")))))))
         (keymap (cond
                  ((derived-mode-p 'agent-shell-viewport-edit-mode)
                   agent-shell-viewport-edit-mode-map)
                  ((derived-mode-p 'agent-shell-viewport-view-mode)
                   agent-shell-viewport-view-mode-map)))
         (model-binding (when keymap
                          (key-description (where-is-internal
                                            'agent-shell-viewport-set-session-model
                                            keymap t))))
         (mode-binding (when keymap
                         (key-description (where-is-internal
                                           'agent-shell-viewport-set-session-mode
                                           keymap t))))
         (thought-level-binding (when keymap
                                  (key-description (where-is-internal
                                                    'agent-shell-viewport-set-session-thought-level
                                                    keymap t)))))
    (when-let* ((shell-buffer (agent-shell-viewport--shell-buffer))
                (header (with-current-buffer shell-buffer
                          (agent-shell--make-header (agent-shell--state)
                                                    :position position-label
                                                    :status status
                                                    :bindings bindings
                                                    :model-binding model-binding
                                                    :mode-binding mode-binding
                                                    :thought-level-binding thought-level-binding))))
      (setq-local header-line-format header))))

(defvar-local agent-shell-viewport--clean-up t)

(cl-defun agent-shell-viewport--shell-buffer (&optional viewport-buffer)
  "Get the shell buffer associated with VIEWPORT-BUFFER.

Derives shell buffer name by removing the viewport suffix from buffer name.
Returns nil if VIEWPORT-BUFFER is not a viewport buffer or shell doesn't exist."
  (when-let* ((viewport-name (buffer-name (or viewport-buffer (current-buffer))))
              ((string-suffix-p agent-shell-viewport--suffix viewport-name))
              (shell-name (substring viewport-name 0
                                     (- (length viewport-name)
                                        (length agent-shell-viewport--suffix)))))
    (get-buffer shell-name)))

(defun agent-shell-viewport--clean-up ()
  "Clean up resources.

For example, offer to kill associated shell session."
  (agent-shell-viewport--ensure-buffer)
  (if (and agent-shell-viewport--clean-up
           ;; Only offer to kill shell buffers when viewport buffer
           ;; is explicitly being killed from a viewport buffer.
           (eq (current-buffer)
               (window-buffer (selected-window))))
      ;; Temporarily disable cleaning up to avoid multiple clean-ups
      ;; triggered by shell buffers attempting to kill viewport buffer.
      (let ((agent-shell-viewport--clean-up nil))
        (when-let* ((shell-buffers (seq-filter (lambda (shell-buffer)
                                                 (and (equal (agent-shell-viewport--buffer
                                                              :shell-buffer shell-buffer
                                                              :existing-only t)
                                                             (current-buffer))
                                                      ;; Skip shells already shutting down (client
                                                      ;; is nil after agent-shell--shutdown).
                                                      (buffer-local-value 'agent-shell--state shell-buffer)
                                                      (map-elt (buffer-local-value 'agent-shell--state shell-buffer) :client)))
                                               (agent-shell-buffers)))
                    ((y-or-n-p "Kill shell session too?")))
          (mapc (lambda (shell-buffer)
                  (kill-buffer shell-buffer))
                shell-buffers)))))

(define-derived-mode agent-shell-viewport-edit-mode text-mode "Agent Shell Viewport (Edit)"
  "Major mode for composing agent shell prompts.

\\{agent-shell-viewport-edit-mode-map}"
  (cursor-intangible-mode +1)
  (setq buffer-read-only nil)
  (when agent-shell-file-completion-enabled
    (agent-shell-completion-mode +1))
  (agent-shell-viewport--update-header)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (add-hook 'kill-buffer-hook #'agent-shell-viewport--clean-up nil t))

(define-derived-mode agent-shell-viewport-view-mode text-mode "Agent Shell Viewport (View)"
  "Major mode for viewing agent shell prompts (read-only).

\\{agent-shell-viewport-view-mode-map}"
  (cursor-intangible-mode +1)
  (agent-shell-ui-mode +1)
  (agent-shell-viewport--update-header)
  (setq-local filter-buffer-substring-function #'agent-shell--filter-buffer-substring)
  (setq buffer-read-only t)
  (add-hook 'kill-buffer-hook #'agent-shell-viewport--clean-up nil t))

(provide 'agent-shell-viewport)

;;; agent-shell-viewport.el ends here
