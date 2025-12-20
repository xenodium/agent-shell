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

(eval-when-compile
  (require 'cl-lib))

(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--make-header "agent-shell")
(declare-function agent-shell--relevant-text "agent-shell")
(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--current-shell "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--get-region "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-next-permission-button "agent-shell")
(declare-function agent-shell-previous-permission-button "agent-shell")
(declare-function agent-shell-project-buffers "agent-shell")
(declare-function agent-shell-select-config "agent-shell")
(declare-function agent-shell-ui-backward-block "agent-shell")
(declare-function agent-shell-ui-forward-block "agent-shell")
(declare-function agent-shell-ui-mode "agent-shell")

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-other-buffer "agent-shell")
(declare-function agent-shell-cycle-session-mode "agent-shell")
(declare-function agent-shell-set-session-mode "agent-shell")
(declare-function agent-shell-set-session-model "agent-shell")

(defvar agent-shell-header-style)
(defvar agent-shell-prefer-viewport-interaction)
(defvar agent-shell-preferred-agent-config)

(cl-defun agent-shell-viewport--show-buffer (&key text submit no-focus shell-buffer)
  "Show a viewport compose buffer for the agent shell.

TEXT is inserted into the viewport compose buffer.
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
  (when shell-buffer
    ;; Momentarily set buffer to same window, so it's recent in stack.
    (let ((current (current-buffer)))
      (pop-to-buffer-same-window shell-buffer)
      (pop-to-buffer-same-window current)))
  (when-let ((shell-buffer (or shell-buffer (agent-shell--shell-buffer)))
             (viewport-buffer (agent-shell-viewport--buffer :shell-buffer shell-buffer))
             (text (or text (agent-shell--relevant-text) "")))
    (let ((insert-start nil)
          (insert-end nil))
      ;; Is there text to be inserted? Reject while busy.
      (when (and (agent-shell-viewport--busy-p
                  :viewport-buffer viewport-buffer)
                 (not (string-empty-p (string-trim text))))
        (user-error "Busy... please wait"))
      (agent-shell--display-buffer viewport-buffer)
      ;; TODO: Do we need to get prompt and partial response,
      ;; in case viewport compose buffer is created for the
      ;; first time on an ongoing/busy shell session?
      (if (agent-shell-viewport--busy-p)
          (agent-shell-viewport-view-mode)
        (if (derived-mode-p 'agent-shell-viewport-edit-mode)
            (unless (string-empty-p text)
              (save-excursion
                (goto-char (point-max))
                (setq insert-start (point))
                (unless (string-empty-p text)
                  (insert "\n\n" text))
                (setq insert-end (point))))
          (agent-shell-viewport-edit-mode)
          ;; Transitioned to edit mode. Start empty.
          (agent-shell-viewport--initialize)
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
  (interactive)
  (if agent-shell-prefer-viewport-interaction
      (agent-shell-viewport-compose-send-and-wait-for-response)
    (agent-shell-viewport-compose-send-and-kill)))

(defun agent-shell-viewport-compose-send-and-kill ()
  "Send the viewport composed prompt to the agent shell and kill compose buffer."
  (interactive)
  (let ((shell-buffer (agent-shell--shell-buffer))
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
  (interactive)
  (catch 'exit
    (unless (derived-mode-p 'agent-shell-viewport-edit-mode)
      (user-error "Not in a shell viewport buffer"))
    (let ((shell-buffer (agent-shell--shell-buffer))
          (viewport-buffer (agent-shell-viewport--buffer))
          (prompt (string-trim (buffer-string))))
      (when (agent-shell-viewport--busy-p)
        (unless (y-or-n-p "Interrupt?")
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
         :text prompt
         :submit t
         :no-focus t)
        ;; TODO: Point should go to beginning of response after submission.
        (let ((inhibit-read-only t))
          (markdown-overlays-put))))))

(defun agent-shell-viewport-interrupt ()
  "Interrupt active agent shell request."
  (interactive)
  (catch 'exit
    (let ((shell-buffer (agent-shell--shell-buffer)))
      (unless (agent-shell-viewport--busy-p)
        (user-error "No pending request"))
      (unless (y-or-n-p "Interrupt?")
        (throw 'exit nil))
      (with-current-buffer shell-buffer
        (agent-shell-interrupt t))
      (user-error "Aborted"))))

(cl-defun agent-shell-viewport--initialize (&key prompt response)
  "Initialize viewport compose buffer.

Optionally set its PROMPT and RESPONSE."
  (unless (or (derived-mode-p 'agent-shell-viewport-view-mode)
              (derived-mode-p 'agent-shell-viewport-edit-mode))
    (user-error "Not in a shell viewport buffer"))
  ;; Recalculate and cache position
  (agent-shell-viewport--position :force-refresh t)
  (let ((inhibit-read-only t)
        (viewport-buffer (current-buffer)))
    (erase-buffer)
    (when-let ((shell-buffer (agent-shell--shell-buffer)))
      (with-current-buffer shell-buffer
        (unless (eq agent-shell-header-style 'graphical)
          ;; Insert read-only newline at the point-min
          ;; purely for display/layout purpose. This
          ;; is only needed for non-graphical header.
          (with-current-buffer viewport-buffer
            (insert (propertize "\n"
                                'read-only t
                                'cursor-intangible t
                                'front-sticky '(read-only cursor-intangible)
                                'rear-nonsticky '(read-only cursor-intangible)))))))
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
    (when-let ((start (if (get-text-property (point-min) 'agent-shell-viewport-prompt)
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
  (interactive)
  (unless (or (derived-mode-p 'agent-shell-viewport-view-mode)
              (derived-mode-p 'agent-shell-viewport-edit-mode))
    (user-error "Not in a shell viewport buffer"))
  (if (or (derived-mode-p 'agent-shell-viewport-view-mode)
          (with-current-buffer (agent-shell--shell-buffer)
            (not (shell-maker-history))))
      (bury-buffer)
    ;; Edit mode
    (when (or (string-empty-p (string-trim (buffer-string)))
              (y-or-n-p "Discard composed prompt? "))
      (agent-shell-viewport-view-last))))

(defun agent-shell-viewport-view-last ()
  "Display the last request/response interaction."
  (interactive)
  (unless (or (derived-mode-p 'agent-shell-viewport-view-mode)
              (derived-mode-p 'agent-shell-viewport-edit-mode))
    (user-error "Not in a shell viewport buffer"))
  (when-let ((shell-buffer (agent-shell--shell-buffer)))
    (with-current-buffer shell-buffer
      (goto-char comint-last-input-start)))
  (agent-shell-viewport-view-mode)
  (agent-shell-viewport-refresh))

(defun agent-shell-viewport-refresh ()
  "Refresh viewport buffer content with current item from shell."
  (interactive)
  (unless (or (derived-mode-p 'agent-shell-viewport-view-mode)
              (derived-mode-p 'agent-shell-viewport-edit-mode))
    (user-error "Not in a shell viewport buffer"))
  (when-let ((shell-buffer (agent-shell--shell-buffer))
             (viewport-buffer (agent-shell-viewport--buffer))
             (current (with-current-buffer shell-buffer
                        (or (shell-maker--command-and-response-at-point)
                            (shell-maker-next-command-and-response t)))))
    (agent-shell-viewport--initialize
     :prompt (car current)
     :response (cdr current))
    (goto-char (point-min))
    current))

(defun agent-shell-viewport-next-item ()
  "Go to next item.

If at point-max, attempt to switch to next interaction."
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

(cl-defun agent-shell-viewport--buffer (&key shell-buffer existing-only)
  "Get the viewport buffer associated with a SHELL-BUFFER.

With EXISTING-ONLY, only return existing buffers without creating."
  (when-let ((shell-buffer (or shell-buffer
                               (agent-shell--shell-buffer))))
    (with-current-buffer shell-buffer
      (let* ((viewport-buffer-name (concat (buffer-name shell-buffer)
                                          " [viewport]"))
             (viewport-buffer (get-buffer viewport-buffer-name)))
        (if viewport-buffer
            viewport-buffer
          (if existing-only
              nil
            (with-current-buffer (get-buffer-create viewport-buffer-name)
              (agent-shell-viewport-edit-mode)
              (current-buffer))))))))

(defun agent-shell-viewport-reply ()
  "Reply as a follow-up and compose another prompt/query."
  (interactive)
  (when (agent-shell-viewport--busy-p)
    (user-error "Busy, please wait"))
  (let* ((region (map-elt (agent-shell--get-region :deactivate t) :content))
         (block-quoted-text (when region
                              (concat
                               (mapconcat (lambda (line)
                                            (concat "> " line))
                                          (split-string region "\n")
                                          "\n")
                               "\n\n"))))
    (with-current-buffer (agent-shell--shell-buffer)
      (goto-char (point-max)))
    (agent-shell-viewport-edit-mode)
    (if block-quoted-text
        (progn
          (agent-shell-viewport--initialize :prompt block-quoted-text)
          (goto-char (point-max)))
      (agent-shell-viewport--initialize)
      (goto-char (point-min)))
    ;; Setting point isn't enough at times. Force scrolling.
    (set-window-start (selected-window) (point-min))))

(defun agent-shell-viewport-previous-page ()
  "Show previous interaction (request / response)."
  (interactive)
  (agent-shell-viewport-next-page :backwards t :start-at-top t))

(cl-defun agent-shell-viewport-next-page (&key backwards start-at-top)
  "Show next interaction (request / response).

If BACKWARDS is non-nil, go to previous interaction.
If START-AT-TOP is non-nil, position at point-min regardless of direction."
  (interactive)
  (unless (derived-mode-p 'agent-shell-viewport-view-mode)
    (error "Not in a viewport buffer"))
  (when (agent-shell-viewport--busy-p)
    (user-error "Busy... please wait"))
  (when-let ((shell-buffer (agent-shell--shell-buffer))
             (viewport-buffer (agent-shell-viewport--buffer))
             (next (with-current-buffer shell-buffer
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
                     (shell-maker-next-command-and-response backwards))))
    (agent-shell-viewport--initialize
     :prompt (car next) :response (cdr next))
    (goto-char (if start-at-top
                   (point-min)
                 (if backwards (point-max) (point-min))))
    (agent-shell-viewport--update-header)
    next))

(defun agent-shell-viewport-set-session-model ()
  "Set session model."
  (interactive)
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
  (interactive)
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

(defun agent-shell-viewport-cycle-session-mode ()
  "Cycle through available session modes."
  (interactive)
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

;; Continuously fetching position can get expensive. Cache it.
(defvar-local agent-shell-viewport--position-cache nil
  "Cached position value (CURRENT . TOTAL).")

(cl-defun agent-shell-viewport--position (&key force-refresh)
  "Return the position in history of the shell buffer.

When FORCE-REFRESH is non-nil, recalculate and update cache."
  (unless (or (derived-mode-p 'agent-shell-viewport-view-mode)
              (derived-mode-p 'agent-shell-viewport-edit-mode))
    (user-error "Not in a shell viewport buffer"))
  (if (and (not force-refresh) agent-shell-viewport--position-cache)
      agent-shell-viewport--position-cache
    (let* ((shell-buffer (agent-shell--shell-buffer))
           (current (with-current-buffer shell-buffer
                      (shell-maker--command-and-response-at-point)))
           (history (with-current-buffer shell-buffer
                      (shell-maker-history)))
           (pos (seq-position history current))
           (position (cond ((and current history pos)
                            (cons (1+ pos) (length history)))
                           (history
                            (cons (1+ (length history))
                                  (1+ (length history)))))))
      (setq agent-shell-viewport--position-cache position)
      position)))

(cl-defun agent-shell-viewport--busy-p (&key viewport-buffer)
  "Return non-nil if the associated shell buffer is busy."
  (when-let ((shell-buffer (agent-shell--shell-buffer
                            :viewport-buffer viewport-buffer
                            :no-error t)))
    (with-current-buffer shell-buffer
      shell-maker--busy)))

(defvar agent-shell-viewport-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-shell-viewport-compose-send)
    (define-key map (kbd "C-c C-k") #'agent-shell-viewport-compose-cancel)
    (define-key map (kbd "C-<tab>") #'agent-shell-viewport-cycle-session-mode)
    (define-key map (kbd "C-c C-m") #'agent-shell-viewport-set-session-mode)
    (define-key map (kbd "C-c C-v") #'agent-shell-viewport-set-session-model)
    (define-key map (kbd "C-c C-o") #'agent-shell-other-buffer)
    map)
  "Keymap for `agent-shell-viewport-edit-mode'.")

(defvar agent-shell-viewport-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-shell-viewport-interrupt)
    (define-key map (kbd "n") #'agent-shell-viewport-next-item)
    (define-key map (kbd "p") #'agent-shell-viewport-previous-item)
    (define-key map (kbd "<tab>") #'agent-shell-viewport-next-item)
    (define-key map (kbd "<backtab>") #'agent-shell-viewport-previous-item)
    (define-key map (kbd "f") #'agent-shell-viewport-next-page)
    (define-key map (kbd "b") #'agent-shell-viewport-previous-page)
    (define-key map (kbd "r") #'agent-shell-viewport-reply)
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "C-<tab>") #'agent-shell-viewport-cycle-session-mode)
    (define-key map (kbd "v") #'agent-shell-viewport-set-session-model)
    (define-key map (kbd "m") #'agent-shell-viewport-set-session-mode)
    (define-key map (kbd "o") #'agent-shell-other-buffer)
    (define-key map (kbd "C-c C-o") #'agent-shell-other-buffer)
    map)
  "Keymap for `agent-shell-viewport-view-mode'.")


(defun agent-shell-viewport--update-header ()
  "Update header and mode line based on `agent-shell-header-style'.

Automatically determines qualifier and bindings based on current major mode."
  (unless (or (derived-mode-p 'agent-shell-viewport-view-mode)
              (derived-mode-p 'agent-shell-viewport-edit-mode))
    (user-error "Not in a shell viewport buffer"))
  (let* ((pos (or (agent-shell-viewport--position)
                  (cons 1 1)))
         (pos-label (format "%d/%d" (car pos) (cdr pos)))
         (qualifier (cond
                     ((agent-shell-viewport--busy-p)
                      (format "[%s][Busy]" pos-label))
                     ((derived-mode-p 'agent-shell-viewport-edit-mode)
                      (format "[%s][Edit]" pos-label))
                     ((derived-mode-p 'agent-shell-viewport-view-mode)
                      (format "[%s][View]" pos-label))))
         (bindings (cond
                    ((derived-mode-p 'agent-shell-viewport-edit-mode)
                     (list
                      `((:key . ,(key-description (where-is-internal
                                                   'agent-shell-viewport-compose-send
                                                   agent-shell-viewport-edit-mode-map t)))
                        (:description . "send"))
                      `((:key . ,(key-description (where-is-internal
                                                   'agent-shell-viewport-compose-cancel
                                                   agent-shell-viewport-edit-mode-map t)))
                        (:description . "cancel"))))
                    ((derived-mode-p 'agent-shell-viewport-view-mode)
                     (append
                      (list
                       `((:key . ,(key-description (where-is-internal
                                                    'agent-shell-viewport-next-item
                                                    agent-shell-viewport-view-mode-map t)))
                         (:description . "next"))
                       `((:key . ,(key-description (where-is-internal
                                                    'agent-shell-viewport-previous-item
                                                    agent-shell-viewport-view-mode-map t)))
                         (:description . "previous")))
                      (unless (agent-shell-viewport--busy-p)
                        (list
                         `((:key . ,(key-description (where-is-internal
                                                      'agent-shell-viewport-reply
                                                      agent-shell-viewport-view-mode-map t)))
                           (:description . "reply"))))
                      (when (agent-shell-viewport--busy-p)
                        (list
                         `((:key . ,(key-description (where-is-internal
                                                      'agent-shell-viewport-interrupt
                                                      agent-shell-viewport-view-mode-map t)))
                           (:description . "interrupt")))))))))
    (when-let* ((shell-buffer (agent-shell--shell-buffer))
                (header (with-current-buffer shell-buffer
                          (cond
                           ((eq agent-shell-header-style 'graphical)
                            (agent-shell--make-header (agent-shell--state)
                                                      :qualifier qualifier
                                                      :bindings bindings))
                           ((memq agent-shell-header-style '(text none nil))
                            (agent-shell--make-header (agent-shell--state)
                                                      :qualifier qualifier
                                                      :bindings bindings))))))
      (setq-local header-line-format header))))

(defvar-local agent-shell-viewport--clean-up t)

;; Continuously fetching position can get expensive. Cache it.
(defvar-local agent-shell-viewport--position-cache nil
  "Cached position value (CURRENT . TOTAL).")

(defun agent-shell-viewport--clean-up ()
  "Clean up resources.

For example, offer to kill associated shell session."
  (unless (or (derived-mode-p 'agent-shell-viewport-view-mode)
              (derived-mode-p 'agent-shell-viewport-edit-mode))
    (user-error "Not in a shell viewport buffer"))
  (if (and agent-shell-viewport--clean-up
           ;; Only offer to kill shell buffers when viewport buffer
           ;; is explicitly being killed from a viewport buffer.
           (eq (current-buffer)
               (window-buffer (selected-window))))
      ;; Temporarily disable cleaning up to avoid multiple clean-ups
      ;; triggered by shell buffers attempting to kill viewport buffer.
      (let ((agent-shell-viewport--clean-up nil))
        (when-let ((shell-buffers (seq-filter (lambda (shell-buffer)
                                                (equal (agent-shell-viewport--buffer
                                                        :shell-buffer shell-buffer
                                                        :existing-only t)
                                                       (current-buffer)))
                                              (agent-shell-buffers))))
          (when (y-or-n-p "Kill shell session too?")
            (mapc (lambda (shell-buffer)
                    (kill-buffer shell-buffer))
                  shell-buffers))))))

(define-derived-mode agent-shell-viewport-edit-mode text-mode "Agent Shell Viewport (Edit)"
  "Major mode for composing agent shell prompts.

\\{agent-shell-viewport-edit-mode-map}"
  (cursor-intangible-mode +1)
  (setq buffer-read-only nil)
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
  (setq buffer-read-only t)
  (add-hook 'kill-buffer-hook #'agent-shell-viewport--clean-up nil t))

(provide 'agent-shell-viewport)

;;; agent-shell-viewport.el ends here
