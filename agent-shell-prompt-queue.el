;;; agent-shell-prompt-queue.el --- Prompt queueing for agent-shell. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

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
;; Queue prompts while an `agent-shell' is busy and manage the pending
;; queue (submit, view, resume, remove).
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'map)
(require 'ring)
(require 'text-property-search)
(require 'agent-shell-faces)
(eval-when-compile (require 'cl-lib))

(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--echo "agent-shell")
(declare-function agent-shell-completion--setup-minibuffer "agent-shell-completion")
(declare-function shell-maker-busy "shell-maker")

(defvar agent-shell--state)
(defvar comint-input-ring)

;; The queueing commands were renamed to the `agent-shell-prompt-queue'
;; namespace.  A package upgrade reloads this file into a running session
;; (see `package--reload-previously-loaded'), which redefines the new
;; names but leaves the old ones bound to stale definitions.  Unbind them
;; so they no longer show up in `M-x' or run outdated code.
;; TODO: Remove after 2026-08-28.
(dolist (command '(agent-shell-queue-request
                   agent-shell-resume-pending-requests
                   agent-shell-remove-pending-request))
  (fmakunbound command))

(defun agent-shell--prompt-queue-migrate ()
  "Migrate the obsolete `:pending-requests' state key to `:pending-prompts'.

Preserves queued prompts in live shells created before the key was
renamed (e.g. across a mid-session package upgrade).

TODO: Remove after 2026-08-28."
  (when (and (assq :pending-requests agent-shell--state)
             (not (assq :pending-prompts agent-shell--state)))
    (nconc agent-shell--state
           (list (cons :pending-prompts
                       (map-elt agent-shell--state :pending-requests))))))

(cl-defun agent-shell--prompt-queue-process-next ()
  "Process the next pending prompt from the queue if available."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (agent-shell--prompt-queue-migrate)
  (when-let* ((pending (map-elt agent-shell--state :pending-prompts))
              (next-prompt (car pending)))
    (map-put! agent-shell--state :pending-prompts (cdr pending))
    (agent-shell--insert-to-shell-buffer
     :text next-prompt
     :submit t
     :no-focus t)))

(defun agent-shell--prompt-queue-display ()
  "Display pending prompts in the shell buffer if queue is not empty."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (agent-shell--prompt-queue-migrate)
  (unless (seq-empty-p (map-elt agent-shell--state :pending-prompts))
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :block-id (format "%s-pending-prompts"
                       (map-elt (agent-shell--state) :request-count))
     :body (format "Pending prompts: %d

%s

Resume: M-x agent-shell-prompt-queue-resume
Remove: M-x agent-shell-prompt-queue-remove
"
                   (seq-length (map-elt agent-shell--state :pending-prompts))
                   (mapconcat
                    (lambda (idx-prompt)
                      (let ((idx (cdr idx-prompt))
                            (first-line (car (split-string (car idx-prompt) "\n" t))))
                        (format "  %d: \"%s\""
                                (1+ idx)
                                (truncate-string-to-width first-line 80 nil nil "..."))))
                    (seq-map-indexed #'cons (map-elt agent-shell--state :pending-prompts))
                    "\n"))
     :create-new t)))

(cl-defun agent-shell--prompt-queue-echo (&key active-prompt pending-prompts)
  "Message the in-progress prompt and PENDING-PROMPTS to the echo area.

ACTIVE-PROMPT is the prompt currently running, or nil if none.

PENDING-PROMPTS is a list of pending prompt strings, in the same form as
\(map-elt agent-shell--state :pending-prompts).

Each prompt is shown on a single line, prefixed by a status column
\(\"active\" or \"queued\"), and truncated to fit the frame width so it
never wraps.

For example, given:

  :active-prompt \"Find that nasty bug\"
  :pending-prompts (\"Next prompt text\" \"Second prompt\")

messages:

  active  Find that nasty bug
  queued  Next prompt text
  queued  Second prompt"
  (if (and (not active-prompt) (seq-empty-p pending-prompts))
      (agent-shell--echo "No pending prompts")
    (let ((available (- (frame-width) 8)))
      (agent-shell--echo
       "%s"
       (mapconcat
        (lambda (row)
          (concat
           (propertize (string-pad (map-elt row :status) 6)
                       'face (map-elt row :face))
           "  "
           (truncate-string-to-width
            (or (car (split-string (map-elt row :prompt) "\n" t)) "")
            available nil nil t)))
        (append
         (when active-prompt
           (list `((:status . "active")
                   (:face . success)
                   (:prompt . ,active-prompt))))
         (seq-map (lambda (prompt)
                    `((:status . "queued")
                      (:face . agent-shell-secondary)
                      (:prompt . ,prompt)))
                  pending-prompts))
        "\n")))))

(cl-defun agent-shell--prompt-queue-enqueue (&key prompt)
  "Add PROMPT to the pending prompts queue and echo the resulting queue.

The running prompt (the most recent `comint-input-ring' entry) is shown
as \"active\" and the queued prompts, PROMPT included, as \"queued\"."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (agent-shell--prompt-queue-migrate)
  (map-put! agent-shell--state :pending-prompts
            (append (map-elt agent-shell--state :pending-prompts)
                    (list prompt)))
  (agent-shell--prompt-queue-echo
   :active-prompt (when (and (bound-and-true-p comint-input-ring)
                             (not (ring-empty-p comint-input-ring)))
                    (ring-ref comint-input-ring 0))
   :pending-prompts (map-elt agent-shell--state :pending-prompts)))

(cl-defun agent-shell--prompt-queue-read (&key initial)
  "Read a queue prompt from the minibuffer.

When INITIAL is non-nil, prefill the minibuffer with it and leave
point at the end (ready to type below the prefill).

While reading, @ completes project files and / completes available
agent commands when the agent has reported them."
  (let ((shell-buffer (current-buffer)))
    (minibuffer-with-setup-hook
        (lambda ()
          (agent-shell-completion--setup-minibuffer shell-buffer)
          (when initial
            (insert initial)))
      (read-string (or (map-nested-elt (agent-shell--state) '(:agent-config :shell-prompt))
                       "Enqueue prompt: ")))))

(defun agent-shell-prompt-queue (prompt)
  "Queue or immediately send a prompt depending on shell busy state.

Read PROMPT from the minibuffer and act on the current project's shell,
resolving it via `agent-shell--shell-buffer' so this works even when
invoked outside a shell buffer.  If the shell is busy, add PROMPT to the
pending prompts queue.  Otherwise, submit it immediately.  Queued prompts
will be automatically sent when the current prompt completes.

While reading, @ completes project files and / completes available agent
commands when the agent has reported them."
  (interactive
   (list (with-current-buffer (agent-shell--shell-buffer :no-create t)
           (agent-shell--prompt-queue-read))))
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (if (shell-maker-busy)
        (agent-shell--prompt-queue-enqueue :prompt prompt)
      (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t))))

(defun agent-shell-prompt-queue-resume ()
  "Resume processing pending prompts in the queue.

Acts on the current project's shell, resolving it via
`agent-shell--shell-buffer' so this works even when invoked outside a
shell buffer."
  (interactive)
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (agent-shell--prompt-queue-migrate)
    (when (seq-empty-p (map-elt agent-shell--state :pending-prompts))
      (user-error "No pending prompts"))
    (if (shell-maker-busy)
        (message "Shell is busy, prompts will auto-resume when ready")
      (agent-shell--prompt-queue-process-next))))

(defun agent-shell-prompt-queue-remove (&optional remove-index)
  "Remove all pending prompts or a specific prompt by REMOVE-INDEX.

Acts on the current project's shell, resolving it via
`agent-shell--shell-buffer' so this works even when invoked outside a
shell buffer.  When called interactively with pending prompts, prompt to
either remove all or select a specific prompt to remove."
  (interactive
   (with-current-buffer (agent-shell--shell-buffer :no-create t)
     (agent-shell--prompt-queue-migrate)
     (when (seq-empty-p (map-elt agent-shell--state :pending-prompts))
       (user-error "No pending prompts"))
     (let* ((pending (map-elt agent-shell--state :pending-prompts))
            (choices (append
                      '(("Remove all" . remove-all))
                      (seq-map-indexed
                       (lambda (prompt idx)
                         (cons (format "%d: %s" (1+ idx)
                                       (truncate-string-to-width prompt 60 nil nil "..."))
                               idx))
                       pending)))
            (selection (cdr (assoc (completing-read "Remove: " choices nil t) choices))))
       (list (unless (eq selection 'remove-all) selection)))))
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (if remove-index
        (when (y-or-n-p (format "Remove \"%s\"?"
                                (nth remove-index
                                     (map-elt agent-shell--state :pending-prompts))))
          (let* ((pending (map-elt agent-shell--state :pending-prompts))
                 (new-pending (append (seq-take pending remove-index)
                                      (seq-drop pending (1+ remove-index)))))
            (map-put! agent-shell--state :pending-prompts new-pending)
            (message "Removed (%d remaining)"
                     (length new-pending))))
      (when (y-or-n-p (format "Remove %d pending prompts?"
                              (length (map-elt agent-shell--state :pending-prompts))))
        (map-put! agent-shell--state :pending-prompts nil)
        (message "Removed all pending prompts")))))

(defun agent-shell-queue-list-next-prompt ()
  "Move to the next queued prompt."
  (declare (modes agent-shell-queue-list-mode))
  (interactive)
  (if-let* ((match (save-excursion
                     (text-property-search-forward 'agent-shell-queue-item nil nil t))))
      (goto-char (prop-match-beginning match))
    (user-error "No next prompt")))

(defun agent-shell-queue-list-previous-prompt ()
  "Move to the previous queued prompt."
  (declare (modes agent-shell-queue-list-mode))
  (interactive)
  (if-let* ((match (save-excursion
                     (text-property-search-backward 'agent-shell-queue-item nil nil t))))
      (goto-char (prop-match-beginning match))
    (user-error "No previous prompt")))

(defvar agent-shell-queue-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'agent-shell-queue-list-next-prompt)
    (define-key map (kbd "p") #'agent-shell-queue-list-previous-prompt)
    map)
  "Keymap for `agent-shell-queue-list-mode'.")

(define-derived-mode agent-shell-queue-list-mode special-mode "Agent Shell Queue"
  "Major mode for viewing pending `agent-shell' prompts.")

(defun agent-shell-queue-list ()
  "Show all pending prompts, in full, in a dedicated read-only buffer.

Acts on the current project's shell, resolving it via
`agent-shell--shell-buffer' so this works even when invoked outside a
shell buffer."
  (interactive)
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (agent-shell--prompt-queue-migrate)
    (let ((pending (map-elt agent-shell--state :pending-prompts))
          (list-buffer (get-buffer-create (concat (buffer-name) " [queue]"))))
      (when (seq-empty-p pending)
        (user-error "No pending prompts"))
      (with-current-buffer list-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "Pending prompts: %d" (length pending))
                              'face 'agent-shell-section-heading)
                  "\n")
          (seq-do-indexed
           (lambda (prompt idx)
             (insert "\n"
                     (propertize (format "%d:" (1+ idx))
                                 'face 'agent-shell-secondary
                                 'agent-shell-queue-item (1+ idx))
                     "\n"
                     (string-trim-right prompt)
                     "\n"))
           pending))
        (agent-shell-queue-list-mode)
        (goto-char (point-min)))
      (pop-to-buffer list-buffer))))

(provide 'agent-shell-prompt-queue)

;;; agent-shell-prompt-queue.el ends here
