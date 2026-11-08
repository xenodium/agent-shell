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
(require 'agent-shell-faces)
(require 'agent-shell-prompt)
(eval-when-compile (require 'cl-lib))

(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--echo "agent-shell")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-steering-supported-p "agent-shell")
(declare-function agent-shell-experimental--send-steering "agent-shell-experimental")
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

(defcustom agent-shell-prompt-queue-merge t
  "Whether queued prompts are sent together as a single prompt.

Prompts queued while a turn runs are typically follow-ups to the same
instruction.  When non-nil, they are joined (separated by blank lines)
and sent as one turn.  When nil, they are sent one turn each, in the
order they were queued."
  :type 'boolean
  :group 'agent-shell)

(cl-defun agent-shell--prompt-queue-process-next ()
  "Submit the next pending prompt, or all of them merged into one.

Merges every pending prompt into a single prompt when
`agent-shell-prompt-queue-merge' is non-nil, otherwise takes just the
first.

For example, given:

  :pending-prompts (\"just the filenames\" \"sorted by size\")

submits:

  just the filenames

  sorted by size

and leaves :pending-prompts empty.  With merging off, submits
\"just the filenames\" and leaves (\"sorted by size\") pending."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (agent-shell--prompt-queue-migrate)
  (when-let* ((pending (map-elt agent-shell--state :pending-prompts))
              (count (if agent-shell-prompt-queue-merge
                         (seq-length pending)
                       1)))
    (map-put! agent-shell--state :pending-prompts (seq-drop pending count))
    ;; The turn just ended, so the persistent prompt may hold text the user
    ;; started typing and has not submitted.  Submitting the queued prompt
    ;; inserts at `point-max', which would put it ahead of that draft and
    ;; send the two as one message.  Set the draft aside and type it back
    ;; into the prompt the submission leaves behind.
    (let ((draft (and agent-shell-persistent-prompt-enabled
                      (agent-shell--take-prompt-input))))
      (agent-shell--insert-to-shell-buffer
       :text (string-join (seq-take pending count) "\n\n")
       :submit t
       :no-focus t)
      (when draft
        (goto-char (point-max))
        (insert draft)))))

(defun agent-shell--prompt-queue-summary ()
  "Return the pending prompts as a numbered list, headed by their count.

Each prompt is shown by its first line, truncated to 80 columns.

For example, given:

  :pending-prompts (\"just the filenames\" \"sorted by size\")

returns:

  Pending prompts: 2

    1: \"just the filenames\"
    2: \"sorted by size\""
  (agent-shell--prompt-queue-migrate)
  (format "Pending prompts: %d

%s"
          (seq-length (map-elt agent-shell--state :pending-prompts))
          (mapconcat
           (lambda (idx-prompt)
             (let ((idx (cdr idx-prompt))
                   (first-line (car (split-string
                                     (substring-no-properties (car idx-prompt))
                                     "\n" t))))
               (format "  %d: \"%s\""
                       (1+ idx)
                       (truncate-string-to-width first-line 80 nil nil "..."))))
           (seq-map-indexed #'cons (map-elt agent-shell--state :pending-prompts))
           "\n")))

(cl-defun agent-shell--prompt-queue-display (&key skip-summary)
  "Display how to manage pending prompts in the shell buffer, if any.

Lists the pending prompts (see `agent-shell--prompt-queue-summary') ahead
of the commands to resume or remove them, unless SKIP-SUMMARY is non-nil
because the user has just seen that list elsewhere."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (agent-shell--prompt-queue-migrate)
  (unless (seq-empty-p (map-elt agent-shell--state :pending-prompts))
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :block-id (format "%s-pending-prompts"
                       (map-elt (agent-shell--state) :request-count))
     :body (concat (unless skip-summary
                     (concat (agent-shell--prompt-queue-summary) "\n\n"))
                   "Resume: M-x agent-shell-prompt-queue-resume
Remove: M-x agent-shell-prompt-queue-remove
")
     :create-new t
     :above-last-prompt (not (shell-maker-busy)))))

(cl-defun agent-shell--prompt-queue-echo (&key active-prompt pending-prompts)
  "Message the in-progress prompt and PENDING-PROMPTS to the echo area.

ACTIVE-PROMPT is the prompt currently running, or nil if none.

PENDING-PROMPTS is a list of pending prompt strings, in the same form as
the :pending-prompts entry in variable `agent-shell--state'.

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
            (or (car (split-string
                      (substring-no-properties (map-elt row :prompt))
                      "\n" t)) "")
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

(defvar agent-shell-prompt-queue-setup-minibuffer-functions nil
  "Abnormal hook run while reading a queued prompt from the minibuffer.

Each function is called with a single alist containing:

  :shell-buffer - the shell the prompt is bound for

and runs with the minibuffer current, so it can decorate or extend the
prompt the way that shell renders its own.  The shell is carried rather
than looked up: it is resolved for the project, so it need not be the
buffer the minibuffer was entered from.")

(cl-defun agent-shell--prompt-queue-read (&key initial)
  "Read a queue prompt from the minibuffer.

When INITIAL is non-nil, prefill the minibuffer with it and leave
point at the end (ready to type below the prefill).

While reading, @ completes project files and / completes available
agent commands when the agent has reported them."
  (let ((shell-buffer (current-buffer)))
    (minibuffer-with-setup-hook
        (lambda ()
          (run-hook-with-args 'agent-shell-prompt-queue-setup-minibuffer-functions
                              `((:shell-buffer . ,shell-buffer)))
          (when initial
            (insert initial)))
      (read-string (or (map-nested-elt (agent-shell--state) '(:agent-config :shell-prompt))
                       "Enqueue prompt: ")))))

(defun agent-shell-prompt-steer (&optional prompt)
  "Steer PROMPT into the turn the agent is currently running.

Unlike `agent-shell-prompt-queue', the prompt reaches the agent while
it's already working on a submitted prompt, so it can change course
instead of finishing first.  Signals a `user-error' when a turn is running
and the agent cannot steer -- use `agent-shell-prompt-queue' for that one.

With no turn running there is nothing to steer into, so PROMPT is simply
submitted and starts the next turn.

Steering is not additive: an agent that interrupts what it is generating
may drop the instruction it was working on, so what the agent was already
doing can be lost.  Whether that happens is the agent's choice, not ours.

When the agent declines the steer, the running turn is interrupted rather
than left to carry on in a direction you believe you already corrected.

Steering a shell awaiting a permission answer asks for confirmation
first: no implementation defines what an agent does with a prompt
injected while a tool sits on that question, and a declined steer
interrupts the turn, which rejects that permission along with it.

While reading, @ completes project files and / completes available agent
commands when the agent has reported them."
  (interactive)
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (if (not (shell-maker-busy))
        ;; No turn to join, so submit PROMPT as usual.
        (progn
          (setq prompt (or prompt (agent-shell--prompt-queue-read)))
          (when (string-empty-p (string-trim prompt))
            (user-error "No prompt given"))
          (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t))
      ;; Refused before reading so it costs no typing.  Support is settled at
      ;; initialize time and cannot change while the prompt is written.
      (unless (agent-shell-steering-supported-p)
        (user-error "This agent does not support steering"))
      (when (eq (agent-shell-status) 'blocked)
        (unless (y-or-n-p
                 "Shell is pending user action (Steering may cancel work).  Steer anyway?")
          (user-error "Steering cancelled")))
      (setq prompt (or prompt (agent-shell--prompt-queue-read)))
      (when (string-empty-p (string-trim prompt))
        (user-error "No prompt given"))
      (agent-shell-experimental--send-steering
       :state (agent-shell--state)
       :prompt prompt))))

(defcustom agent-shell-busy-submit-default-function
  #'agent-shell-busy-submit-queue
  "Function deciding what submitting while the agent is working does.

Called with one argument, the prompt string.  Applies wherever a prompt
is submitted mid-turn: the shell prompt and the viewport's compose
buffer alike.

`agent-shell-busy-submit-override-function' is what
\\[agent-shell-submit-override] reaches for instead, so setting one of
these to each function makes both available without choosing."
  :type '(choice (const :tag "Queue until the turn ends" agent-shell-busy-submit-queue)
                 (const :tag "Steer into the running turn" agent-shell-busy-submit-steer)
                 (function :tag "Custom function"))
  :group 'agent-shell)

(defcustom agent-shell-busy-submit-override-function
  #'agent-shell-busy-submit-steer
  "Function \\[agent-shell-submit-override] submits through.

Called with one argument, the prompt string, and only when the agent is
working.  Overrides `agent-shell-busy-submit-default-function' for that
one submission.

Reached from both surfaces: \\[agent-shell-submit-override] at the shell
prompt, and \\[agent-shell-viewport-compose-send-override] in the
viewport's compose buffer, which keeps its own prefix for keeping that
buffer open."
  :type '(choice (const :tag "Queue until the turn ends" agent-shell-busy-submit-queue)
                 (const :tag "Steer into the running turn" agent-shell-busy-submit-steer)
                 (function :tag "Custom function"))
  :group 'agent-shell)

(defun agent-shell-busy-submit-queue (prompt)
  "Queue PROMPT and send it when the running turn ends.

One of the functions `agent-shell-busy-submit-default-function' and
`agent-shell-busy-submit-override-function' choose between."
  (agent-shell--prompt-queue-enqueue :prompt prompt))

(defun agent-shell-busy-submit-steer (prompt)
  "Hand PROMPT to the running turn so the agent can change course.

One of the functions `agent-shell-busy-submit-default-function' and
`agent-shell-busy-submit-override-function' choose between.

Queues PROMPT instead when the agent cannot steer, which not all can
\(see `agent-shell-steering-supported-p'), so choosing this does not mean
an error every turn.  `agent-shell-prompt-steer' is the command that
refuses outright.

Steering is not additive: see `agent-shell-prompt-steer' for what the
agent may drop in order to change course."
  (if (agent-shell-steering-supported-p)
      (agent-shell-experimental--send-steering
       :state (agent-shell--state)
       :prompt prompt)
    (agent-shell--prompt-queue-enqueue :prompt prompt)))

(cl-defun agent-shell--busy-submit (&key prompt override)
  "Submit PROMPT into the running turn, however the user has asked for.

Routes through `agent-shell-busy-submit-override-function' when OVERRIDE,
otherwise `agent-shell-busy-submit-default-function'.  Callers check that
a turn is running: with none, there is nothing to queue behind or steer
into, and the prompt is just submitted.

Signals whatever the chosen function signals, so callers holding text the
user typed can put it back."
  (funcall (if override
               agent-shell-busy-submit-override-function
             agent-shell-busy-submit-default-function)
           prompt))

(defun agent-shell-prompt-queue (prompt)
  "Queue or immediately send a prompt depending on shell busy state.

Read PROMPT from the minibuffer and act on the current project's shell,
resolving it via `agent-shell--shell-buffer' so this works even when
invoked outside a shell buffer.  If the shell is busy, add PROMPT to the
pending prompts queue.  Otherwise, submit it immediately.  Queued prompts
are automatically sent when the current prompt completes, merged into a
single prompt unless `agent-shell-prompt-queue-merge' is nil.  When it is
cancelled instead, you are asked whether to continue with them.

Always queues, ignoring `agent-shell-busy-submit-default-function'.

To hand PROMPT to the agent mid-turn instead of waiting, see
`agent-shell-prompt-steer'.

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
                                       (truncate-string-to-width
                                        (substring-no-properties prompt) 60 nil nil "..."))
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

(defun agent-shell--prompt-queue-confirm-kill-buffer ()
  "Return non-nil if the current shell may be killed.

Queued prompts live only in the shell buffer, as the agent has not
received them yet, so killing the shell loses them.  Asks for
confirmation when any are queued, unless running in batch mode.

For example, given:

  :pending-prompts (\"just the filenames\" \"sorted by size\")

asks:

  2 prompts queued.  Kill anyway? (y or n)

and with a single prompt queued:

  1 prompt queued.  Kill anyway? (y or n)

Added to `kill-buffer-query-functions' in shell buffers."
  (let ((count (seq-length (map-elt agent-shell--state :pending-prompts))))
    (or noninteractive
        (zerop count)
        (y-or-n-p (format "%d prompt%s queued.  Kill anyway?"
                          count (if (= count 1) "" "s"))))))

(defun agent-shell--prompt-queue-confirm-kill-emacs ()
  "Return non-nil if Emacs may exit, given any shells with queued prompts.

Asks once, however many shells hold queued prompts, unless running in
batch mode.  The question names `agent-shell', as the user may not be
looking at a shell when exiting, and a window lists the shells holding
queued prompts while it is asked, much like `save-buffers-kill-emacs'
does for live processes.  Declining keeps the window, where RET or a
click on a shell name switches to that shell and \\`q' closes it.

For example, given a shell named \"Claude @ proj\" with one queued
prompt, lists:

  Claude @ proj

  Pending prompts: 1

    1: \"just the filenames\"

and asks:

  agent-shell: Queued prompts not sent yet.  Exit anyway? (y or n)

Added to `kill-emacs-query-functions' when a shell starts."
  (let ((shells (seq-filter (lambda (shell-buffer)
                              (map-elt (buffer-local-value 'agent-shell--state shell-buffer)
                                       :pending-prompts))
                            (agent-shell-buffers))))
    (or noninteractive
        (not shells)
        (with-current-buffer-window
         (get-buffer-create "*agent-shell queued prompts*")
         '(display-buffer-at-bottom
           (dedicated . t)
           (window-height . fit-window-to-buffer)
           (preserve-size . (nil . t)))
         (lambda (window _value)
           (let ((exit (with-selected-window window
                         (y-or-n-p "agent-shell: Queued prompts not sent yet.  Exit anyway?"))))
             ;; Kept on decline, so its shell names can be followed.
             (when (and exit (window-live-p window))
               (quit-restore-window window 'kill))
             exit))
         (special-mode)
         (let ((inhibit-read-only t))
           (insert (mapconcat (lambda (shell-buffer)
                                (format "%s

%s"
                                        (buttonize (buffer-name shell-buffer)
                                                   (lambda (_)
                                                     (agent-shell--display-buffer shell-buffer))
                                                   nil
                                                   "Switch to shell")
                                        (with-current-buffer shell-buffer
                                          (agent-shell--prompt-queue-summary))))
                              shells
                              "\n\n")))))))

(provide 'agent-shell-prompt-queue)

;;; agent-shell-prompt-queue.el ends here
