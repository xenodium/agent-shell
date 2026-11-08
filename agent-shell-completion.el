;;; agent-shell-completion.el --- Completion support for agent-shell. -*- lexical-binding: t; -*-

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
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'comint)
(require 'map)
(require 'agent-shell-project)

(declare-function agent-shell--file-mention "agent-shell")
(declare-function agent-shell--live-input-prompt-p "agent-shell")
(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--project-files "agent-shell-project")

(defvar agent-shell--state)
(defvar agent-shell-prompt-queue-setup-minibuffer-functions)

(defcustom agent-shell-file-completion-enabled t
  "Non-nil pops project file completion as @ is typed.

@ completion is available via \\[completion-at-point] either way.  This
only decides whether typing @ pops it up automatically, which can be
slow in large projects."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-command-completion-enabled t
  "Non-nil pops agent command completion as / is typed.

/ only pops at the start of the input, where agents recognize commands.
/ completion is available via \\[completion-at-point] either way."
  :type 'boolean
  :group 'agent-shell)

(defun agent-shell--completion-bounds (char-class trigger-char)
  "Find completion bounds for CHAR-CLASS, if TRIGGER-CHAR precedes them.
Returns alist with :start and :end if TRIGGER-CHAR is found before
the word, nil otherwise."
  (save-excursion
    (when-let* ((end (progn (skip-chars-forward char-class) (point)))
                (start (progn (skip-chars-backward char-class) (point)))
                ((eq (char-before start) trigger-char))
                ((or (= start (1+ (line-beginning-position)))
                     (memq (char-before (1- start)) '(?\s ?\t ?\n)))))
      `((:start . ,start) (:end . ,end)))))

(defun agent-shell-completion--input-start ()
  "Return the position where the prompt input being composed begins.
Handles the shell's live comint prompt, the viewport's compose buffer
and the minibuffer reading a queued prompt.  Falls back to `point-min',
which includes a stale prompt with agent output streaming below it.

With the shell buffer ending in `Claude> hello', where the prompt
`Claude> ' ends at position 30 and the typed `hello' occupies 30 to 35,
returns 30."
  (cond
   ((minibufferp)
    (minibuffer-prompt-end))
   ((derived-mode-p 'agent-shell-viewport-edit-mode)
    (point-min))
   ((and (derived-mode-p 'comint-mode)
         comint-last-prompt
         (agent-shell--live-input-prompt-p comint-last-prompt)
         (>= (point) (cdr comint-last-prompt)))
    (marker-position (cdr comint-last-prompt)))
   (t
    (point-min))))

(defun agent-shell-completion--command-start-p (position)
  "Non-nil when a / typed at POSITION starts a slash command.
Agents only recognize a slash command as the very first character of a
message, so nothing at all may precede POSITION in the input being
composed.

With the shell buffer ending in `Claude> hello ', where the typed input
occupies 30 to 36: a / typed right after the prompt (POSITION 30)
returns non-nil, while one typed after `hello ' (POSITION 36) returns
nil."
  (= position (agent-shell-completion--input-start)))

(defun agent-shell--capf-exit-with-space (_string status)
  "Insert space after completion, if STATUS is `finished'.

Other statuses mean the user may keep typing (`sole' and `exact' arrive
from UIs like Corfu and Company while candidates still match)."
  (when (eq status 'finished)
    (insert " ")))

(defun agent-shell--capf-exit-with-file-mention (string status)
  "Rewrite the completed file STRING as an @ mention, then insert a space.

Completion inserts the candidate bare, so a path holding whitespace would
be read as a mention ending at its first space.  Rewriting through
`agent-shell--file-mention' quotes those paths.

Only acts when STATUS is `finished', as with
`agent-shell--capf-exit-with-space'.

For example, completing at the end of a buffer holding

  \"@src/main.el\"    => \"@src/main.el \"
  \"@My Design.png\"  => \"@\\\"My Design.png\\\" \""
  (when-let* (((eq status 'finished))
              (start (- (point) (length string) 1))
              ((eq (char-after start) ?@)))
    (delete-region start (point))
    (insert (agent-shell--file-mention string)))
  (agent-shell--capf-exit-with-space string status))

(defvar-local agent-shell--project-files-cache nil
  "Session-scoped cache for project files completion.")

(defvar-local agent-shell-completion--shell-buffer nil
  "Override shell buffer to source completion data from.
When non-nil, file and command CAPFs read project files and
available commands from this buffer instead of the current one.
Set in non-shell buffers (e.g. the minibuffer) that want to offer
@ and / completion against a specific shell.")

(defun agent-shell-completion--source-buffer ()
  "Return the buffer to source completion data from.
Returns the override buffer when set and live, otherwise the current
buffer.  Returns nil if the override is set but its buffer is dead."
  (cond
   ((null agent-shell-completion--shell-buffer)
    (current-buffer))
   ((buffer-live-p agent-shell-completion--shell-buffer)
    agent-shell-completion--shell-buffer)))

(defun agent-shell--clear-project-files-cache ()
  "Clear project files cache when completion session ends."
  (unless completion-in-region-mode
    (setq agent-shell--project-files-cache nil)
    (remove-hook 'completion-in-region-mode-hook
                 #'agent-shell--clear-project-files-cache t)))

(defun agent-shell--file-completion-at-point ()
  "Complete project files after @."
  (when-let* ((source (agent-shell-completion--source-buffer))
              (bounds (agent-shell--completion-bounds "[:alnum:]/_.-" ?@)))
    (with-current-buffer source
      (unless agent-shell--project-files-cache
        (setq agent-shell--project-files-cache (agent-shell--project-files))
        (add-hook 'completion-in-region-mode-hook
                  #'agent-shell--clear-project-files-cache nil t)))
    (list (map-elt bounds :start) (map-elt bounds :end)
          (buffer-local-value 'agent-shell--project-files-cache source)
          :exclusive 'no
          ;; Typing @ alone is enough for Company to pop candidates.
          :company-prefix-length t
          :company-kind (lambda (f) (if (string-suffix-p "/" f) 'folder 'file))
          :exit-function #'agent-shell--capf-exit-with-file-mention)))

(defun agent-shell--command-completion-at-point ()
  "Complete available commands after /."
  (when-let* ((bounds (agent-shell--completion-bounds "[:alnum:]_-" ?/))
              ;; Bounds start after /, so the / itself sits one char back.
              ((agent-shell-completion--command-start-p (1- (map-elt bounds :start))))
              (source (or (and (buffer-live-p agent-shell-completion--shell-buffer)
                               agent-shell-completion--shell-buffer)
                          (agent-shell--shell-buffer :no-error t :no-create t)))
              (commands (map-elt (buffer-local-value 'agent-shell--state source)
                                 :available-commands))
              (descriptions (mapcar (lambda (c)
                                      (cons (map-elt c 'name)
                                            (map-elt c 'description)))
                                    commands)))
    (list (map-elt bounds :start) (map-elt bounds :end)
          (mapcar #'car descriptions)
          :exclusive t
          ;; Typing / alone is enough for Company to pop candidates.
          :company-prefix-length t
          :annotation-function
          (lambda (name)
            (when-let* ((desc (map-elt descriptions name)))
              (concat "  " desc)))
          :company-kind (lambda (_) 'function)
          :exit-function #'agent-shell--capf-exit-with-space)))

(defun agent-shell--trigger-completion-at-point ()
  "Trigger completion when @ or / is typed at a word boundary.
Each pops only when `agent-shell-file-completion-enabled' or
`agent-shell-command-completion-enabled' respectively allows it.
Only triggers when the character is at line start or after whitespace,
preventing spurious completions mid-word or in paths.  / additionally
requires being at the start of the input, where agents recognize
commands."
  (when (and (memq (char-before) '(?@ ?/))
             (or (= (point) (1+ (line-beginning-position)))
                 (memq (char-before (1- (point))) '(?\s ?\t ?\n))))
    (cond
     ((and (eq (char-before) ?@)
           agent-shell-file-completion-enabled)
      (completion-at-point))
     ((and (eq (char-before) ?/)
           agent-shell-command-completion-enabled
           (agent-shell--command-completion-at-point))
      (completion-at-point)))))

(defun agent-shell-completion--setup ()
  "Offer @ and / completion via `completion-at-point' in the current buffer.

Also offers it while reading queued prompts.  Popping completion as @
or / are typed is left to `agent-shell-completion-mode', enabled here
unless `agent-shell-file-completion-enabled' and
`agent-shell-command-completion-enabled' are both nil."
  (add-hook 'completion-at-point-functions #'agent-shell--file-completion-at-point nil t)
  (add-hook 'completion-at-point-functions #'agent-shell--command-completion-at-point nil t)
  (add-hook 'agent-shell-prompt-queue-setup-minibuffer-functions
            #'agent-shell-completion--setup-queued-prompt)
  (when (or agent-shell-file-completion-enabled
            agent-shell-command-completion-enabled)
    (agent-shell-completion-mode +1)))

(defun agent-shell-completion--setup-minibuffer (shell-buffer)
  "Enable @ and / completion in the current minibuffer for SHELL-BUFFER.

@ always completes project files.  / completes available agent commands
when SHELL-BUFFER has received them via ACP; if not, / is a no-op.

Completion pops as they are typed only when SHELL-BUFFER has
`agent-shell-completion-mode' enabled, so user preference set in the
shell carries over."
  (when (buffer-live-p shell-buffer)
    (setq-local agent-shell-completion--shell-buffer shell-buffer)
    (add-hook 'completion-at-point-functions
              #'agent-shell--file-completion-at-point nil t)
    (add-hook 'completion-at-point-functions
              #'agent-shell--command-completion-at-point nil t)
    (when (buffer-local-value 'agent-shell-completion-mode shell-buffer)
      (add-hook 'post-self-insert-hook
                #'agent-shell--trigger-completion-at-point nil t))
    (add-hook 'minibuffer-exit-hook
              #'agent-shell-completion--cleanup-minibuffer nil t)))

(defun agent-shell-completion--cleanup-minibuffer ()
  "Remove `agent-shell' completion hooks from the minibuffer."
  (kill-local-variable 'agent-shell-completion--shell-buffer)
  (remove-hook 'completion-at-point-functions
               #'agent-shell--file-completion-at-point t)
  (remove-hook 'completion-at-point-functions
               #'agent-shell--command-completion-at-point t)
  (remove-hook 'post-self-insert-hook
               #'agent-shell--trigger-completion-at-point t)
  (remove-hook 'minibuffer-exit-hook
               #'agent-shell-completion--cleanup-minibuffer t))

(defun agent-shell-completion--setup-queued-prompt (event)
  "Enable completion in the minibuffer reading EVENT\\='s queued prompt.

Runs from `agent-shell-prompt-queue-setup-minibuffer-functions', so the
queue need not know completion exists.

EVENT is an alist as that hook documents, for example:

  \\='((:shell-buffer . #<buffer Claude Agent @ agent-shell>))"
  (when-let* ((shell-buffer (map-elt event :shell-buffer)))
    (agent-shell-completion--setup-minibuffer shell-buffer)))

(define-minor-mode agent-shell-completion-mode
  "Toggle popping completion as @ or / are typed.

With the mode off, \\[completion-at-point] still completes them."
  :lighter " @/Compl"
  (if agent-shell-completion-mode
      (add-hook 'post-self-insert-hook #'agent-shell--trigger-completion-at-point nil t)
    (remove-hook 'post-self-insert-hook #'agent-shell--trigger-completion-at-point t)))

(provide 'agent-shell-completion)

;;; agent-shell-completion.el ends here
