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

(require 'map)
(require 'agent-shell-project)

(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--project-files "agent-shell-project")

(defvar agent-shell--state)

(defcustom agent-shell-file-completion-enabled t
  "Non-nil automatically enables file completion when starting shells."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-file-completion-function
  #'agent-shell--file-completion-at-point
  "Function to call for @ file completion.
Called with no arguments when @ is typed at a word boundary.
If it returns non-nil, `completion-at-point' is not called.
If it returns nil, falls through to the standard CAPF."
  :type 'function
  :group 'agent-shell)

(defun agent-shell--completion-bounds (char-class trigger-char)
  "Find completion bounds for CHAR-CLASS, if TRIGGER-CHAR precedes them.
Returns alist with :start and :end if TRIGGER-CHAR is found before
the word, nil otherwise."
  (save-excursion
    (when-let* ((end (progn (skip-chars-forward char-class) (point)))
                (start (progn (skip-chars-backward char-class) (point)))
                ((eq (char-before start) trigger-char)))
      `((:start . ,start) (:end . ,end)))))

(defun agent-shell--capf-exit-with-space (_string _status)
  "Insert space after completion."
  (insert " "))

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
          :company-kind (lambda (f) (if (string-suffix-p "/" f) 'folder 'file))
          :exit-function #'agent-shell--capf-exit-with-space)))

(defun agent-shell--command-completion-at-point ()
  "Complete available commands after /."
  (when-let* ((bounds (agent-shell--completion-bounds "[:alnum:]_-" ?/))
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
          :annotation-function
          (lambda (name)
            (when-let* ((desc (map-elt descriptions name)))
              (concat "  " desc)))
          :company-kind (lambda (_) 'function)
          :exit-function #'agent-shell--capf-exit-with-space)))

(defun agent-shell--trigger-completion-at-point ()
  "Trigger completion when @ or / is typed at a word boundary.
Only triggers when the character is at line start or after whitespace,
preventing spurious completions mid-word or in paths."
  (when (and (memq (char-before) '(?@ ?/))
             (or (= (point) (1+ (line-beginning-position)))
                 (memq (char-before (1- (point))) '(?\s ?\t ?\n))))
    (cond
     ((eq (char-before) ?@)
      (unless (funcall agent-shell-file-completion-function)
        (completion-at-point)))
     ((and (eq (char-before) ?/)
           (agent-shell--command-completion-at-point))
      (completion-at-point)))))

(defun agent-shell-completion--setup-minibuffer (shell-buffer)
  "Enable @ and / completion in the current minibuffer for SHELL-BUFFER.

@ always completes project files.  / completes available agent commands
when SHELL-BUFFER has received them via ACP; if not, / is a no-op.

No-ops when SHELL-BUFFER does not have `agent-shell-completion-mode'
enabled, so user preference set in the shell carries over."
  (when (and (buffer-live-p shell-buffer)
             (buffer-local-value 'agent-shell-completion-mode shell-buffer))
    (setq-local agent-shell-completion--shell-buffer shell-buffer)
    (add-hook 'completion-at-point-functions
              #'agent-shell--file-completion-at-point nil t)
    (add-hook 'completion-at-point-functions
              #'agent-shell--command-completion-at-point nil t)
    (add-hook 'post-self-insert-hook
              #'agent-shell--trigger-completion-at-point nil t)
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

(define-minor-mode agent-shell-completion-mode
  "Toggle agent shell completion with @ or / prefix."
  :lighter " @/Compl"
  (if agent-shell-completion-mode
      (progn
        (add-hook 'completion-at-point-functions #'agent-shell--file-completion-at-point nil t)
        (add-hook 'completion-at-point-functions #'agent-shell--command-completion-at-point nil t)
        (add-hook 'post-self-insert-hook #'agent-shell--trigger-completion-at-point nil t))
    (remove-hook 'completion-at-point-functions #'agent-shell--file-completion-at-point t)
    (remove-hook 'completion-at-point-functions #'agent-shell--command-completion-at-point t)
    (remove-hook 'post-self-insert-hook #'agent-shell--trigger-completion-at-point t)))

(provide 'agent-shell-completion)

;;; agent-shell-completion.el ends here
