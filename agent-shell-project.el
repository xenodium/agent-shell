;;; agent-shell-project.el --- Project file utilities for agent-shell. -*- lexical-binding: t; -*-

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

(require 'project)
(require 'subr-x)

(defvar projectile-mode)

(declare-function projectile-project-p "projectile")
(declare-function projectile-project-root "projectile")
(declare-function projectile-project-name "projectile")
(declare-function projectile-current-project-files "projectile")

(defun agent-shell--project-files ()
  "Get project files using projectile or project.el."
  (cond
   ((and (boundp 'projectile-mode)
         projectile-mode
         (projectile-project-p))
    (let ((root (file-name-as-directory (expand-file-name (projectile-project-root)))))
      (mapcar (lambda (f)
                (string-remove-prefix root f))
              (projectile-current-project-files))))
   ((fboundp 'project-current)
    (when-let* ((proj (project-current))
                (root (file-name-as-directory (expand-file-name (project-root proj)))))
      (mapcar (lambda (f)
                (string-remove-prefix root f))
              (project-files proj))))
   (t nil)))

(defcustom agent-shell-cwd-function nil
  "When non-nil, a function called to determine the shell's CWD.

The function receives no arguments and should return a directory path.

When nil (the default), `agent-shell-cwd' uses project root detection."
  :type '(choice (const :tag "Default (project root)" nil)
                 (function :tag "Custom function"))
  :group 'agent-shell)

(defun agent-shell-cwd ()
  "Return the CWD for this shell.

If `agent-shell-cwd-function' is set, use it.
Otherwise, if in a project, use project root."
  (expand-file-name
   (or (when agent-shell-cwd-function
         (funcall agent-shell-cwd-function))
       (when (and (boundp 'projectile-mode)
                  projectile-mode
                  (fboundp 'projectile-project-root))
         (projectile-project-root))
       (when (fboundp 'project-root)
         (when-let* ((proj (project-current)))
           (project-root proj)))
       default-directory
       (error "No CWD available"))))

(defun agent-shell--project-name ()
  "Return the project name for this shell.

If in a project, use project name."
  (or (when-let* (((boundp 'projectile-mode))
                 projectile-mode
                 ((fboundp 'projectile-project-name))
                 (root (projectile-project-root)))
        (projectile-project-name root))
      (when-let* (((fboundp 'project-name))
                 (project (project-current)))
        (project-name project))
      (file-name-nondirectory
       (string-remove-suffix "/" default-directory))))

(provide 'agent-shell-project)

;;; agent-shell-project.el ends here
