;;; agent-shell-devcontainer.el --- Devcontainer support -*- lexical-binding: t; -*-

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
;; This file includes support for running agents in Devcontainers.
;;

;;; Code:

(declare-function agent-shell-cwd "agent-shell")

(defun agent-shell-devcontainer--get-workspace-path (cwd)
  "Return devcontainer workspaceFolder for CWD, or default value if none found.

See https://containers.dev for more information on devcontainers."
  (let ((devcontainer-config-file-name (expand-file-name ".devcontainer/devcontainer.json" cwd)))
    (condition-case _err
        (map-elt (json-read-file devcontainer-config-file-name) 'workspaceFolder
                 (concat "/workspaces/" (file-name-nondirectory (directory-file-name cwd)) "/"))
      (file-missing (error "Not found: %s" devcontainer-config-file-name))
      (permission-denied (error "Not readable: %s" devcontainer-config-file-name))
      (json-string-format (error "No valid JSON: %s" devcontainer-config-file-name)))))

(defun agent-shell-devcontainer-resolve-path (path)
  "Resolve PATH from a devcontainer in the local filesystem, and vice versa.

When the current working directory has no .devcontainer/devcontainer.json,
PATH is returned unchanged.  This makes it safe to set this function as
`agent-shell-path-resolver-function' globally, even when mixing
devcontainer- and non-devcontainer projects in the same Emacs session.

For example:

- /workspace/README.md => /home/xenodium/projects/kitchen-sink/README.md
- /home/xenodium/projects/kitchen-sink/README.md => /workspace/README.md
- /any/path => /any/path  (when CWD is not a devcontainer project)"
  (let ((cwd (agent-shell-cwd)))
    (if (not (file-exists-p (expand-file-name ".devcontainer/devcontainer.json" cwd)))
        path
      (let ((devcontainer-path (agent-shell-devcontainer--get-workspace-path cwd)))
        (if (string-prefix-p cwd path)
            (string-replace cwd devcontainer-path path)
          (if agent-shell-text-file-capabilities
              (if-let* ((is-dev-container (string-prefix-p devcontainer-path path))
                        (local-path (expand-file-name (string-replace devcontainer-path cwd path))))
                  (or
                   (and (file-in-directory-p local-path cwd) local-path)
                   (error "Resolves to path outside of working directory: %s" path))
                (error "Unexpected path outside of workspace folder: %s" path))
            (error "Refuse to resolve to local filesystem with text file capabilities disabled: %s" path)))))))

(defalias
  'agent-shell--resolve-devcontainer-path
  #'agent-shell-devcontainer-resolve-path)

(provide 'agent-shell-devcontainer)

;;; agent-shell-devcontainer.el ends here
