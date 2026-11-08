;;; agent-shell-omp.el --- Oh My Pi (omp) agent configurations -*- lexical-binding: t; -*-

;; Author: Sam Kleinman https://tychoish.com
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
;; This file includes Oh My Pi (omp) agent-specific configurations for
;; agent-shell.
;;
;; This integration calls `omp acp' as the ACP client command.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-welcome-message "agent-shell")
(declare-function agent-shell--interpolate-gradient "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(defcustom agent-shell-omp-acp-command
  '("omp" "acp")
  "Command and parameters for the Oh My Pi (omp) ACP client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-omp-environment
  nil
  "Environment variables for the Oh My Pi (omp) client.

This should be a list of environment variables to be used when
starting the omp client process.

Example usage:

  (setq agent-shell-omp-environment
        (agent-shell-make-environment-variables
         \"<KEY>\" \"<VALUE>\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-omp-make-agent-config ()
  "Create an Oh My Pi (omp) agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'omp
   :mode-line-name "OMP"
   :buffer-name "OMP"
   :shell-prompt "OMP> "
   :shell-prompt-regexp "OMP> "
   :welcome-function #'agent-shell-omp--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-omp-make-client :buffer buffer))
   :install-instructions "See https://github.com/can1357/oh-my-pi omp (Oh My Pi) installation."))

(defun agent-shell-omp-start ()
  "Start an interactive Oh My Pi (omp) agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-omp-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-omp-make-client (&key buffer)
  "Create an Oh My Pi (omp) ACP client with BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-omp-acp-command)
                                :command-params (cdr agent-shell-omp-acp-command)
                                :environment-variables agent-shell-omp-environment
                                :context-buffer buffer))

(defun agent-shell-omp--welcome-message (config)
  "Return OMP welcome message using `shell-maker' CONFIG."
  (concat "\n\n"
          (agent-shell--indent-string 4 (agent-shell-omp--ascii-art))
          "\n\n"
          (agent-shell-welcome-message config)))

(defun agent-shell-omp--ascii-art ()
  "Oh My Pi ASCII art with gradient coloring."
  (let* ((text (string-trim "
  ██████╗ ██╗  ██╗     ███╗   ███╗ ██╗   ██╗      ██████╗ ██╗
 ██╔═══██╗██║  ██║     ████╗ ████║ ╚██╗ ██╔╝     ██╔═══██╗██║
 ██║   ██║███████║     ██╔████╔██║  ╚████╔╝█████╗ ██████╔╝██║
 ██║   ██║██╔══██║     ██║╚██╔╝██║   ╚██╔╝ ╚════╝ ██╔═══╝ ██║
 ╚██████╔╝██║  ██║     ██║ ╚═╝ ██║    ██║         ██║     ██║
  ╚═════╝ ╚═╝  ╚═╝     ╚═╝     ╚═╝    ╚═╝         ╚═╝     ╚═╝
" "\n"))
         (is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (gradient-colors (if is-dark
                              '("#7aa2f7" "#bb9af7" "#f7768e")
                            '("#3d6fd6" "#9b6dce" "#d6456e")))
         (lines (split-string text "\n"))
         (result ""))
    (seq-do (lambda (line)
              (let ((line-length (length line))
                    (propertized-line ""))
                (dotimes (i line-length)
                  (let* ((char (substring line i (1+ i)))
                         (progress (/ (float i) (max line-length 1)))
                         (color (agent-shell--interpolate-gradient gradient-colors progress)))
                    (setq propertized-line
                          (concat propertized-line
                                  (propertize char 'font-lock-face
                                              `(:foreground ,color :inherit fixed-pitch))))))
                (setq result (concat result propertized-line "\n"))))
            lines)
    (string-trim-right result)))

(provide 'agent-shell-omp)

;;; agent-shell-omp.el ends here
