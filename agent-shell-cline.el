;;; agent-shell-cline.el --- Cline agent configurations -*- lexical-binding: t; -*-

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
;; This file includes Cline-specific configurations.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-welcome-message "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(defcustom agent-shell-cline-acp-command
  '("cline" "--acp")
  "Command and parameters for the Cline agent client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-cline-default-model-id
  nil
  "Default Cline model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell.

Can be set to either a string or a function that returns a string."
  :type '(choice (const nil) string function)
  :group 'agent-shell)

(defcustom agent-shell-cline-default-session-mode-id
  nil
  "Default Cline session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-cline-environment
  nil
  "Environment variables for the Cline agent client.

This should be a list of environment variables to be used when
starting the Cline agent process."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-cline-make-agent-config ()
  "Create a Cline agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'cline
   :mode-line-name "Cline"
   :buffer-name "Cline"
   :shell-prompt "Cline> "
   :shell-prompt-regexp "Cline> "
   :icon-name "cline.png"
   :welcome-function #'agent-shell-cline--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-cline-make-client :buffer buffer))
   :default-model-id (lambda () (if (functionp agent-shell-cline-default-model-id)
                                    (funcall agent-shell-cline-default-model-id)
                                  agent-shell-cline-default-model-id))
   :default-session-mode-id (lambda () agent-shell-cline-default-session-mode-id)
   :install-instructions "See https://cline.bot/cli for installation."))

;;;###autoload
(defun agent-shell-cline-start-agent ()
  "Start an interactive Cline agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-cline-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-cline-make-client (&key buffer)
  "Create a Cline agent ACP client with BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-cline-acp-command)
                                :command-params (cdr agent-shell-cline-acp-command)
                                :environment-variables agent-shell-cline-environment
                                :context-buffer buffer))

(defun agent-shell-cline--welcome-message (config)
  "Return Cline welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-cline--ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-cline--ascii-art ()
  "Cline ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
  ██████╗ ██╗     ██╗ ███╗   ██╗ ███████╗
 ██╔════╝ ██║     ██║ ████╗  ██║ ██╔════╝
 ██║      ██║     ██║ ██╔██╗ ██║ █████╗
 ██║      ██║     ██║ ██║╚██╗██║ ██╔══╝
 ╚██████╗ ███████╗██║ ██║ ╚████║ ███████╗
  ╚═════╝ ╚══════╝╚═╝ ╚═╝  ╚═══╝ ╚══════╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#e0a050" :inherit fixed-pitch)
                                       '(:foreground "#b07830" :inherit fixed-pitch)))))

(provide 'agent-shell-cline)

;;; agent-shell-cline.el ends here
