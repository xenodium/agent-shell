;;; agent-shell-openclaw.el --- OpenClaw agent configurations -*- lexical-binding: t; -*-

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
;; This file includes OpenClaw-specific configurations.
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

(defcustom agent-shell-openclaw-acp-command
  '("openclaw" "acp")
  "Command and parameters for the OpenClaw ACP bridge.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-openclaw-default-model-id
  nil
  "Default OpenClaw model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-openclaw-environment
  nil
  "Environment variables for the OpenClaw client.

This should be a list of environment variables to be used when
starting the OpenClaw client process.

Example usage to set custom environment variables:

  (setq agent-shell-openclaw-environment
        (`agent-shell-make-environment-variables'
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-openclaw-make-agent-config ()
  "Create an OpenClaw agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'openclaw
   :mode-line-name "OpenClaw"
   :buffer-name "OpenClaw"
   :shell-prompt "OpenClaw> "
   :shell-prompt-regexp "OpenClaw> "
   :welcome-function #'agent-shell-openclaw--welcome-message
   :icon-name "openclaw.png"
   :client-maker (lambda (buffer)
                   (agent-shell-openclaw-make-client :buffer buffer))
   :default-model-id (lambda () agent-shell-openclaw-default-model-id)
   :install-instructions "See https://docs.openclaw.ai/start/getting-started for installation."))

;;;###autoload
(defun agent-shell-openclaw-start-agent ()
  "Start an interactive OpenClaw agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-openclaw-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-openclaw-make-client (&key buffer)
  "Create an OpenClaw ACP client using BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-openclaw-acp-command)
                                :command-params (cdr agent-shell-openclaw-acp-command)
                                :environment-variables agent-shell-openclaw-environment
                                :context-buffer buffer))

(defun agent-shell-openclaw--welcome-message (config)
  "Return OpenClaw welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-openclaw--ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-openclaw--ascii-art ()
  "OpenClaw ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
 ██████╗  ██████╗  ███████╗ ███╗   ██╗  ██████╗ ██╗       █████╗  ██╗    ██╗
██╔═══██╗ ██╔══██╗ ██╔════╝ ████╗  ██║ ██╔════╝ ██║      ██╔══██╗ ██║    ██║
██║   ██║ ██████╔╝ █████╗   ██╔██╗ ██║ ██║      ██║      ███████║ ██║ █╗ ██║
██║   ██║ ██╔═══╝  ██╔══╝   ██║╚██╗██║ ██║      ██║      ██╔══██║ ██║███╗██║
╚██████╔╝ ██║      ███████╗ ██║ ╚████║ ╚██████╗ ███████╗ ██║  ██║ ╚███╔███╔╝
 ╚═════╝  ╚═╝      ╚══════╝ ╚═╝  ╚═══╝  ╚═════╝ ╚══════╝ ╚═╝  ╚═╝  ╚══╝╚══╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#FF5A2D" :inherit fixed-pitch)
                                       '(:foreground "#D14A22" :inherit fixed-pitch)))))

(provide 'agent-shell-openclaw)

;;; agent-shell-openclaw.el ends here
