;;; agent-shell-mistral.el --- Mistral AI agent configurations -*- lexical-binding: t; -*-

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
;; This file includes Mistral AI-specific configurations.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-welcome-message "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(cl-defun agent-shell-mistral-make-authentication (&key api-key)
  "Create Mistral AI authentication configuration.

API-KEY is the Mistral AI API key string or function that returns it."
  (unless api-key
    (error "Must specify :api-key"))
  `((:api-key . ,api-key)))

(defcustom agent-shell-mistral-authentication
  nil
  "Configuration for Mistral AI authentication.
For API key (string):

  (setq agent-shell-mistral-authentication
        (agent-shell-mistral-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-mistral-authentication
        (agent-shell-mistral-make-authentication :api-key (lambda () ...)))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-mistral-default-model-id
  nil
  "Default Mistral AI model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-mistral-default-session-mode-id
  nil
  "Default Mistral AI session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-mistral-acp-command
  '("vibe-acp")
  "Command and parameters for the Mistral Vibe client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-mistral-environment
  nil
  "Environment variables for the Mistral Vibe client.

This should be a list of environment variables to be used when
starting the Mistral Vibe client process.

Example usage to set custom environment variables:

  (setq agent-shell-mistral-environment
        (`agent-shell-make-environment-variables'
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-mistral-make-config ()
  "Create a Mistral Vibe agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'mistral-vibe
   :mode-line-name "Mistral Vibe"
   :buffer-name "Mistral Vibe"
   :shell-prompt "Vibe> "
   :shell-prompt-regexp "Vibe> "
   :icon-name "mistral.png"
   :welcome-function #'agent-shell-mistral--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-mistral-make-client :buffer buffer))
   :default-model-id (lambda () agent-shell-mistral-default-model-id)
   :default-session-mode-id (lambda () agent-shell-mistral-default-session-mode-id)
   :install-instructions "See https://github.com/mistralai/vibe-acp for installation."))

(defun agent-shell-mistral-start-vibe ()
  "Start an interactive Mistral Vibe agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-mistral-make-config)
                     :new-shell t))

(cl-defun agent-shell-mistral-make-client (&key buffer)
  "Create a Mistral Vibe ACP client with BUFFER as context.

See `agent-shell-mistral-authentication' for authentication configuration."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-mistral-command) agent-shell-mistral-command)
    (user-error "Please migrate to use agent-shell-mistral-acp-command and eval (setq agent-shell-mistral-command nil)"))
  (unless agent-shell-mistral-authentication
    (user-error "Please set `agent-shell-mistral-authentication' with your API key"))
  (let ((api-key (agent-shell-mistral-key)))
    (unless api-key
      (user-error "Please set your `agent-shell-mistral-authentication'"))
    (agent-shell--make-acp-client :command (car agent-shell-mistral-acp-command)
                                  :command-params (cdr agent-shell-mistral-acp-command)
                                  :environment-variables (append (list (format "MISTRAL_API_KEY=%s" api-key))
                                                                 agent-shell-mistral-environment)
                                  :context-buffer buffer)))

(defun agent-shell-mistral-key ()
  "Get the Mistral AI API key."
  (cond ((stringp (map-elt agent-shell-mistral-authentication :api-key))
         (map-elt agent-shell-mistral-authentication :api-key))
        ((functionp (map-elt agent-shell-mistral-authentication :api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-mistral-authentication :api-key))
           (error
            (error "API key not found.  Check out `agent-shell-mistral-authentication'"))))
        (t
         nil)))

(defun agent-shell-mistral--welcome-message (config)
  "Return Mistral Vibe welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-mistral--ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-mistral--ascii-art ()
  "Mistral Vibe ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
 ███╗   ███╗ ██╗ ███████╗ ████████╗ ██████╗   █████╗  ██╗
 ████╗ ████║ ██║ ██╔════╝ ╚══██╔══╝ ██╔══██╗ ██╔══██╗ ██║
 ██╔████╔██║ ██║ ███████╗    ██║    ██████╔╝ ███████║ ██║
 ██║╚██╔╝██║ ██║ ╚════██║    ██║    ██╔══██╗ ██╔══██║ ██║
 ██║ ╚═╝ ██║ ██║ ███████║    ██║    ██║  ██║ ██║  ██║ ███████╗
 ╚═╝     ╚═╝ ╚═╝ ╚══════╝    ╚═╝    ╚═╝  ╚═╝ ╚═╝  ╚═╝ ╚══════╝
 ██╗   ██╗ ██╗ ██████╗  ███████╗
 ██║   ██║ ██║ ██╔══██╗ ██╔════╝
 ██║   ██║ ██║ ██████╔╝ █████╗
 ╚██╗ ██╔╝ ██║ ██╔══██╗ ██╔══╝
  ╚████╔╝  ██║ ██████╔╝ ███████╗
   ╚═══╝   ╚═╝ ╚═════╝  ╚══════╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#ff7000" :inherit fixed-pitch)
                                       '(:foreground "#ff5500" :inherit fixed-pitch)))))

(provide 'agent-shell-mistral)

;;; agent-shell-mistral.el ends here
