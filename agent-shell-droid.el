;;; agent-shell-droid.el --- Factory Droid agent configurations -*- lexical-binding: t; -*-

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
;; This file includes Factory Droid specific configurations relying on the
;; droid: https://docs.factory.ai/cli/getting-started/quickstart
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(cl-defun agent-shell-droid-make-authentication (&key api-key none)
  "Create Factory Droid authentication configuration.

API-KEY is the Droid API key string or function that returns it.
NONE when non-nil disables API key authentication (e.g., when droid-acp
is already logged in or uses an alternative auth flow).

Only one of API-KEY or NONE should be provided, never both."
  (when (and api-key none)
    (error "Cannot specify both :api-key and :none - choose one"))
  (unless (or api-key none)
    (error "Must specify either :api-key or :none"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (none `((:none . t)))))

(defcustom agent-shell-droid-authentication
  (agent-shell-droid-make-authentication :none t)
  "Configuration for Factory Droid authentication.
For API key (string):

  (setq agent-shell-droid-authentication
        (agent-shell-droid-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-droid-authentication
        (agent-shell-droid-make-authentication :api-key (lambda () ...)))

For no authentication (e.g., using `droid-acp` built-in login):

  (setq agent-shell-droid-authentication
        (agent-shell-droid-make-authentication :none t))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-droid-default-model-id
  nil
  "Default Droid AI model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-droid-default-session-mode-id
  nil
  "Default Droid AI session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-droid-default-reasoning-effort
  nil
  "Default reasoning effort.

It can be one of the followings.
none, dynamic, off, minimal, low, medium, high, xhigh, max"
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-droid-acp-command
  '("droid" "exec" "--output-format" "acp")
  "Command and parameters for the Factory Droid ACP client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-droid-environment
  nil
  "Environment variables for the Factory Droid ACP client.

This should be a list of environment variables to be used when
starting the Droid client process.

Example usage to set custom environment variables:

  (setq agent-shell-droid-environment
        (`agent-shell-make-environment-variables'
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-droid-make-agent-config ()
  "Create a Factory Droid agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'droid
   :mode-line-name "Droid"
   :buffer-name "Droid"
   :shell-prompt "Droid> "
   :shell-prompt-regexp "Droid> "
   :icon-name "https://avatars.githubusercontent.com/u/131064358"
   :welcome-function #'agent-shell-droid--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-droid-make-client :buffer buffer))
   :default-model-id (lambda () agent-shell-droid-default-model-id)
   :default-session-mode-id (lambda () agent-shell-droid-default-session-mode-id)
   :install-instructions "See https://docs.factory.ai/cli/getting-started/quickstart for installation."))

(defun agent-shell-droid-start-agent ()
  "Start an interactive Factory Droid agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-droid-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-droid-make-client (&key buffer)
  "Create a Factory Droid client using BUFFER as context.

Uses `agent-shell-droid-authentication' for authentication configuration."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-droid-command) agent-shell-droid-command)
    (user-error "Please migrate to use agent-shell-droid-acp-command and eval (setq agent-shell-droid-command nil)"))
  (let ((api-key (agent-shell-droid-key)))
    (agent-shell--make-acp-client :command (car agent-shell-droid-acp-command)
                                  :command-params (append (seq-rest agent-shell-droid-acp-command)
                                                          (when agent-shell-droid-default-reasoning-effort
                                                            (list "--reasoning-effort"
                                                                  agent-shell-droid-default-reasoning-effort)))
                                  :environment-variables (append (cond ((map-elt agent-shell-droid-authentication :none)
                                                                        nil)
                                                                       (api-key
                                                                        (list (format "FACTORY_API_KEY=%s" api-key)))
                                                                       (t
                                                                        (error "Missing Factory Droid authentication (see agent-shell-droid-authentication)")))
                                                                 agent-shell-droid-environment)
                                  :context-buffer buffer)))

(defun agent-shell-droid-key ()
  "Get the Factory Droid API key."
  (cond ((stringp (map-elt agent-shell-droid-authentication :api-key))
         (map-elt agent-shell-droid-authentication :api-key))
        ((functionp (map-elt agent-shell-droid-authentication :api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-droid-authentication :api-key))
           (error
            (error "API key not found.  Check out `agent-shell-droid-authentication'"))))
        (t
         nil)))

(defun agent-shell-droid--welcome-message (config)
  "Return Factory Droid welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-droid--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-droid--ascii-art ()
  "Factory Droid ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
░░░░░░░░░    ░░░░░░░░░     ░░░░░░░░    ░░░   ░░░░░░░░░
░░░    ░░░   ░░░    ░░░   ░░░    ░░░   ░░░   ░░░    ░░░
░░░    ░░░   ░░░    ░░░   ░░░    ░░░   ░░░   ░░░    ░░░
░░░    ░░░   ░░░░░░░░░    ░░░    ░░░   ░░░   ░░░    ░░░
░░░    ░░░   ░░░    ░░░   ░░░    ░░░   ░░░   ░░░    ░░░
░░░    ░░░   ░░░    ░░░   ░░░    ░░░   ░░░   ░░░    ░░░
░░░░░░░░░    ░░░    ░░░    ░░░░░░░░    ░░░   ░░░░░░░░░
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#8b949e" :inherit fixed-pitch)
                                       '(:foreground "#444" :inherit fixed-pitch)))))

(provide 'agent-shell-droid)

;;; agent-shell-droid.el ends here
