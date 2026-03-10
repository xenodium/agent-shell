;;; agent-shell-cursor.el --- Cursor agent configurations -*- lexical-binding: t; -*-

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
;; This file includes Cursor-specific configurations using the official
;; Cursor CLI (agent acp) for ACP (Agent Client Protocol) communication.
;;
;; See https://cursor.com/docs/cli/acp for more information about
;; Cursor's official ACP implementation.
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

(cl-defun agent-shell-cursor-make-authentication (&key api-key auth-token login none)
  "Create Cursor authentication configuration.

API-KEY is the Cursor API key string or function that returns it.
AUTH-TOKEN is the Cursor auth token string or function that returns it.
LOGIN when non-nil indicates to use cursor_login authentication.
NONE when non-nil indicates no authentication method is used.

Only one of API-KEY, AUTH-TOKEN, LOGIN, or NONE should be provided."
  (when (> (seq-count #'identity (list api-key auth-token login)) 1)
    (error "Cannot specify multiple authentication methods - choose one"))
  (unless (> (seq-count #'identity (list api-key auth-token login none)) 0)
    (error "Must specify one of :api-key, :auth-token, :login, or :none"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (auth-token `((:auth-token . ,auth-token)))
   (login `((:login . t)))
   (none `((:none . t)))))

(defcustom agent-shell-cursor-authentication
  (agent-shell-cursor-make-authentication :login t)
  "Configuration for Cursor authentication.

For login-based authentication (default):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :login t))

For API key (string):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :api-key (lambda () ...)))

For auth token (string):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :auth-token \"your-token\"))

For auth token (function):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :auth-token (lambda () ...)))

For no authentication (already authenticated externally):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :none t))

When using :api-key or :auth-token, the corresponding environment
variable (CURSOR_API_KEY or CURSOR_AUTH_TOKEN) will be set automatically."
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-cursor-acp-command
  '("agent" "acp")
  "Command and parameters for the Cursor agent client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-cursor-environment
  nil
  "Environment variables for the Cursor agent client.

This should be a list of environment variables to be used when
starting the Cursor agent process.

Cursor CLI supports the following authentication environment variables:
  - CURSOR_API_KEY: API key for authentication
  - CURSOR_AUTH_TOKEN: Auth token for authentication

These can be used for pre-authentication before the ACP session starts.
If not set, the authentication method specified in
`agent-shell-cursor-authentication' will be used."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-cursor-make-agent-config ()
  "Create a Cursor agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'cursor
   :mode-line-name "Cursor"
   :buffer-name "Cursor"
   :shell-prompt "Cursor> "
   :shell-prompt-regexp "Cursor> "
   :icon-name "cursor.png"
   :welcome-function #'agent-shell-cursor--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-cursor-make-client :buffer buffer))
   :needs-authentication (not (map-elt agent-shell-cursor-authentication :none))
   :authenticate-request-maker (lambda ()
                                 (when (not (map-elt agent-shell-cursor-authentication :none))
                                   (acp-make-authenticate-request
                                    :method-id "cursor_login")))
   :install-instructions "See https://cursor.com/docs/cli for installation."))

(defun agent-shell-cursor-start-agent ()
  "Start an interactive Cursor agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-cursor-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-cursor-make-client (&key buffer)
  "Create a Cursor agent ACP client with BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-cursor-command) agent-shell-cursor-command)
    (user-error "Please migrate to use agent-shell-cursor-acp-command and eval (setq agent-shell-cursor-command nil)"))
  (let* ((api-key-value (map-elt agent-shell-cursor-authentication :api-key))
         (auth-token-value (map-elt agent-shell-cursor-authentication :auth-token))
         (env-vars-overrides
          (cond
           (api-key-value
            (list (format "CURSOR_API_KEY=%s"
                          (if (functionp api-key-value)
                              (funcall api-key-value)
                            api-key-value))))
           (auth-token-value
            (list (format "CURSOR_AUTH_TOKEN=%s"
                          (if (functionp auth-token-value)
                              (funcall auth-token-value)
                            auth-token-value))))
           ((map-elt agent-shell-cursor-authentication :login)
            ;; Set empty API key to force login flow if not already authenticated
            (list "CURSOR_API_KEY="))
           (t nil))))
    (agent-shell--make-acp-client :command (car agent-shell-cursor-acp-command)
                                  :command-params (cdr agent-shell-cursor-acp-command)
                                  :environment-variables (append env-vars-overrides
                                                                 agent-shell-cursor-environment)
                                  :context-buffer buffer)))

(defun agent-shell-cursor--welcome-message (config)
  "Return Cursor welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-cursor--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-cursor--ascii-art ()
  "Cursor ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
  ██████╗ ██╗   ██╗ ██████╗  ███████╗  ██████╗  ██████╗
 ██╔════╝ ██║   ██║ ██╔══██╗ ██╔════╝ ██╔═══██╗ ██╔══██╗
 ██║      ██║   ██║ ██████╔╝ ███████╗ ██║   ██║ ██████╔╝
 ██║      ██║   ██║ ██╔══██╗ ╚════██║ ██║   ██║ ██╔══██╗
 ╚██████╗ ╚██████╔╝ ██║  ██║ ███████║ ╚██████╔╝ ██║  ██║
  ╚═════╝  ╚═════╝  ╚═╝  ╚═╝ ╚══════╝  ╚═════╝  ╚═╝  ╚═╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#00d4ff" :inherit fixed-pitch)
                                       '(:foreground "#0066cc" :inherit fixed-pitch)))))

(provide 'agent-shell-cursor)

;;; agent-shell-cursor.el ends here
