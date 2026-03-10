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
;; This file includes Cursor-specific configurations.
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

(cl-defun agent-shell-cursor-make-authentication (&key api-key login)
  "Create Cursor authentication configuration.

API-KEY is the Cursor API key string or function that returns it.
When set, the key is injected into the CLI as --api-key per
https://cursor.com/docs/cli/acp.
LOGIN when non-nil indicates to use login-based authentication."
  (when (> (seq-count #'identity (list api-key login)) 1)
    (error "Cannot specify multiple authentication methods - choose one"))
  (unless (> (seq-count #'identity (list api-key login)) 0)
    (error "Must specify one of :api-key, :login"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (login `((:login . t)))))

(defcustom agent-shell-cursor-authentication
  (agent-shell-cursor-make-authentication :login t)
  "Configuration for Cursor authentication.

For login-based authentication (default, run \"agent login\" first):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :login t))

For API key (injected into CLI as --api-key):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :api-key \"your-key\"))

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :api-key (lambda () (getenv \"CURSOR_API_KEY\"))))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-cursor-acp-command
  '("cursor-agent-acp")
  "Command and parameters for the Cursor agent client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-cursor-default-session-mode-id
  nil
  "Default Cursor session mode ID.

Must be one of the session ID's displayed under \"Available modes\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-cursor-environment
  nil
  "Environment variables for the Cursor agent client.

This should be a list of environment variables to be used when
starting the Cursor agent process."
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
   :needs-authentication t
   :authenticate-request-maker (lambda ()
                                 (acp-make-authenticate-request :method-id "cursor_login"))
   :default-session-mode-id (lambda () agent-shell-cursor-default-session-mode-id)
   :install-instructions "See https://cursor.com/docs/cli/acp for installation"))

(defun agent-shell-cursor-start-agent ()
  "Start an interactive Cursor agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-cursor-make-agent-config)
                     :new-shell t))

(defun agent-shell-cursor-api-key ()
  "Get the Cursor API key from `agent-shell-cursor-authentication'."
  (cond ((stringp (map-elt agent-shell-cursor-authentication :api-key))
         (map-elt agent-shell-cursor-authentication :api-key))
        ((functionp (map-elt agent-shell-cursor-authentication :api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-cursor-authentication :api-key))
           (error
            (error "Cursor API key not found.  Check `agent-shell-cursor-authentication'"))))
        (t
         nil)))

(cl-defun agent-shell-cursor-make-client (&key buffer)
  "Create a Cursor agent ACP client with BUFFER as context.

When :api-key is set in `agent-shell-cursor-make-authentication', injects
--api-key and the key into the CLI command per https://cursor.com/docs/cli/acp."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-cursor-command) agent-shell-cursor-command)
    (user-error "Please migrate to use agent-shell-cursor-acp-command and eval (setq agent-shell-cursor-command nil)"))
  (cond
   ((map-elt agent-shell-cursor-authentication :api-key)
    (let ((api-key (agent-shell-cursor-api-key)))
      (unless api-key
        (user-error "Please set your `agent-shell-cursor-authentication' with :api-key"))
      (let ((command-list (append (list (car agent-shell-cursor-acp-command) "--api-key" api-key)
                                  (cdr agent-shell-cursor-acp-command))))
        (agent-shell--make-acp-client :command (car command-list)
                                      :command-params (cdr command-list)
                                      :environment-variables agent-shell-cursor-environment
                                      :context-buffer buffer))))
   ((map-elt agent-shell-cursor-authentication :login)
    (agent-shell--make-acp-client :command (car agent-shell-cursor-acp-command)
                                  :command-params (cdr agent-shell-cursor-acp-command)
                                  :environment-variables agent-shell-cursor-environment
                                  :context-buffer buffer))
   (t
    (error "Invalid authentication configuration.  Set `agent-shell-cursor-authentication'"))))

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
