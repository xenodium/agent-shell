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
(require 'json)

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

(defcustom agent-shell-cursor--todos-icon "☑️"
  "Icon displayed during the AI's thought process.

You may use \"􁷘\" as an SF Symbol on macOS."
  :type 'string
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
   :install-instructions "See https://cursor.com/docs/cli/acp for installation"
   :request-handlers '(("_cursor/create_plan" . agent-shell-cursor--on-create-plan))))

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

(defun agent-shell-cursor--format-todos-as-markdown (todos)
  "Format TODOS list (from cursor/update_todos params) as human-readable markdown.

Each todo has id, content, status.  Status: pending, in_progress, completed."
  (mapconcat
   (lambda (todo)
     (let* ((content (or (map-elt todo 'content) ""))
            (status (or (map-elt todo 'status) "pending"))
            (marker (cond
                     ((member status '("completed" "done")) "- [x] ")
                     ((equal status "in_progress") "- [~] ")
                     (t "- [ ] ")))
            (status-label (cond
                           ((member status '("completed" "done")) "done")
                           ((equal status "in_progress") "in progress")
                           (t nil))))
       (format "%s%s%s"
               marker
               content
               (when status-label
                 (format " *(%s)*" status-label)))))
   todos
   "\n"))

(defun agent-shell-cursor--on-update-todos-display (state acp-message)
  "Display todos from ACP-MESSAGE in agent shell as a fragment.
STATE and ACP-MESSAGE are the handler arguments (request or notification)."
  (let* ((params (or (map-elt acp-message 'params) (list)))
         (todos (map-elt params 'todos))
         (body (or (map-elt params 'body) (map-elt params 'message)))
         (content (condition-case _err
                      (cond
                       ((and (seqp todos) (seq-length todos))
                        (agent-shell-cursor--format-todos-as-markdown todos))
                       (body (if (stringp body) body (json-encode body)))
                       (t (json-encode params)))
                    (error (json-encode params))))
         (block-id (format "cursor-todos-%s"
                           (or (map-elt params 'toolCallId)
                               (format-time-string "%s")))))
    (agent-shell--update-fragment
     :state state
     :block-id block-id
     :label-left (concat
                  agent-shell-cursor--todos-icon
                  " "
                  (propertize "Todos" 'font-lock-face 'font-lock-doc-markup-face))
     :body content
     :create-new t
     :expanded t
     :navigation 'never)))

(defun agent-shell-cursor--on-update-todos (state acp-request)
  "Handle cursor/update_todos ACP request.

STATE and ACP-REQUEST are as per `agent-shell-make-agent-config' :request-handlers.
Sends success response immediately; displays todos as a fragment in the agent shell."
  (agent-shell--send-unhandled-request-response state acp-request)
  (agent-shell-cursor--on-update-todos-display state acp-request)
  t)

(defun agent-shell-cursor--on-create-plan (state acp-request)
  "Handle _cursor/create_plan ACP request.

Plan display is already handled via session/update notifications.
This handler only sends a method-not-found error response."
  (agent-shell--send-unhandled-request-response state acp-request)
  t)

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
