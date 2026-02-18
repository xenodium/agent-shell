;;; agent-shell-anthropic.el --- Anthropic agent configurations -*- lexical-binding: t; -*-

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
;; This file includes Anthropic-specific configurations.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(cl-defun agent-shell-anthropic-make-authentication (&key api-key login)
  "Create anthropic authentication configuration.

API-KEY is the Anthropic API key string.
LOGIN when non-nil indicates to use login-based authentication.

Only one of API-KEY or LOGIN should be provided, never both."
  (when (and api-key login)
    (error "Cannot specify both :api-key and :login - choose one"))
  (unless (or api-key login)
    (error "Must specify either :api-key or :login"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (login `((:login . t)))))

(defcustom agent-shell-anthropic-authentication
  (agent-shell-anthropic-make-authentication :login t)
  "Configuration for Anthropic authentication.
For Subcription/login (default):

  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))

For api key:

  (setq agent-shell-anthropic-authentication
        (agent-shell-make-anthropic-authentication :api-key \"your-key\"))

  or

  (setq agent-shell-anthropic-authentication
        (agent-shell-make-anthropic-authentication :api-key (lambda () ... )))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-anthropic-default-model-id
  nil
  "Default Anthropic model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-anthropic-default-model-name
  nil
  "Default Anthropic model name.

Must be one of the model names displayed under \"Available models\"
when starting a new shell. For example: \"Opus\", \"Sonnet\", \"Haiku\".

Cannot be used together with `agent-shell-anthropic-default-model-id'.
If both are set, an error will be raised."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-anthropic-default-session-mode-id
  nil
  "Default Anthropic session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-anthropic-claude-command
  '("claude-agent-acp")
  "Command and parameters for the Anthropic Claude client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-anthropic-claude-environment
  nil
  "Environment variables for the Anthropic Claude client.

This should be a list of environment variables to be used when
starting the Claude client process.

Example usage to set a custom Anthropic API base URL:

  (setq agent-shell-anthropic-claude-environment
        (`agent-shell-make-environment-variables'
         \"ANTHROPIC_BASE_URL\" \"https://api.moonshot.cn/anthropic/\"
         \"ANTHROPIC_MODEL\" \"moonshot-v1-auto\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-anthropic--find-model-id-by-name (name models)
  "Find model ID for model with NAME in MODELS list.

MODELS is a list of model alists with :name and :model-id keys.
Returns the model-id if found, nil otherwise."
  (when (and name models)
    (let ((model (seq-find (lambda (m)
                            (string-equal (downcase (or (map-elt m :name) ""))
                                         (downcase name)))
                          models)))
      (when model
        (map-elt model :model-id)))))

(defun agent-shell-anthropic--resolve-default-model-id ()
  "Resolve the default model ID from name or ID configuration.

Returns the model ID to use.  Only one of
`agent-shell-anthropic-default-model-name' or
`agent-shell-anthropic-default-model-id' should be configured.
If both are set, an error is raised.

When a model name is configured, this function will attempt
to resolve it to a model ID once models are available in the
session state.  If the name cannot be resolved, an error is
raised."
  ;; Error if both are configured to avoid confusion
  (when (and agent-shell-anthropic-default-model-name
             agent-shell-anthropic-default-model-id)
    (error "Both `agent-shell-anthropic-default-model-name' and `agent-shell-anthropic-default-model-id' are configured. Please use only one"))
  (cond
   ;; If model name is specified and we have session models, resolve it
   ((and agent-shell-anthropic-default-model-name
         (boundp 'agent-shell--state)
         (map-nested-elt agent-shell--state '(:session :models)))
    (let ((resolved-id (agent-shell-anthropic--find-model-id-by-name
                       agent-shell-anthropic-default-model-name
                       (map-nested-elt agent-shell--state '(:session :models)))))
      (unless resolved-id
        (error "Model name '%s' not found in available models"
               agent-shell-anthropic-default-model-name))
      resolved-id))
   ;; If only model name is specified but models aren't loaded yet, return marker
   (agent-shell-anthropic-default-model-name
    ;; This tells agent-shell to call us again after session init
    'resolve-by-name)
   ;; Otherwise use the model ID directly
   (agent-shell-anthropic-default-model-id
    agent-shell-anthropic-default-model-id)
   ;; No default specified
   (t nil)))

(defun agent-shell-anthropic-make-claude-code-config ()
  "Create a Claude Code agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'claude-code
   :mode-line-name "Claude Code"
   :buffer-name "Claude Code"
   :shell-prompt "Claude Code> "
   :shell-prompt-regexp "Claude Code> "
   :icon-name "anthropic.png"
   :welcome-function #'agent-shell-anthropic--claude-code-welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-anthropic-make-claude-client :buffer buffer))
   :default-model-id #'agent-shell-anthropic--resolve-default-model-id
   :default-session-mode-id (lambda () agent-shell-anthropic-default-session-mode-id)
   :install-instructions "See https://github.com/zed-industries/claude-agent-acp for installation."))

(defun agent-shell-anthropic-start-claude-code ()
  "Start an interactive Claude Code agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-anthropic-make-claude-code-config)
                     :new-shell t))

(cl-defun agent-shell-anthropic-make-claude-client (&key buffer)
  "Create a Claude Code ACP client with BUFFER as context.

See `agent-shell-anthropic-authentication' for authentication
and optionally `agent-shell-anthropic-claude-environment' for
additional environment variables."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-anthropic-key) agent-shell-anthropic-key)
    (user-error "Please migrate to use agent-shell-anthropic-authentication and eval (setq agent-shell-anthropic-key nil)"))
  (let ((env-vars-overrides (cond
                             ((map-elt agent-shell-anthropic-authentication :api-key)
                              (list (format "ANTHROPIC_API_KEY=%s"
                                            (agent-shell-anthropic-key))))
                             ((map-elt agent-shell-anthropic-authentication :login)
                              (list "ANTHROPIC_API_KEY="))
                             (t
                              (error "Invalid authentication configuration")))))
    (agent-shell--make-acp-client :command (car agent-shell-anthropic-claude-command)
                                  :command-params (cdr agent-shell-anthropic-claude-command)
                                  :environment-variables (append env-vars-overrides
                                                                 agent-shell-anthropic-claude-environment)
                                  :context-buffer buffer)))

(defun agent-shell-anthropic-key ()
  "Get the Anthropic API key."
  (cond ((stringp (map-elt agent-shell-anthropic-authentication :api-key))
         (map-elt agent-shell-anthropic-authentication :api-key))
        ((functionp (map-elt agent-shell-anthropic-authentication :api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-anthropic-authentication :api-key))
           (error
            "Api key not found.  Check out `agent-shell-anthropic-authentication'")))
        (t
         nil)))

(defun agent-shell-anthropic--claude-code-welcome-message (config)
  "Return Claude Code ASCII art as per own repo using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-anthropic--claude-code-ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-anthropic--claude-code-ascii-art ()
  "Claude Code ASCII art.

Generated by https://github.com/shinshin86/oh-my-logo."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
  ██████╗ ██╗       █████╗  ██╗   ██╗ ██████╗  ███████╗
 ██╔════╝ ██║      ██╔══██╗ ██║   ██║ ██╔══██╗ ██╔════╝
 ██║      ██║      ███████║ ██║   ██║ ██║  ██║ █████╗
 ██║      ██║      ██╔══██║ ██║   ██║ ██║  ██║ ██╔══╝
 ╚██████╗ ███████╗ ██║  ██║ ╚██████╔╝ ██████╔╝ ███████╗
  ╚═════╝ ╚══════╝ ╚═╝  ╚═╝  ╚═════╝  ╚═════╝  ╚══════╝
  ██████╗  ██████╗  ██████╗  ███████╗
 ██╔════╝ ██╔═══██╗ ██╔══██╗ ██╔════╝
 ██║      ██║   ██║ ██║  ██║ █████╗
 ██║      ██║   ██║ ██║  ██║ ██╔══╝
 ╚██████╗ ╚██████╔╝ ██████╔╝ ███████╗
  ╚═════╝  ╚═════╝  ╚═════╝  ╚══════╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#d26043" :inherit fixed-pitch)
                                       '(:foreground "#b8431f" :inherit fixed-pitch)))))

(provide 'agent-shell-anthropic)

;;; agent-shell-anthropic.el ends here
