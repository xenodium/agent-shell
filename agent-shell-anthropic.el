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
(declare-function agent-shell-welcome-message "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(cl-defun agent-shell-anthropic-make-authentication (&key api-key login oauth)
  "Create anthropic authentication configuration.

API-KEY is the Anthropic API key string or a function returning one.
LOGIN when non-nil indicates to use login-based authentication.
OAUTH is an OAuth token string or a function returning one.

Only one of API-KEY, LOGIN, or OAUTH should be provided, never more than one."
  (when (and api-key login)
    (error "Cannot specify both :api-key and :login - choose one"))
  (when (and oauth login)
    (error "Cannot specify both :oauth and :login - choose one"))
  (when (and api-key oauth)
    (error "Cannot specify both :api-key and :oauth - choose one"))
  (unless (or api-key login oauth)
    (error "Must specify either :api-key, :login, or :oauth"))
  (cond
   (oauth `((:oauth . ,oauth)))
   (api-key `((:api-key . ,api-key)))
   (login `((:login . t)))))

(defcustom agent-shell-anthropic-authentication
  (agent-shell-anthropic-make-authentication :login t)
  "Configuration for Anthropic authentication.
For subscription/login (default):

  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))

For api key:

  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :api-key \"your-key\"))

  or

  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :api-key (lambda () ... )))

For OAuth token:

  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :oauth \"your-token\"))

  or

  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :oauth (lambda () ... )))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-anthropic-default-model-id
  nil
  "Default Anthropic model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell.

Can be set to either a string or a function that returns a string."
  :type '(choice (const nil) string function)
  :group 'agent-shell)

(defcustom agent-shell-anthropic-default-session-mode-id
  nil
  "Default Anthropic session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-anthropic-claude-acp-command
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

(defun agent-shell-anthropic-make-claude-code-config ()
  "Create a Claude Agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'claude-code
   :mode-line-name "Claude"
   :buffer-name "Claude"
   :shell-prompt "Claude> "
   :shell-prompt-regexp "Claude> "
   :icon-name "claudecode.png"
   :welcome-function #'agent-shell-anthropic--claude-code-welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-anthropic-make-claude-client :buffer buffer))
   :default-model-id (lambda () (if (functionp agent-shell-anthropic-default-model-id)
                                    (funcall agent-shell-anthropic-default-model-id)
                                  agent-shell-anthropic-default-model-id))
   :default-session-mode-id (lambda () agent-shell-anthropic-default-session-mode-id)
   ;; Recent Claude models default `thinking.display' to "omitted", which
   ;; streams signature-only thinking blocks with no visible text.  Request
   ;; "summarized" so thinking is shown.  This is orthogonal to the effort /
   ;; thought level, which the agent advertises as a config option.
   :session-meta '((claudeCode
                    . ((options
                        . ((thinking
                            . ((type . "adaptive")
                               (display . "summarized"))))))))
   :install-instructions "See https://github.com/agentclientprotocol/claude-agent-acp for installation."))

(defun agent-shell-anthropic-start-claude-code ()
  "Start an interactive Claude Agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-anthropic-make-claude-code-config)
                     :new-shell t))

(cl-defun agent-shell-anthropic-make-claude-client (&key buffer)
  "Create a Claude Agent ACP client with BUFFER as context.

See `agent-shell-anthropic-authentication' for authentication
and optionally `agent-shell-anthropic-claude-environment' for
additional environment variables."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-anthropic-key) agent-shell-anthropic-key)
    (user-error "Please migrate to use agent-shell-anthropic-authentication and eval (setq agent-shell-anthropic-key nil)"))
  (when (and (boundp 'agent-shell-anthropic-claude-command) agent-shell-anthropic-claude-command)
    (user-error "Please migrate to use agent-shell-anthropic-claude-acp-command and eval (setq agent-shell-anthropic-claude-command nil)"))
  (let ((env-vars-overrides (cond
                             ((map-elt agent-shell-anthropic-authentication :api-key)
                              (list (format "ANTHROPIC_API_KEY=%s"
                                            (agent-shell-anthropic-key))))
                             ((map-elt agent-shell-anthropic-authentication :login)
                              (list "ANTHROPIC_API_KEY="))
                             ((map-elt agent-shell-anthropic-authentication :oauth)
                              (list (format "CLAUDE_CODE_OAUTH_TOKEN=%s"
                                            (agent-shell-anthropic-oauth-token))))
                             (t
                              (error "Invalid authentication configuration")))))
    (agent-shell--make-acp-client :command (car agent-shell-anthropic-claude-acp-command)
                                  :command-params (cdr agent-shell-anthropic-claude-acp-command)
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

(defun agent-shell-anthropic-oauth-token ()
  "Get the Anthropic OAuth token."
  (cond ((stringp (map-elt agent-shell-anthropic-authentication :oauth))
         (map-elt agent-shell-anthropic-authentication :oauth))
        ((functionp (map-elt agent-shell-anthropic-authentication :oauth))
         (condition-case _err
             (funcall (map-elt agent-shell-anthropic-authentication :oauth))
           (error
            "OAuth token not found.  Check out `agent-shell-anthropic-authentication'")))
        (t
         nil)))

(defun agent-shell-anthropic--claude-code-welcome-message (config)
  "Return Claude Agent ASCII art as per own repo using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-anthropic--claude-code-ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-anthropic--claude-code-ascii-art ()
  "Claude Agent ASCII art.

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
