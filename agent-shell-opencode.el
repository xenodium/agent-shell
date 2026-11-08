;;; agent-shell-opencode.el --- OpenCode agent configurations -*- lexical-binding: t; -*-

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
;; This file includes OpenCode-specific configurations.
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

(cl-defun agent-shell-opencode-make-authentication (&key api-key none)
  "Create OpenCode authentication configuration.

API-KEY is the OpenCode API key string or function that returns it.
NONE when non-nil disables API key authentication.

Only one of API-KEY or NONE should be provided, never both."
  (when (and api-key none)
    (error "Cannot specify both :api-key and :none - choose one"))
  (unless (or api-key none)
    (error "Must specify either :api-key or :none"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (none `((:none . t)))))

(defcustom agent-shell-opencode-authentication
  (agent-shell-opencode-make-authentication :none t)
  "Configuration for OpenCode authentication.
For API key (string):

  (setq agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication :api-key (lambda () ...)))

For no authentication (when using `opencode auth login`):

  (setq agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication :none t))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-opencode-default-model-id
  nil
  "Default OpenCode model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-opencode-default-config-options
  nil
  "Default OpenCode config options, applied at session start.

An alist of (OPTION . VALUE).  Both are the ids listed under
\"Available config options\" when starting a new shell, so whatever
OpenCode advertises can be set here without further agent-shell
changes.  OPTION also accepts the ACP category names (\"model\",
\"mode\", \"thought_level\") for options carrying one.

OpenCode exposes model variants (provider-specific reasoning effort)
as its \"effort\" option:

  (setq agent-shell-opencode-default-config-options
        \\='((\"model\" . \"anthropic/claude-opus-4-5\")
          (\"effort\" . \"high\")
          (\"mode\" . \"plan\")))

Options are applied in the order listed.  Order matters: OpenCode
scopes the available efforts to the active model, so \"effort\" belongs
after \"model\".  An option or value OpenCode does not offer is
reported and skipped."
  :type '(alist :key-type string :value-type string)
  :group 'agent-shell)

(defcustom agent-shell-opencode-default-session-mode-id
  nil
  "Default OpenCode session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-opencode-acp-command
  '("opencode" "acp")
  "Command and parameters for the OpenCode client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-opencode-environment
  nil
  "Environment variables for the OpenCode client.

This should be a list of environment variables to be used when
starting the OpenCode client process.

Example usage to set custom environment variables:

  (setq agent-shell-opencode-environment
        (`agent-shell-make-environment-variables'
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-opencode-make-agent-config ()
  "Create an OpenCode agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'opencode
   :mode-line-name "OpenCode"
   :buffer-name "OpenCode"
   :shell-prompt "OpenCode> "
   :shell-prompt-regexp "OpenCode> "
   :welcome-function #'agent-shell-opencode--welcome-message
   :icon-name "opencode.png"
   :client-maker (lambda (buffer)
                   (agent-shell-opencode-make-client :buffer buffer))
   :default-model-id (lambda () agent-shell-opencode-default-model-id)
   :default-session-mode-id (lambda () agent-shell-opencode-default-session-mode-id)
   :default-config-options (lambda () agent-shell-opencode-default-config-options)
   :install-instructions "See https://opencode.ai/docs for installation."))

;;;###autoload
(defun agent-shell-opencode-start-agent ()
  "Start an interactive OpenCode agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-opencode-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-opencode-make-client (&key buffer)
  "Create an OpenCode client using BUFFER as context.

Uses `agent-shell-opencode-authentication' for authentication configuration."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-opencode-command) agent-shell-opencode-command)
    (user-error "Please migrate to use agent-shell-opencode-acp-command and eval (setq agent-shell-opencode-command nil)"))
  (let ((api-key (agent-shell-opencode-key)))
    (agent-shell--make-acp-client :command (car agent-shell-opencode-acp-command)
                                  :command-params (cdr agent-shell-opencode-acp-command)
                                  :environment-variables (append (cond ((map-elt agent-shell-opencode-authentication :none)
                                                                        nil)
                                                                       (api-key
                                                                        (list (format "OPENCODE_API_KEY=%s" api-key)))
                                                                       (t
                                                                        (error "Missing OpenCode authentication (see agent-shell-opencode-authentication)")))
                                                                 agent-shell-opencode-environment)
                                  :context-buffer buffer)))

(defun agent-shell-opencode-key ()
  "Get the OpenCode API key."
  (cond ((stringp (map-elt agent-shell-opencode-authentication :api-key))
         (map-elt agent-shell-opencode-authentication :api-key))
        ((functionp (map-elt agent-shell-opencode-authentication :api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-opencode-authentication :api-key))
           (error
            (error "API key not found.  Check out `agent-shell-opencode-authentication'"))))
        (t
         nil)))

(defun agent-shell-opencode--welcome-message (config)
  "Return OpenCode welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-opencode--ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-opencode--ascii-art ()
  "OpenCode ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
                                  ▄
 █▀▀█ █▀▀█ █▀▀█ █▀▀▄ █▀▀▀ █▀▀█ █▀▀█ █▀▀█
 █░░█ █░░█ █▀▀▀ █░░█ █░░░ █░░█ █░░█ █▀▀▀
 ▀▀▀▀ █▀▀▀ ▀▀▀▀ ▀  ▀ ▀▀▀▀ ▀▀▀▀ ▀▀▀▀ ▀▀▀▀
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#4a9eff" :inherit fixed-pitch)
                                       '(:foreground "#2563eb" :inherit fixed-pitch)))))

(provide 'agent-shell-opencode)

;;; agent-shell-opencode.el ends here
