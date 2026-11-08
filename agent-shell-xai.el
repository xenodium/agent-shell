;;; agent-shell-xai.el --- xAI Grok Build agent configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eddie Jesinsky

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
;; This file includes xAI Grok Build-specific configurations.
;;
;; Grok Build speaks ACP over stdio via `grok agent stdio'.  Auth is the
;; local CLI login flow (~/.grok/auth.json); no separate API-key env is
;; required for the default setup.
;;

;;; Code:

(defconst agent-shell-xai-icon-name
  "xai.png"
  "Icon name for xAI / Grok Build (from lobe-icons).")

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

(defcustom agent-shell-xai-acp-command
  '("grok" "agent" "stdio")
  "Command and parameters for the Grok Build ACP client.

The first element is the command name, and the rest are command parameters.

Examples:

  (\"grok\" \"agent\" \"stdio\")
  (\"grok\" \"agent\" \"-m\" \"grok-build\" \"stdio\")
  (\"grok\" \"agent\" \"--always-approve\" \"stdio\")"
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-xai-environment
  nil
  "Environment variables for the Grok Build ACP client.

This should be a list of environment variables to be used when
starting the Grok Build client process.

Example usage to set custom environment variables:

  (setq agent-shell-xai-environment
        (`agent-shell-make-environment-variables'
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-xai-default-model-id
  nil
  "Default Grok Build model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell.

Can be set to either a string or a function that returns a string."
  :type '(choice (const nil) string function)
  :group 'agent-shell)

(defcustom agent-shell-xai-default-session-mode-id
  nil
  "Default Grok Build session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defun agent-shell-xai-make-grok-config ()
  "Create a Grok Build agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'grok-build
   :mode-line-name "Grok"
   :buffer-name "Grok"
   :shell-prompt "Grok> "
   :shell-prompt-regexp "Grok> "
   :icon-name agent-shell-xai-icon-name
   :welcome-function #'agent-shell-xai--welcome-message
   ;; Grok ACP advertises authMethods: cached_token (from ~/.grok/auth.json)
   ;; and grok.com (interactive sign-in). defaultAuthMethodId is cached_token.
   ;; Without an authenticate request, session/new fails with
   ;; AuthorizationRequired / "Transport channel closed".
   :needs-authentication t
   :authenticate-request-maker (lambda ()
                                 (acp-make-authenticate-request
                                  :method-id "cached_token"))
   :client-maker (lambda (buffer)
                   (agent-shell-xai-make-client :buffer buffer))
   :default-model-id (lambda () (if (functionp agent-shell-xai-default-model-id)
                                    (funcall agent-shell-xai-default-model-id)
                                  agent-shell-xai-default-model-id))
   :default-session-mode-id (lambda () agent-shell-xai-default-session-mode-id)
   :install-instructions
   "Install the Grok Build CLI so `grok' is on PATH (typically ~/.grok/bin).
Authenticate once via the CLI login flow (stores ~/.grok/auth.json).
See https://docs.x.ai/docs/overview and https://zed.dev/acp/agent/grok-build.
ACP over stdio: grok agent stdio"))

(defun agent-shell-xai-start-grok ()
  "Start an interactive Grok Build agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-xai-make-grok-config)
                     :new-shell t))

(cl-defun agent-shell-xai-make-client (&key buffer)
  "Create a Grok Build ACP client with BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-xai-acp-command)
                                :command-params (cdr agent-shell-xai-acp-command)
                                :environment-variables agent-shell-xai-environment
                                :context-buffer buffer))

(defun agent-shell-xai--welcome-message (config)
  "Return Grok Build welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-xai--ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-xai--ascii-art ()
  "Grok Build ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
 ██████╗ ██████╗   ██████╗  ██╗  ██╗
██╔════╝ ██╔══██╗ ██╔═══██╗ ██║ ██╔╝
██║  ███╗██████╔╝ ██║   ██║ █████╔╝
██║   ██║██╔══██╗ ██║   ██║ ██╔═██╗
╚██████╔╝██║  ██║ ╚██████╔╝ ██║  ██╗
 ╚═════╝ ╚═╝  ╚═╝  ╚═════╝  ╚═╝  ╚═╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#a78bfa" :inherit fixed-pitch)
                                       '(:foreground "#6d28d9" :inherit fixed-pitch)))))

(provide 'agent-shell-xai)

;;; agent-shell-xai.el ends here
