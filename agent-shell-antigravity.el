;;; agent-shell-antigravity.el --- Antigravity agent configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 tycho garen <garen@tychoish.com>

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
;; This file includes Google Antigravity-specific configurations.
;;
;; Antigravity ships as a prebuilt "agy_acp_server" binary (no npm/pip
;; package), distributed per-platform.  Download and unpack the archive
;; for your platform from
;; https://github.com/agentclientprotocol/registry/tree/main/antigravity-acp
;; and make sure the resulting executable is on `exec-path' (or set
;; `agent-shell-antigravity-acp-command' to an absolute path).
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

(defcustom agent-shell-antigravity-acp-command
  (cond ((memq system-type '(windows-nt ms-dos))
         '("agy_acp_server.exe"))
        ((eq system-type 'darwin)
         '("agy_acp_server.par"))
        (t
         '("agy_acp_server.par" "--uid=")))
  "Command and parameters for the Antigravity ACP server.

The first element is the command name, and the rest are command
parameters.  Defaults follow the per-platform distribution metadata at
https://github.com/agentclientprotocol/registry/tree/main/antigravity-acp
\(Linux additionally passes \"--uid=\").  Set this to an absolute path
if the binary isn't on variable `exec-path'."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-antigravity-environment
  nil
  "Environment variables for the Antigravity ACP server.

This should be a list of environment variables to be used when
starting the Antigravity agent process."
  :type '(repeat string)
  :group 'agent-shell)

(cl-defun agent-shell-antigravity-make-authentication (&key login api-key business agent-platform none)
  "Create Antigravity authentication configuration.

LOGIN when non-nil indicates to use Google account (oauth-personal)
authentication.
API-KEY is the Gemini API key string or function that returns it.
BUSINESS when non-nil indicates to use Gemini Enterprise (oauth-business)
authentication.
AGENT-PLATFORM when non-nil indicates to use Gemini Enterprise Agent
Platform authentication.
NONE when non-nil indicates no authentication method is used (when
authentication is managed externally).

Only one of LOGIN, API-KEY, BUSINESS, AGENT-PLATFORM, or NONE
should be provided."
  (when (> (seq-count #'identity (list login api-key business agent-platform none)) 1)
    (error "Cannot specify multiple authentication methods - choose one"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (business `((:business . t)))
   (agent-platform `((:agent-platform . t)))
   (none `((:none . t)))
   (t `((:login . t)))))

(defcustom agent-shell-antigravity-authentication
  (agent-shell-antigravity-make-authentication :login t)
  "Configuration for Antigravity authentication.

For login-based authentication (default, Google account):

  (setq agent-shell-antigravity-authentication
        (agent-shell-antigravity-make-authentication :login t))

For API key (string):

  (setq agent-shell-antigravity-authentication
        (agent-shell-antigravity-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-antigravity-authentication
        (agent-shell-antigravity-make-authentication :api-key (lambda () ...)))

For Gemini Enterprise (business) authentication:

  (setq agent-shell-antigravity-authentication
        (agent-shell-antigravity-make-authentication :business t))

For Gemini Enterprise Agent Platform authentication:

  (setq agent-shell-antigravity-authentication
        (agent-shell-antigravity-make-authentication :agent-platform t))

For no authentication (when managed externally):

  (setq agent-shell-antigravity-authentication
        (agent-shell-antigravity-make-authentication :none t))"
  :type 'alist
  :group 'agent-shell)

(defun agent-shell-antigravity-key ()
  "Get the Gemini API key."
  (cond ((stringp (map-elt agent-shell-antigravity-authentication :api-key))
         (map-elt agent-shell-antigravity-authentication :api-key))
        ((functionp (map-elt agent-shell-antigravity-authentication :api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-antigravity-authentication :api-key))
           (error
            (error "Gemini API key not found.  Check out `agent-shell-antigravity-authentication'"))))
        (t
         nil)))

(defcustom agent-shell-antigravity-default-model-id
  nil
  "Default Antigravity model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell.

Can be set to either a string or a function that returns a string."
  :type '(choice (const nil) string function)
  :group 'agent-shell)

(defcustom agent-shell-antigravity-default-session-mode-id
  nil
  "Default Antigravity session mode ID.

Must be one of the session mode ID's displayed under \"Available session modes\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defun agent-shell-antigravity-make-agent-config ()
  "Create an Antigravity agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'antigravity
   :mode-line-name "Antigravity"
   :buffer-name "Antigravity"
   :shell-prompt "Antigravity> "
   :shell-prompt-regexp "Antigravity> "
   :welcome-function #'agent-shell-antigravity--welcome-message
   :icon-name "antigravity.png"
   :needs-authentication (not (map-elt agent-shell-antigravity-authentication :none))
   :authenticate-request-maker #'agent-shell-antigravity--authenticate-request
   :client-maker (lambda (buffer)
                   (agent-shell-antigravity-make-client :buffer buffer))
   :default-model-id (lambda () (if (functionp agent-shell-antigravity-default-model-id)
                                    (funcall agent-shell-antigravity-default-model-id)
                                  agent-shell-antigravity-default-model-id))
   :default-session-mode-id (lambda () agent-shell-antigravity-default-session-mode-id)
   :install-instructions "See https://antigravity.google/docs/ide/extensions for installation."))

(defun agent-shell-antigravity--authenticate-request ()
  "Return the `authenticate' request for the configured method.

Method resolution follows `agent-shell-antigravity-authentication'."
  ;; TODO: Save authentication methods from initialization and resolve
  ;; :method-id to :method which came from the agent.
  (cond ((map-elt agent-shell-antigravity-authentication :api-key)
         (acp-make-authenticate-request
          :method-id "gemini-api-key"
          :method '((id . "gemini-api-key")
                    (name . "Gemini API key")
                    (description . "Use an API key with Gemini Developer API"))))
        ((map-elt agent-shell-antigravity-authentication :business)
         (acp-make-authenticate-request
          :method-id "oauth-business"
          :method '((id . "oauth-business")
                    (name . "Log in with Gemini Enterprise")
                    (description . "Log in with your Gemini Enterprise account"))))
        ((map-elt agent-shell-antigravity-authentication :agent-platform)
         (acp-make-authenticate-request
          :method-id "agent-platform"
          :method '((id . "agent-platform")
                    (name . "Gemini Enterprise Agent Platform")
                    (description . "Use Gemini Enterprise Agent Platform (formerly Vertex AI)"))))
        ((map-elt agent-shell-antigravity-authentication :none)
         nil)
        (t
         (acp-make-authenticate-request
          :method-id "oauth-personal"
          :method '((id . "oauth-personal")
                    (name . "Log in with Google")
                    (description . "Log in with your Google account"))))))

;;;###autoload
(defun agent-shell-antigravity-start-agent ()
  "Start an interactive Antigravity agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-antigravity-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-antigravity-make-client (&key buffer)
  "Create an Antigravity ACP client with BUFFER as context.

Uses `agent-shell-antigravity-authentication' for authentication configuration."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-antigravity-acp-command)
                                :command-params (cdr agent-shell-antigravity-acp-command)
                                :environment-variables (append (when-let* ((api-key (agent-shell-antigravity-key)))
                                                                 (list (format "GEMINI_API_KEY=%s" api-key)))
                                                               agent-shell-antigravity-environment)
                                :context-buffer buffer))

(defun agent-shell-antigravity--welcome-message (config)
  "Return Antigravity ASCII art using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-antigravity--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-antigravity--ascii-art ()
  "Antigravity ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
 █████╗ ███╗   ██╗████████╗██╗
██╔══██╗████╗  ██║╚══██╔══╝██║
███████║██╔██╗ ██║   ██║   ██║
██╔══██║██║╚██╗██║   ██║   ██║
██║  ██║██║ ╚████║   ██║   ██║
╚═╝  ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚═╝
 ██████╗ ██████╗  █████╗ ██╗   ██╗██╗████████╗██╗   ██╗
██╔════╝ ██╔══██╗██╔══██╗██║   ██║██║╚══██╔══╝╚██╗ ██╔╝
██║  ███╗██████╔╝███████║██║   ██║██║   ██║    ╚████╔╝
██║   ██║██╔══██╗██╔══██║╚██╗ ██╔╝██║   ██║     ╚██╔╝
╚██████╔╝██║  ██║██║  ██║ ╚████╔╝ ██║   ██║      ██║
 ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝  ╚═══╝  ╚═╝   ╚═╝      ╚═╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#8E75FF" :inherit fixed-pitch)
                                       '(:foreground "#5B3FD9" :inherit fixed-pitch)))))

(provide 'agent-shell-antigravity)

;;; agent-shell-antigravity.el ends here
