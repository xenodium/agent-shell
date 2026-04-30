;;; agent-shell-openai.el --- OpenAI agent configurations -*- lexical-binding: t; -*-

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
;; This file includes OpenAI-specific configurations.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'json)
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(cl-defun agent-shell-openai-make-authentication (&key api-key codex-api-key login)
  "Create OpenAI authentication configuration.

API-KEY is the OpenAI API key string or function that returns it.
CODEX-API-KEY is the Codex-specific API key.
LOGIN when non-nil indicates to use login-based authentication."
  (when (> (seq-count #'identity (list api-key codex-api-key login)) 1)
    (error "Cannot specify multiple authentication methods - choose one"))
  (unless (> (seq-count #'identity (list api-key codex-api-key login)) 0)
    (error "Must specify one of :api-key, :codex-api-key, :login"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (codex-api-key `((:codex-api-key . ,codex-api-key)))
   (login `((:login . t)))))

(defcustom agent-shell-openai-authentication
  (agent-shell-openai-make-authentication :login t)
  "Configuration for OpenAI authentication.
For login-based authentication (default):

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t))

For OpenAI API key (string):

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :api-key \"your-key\"))

For OpenAI API key (function):

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :api-key (lambda () ...)))

For Codex API key (string):

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :codex-api-key \"codex-key\"))

For Codex API key (function):

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :codex-api-key (lambda () ...)))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-openai-codex-acp-command
  '("codex-acp")
  "Command and parameters for the OpenAI Codex client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-openai-codex-environment
  nil
  "Environment variables for the OpenAI Codex client.

This should be a list of environment variables to be used when
starting the Codex client process.

Example usage to set custom environment variables:

  (setq agent-shell-openai-codex-environment
        (`agent-shell-make-environment-variables'
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-openai-default-model-id
  nil
  "Default Codex model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new Codex shell.

Can be set to either a string or a function that returns a string."
  :type '(choice (const nil) string function)
  :group 'agent-shell)

(defcustom agent-shell-openai-default-session-mode-id
  nil
  "Default Codex session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new Codex shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defun agent-shell-openai--codex-default-auth-request ()
  "Create the Codex default auth request for the current auth config."
  (cond ((map-elt agent-shell-openai-authentication :api-key)
         (let ((api-key (agent-shell-openai-key)))
           (unless api-key
             (user-error "Please set your `agent-shell-openai-authentication'"))
           `((methodId . "api-key")
             (_meta . ((api-key . ((apiKey . ,api-key))))))))
        ((map-elt agent-shell-openai-authentication :codex-api-key)
         (let ((codex-key (agent-shell-openai-key)))
           (unless codex-key
             (user-error "Please set your `agent-shell-openai-authentication'"))
           `((methodId . "api-key")
             (_meta . ((api-key . ((apiKey . ,codex-key))))))))
        (t
         '((methodId . "chat-gpt")))))

(defun agent-shell-openai--codex-default-auth-environment ()
  "Return DEFAULT_AUTH_REQUEST environment for Codex ACP."
  (list (concat "DEFAULT_AUTH_REQUEST="
                (json-serialize
                 (agent-shell-openai--codex-default-auth-request)))))

(defun agent-shell-openai-make-codex-config ()
  "Create a Codex agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (when (and (boundp 'agent-shell-openai-key) agent-shell-openai-key)
    (user-error "Please migrate to use agent-shell-openai-authentication and eval (setq agent-shell-openai-key nil)"))
  (agent-shell-make-agent-config
   :identifier 'codex
   :mode-line-name "Codex"
   :buffer-name "Codex"
   :shell-prompt "Codex> "
   :shell-prompt-regexp "Codex> "
   :welcome-function #'agent-shell-openai--codex-welcome-message
   :icon-name "openai.png"
   :needs-authentication nil
   :default-model-id (lambda () (if (functionp agent-shell-openai-default-model-id)
                                    (funcall agent-shell-openai-default-model-id)
                                  agent-shell-openai-default-model-id))
   :default-session-mode-id (lambda () agent-shell-openai-default-session-mode-id)
   :client-maker (lambda (buffer)
                   (agent-shell-openai-make-codex-client :buffer buffer))
   :install-instructions "See https://github.com/agentclientprotocol/codex-acp for installation."))

(defun agent-shell-openai-start-codex ()
  "Start an interactive Codex agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-openai-make-codex-config)
                     :new-shell t))

(cl-defun agent-shell-openai-make-codex-client (&key buffer)
  "Create a Codex client using configured authentication with BUFFER as context.

Uses `agent-shell-openai-authentication' for authentication configuration."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-openai-codex-command) agent-shell-openai-codex-command)
    (user-error "Please migrate to use agent-shell-openai-codex-acp-command and eval (setq agent-shell-openai-codex-command nil)"))
  (cond
   ((map-elt agent-shell-openai-authentication :api-key)
    (let ((api-key (agent-shell-openai-key)))
      (unless api-key
        (user-error "Please set your `agent-shell-openai-authentication'"))
      (agent-shell--make-acp-client :command (car agent-shell-openai-codex-acp-command)
                                    :command-params (cdr agent-shell-openai-codex-acp-command)
                                    :environment-variables (append (list (format "OPENAI_API_KEY=%s" api-key))
                                                                   (agent-shell-openai--codex-default-auth-environment)
                                                                   agent-shell-openai-codex-environment)
                                    :context-buffer buffer)))
   ((map-elt agent-shell-openai-authentication :codex-api-key)
    (let ((codex-key (agent-shell-openai-key)))
      (unless codex-key
        (user-error "Please set your `agent-shell-openai-authentication'"))
      (agent-shell--make-acp-client :command (car agent-shell-openai-codex-acp-command)
                                    :command-params (cdr agent-shell-openai-codex-acp-command)
                                    :environment-variables (append (list (format "CODEX_API_KEY=%s" codex-key))
                                                                   (agent-shell-openai--codex-default-auth-environment)
                                                                   agent-shell-openai-codex-environment)
                                    :context-buffer buffer)))
   ((map-elt agent-shell-openai-authentication :login)
     (agent-shell--make-acp-client :command (car agent-shell-openai-codex-acp-command)
                                   :command-params (cdr agent-shell-openai-codex-acp-command)
                                   :environment-variables (append '("OPENAI_API_KEY=")
                                                                  (agent-shell-openai--codex-default-auth-environment)
                                                                  agent-shell-openai-codex-environment)
                                   :context-buffer buffer))
   (t
    (error "Invalid authentication configuration"))))

(defun agent-shell-openai--codex-welcome-message (config)
  "Return Codex welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-openai--codex-ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-openai--codex-ascii-art ()
  "Codex ASCII art.

From https://github.com/openai/codex/blob/main/codex-rs/tui/frames/slug/frame_1.txt."
  (let* ((text (string-trim "
          d-dcottoottd
      dot5pot5tooeeod dgtd
    tepetppgde   egpegxoxeet
   cpdoppttd            5pecet
  odc5pdeoeoo            g-eoot
 xp te  ep5ceet           p-oeet
tdg-p    poep5ged          g e5e
eedee     t55ecep            gee
eoxpe    ceedoeg-xttttttdtt og e
 dxcp  dcte 5p egeddd-cttte5t5te
 oddgd dot-5e   edpppp dpg5tcd5
  pdt gt e              tp5pde
    doteotd          dodtedtg
      dptodgptccocc-optdtep
        epgpexxdddtdctpg
" "\n")))
    (propertize text 'font-lock-face 'font-lock-doc-face)))

(defun agent-shell-openai-key ()
  "Get the OpenAI API key."
  (cond ((stringp (map-elt agent-shell-openai-authentication :api-key))
         (map-elt agent-shell-openai-authentication :api-key))
        ((functionp (map-elt agent-shell-openai-authentication :api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-openai-authentication :api-key))
           (error
            (error "Api key not found.  Check out `agent-shell-openai-authentication'"))))
        ((stringp (map-elt agent-shell-openai-authentication :codex-api-key))
         (map-elt agent-shell-openai-authentication :codex-api-key))
        ((functionp (map-elt agent-shell-openai-authentication :codex-api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-openai-authentication :codex-api-key))
           (error
            (error "Codex API key not found.  Check out `agent-shell-openai-authentication'"))))
        (t
         nil)))

(provide 'agent-shell-openai)

;;; agent-shell-openai.el ends here
