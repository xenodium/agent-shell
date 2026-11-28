;;; agent-shell-devin.el --- Devin agent configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bhavin Gandhi

;; Author: Bhavin Gandhi https://geeksocket.in
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
;; This file includes Devin-specific configurations relying on the
;; Devin CLI: https://docs.devin.ai/cli
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

(cl-defun agent-shell-devin-make-authentication (&key api-key login none)
  "Create Devin authentication configuration.

API-KEY is the Devin API key string or function that returns it.
It is passed to the agent via the WINDSURF_API_KEY environment
variable.
LOGIN when non-nil uses interactive \"devin-browser\" authentication,
which opens a browser tab for sign-in.
NONE when non-nil indicates authentication is handled externally
\(for example via `devin auth login').

Only one of API-KEY, LOGIN, or NONE should be provided."
  (when (> (seq-count #'identity (list api-key login none)) 1)
    (error "Cannot specify multiple authentication methods - choose one"))
  (unless (> (seq-count #'identity (list api-key login none)) 0)
    (error "Must specify one of :api-key, :login, or :none"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (login `((:login . t)))
   (none `((:none . t)))))

(defcustom agent-shell-devin-authentication
  (agent-shell-devin-make-authentication :none t)
  "Configuration for Devin authentication.

By default authentication is handled externally: `agent-shell' sends no
ACP authenticate request and relies on credentials stored by
`devin auth login' (or the WINDSURF_API_KEY environment variable,
which the ACP server also reads).  Run `devin auth login' once outside
Emacs to set this up.

For no authentication, handled externally (default):

  (setq agent-shell-devin-authentication
        (agent-shell-devin-make-authentication :none t))

For login-based authentication (agent-shell drives \"devin-browser\",
which opens a browser tab):

  (setq agent-shell-devin-authentication
        (agent-shell-devin-make-authentication :login t))

Note that every new shell sends the authenticate request, so each one
opens a browser tab.  Once logged in, prefer :none since credentials
are stored and reused.

For API key (string):

  (setq agent-shell-devin-authentication
        (agent-shell-devin-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-devin-authentication
        (agent-shell-devin-make-authentication
         :api-key (lambda ()
                    (auth-source-pass-get \\='secret \"devin-api-key\")))))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-devin-acp-command
  '("devin" "acp")
  "Command and parameters for the Devin agent client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-devin-default-config-options
  nil
  "Default Devin config options, applied at session start.

An alist of (OPTION . VALUE).  Both are the ids listed under
\"Available config options\" when starting a new shell, so whatever
Devin advertises can be set here without further `agent-shell'
changes.  OPTION also accepts the ACP category names (\"model\",
\"mode\") for options carrying one.

For example:

  (setq agent-shell-devin-default-config-options
        \\='((\"model\" . \"swe-2-high\")
          (\"mode\" . \"plan\")))

Options are applied in the order listed.  An option or value Devin
does not offer is reported and skipped."
  :type '(alist :key-type string :value-type string)
  :group 'agent-shell)

(defcustom agent-shell-devin-environment
  nil
  "Environment variables for the Devin agent client.

This should be a list of environment variables to be used when
starting the Devin agent process.

A value given as a function is resolved each time the agent starts,
rather than when this variable is set.  See
`agent-shell-make-environment-variables'."
  :type '(repeat (choice (string :tag "NAME=VALUE")
                         (cons :tag "Resolved at agent start"
                               (string :tag "Name")
                               (function :tag "Value function"))))
  :group 'agent-shell)

(defun agent-shell-devin-make-agent-config ()
  "Create a Devin agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'devin
   :mode-line-name "Devin"
   :buffer-name "Devin"
   :shell-prompt "Devin> "
   :shell-prompt-regexp "Devin> "
   :icon-name "devin.png"
   :welcome-function #'agent-shell-devin--welcome-message
   :default-config-options (lambda () agent-shell-devin-default-config-options)
   :needs-authentication (and (map-elt agent-shell-devin-authentication :login) t)
   :authenticate-request-maker (lambda ()
                                 (acp-make-authenticate-request :method-id "devin-browser"))
   :client-maker (lambda (buffer)
                   (agent-shell-devin-make-client :buffer buffer))
   :install-instructions "Install the Devin CLI and ensure `devin acp' is available. See https://docs.devin.ai/cli for installation."))

;;;###autoload
(defun agent-shell-devin-start-agent ()
  "Start an interactive Devin agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-devin-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-devin-make-client (&key buffer)
  "Create a Devin ACP client with BUFFER as context.

Uses `agent-shell-devin-authentication' for authentication configuration."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (let ((api-key (agent-shell-devin--resolve-secret
                  (map-elt agent-shell-devin-authentication :api-key))))
    (agent-shell--make-acp-client :command (car agent-shell-devin-acp-command)
                                  :command-params (cdr agent-shell-devin-acp-command)
                                  :environment-variables (append
                                                          (cond
                                                           ((or (map-elt agent-shell-devin-authentication :none)
                                                                (map-elt agent-shell-devin-authentication :login))
                                                            nil)
                                                           (api-key
                                                            (list (format "WINDSURF_API_KEY=%s" api-key)))
                                                           (t
                                                            (error "Missing Devin authentication (see agent-shell-devin-authentication)")))
                                                          agent-shell-devin-environment)
                                  :context-buffer buffer)))

(defun agent-shell-devin--resolve-secret (value)
  "Resolve VALUE to a string.
VALUE may be a string or a function that returns a string."
  (cond ((stringp value) value)
        ((functionp value)
         (condition-case _err
             (funcall value)
           (error
            (error "API key not found.  Check out `agent-shell-devin-authentication'"))))
        (t nil)))

(defun agent-shell-devin--welcome-message (config)
  "Return Devin welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-devin--ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-devin--ascii-art ()
  "Devin ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
██████╗ ███████╗██╗   ██╗██╗███╗   ██╗
██╔══██╗██╔════╝██║   ██║██║████╗  ██║
██║  ██║█████╗  ██║   ██║██║██╔██╗ ██║
██║  ██║██╔══╝  ╚██╗ ██╔╝██║██║╚██╗██║
██████╔╝███████╗ ╚████╔╝ ██║██║ ╚████║
╚═════╝ ╚══════╝  ╚═══╝  ╚═╝╚═╝  ╚═══╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#7aa2f7" :inherit fixed-pitch)
                                       '(:foreground "#2d5fd0" :inherit fixed-pitch)))))

(provide 'agent-shell-devin)

;;; agent-shell-devin.el ends here
