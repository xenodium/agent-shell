;;; agent-shell-auggie.el --- Auggie agent configurations -*- lexical-binding: t; -*-

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
;; This file includes Auggie-specific configurations.
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
(declare-function agent-shell-start "agent-shell")

(cl-defun agent-shell-make-auggie-authentication (&key login none)
  "Create Auggie authentication configuration.

LOGIN when non-nil indicates to use login-based authentication.
NONE when non-nil disables authentication (for local usage).

Only one of LOGIN or NONE should be provided, never both."
  (when (and login none)
    (error "Cannot specify both :login and :none - choose one"))
  (unless (or login none)
    (error "Must specify either :login or :none"))
  (cond
   (login `((:login . t)))
   (none `((:none . t)))))

(defcustom agent-shell-auggie-authentication
  (agent-shell-make-auggie-authentication :login t)
  "Configuration for Auggie authentication.
For login-based authentication (default):

  (setq agent-shell-auggie-authentication
        (agent-shell-make-auggie-authentication :login t))

For no authentication (when using alternative authentication methods):

  (setq agent-shell-auggie-authentication
        (agent-shell-make-auggie-authentication :none t))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-auggie-acp-command
  '("auggie" "--acp")
  "Command and parameters for the Auggie client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-auggie-environment
  nil
  "Environment variables for the Auggie client.

This should be a list of environment variables to be used when
starting the Auggie client process.

Example usage to set custom environment variables:

  (setq agent-shell-auggie-environment
        (`agent-shell-make-environment-variables'
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-auggie-make-agent-config ()
  "Create an Auggie agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'auggie
   :mode-line-name "Auggie"
   :buffer-name "Auggie"
   :shell-prompt "Auggie> "
   :shell-prompt-regexp "Auggie> "
   :welcome-function #'agent-shell-auggie--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-auggie-make-client :buffer buffer))
   :install-instructions "See https://docs.augmentcode.com/cli/overview for installation."))

;;;###autoload
(defun agent-shell-auggie-start-agent ()
  "Start an interactive Auggie agent shell."
  (interactive)
  (agent-shell-start
   :config (agent-shell-auggie-make-agent-config)))

(cl-defun agent-shell-auggie-make-client (&key buffer)
  "Create an Auggie client using configured authentication with BUFFER as context.

Uses `agent-shell-auggie-authentication' for authentication configuration."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-auggie-command) agent-shell-auggie-command)
    (user-error "Please migrate to use agent-shell-auggie-acp-command and eval (setq agent-shell-auggie-command nil)"))
  (agent-shell--make-acp-client :command (car agent-shell-auggie-acp-command)
                                :command-params (cdr agent-shell-auggie-acp-command)
                                :environment-variables (cond ((map-elt agent-shell-auggie-authentication :none)
                                                              agent-shell-auggie-environment)
                                                             ((map-elt agent-shell-auggie-authentication :login)
                                                              agent-shell-auggie-environment)
                                                             (t
                                                              (error "Invalid Auggie authentication configuration")))
                                :context-buffer buffer))

(defun agent-shell-auggie--welcome-message (config)
  "Return Auggie welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-auggie--ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-auggie--ascii-art ()
  "Auggie ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
 █████╗ ██╗   ██╗ ██████╗  ██████╗ ██╗███████╗
██╔══██╗██║   ██║██╔════╝ ██╔════╝ ██║██╔════╝
███████║██║   ██║██║  ███╗██║  ███╗██║█████╗
██╔══██║██║   ██║██║   ██║██║   ██║██║██╔══╝
██║  ██║╚██████╔╝╚██████╔╝╚██████╔╝██║███████╗
╚═╝  ╚═╝ ╚═════╝  ╚═════╝  ╚═════╝ ╚═╝╚══════╝" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#3D855E")
                                       '(:foreground "#2D6B4A")))))

(provide 'agent-shell-auggie)

;;; agent-shell-auggie.el ends here
