;;; agent-shell-github.el --- GitHub Copilot agent configurations -*- lexical-binding: t; -*-

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
;; This file includes GitHub Copilot-specific configurations.
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

(defcustom agent-shell-github-acp-command
  '("copilot" "--acp")
  "Command and parameters for the GitHub Copilot agent client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-github-default-model-id
  nil
  "Default GitHub Copilot model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-github-default-session-mode-id
  nil
  "Default GitHub Copilot session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-github-environment
  nil
  "Environment variables for the GitHub Copilot agent client.

This should be a list of environment variables to be used when
starting the GitHub Copilot agent process."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-github-make-copilot-config ()
  "Create a GitHub Copilot agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'copilot
   :mode-line-name "Copilot"
   :buffer-name "Copilot"
   :shell-prompt "Copilot> "
   :shell-prompt-regexp "Copilot> "
   :icon-name "githubcopilot.png"
   :welcome-function #'agent-shell-github--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-github-make-client :buffer buffer))
   :default-model-id (lambda () agent-shell-github-default-model-id)
   :default-session-mode-id (lambda () agent-shell-github-default-session-mode-id)
   :install-instructions "See https://github.com/github/copilot-cli for installation."))

(defun agent-shell-github-start-copilot ()
  "Start an interactive GitHub Copilot agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-github-make-copilot-config)
                     :new-shell t))

(cl-defun agent-shell-github-make-client (&key buffer)
  "Create a GitHub Copilot agent ACP client with BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-github-command) agent-shell-github-command)
    (user-error "Please migrate to use agent-shell-github-acp-command and eval (setq agent-shell-github-command nil)"))
  (agent-shell--make-acp-client :command (car agent-shell-github-acp-command)
                                :command-params (cdr agent-shell-github-acp-command)
                                :environment-variables agent-shell-github-environment
                                :context-buffer buffer))

(defun agent-shell-github--welcome-message (config)
  "Return GitHub Copilot welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-github--ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-github--ascii-art ()
  "GitHub Copilot ASCII art matching the official CLI banner."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
  █████┐ █████┐ █████┐ ██┐██┐     █████┐ ██████┐
 ██┌───┘██┌──██┐██┌─██┐██│██│    ██┌──██┐└─██┌─┘
 ██│    ██│  ██│█████┌┘██│██│    ██│  ██│  ██│
 ██│    ██│  ██│██┌──┘ ██│██│    ██│  ██│  ██│
 └█████┐└█████┌┘██│    ██│██████┐└█████┌┘  ██│
  └────┘ └────┘ └─┘    └─┘└─────┘ └────┘   └─┘
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#6e40c9" :inherit fixed-pitch)
                                       '(:foreground "#8250df" :inherit fixed-pitch)))))

(provide 'agent-shell-github)

;;; agent-shell-github.el ends here
