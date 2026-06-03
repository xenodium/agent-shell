;;; agent-shell-codebuddy.el --- CodeBuddy agent configurations -*- lexical-binding: t; -*-

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
;; This file includes CodeBuddy-specific configurations.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(defcustom agent-shell-codebuddy-acp-command
  '("codebuddy" "--acp")
  "Command and parameters for the CodeBuddy agent client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-codebuddy-environment
  nil
  "Environment variables for the CodeBuddy agent client.

This should be a list of environment variables to be used when
starting the CodeBuddy agent process."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-codebuddy-make-agent-config ()
  "Create a CodeBuddy agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'codebuddy
   :mode-line-name "CodeBuddy"
   :buffer-name "CodeBuddy"
   :shell-prompt "CodeBuddy> "
   :shell-prompt-regexp "CodeBuddy> "
   :icon-name "codebuddy.png"
   :welcome-function #'agent-shell-codebuddy--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-codebuddy-make-client :buffer buffer))
   :install-instructions "Install the CodeBuddy CLI and ensure it supports ACP mode (for example, `codebuddy --acp`). See https://www.codebuddy.ai/docs/zh/ide/Getting-Started/Installation for installation."))

(defun agent-shell-codebuddy-start-agent ()
  "Start an interactive CodeBuddy agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-codebuddy-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-codebuddy-make-client (&key buffer)
  "Create a CodeBuddy agent ACP client with BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-codebuddy-acp-command)
                                :command-params (cdr agent-shell-codebuddy-acp-command)
                                :environment-variables agent-shell-codebuddy-environment
                                :context-buffer buffer))

(defun agent-shell-codebuddy--welcome-message (config)
  "Return CodeBuddy welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-codebuddy--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-codebuddy--ascii-art ()
  "CodeBuddy ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
   ______          __     ____            __    __
  / ____/___  ____/ /__  / __ )__  ______/ /___/ /_  __
 / /   / __ \/ __  / _ \/ __  / / / / __  / __  / / / /
/ /___/ /_/ / /_/ /  __/ /_/ / /_/ / /_/ / /_/ / /_/ /
\____/\____/\__,_/\___/_____/\__,_/\__,_/\__,_/\__, /
                                              /____/
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#4db6ff" :inherit fixed-pitch)
                                       '(:foreground "#005fa3" :inherit fixed-pitch)))))

(provide 'agent-shell-codebuddy)

;;; agent-shell-codebuddy.el ends here
