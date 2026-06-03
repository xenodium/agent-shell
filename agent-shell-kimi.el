;;; agent-shell-kimi.el --- Kimi Code agent configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nicolai Singh

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
;; This file includes Kimi Code configuration using `kimi acp'.
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

(defcustom agent-shell-kimi-acp-command
  '("kimi" "acp")
  "Command and parameters for the Kimi Code CLI client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-kimi-default-model-id
  nil
  "Default Kimi model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell.

Can be set to either a string or a function that returns a string."
  :type '(choice (const nil) string function)
  :group 'agent-shell)

(defcustom agent-shell-kimi-default-session-mode-id
  nil
  "Default Kimi Code session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-kimi-environment
  nil
  "Environment variables for the Kimi Code CLI client.

This should be a list of environment variables to be used when
starting the Kimi client process."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-kimi-make-config ()
  "Create a Kimi Code agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'kimi
   :mode-line-name "Kimi"
   :buffer-name "Kimi"
   :shell-prompt "Kimi> "
   :shell-prompt-regexp "Kimi> "
   :welcome-function #'agent-shell-kimi--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-kimi-make-client :buffer buffer))
   :default-model-id (lambda () (if (functionp agent-shell-kimi-default-model-id)
                                    (funcall agent-shell-kimi-default-model-id)
                                  agent-shell-kimi-default-model-id))
   :default-session-mode-id (lambda () agent-shell-kimi-default-session-mode-id)
   :install-instructions "See https://www.kimi.com/code for installation."))

(defun agent-shell-kimi-start-agent ()
  "Start an interactive Kimi Code agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-kimi-make-config)
                     :new-shell t))

(cl-defun agent-shell-kimi-make-client (&key buffer)
  "Create a Kimi Code ACP client with BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-kimi-acp-command)
                                :command-params (cdr agent-shell-kimi-acp-command)
                                :environment-variables agent-shell-kimi-environment
                                :context-buffer buffer))

(defun agent-shell-kimi--welcome-message (config)
  "Return Kimi ASCII art using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-kimi--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-kimi--ascii-art ()
  "Kimi ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
 ██╗  ██╗ ██╗ ███╗   ███╗ ██╗
 ██║ ██╔╝ ██║ ████╗ ████║ ██║
 █████╔╝  ██║ ██╔████╔██║ ██║
 ██╔═██╗  ██║ ██║╚██╔╝██║ ██║
 ██║  ██╗ ██║ ██║ ╚═╝ ██║ ██║
 ╚═╝  ╚═╝ ╚═╝ ╚═╝     ╚═╝ ╚═╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#dddddd" :inherit fixed-pitch)
                                       '(:foreground "#000000" :inherit fixed-pitch)))))

(provide 'agent-shell-kimi)

;;; agent-shell-kimi.el ends here
