;;; agent-shell-fabrica.el --- Fabrica coding agent configurations -*- lexical-binding: t; -*-

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
;; This file includes Fabrica coding agent specific configurations.
;;
;; Fabrica is a terminal-based coding agent built in Rust with built-in
;; ACP support.  See https://github.com/Endi1/fabrica
;;
;; Fabrica supports multiple providers (Google Gemini, Anthropic Claude,
;; OpenAI) and includes built-in tools for file operations and shell
;; commands.
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

(defcustom agent-shell-fabrica-acp-command
  '("fabrica" "serve")
  "Command and parameters for the Fabrica ACP client.

The first element is the command name, and the rest are command parameters.

Fabrica has built-in ACP support via its `serve' subcommand."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-fabrica-make-agent-config ()
  "Create a Fabrica coding agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'fabrica
   :mode-line-name "Fabrica"
   :buffer-name "Fabrica"
   :shell-prompt "Fabrica> "
   :shell-prompt-regexp "Fabrica> "
   :welcome-function #'agent-shell-fabrica--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-fabrica-make-client :buffer buffer))
   :install-instructions "Install with: cargo install fabrica-cli
See https://github.com/Endi1/fabrica for details."))

(defun agent-shell-fabrica-start-agent ()
  "Start an interactive Fabrica coding agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-fabrica-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-fabrica-make-client (&key buffer)
  "Create a Fabrica client using BUFFER as context.

Fabrica uses environment variables for provider authentication.
Set GEMINI_KEY, ANTHROPIC_KEY, or OPENAI_KEY as needed."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-fabrica-acp-command)
                                :command-params (cdr agent-shell-fabrica-acp-command)
                                :environment-variables agent-shell-fabrica-environment
                                :context-buffer buffer))

(defun agent-shell-fabrica--welcome-message (config)
  "Return Fabrica welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-fabrica--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-fabrica--ascii-art ()
  "Fabrica ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
░░░░░░░░░░   ░░░░░░░    ░░░░░░░░░    ░░░░░░░░     ░░░    ░░░░░░░     ░░░░░░░
░░░         ░░░   ░░░   ░░░    ░░░   ░░░    ░░░   ░░░   ░░░   ░░░   ░░░   ░░░
░░░░░░░░    ░░░░░░░░░   ░░░░░░░░░    ░░░░░░░░     ░░░   ░░░         ░░░░░░░░░
░░░         ░░░   ░░░   ░░░    ░░░   ░░░    ░░░   ░░░   ░░░   ░░░   ░░░   ░░░
░░░         ░░░   ░░░   ░░░░░░░░     ░░░    ░░░   ░░░    ░░░░░░░    ░░░   ░░░
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#e0a526" :inherit fixed-pitch)
                                       '(:foreground "#b8860b" :inherit fixed-pitch)))))

(provide 'agent-shell-fabrica)

;;; agent-shell-fabrica.el ends here
