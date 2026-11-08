;;; agent-shell-mock-agent.el --- Mock ACP agent configuration -*- lexical-binding: t; -*-

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
;; This file includes a mock ACP agent configuration for testing.
;;
;; mock-acp is a deterministic ACP server that exercises all protocol
;; features without requiring an API key or network access.  Each
;; successive prompt cycles through different response patterns:
;; text + tool call, thinking, permission requests, fs read/write,
;; plan, and usage updates.
;;
;; Build the mock-acp binary with:
;;   cd /path/to/mock-acp && swift build
;;
;; Then point `agent-shell-mock-agent-acp-command' at the built binary.

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

(defcustom agent-shell-mock-agent-acp-command
  '("mock-acp")
  "Command and parameters for the mock ACP agent.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-mock-agent-make-agent-config ()
  "Create a mock ACP agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'mock-agent
   :mode-line-name "Mock"
   :buffer-name "Mock"
   :shell-prompt "Mock> "
   :shell-prompt-regexp "Mock> "
   :welcome-function #'agent-shell-mock-agent--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-mock-agent-make-client :buffer buffer))
   :install-instructions "Build mock-acp with: cd /path/to/mock-acp && swift build"))

(defun agent-shell-mock-agent-start-agent ()
  "Start an interactive mock ACP agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-mock-agent-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-mock-agent-make-client (&key buffer)
  "Create a mock ACP client using BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-mock-agent-acp-command)
                                :command-params (cdr agent-shell-mock-agent-acp-command)
                                :context-buffer buffer))

(defun agent-shell-mock-agent--welcome-message (config)
  "Return mock agent welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-mock-agent--ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-mock-agent--ascii-art ()
  "Mock agent ASCII art."
  (let ((is-dark (eq (frame-parameter nil 'background-mode) 'dark)))
    (propertize (string-trim "
    ███╗   ███╗ ██████╗  ██████╗██╗  ██╗
    ████╗ ████║██╔═══██╗██╔════╝██║ ██╔╝
    ██╔████╔██║██║   ██║██║     █████╔╝
    ██║╚██╔╝██║██║   ██║██║     ██╔═██╗
    ██║ ╚═╝ ██║╚██████╔╝╚██████╗██║  ██╗
    ╚═╝     ╚═╝ ╚═════╝  ╚═════╝╚═╝  ╚═╝
" "\n")
                'font-lock-face (if is-dark
                                    '(:foreground "#7ec8e3" :inherit fixed-pitch)
                                  '(:foreground "#2980b9" :inherit fixed-pitch)))))

(provide 'agent-shell-mock-agent)

;;; agent-shell-mock-agent.el ends here
