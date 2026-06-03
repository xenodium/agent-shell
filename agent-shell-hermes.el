;;; agent-shell-hermes.el --- Hermes Agent configuration -*- lexical-binding: t; -*-

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
;; This file includes Hermes Agent-specific configurations.
;;

;;; Code:

(defconst agent-shell-hermes-icon-name
  "hermesagent.png"
  "Hermes Agent icon name (from lobe-icons).")

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(defcustom agent-shell-hermes-acp-command
  '("hermes" "acp")
  "Command and parameters for the Hermes agent client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-hermes-environment
  nil
  "Environment variables for the Hermes agent client.

This should be a list of environment variables to be used when
starting the Hermes agent process."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-hermes-make-agent-config ()
  "Create a Hermes agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'hermes
   :mode-line-name "Hermes"
   :buffer-name "Hermes"
   :shell-prompt "Hermes> "
   :shell-prompt-regexp "Hermes> "
   :icon-name agent-shell-hermes-icon-name
   :welcome-function #'agent-shell-hermes--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-hermes-make-client :buffer buffer))
   :install-instructions
   "Defaults to running 'hermes acp' locally.
Customize \\\\[customize-variable] `agent-shell-hermes-acp-command' for remote setups (e.g., via SSH)."))

(defun agent-shell-hermes-start-agent ()
  "Start an interactive Hermes agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-hermes-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-hermes-make-client (&key buffer)
  "Create a Hermes agent ACP client with BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-hermes-acp-command)
                                :command-params (cdr agent-shell-hermes-acp-command)
                                :environment-variables agent-shell-hermes-environment
                                :context-buffer buffer))

(defun agent-shell-hermes--welcome-message (config)
  "Return Hermes welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-hermes--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-hermes--ascii-art ()
  "Hermes ASCII art (banner font)."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         ;; pyfiglet banner font, width=60, HERMES in # characters
         (text (string-trim "
#     # ####### ######  #     # #######  #####
#     # #       #     # ##   ## #       #     #
#     # #       #     # # # # # #       #
####### #####   ######  #  #  # #####    #####
#     # #       #   #   #     # #             #
#     # #       #    #  #     # #       #     #
#     # ####### #     # #     # #######  ##### " "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#e0a050" :inherit fixed-pitch)
                                       '(:foreground "#b07830" :inherit fixed-pitch)))))

(provide 'agent-shell-hermes)

;;; agent-shell-hermes.el ends here
