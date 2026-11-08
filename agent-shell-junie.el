;;; agent-shell-junie.el --- Junie agent configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Álvaro Ramírez

;; Author: Thanh Vuong https://github.com/thanhvg
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
;; This file includes Junie-specific configurations.
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

(defcustom agent-shell-junie-acp-command
  '("junie" "--acp" "true")
  "Command and parameters for the JetBrains Junie ACP client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-junie-environment
  nil
  "Environment variables for the Junie ACP client.

This should be a list of environment variables to be used when
starting the Junie process."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-junie-default-model-id
  nil
  "Default Junie model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-junie-default-session-mode-id
  nil
  "Default Junie session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defun agent-shell-junie-make-agent-config ()
  "Create a Junie agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'junie
   :mode-line-name "Junie"
   :buffer-name "Junie"
   :shell-prompt "Junie> "
   :shell-prompt-regexp "Junie> "
   :welcome-function #'agent-shell-junie--welcome-message
   :icon-name "junie.png"
   :client-maker (lambda (buffer)
                   (agent-shell-junie-make-client :buffer buffer))

   :default-model-id (lambda () agent-shell-junie-default-model-id)
   :default-session-mode-id (lambda () agent-shell-junie-default-session-mode-id)
   :install-instructions "Install the Junie CLI and ensure ACP mode is available (`junie --acp true'). See https://junie.jetbrains.com/docs/junie-cli-acp.html for installation."))

;;;###autoload
(defun agent-shell-junie-start-agent ()
  "Start an interactive Junie agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-junie-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-junie-make-client (&key buffer)
  "Create a Junie ACP client with BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-junie-acp-command)
                                :command-params (cdr agent-shell-junie-acp-command)
                                :environment-variables agent-shell-junie-environment
                                :context-buffer buffer))

(defun agent-shell-junie--welcome-message (config)
  "Return Junie ASCII art using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-junie--ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-junie--ascii-art ()
  "Junie ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
       ///////               ///                           ///
       ///////               ///                           ///
       ///////               ///  ///     ///  /////////         ///////
///////      ///////         ///  ///     ///  //////////  ///  //////////
///////      ///////         ///  ///     ///  ///     /// /// ///     ////
///////     ////////         ///  ///     ///  ///     /// /// ////////////
       ///////////           ///  ///    ////  ///     /// /// ///
       /////////        ////////  //////////   ///     /// ///  ///////////
       //////           //////     ////////    ///     /// ///   ////////
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#dddddd" :inherit fixed-pitch)
                                       '(:foreground "#000000" :inherit fixed-pitch)))))

(provide 'agent-shell-junie)

;;; agent-shell-junie.el ends here
