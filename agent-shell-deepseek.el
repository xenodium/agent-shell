;;; agent-shell-deepseek.el --- DeepSeek Harness agent configurations -*- lexical-binding: t; -*-

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
;; This file includes DeepSeek Harness-specific configurations.
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

(cl-defun agent-shell-deepseek-make-authentication (&key api-key)
  "Create DeepSeek Harness authentication configuration.

API-KEY is the DeepSeek API key string or function that returns it."
  (unless api-key
    (error "Must specify :api-key"))
  `((:api-key . ,api-key)))

(defcustom agent-shell-deepseek-authentication
  nil
  "Configuration for DeepSeek Harness authentication.

When nil, the ACP server reads DEEPSEEK_API_KEY from the environment
Emacs was started with.

For API key (string):

  (setq agent-shell-deepseek-authentication
        (agent-shell-deepseek-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-deepseek-authentication
        (agent-shell-deepseek-make-authentication :api-key (lambda () ...)))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-deepseek-acp-command
  '("dsh" "--profile" "acp")
  "Command and parameters for the DeepSeek Harness ACP server.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-deepseek-default-config-options
  nil
  "Default DeepSeek Harness config options, applied at session start.

An alist of (OPTION . VALUE).  Both are the ids listed under
\"Available config options\" when starting a new shell.  OPTION also
accepts the ACP category names (\"model\", \"thought_level\") for
options carrying one.

For example:

  (setq agent-shell-deepseek-default-config-options
        \\='((\"reasoning_effort\" . \"max\")))

Options are applied in the order listed.  An option or value DeepSeek
Harness does not offer is reported and skipped."
  :type '(alist :key-type string :value-type string)
  :group 'agent-shell)

(defcustom agent-shell-deepseek-environment
  nil
  "Environment variables for the DeepSeek Harness ACP server.

This should be a list of environment variables to be used when
starting the DeepSeek Harness ACP server.

Example usage to set custom environment variables:

  (setq agent-shell-deepseek-environment
        (`agent-shell-make-environment-variables'
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))

See `agent-shell-deepseek-authentication' for the API key."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-deepseek-make-agent-config ()
  "Create a DeepSeek Harness agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'deepseek
   :mode-line-name "DeepSeek"
   :buffer-name "DeepSeek"
   :shell-prompt "DeepSeek> "
   :shell-prompt-regexp "DeepSeek> "
   :welcome-function #'agent-shell-deepseek--welcome-message
   :icon-name "deepseek.png"
   :client-maker (lambda (buffer)
                   (agent-shell-deepseek-make-client :buffer buffer))
   :default-config-options (lambda () agent-shell-deepseek-default-config-options)
   :install-instructions
   "Install with `npm install -g @deepseek-ai/dsh' and set
`agent-shell-deepseek-authentication' (or DEEPSEEK_API_KEY).
See https://github.com/deepseek-ai/deepseek-harness for details."))

;;;###autoload
(defun agent-shell-deepseek-start-agent ()
  "Start an interactive DeepSeek Harness agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-deepseek-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-deepseek-make-client (&key buffer)
  "Create a DeepSeek Harness ACP client using BUFFER as context.

See `agent-shell-deepseek-authentication' for authentication configuration."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-deepseek-acp-command)
                                :command-params (cdr agent-shell-deepseek-acp-command)
                                :environment-variables (append (when-let* ((api-key (agent-shell-deepseek-key)))
                                                                 (list (format "DEEPSEEK_API_KEY=%s" api-key)))
                                                               agent-shell-deepseek-environment)
                                :context-buffer buffer))

(defun agent-shell-deepseek-key ()
  "Return the DeepSeek API key from `agent-shell-deepseek-authentication'.

Return nil when unset, leaving DEEPSEEK_API_KEY to the inherited
environment."
  (let ((api-key (map-elt agent-shell-deepseek-authentication :api-key)))
    (cond ((stringp api-key)
           api-key)
          ((functionp api-key)
           (condition-case _err
               (funcall api-key)
             (error
              (error "API key not found.  Check out `agent-shell-deepseek-authentication'")))))))

(defun agent-shell-deepseek--welcome-message (config)
  "Return DeepSeek Harness welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-deepseek--ascii-art)))
        (message (agent-shell-welcome-message config)))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-deepseek--ascii-art ()
  "DeepSeek ASCII art.

The compact whale from DeepSeek Harness's TUI first-run welcome (MIT),
rasterized from the official DeepSeek icon."
  (propertize (string-trim "
     ▄▄▄▄▄▄▄██▀    █▄      ▄
  ▄███████████▄▄   ███▄▄████
 ████████████████▄ ▀██████▀
██▀▀▀▀▀████████████▄▄██▀
██       ▀█████▄ ▀█████
██▄        ▀████▄ ▄████
 ██▄         ████████▀
  ██▄    ▄▄   ▀█████▀
   ▀███▄▄▄███▄  ████▄▄
     ▀▀▀███████▀▀
" "\n")
              'font-lock-face '(:foreground "#4D6BFE" :inherit fixed-pitch)))

(provide 'agent-shell-deepseek)

;;; agent-shell-deepseek.el ends here
