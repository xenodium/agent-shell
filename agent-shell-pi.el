;;; agent-shell-pi.el --- Pi coding agent configurations -*- lexical-binding: t; -*-

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
;; This file includes Pi coding agent-specific configurations.
;;
;; Pi is a minimal terminal coding agent by Mario Zechner.
;; See https://github.com/badlogic/pi-mono/tree/main/packages/coding-agent
;;
;; This integration requires the pi-acp adapter to be installed.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)
(require 'map)
(require 'seq)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(defcustom agent-shell-pi-acp-command
  '("pi-acp")
  "Command and parameters for the Pi ACP client.

The first element is the command name, and the rest are command parameters.

Pi requires the pi-acp adapter for ACP integration."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-pi-environment
  nil
  "Environment variables for the Pi client.

This should be a list of environment variables to be used when
starting the Pi client process.

Example usage to set custom environment variables:

  (setq agent-shell-pi-environment
        (`agent-shell-make-environment-variables'
         \"ANTHROPIC_API_KEY\" \"your-key\"
         \"PI_CODING_AGENT_DIR\" \"~/.pi/agent\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defvar-local agent-shell-pi--terminal-output-snapshots nil
  "Alist of Pi terminal IDs and accumulated output for the current buffer.")

(defun agent-shell-pi--terminal-output-content (text)
  "Return standard ACP content for terminal output TEXT.

Examples:

  (agent-shell-pi--terminal-output-content \"hello\")
    => (((type . \"content\")
         (content (type . \"text\") (text . \"```\\nhello\\n```\"))))"
  (when (and (stringp text) (not (string-empty-p text)))
    (list `((type . "content")
            (content (type . "text")
                     (text . ,(format "```\n%s\n```" text)))))))

(defun agent-shell-pi--forget-terminal-output (terminal-id)
  "Forget accumulated output for Pi TERMINAL-ID in the current buffer."
  (setq agent-shell-pi--terminal-output-snapshots
        (seq-remove (lambda (entry)
                      (equal (car entry) terminal-id))
                    agent-shell-pi--terminal-output-snapshots)))

(cl-defun agent-shell-pi--notification-adapter (&key acp-notification)
  "Adapt Pi terminal metadata into standard ACP tool-call content.

Pi's ACP adapter emits terminal output as incremental `data' values in
`_meta.terminal_output' and signals completion with `_meta.terminal_exit'.
Accumulate those values by terminal ID and expose the result as regular
text content for `agent-shell'.

Existing tool-call content is left unchanged.  ACP-NOTIFICATION is returned
after adaptation."
  (when-let* ((method (map-elt acp-notification 'method))
              ((equal method "session/update"))
              (update-type (map-nested-elt acp-notification
                                            '(params update sessionUpdate)))
              ((equal update-type "tool_call_update")))
    (let* ((update (map-nested-elt acp-notification '(params update)))
           (terminal-output (map-nested-elt update '(_meta terminal_output)))
           (terminal-exit (map-nested-elt update '(_meta terminal_exit)))
           (terminal-id (or (map-elt terminal-output 'terminal_id)
                            (map-elt terminal-exit 'terminal_id)
                            (map-elt update 'toolCallId))))
      (when (and (stringp terminal-id)
                 (or terminal-output terminal-exit))
        (when-let* ((data (map-elt terminal-output 'data)))
          (when (stringp data)
            (setf (alist-get terminal-id agent-shell-pi--terminal-output-snapshots
                             nil nil #'equal)
                  (concat (alist-get terminal-id agent-shell-pi--terminal-output-snapshots
                                     "" nil #'equal)
                          data))))
        (when-let* ((output (alist-get terminal-id agent-shell-pi--terminal-output-snapshots
                                      nil nil #'equal))
                    ((seq-empty-p (map-elt update 'content)))
                    (content (agent-shell-pi--terminal-output-content output)))
          (setf (alist-get 'content
                           (alist-get 'update
                                      (alist-get 'params acp-notification)))
                content))
        (when terminal-exit
          (agent-shell-pi--forget-terminal-output terminal-id)))))
  acp-notification)

(defun agent-shell-pi-make-agent-config ()
  "Create a Pi coding agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'pi
   :mode-line-name "Pi"
   :buffer-name "Pi"
   :shell-prompt "Pi> "
   :shell-prompt-regexp "Pi> "
   :icon-name "pi.png"
   :welcome-function #'agent-shell-pi--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-pi-make-client :buffer buffer))
   :notification-adapter #'agent-shell-pi--notification-adapter
   :install-instructions "See https://github.com/badlogic/pi-mono/tree/main/packages/coding-agent for Pi installation.
Requires pi-acp adapter for ACP integration."))

;;;###autoload
(defun agent-shell-pi-start-agent ()
  "Start an interactive Pi coding agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-pi-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-pi-make-client (&key buffer)
  "Create a Pi client using BUFFER as context.

Pi uses OAuth login via the `/login' command, so no API key
environment variables are required by default."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-pi-command) agent-shell-pi-command)
    (user-error "Please migrate to use agent-shell-pi-acp-command and eval (setq agent-shell-pi-command nil)"))
  (agent-shell--make-acp-client :command (car agent-shell-pi-acp-command)
                                :command-params (cdr agent-shell-pi-acp-command)
                                :environment-variables agent-shell-pi-environment
                                :context-buffer buffer))

(defun agent-shell-pi--welcome-message (config)
  "Return Pi welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-pi--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-pi--ascii-art ()
  "Pi ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
        ████████████
        ████████████
        ████    ████
        ████    ████
        ████████    ████
        ████████    ████
        ████        ████
        ████        ████
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#ffffff" :inherit fixed-pitch)
                                       '(:foreground "#000000" :inherit fixed-pitch)))))

(provide 'agent-shell-pi)

;;; agent-shell-pi.el ends here
