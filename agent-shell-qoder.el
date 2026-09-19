;;; agent-shell-qoder.el --- Qoder agent configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; This file includes Qoder-specific configurations.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(defcustom agent-shell-qoder-acp-command
  '("qoder" "--acp")
  "Command and parameters for the Qoder CLI ACP server.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-qoder-environment
  nil
  "Environment variables for the Qoder CLI ACP server.

This should be a list of environment variables to use when starting Qoder.
For token-based authentication, add `QODER_PERSONAL_ACCESS_TOKEN=...` here."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-qoder-make-agent-config ()
  "Create a Qoder agent configuration."
  (agent-shell-make-agent-config
   :identifier 'qoder
   :mode-line-name "Qoder"
   :buffer-name "Qoder"
   :shell-prompt "Qoder> "
   :shell-prompt-regexp "Qoder> "
   :icon-name "qoder-color.png"
   :client-maker (lambda (buffer)
                   (agent-shell-qoder-make-client :buffer buffer))
   :install-instructions
   "Install Qoder CLI (see https://docs.qoder.com/cli/installation), then run `qoder login`."))

;;;###autoload
(defun agent-shell-qoder-start-agent ()
  "Start an interactive Qoder agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-qoder-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-qoder-make-client (&key buffer)
  "Create a Qoder ACP client with BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-qoder-acp-command)
                                :command-params (cdr agent-shell-qoder-acp-command)
                                :environment-variables agent-shell-qoder-environment
                                :context-buffer buffer))

(provide 'agent-shell-qoder)

;;; agent-shell-qoder.el ends here
