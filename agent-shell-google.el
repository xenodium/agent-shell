;;; agent-shell-google.el --- Google agent configurations -*- lexical-binding: t; -*-

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
;; This file includes Google-specific configurations.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell--interpolate-gradient "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(cl-defun agent-shell-google-make-authentication (&key api-key login vertex-ai none)
  "Create Google authentication configuration.

API-KEY is the Google API key string or function that returns it.
LOGIN when non-nil indicates to use login-based authentication.
VERTEX-AI when non-nil indicates to use Vertex AI authentication.
NONE when non-nil indicates no authentication method is used.

Only one of API-KEY, LOGIN, VERTEX-AI, or NONE should be provided."
  (when (> (seq-count #'identity (list api-key login vertex-ai)) 1)
    (error "Cannot specify multiple authentication methods - choose one"))
  (unless (> (seq-count #'identity (list api-key login vertex-ai none)) 0)
    (error "Must specify one of :api-key, :login, or :vertex-ai"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (login `((:login . t)))
   (vertex-ai `((:vertex-ai . t)))
   (none `((:none . t)))))

(defcustom agent-shell-google-authentication
  (agent-shell-google-make-authentication :login t)
  "Configuration for Google authentication.

For login-based authentication (default):

  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :login t))

For API key (string):

  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :api-key (lambda () ...)))

For Vertex AI authentication:

  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :vertex-ai t))

For no authentication (when using alternative authentication methods):

  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :none t))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-google-gemini-acp-command
  '("gemini" "--experimental-acp")
  "Command and parameters for the Gemini client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-google-gemini-environment
  nil
  "Environment variables for the Google Gemini client.

This should be a list of environment variables to be used when
starting the Gemini client process.

Example usage to set custom environment variables:

  (setq agent-shell-google-gemini-environment
        (`agent-shell-make-environment-variables'
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-google-make-gemini-config ()
  "Create a Gemini CLI agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (when (and (boundp 'agent-shell-google-key) agent-shell-google-key)
    (user-error "Please migrate to use agent-shell-google-authentication and eval (setq agent-shell-google-key nil)"))
  (agent-shell-make-agent-config
   :identifier 'gemini-cli
   :mode-line-name "Gemini"
   :buffer-name "Gemini"
   :shell-prompt "Gemini> "
   :shell-prompt-regexp "Gemini> "
   :icon-name "gemini.png"
   :welcome-function #'agent-shell-google--gemini-welcome-message
   :needs-authentication (not (map-elt agent-shell-google-authentication :none))
   :authenticate-request-maker (lambda ()
                                 (cond ((map-elt agent-shell-google-authentication :api-key)
                                        ;; TODO: Save authentication methods from
                                        ;; initialization and resolve :method-id
                                        ;; to :method which came from the agent.
                                        (acp-make-authenticate-request
                                         :method-id "gemini-api-key"
                                         :method '((id . "gemini-api-key")
                                                   (name . "Use Gemini API key")
                                                   (description . "Requires setting the `GEMINI_API_KEY` environment variable"))))
                                       ((map-elt agent-shell-google-authentication :vertex-ai)
                                        ;; TODO: Save authentication methods from
                                        ;; initialization and resolve :method-id
                                        ;; to :method which came from the agent.
                                        (acp-make-authenticate-request
                                         :method-id "vertex-ai"
                                         :method '((id . "vertex-ai")
                                                   (name . "Vertex AI")
                                                   (description . ""))))
                                       ((map-elt agent-shell-google-authentication :none)
                                        nil)
                                       (t
                                        ;; TODO: Save authentication methods from
                                        ;; initialization and resolve :method-id
                                        ;; to :method which came from the agent.
                                        (acp-make-authenticate-request
                                         :method-id "oauth-personal"
                                         :method '((id . "oauth-personal")
                                                   (name . "Log in with Google")
                                                   (description . ""))))))
   :client-maker (lambda (buffer)
                   (agent-shell-google-make-gemini-client :buffer buffer))
   :install-instructions "See https://github.com/google-gemini/gemini-cli for installation."))

(defun agent-shell-google-start-gemini ()
  "Start an interactive Gemini CLI agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-google-make-gemini-config)
                     :new-shell t))

(cl-defun agent-shell-google-make-gemini-client (&key buffer)
  "Create a Gemini client using configured authentication with BUFFER as context.

Uses `agent-shell-google-authentication' for authentication configuration."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-google-key) agent-shell-google-key)
    (user-error "Please migrate to use agent-shell-google-authentication and eval (setq agent-shell-google-key nil)"))
  (when (and (boundp 'agent-shell-google-gemini-command) agent-shell-google-gemini-command)
    (user-error "Please migrate to use agent-shell-google-gemini-acp-command and eval (setq agent-shell-google-gemini-command nil)"))
  (cond
   ((map-elt agent-shell-google-authentication :api-key)
    (agent-shell--make-acp-client :command (car agent-shell-google-gemini-acp-command)
                                  :command-params (cdr agent-shell-google-gemini-acp-command)
                                  :environment-variables (append (when-let* ((api-key (agent-shell-google-key)))
                                                                   (list (format "GEMINI_API_KEY=%s" api-key)))
                                                                 agent-shell-google-gemini-environment)
                                  :context-buffer buffer))
   ((map-elt agent-shell-google-authentication :login)
    (agent-shell--make-acp-client :command (car agent-shell-google-gemini-acp-command)
                                  :command-params (cdr agent-shell-google-gemini-acp-command)
                                  :environment-variables agent-shell-google-gemini-environment
                                  :context-buffer buffer))
   ((map-elt agent-shell-google-authentication :vertex-ai)
    (agent-shell--make-acp-client :command (car agent-shell-google-gemini-acp-command)
                                  :command-params (cdr agent-shell-google-gemini-acp-command)
                                  :environment-variables agent-shell-google-gemini-environment
                                  :context-buffer buffer))
   ((map-elt agent-shell-google-authentication :none)
    (agent-shell--make-acp-client :command (car agent-shell-google-gemini-acp-command)
                                  :command-params (cdr agent-shell-google-gemini-acp-command)
                                  :environment-variables agent-shell-google-gemini-environment
                                  :context-buffer buffer))
   (t
    (error "Invalid authentication configuration"))))

(defun agent-shell-google--gemini-welcome-message (config)
  "Return Gemini CLI ASCII art as per own repo using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-google--gemini-ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-google--gemini-ascii-art ()
  "Generate Gemini CLI ASCII art, inspired by its codebase."
  ;; Based on:
  ;; https://github.com/google-gemini/gemini-cli/tree/main/packages/cli/src/ui/components/Header.tsx
  ;; https://github.com/google-gemini/gemini-cli/tree/main/packages/cli/src/ui/components/AsciiArt.ts
  ;; https://github.com/google-gemini/gemini-cli/tree/main/packages/cli/src/ui/themes/theme.ts
  (let* ((text (string-trim "
 ███            █████████  ██████████ ██████   ██████ █████ ██████   █████ █████
░░░███         ███░░░░░███░░███░░░░░█░░██████ ██████ ░░███ ░░██████ ░░███ ░░███
  ░░░███      ███     ░░░  ░███  █ ░  ░███░█████░███  ░███  ░███░███ ░███  ░███
    ░░░███   ░███          ░██████    ░███░░███ ░███  ░███  ░███░░███░███  ░███
     ███░    ░███    █████ ░███░░█    ░███ ░░░  ░███  ░███  ░███ ░░██████  ░███
   ███░      ░░███  ░░███  ░███ ░   █ ░███      ░███  ░███  ░███  ░░█████  ░███
 ███░         ░░█████████  ██████████ █████     █████ █████ █████  ░░█████ █████
░░░            ░░░░░░░░░  ░░░░░░░░░░ ░░░░░     ░░░░░ ░░░░░ ░░░░░    ░░░░░ ░░░░░" "\n"))
         (is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (gradient-colors (if is-dark
                              '("#4796E4" "#847ACE" "#C3677F")
                            '("#3B82F6" "#8B5CF6" "#DD4C4C")))
         (lines (split-string text "\n"))
         (result ""))
    (dolist (line lines)
      (let ((line-length (length line))
            (propertized-line ""))
        (dotimes (i line-length)
          (let* ((char (substring line i (1+ i)))
                 (progress (/ (float i) line-length))
                 (color (agent-shell--interpolate-gradient gradient-colors progress)))
            (setq propertized-line
                  (concat propertized-line
                          (propertize char 'font-lock-face `(:foreground ,color :inherit fixed-pitch))))))
        (setq result (concat result propertized-line "\n"))))
    (string-trim-right result)))

(defun agent-shell-google--gemini-text ()
  "Colorized Gemini text with Google-branded colors."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (colors (if is-dark
                     '("#4796E4" "#6B82D9" "#847ACE" "#9E6FA8" "#B16C93" "#C3677F")
                   '("#3B82F6" "#5F6CF6" "#8B5CF6" "#A757D0" "#C354A0" "#DD4C4C")))
         (text "Gemini")
         (result ""))
    (dotimes (i (length text))
      (setq result (concat result
                           (propertize (substring text i (1+ i))
                                       'font-lock-face `(:foreground ,(nth (mod i (length colors)) colors) :inherit fixed-pitch)))))
    result))

(defun agent-shell-google-key ()
  "Get the Google API key."
  (cond ((stringp (map-elt agent-shell-google-authentication :api-key))
         (map-elt agent-shell-google-authentication :api-key))
        ((functionp (map-elt agent-shell-google-authentication :api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-google-authentication :api-key))
           (error
            "Api key not found.  Check out `agent-shell-google-authentication'")))
        (t
         nil)))

(provide 'agent-shell-google)

;;; agent-shell-google.el ends here
