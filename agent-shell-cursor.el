;;; agent-shell-cursor.el --- Cursor agent configurations -*- lexical-binding: t; -*-

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
;; This file includes Cursor-specific configurations.
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
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--delete-fragment "agent-shell")
(declare-function agent-shell--format-plan "agent-shell")
(declare-function agent-shell--make-permission-button "agent-shell")
(declare-function agent-shell-jump-to-latest-permission-button-row "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell--cancel-idle-timer "agent-shell")
(declare-function agent-shell--emit-event "agent-shell")
(declare-function agent-shell--start-idle-timer "agent-shell")
(declare-function agent-shell--active-requests-p "agent-shell")
(defvar agent-shell-permission-icon)

(cl-defun agent-shell-cursor-make-authentication (&key api-key auth-token login none)
  "Create Cursor authentication configuration.

API-KEY is the Cursor API key string or function that returns it.
AUTH-TOKEN is the Cursor auth token string or function that returns it.
LOGIN when non-nil uses interactive \"cursor_login\" authentication.
NONE when non-nil indicates authentication is handled externally
\(for example via `agent login').

Only one of API-KEY, AUTH-TOKEN, LOGIN, or NONE should be provided."
  (when (> (seq-count #'identity (list api-key auth-token login none)) 1)
    (error "Cannot specify multiple authentication methods - choose one"))
  (unless (> (seq-count #'identity (list api-key auth-token login none)) 0)
    (error "Must specify one of :api-key, :auth-token, :login, or :none"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (auth-token `((:auth-token . ,auth-token)))
   (login `((:login . t)))
   (none `((:none . t)))))

(defcustom agent-shell-cursor-authentication
  (agent-shell-cursor-make-authentication :none t)
  "Configuration for Cursor authentication.

By default authentication is handled externally: `agent-shell' sends no
ACP authenticate request and relies on an existing Cursor login (run
`agent login' once outside Emacs).  This matches how Cursor was used
before and needs no configuration.

Optionally, configure `agent-shell' to manage authentication instead.

For no authentication, handled externally (default):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :none t))

For login-based authentication (agent-shell drives \"cursor_login\"):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :login t))

For API key (string):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :api-key (lambda () ...)))

For auth token (string):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :auth-token \"your-token\"))

For auth token (function):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :auth-token (lambda () ...)))

For no authentication (already authenticated via `agent login'):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :none t))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-cursor-acp-command
  '("agent" "acp")
  "Command and parameters for the Cursor agent client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-cursor-environment
  nil
  "Environment variables for the Cursor agent client.

This should be a list of environment variables to be used when
starting the Cursor agent process."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-cursor--make-text-content-block (text)
  "Wrap TEXT in a standard ACP tool-call content block alist.

Return nil when TEXT is nil or empty.

Examples:

  (agent-shell-cursor--make-text-content-block \"hello\")
    => \\='((type . \"content\")
         (content (type . \"text\") (text . \"hello\")))"
  (when (and (stringp text) (not (string-empty-p text)))
    `((type . "content")
      (content (type . "text") (text . ,text)))))

(defun agent-shell-cursor--content-from-raw-output (raw-output)
  "Convert Cursor-style RAW-OUTPUT alist into ACP content blocks.

Examples:

  ;; raw output
  (agent-shell-cursor--content-from-raw-output \\='((stdout . \"done\")))
    => \\='(((type . \"content\")
          (content (type . \"text\") (text . \"```\\ndone\\n```\"))))

  ;; error message
  (agent-shell-cursor--content-from-raw-output
   \\='((error . \"permission denied\")))
   => \\='(((type . \"content\")
          (content (type . \"text\") (text . \"permission denied\"))))

  ;; read/content
  (agent-shell-cursor--content-from-raw-output
   \\='((content . \"file contents\")))
   => \\='(((type . \"content\")
          (content (type . \"text\") (text . \"file contents\"))))

  ;; grep matches
  (agent-shell-cursor--content-from-raw-output
   \\='((totalMatches . 42) (truncated . t)))
   => \\='(((type . \"content\")
          (content (type . \"text\") (text . \"42 matches (truncated)\"))))

  ;; glob/search results
  (agent-shell-cursor--content-from-raw-output \\='((resultCount . 7)))
   => \\='(((type . \"content\")
          (content (type . \"text\") (text . \"7 results\"))))"
  (when raw-output
    (cond
     ((map-elt raw-output 'error)
      (list (agent-shell-cursor--make-text-content-block
             (map-elt raw-output 'error))))
     ((stringp (map-elt raw-output 'content))
      (list (agent-shell-cursor--make-text-content-block
             (map-elt raw-output 'content))))
     ((or (map-elt raw-output 'stdout)
          (map-elt raw-output 'stderr)
          (map-elt raw-output 'exitCode))
      (let* ((exit (map-elt raw-output 'exitCode))
             (stdout (or (map-elt raw-output 'stdout) ""))
             (stderr (or (map-elt raw-output 'stderr) ""))
             (parts (delq nil
                          (list (when (numberp exit) (format "Exit code: %s" exit))
                                (when (not (string-empty-p stdout)) stdout)
                                (when (and (stringp stderr)
                                           (not (string-empty-p stderr)))
                                  stderr))))
             (text (if parts
                       (mapconcat #'identity parts "\n\n")
                     "(no output)")))
        (list (agent-shell-cursor--make-text-content-block
               (format "```\n%s\n```" text)))))
     ((map-elt raw-output 'totalMatches)
      (list (agent-shell-cursor--make-text-content-block
             (format "%s matches%s"
                     (map-elt raw-output 'totalMatches)
                     (if (map-elt raw-output 'truncated)
                         " (truncated)"
                       "")))))
     ((map-elt raw-output 'resultCount)
      (list (agent-shell-cursor--make-text-content-block
             (format "%s results"
                     (map-elt raw-output 'resultCount)))))
     (t nil))))

(cl-defun agent-shell-cursor--notification-adapter (&key acp-notification)
  "Adapt Cursor ACP notifications by filling tool call content from rawOutput.

When a completed `tool_call_update' notification has `rawOutput' but no
`content', convert `rawOutput' into standard ACP content blocks and
attach them to the notification in place.

Examples:

  ;; completed shell tool with stdout only
  (agent-shell-cursor--notification-adapter :ACP-NOTIFICATION
      \\='((method . \"session/update\")
                 (params (update (sessionUpdate . \"tool_call_update\")
                                 (status . \"completed\")
                                 (rawOutput (stdout . \"done\"))))))

   => \\='((method . \"session/update\")
        (params (update (sessionUpdate . \"tool_call_update\")
                        (status . \"completed\")
                        (content ((type . \"content\")
                                  (content (type . \"text\")
                                           (text . \"```\\ndone\\n```\")))))))


  ;; existing content is left unchanged
  (agent-shell-cursor--notification-adapter :acp-notification
      \\='((method . \"session/update\")
          (params (update (sessionUpdate . \"tool_call_update\")
                          (status . \"completed\")
                          (content ((type . \"content\")
                                    (content (type . \"text\")
                                             (text . \"keep me\"))))
                          (rawOutput (stdout . \"ignored\"))))))
  
    => \\='((method . \"session/update\")
          (params (update (sessionUpdate . \"tool_call_update\")
                          (status . \"completed\")
                          (content ((type . \"content\")
                                      (content (type . \"text\")
                                               (text . \"keep me\"))))
                          (rawOutput (stdout . \"ignored\")))))"
  (when-let* ((method (map-elt acp-notification 'method))
              ((equal method "session/update"))
              (update-type (map-nested-elt acp-notification
                                            '(params update sessionUpdate)))
              ((equal update-type "tool_call_update"))
              ((seq-empty-p (map-nested-elt acp-notification
                                                 '(params update content))))
              (raw-output (map-nested-elt acp-notification
                                          '(params update rawOutput)))
              (content (agent-shell-cursor--content-from-raw-output raw-output)))
    (setf (alist-get 'content (alist-get 'update (alist-get 'params acp-notification)))
          content))
  acp-notification)

(defun agent-shell-cursor-make-agent-config ()
  "Create a Cursor agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'cursor
   :mode-line-name "Cursor"
   :buffer-name "Cursor"
   :shell-prompt "Cursor> "
   :shell-prompt-regexp "Cursor> "
   :icon-name "cursor.png"
   :welcome-function #'agent-shell-cursor--welcome-message
   ;; Only the interactive login flow uses an ACP authenticate request
   ;; (method id \"cursor_login\").  API key and auth token are supplied
   ;; out-of-band via CURSOR_API_KEY / CURSOR_AUTH_TOKEN, and :none is
   ;; authenticated externally (for example via `agent login').
   :needs-authentication (and (map-elt agent-shell-cursor-authentication :login) t)
   :authenticate-request-maker (lambda ()
                                 (acp-make-authenticate-request :method-id "cursor_login"))
   :client-maker (lambda (buffer)
                   (agent-shell-cursor-make-client :buffer buffer))
   :notification-adapter #'agent-shell-cursor--notification-adapter
   :install-instructions "See https://cursor.com/docs/cli for installation."))

(defun agent-shell-cursor-start-agent ()
  "Start an interactive Cursor agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-cursor-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-cursor-make-client (&key buffer)
  "Create a Cursor agent ACP client with BUFFER as context.

Uses `agent-shell-cursor-authentication' for authentication configuration."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-cursor-command) agent-shell-cursor-command)
    (user-error "Please migrate to use agent-shell-cursor-acp-command and eval (setq agent-shell-cursor-command nil)"))
  (agent-shell--make-acp-client :command (car agent-shell-cursor-acp-command)
                                :command-params (cdr agent-shell-cursor-acp-command)
                                :environment-variables (append
                                                        (cond
                                                         ((map-elt agent-shell-cursor-authentication :api-key)
                                                          (let ((api-key (agent-shell-cursor--resolve-secret
                                                                          (map-elt agent-shell-cursor-authentication :api-key))))
                                                            (unless api-key
                                                              (user-error "Please set your `agent-shell-cursor-authentication'"))
                                                            (list (format "CURSOR_API_KEY=%s" api-key))))
                                                         ((map-elt agent-shell-cursor-authentication :auth-token)
                                                          (let ((auth-token (agent-shell-cursor--resolve-secret
                                                                             (map-elt agent-shell-cursor-authentication :auth-token))))
                                                            (unless auth-token
                                                              (user-error "Please set your `agent-shell-cursor-authentication'"))
                                                            (list (format "CURSOR_AUTH_TOKEN=%s" auth-token)))))
                                                        agent-shell-cursor-environment)
                                :context-buffer buffer))

(defun agent-shell-cursor--resolve-secret (value)
  "Resolve VALUE to a string.
VALUE may be a string or a function that returns a string."
  (cond ((stringp value) value)
        ((functionp value)
         (condition-case _err
             (funcall value)
           (error
            (error "Secret not found.  Check out `agent-shell-cursor-authentication'"))))
        (t nil)))

(defun agent-shell-cursor--welcome-message (config)
  "Return Cursor welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-cursor--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-cursor--ascii-art ()
  "Cursor ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
  ██████╗ ██╗   ██╗ ██████╗  ███████╗  ██████╗  ██████╗
 ██╔════╝ ██║   ██║ ██╔══██╗ ██╔════╝ ██╔═══██╗ ██╔══██╗
 ██║      ██║   ██║ ██████╔╝ ███████╗ ██║   ██║ ██████╔╝
 ██║      ██║   ██║ ██╔══██╗ ╚════██║ ██║   ██║ ██╔══██╗
 ╚██████╗ ╚██████╔╝ ██║  ██║ ███████║ ╚██████╔╝ ██║  ██║
  ╚═════╝  ╚═════╝  ╚═╝  ╚═╝ ╚══════╝  ╚═════╝  ╚═╝  ╚═╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#00d4ff" :inherit fixed-pitch)
                                       '(:foreground "#0066cc" :inherit fixed-pitch)))))

(cl-defun agent-shell-cursor--on-request (&key state acp-request)
  "Handle Cursor ACP extension ACP-REQUEST using STATE.

Supports methods documented at
https://cursor.com/docs/cli/acp#cursor-extension-methods.

`cursor/update_todos' is documented as a notification, but Cursor
often sends it as a request; reply so the agent does not stall.

Return non-nil when the request was handled."
  (pcase (map-elt acp-request 'method)
    ("cursor/create_plan"
     (agent-shell-cursor--on-create-plan-request
      :state state
      :acp-request acp-request)
     t)
    ("cursor/ask_question"
     (agent-shell-cursor--on-ask-question-request
      :state state
      :acp-request acp-request)
     t)
    ("cursor/update_todos"
     (agent-shell-cursor--on-update-todos-request
      :state state
      :acp-request acp-request)
     t)
    (_ nil)))

(cl-defun agent-shell-cursor--merge-todos (&key existing incoming merge)
  "Return todo list after applying INCOMING to EXISTING.

When MERGE is non-nil, update or append by `id'.  Otherwise replace
with INCOMING.  EXISTING and INCOMING may be lists or vectors."
  (let ((incoming-list (append incoming nil))
        (existing-list (append existing nil)))
    (if (not merge)
        incoming-list
      (append
       (mapcar (lambda (todo)
                 (or (seq-find (lambda (inc)
                                 (equal (map-elt inc 'id)
                                        (map-elt todo 'id)))
                               incoming-list)
                     todo))
               existing-list)
       (seq-remove (lambda (todo)
                     (seq-find (lambda (ex)
                                 (equal (map-elt ex 'id)
                                        (map-elt todo 'id)))
                               existing-list))
                   incoming-list)))))

(cl-defun agent-shell-cursor--make-extension-response (&key request-id outcome)
  "Build an ACP response for REQUEST-ID with Cursor extension OUTCOME.

OUTCOME is an alist such as ((outcome . \"accepted\"))."
  (unless request-id
    (error ":request-id is required"))
  (unless outcome
    (error ":outcome is required"))
  `((:request-id . ,request-id)
    (:result . ((outcome . ,outcome)))))

(cl-defun agent-shell-cursor--send-extension-response (&key state request-id block-ids outcome message-text interrupt)
  "Send Cursor extension response and clean up dialog UI.

STATE is agent-shell state.  REQUEST-ID identifies the ACP request.
BLOCK-IDS are fragments to remove after responding.  OUTCOME is the
Cursor outcome alist.  MESSAGE-TEXT is shown after sending.  INTERRUPT
non-nil also interrupts the shell after a rejection."
  (acp-send-response
   :client (map-elt state :client)
   :response (agent-shell-cursor--make-extension-response
              :request-id request-id
              :outcome outcome))
  (with-current-buffer (map-elt state :buffer)
    (dolist (block-id (ensure-list block-ids))
      (agent-shell--delete-fragment :state state :block-id block-id))
    (agent-shell--cancel-idle-timer)
    (agent-shell--emit-event
     :event 'cursor-extension-response
     :data (list (cons :request-id request-id)
                 (cons :outcome outcome)))
    (when message-text
      (message "%s" message-text))
    (or (agent-shell-jump-to-latest-permission-button-row)
        (goto-char (point-max)))
    (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                                  :shell-buffer (map-elt state :buffer)
                                  :existing-only t)))
      (with-current-buffer viewport-buffer
        (or (agent-shell-jump-to-latest-permission-button-row)
            (goto-char (point-max)))))
    (when interrupt
      (agent-shell-interrupt t))))

(defun agent-shell-cursor--format-create-plan-body (params)
  "Return display text for cursor/create_plan PARAMS."
  (let* ((name (map-elt params 'name))
         (overview (map-elt params 'overview))
         (plan (map-elt params 'plan))
         (todos (map-elt params 'todos))
         (phases (map-elt params 'phases))
         (parts (delq nil
                      (list
                       (when name
                         (propertize name 'font-lock-face 'agent-shell-section-heading))
                       (when overview overview)
                       (when plan plan)
                       (when (and todos (> (length todos) 0))
                         (concat (propertize "Todos" 'font-lock-face 'agent-shell-section-heading)
                                 "\n"
                                 (agent-shell--format-plan todos)))
                       (when (and phases (> (length phases) 0))
                         (mapconcat
                          (lambda (phase)
                            (concat (propertize (or (map-elt phase 'name) "Phase")
                                                'font-lock-face 'agent-shell-section-heading)
                                    "\n"
                                    (agent-shell--format-plan (map-elt phase 'todos))))
                          phases
                          "\n\n"))))))
    (string-trim (mapconcat #'identity parts "\n\n"))))

(cl-defun agent-shell-cursor--make-create-plan-dialog (&key state request-id block-ids)
  "Return Accept/Reject/Cancel dialog text for create_plan.

STATE, REQUEST-ID, and BLOCK-IDS identify the pending request UI."
  (let* ((shell-buffer (map-elt state :buffer))
         (respond (lambda (outcome message &optional interrupt)
                    (lambda ()
                      (interactive)
                      (agent-shell-cursor--send-extension-response
                       :state state
                       :request-id request-id
                       :block-ids block-ids
                       :outcome outcome
                       :message-text message
                       :interrupt interrupt))))
         (actions (list
                   (list (cons :char "y")
                         (cons :label "Accept (y)")
                         (cons :option "accept")
                         (cons :outcome '((outcome . "accepted")))
                         (cons :message "Plan accepted")
                         (cons :interrupt nil))
                   (list (cons :char "n")
                         (cons :label "Reject (n)")
                         (cons :option "reject")
                         (cons :outcome '((outcome . "rejected")
                                          (reason . "User rejected the plan")))
                         (cons :message "Plan rejected")
                         (cons :interrupt t))
                   (list (cons :char "c")
                         (cons :label "Cancel (c)")
                         (cons :option "cancel")
                         (cons :outcome '((outcome . "cancelled")))
                         (cons :message "Plan cancelled")
                         (cons :interrupt t))))
         (keymap (let ((map (make-sparse-keymap)))
                   (dolist (action actions)
                     (define-key map (kbd (map-elt action :char))
                                 (funcall respond
                                          (map-elt action :outcome)
                                          (map-elt action :message)
                                          (map-elt action :interrupt))))
                   (define-key map (kbd "C-c C-c")
                               (lambda ()
                                 (interactive)
                                 (with-current-buffer shell-buffer
                                   (agent-shell-interrupt t))))
                   map)))
    (format "╭─

    %s %s %s


    %s


╰─"
            (propertize agent-shell-permission-icon
                        'font-lock-face 'agent-shell-warning)
            (propertize "Plan Approval" 'font-lock-face 'agent-shell-permission-title)
            (propertize agent-shell-permission-icon
                        'font-lock-face 'agent-shell-warning)
            (mapconcat
             (lambda (action)
               (agent-shell--make-permission-button
                :text (map-elt action :label)
                :help (map-elt action :label)
                :action (funcall respond
                                 (map-elt action :outcome)
                                 (map-elt action :message)
                                 (map-elt action :interrupt))
                :keymap keymap
                :char (map-elt action :char)
                :option (map-elt action :option)
                :navigatable t))
             actions
             " "))))

(cl-defun agent-shell-cursor--on-create-plan-request (&key state acp-request)
  "Handle blocking cursor/create_plan ACP-REQUEST with STATE."
  (let* ((params (map-elt acp-request 'params))
         (request-id (map-elt acp-request 'id))
         (tool-call-id (or (map-elt params 'toolCallId) request-id))
         (dialog-block-id (format "cursor-create-plan-%s" tool-call-id))
         (plan-block-id (concat dialog-block-id "-body"))
         (block-ids (list plan-block-id dialog-block-id)))
    (agent-shell--update-fragment
     :state state
     :block-id plan-block-id
     :label-left (propertize "Proposed plan" 'font-lock-face 'agent-shell-section-heading)
     :body (agent-shell-cursor--format-create-plan-body params)
     :expanded t)
    (agent-shell--update-fragment
     :state state
     :block-id dialog-block-id
     :body (agent-shell-cursor--make-create-plan-dialog
            :state state
            :request-id request-id
            :block-ids block-ids)
     :expanded t
     :navigation 'never)
    (agent-shell-jump-to-latest-permission-button-row)
    (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                                  :shell-buffer (map-elt state :buffer)
                                  :existing-only t)))
      (with-current-buffer viewport-buffer
        (agent-shell-jump-to-latest-permission-button-row)))
    (let ((data (list (cons :request-id request-id)
                      (cons :tool-call-id tool-call-id)
                      (cons :method "cursor/create_plan"))))
      (agent-shell--emit-event :event 'cursor-create-plan :data data)
      (agent-shell--start-idle-timer :event 'cursor-create-plan :data data))
    (map-put! state :last-entry-type "cursor/create_plan")))

(cl-defun agent-shell-cursor--read-ask-question-answers (questions)
  "Prompt for answers to QUESTIONS from cursor/ask_question.

Return a vector of answer alists, or nil if the user quits."
  (condition-case nil
      (vconcat
       (mapcar
        (lambda (question)
          (let* ((prompt (or (map-elt question 'prompt)
                             (map-elt question 'id)
                             "Select option"))
                 (options (append (map-elt question 'options) nil))
                 (allow-multiple (map-elt question 'allowMultiple))
                 (choices (mapcar (lambda (option)
                                    (cons (or (map-elt option 'label)
                                              (map-elt option 'id))
                                          (map-elt option 'id)))
                                  options))
                 (selected
                  (if allow-multiple
                      (completing-read-multiple
                       (format "%s (comma-separated): " prompt)
                       (mapcar #'car choices)
                       nil t)
                    (list (completing-read
                           (format "%s: " prompt)
                           (mapcar #'car choices)
                           nil t)))))
            `((questionId . ,(map-elt question 'id))
              (selectedOptionIds . ,(vconcat
                                    (mapcar (lambda (label)
                                              (map-elt choices label))
                                            selected))))))
        (append questions nil)))
    (quit nil)))

(cl-defun agent-shell-cursor--on-ask-question-request (&key state acp-request)
  "Handle blocking cursor/ask_question ACP-REQUEST with STATE."
  (let* ((params (map-elt acp-request 'params))
         (request-id (map-elt acp-request 'id))
         (title (map-elt params 'title))
         (questions (map-elt params 'questions))
         (block-id (format "cursor-ask-question-%s"
                           (or (map-elt params 'toolCallId) request-id)))
         (body (string-trim
                (mapconcat
                 (lambda (question)
                   (concat
                    (propertize (or (map-elt question 'prompt)
                                    (map-elt question 'id)
                                    "Question")
                                'font-lock-face 'agent-shell-section-heading)
                    "\n"
                    (mapconcat
                     (lambda (option)
                       (format "  - %s" (or (map-elt option 'label)
                                            (map-elt option 'id))))
                     (append (map-elt question 'options) nil)
                     "\n")))
                 (append questions nil)
                 "\n\n"))))
    (agent-shell--update-fragment
     :state state
     :block-id block-id
     :label-left (propertize (or title "Cursor question")
                             'font-lock-face 'agent-shell-section-heading)
     :body body
     :expanded t)
    (if-let* ((answers (agent-shell-cursor--read-ask-question-answers questions)))
        (agent-shell-cursor--send-extension-response
         :state state
         :request-id request-id
         :block-ids block-id
         :outcome `((outcome . "answered")
                    (answers . ,answers))
         :message-text "Answered Cursor question")
      (agent-shell-cursor--send-extension-response
       :state state
       :request-id request-id
       :block-ids block-id
       :outcome '((outcome . "cancelled"))
       :message-text "Cancelled Cursor question"
       :interrupt t))
    (map-put! state :last-entry-type "cursor/ask_question")))

(cl-defun agent-shell-cursor--on-update-todos-request (&key state acp-request)
  "Handle cursor/update_todos ACP-REQUEST with STATE.

Display the todo list and auto-accept so the agent can continue."
  (let* ((params (map-elt acp-request 'params))
         (request-id (map-elt acp-request 'id))
         (tool-call-id (map-elt params 'toolCallId))
         (merge (map-elt params 'merge))
         (todos (agent-shell-cursor--merge-todos
                 :existing (map-elt state :cursor-todos)
                 :incoming (map-elt params 'todos)
                 :merge merge)))
    (map-put! state :cursor-todos todos)
    (agent-shell--update-fragment
     :state state
     :block-id "cursor-todos"
     :label-left (propertize "Todos" 'font-lock-face 'agent-shell-section-heading)
     :body (agent-shell--format-plan todos)
     :expanded t
     :above-last-prompt (not (agent-shell--active-requests-p state)))
    (agent-shell-cursor--send-extension-response
     :state state
     :request-id request-id
     :outcome `((outcome . "accepted")
                (todos . ,(vconcat todos))))
    (agent-shell--emit-event
     :event 'cursor-update-todos
     :data (list (cons :request-id request-id)
                 (cons :tool-call-id tool-call-id)
                 (cons :merge merge)
                 (cons :todos todos)))
    (map-put! state :last-entry-type "cursor/update_todos")))

(provide 'agent-shell-cursor)

;;; agent-shell-cursor.el ends here
