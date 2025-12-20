;;; agent-shell.el --- A single native shell experience to interact with agentic providers (Claude Code, Cursor, Gemini CLI, Goose, Codex, OpenCode, Qwen, etc.)  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell
;; Version: 0.25.3
;; Package-Requires: ((emacs "29.1") (shell-maker "0.84.4") (acp "0.8.2"))

(defconst agent-shell--version "0.25.3")

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
;; `agent-shell' offers a native `comint' shell experience to
;; interact with any agent powered by ACP (Agent Client Protocol).
;;
;; `agent-shell' currently provides access to Claude Code, Cursor,
;; Gemini CLI, Goose, Codex, OpenCode, and Qwen amongst other agents.
;;
;; This package depends on the `acp' package to provide the ACP layer
;; as per https://agentclientprotocol.com spec.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'acp)
(eval-when-compile
  (require 'cl-lib))
(require 'dired)
(require 'image)
(require 'json)
(require 'map)
(unless (require 'markdown-overlays nil 'noerror)
  (error "Please update 'shell-maker' to v0.84.4 or newer"))
(require 'shell-maker)
(require 'markdown-overlays)
(require 'project)
(require 'transient)
(require 'agent-shell-ui)
(require 'svg nil :noerror)
(require 'agent-shell-anthropic)
(require 'agent-shell-cursor)
(require 'agent-shell-diff)
(require 'agent-shell-google)
(require 'agent-shell-goose)
(require 'agent-shell-openai)
(require 'agent-shell-opencode)
(require 'agent-shell-qwen)
(require 'agent-shell-heartbeat)
(require 'agent-shell-viewport)

(declare-function projectile-current-project-files "projectile")
(declare-function projectile-project-root "projectile")
(declare-function projectile-project-p "projectile")

(defcustom agent-shell-permission-icon "⚠"
  "Icon displayed when shell commands require permission to execute.

You may use \"􀇾\" as an SF Symbol on macOS."
  :type 'string
  :group 'agent-shell)

(defcustom agent-shell-thought-process-icon "💡"
  "Icon displayed during the AI's thought process.

You may use \"􁷘\" as an SF Symbol on macOS."
  :type 'string
  :group 'agent-shell)

(defcustom agent-shell-thought-process-expand-by-default nil
  "Whether thought process sections should be expanded by default.

When nil (the default), thought process sections are collapsed.
When non-nil, thought process sections are expanded."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-tool-use-expand-by-default nil
  "Whether tool use sections should be expanded by default.

When nil (the default), tool use sections are collapsed.
When non-nil, tool use sections are expanded."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-show-config-icons t
  "Whether to show icons in agent config selection."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-path-resolver-function nil
  "Function for resolving remote paths on the local file-system, and vice versa.

Expects a function that takes the path as its single argument, and
returns the resolved path.  Set to nil to disable mapping."
  :type 'function
  :group 'agent-shell)

(defcustom agent-shell-container-command-runner nil
  "Command prefix for executing commands in a container.

When non-nil, both the agent command and shell commands will be
executed using this runner.  Should be a list of command arguments.

Example for devcontainer:
  \\='(\"devcontainer\" \"exec\" \"--workspace-folder\" \".\")"
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-section-functions nil
  "Abnormal hook run after overlays are applied (experimental).
Called in `agent-shell--update-fragment' after all overlays
are applied.  Each function is called with a range alist containing:
  :block       - The block range with :start and :end positions
  :body        - The body range (if present)
  :label-left  - The left label range (if present)
  :label-right - The right label range (if present)
  :padding     - The padding range with :start and :end (if present)"
  :type 'hook
  :group 'agent-shell)

(defcustom agent-shell-highlight-blocks nil
  "Whether or not to highlight source blocks.

Highlighting source blocks is currently turned off by default
as we need a more efficient mechanism.

See https://github.com/xenodium/agent-shell/issues/119"
  :type 'boolean
  :group 'agent-shell)

(cl-defun agent-shell--make-acp-client (&key command
                                             command-params
                                             environment-variables
                                             context-buffer)
  "Create an ACP client, optionally wrapping with container runner.

COMMAND, COMMAND-PARAMS, ENVIRONMENT-VARIABLES, and CONTEXT-BUFFER are
passed through to `acp-make-client'.

If `agent-shell-container-command-runner' is set, the command will be
wrapped with the runner prefix."
  (let* ((full-command (append (list command) command-params))
         (wrapped-command
          (if agent-shell-container-command-runner
              (append agent-shell-container-command-runner
                      full-command)
            full-command)))
    (acp-make-client :command (car wrapped-command)
                     :command-params (cdr wrapped-command)
                     :environment-variables environment-variables
                     :context-buffer context-buffer)))

(defcustom agent-shell-text-file-capabilities t
  "Whether agents are initialized with read/write text file capabilities.

See `acp-make-initialize-request' for details."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-display-action
  '(display-buffer-same-window)
  "Display action for agent shell buffers.
See `display-buffer' for the format of display actions."
  :type '(cons (repeat function) alist)
  :group 'agent-shell)

(defcustom agent-shell-file-completion-enabled nil
  "Non-nil automatically enables file completion when starting shells."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-prefer-viewport-interaction nil
  "Non-nil makes `agent-shell' prefer viewport interaction over shell interaction.

For example, `agent-shell-send*' will insert text into the viewport
buffer instead of the shell buffer.  If no viewport buffer exists, one
will be created."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-embed-file-size-limit 102400
  "Maximum file size in bytes for embedding with ContentBlock::Resource.
Files larger than this will use ContentBlock::ResourceLink instead.
Default is 100KB (102400 bytes)."
  :type 'integer
  :group 'agent-shell)

(defcustom agent-shell-header-style (if (display-graphic-p) 'graphical 'text)
  "Style for agent shell buffer headers.

Can be one of:

 \='graphical: Display header with icon and styled text.
 \='text: Display simple text-only header.
 nil: Display no header."
  :type '(choice (const :tag "Graphical" graphical)
                 (const :tag "Text only" text)
                 (const :tag "No header" nil))
  :group 'agent-shell)

(defcustom agent-shell-show-welcome-message t
  "Non-nil to show welcome message."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-screenshot-command
  (if (eq system-type 'darwin)
      '("/usr/sbin/screencapture" "-i")
    ;; ImageMagick is common on Linux and many other *nix systems.
    '("/usr/bin/import"))
  "The program to use for capturing screenshots.

Assume screenshot file path will be appended to this list."
  :type '(repeat string)
  :group 'agent-shell)

(cl-defun agent-shell-make-agent-config (&key mode-line-name welcome-function
                                              buffer-name shell-prompt shell-prompt-regexp
                                              client-maker
                                              needs-authentication
                                              authenticate-request-maker
                                              default-model-id
                                              default-session-mode-id
                                              icon-name
                                              install-instructions)
  "Create an agent configuration alist.

Keyword arguments:
- MODE-LINE-NAME: Name to display in the mode line
- WELCOME-FUNCTION: Function to call for welcome message
- BUFFER-NAME: Name of the agent buffer
- SHELL-PROMPT: The shell prompt string
- SHELL-PROMPT-REGEXP: Regexp to match the shell prompt
- CLIENT-MAKER: Function to create the client
- NEEDS-AUTHENTICATION: Non-nil authentication is required
- AUTHENTICATE-REQUEST-MAKER: Function to create authentication requests
- DEFAULT-MODEL-ID: Default model ID (function returning value).
- DEFAULT-SESSION-MODE-ID: Default session mode ID (function returning value).
- ICON-NAME: Name of the icon to use
- INSTALL-INSTRUCTIONS: Instructions to show when executable is not found

Returns an alist with all specified values."
  `((:mode-line-name . ,mode-line-name)
    (:welcome-function . ,welcome-function)                     ;; function
    (:buffer-name . ,buffer-name)
    (:shell-prompt . ,shell-prompt)
    (:shell-prompt-regexp . ,shell-prompt-regexp)
    (:client-maker . ,client-maker)                             ;; function
    (:needs-authentication . ,needs-authentication)
    (:authenticate-request-maker . ,authenticate-request-maker) ;; function
    (:default-model-id . ,default-model-id)                     ;; function
    (:default-session-mode-id . ,default-session-mode-id)       ;; function
    (:icon-name . ,icon-name)
    (:install-instructions . ,install-instructions)))

(defun agent-shell--make-default-agent-configs ()
  "Create a list of default agent configs.

This function aggregates agents from OpenAI, Anthropic, Google,
Goose, Cursor, and others."
  (list (agent-shell-anthropic-make-claude-code-config)
        (agent-shell-cursor-make-agent-config)
        (agent-shell-google-make-gemini-config)
        (agent-shell-goose-make-agent-config)
        (agent-shell-openai-make-codex-config)
        (agent-shell-opencode-make-agent-config)
        (agent-shell-qwen-make-agent-config)))

(defcustom agent-shell-agent-configs
  (agent-shell--make-default-agent-configs)
  "The list of known agent configurations.

See `agent-shell-*-make-*-config' for details."
  :type '(repeat (alist :key-type symbol :value-type sexp))
  :group 'agent-shell)

(defcustom agent-shell-preferred-agent-config nil
  "Default configuration to use for all new shells.

If this is set, `agent-shell' will unconditionally use this
config and not prompt you to select one."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'agent-shell)

(defcustom agent-shell-mcp-servers nil
  "List of MCP servers to initialize when creating a new session.

Each element should be an alist representing an MCP server configuration
following the ACP schema for McpServer as defined at:

https://agentclientprotocol.com/protocol/schema#mcpserver

The schema supports three transport variants:

1. Stdio Transport (universally supported):
   ((name . \"server-name\")
    (command . \"/path/to/executable\")
    (args . (\"arg1\" \"arg2\"))
    (env . (((name . \"ENV_VAR\") (value . \"value\")))))

2. HTTP Transport (requires mcpCapabilities.http):
   ((name . \"server-name\")
    (type . \"http\")
    (url . \"https://example.com/mcp\")
    (headers . (((name . \"Authorization\") (value . \"Bearer token\")))))

3. SSE Transport (requires mcpCapabilities.sse):
   ((name . \"server-name\")
    (type . \"sse\")
    (url . \"https://example.com/mcp\")
    (headers . (((name . \"Authorization\") (value . \"Bearer token\")))))

Example configuration with multiple servers:

  (setq agent-shell-mcp-servers
        \='(((name . \"notion\")
           (type . \"http\")
           (url . \"https://mcp.notion.com/mcp\")
           (headers . ()))
          ((name . \"filesystem\")
           (command . \"npx\")
           (args . (\"-y\"
                    \"@modelcontextprotocol/server-filesystem\" \"/tmp\"))
           (env . ()))))"
  :type '(repeat (alist :key-type symbol :value-type sexp))
  :group 'agent-shell)

(cl-defun agent-shell--make-state (&key agent-config buffer client-maker needs-authentication authenticate-request-maker heartbeat)
  "Construct shell agent state with AGENT-CONFIG and BUFFER.

Shell state is provider-dependent and needs CLIENT-MAKER, NEEDS-AUTHENTICATION,
HEARTBEAT, and AUTHENTICATE-REQUEST-MAKER."
  (list (cons :agent-config agent-config)
        (cons :buffer buffer)
        (cons :client nil)
        (cons :client-maker client-maker)
        (cons :heartbeat heartbeat)
        (cons :initialized nil)
        (cons :needs-authentication needs-authentication)
        (cons :authenticate-request-maker authenticate-request-maker)
        (cons :authenticated nil)
        (cons :set-model nil)
        (cons :set-session-mode nil)
        (cons :session (list (cons :id nil)
                             (cons :mode-id nil)
                             (cons :modes nil)))
        (cons :last-entry-type nil)
        (cons :chunked-group-count 0)
        (cons :request-count 0)
        (cons :tool-calls nil)
        (cons :available-commands nil)
        (cons :available-modes nil)
        (cons :prompt-capabilities nil)
        (cons :pending-requests nil)))

(defvar-local agent-shell--state
    (agent-shell--make-state))

(defvar-local agent-shell--transcript-file nil
  "Path to the shell's transcript file.")

(defvar agent-shell--shell-maker-config nil)

;;;###autoload
(defun agent-shell (&optional new-shell)
  "Start or reuse an existing agent shell.

If in a project without a shell, offer to create one.

If already in a shell, invoke `agent-shell-toggle'.

With prefix argument NEW-SHELL, force start a new shell."
  (interactive "P")
  (agent-shell--dwim :new-shell new-shell))

(cl-defun agent-shell--dwim (&key config new-shell)
  "Start or reuse an agent shell with DWIM behavior.

CONFIG is the agent configuration to use.
NEW-SHELL when non-nil forces starting a new shell.

This function respects `agent-shell-prefer-viewport-interaction' and
handles viewport mode detection, existing shell reuse, and project context."
  (if agent-shell-prefer-viewport-interaction
      (if (and (not new-shell)
               (or (derived-mode-p 'agent-shell-viewport-view-mode)
                   (derived-mode-p 'agent-shell-viewport-edit-mode)))
          (agent-shell-toggle)
        (agent-shell-viewport--show-buffer
         :shell-buffer (when new-shell
                         (agent-shell--start :config (or config
                                                         agent-shell-preferred-agent-config
                                                         (agent-shell-select-config
                                                          :prompt "Start new agent: ")
                                                         (error "No agent config found"))
                                             :no-focus t
                                             :new-session t))))
    (if new-shell
        (agent-shell-start :config (or config
                                       agent-shell-preferred-agent-config
                                       (agent-shell-select-config
                                        :prompt "Start new agent: ")
                                       (error "No agent config found")))
      (if (and (not new-shell)
               (derived-mode-p 'agent-shell-mode))
          (let ((text (agent-shell--relevant-text)))
            (agent-shell-toggle)
            (when text
              (agent-shell--insert-to-shell-buffer :text text)))
        (if-let ((existing-shell (seq-first (agent-shell-project-buffers))))
            (let ((text (agent-shell--relevant-text)))
              (agent-shell--display-buffer existing-shell)
              (when text
                (agent-shell--insert-to-shell-buffer :text text)))
          (if-let ((other-project-shell (seq-first (agent-shell-buffers))))
              (if (y-or-n-p "No shells in project.  Start a new one? ")
                  (let ((text (agent-shell--relevant-text)))
                    (agent-shell-start :config (or config
                                                   agent-shell-preferred-agent-config
                                                   (agent-shell-select-config
                                                    :prompt "Start new agent: ")
                                                   (error "No agent config found")))
                    (when text
                      (agent-shell--insert-to-shell-buffer :text text)))
                (let ((text (agent-shell--relevant-text)))
                  (agent-shell--display-buffer other-project-shell)
                  (when text
                    (agent-shell--insert-to-shell-buffer :text text))))
            (let ((text (agent-shell--relevant-text)))
              (agent-shell-start :config (or config
                                             agent-shell-preferred-agent-config
                                             (agent-shell-select-config
                                              :prompt "Start new agent: ")
                                             (error "No agent config found")))
              (when text
                (agent-shell--insert-to-shell-buffer :text text)))))))))

;;;###autoload
(defun agent-shell-toggle ()
  "Toggle agent shell display."
  (interactive)
  (let ((shell-buffer (if agent-shell-prefer-viewport-interaction
                          (agent-shell-viewport--buffer)
                        (agent-shell--current-shell))))
    (unless shell-buffer
      (user-error "No agent shell buffers available for current project"))
    (if-let ((window (get-buffer-window shell-buffer)))
        (if (and (> (count-windows) 1)
                 (not (bound-and-true-p transient--prefix)))
            (delete-window window)
          (switch-to-prev-buffer))
      (agent-shell--display-buffer shell-buffer))))

;;;###autoload
(defun agent-shell-new-shell ()
  "Start a new agent shell.

Always prompts for agent selection, even if existing shells are available."
  (interactive)
  (agent-shell t))

;;;###autoload
(defun agent-shell-prompt-compose ()
  "Compose an `agent-shell' prompt in a dedicated buffer."
  (interactive)
  (agent-shell-viewport--show-buffer))

(cl-defun agent-shell-start (&key config)
  "Programmatically start shell with CONFIG.

See `agent-shell-make-agent-config' for config format."
  (agent-shell--start :config config
                      :no-focus nil
                      :new-session t))

(cl-defun agent-shell--config-icon (&key config)
  "Create icon string for CONFIG if available and icons are enabled.
Returns an empty string if no icon should be displayed."
  (if-let* ((graphics-capable (display-graphic-p))
            (icon-filename (if (map-elt config :icon-name)
                               (agent-shell--fetch-agent-icon
                                (map-elt config :icon-name))
                             (agent-shell--make-agent-fallback-icon
                              (map-elt config :buffer-name) 100))))
      (with-temp-buffer
        (insert-image (create-image icon-filename nil nil
                                    :ascent 'center
                                    :height (frame-char-height)))
        (buffer-string))
    ""))

(cl-defun agent-shell-select-config (&key prompt)
  "Display PROMPT to select an agent config from `agent-shell-agent-configs'."
  (let* ((configs agent-shell-agent-configs)
         (choices (mapcar (lambda (config)
                            (let ((display-name (or (map-elt config :mode-line-name)
                                                    (map-elt config :buffer-name)
                                                    "Unknown Agent"))
                                  (icon (when agent-shell-show-config-icons
                                          (agent-shell--config-icon :config config))))
                              (cons (format "%s%s%s" icon (when icon " ") display-name)
                                    config)))
                          configs))
         (selected-name (completing-read (or prompt "Select agent: ") choices nil t)))
    (map-elt choices selected-name)))

(defun agent-shell-buffers ()
  "Return all shell buffers."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (derived-mode-p 'agent-shell-mode)))
              (buffer-list)))

(defun agent-shell-other-buffer ()
  "Switch to other associated buffer (viewport vs shell)."
  (interactive)
  (cond ((or (derived-mode-p 'agent-shell-viewport-view-mode)
             (derived-mode-p 'agent-shell-viewport-edit-mode))
         (switch-to-buffer (or (agent-shell--shell-buffer
                                :viewport-buffer (current-buffer)
                                :no-create t)
                               "No shell available")))
        ((derived-mode-p 'agent-shell-mode)
         (when-let ((viewport-buffer (or (agent-shell-viewport--buffer
                                          :shell-buffer (current-buffer))
                                         "Not in a shell viewport buffer")))
           (with-current-buffer viewport-buffer
             (when (derived-mode-p 'agent-shell-viewport-view-mode)
               (agent-shell-viewport-refresh)))
           (switch-to-buffer viewport-buffer)))
        (t
         (user-error "Not in an agent-shell buffer"))))

(defun agent-shell-version ()
  "Show `agent-shell' mode version."
  (interactive)
  (message "agent-shell v%s" agent-shell--version))

(defun agent-shell-interrupt (&optional force)
  "Interrupt in-progress request and reject all pending permissions.
When FORCE is non-nil, skip confirmation prompt."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (error "No active session"))
  (when (or force (y-or-n-p "Interrupt?"))
    ;; First cancel all pending permission requests
    (map-do
     (lambda (tool-call-id tool-call-data)
       (when (map-elt tool-call-data :permission-request-id)
         (agent-shell--send-permission-response
          :client (map-elt (agent-shell--state) :client)
          :request-id (map-elt tool-call-data :permission-request-id)
          :cancelled t
          :state (agent-shell--state)
          :tool-call-id tool-call-id)))
     (map-elt (agent-shell--state) :tool-calls))
    ;; Then send the cancel notification
    (acp-send-notification
     :client (map-elt (agent-shell--state) :client)
     :notification (acp-make-session-cancel-notification
                    :session-id (map-nested-elt (agent-shell--state) '(:session :id))
                    :reason "User cancelled"))))

(cl-defun agent-shell--make-shell-maker-config (&key prompt prompt-regexp)
  "Create `shell-maker' configuration with PROMPT and PROMPT-REGEXP."
  (make-shell-maker-config
   :name "agent"
   :prompt prompt
   :prompt-regexp prompt-regexp
   :execute-command
   (lambda (command shell)
     (agent-shell--handle
      :command command
      :shell shell))))

(defvar-keymap agent-shell-mode-map
  :parent shell-maker-mode-map
  :doc "Keymap for `agent-shell-mode'."
  "<tab>" #'agent-shell-next-item
  "<backtab>" #'agent-shell-previous-item
  "n" #'agent-shell-next-item
  "p" #'agent-shell-previous-item
  "C-<tab>" #'agent-shell-cycle-session-mode
  "C-c C-c" #'agent-shell-interrupt
  "C-c C-m" #'agent-shell-set-session-mode
  "C-c C-v" #'agent-shell-set-session-model
  "C-c C-o" #'agent-shell-other-buffer)

(shell-maker-define-major-mode (agent-shell--make-shell-maker-config) agent-shell-mode-map)

(cl-defun agent-shell--handle (&key command shell)
  "Handle SHELL COMMAND (and lazy initialize the ACP stack).

SHELL is from `shell-maker'.

Flow:

  Before a shell COMMAND can be sent as a prompt to the agent, a
  handful of ACP initialization steps must take place (some asynchronously).
  Once all initialization steps are cleared, only then the COMMAND
  can be sent to the agent as a prompt (thus recursive nature of this function).

  -> Initialize ACP client
      |-> Subscribe to ACP events
           |-> Initiate handshake (ie.  initialize RPC)
                |-> Authenticate (optional)
                     |-> Start prompt session
                          |-> Send COMMAND/prompt (finally!)"
  (with-current-buffer (map-elt shell :buffer)
    (unless (derived-mode-p 'agent-shell-mode)
      (error "Not in a shell"))
    (map-put! (agent-shell--state) :request-count
              ;; TODO: Make public in shell-maker.
              (shell-maker--current-request-id))
    (cond ((not (map-elt (agent-shell--state) :client))
           (when-let* ((shell-buffer (map-elt shell :buffer))
                       (viewport-buffer (agent-shell-viewport--buffer
                                         :shell-buffer shell-buffer
                                         :existing-only t)))
             (with-current-buffer viewport-buffer
               (agent-shell-viewport-view-mode)
               (agent-shell-viewport--initialize
                :prompt  command
                :response (agent-shell-viewport--response))))
           (when (agent-shell--initialize-client :shell shell)
             (agent-shell--handle :command command :shell shell)))
          ((or (not (map-nested-elt (agent-shell--state) '(:client :request-handlers)))
               (not (map-nested-elt (agent-shell--state) '(:client :notification-handlers)))
               (not (map-nested-elt (agent-shell--state) '(:client :error-handlers))))
           (when (agent-shell--initialize-subscriptions :shell shell)
             (agent-shell--handle :command command :shell shell)))
          ((not (map-elt (agent-shell--state) :initialized))
           (agent-shell--initiate-handshake
            :shell shell
            :on-initiated (lambda ()
                            (map-put! (agent-shell--state) :initialized t)
                            (agent-shell--handle :command command :shell shell))))
          ((and (map-elt (agent-shell--state) :needs-authentication)
                (not (map-elt (agent-shell--state) :authenticated)))
           (agent-shell--authenticate
            :shell shell
            :on-authenticated (lambda ()
                                (map-put! (agent-shell--state) :authenticated t)
                                (agent-shell--handle :command command :shell shell))))
          ((not (map-nested-elt (agent-shell--state) '(:session :id)))
           (agent-shell--initiate-session
            :shell shell
            :on-session-init (lambda ()
                               (agent-shell--handle :command command :shell shell))))
          ((and (map-nested-elt (agent-shell--state) '(:agent-config :default-model-id))
                (funcall (map-nested-elt (agent-shell--state)
                                         '(:agent-config :default-model-id)))
                (not (map-elt (agent-shell--state) :set-model)))
           (agent-shell--set-default-model
            :shell shell
            :model-id (funcall (map-nested-elt (agent-shell--state)
                                               '(:agent-config :default-model-id)))
            :on-model-changed (lambda ()
                                (map-put! (agent-shell--state) :set-model t)
                                (agent-shell--handle :command command :shell shell))))
          ((and (map-nested-elt (agent-shell--state) '(:agent-config :default-session-mode-id))
                (funcall (map-nested-elt (agent-shell--state) '(:agent-config :default-session-mode-id)))
                (not (map-elt (agent-shell--state) :set-session-mode)))
           (agent-shell--set-default-session-mode
            :shell shell
            :mode-id (funcall (map-nested-elt (agent-shell--state) '(:agent-config :default-session-mode-id)))
            :on-mode-changed (lambda ()
                               (map-put! (agent-shell--state) :set-session-mode t)
                               (agent-shell--handle :command command :shell shell))))
          (t
           (agent-shell--send-command :prompt command :shell shell)))))

(cl-defun agent-shell--on-error (&key state error)
  "Handle ERROR with SHELL an STATE."
  (let-alist error
    (agent-shell--update-fragment
     :state state
     :block-id "Error"
     :body (or .message "Some error ¯\\_ (ツ)_/¯")
     :create-new t
     :navigation 'never)))

(cl-defun agent-shell--on-notification (&key state notification)
  "Handle incoming notification using SHELL, STATE, and NOTIFICATION."
  (let-alist notification
    (cond ((equal .method "session/update")
           (let ((update (map-elt (map-elt notification 'params) 'update)))
             (cond
              ((equal (map-elt update 'sessionUpdate) "tool_call")
               (agent-shell--save-tool-call
                state
                (map-elt update 'toolCallId)
                (append (list (cons :title (if (and (string= (map-elt update 'title) "Skill")
                                                    (map-nested-elt update '(rawInput command)))
                                               (format "Skill: %s"
                                                       (map-nested-elt update '(rawInput command)))
                                             (map-elt update 'title)))
                              (cons :status (map-elt update 'status))
                              (cons :kind (map-elt update 'kind))
                              (cons :command (map-nested-elt update '(rawInput command)))
                              (cons :description (map-nested-elt update '(rawInput description)))
                              (cons :content (map-elt update 'content)))
                        (when-let ((diff (agent-shell--make-diff-info (map-elt update 'content))))
                          (list (cons :diff diff)))))
               (let ((tool-call-labels (agent-shell-make-tool-call-label
                                        state (map-elt update 'toolCallId))))
                 (agent-shell--update-fragment
                  :state state
                  :block-id (map-elt update 'toolCallId)
                  :label-left (map-elt tool-call-labels :status)
                  :label-right (map-elt tool-call-labels :title)
                  :expanded agent-shell-tool-use-expand-by-default)
                 ;; Display plan as markdown block if present
                 (when-let ((plan (map-nested-elt update '(rawInput plan))))
                   (agent-shell--update-fragment
                    :state state
                    :block-id (concat (map-elt update 'toolCallId) "-plan")
                    :label-left (propertize "Proposed plan" 'font-lock-face 'font-lock-doc-markup-face)
                    :body plan
                    :expanded t)))
               (map-put! state :last-entry-type "tool_call"))
              ((equal (map-elt update 'sessionUpdate) "agent_thought_chunk")
               (let-alist update
                 ;; (message "agent_thought_chunk: last-type=%s, will-append=%s"
                 ;;          (map-elt state :last-entry-type)
                 ;;          (equal (map-elt state :last-entry-type) "agent_thought_chunk"))
                 (unless (equal (map-elt state :last-entry-type)
                                "agent_thought_chunk")
                   (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count))))
                 (agent-shell--update-fragment
                  :state state
                  :block-id (format "%s-agent_thought_chunk"
                                    (map-elt state :chunked-group-count))
                  :label-left  (concat
                                agent-shell-thought-process-icon
                                " "
                                (propertize "Thought process" 'font-lock-face font-lock-doc-markup-face))
                  :body .content.text
                  :append (equal (map-elt state :last-entry-type)
                                 "agent_thought_chunk")
                  :expanded agent-shell-thought-process-expand-by-default))
               (map-put! state :last-entry-type "agent_thought_chunk"))
              ((equal (map-elt update 'sessionUpdate) "agent_message_chunk")
               (unless (equal (map-elt state :last-entry-type) "agent_message_chunk")
                 (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count)))
                 (agent-shell--append-transcript
                  :text (format "## Agent (%s)\n\n" (format-time-string "%F %T"))
                  :file-path agent-shell--transcript-file))
               (let-alist update
                 (agent-shell--append-transcript
                  :text .content.text
                  :file-path agent-shell--transcript-file)
                 (agent-shell--update-fragment
                  :state state
                  :block-id (format "%s-agent_message_chunk"
                                    (map-elt state :chunked-group-count))
                  :body .content.text
                  :create-new (not (equal (map-elt state :last-entry-type)
                                          "agent_message_chunk"))
                  :append t
                  :navigation 'never))
               (map-put! state :last-entry-type "agent_message_chunk"))
              ((equal (map-elt update 'sessionUpdate) "plan")
               (let-alist update
                 (agent-shell--update-fragment
                  :state state
                  :block-id "plan"
                  :label-left (propertize "Plan" 'font-lock-face 'font-lock-doc-markup-face)
                  :body (agent-shell--format-plan .entries)
                  :expanded t))
               (map-put! state :last-entry-type "plan"))
              ((equal (map-elt update 'sessionUpdate) "tool_call_update")
               (let-alist update
                 ;; Update stored tool call data with new status and content
                 (agent-shell--save-tool-call
                  state
                  .toolCallId
                  (append (list (cons :status (map-elt update 'status))
                                (cons :content (map-elt update 'content)))
                          (when-let ((diff (agent-shell--make-diff-info (map-elt update 'content))))
                            (list (cons :diff diff)))))
                 (let* ((diff (map-nested-elt state `(:tool-calls ,.toolCallId :diff)))
                        (output (concat
                                 "\n\n"
                                 ;; TODO: Consider if there are other
                                 ;; types of content to display.
                                 (mapconcat (lambda (item)
                                              (let-alist item
                                                .content.text))
                                            .content
                                            "\n\n")
                                 "\n\n"))
                        (diff-text (agent-shell--format-diff-as-text diff))
                        (body-text (if diff-text
                                       (concat output
                                               "\n\n"
                                               "╭─────────╮\n"
                                               "│ changes │\n"
                                               "╰─────────╯\n\n" diff-text)
                                     output)))
                   ;; Log tool call to transcript when completed or failed
                   (when (and (map-elt update 'status)
                              (member (map-elt update 'status) '("completed" "failed")))
                     (agent-shell--append-transcript
                      :text (agent-shell--make-transcript-tool-call-entry
                             :status (map-elt update 'status)
                             :title (map-nested-elt state `(:tool-calls ,.toolCallId :title))
                             :kind (map-nested-elt state `(:tool-calls ,.toolCallId :kind))
                             :description (map-nested-elt state `(:tool-calls ,.toolCallId :description))
                             :command (map-nested-elt state `(:tool-calls ,.toolCallId :command))
                             :output body-text)
                      :file-path agent-shell--transcript-file))
                   ;; Hide permission after sending response.
                   ;; Status and permission are no longer pending. User
                   ;; likely selected one of: accepted/rejected/always.
                   ;; Remove stale permission dialog.
                   (when (and (map-elt update 'status)
                              (not (equal (map-elt update 'status) "pending")))
                     ;; block-id must be the same as the one used as
                     ;; agent-shell--update-fragment param by "session/request_permission".
                     (agent-shell--delete-fragment :state state :block-id (format "permission-%s" .toolCallId)))
                   (let ((tool-call-labels (agent-shell-make-tool-call-label
                                            state .toolCallId)))
                     (agent-shell--update-fragment
                      :state state
                      :block-id .toolCallId
                      :label-left (map-elt tool-call-labels :status)
                      :label-right (map-elt tool-call-labels :title)
                      :body (string-trim body-text)
                      :expanded agent-shell-tool-use-expand-by-default))))
               (map-put! state :last-entry-type "tool_call_update"))
              ((equal (map-elt update 'sessionUpdate) "available_commands_update")
               (let-alist update
                 (map-put! state :available-commands (map-elt update 'availableCommands))
                 (agent-shell--update-fragment
                  :state state
                  :block-id "available_commands_update"
                  :label-left (propertize "Available commands" 'font-lock-face 'font-lock-doc-markup-face)
                  :body (agent-shell--format-available-commands (map-elt update 'availableCommands))))
               (map-put! state :last-entry-type "available_commands_update"))
              ((equal (map-elt update 'sessionUpdate) "current_mode_update")
               (let ((updated-session (map-elt state :session))
                     (new-mode-id (map-elt update 'currentModeId)))
                 (map-put! updated-session :mode-id new-mode-id)
                 (map-put! state :session updated-session)
                 (message "Session mode: %s"
                          (agent-shell--resolve-session-mode-name
                           new-mode-id
                           (agent-shell--get-available-modes state)))
                 ;; Note: No need to set :last-entry-type as no text was inserted.
                 (agent-shell--update-header-and-mode-line)))
              (t
               (agent-shell--update-fragment
                :state state
                :block-id "Session Update - fallback"
                :body (format "%s" notification)
                :create-new t
                :navigation 'never)
               (map-put! state :last-entry-type nil)))))
          (t
           (agent-shell--update-fragment
            :state state
            :block-id "Notification - fallback"
            :body (format "Unhandled notification (%s) and include:

```json
%s
```"
                          (agent-shell-ui-add-action-to-text
                           "please file a feature request"
                           (lambda ()
                             (interactive)
                             (browse-url "https://github.com/xenodium/agent-shell/issues/new/choose"))
                           (lambda ()
                             (message "Press RET to open URL"))
                           'link)
                          (with-temp-buffer
                            (insert (json-serialize notification))
                            (json-pretty-print (point-min) (point-max))
                            (buffer-string)))
            :create-new t
            :navigation 'never)
           (map-put! state :last-entry-type nil)))))

(cl-defun agent-shell--on-request (&key state request)
  "Handle incoming request using SHELL, STATE, and REQUEST."
  (let-alist request
    (cond ((equal .method "session/request_permission")
           (agent-shell--save-tool-call
            state .params.toolCall.toolCallId
            (append (list (cons :title .params.toolCall.title)
                          (cons :status .params.toolCall.status)
                          (cons :kind .params.toolCall.kind)
                          (cons :permission-request-id .id))
                    (when-let ((diff (agent-shell--make-diff-info .params.toolCall.content)))
                      (list (cons :diff diff)))))
           (agent-shell--update-fragment
            :state state
            ;; block-id must be the same as the one used
            ;; in agent-shell--delete-fragment param.
            :block-id (format "permission-%s" .params.toolCall.toolCallId)
            :body (with-current-buffer (map-elt state :buffer)
                    (agent-shell--make-tool-call-permission-text
                     :request request
                     :client (map-elt state :client)
                     :state state))
            :expanded t
            :navigation 'never)
           (agent-shell-jump-to-latest-permission-button-row)
           (when-let* ((shell-buffer (map-elt state :buffer))
                       (viewport-buffer (agent-shell-viewport--buffer
                                         :shell-buffer shell-buffer
                                         :existing-only t)))
             (with-current-buffer viewport-buffer
               (agent-shell-jump-to-latest-permission-button-row)))
           (map-put! state :last-entry-type "session/request_permission"))
          ((equal .method "fs/read_text_file")
           (agent-shell--on-fs-read-text-file-request
            :state state
            :request request))
          ((equal .method "fs/write_text_file")
           (agent-shell--on-fs-write-text-file-request
            :state state
            :request request))
          (t
           (agent-shell--update-fragment
            :state state
            :block-id "Unhandled Incoming Request"
            :body (format "⚠ Unhandled incoming request: \"%s\"" .method)
            :create-new t
            :navigation 'never)
           (map-put! state :last-entry-type nil)))))

(cl-defun agent-shell--extract-buffer-text (&key buffer line limit)
  "Extract text from BUFFER starting from LINE with optional LIMIT.
If the buffer's file has changed, prompt the user to reload it."
  (with-current-buffer buffer
    (when (and (buffer-file-name)
               (not (verify-visited-file-modtime))
               (y-or-n-p (format "%s has changed on file.  Reload? "
                                 (buffer-name))))
      (revert-buffer t nil nil))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (and line (> line 1))
          ;; Seems odd to use forward-line but
          ;; that's what `goto-line' recommends.
          (forward-line (1- line)))
        (let ((start (point)))
          (if limit
              ;; Seems odd to use forward-line but
              ;; that's what `goto-line' recommends.
              (forward-line limit)
            (goto-char (point-max)))
          (buffer-substring-no-properties start (point)))))))

(cl-defun agent-shell--on-fs-read-text-file-request (&key state request)
  "Handle fs/read_text_file REQUEST with STATE."
  (let-alist request
    (condition-case err
        (let* ((path (agent-shell--resolve-path .params.path))
               (line (or .params.line 1))
               (limit .params.limit)
               (existing-buffer (find-buffer-visiting path))
               (content (if existing-buffer
                            ;; Read from open buffer (includes unsaved changes)
                            (agent-shell--extract-buffer-text :buffer existing-buffer :line line :limit limit)
                          ;; No open buffer, read from file
                          (with-temp-buffer
                            (insert-file-contents path)
                            (agent-shell--extract-buffer-text :buffer (current-buffer) :line line :limit limit)))))
          (acp-send-response
           :client (map-elt state :client)
           :response (acp-make-fs-read-text-file-response
                      :request-id .id
                      :content content)))
      (quit
       ;; Handle C-g interrupts during file read prompts
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-read-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message "Operation cancelled by user"))))
      (error
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-read-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message (error-message-string err))))))))

(cl-defun agent-shell--on-fs-write-text-file-request (&key state request)
  "Handle fs/write_text_file REQUEST with STATE."
  (let-alist request
    (condition-case err
        (let* ((path (agent-shell--resolve-path .params.path))
               (content .params.content)
               (dir (file-name-directory path))
               (buffer (or (find-buffer-visiting path)
                           (find-file-noselect path))))
          (when (and dir (not (file-exists-p dir)))
            (make-directory dir t))
          (with-temp-buffer
            (insert content)
            (let ((content-buffer (current-buffer))
                  (inhibit-read-only t))
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  (replace-buffer-contents content-buffer)
                  (basic-save-buffer)))))
          (acp-send-response
           :client (map-elt state :client)
           :response (acp-make-fs-write-text-file-response
                      :request-id .id)))
      (quit
       ;; Handle C-g interrupts during file save prompts
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-write-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message "Operation cancelled by user"))))
      (error
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-write-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message (error-message-string err))))))))

(defun agent-shell--resolve-path (path)
  "Resolve PATH using `agent-shell-path-resolver-function'."
  (funcall (or agent-shell-path-resolver-function #'identity) path))

(defun agent-shell--get-devcontainer-workspace-path (cwd)
  "Return devcontainer workspaceFolder for CWD, or default value if none found.

See https://containers.dev for more information on devcontainers."
  (let ((devcontainer-config-file-name (expand-file-name ".devcontainer/devcontainer.json" cwd)))
    (condition-case _err
        (map-elt (json-read-file devcontainer-config-file-name) 'workspaceFolder
                 (concat "/workspaces/" (file-name-nondirectory (directory-file-name cwd))))
      (file-missing (error "Not found: %s" devcontainer-config-file-name))
      (permission-denied (error "Not readable: %s" devcontainer-config-file-name))
      (json-string-format (error "No valid JSON: %s" devcontainer-config-file-name)))))

(defun agent-shell--resolve-devcontainer-path (path)
  "Resolve PATH from a devcontainer in the local filesystem, and vice versa.

For example:

- /workspace/README.md => /home/xenodium/projects/kitchen-sink/README.md
- /home/xenodium/projects/kitchen-sink/README.md => /workspace/README.md"
  (let* ((cwd (agent-shell-cwd))
         (devcontainer-path (agent-shell--get-devcontainer-workspace-path cwd)))
    (if (string-prefix-p cwd path)
        (string-replace cwd devcontainer-path path)
      (if agent-shell-text-file-capabilities
          (if-let* ((is-dev-container (string-prefix-p devcontainer-path path))
                    (local-path (expand-file-name (string-replace devcontainer-path cwd path))))
              (or
               (and (file-in-directory-p local-path cwd) local-path)
               (error "Resolves to path outside of working directory: %s" path))
            (error "Unexpected path outside of workspace folder: %s" path))
        (error "Refuse to resolve to local filesystem with text file capabilities disabled: %s" path)))))

(defun agent-shell--stop-reason-description (stop-reason)
  "Return a human-readable text description for STOP-REASON.

https://agentclientprotocol.com/protocol/schema#param-stop-reason"
  (pcase stop-reason
    ("end_turn" "Finished")
    ("max_tokens" "Max token limit reached")
    ("max_turn_requests" "Exceeded request limit")
    ("refusal" "Refused")
    ("cancelled" "Cancelled")
    (_ (format "Stop for unknown reason: %s" stop-reason))))

(defun agent-shell--format-available-commands (commands)
  "Format COMMANDS for shell rendering."
  (agent-shell--align-alist
   :data commands
   :columns (list
             (lambda (cmd)
               (propertize (concat "/" (map-elt cmd 'name))
                           'font-lock-face 'font-lock-function-name-face))
             (lambda (cmd)
               (propertize (map-elt cmd 'description)
                           'font-lock-face 'font-lock-comment-face)))
   :joiner "\n"))

(defun agent-shell--format-agent-capabilities (capabilities)
  "Format agent CAPABILITIES for shell rendering.

CAPABILITIES is as per ACP spec:

  https://agentclientprotocol.com/protocol/schema#agentcapabilities

Groups capabilities by category and displays them as comma-separated values.

Example output:

  prompt        image, and embedded context
  mcp           http, and sse"
  (let* ((case-fold-search nil)
         (categories (delq nil
                           (mapcar
                            (lambda (pair)
                              (let* ((key (if (symbolp (car pair))
                                              (symbol-name (car pair))
                                            (car pair)))
                                     (value (cdr pair))
                                     ;; "prompt Capabilities" -> "prompt"
                                     (group-name (replace-regexp-in-string
                                                  " Capabilities$" ""
                                                  ;; "promptCapabilities" -> "prompt Capabilities"
                                                  (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" key))))
                                (cond
                                 ;; Nested capability groups (promptCapabilities, mcpCapabilities)
                                 ((and (listp value)
                                       (not (vectorp value))
                                       (consp (car value)))
                                  (when-let ((enabled-items (delq nil (mapcar
                                                                       (lambda (cap-pair)
                                                                         (when (eq (cdr cap-pair) t)
                                                                           (let* ((cap-key (car cap-pair))
                                                                                  (cap-name (if (symbolp cap-key)
                                                                                                (symbol-name cap-key)
                                                                                              cap-key)))
                                                                             (downcase
                                                                              (replace-regexp-in-string
                                                                               "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2"
                                                                               cap-name)))))
                                                                       value))))
                                    (cons (downcase group-name)
                                          (if (= (length enabled-items) 1)
                                              (car enabled-items)
                                            (concat (string-join (butlast enabled-items) ", ")
                                                    " and "
                                                    (car (last enabled-items)))))))
                                 ;; Top-level boolean capabilities (loadSession)
                                 ((eq value t)
                                  (cons (downcase group-name) nil)))))
                            capabilities))))
    (agent-shell--align-alist
     :data categories
     :columns (list
               (lambda (pair)
                 (propertize (car pair)
                             'font-lock-face 'font-lock-function-name-face))
               (lambda (pair)
                 (when (cdr pair)
                   (propertize (cdr pair)
                               'font-lock-face 'font-lock-comment-face))))
     :joiner "\n")))

(defun agent-shell--make-diff-info (acp-content)
  "Make diff information from ACP's tool_call_update's ACP-CONTENT.

CONTENT is of the the type ToolCallContent as per ACP spec:

https://agentclientprotocol.com/protocol/schema#toolcallcontent

Returns in the form:

 `((:old . old-text)
   (:new . new-text)
   (:file . file-path))."
  (when-let* ((diff-item (cond
                          ;; Single diff object
                          ((and acp-content (equal (map-elt acp-content 'type) "diff"))
                           acp-content)
                          ;; TODO: Is this needed?
                          ;; Isn't acp-content always an alist?
                          ;; Vector/array acp-content - find diff item
                          ((vectorp acp-content)
                           (seq-find (lambda (item)
                                       (equal (map-elt item 'type) "diff"))
                                     acp-content))
                          ;; TODO: Is this needed?
                          ;; Isn't acp-content always an alist?
                          ;; List acp-content - find diff item
                          ((listp acp-content)
                           (seq-find (lambda (item)
                                       (equal (map-elt item 'type) "diff"))
                                     acp-content))))
              ;; oldText can be nil for Write tools creating new files, default to ""
              ;; TODO: Currently don't have a way to capture overwrites
              (old-text (or (map-elt diff-item 'oldText) ""))
              (new-text (map-elt diff-item 'newText))
              (file-path (map-elt diff-item 'path)))
    (append (list (cons :old old-text)
                  (cons :new new-text))
            (when file-path
              (list (cons :file file-path))))))


(defun agent-shell--format-diff-as-text (diff)
  "Format DIFF info as text suitable for display in tool call body.

DIFF should be in the form returned by `agent-shell--make-diff-info':
  ((:old . old-text) (:new . new-text) (:file . file-path))"
  (when-let (((identity diff))
             (old-file (make-temp-file "old"))
             (new-file (make-temp-file "new")))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert (map-elt diff :old)))
          (with-temp-file new-file (insert (map-elt diff :new)))
          (with-temp-buffer
            (call-process "diff" nil t nil "-U3" old-file new-file)
            ;; Remove file header lines with timestamps
            (goto-char (point-min))
            (when (looking-at "^---")
              (delete-region (point) (progn (forward-line 1) (point))))
            (when (looking-at "^\\+\\+\\+")
              (delete-region (point) (progn (forward-line 1) (point))))
            ;; Apply diff syntax highlighting
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line-start (point))
                    (line-end (line-end-position)))
                (cond
                 ;; Removed lines (start with -)
                 ((looking-at "^-")
                  (add-text-properties line-start line-end
                                       '(font-lock-face diff-removed)))
                 ;; Added lines (start with +)
                 ((looking-at "^\\+")
                  (add-text-properties line-start line-end
                                       '(font-lock-face diff-added)))
                 ;; Hunk headers (@@)
                 ((looking-at "^@@")
                  (add-text-properties line-start line-end
                                       '(font-lock-face diff-hunk-header))))
                (forward-line 1)))
            (buffer-string)))
      (delete-file old-file)
      (delete-file new-file))))
(cl-defun agent-shell--make-error-handler (&key state shell)
  "Create ACP error handler with SHELL STATE."
  (lambda (error raw-message)
    (let-alist error
      (with-current-buffer (map-elt state :buffer)
        (agent-shell--update-fragment
         :state (agent-shell--state)
         :block-id (format "failed-%s-id:%s-code:%s"
                           (map-elt state :request-count)
                           (or .id "?")
                           (or .code "?"))
         :body (agent-shell--make-error-dialog-text
                :code .code
                :message .message
                :raw-message raw-message)
         :create-new t)))
    ;; TODO: Mark buffer command with shell failure.
    (with-current-buffer (map-elt state :buffer)
      (funcall (map-elt shell :finish-output) t))))

(defun agent-shell--save-tool-call (state tool-call-id tool-call)
  "Store TOOL-CALL with TOOL-CALL-ID in STATE's :tool-calls alist."
  (let* ((tool-calls (map-elt state :tool-calls))
         (old-tool-call (map-elt tool-calls tool-call-id))
         (updated-tools (copy-alist tool-calls))
         (tool-call-overrides (seq-filter (lambda (pair)
                                            (cdr pair))
                                          tool-call)))
    (setf (map-elt updated-tools tool-call-id)
          (if old-tool-call
              (map-merge 'alist old-tool-call tool-call-overrides)
            tool-call-overrides))
    (map-put! state :tool-calls updated-tools)))

(cl-defun agent-shell--make-error-dialog-text (&key code message raw-message)
  "Create formatted error dialog text with CODE, MESSAGE, and RAW-MESSAGE."
  (format "╭─

  %s Error (%s) %s

  %s

  %s

╰─"
          (propertize "⚠" 'font-lock-face 'error)
          (or code "?")
          (propertize "⚠" 'font-lock-face 'error)
          (or message "¯\\_ (ツ)_/¯")
          (agent-shell--make-button
           :text "Details" :help "Details" :kind 'error
           :action (lambda ()
                     (interactive)
                     (agent-shell--view-as-error
                      (with-temp-buffer
                        (let ((print-circle t))
                          (pp raw-message (current-buffer))
                          (buffer-string))))))))

(defun agent-shell--view-as-error (text)
  "Display TEXT in a `read-only' error buffer."
  (let ((buf (get-buffer-create "*acp error*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (read-only-mode 1))
    (display-buffer buf)))

(defun agent-shell--clean-up ()
  "Clean up resources.

For example, shut down ACP client."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (when (map-elt (agent-shell--state) :client)
    (acp-shutdown :client (map-elt (agent-shell--state) :client)))
  (agent-shell-heartbeat-stop
   :heartbeat (map-elt (agent-shell--state) :heartbeat))
  (when-let* ((shell-buffer (map-elt (agent-shell--state) :buffer))
              (viewport-buffer (agent-shell-viewport--buffer
                                :shell-buffer shell-buffer
                                :existing-only t))
              (buffer-live-p viewport-buffer))
    (kill-buffer viewport-buffer)))

(cl-defun agent-shell--capture-screenshot (&key destination-dir)
  "Capture a screenshot and save it to DESTINATION-DIR.

Returns the full path to the captured screenshot file on success.
Signals an error on failure.

DESTINATION-DIR is required and must be provided."
  (unless destination-dir
    (error "Destination-dir is required"))
  (let* ((file-path (expand-file-name
                     (format "screenshot-%s.png"
                             (format-time-string "%Y%m%d-%H%M%S"))
                     destination-dir))
         (command (car agent-shell-screenshot-command))
         (args (append (cdr agent-shell-screenshot-command)
                       (list file-path))))
    (unless (file-directory-p destination-dir)
      (make-directory destination-dir t))
    (redisplay) ;; Give redisplay a chance before blocking call-process
    (let ((exit-code (apply #'call-process command nil nil nil args)))
      (cond
       ((not (zerop exit-code))
        (error "Screenshot command failed with exit code %d" exit-code))
       ((not (file-exists-p file-path))
        (error "Screenshot file was not created"))
       ((zerop (nth 7 (file-attributes file-path)))
        (error "Screenshot file is empty"))
       (t
        file-path)))))

(defun agent-shell--status-label (status)
  "Convert STATUS codes to user-visible labels."
  (let* ((config (pcase status
                   ("pending" '("pending" font-lock-comment-face))
                   ("in_progress" '("in progress" warning))
                   ("completed" '("completed" success))
                   ("failed" '("failed" error))
                   (_ '("unknown" warning))))
         (label (car config))
         (face (cadr config))
         (color (face-foreground face nil t)))
    (agent-shell--add-text-properties
     (propertize (format " %s " label) 'font-lock-face 'default)
     'font-lock-face
     `(:foreground ,color :box (:color ,color)))))

(defun agent-shell--shorten-paths (text &optional include-project)
  "Shorten file paths in TEXT relative to project root.

\"/path/to/project/file.txt\" -> \"file.txt\"

With INCLUDE-PROJECT

\"/path/to/project/file.txt\" -> \"project/file.txt\""
  (when text
    (let ((cwd (string-remove-suffix "/" (agent-shell-cwd))))
      (replace-regexp-in-string (concat (regexp-quote
                                         (if include-project
                                             (string-remove-suffix
                                              "/"
                                              (file-name-directory
                                               (directory-file-name cwd)))
                                           cwd)) "/")
                                ""
                                (or text "")))))

(defun agent-shell-make-tool-call-label (state tool-call-id)
  "Create tool call label from STATE using TOOL-CALL-ID.

Returns propertized labels in :status and :title propertized."
  (when-let ((tool-call (map-nested-elt state `(:tool-calls ,tool-call-id))))
    `((:status . ,(let ((status (when (map-elt tool-call :status)
                                  (agent-shell--status-label (map-elt tool-call :status))))
                        (kind (when (map-elt tool-call :kind)
                                (agent-shell--add-text-properties
                                 (propertize (format " %s " (map-elt tool-call :kind))
                                             'font-lock-face 'default)
                                 'font-lock-face
                                 `(:box t)))))
                    (concat
                     (when status
                       status)
                     (when (and status kind)
                       " ")
                     (when kind
                       kind))))
      (:title . ,(let* ((title (when (map-elt tool-call :title)
                                 (agent-shell--shorten-paths (map-elt tool-call :title))))
                        (description (when (map-elt tool-call :description)
                                       (agent-shell--shorten-paths (map-elt tool-call :description)))))
                   (cond ((and title description
                               (not (equal (string-remove-prefix "`" (string-remove-suffix "`" (string-trim title)))
                                           (string-remove-prefix "`" (string-remove-suffix "`" (string-trim description))))))
                          (concat
                           (propertize title 'font-lock-face 'font-lock-doc-markup-face)
                           " "
                           (propertize description 'font-lock-face 'font-lock-doc-face)))
                         (title
                          (propertize title 'font-lock-face 'font-lock-doc-markup-face))
                         (description
                          (propertize description 'font-lock-face 'font-lock-doc-markup-face))))))))

(defun agent-shell--format-plan (entries)
  "Format plan ENTRIES for shell rendering."
  (agent-shell--align-alist
   :data entries
   :columns (list
             (lambda (entry)
               (agent-shell--status-label (map-elt entry 'status)))
             (lambda (entry)
               (map-elt entry 'content)))
   :separator "  "
   :joiner "\n"))

(cl-defun agent-shell--make-button (&key text help kind action keymap)
  "Make button with TEXT, HELP text, KIND, KEYMAP, and ACTION."
  (let ((button (propertize
                 (format " %s " text)
                 'font-lock-face '(:box t)
                 'help-echo help
                 'pointer 'hand
                 'keymap (let ((map (make-sparse-keymap)))
                           (define-key map [mouse-1] action)
                           (define-key map (kbd "RET") action)
                           (define-key map [remap self-insert-command] 'ignore)
                           (when keymap
                             (set-keymap-parent map keymap))
                           map)
                 'button kind)))
    button))

(defun agent-shell--add-text-properties (string &rest properties)
  "Add text PROPERTIES to entire STRING and return the propertized string.
PROPERTIES should be a plist of property-value pairs."
  (let ((str (copy-sequence string))
        (len (length string)))
    (while properties
      (let ((prop (car properties))
            (value (cadr properties)))
        (if (memq prop '(face font-lock-face))
            ;; Merge face properties
            (let ((existing (get-text-property 0 prop str)))
              (put-text-property 0 len prop
                                 (if existing
                                     (list value existing)
                                   value)
                                 str))
          ;; Regular property replacement
          (put-text-property 0 len prop value str))
        (setq properties (cddr properties))))
    str))

(cl-defun agent-shell--apply (&key function alist)
  "Apply keyword ALIST to FUNCTION.

ALIST should be a list of keyword-value pairs like (:foo 1 :bar 2).
FUNCTION should be a function accepting keyword arguments (&key ...)."
  (unless function
    (error "Missing required argument: :function"))
  (unless alist
    (error "Missing required argument: :alist"))
  (apply function
         (mapcan (lambda (pair)
                   (list (car pair) (cdr pair)))
                 alist)))

(cl-defun agent-shell--start (&key config no-focus new-session)
  "Programmatically start shell with CONFIG.

See `agent-shell-make-agent-config' for config format.

Set NO-FOCUS to start in background.
Set NEW-SESSION to start a separate new session."
  (unless (version<= "0.84.4" shell-maker-version)
    (error "Please update shell-maker to version 0.84.4 or newer"))
  (unless (version<= "0.8.2" acp-package-version)
    (error "Please update acp.el to version 0.8.2 or newer"))
  (when (boundp 'agent-shell--transcript-file-path-function)
    (user-error "'agent-shell--transcript-file-path-function is retired.

Please use 'agent-shell-transcript-file-path-function and unbind old
variable (see makunbound)."))
  (with-temp-buffer ;; client-maker needs a buffer (use a temp one)
    (unless (and (map-elt config :client-maker)
                 (funcall (map-elt config :client-maker) (current-buffer)))
      (error "No way to create a new client"))
    (agent-shell--ensure-executable
     (map-elt (funcall (map-elt config :client-maker) (current-buffer)) :command)
     (map-elt config :install-instructions)))
  (let* ((shell-maker-config (agent-shell--make-shell-maker-config
                              :prompt (map-elt config :shell-prompt)
                              :prompt-regexp (map-elt config :shell-prompt-regexp)))
         (agent-shell--shell-maker-config shell-maker-config)
         (default-directory (agent-shell-cwd))
         (shell-buffer
          (shell-maker-start agent-shell--shell-maker-config
                             t  ; Always use no-focus, handle display below
                             (when agent-shell-show-welcome-message
                               (map-elt config :welcome-function))
                             new-session
                             (concat (map-elt config :buffer-name)
                                     " Agent @ "
                                     (file-name-nondirectory
                                      (string-remove-suffix "/" default-directory)))
                             (map-elt config :mode-line-name))))
    (with-current-buffer shell-buffer
      ;; Initialize buffer-local state
      (setq-local agent-shell--state (agent-shell--make-state
                                      :buffer shell-buffer
                                      :heartbeat (agent-shell-heartbeat-make
                                                  :on-heartbeat
                                                  (lambda (_heartbeat _status)
                                                    (when (get-buffer-window shell-buffer)
                                                      (with-current-buffer shell-buffer
                                                        (agent-shell--update-header-and-mode-line)))
                                                    (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                                                                                  :shell-buffer shell-buffer
                                                                                  :existing-only t))
                                                                ((get-buffer-window viewport-buffer)))
                                                      (with-current-buffer viewport-buffer
                                                        (agent-shell-viewport--update-header)))))
                                      :client-maker (map-elt config :client-maker)
                                      :needs-authentication (map-elt config :needs-authentication)
                                      :authenticate-request-maker (map-elt config :authenticate-request-maker)
                                      :agent-config config))
      ;; Initialize buffer-local shell-maker-config
      (setq-local agent-shell--shell-maker-config shell-maker-config)
      (agent-shell--update-header-and-mode-line)
      (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t)
      (agent-shell-ui-mode +1)
      (when agent-shell-file-completion-enabled
        (agent-shell-completion-mode +1))
      (agent-shell--setup-modeline)
      (setq-local agent-shell--transcript-file (agent-shell--init-transcript config))
      ;; agent-shell does not support restoring sessions from transcript
      ;; via shell-maker. Unalias this functionality so it's not
      ;; misleading to users or appear via M-x.
      (fmakunbound 'agent-shell-restore-session-from-transcript)
      (when agent-shell--transcript-file
        ;; Prefer agent-shell--transcript-file over shell-maker's
        ;; transcript capabilities. Unalias to hide this in favor
        ;; of agent-shell's agent-shell--transcript-file usage.
        (fmakunbound 'agent-shell-save-session-transcript)
        (setq-local shell-maker-prompt-before-killing-buffer nil)))
    ;; Display buffer if no-focus was nil, respecting agent-shell-display-action
    (unless no-focus
      (agent-shell--display-buffer shell-buffer))
    shell-buffer))

(cl-defun agent-shell--delete-fragment (&key state block-id)
  "Delete fragment with STATE and BLOCK-ID."
  (when-let* ((shell-buffer (map-elt state :buffer))
              (viewport-buffer (agent-shell-viewport--buffer
                                :shell-buffer shell-buffer
                                :existing-only t)))
    (with-current-buffer viewport-buffer
      (agent-shell-ui-delete-fragment :namespace-id (map-elt state :request-count) :block-id block-id :no-undo t)))
  (with-current-buffer (map-elt state :buffer)
    (unless (and (derived-mode-p 'agent-shell-mode)
                 (equal (current-buffer)
                        (map-elt state :buffer)))
      (error "Editing the wrong buffer: %s" (current-buffer)))
    (agent-shell-ui-delete-fragment :namespace-id (map-elt state :request-count) :block-id block-id :no-undo t)))

(cl-defun agent-shell--update-fragment (&key state block-id label-left label-right body append create-new navigation expanded)
  "Update fragment in the shell buffer.

Creates or updates existing dialog using STATE's request count as namespace.
BLOCK-ID uniquely identifies the block.

Dialog can have LABEL-LEFT, LABEL-RIGHT, and BODY.

Optional flags: APPEND text to existing content, CREATE-NEW block,
NAVIGATION for navigation style, EXPANDED to show block expanded
by default."
  (when-let* ((shell-buffer (map-elt state :buffer))
              (viewport-buffer (agent-shell-viewport--buffer
                                :shell-buffer shell-buffer
                                :existing-only t)))
    (with-current-buffer viewport-buffer
      (let ((inhibit-read-only t))
        ;; TODO: Investigate why save-restriction isn't enough
        ;; to save point. Saving (point) for now.
        (when-let* ((saved-point (point))
                    (range (agent-shell-ui-update-fragment
                            (agent-shell-ui-make-fragment-model
                             :namespace-id (map-elt state :request-count)
                             :block-id block-id
                             :label-left label-left
                             :label-right label-right
                             :body body)
                            :navigation navigation
                            :append append
                            :create-new create-new
                            :expanded expanded
                            :no-undo t))
                    (padding-start (map-nested-elt range '(:padding :start)))
                    (padding-end (map-nested-elt range '(:padding :end)))
                    (block-start (map-nested-elt range '(:block :start)))
                    (block-end (map-nested-elt range '(:block :end))))
          ;; Apply markdown overlay to body.
          (save-restriction
            (when-let ((body-start (map-nested-elt range '(:body :start)))
                       (body-end (map-nested-elt range '(:body :end))))
              (narrow-to-region body-start body-end)
              (let ((markdown-overlays-highlight-blocks agent-shell-highlight-blocks))
                (markdown-overlays-put))))
          ;; Note: For now, we're skipping applying markdown overlays
          ;; on left labels as they currently carry propertized text
          ;; for statuses (ie. boxed).
          ;;
          ;; Apply markdown overlay to right label.
          (save-restriction
            (when-let ((label-right-start (map-nested-elt range '(:label-right :start)))
                       (label-right-end (map-nested-elt range '(:label-right :end))))
              (narrow-to-region label-right-start label-right-end)
              (let ((markdown-overlays-highlight-blocks agent-shell-highlight-blocks))
                (markdown-overlays-put))))
          (goto-char saved-point)))))
  (with-current-buffer (map-elt state :buffer)
    (unless (and (derived-mode-p 'agent-shell-mode)
                 (equal (current-buffer)
                        (map-elt state :buffer)))
      (error "Editing the wrong buffer: %s" (current-buffer)))
    (shell-maker-with-auto-scroll-edit
     (when-let* ((range (agent-shell-ui-update-fragment
                         (agent-shell-ui-make-fragment-model
                          :namespace-id (map-elt state :request-count)
                          :block-id block-id
                          :label-left label-left
                          :label-right label-right
                          :body body)
                         :navigation navigation
                         :append append
                         :create-new create-new
                         :expanded expanded
                         :no-undo t))
                 (padding-start (map-nested-elt range '(:padding :start)))
                 (padding-end (map-nested-elt range '(:padding :end)))
                 (block-start (map-nested-elt range '(:block :start)))
                 (block-end (map-nested-elt range '(:block :end))))
       (save-restriction
         ;; TODO: Move this to shell-maker?
         (let ((inhibit-read-only t))
           ;; comint relies on field property to
           ;; derive `comint-next-prompt'.
           ;; Marking as field to avoid false positives in
           ;; `agent-shell-next-item' and `agent-shell-previous-item'.
           (add-text-properties (or padding-start block-start)
                                (or padding-end block-end) '(field output)))
         ;; Apply markdown overlay to body.
         (when-let ((body-start (map-nested-elt range '(:body :start)))
                    (body-end (map-nested-elt range '(:body :end))))
           (narrow-to-region body-start body-end)
           (let ((markdown-overlays-highlight-blocks agent-shell-highlight-blocks))
             (markdown-overlays-put))
           (widen))
         ;;
         ;; Note: For now, we're skipping applying markdown overlays
         ;; on left labels as they currently carry propertized text
         ;; for statuses (ie. boxed).
         ;;
         ;; Apply markdown overlay to right label.
         (when-let ((label-right-start (map-nested-elt range '(:label-right :start)))
                    (label-right-end (map-nested-elt range '(:label-right :end))))
           (narrow-to-region label-right-start label-right-end)
           (let ((markdown-overlays-highlight-blocks agent-shell-highlight-blocks))
             (markdown-overlays-put))
           (widen)))
       (run-hook-with-args 'agent-shell-section-functions range)))))

(defun agent-shell-toggle-logging ()
  "Toggle logging."
  (interactive)
  (setq acp-logging-enabled (not acp-logging-enabled))
  (message "Logging: %s" (if acp-logging-enabled "ON" "OFF")))

(defun agent-shell-reset-logs ()
  "Reset all log buffers."
  (interactive)
  (acp-reset-logs :client (map-elt (agent-shell--state) :client))
  (message "Logs reset"))

(defun agent-shell-next-item ()
  "Go to next item.

Could be a prompt or an expandable item.
If point is at the input prompt and a character key was pressed,
insert the character instead."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  ;; Check if at prompt and inserting a character
  ;; (Ignore special keys like TAB/Shift-TAB).
  (if (and (not (shell-maker-busy))
           (shell-maker-point-at-last-prompt-p)
           (integerp last-command-event)
           (> (length (this-command-keys-vector)) 0)
           ;; Ensure invoked using a key binding.
           (eq (key-binding (this-command-keys-vector)) this-command))
      ;; At prompt, insert character.
      (self-insert-command 1)
    ;; Otherwise navigate.
    (let* ((prompt-pos (save-mark-and-excursion
                         (when (comint-next-prompt 1)
                           (point))))
           (block-pos (save-mark-and-excursion
                        (agent-shell-ui-forward-block)))
           (button-pos (save-mark-and-excursion
                         (agent-shell-next-permission-button)))
           (next-pos (apply #'min (delq nil (list prompt-pos
                                                  block-pos
                                                  button-pos)))))
      (when next-pos
        (deactivate-mark)
        (goto-char next-pos)
        (when (eq next-pos prompt-pos)
          (comint-skip-prompt))))))

(defun agent-shell-previous-item ()
  "Go to previous item.

Could be a prompt or an expandable item.
If point is at the input prompt and a character key was pressed,
insert the character instead."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  ;; Check if at prompt and inserting a character
  ;; (Ignore special keys like TAB/Shift-TAB).
  (if (and (not (shell-maker-busy))
           (shell-maker-point-at-last-prompt-p)
           (integerp last-command-event)
           (> (length (this-command-keys-vector)) 0)
           ;; Ensure invoked using a key binding.
           (eq (key-binding (this-command-keys-vector)) this-command))
      ;; At prompt, insert character.
      (self-insert-command 1)
    ;; Otherwise navigate.
    (let* ((current-pos (point))
           (prompt-pos (save-mark-and-excursion
                         (when (comint-next-prompt (- 1))
                           (let ((pos (point)))
                             (when (< pos current-pos)
                               pos)))))
           (block-pos (save-mark-and-excursion
                        (let ((pos (agent-shell-ui-backward-block)))
                          (when (and pos (< pos current-pos))
                            pos))))
           (button-pos (save-mark-and-excursion
                         (let ((pos (agent-shell-previous-permission-button)))
                           (when (and pos (< pos current-pos))
                             pos))))
           (positions (delq nil (list prompt-pos
                                      block-pos
                                      button-pos)))
           (next-pos (when positions
                       (apply #'max positions))))
      (when next-pos
        (deactivate-mark)
        (goto-char next-pos)
        (when (eq next-pos prompt-pos)
          (comint-skip-prompt))))))

(cl-defun agent-shell-make-environment-variables (&rest vars &key inherit-env load-env &allow-other-keys)
  "Return VARS in the form expected by `process-environment'.

With `:INHERIT-ENV' t, also inherit system environment (as per `setenv')
With `:LOAD-ENV' PATH-OR-PATHS, load .env files from given path(s).

For example:

  (agent-shell-make-environment-variables
    \"PATH\" \"/usr/bin\"
    \"HOME\" \"/home/user\"
    :load-env \"~/.env\")

Returns:

   (\"PATH=/usr/bin\"
    \"HOME=/home/user\")."
  (unless (zerop (mod (length vars) 2))
    (error "`agent-shell-make-environment' must receive complete pairs"))
  (append (mapcan (lambda (pair)
                    (unless (keywordp (car pair))
                      (list (format "%s=%s" (car pair) (cadr pair)))))
                  (seq-partition vars 2))
          (when load-env
            (let ((paths (if (listp load-env) load-env (list load-env))))
              (mapcan (lambda (path)
                        (unless (file-exists-p path)
                          (error "File not found: %s" path))
                        (with-temp-buffer
                          (insert-file-contents path)
                          (let (result)
                            (dolist (line (mapcar #'string-trim (split-string (buffer-string) "\n" t)))
                              (unless (or (string-empty-p line)
                                          (string-prefix-p "#" line))
                                (if (string-match "^\\([^=]+\\)=\\(.*\\)$" line)
                                    (push line result)
                                  (error "Malformed line in %s: %s" path line))))
                            (nreverse result))))
                      paths)))
          (when inherit-env
            process-environment)))

(cl-defun agent-shell--make-header (state &key qualifier bindings)
  "Return header text for current STATE.

STATE should contain :agent-config with :icon-name, :buffer-name, and
:session with :mode-id and :modes for displaying the current session mode.

QUALIFIER: Any text to prefix BINDINGS row with.

BINDINGS is a list of alists defining key bindings to display, each with:
  :key         - Key string (e.g., \"n\")
  :description - Description to display (e.g., \"next hunk\")"
  (unless state
    (error "STATE is required"))
  (let* ((model-name (or (map-elt (seq-find (lambda (model)
                                              (string= (map-elt model :model-id)
                                                       (map-nested-elt state '(:session :model-id))))
                                            (map-nested-elt state '(:session :models)))
                                  :name)
                         (map-nested-elt state '(:session :model-id))))
         (mode-name (when-let ((mode-id (map-nested-elt state '(:session :mode-id))))
                      (or (agent-shell--resolve-session-mode-name
                           mode-id
                           (agent-shell--get-available-modes state))
                          (map-nested-elt state '(:session :mode-id)))))
         (text-header (format " %s%s%s @ %s"
                              (propertize (concat (map-nested-elt state '(:agent-config :buffer-name)) " Agent")
                                          'font-lock-face 'font-lock-variable-name-face)
                              (if model-name
                                  (concat " ➤ " (propertize model-name 'font-lock-face 'font-lock-negation-char-face))
                                "")
                              (if mode-name
                                  (concat " ➤ " (propertize mode-name 'font-lock-face 'font-lock-type-face))
                                "")
                              (propertize (string-remove-suffix "/" (abbreviate-file-name default-directory))
                                          'font-lock-face 'font-lock-string-face))))
    (pcase agent-shell-header-style
      ((or 'none (pred null)) nil)
      ('text text-header)
      ('graphical
       (if (display-graphic-p)
           ;; +------+
           ;; | icon | Top text line
           ;; |      | Bottom text line
           ;; +------+
           ;; [Qualifier] Bindings row (optional, last row)
           (let* ((image-height (* 3 (default-font-height)))
                  (image-width image-height)
                  (text-height 25)
                  (row-spacing 0)  ; Spacing between icon/text rows and bindings row
                  (icon-text-row-height image-height)
                  (bindings-row-height (if (or bindings qualifier) text-height 0))
                  (total-height (+ icon-text-row-height bindings-row-height 10))
                  ;; Y positions for each row (baseline positions for text)
                  (icon-y 0)
                  (icon-text-y text-height)
                  ;; Bindings positioned right after the bottom text (2 text lines) plus spacing
                  (bindings-y (+ (* 3 text-height) row-spacing))
                  (svg (svg-create (frame-pixel-width) total-height))
                  (icon-filename
                   (if (map-nested-elt state '(:agent-config :icon-name))
                       (agent-shell--fetch-agent-icon (map-nested-elt state '(:agent-config :icon-name)))
                     (agent-shell--make-agent-fallback-icon (map-nested-elt state '(:agent-config :buffer-name)) 100)))
                  (image-type (or (agent-shell--image-type-to-mime icon-filename)
                                  "image/png")))
             ;; Icon
             (when (and icon-filename image-type)
               (svg-embed svg icon-filename
                          image-type nil
                          :x 0 :y icon-y :width image-width :height image-height))
             ;; Top text line
             (svg--append svg (let ((text-node (dom-node 'text
                                                         `((x . ,(+ image-width 10))
                                                           (y . ,icon-text-y)))))
                                ;; Agent name
                                (dom-append-child text-node
                                                  (dom-node 'tspan
                                                            `((fill . ,(face-attribute 'font-lock-variable-name-face :foreground)))
                                                            (concat (map-nested-elt state '(:agent-config :buffer-name)) " Agent")))
                                ;; Model name (optional)
                                (when model-name
                                  ;; Add separator arrow
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(face-attribute 'default :foreground))
                                                                (dx . "8"))
                                                              "➤"))
                                  ;; Add model name
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(face-attribute 'font-lock-negation-char-face :foreground))
                                                                (dx . "8"))
                                                              model-name)))
                                ;; Session mode (optional)
                                (when-let ((mode-id (map-nested-elt state '(:session :mode-id))))
                                  ;; Add separator arrow
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(face-attribute 'default :foreground))
                                                                (dx . "8"))
                                                              "➤"))
                                  ;; Add session mode text
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(or (face-attribute 'font-lock-type-face :foreground nil t)
                                                                             "#6699cc"))
                                                                (dx . "8"))
                                                              mode-name)))
                                (when-let ((status-frame (agent-shell--status-frame)))
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(face-attribute 'default :foreground))
                                                                (dx . "8"))
                                                              status-frame)))
                                text-node))
             ;; Bottom text line
             (svg-text svg (string-remove-suffix "/" (abbreviate-file-name default-directory))
                       :x (+ image-width 10) :y (+ icon-text-y text-height)
                       :fill (face-attribute 'font-lock-string-face :foreground))
             ;; Bindings row (last row if bindings or qualifier present)
             (when (or bindings qualifier)
               (svg--append svg (let ((text-node (dom-node 'text
                                                           `((x . 0)
                                                             (y . ,bindings-y))))
                                      (first t))
                                  ;; Add qualifier if present
                                  (when qualifier
                                    (dom-append-child text-node
                                                      (dom-node 'tspan
                                                                `((fill . ,(face-attribute 'default :foreground)))
                                                                qualifier))
                                    (setq first nil))
                                  (dolist (binding bindings)
                                    (when (map-elt binding :description)
                                      ;; Add key (XML-escape angle brackets)
                                      (dom-append-child text-node
                                                        (dom-node 'tspan
                                                                  `((fill . ,(face-attribute 'help-key-binding :foreground))
                                                                    ,@(unless first '((dx . "8"))))
                                                                  (replace-regexp-in-string
                                                                   "<" "&lt;"
                                                                   (replace-regexp-in-string
                                                                    ">" "&gt;"
                                                                    (map-elt binding :key)))))
                                      (setq first nil)
                                      ;; Add space and description
                                      (dom-append-child text-node
                                                        (dom-node 'tspan
                                                                  `((fill . ,(face-attribute 'default :foreground))
                                                                    (dx . "8"))
                                                                  (map-elt binding :description)))))
                                  text-node)))
             (format " %s" (with-temp-buffer
                             (svg-insert-image svg)
                             (buffer-string))))
         text-header))
      (_ text-header))))

(defun agent-shell--image-type-to-mime (filename)
  "Convert image type from FILENAME to MIME type string.
Returns a MIME type like \"image/png\" or \"image/jpeg\"."
  (when-let ((type (image-supported-file-p filename)))
    (pcase type
      ('svg "image/svg+xml")
      (_ (format "image/%s" type)))))

(defun agent-shell--update-header-and-mode-line ()
  "Update header and mode line based on `agent-shell-header-style'."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (cond
   ((eq agent-shell-header-style 'graphical)
    (setq header-line-format (agent-shell--make-header (agent-shell--state))))
   ((memq agent-shell-header-style '(text none nil))
    (setq header-line-format (agent-shell--make-header (agent-shell--state)))
    (force-mode-line-update))))

(defun agent-shell--fetch-agent-icon (icon-name)
  "Download icon with ICON-NAME from GitHub, only if it exists, and save as binary.

Names can be found at https://github.com/lobehub/lobe-icons/tree/master/packages/static-png

Icon names starting with https:// are downloaded directly from that location."
  (when icon-name
    (let* ((mode (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "light"))
           (is-url (string-prefix-p "https://" (downcase icon-name)))
           (url (if is-url
                    icon-name
                  (concat "https://raw.githubusercontent.com/lobehub/lobe-icons/refs/heads/master/packages/static-png/"
                          mode "/" icon-name)))
           (filename (if is-url
                         ;; For URLs, sanitize to create readable filename
                         ;; e.g., "https://opencode.ai/favicon.svg" -> "opencode.ai-favicon.svg"
                         (replace-regexp-in-string
                          "[/:]" "-"
                          (replace-regexp-in-string
                           "^https?://" ""
                           url))
                       ;; For lobe-icons names, use the original filename
                       (file-name-nondirectory url)))
           (cache-dir (file-name-concat (temporary-file-directory) "agent-shell" mode))
           (cache-path (expand-file-name filename cache-dir)))
      (unless (file-exists-p cache-path)
        (make-directory cache-dir t)
        (let ((buffer (url-retrieve-synchronously url t t 5.0)))
          (when buffer
            (with-current-buffer buffer
              (goto-char (point-min))
              (if (re-search-forward "^HTTP/1.1 200 OK" nil t)
                  (progn
                    (re-search-forward "\r?\n\r?\n")
                    (let ((coding-system-for-write 'no-conversion))
                      (write-region (point) (point-max) cache-path)))
                (message "Icon fetch failed: %s" url)))
            (kill-buffer buffer))))
      (when (file-exists-p cache-path)
        cache-path))))

(defun agent-shell--make-agent-fallback-icon (icon-name width)
  "Create SVG icon with first character of ICON-NAME and WIDTH.
Return file path of the generated SVG."
  (when (and icon-name (not (string-empty-p icon-name)))
    (let* ((icon-text (char-to-string (string-to-char icon-name)))
           (mode (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "light"))
           (filename (format "%s-%s.svg" icon-name width))
           (cache-dir (file-name-concat (temporary-file-directory) "agent-shell" mode))
           (cache-path (expand-file-name filename cache-dir))
           (font-size (* 0.7 width))
           (x (/ width 2))
           (y (/ width 2)))
      (unless (file-exists-p cache-path)
        (make-directory cache-dir t)
        (let ((svg (svg-create width width :stroke "white" :fill "black")))
          (svg-text svg icon-text
                    :x x :y y
                    :text-anchor "middle"
                    :dominant-baseline "central"
                    :font-weight "bold"
                    :font-size font-size
                    ;; :font-family "Monaco, Courier New, Courier, monospace"
                    :font-family (face-attribute 'default :family)
                    :fill (face-attribute 'default :foreground))
          (with-temp-buffer
            (let ((standard-output (current-buffer)))
              (svg-print svg))
            (write-region (point-min) (point-max) cache-path))))
      cache-path)))

(defun agent-shell-view-traffic ()
  "View agent shell traffic buffer."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (let ((traffic-buffer (acp-traffic-buffer :client (map-elt (agent-shell--state) :client))))
    (when (with-current-buffer traffic-buffer
            (= (buffer-size) 0))
      (error "No traffic logs available.  Try M-x agent-shell-toggle-logging?"))
    (pop-to-buffer traffic-buffer)))

(defun agent-shell-view-acp-logs ()
  "View agent shell ACP logs buffer."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (let ((logs-buffer (acp-logs-buffer :client (map-elt (agent-shell--state) :client))))
    (when (with-current-buffer logs-buffer
            (= (buffer-size) 0))
      (error "No traffic logs available.  Try M-x agent-shell-toggle-logging?"))
    (pop-to-buffer logs-buffer)))

(defun agent-shell--indent-string (n str)
  "Indent STR lines by N spaces."
  (mapconcat (lambda (line)
               (concat (make-string n ?\s) line))
             (split-string str "\n")
             "\n"))

(defun agent-shell--interpolate-gradient (colors progress)
  "Interpolate between gradient COLORS based on PROGRESS (0.0 to 1.0)."
  (let* ((segments (1- (length colors)))
         (segment-size (/ 1.0 segments))
         (segment (min (floor (/ progress segment-size)) (1- segments)))
         (local-progress (/ (- progress (* segment segment-size)) segment-size))
         (from-color (nth segment colors))
         (to-color (nth (1+ segment) colors)))
    (agent-shell--mix-colors from-color to-color local-progress)))

(defun agent-shell--mix-colors (color1 color2 ratio)
  "Mix two hex colors by RATIO (0.0 = COLOR1, 1.0 = COLOR2)."
  (let* ((r1 (string-to-number (substring color1 1 3) 16))
         (g1 (string-to-number (substring color1 3 5) 16))
         (b1 (string-to-number (substring color1 5 7) 16))
         (r2 (string-to-number (substring color2 1 3) 16))
         (g2 (string-to-number (substring color2 3 5) 16))
         (b2 (string-to-number (substring color2 5 7) 16))
         (r (round (+ (* r1 (- 1 ratio)) (* r2 ratio))))
         (g (round (+ (* g1 (- 1 ratio)) (* g2 ratio))))
         (b (round (+ (* b1 (- 1 ratio)) (* b2 ratio)))))
    (format "#%02x%02x%02x" r g b)))

(defun agent-shell--ensure-executable (executable &optional error-message &rest format-args)
  "Ensure EXECUTABLE exists in PATH or signal error.
ERROR-MESSAGE defaults to \"Executable %s not found\".
FORMAT-ARGS are passed to `format' with ERROR-FORMAT."
  (unless (executable-find executable)
    (apply #'error (concat (format "Executable \"%s\" not found.  Do you need (add-to-list 'exec-path \"another/path/to/consider/\")?" executable)
                           (when error-message
                             "  ")
                           error-message) format-args)))

(defun agent-shell--display-buffer (shell-buffer)
  "Toggle agent SHELL-BUFFER display."
  (interactive)
  (if (get-buffer-window shell-buffer)
      (select-window (get-buffer-window shell-buffer))
    (select-window (display-buffer shell-buffer agent-shell-display-action))))

(defun agent-shell--state ()
  "Get shell state or fail in an incompatible buffer."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Processed outside shell: %s" major-mode))
  (unless agent-shell--state
    (error "No shell state available"))
  agent-shell--state)

;;; Initialization

(cl-defun agent-shell--initialize-client (&key shell)
  "Initialize ACP client with SHELL."
  (agent-shell--update-fragment
   :state (agent-shell--state)
   :block-id "starting"
   :label-left (format "%s %s"
                       (agent-shell--status-label "in_progress")
                       (propertize "Starting agent" 'font-lock-face 'font-lock-doc-markup-face))
   :body "Creating client..."
   :create-new t)
  (if (map-elt (agent-shell--state) :client-maker)
      (progn
        (map-put! (agent-shell--state)
                  :client (funcall (map-elt agent-shell--state :client-maker)
                                   (map-elt agent-shell--state :buffer)))
        t)
    (funcall (map-elt shell :write-output) "No :client-maker found")
    (funcall (map-elt shell :finish-output) nil)
    nil))

(cl-defun agent-shell--initialize-subscriptions (&key shell)
  "Initialize ACP client subscriptions with SHELL.."
  (agent-shell--update-fragment
   :state agent-shell--state
   :block-id "starting"
   :label-left (format "%s %s"
                       (agent-shell--status-label "in_progress")
                       (propertize "Starting agent" 'font-lock-face 'font-lock-doc-markup-face))
   :body "\n\nSubscribing..."
   :append t)
  (if (map-elt agent-shell--state :client)
      (progn
        (agent-shell--subscribe-to-client-events :state agent-shell--state)
        t)
    (funcall (map-elt shell :write-output) "No :client found")
    (funcall (map-elt shell :finish-output) nil)
    nil))

(cl-defun agent-shell--initiate-handshake (&key shell on-initiated)
  "Initiate ACP handshake with SHELL.

Must provide ON-INITIATED (lambda ())."
  (unless on-initiated
    (error "Missing required argument: :on-initiated"))
  (with-current-buffer (map-elt agent-shell--state :buffer)
    (agent-shell--update-fragment
     :state agent-shell--state
     :block-id "starting"
     :body "\n\nInitializing..."
     :append t))
  (acp-send-request
   :client (map-elt agent-shell--state :client)
   :request (acp-make-initialize-request
             :protocol-version 1
             :read-text-file-capability agent-shell-text-file-capabilities
             :write-text-file-capability agent-shell-text-file-capabilities)
   :on-success (lambda (response)
                 (with-current-buffer (map-elt shell :buffer)
                   ;; Save prompt capabilities from agent, converting to internal symbols
                   (when-let ((prompt-capabilities
                               (map-nested-elt response '(agentCapabilities promptCapabilities))))
                     (map-put! agent-shell--state :prompt-capabilities
                               (list (cons :image (map-elt prompt-capabilities 'image))
                                     (cons :embedded-context (map-elt prompt-capabilities 'embeddedContext)))))
                   ;; Save available modes from agent, converting to internal symbols
                   (when-let ((modes (map-elt response 'modes)))
                     (map-put! agent-shell--state :available-modes
                               (list (cons :current-mode-id (map-elt modes 'currentModeId))
                                     (cons :modes (mapcar (lambda (mode)
                                                            `((:id . ,(map-elt mode 'id))
                                                              (:name . ,(map-elt mode 'name))
                                                              (:description . ,(map-elt mode 'description))))
                                                          (map-elt modes 'availableModes))))))
                   (when-let ((agent-capabilities (map-elt response 'agentCapabilities)))
                     (agent-shell--update-fragment
                      :state agent-shell--state
                      :block-id "agent_capabilities"
                      :label-left (propertize "Agent capabilities" 'font-lock-face 'font-lock-doc-markup-face)
                      :body (agent-shell--format-agent-capabilities agent-capabilities))))
                 (funcall on-initiated))
   :on-failure (agent-shell--make-error-handler
                :state agent-shell--state :shell shell)))

(cl-defun agent-shell--authenticate (&key shell on-authenticated)
  "Initiate ACP authentication with SHELL.

Must provide ON-AUTHENTICATED (lambda ())."
  (with-current-buffer (map-elt agent-shell--state :buffer)
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :block-id "starting"
     :body "\n\nAuthenticating..."
     :append t))
  (if (map-elt (agent-shell--state) :authenticate-request-maker)
      (acp-send-request
       :client (map-elt (agent-shell--state) :client)
       :request (funcall (map-elt agent-shell--state :authenticate-request-maker))
       :on-success (lambda (_response)
                     ;; TODO: More to be handled?
                     (funcall on-authenticated))
       :on-failure (agent-shell--make-error-handler
                    :state (agent-shell--state) :shell shell))
    (funcall (map-elt shell :write-output) "No :authenticate-request-maker")
    (funcall (map-elt shell :finish-output) nil)))

(cl-defun agent-shell--set-default-model (&key shell model-id on-model-changed)
  "Initiate ACP authentication with SHELL MODEL-ID and ON-MODEL-CHANGED."
  (when-let ((session-id (map-nested-elt (agent-shell--state) '(:session :id))))
    (with-current-buffer (map-elt agent-shell--state :buffer)
      (agent-shell--update-fragment
       :state (agent-shell--state)
       :block-id "set-model"
       :label-left (propertize "Setting model" 'font-lock-face 'font-lock-doc-markup-face)
       :body (format "Requesting %s..." model-id)
       :append t))
    (acp-send-request
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-model-request
               :session-id session-id
               :model-id model-id)
     :on-success (lambda (_response)
                   (agent-shell--update-fragment
                    :state (agent-shell--state)
                    :block-id "set-model"
                    :body "\n\nDone"
                    :append t)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :model-id model-id)
                     (map-put! (agent-shell--state) :session updated-session))
                   (agent-shell--update-header-and-mode-line)
                   (when on-model-changed
                     (funcall on-model-changed)))
     :on-failure (agent-shell--make-error-handler
                  :state (agent-shell--state) :shell shell))))

(cl-defun agent-shell--set-default-session-mode (&key shell mode-id on-mode-changed)
  "Set default session mode with SHELL MODE-ID and ON-MODE-CHANGED."
  (when-let ((session-id (map-nested-elt (agent-shell--state) '(:session :id))))
    (with-current-buffer (map-elt agent-shell--state :buffer)
      (agent-shell--update-fragment
       :state (agent-shell--state)
       :block-id "set-session-mode"
       :label-left (propertize "Setting session mode" 'font-lock-face 'font-lock-doc-markup-face)
       :body (format "Requesting %s..." mode-id)
       :append t))
    (acp-send-request
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-mode-request
               :session-id session-id
               :mode-id mode-id)
     :on-success (lambda (_response)
                   (agent-shell--update-fragment
                    :state (agent-shell--state)
                    :block-id "set-session-mode"
                    :body "\n\nDone"
                    :append t)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :mode-id mode-id)
                     (map-put! (agent-shell--state) :session updated-session))
                   (agent-shell--update-header-and-mode-line)
                   (when on-mode-changed
                     (funcall on-mode-changed)))
     :on-failure (agent-shell--make-error-handler
                  :state (agent-shell--state) :shell shell))))

(cl-defun agent-shell--initiate-session (&key shell on-session-init)
  "Initiate ACP session creation with SHELL.

Must provide ON-SESSION-INIT (lambda ())."
  (unless on-session-init
    (error "Missing required argument: :on-session-init"))
  (with-current-buffer (map-elt (agent-shell--state) :buffer)
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :block-id "starting"
     :body "\n\nCreating session..."
     :append t))
  (acp-send-request
   :client (map-elt (agent-shell--state) :client)
   :request (acp-make-session-new-request
             :cwd (agent-shell--resolve-path (agent-shell-cwd))
             :mcp-servers (agent-shell--mcp-servers))
   :buffer (current-buffer)
   :on-success (lambda (response)
                 (map-put! agent-shell--state
                           :session (list (cons :id (map-elt response 'sessionId))
                                          (cons :mode-id (map-nested-elt response '(modes currentModeId)))
                                          (cons :modes (mapcar (lambda (mode)
                                                                 `((:id . ,(map-elt mode 'id))
                                                                   (:name . ,(map-elt mode 'name))
                                                                   (:description . ,(map-elt mode 'description))))
                                                               (map-nested-elt response '(modes availableModes))))
                                          (cons :model-id (map-nested-elt response '(models currentModelId)))
                                          (cons :models (mapcar (lambda (model)
                                                                  `((:model-id . ,(map-elt model 'modelId))
                                                                    (:name . ,(map-elt model 'name))
                                                                    (:description . ,(map-elt model 'description))))
                                                                (map-nested-elt response '(models availableModels))))))
                 (agent-shell--update-fragment
                  :state agent-shell--state
                  :block-id "starting"
                  :label-left (format "%s %s"
                                      (agent-shell--status-label "completed")
                                      (propertize "Starting agent" 'font-lock-face 'font-lock-doc-markup-face))
                  :body "\n\nReady"
                  :append t)
                 (agent-shell--update-header-and-mode-line)
                 (when (map-nested-elt agent-shell--state '(:session :models))
                   (agent-shell--update-fragment
                    :state agent-shell--state
                    :block-id "available_models"
                    :label-left (propertize "Available models" 'font-lock-face 'font-lock-doc-markup-face)
                    :body (agent-shell--format-available-models
                           (map-nested-elt agent-shell--state '(:session :models)))))
                 (when (agent-shell--get-available-modes agent-shell--state)
                   (agent-shell--update-fragment
                    :state agent-shell--state
                    :block-id "available_modes"
                    :label-left (propertize "Available modes" 'font-lock-face 'font-lock-doc-markup-face)
                    :body (agent-shell--format-available-modes
                           (agent-shell--get-available-modes agent-shell--state))))
                 (agent-shell--update-header-and-mode-line)
                 (funcall on-session-init))
   :on-failure (agent-shell--make-error-handler
                :state agent-shell--state :shell shell)))

(defun agent-shell--mcp-servers ()
  "Return normalized MCP servers configuration for JSON serialization.

Converts list-valued `args', `env', and `headers' fields to vectors
so they serialize properly to JSON arrays.  Returns a vector of
normalized server configs."
  (when agent-shell-mcp-servers
    (apply #'vector
           (mapcar (lambda (server)
                     (let ((normalized (copy-alist server)))
                       (when-let ((args (map-elt normalized 'args))
                                  ((listp args)))
                         (map-put! normalized 'args (apply #'vector args)))
                       (when-let ((env (map-elt normalized 'env))
                                  ((listp env)))
                         (map-put! normalized 'env (apply #'vector env)))
                       (when-let ((headers (map-elt normalized 'headers))
                                  ((listp headers)))
                         (map-put! normalized 'headers (apply #'vector headers)))
                       normalized))
                   agent-shell-mcp-servers))))

(cl-defun agent-shell--subscribe-to-client-events (&key state)
  "Subscribe SHELL and STATE to ACP events."
  (acp-subscribe-to-errors
   :client (map-elt state :client)
   :on-error (lambda (error)
               (agent-shell--update-fragment
                :state state
                :block-id (format "%s-notices"
                                  (map-elt state :request-count))
                :label-left (propertize "Notices" 'font-lock-face 'font-lock-doc-markup-face) ;;
                :body (or (map-elt error 'message)
                          (map-elt error 'data)
                          "Something is up ¯\\_ (ツ)_/¯")
                :append t)))
  (acp-subscribe-to-notifications
   :client (map-elt state :client)
   :on-notification (lambda (notification)
                      (agent-shell--on-notification :state state :notification notification)))
  (acp-subscribe-to-requests
   :client (map-elt state :client)
   :on-request (lambda (request)
                 (agent-shell--on-request :state state :request request))))

(defun agent-shell--parse-file-mentions (prompt)
  "Parse @ file mentions from PROMPT string.
Returns list of alists with :start, :end, and :path for each mention."
  (let ((mentions '())
        (pos 0))
    (while (string-match "\\(?:^\\|[[:space:]]\\)@\\(?:\"\\([^\"]+\\)\"\\|\\([^[:space:]]+\\)\\)" prompt pos)
      (push `((:start . ,(match-beginning 0))
              (:end . ,(match-end 0))
              (:path . ,(or (match-string 1 prompt) (match-string 2 prompt))))
            mentions)
      (setq pos (match-end 0)))
    (nreverse mentions)))

(cl-defun agent-shell--build-content-blocks (prompt)
  "Build content blocks from the PROMPT."
  (let* ((supports-embedded-context (map-nested-elt agent-shell--state '(:prompt-capabilities :embedded-context)))
         (supports-image (map-nested-elt agent-shell--state '(:prompt-capabilities :image)))
         (mentions (agent-shell--parse-file-mentions prompt))
         (content-blocks '())
         (pos 0))
    (dolist (mention mentions)
      (let* ((start (map-elt mention :start))
             (end (map-elt mention :end))
             (path (map-elt mention :path))
             (resolved-path (expand-file-name path (agent-shell-cwd))))
        ;; Add text before mention
        (when (> start pos)
          (push `((type . "text")
                  (text . ,(substring-no-properties prompt pos start)))
                content-blocks))

        ;; Try to embed or link file
        (condition-case nil
            (let ((file (and (file-readable-p resolved-path)
                             (agent-shell--read-file-content :file-path resolved-path))))
              (cond
               ;; File not readable - keep mention as text
               ((not file)
                (push `((type . "text")
                        (text . ,(substring-no-properties prompt start end)))
                      content-blocks))
               ;; Binary image and image capability supported
               ;; Use ContentBlock::Image
               ((and supports-image (map-elt file :base64-p)
                     (string-prefix-p "image/" (map-elt file :mime-type)))
                (push `((type . "image")
                        (data . ,(map-elt file :content))
                        (mimeType . ,(map-elt file :mime-type))
                        (uri . ,(concat "file://" resolved-path)))
                      content-blocks))
               ;; Text file, small enough, and embeddedContext supported
               ;; Use ContentBlock::Resource
               ((and supports-embedded-context (map-elt file :size)
                     (< (map-elt file :size) agent-shell-embed-file-size-limit))
                (push `((type . "resource")
                        (resource . ((uri . ,(concat "file://" resolved-path))
                                     (text . ,(map-elt file :content))
                                     (mimeType . ,(map-elt file :mime-type)))))
                      content-blocks))
               ;; File too large or agent doesn't support embeddedContext
               ;; Use resource link
               (t
                (push `((type . "resource_link")
                        (uri . ,(concat "file://" resolved-path))
                        (name . ,path)
                        (mimeType . ,(map-elt file :mime-type))
                        (size . ,(map-elt file :size)))
                      content-blocks))))
          (error
           ;; On error, just keep the mention as text
           (push `((type . "text")
                   (text . ,(substring-no-properties prompt start end)))
                 content-blocks)))

        (setq pos end)))

    ;; Add remaining text
    (when (< pos (length prompt))
      (push `((type . "text")
              (text . ,(substring-no-properties prompt pos)))
            content-blocks))

    (nreverse content-blocks)))

(cl-defun agent-shell--read-file-content (&key file-path shallow)
  "Read FILE-PATH and return metadata and content as an alist.

When SHALLOW is non-nil, only metadata is returned without loading file content.

Returns an alist with:
  :size - file size in bytes
  :extension - file extension (lowercase)
  :mime-type - MIME type based on extension
  :base64-p - t if content is base64-encoded (binary image), nil otherwise
  :content - file content (omitted when SHALLOW is non-nil)"
  (let* ((ext (downcase (or (file-name-extension file-path) "")))
         (mime-type (or (agent-shell--image-type-to-mime file-path)
                        "text/plain"))
         ;; Only treat supported binary image formats as binary
         ;; SVG is XML/text and should not be base64-encoded
         ;; API only supports: image/png, image/jpeg, image/gif, image/webp
         (is-binary (member mime-type '("image/png" "image/jpeg" "image/gif" "image/webp")))
         (file-size (file-attribute-size (file-attributes file-path)))
         (content (unless shallow
                    (with-temp-buffer
                      (if is-binary
                          (progn
                            (insert-file-contents-literally file-path)
                            (base64-encode-string (buffer-string) t))
                        (insert-file-contents file-path)
                        (buffer-string))))))
    (append (list (cons :size file-size)
                  (cons :extension ext)
                  (cons :mime-type mime-type)
                  (cons :base64-p is-binary))
            (unless shallow
              (list (cons :content content))))))

(cl-defun agent-shell--load-image (&key file-path (max-width 200))
  "Load image from FILE-PATH and return the image object.

MAX-WIDTH specifies the maximum width in pixels for the image (default 200).
If FILE-PATH is not an image, returns nil."
  (when-let* (((display-graphic-p))
              (metadata (agent-shell--read-file-content :file-path file-path :shallow t))
              (mime-type (map-elt metadata :mime-type))
              ;; Check if it's an image type
              (is-image (string-prefix-p "image/" mime-type)))
    (create-image file-path nil nil :max-width max-width)))

(cl-defun agent-shell--collect-attached-files (content-blocks)
  "Collect attached resource uris from CONTENT-BLOCKS."
  (mapcan
   (lambda (content-block)
     (let ((type (map-elt content-block 'type)))
       (cond
        ((equal type "resource") (list (map-nested-elt content-block '(resource uri))))
        ((equal type "resource_link") (list (map-elt content-block 'uri)))
        (t nil))))
   content-blocks))

(cl-defun agent-shell--display-attached-files (uris)
  "Display the attached URIS in the buffer."
  (agent-shell--update-fragment
   :state agent-shell--state
   :block-id "attached-files"
   :label-left (format "%d file%s attached"
                       (length uris)
                       (if (= (length uris) 1) "" "s"))
   :body (mapconcat (lambda (f) (format "• %s" f))
                    (nreverse uris)
                    "\n")
   :create-new t))

(cl-defun agent-shell--send-command (&key prompt shell)
  "Send PROMPT to agent using SHELL."
  (let* ((content-blocks (condition-case nil
                             (agent-shell--build-content-blocks prompt)
                           (error `[((type . "text")
                                     (text . ,(substring-no-properties prompt)))])))
         (attached-files (agent-shell--collect-attached-files content-blocks)))
    (when attached-files
      (agent-shell--display-attached-files attached-files))
    (agent-shell-heartbeat-start
     :heartbeat (map-elt agent-shell--state :heartbeat))

    (map-put! agent-shell--state :last-entry-type nil)

    (agent-shell--append-transcript
     :text (format "## User (%s)\n\n%s\n\n"
                   (format-time-string "%F %T")
                   prompt)
     :file-path agent-shell--transcript-file)

    (when-let* ((shell-buffer (map-elt shell :buffer))
                (viewport-buffer (agent-shell-viewport--buffer
                                  :shell-buffer shell-buffer
                                  :existing-only t)))
      (with-current-buffer viewport-buffer
        (agent-shell-viewport-view-mode)
        (agent-shell-viewport--initialize
         :prompt  prompt)))

    (acp-send-request
     :client (map-elt agent-shell--state :client)
     :request (acp-make-session-prompt-request
               :session-id (map-nested-elt agent-shell--state '(:session :id))
               :prompt content-blocks)
     :buffer (current-buffer)
     :on-success (lambda (response)
                   (when (equal (map-elt (agent-shell--state) :last-entry-type) "agent_message_chunk")
                     (agent-shell--append-transcript
                      :text "\n\n"
                      :file-path agent-shell--transcript-file))
                   ;; Tool call details are no longer needed after
                   ;; a session prompt request is finished.
                   ;; Avoid accumulating them unnecessarily.
                   (map-put! (agent-shell--state) :tool-calls nil)
                   (let ((success (equal (map-elt response 'stopReason)
                                         "end_turn")))
                     (unless success
                       (agent-shell--update-fragment
                        :state (agent-shell--state)
                        :block-id (format "%s-stop-reason"
                                          (map-elt (agent-shell--state) :request-count))
                        :body (agent-shell--stop-reason-description
                               (map-elt response 'stopReason))
                        :create-new t))
                     (agent-shell-heartbeat-stop
                      :heartbeat (map-elt agent-shell--state :heartbeat))
                     (unless success
                       (agent-shell--display-pending-requests))
                     (funcall (map-elt shell :finish-output) t)
                     ;; Update viewport header (longer busy)
                     (when-let ((viewport-buffer (agent-shell-viewport--buffer
                                                  :shell-buffer (map-elt shell :buffer)
                                                  :existing-only t)))
                       (with-current-buffer viewport-buffer
                         (agent-shell-viewport--update-header)))
                     (when success
                       (agent-shell--process-pending-request))))
     :on-failure (lambda (error raw-message)
                   ;; Display pending requests on failure.
                   (agent-shell--display-pending-requests)
                   (funcall (agent-shell--make-error-handler :state agent-shell--state :shell shell)
                            error raw-message)
                   (agent-shell-heartbeat-stop
                    :heartbeat (map-elt agent-shell--state :heartbeat))
                   ;; Update viewport header (longer busy)
                   (when-let ((viewport-buffer (agent-shell-viewport--buffer
                                                :shell-buffer (map-elt shell :buffer)
                                                :existing-only t)))
                     (with-current-buffer viewport-buffer
                       (agent-shell-viewport--update-header)))))))

;;; Projects

(defun agent-shell-project-buffers ()
  "Return all shell buffers in the same project as current buffer."
  (let ((project-root (agent-shell-cwd)))
    (seq-filter (lambda (buffer)
                  (with-current-buffer buffer
                    (string-prefix-p project-root
                                     (expand-file-name default-directory))))
                (agent-shell-buffers))))

(cl-defun agent-shell--shell-buffer (&key viewport-buffer no-error no-create)
  "Get an `agent-shell' buffer for the current project.

When VIEWPORT-BUFFER is non-nil, return counterpart.
When NO-CREATE is nil (default), prompt to create a new shell if none exists.
When NO-CREATE is non-nil, return existing shell or nil/error if none exists.
When NO-ERROR is non-nil, return nil instead of raising an error.

Returns a buffer object or nil."
  (let ((shell-buffer (if viewport-buffer
                          (seq-first (seq-filter (lambda (shell-buffer)
                                                   (equal (agent-shell-viewport--buffer
                                                           :shell-buffer shell-buffer
                                                           :existing-only t)
                                                          viewport-buffer))
                                                 (agent-shell-buffers)))
                        (seq-first (agent-shell-project-buffers)))))
    (if shell-buffer
        shell-buffer
      (if no-create
          (unless no-error
            (user-error "No agent shell buffers available for current project"))
        (when (y-or-n-p "No shells in project.  Start a new one? ")
          (get-buffer
           (agent-shell--start :config (or agent-shell-preferred-agent-config
                                           (agent-shell-select-config
                                            :prompt "Start new agent: ")
                                           (error "No agent config found"))
                               :no-focus t
                               :new-session t)))))))

(defun agent-shell-cwd ()
  "Return the CWD for this shell.

If in a project, use project root."
  (expand-file-name
   (or (when (fboundp 'projectile-project-root)
         (projectile-project-root))
       (when (fboundp 'project-root)
         (when-let ((proj (project-current)))
           (project-root proj)))
       default-directory
       (error "No CWD available"))))

(defun agent-shell--current-shell ()
  "Current shell for viewport or shell buffer."
  (cond ((derived-mode-p 'agent-shell-mode)
         (current-buffer))
        ((or (derived-mode-p 'agent-shell-viewport-view-mode)
             (derived-mode-p 'agent-shell-viewport-edit-mode))
         (seq-first (seq-filter (lambda (shell-buffer)
                                  (equal (agent-shell-viewport--buffer
                                          :shell-buffer shell-buffer
                                          :existing-only t)
                                         (current-buffer)))
                                (agent-shell-buffers))))))

;;; Shell

(defun agent-shell-insert-shell-command-output ()
  "Execute a shell command and insert output as a code block.

The command executes asynchronously.  When finished, the output is
inserted into the shell buffer prompt."
  (interactive)
  (unless (or (derived-mode-p 'agent-shell-viewport-view-mode)
              (derived-mode-p 'agent-shell-viewport-edit-mode)
              (derived-mode-p 'agent-shell-mode))
    (user-error "Not in an `agent-shell' buffer"))
  (let* ((command (read-string "insert command output: "))
         (shell-buffer (or (agent-shell--current-shell)
                           (user-error "No shell available")))
         (destination-buffer (progn
                               (when (with-current-buffer shell-buffer
                                       (shell-maker-busy))
                                 (user-error "Busy, try later"))
                               (if (or (derived-mode-p 'agent-shell-viewport-view-mode)
                                       (derived-mode-p 'agent-shell-viewport-edit-mode))
                                   (agent-shell-viewport--buffer
                                    :shell-buffer shell-buffer)
                                 shell-buffer)))
         (output-buffer (with-current-buffer (generate-new-buffer (format "*%s*" command))
                          (insert "$ " command "\n\n")
                          (setq-local buffer-read-only t)
                          (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "q") #'quit-window)
                            (use-local-map map))
                          (current-buffer)))
         (window-config (current-window-configuration))
         (proc (make-process
                :name command
                :buffer output-buffer
                :command (let ((cmd (list shell-file-name
                                          shell-command-switch
                                          ;; Merge stderr into stdout output
                                          ;; (all into output buffer)
                                          (format "%s 2>&1" command))))
                           (if agent-shell-container-command-runner
                               (append agent-shell-container-command-runner cmd)
                             cmd))
                :connection-type 'pipe
                :filter
                (lambda (proc output)
                  (when (buffer-live-p (process-buffer proc))
                    (with-current-buffer (process-buffer proc)
                      (let ((inhibit-read-only t))
                        (goto-char (point-max))
                        (insert output)))))
                :sentinel
                (lambda (process _event)
                  (when (memq (process-status process) '(exit signal))
                    (message "Done")
                    (set-window-configuration window-config)
                    (save-excursion
                      (goto-char (point-max))
                      (with-current-buffer destination-buffer
                        (insert "\n\n" (format "```shell
%s
```" (with-current-buffer output-buffer
       (buffer-string))))))
                    (let ((markdown-overlays-highlight-blocks agent-shell-highlight-blocks))
                      (markdown-overlays-put))
                    (when (buffer-live-p output-buffer)
                      (kill-buffer output-buffer)))))))
    (set-process-query-on-exit-flag proc nil)
    (run-at-time "0.2 sec" nil
                 (lambda ()
                   (unless (equal (process-status proc) 'exit)
                     (agent-shell--display-buffer output-buffer))))))

;;; Completion

(cl-defun agent-shell--processed-files (&key files)
  "Process FILES into sendable text with image preview if applicable."
  (when files
    (mapconcat (lambda (file)
                 (let ((text (concat "@" file)))
                   (if-let ((image-display (agent-shell--load-image :file-path file :max-width 200)))
                       ;; Propertize text to display the image
                       (propertize text 'display image-display)
                     ;; Not an image, insert as normal text
                     text)))
               files
               "\n\n")))

(defun agent-shell-send-file (&optional prompt-for-file)
  "Insert a file into `agent-shell'.

If visiting a file, send this file.

If invoked from shell, select a project file.

If invoked from `dired', use selection or region files.

With prefix argument PROMPT-FOR-FILE, always prompt for file selection."
  (interactive "P")
  (if (and (region-active-p)
           (buffer-file-name))
      (agent-shell-send-region)
    (let* ((in-shell (derived-mode-p 'agent-shell-mode))
           (files (if (or in-shell prompt-for-file)
                      (list (completing-read "Send file: " (agent-shell--project-files)))
                    (or (agent-shell--buffer-files)
                        (list (completing-read "Send file: " (agent-shell--project-files)))
                        (user-error "No file to send")))))
      (agent-shell--insert-to-shell-buffer
       :text (agent-shell--processed-files :files files)))))

(cl-defun agent-shell--buffer-files (&key obvious)
  "Return buffer file(s) or `dired' selected file(s).

Buffer filename is OBVIOUS if its an image."
  (if (and obvious
           (buffer-file-name)
           (image-supported-file-p (buffer-file-name)))
      (list (buffer-file-name))
    (or
     (agent-shell--dired-paths-in-region)
     (dired-get-marked-files))))

(defun agent-shell--dired-paths-in-region ()
  "If `dired' buffer, return region files.  nil otherwise."
  (when (and (equal major-mode 'dired-mode)
             (use-region-p))
    (let ((start (region-beginning))
          (end (region-end))
          (paths))
      (save-excursion
        (save-restriction
          (goto-char start)
          (while (< (point) end)
            ;; Skip non-file lines.
            (while (and (< (point) end) (dired-between-files))
              (forward-line 1))
            (when (dired-get-filename nil t)
              (setq paths (append paths (list (dired-get-filename nil t)))))
            (forward-line 1))))
      paths)))

(defalias 'agent-shell-insert-file #'agent-shell-send-file)

(defalias 'agent-shell-send-current-file #'agent-shell-send-file)

(defun agent-shell-send-other-file ()
  "Prompt to send a file into `agent-shell'.

Always prompts for file selection, even if a current file is available."
  (interactive)
  (agent-shell-send-file t))

(defun agent-shell-send-screenshot ()
  "Capture a screenshot and insert it into `agent-shell'.

The screenshot is saved to .agent-shell/screenshots in the project root.
The captured screenshot file path is then inserted into the shell prompt."
  (interactive)
  (let* ((screenshots-dir (expand-file-name ".agent-shell/screenshots" (agent-shell-cwd)))
         (screenshot-path (agent-shell--capture-screenshot :destination-dir screenshots-dir)))
    (agent-shell-insert
     :text (agent-shell--processed-files :files (list screenshot-path)))))

(defun agent-shell--project-files ()
  "Get project files using projectile or project.el."
  (cond
   ((and (boundp 'projectile-mode)
         projectile-mode
         (projectile-project-p))
    (mapcar (lambda (f)
              (file-relative-name f (projectile-project-root)))
            (projectile-current-project-files)))
   ((fboundp 'project-current)
    (when-let ((proj (project-current)))
      (mapcar (lambda (f) (file-relative-name f (project-root proj)))
              (project-files proj))))
   (t nil)))

(defun agent-shell--file-completion-at-point ()
  "Complete project files after @."
  (when-let* ((triggered (eq (char-before) ?@))
              (start (point))
              (end (save-excursion
                     (skip-chars-forward "[:alnum:]/_.-")
                     (point)))
              (prefix (buffer-substring start end))
              (files (agent-shell--project-files)))
    (list start end
          (seq-filter
           (lambda (f) (string-prefix-p prefix (file-name-nondirectory f)))
           files)
          :exclusive 'no
          :exit-function
          (lambda (_status _string)
            (insert " ")))))

(defun agent-shell--command-completion-at-point ()
  "Complete available commands after /."
  (when (eq (char-before) ?/)
    (let* ((start (point))
           (end (save-excursion
                  (skip-chars-forward "[:alnum:]_-")
                  (point)))
           (prefix (buffer-substring start end))
           (commands (map-elt agent-shell--state :available-commands)))
      (list start end
            (mapcar (lambda (command)
                      (map-elt command 'name))
                    (seq-filter
                     (lambda (command)
                       (string-prefix-p prefix (map-elt command 'name)))
                     commands))
            :exclusive t ; prevent / file completion fallback (already covered by @).
            :exit-function
            (lambda (_status _string)
              (insert " "))))))

(defun agent-shell--trigger-completion-at-point ()
  "Trigger completion when @ or / is typed."
  (when (or (eq (char-before) ?@)
            (eq (char-before) ?/))
    (completion-at-point)))

(define-minor-mode agent-shell-completion-mode
  "Toggle agent shell completion with @ or / prefix."
  :lighter " @/Compl"
  (if agent-shell-completion-mode
      (progn
        (add-hook 'completion-at-point-functions #'agent-shell--file-completion-at-point nil t)
        (add-hook 'completion-at-point-functions #'agent-shell--command-completion-at-point nil t)
        (add-hook 'post-self-insert-hook #'agent-shell--trigger-completion-at-point nil t))
    (remove-hook 'completion-at-point-functions #'agent-shell--file-completion-at-point t)
    (remove-hook 'completion-at-point-functions #'agent-shell--command-completion-at-point t)
    (remove-hook 'post-self-insert-hook #'agent-shell--trigger-completion-at-point t)))

;;; Permissions

(cl-defun agent-shell--make-tool-call-permission-text (&key request client state)
  "Create text to render permission dialog using REQUEST, CLIENT, and STATE.

For example:

   ╭─

       ⚠ Tool Permission ⚠


       [ View (v) ] [ Allow (y) ] [ Reject (n) ] [ Always Allow (!) ]


   ╰─"
  (let* ((tool-call-id (map-nested-elt request '(params toolCall toolCallId)))
         (diff (map-nested-elt state `(:tool-calls ,tool-call-id :diff)))
         (actions (agent-shell--make-permission-actions (map-nested-elt request '(params options))))
         (shell-buffer (map-elt state :buffer))
         (keymap (let ((map (make-sparse-keymap)))
                   (dolist (action actions)
                     (when-let ((char (map-elt action :char)))
                       (define-key map (kbd char)
                                   (lambda ()
                                     (interactive)
                                     (agent-shell--send-permission-response
                                      :client client
                                      :request-id (map-elt request 'id)
                                      :option-id (map-elt action :option-id)
                                      :state state
                                      :tool-call-id tool-call-id
                                      :message-text (map-elt action :option))
                                     (when (equal (map-elt action :kind) "reject_once")
                                       ;; No point in rejecting the change but letting
                                       ;; the agent continue (it doesn't know why you
                                       ;; have rejected the change).
                                       ;; May as well interrupt so you can course-correct.
                                       (with-current-buffer shell-buffer
                                         (agent-shell-interrupt t)))))))
                   ;; Add diff keybinding if diff info is available
                   (when diff
                     (define-key map "v" (agent-shell--make-diff-viewing-function
                                          :diff diff
                                          :actions actions
                                          :client client
                                          :request-id (map-elt request 'id)
                                          :state state
                                          :tool-call-id tool-call-id)))
                   ;; Add interrupt keybinding
                   (define-key map (kbd "C-c C-c")
                               (lambda ()
                                 (interactive)
                                 (with-current-buffer shell-buffer
                                   (agent-shell-interrupt t))))
                   map))
         (diff-button (when diff
                        (agent-shell--make-permission-button
                         :text "View (v)"
                         :help "Press v to view diff"
                         :action (agent-shell--make-diff-viewing-function
                                  :diff diff
                                  :actions actions
                                  :client client
                                  :request-id (map-elt request 'id)
                                  :state state
                                  :tool-call-id tool-call-id)
                         :keymap keymap
                         :navigatable t
                         :char ?v
                         :option "view diff"))))
    (format "╭─

    %s %s %s%s


    %s%s


╰─"
            (propertize agent-shell-permission-icon
                        'font-lock-face 'warning)
            (propertize "Tool Permission" 'font-lock-face 'bold)
            (propertize agent-shell-permission-icon
                        'font-lock-face 'warning)
            (if (map-nested-elt request '(params toolCall title))
                (propertize
                 (format "\n\n\n    %s" (map-nested-elt request '(params toolCall title)))
                 'font-lock-face 'comint-highlight-input)
              "")
            (if diff-button
                (concat diff-button " ")
              "")
            (mapconcat (lambda (action)
                         (agent-shell--make-permission-button
                          :text (map-elt action :label)
                          :help (map-elt action :label)
                          :action (lambda ()
                                    (interactive)
                                    (agent-shell--send-permission-response
                                     :client client
                                     :request-id (map-elt request 'id)
                                     :option-id (map-elt action :option-id)
                                     :state state
                                     :tool-call-id tool-call-id
                                     :message-text (format "Selected: %s" (map-elt action :option)))
                                    (when (equal (map-elt action :kind) "reject_once")
                                      ;; No point in rejecting the change but letting
                                      ;; the agent continue (it doesn't know why you
                                      ;; have rejected the change).
                                      ;; May as well interrupt so you can course-correct.
                                      (with-current-buffer shell-buffer
                                        (agent-shell-interrupt t))))
                          :keymap keymap
                          :char (map-elt action :char)
                          :option (map-elt action :option)
                          :navigatable t))
                       actions
                       " "))))

(cl-defun agent-shell--send-permission-response (&key client request-id option-id cancelled state tool-call-id message-text)
  "Send a response to a permission request and clean up related dialog UI.

Choose OPTION-ID or CANCELLED (never both).

CLIENT: The ACP client used to send the response.
REQUEST-ID: The ID of the original permission request.
OPTION-ID: The ID of the selected permission option.
CANCELLED: Non-nil if the request was cancelled instead of selecting an option.
STATE: The buffer-local agent-shell session state.
TOOL-CALL-ID: The tool call identifier.
MESSAGE-TEXT: Optional message to display after sending the response."
  (acp-send-response
   :client client
   :response (acp-make-session-request-permission-response
              :request-id request-id
              :cancelled cancelled
              :option-id option-id))
  ;; Hide permission after sending response.
  ;; block-id must be the same as the one used as
  ;; agent-shell--update-fragment param by "session/request_permission".
  (agent-shell--delete-fragment :state state :block-id (format "permission-%s" tool-call-id))
  (let ((updated-tool-calls (map-copy (map-elt state :tool-calls))))
    (map-delete updated-tool-calls tool-call-id)
    (map-put! state :tool-calls updated-tool-calls))
  (when message-text
    (message "%s" message-text))
  (goto-char (point-max)))

(cl-defun agent-shell--resolve-permission-choice-to-action (&key choice actions)
  "Resolve `agent-shell-diff' CHOICE to permission action from ACTIONS.

CHOICE can be \\='accept or \\='reject.
Returns the matching action or nil if no match found."
  (cond
   ((equal choice 'accept)
    (seq-find (lambda (action)
                (string= (map-elt action :kind) "allow_once"))
              actions))
   ((equal choice 'reject)
    (seq-find (lambda (action)
                (string= (map-elt action :kind) "reject_once"))
              actions))
   (t nil)))

(cl-defun agent-shell--make-diff-viewing-function (&key diff actions client request-id state tool-call-id)
  "Create a diffing handler for the ACP CLIENT's REQUEST-ID and TOOL-CALL-ID.

DIFF as per `agent-shell--make-diff-info'.
ACTIONS as per `agent-shell--make-permission-action'."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (let ((shell-buffer (current-buffer)))
    (lambda ()
      (interactive)
      (agent-shell-diff
       :old (map-elt diff :old)
       :new (map-elt diff :new)
       :title (file-name-nondirectory (map-elt diff :file))
       :bindings (list (list :key "n"
                             :description "next hunk"
                             :command 'diff-hunk-next)
                       (list :key "p"
                             :description "previous hunk"
                             :command 'diff-hunk-prev)
                       (list :key "y"
                             :description "accept all"
                             :command (lambda ()
                                        (interactive)
                                        (let ((action (agent-shell--resolve-permission-choice-to-action
                                                       :choice 'accept
                                                       :actions actions))
                                              (agent-shell-on-exit nil))
                                          ;; Disable on-exit since killing
                                          ;; the buffer should not trigger
                                          ;; asking user if they want to
                                          ;; keep or reject changes.
                                          (kill-current-buffer)
                                          (with-current-buffer shell-buffer
                                            (agent-shell--send-permission-response
                                             :client client
                                             :request-id request-id
                                             :option-id (map-elt action :option-id)
                                             :state state
                                             :tool-call-id tool-call-id
                                             :message-text (map-elt action :option))))))
                       (list :key (key-description (where-is-internal 'agent-shell-interrupt agent-shell-mode-map t))
                             :description nil ;; hide from header-line-format
                             :command (lambda ()
                                        (interactive)
                                        (when (y-or-n-p "Interrupt?")
                                          (let ((agent-shell-on-exit nil))
                                            ;; Disable on-exit since killing
                                            ;; the buffer should not trigger
                                            ;; asking user if they want to
                                            ;; keep or reject changes.
                                            (kill-current-buffer))
                                          (with-current-buffer shell-buffer
                                            (agent-shell-interrupt t)))))
                       (list :key "q" :description "exit" :command 'kill-current-buffer))
       :on-exit (lambda ()
                  (if-let ((choice (condition-case nil
                                       (if (y-or-n-p "Accept changes?")
                                           'accept
                                         'reject)
                                     (quit 'ignore)))
                           (action (agent-shell--resolve-permission-choice-to-action
                                    :choice choice
                                    :actions actions)))
                      (progn
                        (agent-shell--send-permission-response
                         :client client
                         :request-id request-id
                         :option-id (map-elt action :option-id)
                         :state state
                         :tool-call-id tool-call-id
                         :message-text (map-elt action :option))
                        (when (eq choice 'reject)
                          ;; No point in rejecting the change but letting
                          ;; the agent continue (it doesn't know why you
                          ;; have rejected the change).
                          ;; May as well interrupt so you can course-correct.
                          (with-current-buffer shell-buffer
                            (agent-shell-interrupt t))))
                    (message "Ignored")))))))

(cl-defun agent-shell--make-permission-button (&key text help action keymap navigatable char option)
  "Create a permission button with TEXT, HELP, ACTION, and KEYMAP.

For example:

  \"[ Allow (y) ]\"

When NAVIGATABLE is non-nil, make button character navigatable.
CHAR and OPTION are used for cursor sensor messages."
  (let ((button (agent-shell--make-button
                 :text text
                 :help help
                 :kind 'permission
                 :keymap keymap
                 :action action)))
    (when navigatable
      ;; Make the button character navigatable.
      ;;
      ;; For example, the "y" in:
      ;;
      ;; [ Allow (y) ]
      (put-text-property (- (length button) 3) (- (length button) 1)
                         'agent-shell-permission-button t button)
      (put-text-property (- (length button) 3) (- (length button) 1)
                         'cursor-sensor-functions
                         (list (lambda (_window _old-pos sensor-action)
                                 (when (eq sensor-action 'entered)
                                   (message "Press RET or %s to %s"
                                            char option))))
                         button))
    button))

(defun agent-shell--make-permission-actions (acp-options)
  "Make actions from ACP-OPTIONS for shell rendering.

See `agent-shell--make-permission-action' for ACP-OPTION and return schema."
  (seq-sort (lambda (a b)
              (< (length (map-elt a :label))
                 (length (map-elt b :label))))
            (delq nil (mapcar #'agent-shell--make-permission-action acp-options))))

(defun agent-shell--make-permission-action (acp-option)
  "Convert a single ACP-OPTION to an action alist.

ACP-OPTION should be a PermissionOption per ACP spec:

  https://agentclientprotocol.com/protocol/schema#permissionoption

  An alist of the form:

  ((\='kind . \"allow_once\")
   (\='name . \"Allow\")
   (\='optionId . \"allow\"))

Returns an alist of the form:

  ((:label . \"Allow (y)\")
   (:option . \"Allow\")
   (:char . ?y)
   (:kind . \"allow_once\")
   (:option-id . ...))

Returns nil if the ACP-OPTION kind is not recognized."
  (let* ((char-map `(("allow_always" . "!")
                     ("allow_once" . "y")
                     ("reject_once" . ,(or (ignore-errors
                                             (key-description (where-is-internal 'agent-shell-interrupt
                                                                                 agent-shell-mode-map t)))
                                           "n"))))
         (kind (map-elt acp-option 'kind))
         (char (map-elt char-map kind))
         (name (map-elt acp-option 'name)))
    (when char
      (map-into `((:label . ,(format "%s (%s)" name char))
                  (:option . ,name)
                  (:char . ,char)
                  (:kind . ,kind)
                  (:option-id . ,(map-elt acp-option 'optionId)))
                'alist))))

(defun agent-shell-jump-to-latest-permission-button-row ()
  "Jump to the latest permission button row."
  (interactive)
  (when-let ((found (save-mark-and-excursion
                      (goto-char (point-max))
                      (agent-shell-previous-permission-button))))
    (deactivate-mark)
    (goto-char found)
    (beginning-of-line)
    (agent-shell-next-permission-button)
    (when-let ((window (get-buffer-window (current-buffer))))
      (set-window-point window (point)))))

(defun agent-shell-next-permission-button ()
  "Jump to the next button."
  (interactive)
  (when-let* ((found (save-mark-and-excursion
                       (when (get-text-property (point) 'agent-shell-permission-button)
                         (when-let ((next-change (next-single-property-change (point) 'agent-shell-permission-button)))
                           (goto-char next-change)))
                       (when-let ((next (text-property-search-forward
                                         'agent-shell-permission-button t t)))
                         (prop-match-beginning next)))))
    (deactivate-mark)
    (goto-char found)
    found))

(defun agent-shell-previous-permission-button ()
  "Jump to the previous button."
  (interactive)
  (when-let* ((found (save-mark-and-excursion
                       (when (get-text-property (point) 'agent-shell-permission-button)
                         (when-let ((prev-change (previous-single-property-change (point) 'agent-shell-permission-button)))
                           (goto-char prev-change)))
                       (when-let ((prev (text-property-search-backward
                                         'agent-shell-permission-button t t)))
                         (prop-match-beginning prev)))))
    (deactivate-mark)
    (goto-char found)
    found))

;;; Region

(cl-defun agent-shell--insert-to-shell-buffer (&key text submit no-focus)
  "Insert TEXT into the agent shell buffer at `point-max'.

SUBMIT, when non-nil, submits the shell buffer after insertion.

NO-FOCUS, when non-nil, avoid focusing shell on insertion.

Returns an alist with insertion details or nil otherwise:

  ((:buffer . BUFFER)
   (:start . START)
   (:end . END))"
  (unless text
    (user-error "No text provided to insert"))
  (let* ((shell-buffer (agent-shell--shell-buffer :no-create t))
         (inhibit-read-only t)
         ;; Displaying before with-current-buffer below
         ;; ensures window is selected, thus window-point
         ;; is also updated after insertion.
         (insert-start (if no-focus
                           (with-current-buffer shell-buffer
                             (point-max))
                         (agent-shell--display-buffer shell-buffer)
                         (point-max)))
         (insert-end nil))
    (with-current-buffer shell-buffer
      (when (shell-maker-busy)
        (user-error "Busy, try later"))
      (save-excursion
        (save-restriction
          (goto-char insert-start)
          (unless submit
            (insert "\n\n"))
          (insert text)
          (setq insert-end (point))
          (narrow-to-region insert-start insert-end)
          (let ((markdown-overlays-highlight-blocks agent-shell-highlight-blocks))
            (markdown-overlays-put))))
      (when submit
        (shell-maker-submit)))
    `((:buffer . ,shell-buffer)
      (:start . ,insert-start)
      (:end . ,insert-end))))

(cl-defun agent-shell-insert (&key text submit no-focus)
  "Insert TEXT into the agent shell at `point-max'.

SUBMIT, when non-nil, submits the shell buffer after insertion.

NO-FOCUS, when non-nil, avoid focusing shell on insertion.

When `agent-shell-prefer-viewport-interaction' is non-nil, prefer inserting
into the viewport compose buffer instead of the shell buffer.  If no compose
buffer exists, one will be created.

Returns an alist with insertion details or nil otherwise:

  ((:buffer . BUFFER)
   (:start . START)
   (:end . END))"
  (if agent-shell-prefer-viewport-interaction
      (agent-shell-viewport--show-buffer :text text :submit submit :no-focus no-focus)
    (agent-shell--insert-to-shell-buffer :text text :submit submit :no-focus no-focus)))

(cl-defun agent-shell-send-region ()
  "Send region to last accessed shell buffer in project."
  (interactive)
  (agent-shell-insert
   :text (agent-shell--get-processed-region :deactivate t :no-error t)))

(cl-defun agent-shell-send-dwim ()
  "Send region or error at point to last accessed shell buffer in project."
  (interactive)
  (agent-shell-insert :text (agent-shell--relevant-text)))

(cl-defun agent-shell--get-processed-region (&key deactivate no-error)
  "Get region as insertable text, ready for sending to agent.

When DEACTIVATE is non-nil, deactivate region.

When NO-ERROR is non-nil, return nil and continue without error."
  (let* ((region (or (agent-shell--get-region :deactivate deactivate)
                     (unless no-error
                       (user-error "No region selected"))))
         (processed-text (if (map-elt region :file)
                             (let ((file-link (agent-shell-ui-add-action-to-text
                                               (format "%s:%d-%d"
                                                       (file-relative-name (map-elt region :file)
                                                                           (agent-shell-cwd))
                                                       (map-elt region :line-start)
                                                       (map-elt region :line-end))
                                               (lambda ()
                                                 (interactive)
                                                 (if (and (map-elt region :file) (file-exists-p (map-elt region :file)))
                                                     (if-let ((window (when (get-file-buffer (map-elt region :file))
                                                                        (get-buffer-window (get-file-buffer (map-elt region :file))))))
                                                         (progn
                                                           (select-window window)
                                                           (goto-char (point-min))
                                                           (forward-line (1- (map-elt region :line-start)))
                                                           (beginning-of-line)
                                                           (push-mark (save-excursion
                                                                        (goto-char (point-min))
                                                                        (forward-line (1- (map-elt region :line-end)))
                                                                        (end-of-line)
                                                                        (point))
                                                                      t t))
                                                       (find-file (map-elt region :file))
                                                       (goto-char (point-min))
                                                       (forward-line (1- (map-elt region :line-start)))
                                                       (beginning-of-line)
                                                       (push-mark (save-excursion
                                                                    (goto-char (point-min))
                                                                    (forward-line (1- (map-elt region :line-end)))
                                                                    (end-of-line)
                                                                    (point))
                                                                  t t))
                                                   (message "File not found")))
                                               (lambda ()
                                                 (message "Press RET to open file"))
                                               'link))
                                   (numbered-preview
                                    (when-let ((buffer (get-file-buffer (map-elt region :file))))
                                      (let ((char-start (map-elt region :char-start))
                                            (char-end (map-elt region :char-end))
                                            (max-preview-lines 5))
                                        (if (equal (line-number-at-pos char-start)
                                                   (line-number-at-pos char-end))
                                            ;; Same line region? Avoid numbering.
                                            (buffer-substring char-start char-end)
                                          (agent-shell--get-numbered-region
                                           :buffer buffer
                                           :from char-start
                                           :to char-end
                                           :cap max-preview-lines))))))
                               (if numbered-preview
                                   (concat file-link "\n\n" numbered-preview)
                                 file-link))
                           (map-elt region :content))))
    processed-text))

(cl-defun agent-shell--get-numbered-region (&key buffer from to cap)
  "Get region from BUFFER between FROM and TO locations.

Expands to include entire lines.  Trims empty lines from beginning and end.

If CAP is non-nil, truncate at CAP."
  (with-current-buffer buffer
    (save-excursion
      (goto-char from)
      (let* ((start-line (line-number-at-pos from))
             (end-line (line-number-at-pos to))
             (lines '())
             (current-line start-line))
        (goto-char (point-min))
        (forward-line (1- start-line))
        (while (<= current-line end-line)
          (let ((line-content (buffer-substring
                               (line-beginning-position)
                               (line-end-position))))
            (push (format "   %d: %s" current-line line-content)
                  lines))
          (forward-line 1)
          (setq current-line (1+ current-line)))
        ;; Reverse the lines and trim empty lines from start and end
        (let ((reversed-lines (nreverse lines)))
          ;; Trim empty lines from the beginning
          (while (and reversed-lines
                      (string-match-p "^   [0-9]+:[[:space:]]*$" (car reversed-lines)))
            (setq reversed-lines (cdr reversed-lines)))
          ;; Trim empty lines from the end
          (setq reversed-lines (nreverse reversed-lines))
          (while (and reversed-lines
                      (string-match-p "^   [0-9]+:[[:space:]]*$" (car reversed-lines)))
            (setq reversed-lines (cdr reversed-lines)))
          ;; Reverse back to correct order and apply cap before final join
          (let ((final-lines (nreverse reversed-lines)))
            ;; Apply cap if specified
            (when (and cap (> (length final-lines) cap))
              (setq final-lines (append (seq-take final-lines cap) '("   ..."))))
            (string-join final-lines "\n")))))))

(defun agent-shell--get-processed-flymake-error-at-point ()
  "Get flymake error at point, ready for sending to agent."
  (when-let ((diagnostics (flymake-diagnostics (point))))
    (mapconcat
     (lambda (diagnostic)
       (let* ((buffer (flymake-diagnostic-buffer diagnostic))
              (beg (flymake-diagnostic-beg diagnostic))
              (end (flymake-diagnostic-end diagnostic))
              (type (flymake-diagnostic-type diagnostic))
              (text (flymake-diagnostic-text diagnostic))
              (file (agent-shell--shorten-paths (buffer-file-name buffer) t))
              (line (with-current-buffer buffer
                      (line-number-at-pos beg)))
              (col (with-current-buffer buffer
                     (save-excursion
                       (goto-char beg)
                       (current-column))))
              (code (with-current-buffer buffer
                      (buffer-substring beg end)))
              (context-lines 3)
              (context (with-current-buffer buffer
                         (save-excursion
                           (goto-char beg)
                           (let* ((start-line (max 1 (- line context-lines)))
                                  (context-beg (progn
                                                 (goto-char (point-min))
                                                 (forward-line (1- start-line))
                                                 (point)))
                                  (context-end (progn
                                                 (forward-line (+ context-lines context-lines 1))
                                                 (point)))
                                  (numbered-region (agent-shell--get-numbered-region
                                                    :buffer buffer
                                                    :from context-beg
                                                    :to context-end))
                                  ;; Replace the line number prefix for the error line
                                  (error-line-prefix (format "   %d:" line))
                                  (highlight-prefix (format "-> %d:" line)))
                             (replace-regexp-in-string
                              (regexp-quote error-line-prefix)
                              highlight-prefix
                              numbered-region
                              nil 'literal))))))
         (format (if (string-empty-p (string-trim code))
                     "%s:%d:%d: %s: %s

%s")
                 (or file (buffer-name buffer))
                 line
                 col
                 type
                 text
                 context)))
     diagnostics
     "\n\n")))

(defun agent-shell--relevant-text ()
  "Return relevant text (if available).  Nil otherwise.

Relevant text could be either a region or error at point or files."
  (or (agent-shell--processed-files
       :files (agent-shell--buffer-files :obvious t))
      (agent-shell--get-processed-region
       :deactivate t :no-error t)
      (agent-shell--get-processed-flymake-error-at-point)))

(cl-defun agent-shell--get-region (&key deactivate)
  "Get the active region as an alist.

When DEACTIVATE is non-nil, deactivate region/selection.

Available values:

 :file :language :char-start :char-end :line-start :line-end and :content."
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end))
          (content (buffer-substring-no-properties (region-beginning) (region-end)))
          (language (cond ((listp mode-name)
                           (downcase (car mode-name)))
                          ((stringp mode-name)
                           (downcase mode-name))
                          (t
                           "")))
          (file (when-let ((buffer-file-name (buffer-file-name)))
                  (file-relative-name buffer-file-name (agent-shell-cwd)))))
      (when deactivate
        (deactivate-mark))
      `((:file . ,file)
        (:language . ,language)
        (:char-start . ,start)
        (:char-end . ,end)
        (:line-start . ,(save-excursion (goto-char start) (line-number-at-pos)))
        (:line-end . ,(save-excursion (goto-char end) (line-number-at-pos)))
        (:content . ,content)))))

(cl-defun agent-shell--align-alist (&key data columns (separator "  ") joiner)
  "Align COLUMNS from DATA.

DATA is a list of alists.  COLUMNS is a list of extractor functions,
where each extractor takes one alist and returns a string for that
column.  SEPARATOR is the string used to join columns (defaults to
two spaces).  JOINER, when provided, wraps the result with
`string-join' using JOINER as the separator.

Returns a list of strings with spaced-aligned columns, or a single
joined string if JOINER is provided."
  (let* ((rows (mapcar
                (lambda (item)
                  (mapcar (lambda (extractor) (funcall extractor item))
                          columns))
                data))
         (widths (seq-reduce
                  (lambda (acc row)
                    (seq-mapn #'max
                              acc
                              (mapcar (lambda (cell) (length (or cell ""))) row)))
                  rows
                  (make-list (length columns) 0)))
         (result (mapcar (lambda (row)
                           (string-join
                            (seq-mapn (lambda (cell width)
                                        (format (format "%%-%ds" width) (or cell "")))
                                      row
                                      widths)
                            separator))
                         rows)))
    (if joiner
        (string-join result joiner)
      result)))

(cl-defun agent-shell--get-decorated-region (&key deactivate)
  "Get the active region decorated with file path and Markdown code block.

When DEACTIVATE is non-nil, deactivate region/selection."
  (when-let ((region-data (agent-shell--get-region :deactivate deactivate)))
    (let ((file (map-elt region-data :file))
          (start (map-elt region-data :char-start))
          (end (map-elt region-data :char-end))
          (language (map-elt region-data :language))
          (content (map-elt region-data :content)))
      (concat (if file
                  (format "%s#C%d-C%d\n\n" file start end)
                "")
              "```"
              language
              "\n"
              content
              "\n"
              "```"))))

;;; Session modes

(defun agent-shell--get-available-modes (state)
  "Get available modes list, preferring session modes over agent modes.

STATE is the agent shell state.

Returns the modes list from session if available, otherwise from
the agent's available modes."
  (or (map-nested-elt state '(:session :modes))
      ;; Use agent-level availability as fallback.
      (map-nested-elt state '(:available-modes :modes))))

(defun agent-shell--resolve-session-mode-name (mode-id available-session-modes)
  "Get the name of the session mode with MODE-ID from AVAILABLE-SESSION-MODES.

AVAILABLE-SESSION-MODES is the list of mode objects from the ACP
session/new response.  Each mode has an `:id' and `:name' field.
We look up the mode by ID to get its display name.

See https://agentclientprotocol.com/protocol/session-modes for details."
  (when-let ((mode (seq-find (lambda (m)
                               (string= mode-id (map-elt m :id)))
                             available-session-modes)))
    (map-elt mode :name)))

(defun agent-shell--status-frame ()
  "Return busy frame string or nil if not busy."
  (when (eq 'busy (map-nested-elt (agent-shell--state) '(:heartbeat :status)))
    (let ((frames [" ░   " " ░░  " " ░░░ " " ░░░░" " ░░░ " " ░░  " " ░   " "     "]))
      (seq-elt frames (mod (map-nested-elt (agent-shell--state) '(:heartbeat :value))
                           (length frames))))))

(defun agent-shell--mode-line-format ()
  "Return `agent-shell''s mode-line format.

Typically includes the model, session mode and activity or nil if unavailable.

For example: \" [Sonnet] [Accept Edits] ░░░ \"."
  (when-let* (((derived-mode-p 'agent-shell-mode))
              ((memq agent-shell-header-style '(text none nil))))
    (concat (when-let ((model-name (or (map-elt (seq-find (lambda (model)
                                                            (string= (map-elt model :model-id)
                                                                     (map-nested-elt (agent-shell--state) '(:session :model-id))))
                                                          (map-nested-elt (agent-shell--state) '(:session :models)))
                                                :name)
                                       (map-nested-elt (agent-shell--state) '(:session :model-id)))))
              (propertize (format " [%s]" model-name)
                          'face 'font-lock-variable-name-face
                          'help-echo (format "Model: %s" model-name)))
            (when-let ((mode-name (agent-shell--resolve-session-mode-name
                                   (map-nested-elt (agent-shell--state) '(:session :mode-id))
                                   (agent-shell--get-available-modes (agent-shell--state)))))
              (propertize (format " [%s]" mode-name)
                          'face 'font-lock-type-face
                          'help-echo (format "Session Mode: %s" mode-name)))
            (agent-shell--status-frame))))

(defun agent-shell--setup-modeline ()
  "Set up the modeline to display session mode.
Uses :eval so the mode updates automatically when state changes."
  (setq-local mode-line-misc-info
              (append mode-line-misc-info
                      '((:eval (agent-shell--mode-line-format))))))

(defun agent-shell-cycle-session-mode (&optional on-success)
  "Cycle through available session modes for the current `agent-shell' session.

Optionally, get notified of completion with ON-SUCCESS function."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (user-error "No active session"))
  (unless (agent-shell--get-available-modes (agent-shell--state))
    (user-error "No session modes available"))
  (let* ((mode-ids (mapcar (lambda (mode)
                             (map-elt mode :id))
                           (agent-shell--get-available-modes (agent-shell--state))))
         (mode-idx (or (seq-position mode-ids
                                     (map-nested-elt (agent-shell--state) '(:session :mode-id))
                                     #'string=) -1))
         (next-mode-idx (mod (1+ mode-idx) (length mode-ids)))
         (next-mode-id (nth next-mode-idx mode-ids)))
    (acp-send-request
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-mode-request
               :session-id (map-nested-elt (agent-shell--state) '(:session :id))
               :mode-id next-mode-id)
     :buffer (current-buffer)
     :on-success (lambda (_response)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :mode-id next-mode-id)
                     (map-put! (agent-shell--state) :session updated-session)
                     (message "Session mode: %s"
                              (agent-shell--resolve-session-mode-name
                               next-mode-id
                               (agent-shell--get-available-modes (agent-shell--state)))))
                   (agent-shell--update-header-and-mode-line)
                   (when on-success
                     (funcall on-success)))
     :on-failure (lambda (error _raw-message)
                   (message "Failed to change session mode: %s" error)))))

(defun agent-shell-set-session-mode (&optional on-success)
  "Set session mode (if any available).

Optionally, get notified of completion with ON-SUCCESS function."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (user-error "No active session"))
  (unless (agent-shell--get-available-modes (agent-shell--state))
    (user-error "No session modes available"))
  (let* ((current-mode-id (map-nested-elt (agent-shell--state) '(:session :mode-id)))
         (default-mode-name (and current-mode-id
                                 (agent-shell--resolve-session-mode-name
                                  current-mode-id
                                  (agent-shell--get-available-modes (agent-shell--state)))))
         (mode-choices (mapcar (lambda (mode)
                                 (cons (map-elt mode :name)
                                       (map-elt mode :id)))
                               (agent-shell--get-available-modes (agent-shell--state))))
         (selection (completing-read "Set session mode: "
                                     (mapcar #'car mode-choices)
                                     nil t nil nil default-mode-name))
         (selected-mode-id (cdr (seq-find (lambda (choice)
                                            (string= selection (car choice)))
                                          mode-choices))))
    (unless selected-mode-id
      (user-error "Unknown session mode: %s" selection))
    (when (and current-mode-id (string= selected-mode-id current-mode-id))
      (error "Session mode already %s" selection))
    (acp-send-request
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-mode-request
               :session-id (map-nested-elt (agent-shell--state) '(:session :id))
               :mode-id selected-mode-id)
     :buffer (current-buffer)
     :on-success (lambda (_response)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :mode-id selected-mode-id)
                     (map-put! (agent-shell--state) :session updated-session)
                     (message "Session mode: %s"
                              (agent-shell--resolve-session-mode-name
                               selected-mode-id
                               (agent-shell--get-available-modes (agent-shell--state)))))
                   (agent-shell--update-header-and-mode-line)
                   (when on-success
                     (funcall on-success)))
     :on-failure (lambda (error _raw-message)
                   (message "Failed to change session mode: %s" error)))))

(defun agent-shell-set-session-model (&optional on-success)
  "Set session model.

Optionally, get notified of completion with ON-SUCCESS function."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (user-error "No active session"))
  (unless (map-nested-elt (agent-shell--state) '(:session :models))
    (user-error "No session models available"))
  (let* ((current-model-id (map-nested-elt (agent-shell--state) '(:session :model-id)))
         (available-models (map-nested-elt (agent-shell--state) '(:session :models)))
         (default-model-name (and current-model-id
                                  (map-elt (seq-find (lambda (model)
                                                       (string= (map-elt model :model-id) current-model-id))
                                                     available-models)
                                           :name)))
         (model-choices (seq-mapn (lambda (title model)
                                    (cons title (map-elt model :model-id)))
                                  (agent-shell--align-alist
                                   :data available-models
                                   :columns (list
                                             (lambda (model)
                                               (map-elt model :name))
                                             (lambda (model)
                                               (format "(%s)" (map-elt model :model-id)))))
                                  available-models))
         (selection (completing-read "Set model: "
                                     (mapcar #'car model-choices)
                                     nil t nil nil
                                     (and default-model-name
                                          (car (seq-find (lambda (choice)
                                                           (string-prefix-p default-model-name (car choice)))
                                                         model-choices)))))
         (selected-model-id (cdr (seq-find (lambda (choice)
                                             (string= selection (car choice)))
                                           model-choices))))
    (unless selected-model-id
      (user-error "Unknown model: %s" selection))
    (when (and current-model-id (string= selected-model-id current-model-id))
      (error "Session model already %s" (map-elt (seq-find (lambda (model)
                                                             (string= (map-elt model :model-id) selected-model-id))
                                                           available-models)
                                                 :name)))
    (acp-send-request
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-model-request
               :session-id (map-nested-elt (agent-shell--state) '(:session :id))
               :model-id selected-model-id)
     :on-success (lambda (_response)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :model-id selected-model-id)
                     (map-put! (agent-shell--state) :session updated-session)
                     (message "Model: %s"
                              (map-elt (seq-find (lambda (model)
                                                   (string= (map-elt model :model-id) selected-model-id))
                                                 (map-nested-elt (agent-shell--state) '(:session :models)))
                                       :name)))
                   (agent-shell--update-header-and-mode-line)
                   (when on-success
                     (funcall on-success)))
     :on-failure (lambda (error _raw-message)
                   (message "Failed to change model: %s" error)))))

(defun agent-shell--format-available-modes (modes)
  "Format MODES for shell rendering.
If CURRENT-MODE-ID is provided, append \"(current)\" to the matching mode name."
  (agent-shell--align-alist
   :data modes
   :columns (list
             (lambda (mode)
               (when (map-elt mode :name)
                 (propertize (format "%s (%s)"
                                     (map-elt mode :name)
                                     (map-elt mode :id))
                             'font-lock-face 'font-lock-function-name-face)))
             (lambda (mode)
               (when (map-elt mode :description)
                 (propertize (map-elt mode :description)
                             'font-lock-face 'font-lock-comment-face))))
   :joiner "\n"))

(defun agent-shell--format-available-models (models)
  "Format MODELS for shell rendering.

Mark model using CURRENT-MODEL-ID."
  (agent-shell--align-alist
   :data models
   :columns (list
             (lambda (model)
               (map-elt model :name)
               (when (map-elt model :name)
                 (propertize (map-elt model :name)
                             'font-lock-face 'font-lock-function-name-face)))
             (lambda (model)
               (when (map-elt model :description)
                 (propertize (map-elt model :description)
                             'font-lock-face 'font-lock-comment-face))))
   :joiner "\n"))

;;; Transient

(transient-define-prefix agent-shell-help-menu ()
  "Transient menu for `agent-shell' commands."
  [["Navigation"
    ("<tab>" "Next item" agent-shell-next-item :transient t)
    ("<backtab>" "Previous item" agent-shell-previous-item :transient t)]
   ["Insert"
    ("!" "Shell command" agent-shell-insert-shell-command-output :transient t)
    ("@" "File" agent-shell-insert-file :transient t)
    ]]
  [["Session"
    ("m" "Cycle modes" agent-shell-cycle-session-mode :transient t)
    ("M" "Set mode" agent-shell-set-session-mode :transient t)
    ("v" "Set model" agent-shell-set-session-model :transient t)
    ("C" "Interrupt" agent-shell-interrupt :transient t)]
   ["Shell"
    ("b" "Toggle" agent-shell-toggle :transient t)
    ("N" "New shell" agent-shell-new-shell)]])

;;; Transcript

(defcustom agent-shell-transcript-file-path-function #'agent-shell--default-transcript-file-path
  "Function to generate the full transcript file path.
Called with no arguments, should return a string path or nil to disable.
When nil, transcript saving is disabled."
  :type '(choice (const :tag "Disabled" nil)
                 (function :tag "Custom function"))
  :group 'agent-shell)

(defun agent-shell--default-transcript-file-path ()
  "Generate a transcript file path in project root.

For example:

 project/.agent-shell/transcripts/."
  (let* ((dir (expand-file-name ".agent-shell/transcripts" (agent-shell-cwd)))
         (filename (format-time-string "%F-%H-%M-%S.md"))
         (filepath (expand-file-name filename dir)))
    filepath))

(defun agent-shell--init-transcript (config)
  "Initialize a new transcript file for this buffer using CONFIG.
Returns the path to the transcript file, or nil if disabled."
  (when-let* ((path-fn agent-shell-transcript-file-path-function)
              (filepath (condition-case err
                            (funcall path-fn)
                          (error
                           (message "Failed to generate transcript path: %S" err)
                           nil)))
              (dir (file-name-directory filepath))
              (agent-name (or (map-elt config :mode-line-name)
                              (map-elt config :buffer-name)
                              "Unknown Agent")))
    (condition-case err
        (progn
          (make-directory dir t)
          (write-region
           (format "# Agent Shell Transcript

**Agent:** %s
**Started:** %s
**Working Directory:** %s

---

"
                   agent-name
                   (format-time-string "%F %T")
                   (agent-shell-cwd))
           nil filepath nil 'no-message)
          (message "Created %s"
                   (agent-shell--shorten-paths filepath t))
          filepath)
      (error
       (message "Failed to initialize transcript: %S" err)
       nil))))

(cl-defun agent-shell--append-transcript (&key text file-path)
  "Append TEXT to the transcript at FILE-PATH."
  (when (and file-path (file-exists-p file-path))
    (condition-case err
        (write-region text nil file-path t 'no-message)
      (error
       (message "Error writing to transcript: %S" err)))))

(cl-defun agent-shell--make-transcript-tool-call-entry (&key status title kind description command output)
  "Create a formatted transcript entry for a tool call.

Includes STATUS, TITLE, KIND, DESCRIPTION, COMMAND, and OUTPUT."
  (concat
   (format "\n\n### Tool Call [%s]: %s\n"
           (or status "no status") (or title ""))
   (when kind
     (format "\n**Tool:** %s" kind))
   (format "\n**Timestamp:** %s" (format-time-string "%F %T"))
   (when description
     (format "\n**Description:** %s" description))
   (when command
     (format "\n**Command:** %s" command))
   "\n\n"
   "```"
   "\n"
   (string-trim output
                "^```\n?" "```$")
   "\n"
   "```"
   "\n"))

(defun agent-shell-open-transcript ()
  "Open the transcript file for the current `agent-shell' buffer."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in an agent-shell buffer"))
  (unless agent-shell--transcript-file
    (error "No transcript file available for this buffer"))
  (unless (file-exists-p agent-shell--transcript-file)
    (error "Transcript file does not exist: %s" agent-shell--transcript-file))
  (find-file agent-shell--transcript-file))

;;; Queueing

(cl-defun agent-shell--process-pending-request ()
  "Process the next pending request from the queue if available."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (when-let ((pending (map-elt agent-shell--state :pending-requests))
             (next-request (car pending)))
    (map-put! agent-shell--state :pending-requests (cdr pending))
    (agent-shell--insert-to-shell-buffer
     :text next-request
     :submit t
     :no-focus t)))

(defun agent-shell--display-pending-requests ()
  "Display pending requests in the shell buffer if queue is not empty."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (unless (seq-empty-p (map-elt agent-shell--state :pending-requests))
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :block-id (format "%s-pending-requests"
                       (map-elt (agent-shell--state) :request-count))
     :body (format "Pending requests: %d

%s

Resume: M-x agent-shell-resume-pending-requests
Remove: M-x agent-shell-remove-pending-request
"
                   (seq-length (map-elt agent-shell--state :pending-requests))
                   (mapconcat
                    (lambda (idx-req)
                      (let* ((req (car idx-req))
                             (idx (cdr idx-req))
                             (first-line (car (split-string req "\n" t))))
                        (format "  %d: \"%s\""
                                (1+ idx)
                                (truncate-string-to-width first-line 80 nil nil "..."))))
                    (seq-map-indexed #'cons (map-elt agent-shell--state :pending-requests))
                    "\n"))
     :create-new t)))

(cl-defun agent-shell--enqueue-request (&key prompt)
  "Add PROMPT to the pending requests queue."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (let ((pending (map-elt agent-shell--state :pending-requests)))
    (map-put! agent-shell--state :pending-requests
              (append pending (list prompt)))
    (message "Request queued (%d pending)" (length (map-elt agent-shell--state :pending-requests)))))

(defun agent-shell-queue-request (prompt)
  "Queue or immediately send a request depending on shell busy state.

Read PROMPT from the minibuffer.  If the shell is busy, add it to the pending
requests queue.  Otherwise, submit it immediately.  Queued requests will be
automatically sent when the current request completes."
  (interactive
   (progn
     (unless (derived-mode-p 'agent-shell-mode)
       (error "Not in a shell"))
     (list (read-string (or (map-nested-elt (agent-shell--state) '(:agent-config :shell-prompt))
                            "Enqueue request: ")))))
  (if (shell-maker-busy)
      (agent-shell--enqueue-request :prompt prompt)
    (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t)))

(defun agent-shell-resume-pending-requests ()
  "Resume processing pending requests in the queue."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (when (seq-empty-p (map-elt agent-shell--state :pending-requests))
    (user-error "No pending requests"))
  (if (shell-maker-busy)
      (message "Shell is busy, requests will auto-resume when ready")
    (agent-shell--process-pending-request)))

(defun agent-shell-remove-pending-request (&optional remove-index)
  "Remove all pending requests or a specific request by REMOVE-INDEX.

When called interactively with pending requests, prompt to either remove all
or select a specific request to remove."
  (interactive
   (let ((pending (map-elt agent-shell--state :pending-requests)))
     (unless (derived-mode-p 'agent-shell-mode)
       (error "Not in a shell"))
     (when (seq-empty-p pending)
       (user-error "No pending requests"))
     (let* ((choices (append
                      '(("Remove all" . remove-all))
                      (seq-map-indexed
                       (lambda (req idx)
                         (cons (format "%d: %s" (1+ idx)
                                       (truncate-string-to-width req 60 nil nil "..."))
                               idx))
                       pending)))
            (selection (cdr (assoc (completing-read "Remove: " choices nil t) choices))))
       (list (unless (eq selection 'remove-all) selection)))))
  (if remove-index
      (when-let* ((confirmed (y-or-n-p (format "Remove? \"%s\""
                                               (nth remove-index
                                                    (map-elt agent-shell--state :pending-requests)))))
                  (pending (map-elt agent-shell--state :pending-requests))
                  (new-pending (append (seq-take pending remove-index)
                                       (seq-drop pending (1+ remove-index)))))
        (map-put! agent-shell--state :pending-requests new-pending)
        (message "Removed (%d remaining)"
                 (length new-pending)))
    (when (y-or-n-p (format "Remove %d pending requests? "
                            (length (map-elt agent-shell--state :pending-requests))))
      (map-put! agent-shell--state :pending-requests nil)
      (message "Removed all pending requests"))))

(provide 'agent-shell)

;;; agent-shell.el ends here
