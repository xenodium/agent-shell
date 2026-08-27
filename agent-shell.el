;;; agent-shell.el --- Native agentic integrations for Claude Code, Gemini CLI, etc  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell
;; Version: 0.74.3
;; Package-Requires: ((emacs "29.1") (shell-maker "0.97.2") (acp "0.13.1"))

(defconst agent-shell--version "0.74.3")

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
;; CodeBuddy, Gemini CLI, Goose, Grok Build (xAI), Codex, OpenCode,
;; Qwen, and Auggie amongst other agents.
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
(require 'color)
(require 'agent-shell-work-buffer)
(require 'dired)
(require 'diff)
(require 'json)
(require 'mailcap)
(require 'map)
(unless (require 'markdown-overlays nil 'noerror)
  (error "Please update 'shell-maker' to v0.91.2 or newer"))
(require 'agent-shell-artist)
(require 'agent-shell-faces)
(require 'agent-shell-markdown)
(require 'agent-shell-antigravity)
(require 'agent-shell-anthropic)
(require 'agent-shell-auggie)
(require 'agent-shell-chat-mode)
(require 'agent-shell-codebuddy)
(require 'agent-shell-cline)
(require 'agent-shell-completion)
(require 'agent-shell-config)
(require 'agent-shell-cursor)
(require 'agent-shell-devcontainer)
(require 'agent-shell-diff)
(require 'agent-shell-experimental)
(require 'agent-shell-droid)
(require 'agent-shell-github)
(require 'agent-shell-google)
(require 'agent-shell-goose)
(require 'agent-shell-heartbeat)
(require 'agent-shell-active-message)
(require 'agent-shell-hermes)
(require 'agent-shell-kimi)
(require 'agent-shell-kiro)
(require 'agent-shell-mistral)
(require 'agent-shell-omp)
(require 'agent-shell-openai)
(require 'agent-shell-opencode)
(require 'agent-shell-pi)
(require 'agent-shell-project)
(require 'agent-shell-prompt-queue)
(require 'agent-shell-qwen)
(require 'agent-shell-styles)
(require 'agent-shell-usage)
(require 'agent-shell-worktree)
(require 'agent-shell-ui)
(require 'agent-shell-viewport)
(require 'agent-shell-xai)
(require 'image)
(require 'markdown-overlays)
(require 'shell-maker)
(require 'svg nil :noerror)
(require 'transient)

;; Optional flycheck integration (used in agent-shell--get-flycheck-error-context)
(declare-function flycheck-overlay-errors-at "flycheck" (pos))
(declare-function flycheck-error-pos "flycheck" (err))
(declare-function flycheck-error-end-line "flycheck" (err))
(declare-function flycheck-error-end-column "flycheck" (err))
(declare-function flycheck-error-level "flycheck" (err))
(declare-function flycheck-error-message "flycheck" (err))
(declare-function flycheck-error-line "flycheck" (err))
(declare-function flycheck-error-column "flycheck" (err))

;; Declare as special so byte-compilation doesn't turn `let' bindings into
;; lexical bindings (which would not affect `auto-insert' behavior).
(defvar auto-insert)


(defcustom agent-shell-permission-icon "⚠"
  "Icon displayed when shell commands require permission to execute.

You may use \"􀇾\" as an SF Symbol on macOS."
  :type 'string
  :group 'agent-shell)

(defcustom agent-shell-thought-process-icon "✶"
  "Icon displayed during the AI's thought process.

Displays a diamond instead on displays that cannot draw it.

Favour single width icons as they line \"Thinking\" up with the status
icons beside tool calls; wider ones, including most emoji and the SF
Symbols shift it right by the difference."
  :type 'string
  :group 'agent-shell)

(defun agent-shell--thought-process-icon ()
  "Return the thought process icon this display can draw, else nil.

Resolved per call rather than at load time: whether a character is
drawable depends on the frame, which may not exist yet when this file
loads (a daemon starting before its first graphical frame).  Falls back
to a diamond, drawn by most monospace fonts.

  (agent-shell--thought-process-icon)
  ;; => \"⚹\" where its font is available, \"◇\" otherwise"
  (when-let* ((icon agent-shell-thought-process-icon)
              ((not (string-empty-p icon))))
    (if (seq-every-p #'char-displayable-p (string-to-list icon))
        icon
      "◇")))

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

(defcustom agent-shell-activity-group-expand-by-default 'latest
  "When activity group sections should be expanded.

An activity group is a run of consecutive agent actions (tool calls,
and eventually thoughts) rendered under one collapsible header.

  nil      Groups start collapsed, showing only the header with its
           aggregated status and completed/total count.
  t        Groups show their members.
  `latest' The group the agent is currently working in shows its
           members, and collapses once the agent moves on to a new
           group or the turn ends.  Keeps the session tidy while
           still making the agent's current activity followable.

Individual members still follow `agent-shell-tool-use-expand-by-default'."
  :type '(choice (const :tag "Never (collapsed)" nil)
                 (const :tag "Always (expanded)" t)
                 (const :tag "While the agent is working in it" latest))
  :group 'agent-shell)

(defun agent-shell--activity-group-initial-expanded-p ()
  "Return non-nil when a newly created activity group starts expanded.

Both t and `latest' start expanded; `latest' collapses the group again
once the agent moves on (see `agent-shell--sync-activity-group-fold')."
  (and agent-shell-activity-group-expand-by-default t))

(defvar agent-shell-mode-hook nil
  "Hook run after an `agent-shell-mode' buffer is fully initialized.
Runs after the buffer-local state has been set up, so it is safe to
call `agent-shell-subscribe-to' from here.")

(defvar agent-shell-permission-responder-function nil
  "When non-nil, a function called before showing the permission prompt.

Return non-nil to indicate the request was handled (UI is skipped).
Return nil to fall back to the interactive permission dialog.

Called with an alist containing:

  :tool-call - the tool call alist with :title, :kind, :status,
               :permission-request-id, and optionally :diff
  :options   - enriched actions, each with :kind, :option-id,
               :label, :option, :char
  :respond   - function taking an option-id to respond programmatically

See `agent-shell-permission-allow-always' for a built-in handler
that auto-approves all requests.

Example -- auto-approve reads:

  (setq agent-shell-permission-responder-function
        (lambda (permission)
          (when-let* (((equal (map-elt (map-elt permission :tool-call) :kind)
                             \"read\"))
                      (choice (seq-find
                                (lambda (option)
                                  (equal (map-elt option :kind) \"allow_once\"))
                                (map-elt permission :options))))
            (funcall (map-elt permission :respond)
                     (map-elt choice :option-id))
            t)))")

(defun agent-shell-permission-allow-always (permission)
  "Auto-approve all PERMISSION requests.

Intended for use with `agent-shell-permission-responder-function'.

Example:

  (setq agent-shell-permission-responder-function
        #\\='agent-shell-permission-allow-always)"
  (when-let* ((choice (seq-find
                       (lambda (option) (equal (map-elt option :kind) "allow_once"))
                       (map-elt permission :options))))
    (funcall (map-elt permission :respond)
             (map-elt choice :option-id))
    t))

(defcustom agent-shell-user-message-expand-by-default nil
  "Whether user message sections should be expanded by default.

When nil (the default), user message sections are collapsed.
When non-nil, user message sections are expanded."
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

(defvaralias
  'agent-shell-container-command-runner
  'agent-shell-command-prefix)

(defcustom agent-shell-command-prefix nil
  "Prefix to apply when executing agent commands and shell commands.

Can be a list of strings or a function or lambda that takes a buffer and
returns a list of strings.

Example for static list of strings:
  \\='(\"devcontainer\" \"exec\" \"--workspace-folder\" \".\")

Example for a lambda:
  (lambda (buffer)
    (let ((config (agent-shell-get-config buffer)))
      (pcase (map-elt config :identifier)
        (\\='claude-code \\='(\"docker\" \"exec\" \"claude-dev\" \"--\"))
        (\\='gemini-cli \\='(\"docker\" \"exec\" \"gemini-dev\" \"--\"))
        (_ (error \"Unknown identifier\")))))"
  :type '(choice (repeat string) function)
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

(defcustom agent-shell-highlight-blocks t
  "Whether or not to highlight source blocks."
  :type 'boolean
  :group 'agent-shell)

(cl-defun agent-shell--markdown-overlays-put (&key render-images highlight-blocks
                                                   &allow-other-keys)
  "Deprecated overlay-based markdown renderer.

Wraps `markdown-overlays-put' from the `markdown-overlays' package
and translates agent-shell's renderer-agnostic config to the
`markdown-overlays-*' variables it expects, so call sites don't
need to know about the overlay package's variable names.
RENDER-IMAGES toggles image rendering; HIGHLIGHT-BLOCKS toggles
source-block highlighting.

Deprecated in favour of `agent-shell-markdown-replace-markup' (the
in-place renderer, now the default).  Kept for backwards
compatibility; will be removed once the in-place renderer has
settled and `markdown-overlays' is no longer a dependency."
  (let ((markdown-overlays-render-images render-images)
        (markdown-overlays-highlight-blocks highlight-blocks))
    (markdown-overlays-put)))

(defcustom agent-shell-markdown-render-function
  #'agent-shell-markdown-replace-markup
  "Function called to render markdown in the current narrowed buffer.

The function accepts `&key render-images highlight-blocks complete
image-cache-directory' (use `&allow-other-keys' to tolerate keys a
renderer ignores) and is expected to render markdown in the
current buffer.  COMPLETE marks a render nothing will be
appended to, so a renderer holding markup back while it could
still grow can settle it (see
`agent-shell--render-deferred-images').

Callers narrow the buffer to the target span
\(for example, a fragment body or label) before calling, so the function can
scan the whole accessible portion.

Two implementations ship with agent-shell:

  - `agent-shell-markdown-replace-markup' (default): in-place
    renderer that rewrites markup characters into propertized text
    (no overlays).  Faster on streaming workloads by rewriting
    buffer.

  - `agent-shell--markdown-overlays-put' (deprecated):
    overlay-based renderer wrapping `markdown-overlays-put'.
    Honors both keyword arguments via the corresponding
    `markdown-overlays-*' variables.  Will be removed once the
    in-place renderer has settled.

Set to a custom function to plug in a different renderer; the
function should accept `&key render-images highlight-blocks
image-cache-directory &allow-other-keys'."
  :type 'function
  :group 'agent-shell)

(cl-defun agent-shell--render-markdown
    (&key (render-images t) (highlight-blocks agent-shell-highlight-blocks)
          (external-renderers t) complete)
  "Render markdown in the current narrowed buffer.

Dispatches to `agent-shell-markdown-render-function', forwarding
RENDER-IMAGES and HIGHLIGHT-BLOCKS.  HIGHLIGHT-BLOCKS defaults to
the current value of `agent-shell-highlight-blocks' so most call
sites can omit it; RENDER-IMAGES defaults to t, override with nil
on label spans where images shouldn't appear.

COMPLETE marks a render nothing will be appended to, so markup the
streaming passes hold back renders now (see
`agent-shell--render-deferred-images').  Left nil while streaming.

EXTERNAL-RENDERERS defaults to t.  Pass nil on single-line label
spans to suppress `agent-shell-markdown-render-functions'.  Those
renderers (e.g. a LaTeX-math package) draw images, which labels
already opt out of via RENDER-IMAGES, and the render-function
contract gives them no way to tell UI chrome from agent prose.
The hook is bound to nil rather than emptied selectively: nil also
drops the t marker a buffer-local hook value uses to run global
members, so neither local nor global renderers run.

Passes agent-shell's own cache directory as the renderer's remote-image
cache so downloaded images share `agent-shell-cache-dir'."
  (let ((agent-shell-markdown-render-functions
         (when external-renderers
           agent-shell-markdown-render-functions)))
    (funcall agent-shell-markdown-render-function
             :render-images render-images
             :highlight-blocks highlight-blocks
             :complete complete
             :image-cache-directory (agent-shell-cache-dir "content"))))

(defun agent-shell--render-deferred-images ()
  "Render image markup the streaming passes held back, the turn being over.

An image whose markup ends the text rendered so far is left raw: a
`{width=...}' block may still be streaming in behind it, and rendering
before it lands would strand those attributes as literal text (see
`agent-shell-markdown--image-attributes-pending-p').  A response ending
in an image never gets that following chunk, so its markup stays raw
until a render marked complete comes along.

Re-renders, as complete, every fragment body still holding raw image
markup.  Bodies without any are left untouched, so a turn ending in
prose costs one scan.

Collapsed bodies are re-rendered too, unlike while streaming, where
they are skipped because expanding one renders it.  That later render
is not marked complete, so skipping them here would leave an image
ending a folded tool call raw for good.

For example, a body left as \"Here it is\\n\\n![plot](/tmp/plot.png)\"
ends up showing the image, while a body of prose is untouched."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
          (buffer-undo-list t)
          (regexp (agent-shell-markdown--link-markup-regexp :as-image? t))
          (match nil))
      (while (setq match (text-property-search-forward
                          'agent-shell-ui-section 'body #'eq))
        (when-let* ((start (prop-match-beginning match))
                    (end (prop-match-end match))
                    ((save-excursion
                       (goto-char start)
                       (re-search-forward regexp end t))))
          (save-restriction
            (narrow-to-region start end)
            (agent-shell--render-markdown :complete t)))))))

(defcustom agent-shell-confirm-interrupt t
  "Whether to prompt for confirmation before interrupting.

When non-nil (the default), `agent-shell-interrupt' and related
commands ask \"Interrupt?\" via `y-or-n-p' before cancelling the
in-progress request.  Set to nil to interrupt immediately without
prompting."
  :type 'boolean
  :group 'agent-shell)

(defun agent-shell-interrupt-confirmed-p ()
  "Prompt the user to confirm an interrupt and return non-nil if confirmed.
When `agent-shell-confirm-interrupt' is nil, skip the prompt and return t."
  (or (not agent-shell-confirm-interrupt)
      (y-or-n-p "Interrupt?")))

(defcustom agent-shell-context-sources '(files region error line)
  "Sources to consider when determining \\<agent-shell-mode-map>\\[agent-shell] automatic context.

Each element can be:
- A symbol: `files', `region', `error', or `line'
- A function: Called with no arguments, should return context or nil

Sources are checked in order until one returns non-nil."
  :type '(repeat (choice (const :tag "Buffer files" files)
                         (const :tag "Selected region" region)
                         (const :tag "Error at point" error)
                         (const :tag "Current line" line)
                         (function :tag "Custom function")))
  :group 'agent-shell)

(cl-defun agent-shell--make-acp-client (&key command
                                             command-params
                                             environment-variables
                                             context-buffer)
  "Create an ACP client.

COMMAND, COMMAND-PARAMS, ENVIRONMENT-VARIABLES, and CONTEXT-BUFFER are
passed through to `acp-make-client'."
  (let* ((full-command (append (list command) command-params))
         (wrapped-command (agent-shell--build-command-for-execution full-command)))
    (acp-make-client :command (car wrapped-command)
                     :command-params (cdr wrapped-command)
                     :environment-variables environment-variables
                     :context-buffer context-buffer
                     :outgoing-request-decorator (when context-buffer
                                                   (map-elt (buffer-local-value 'agent-shell--state context-buffer)
                                                            :outgoing-request-decorator)))))

(defcustom agent-shell-text-file-capabilities t
  "Whether agents are initialized with read/write text file capabilities.

See `acp-make-initialize-request' for details."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-write-inhibit-minor-modes '(aggressive-indent-mode)
  "List of minor mode commands to inhibit during `fs/write_text_file' edits.

Each element is a minor mode command symbol, such as
`aggressive-indent-mode'.

Agent Shell disables any listed modes that are enabled in the target
buffer before applying `fs/write_text_file' edits, and then restores
them.

Modes whose variables are not buffer-local in the target buffer (for
example, globalized minor modes) are ignored."
  :type '(repeat symbol)
  :group 'agent-shell)

(defcustom agent-shell-display-action
  '(display-buffer-same-window)
  "Display action for agent shell buffers.
See `display-buffer' for the format of display actions."
  :type '(cons (repeat function) alist)
  :group 'agent-shell)

(defcustom agent-shell-file-display-action
  '((display-buffer-reuse-window display-buffer-same-window))
  "Display action for files opened from a link, image, or `@file' mention.

See `display-buffer' for the format of display actions: a list of
functions to try, optionally consed onto an alist of parameters.  The
default reuses a window already showing the file, else takes over the
current one.  To keep the conversation in view instead, open beside it:

  (setq agent-shell-file-display-action
        \='(display-buffer-pop-up-window))

The window is selected either way, a link being followed to read what
it points at.  Binary files the operating system handles never reach
this: they open externally.

Note `agent-shell-diff-visit-file' ignores this.  It is a command the
user invokes, where replacing the current window is the usual
expectation."
  :type '(cons (repeat function) alist)
  :group 'agent-shell)

;; Injected rather than read directly: agent-shell-markdown.el knows
;; nothing of agent-shell.  Wrapped in a lambda so the setting is read
;; per call, leaving `setq' and let-binding it working.
(setq agent-shell-markdown-open-file-function
      (lambda (path)
        ;; Nil when the action showed nothing (`display-buffer-no-window'
        ;; with `allow-no-window'), which is a legal answer meaning "leave
        ;; point alone" rather than something to select.
        (when-let* ((window (display-buffer (find-file-noselect path)
                                            agent-shell-file-display-action)))
          (select-window window))))

(defcustom agent-shell-prefer-viewport-interaction nil
  "Non-nil makes `agent-shell' prefer viewport interaction over shell interaction.

For example, `agent-shell-send*' will insert text into the viewport
buffer instead of the shell buffer.  If no viewport buffer exists, one
will be created."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-viewport-dismiss-on-send nil
  "Non-nil dismisses the viewport compose window after sending.

Enables a fire-and-forget workflow: `agent-shell-viewport-compose-send'
queues the composed prompt (or submits it when the shell is idle) and
then dismisses the compose window, restoring the previous window layout.
Prompts are sent without switching to watch the response."
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

(defcustom agent-shell-show-session-id nil
  "Non-nil to display the session ID in the header and session selection.

When enabled, the session ID is shown after the directory path in the
header and as an additional column in the session selection prompt.
Only appears when a session is active."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-show-welcome-message t
  "Non-nil to show welcome message."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-show-busy-indicator t
  "Non-nil to show the busy indicator animation in the header and mode line."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-busy-indicator-frames 'wide
  "Frames for the busy indicator animation.
Can be a symbol selecting a predefined style, or a list of frame strings.
When providing custom frames, do not include leading spaces as padding
is added automatically."
  :type '(choice (const :tag "Circle (blinks)" circle)
                 (const :tag "Wave (pulses up and down)" wave)
                 (const :tag "Dots Block (circular spin)" dots-block)
                 (const :tag "Dots Round (circular spin)" dots-round)
                 (const :tag "Wide (horizontal blocks)" wide)
                 (repeat :tag "Custom frames" string))
  :group 'agent-shell)

(defcustom agent-shell-inhibit-system-sleep t
  "Non-nil to keep the system awake while an agent is busy.

Long-running agent turns can outlast the system idle-sleep timeout,
suspending the machine (and the agent) before the turn completes.  When
non-nil, `agent-shell' blocks system idle sleep for the duration of each
turn and releases the block once the turn finishes, so the system is
only kept awake while there is work in progress.  The display is still
allowed to blank.

This relies on the `system-sleep' library introduced in Emacs 31.1 and
has no effect on earlier versions.  It does not prevent \"hard\" sleep
such as closing a laptop lid."
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

(defcustom agent-shell-clipboard-image-handlers
  (list
   (list (cons :command "wl-paste")
         (cons :save (lambda (file-path)
                       (with-temp-buffer
                         (let* ((coding-system-for-read 'binary)
                                (exit-code (call-process "wl-paste" nil (list t nil) nil "--type" "image/png")))
                           (if (zerop exit-code)
                               (write-region nil nil file-path)
                             (error "Command wl-paste failed with exit code %d" exit-code)))))))
   (list (cons :command "pngpaste")
         (cons :save (lambda (file-path)
                       (let ((exit-code (call-process "pngpaste" nil nil nil file-path)))
                         (unless (zerop exit-code)
                           (error "Command pngpaste failed with exit code %d" exit-code))))))
   (list (cons :command "xclip")
         (cons :save (lambda (file-path)
                       (when-let* ((targets (and (eq (window-system) 'x)
                                                 (gui-get-selection 'CLIPBOARD 'TARGETS)))
                                   ((vectorp targets))
                                   ((not (seq-contains-p targets 'image/png))))
                         (error "No image/png in clipboard"))
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (let ((exit-code (call-process "xclip" nil t nil
                                                        "-selection" "clipboard"
                                                        "-t" "image/png" "-o")))
                           (unless (zerop exit-code)
                             (error "Command xclip failed with exit code %d" exit-code))
                           (write-region (point-min) (point-max) file-path nil 'silent))))))
   (list (cons :command "powershell")
         (cons :save (lambda (file-path)
                       (let ((exit-code (call-process "powershell" nil nil nil
                                                      "-Command"
                                                      (format "& {(Get-Clipboard -Format image).Save(%s)}"
                                                              (shell-quote-argument file-path)))))
                         (unless (zerop exit-code)
                           (error "Command powershell failed with exit code %d" exit-code)))))))
  "Handlers for saving clipboard images to a file.

Each handler is an alist with the following keys:

  :command  The executable name to look up via `executable-find'.
  :save     A function taking FILE-PATH that saves the clipboard
            image there, signaling an error on failure.

Handlers are tried in order.  The first whose :command is found
on the system is used."
  :type '(repeat (alist :key-type symbol :value-type sexp))
  :group 'agent-shell)

(defcustom agent-shell-buffer-name-format 'default
  "Format to use when generating agent shell buffer names.

Each element can be:
- Default: For example \='Claude Agent @ My Project\='
- Kebab case: For example \='claude-agent @ my-project\='
- A function: Called with agent name and project name."
  :type '(choice (const :tag "Default" default)
                 (const :tag "Kebab case" kebab-case)
                 (function :tag "Custom format"))
  :group 'agent-shell)

;;;###autoload
(cl-defun agent-shell-make-agent-config (&key identifier
                                              mode-line-name welcome-function
                                              buffer-name shell-prompt shell-prompt-regexp
                                              client-maker
                                              needs-authentication
                                              authenticate-request-maker
                                              default-model-id
                                              default-session-mode-id
                                              session-meta
                                              mcp-servers
                                              notification-adapter
                                              icon-name
                                              install-instructions)
  "Create an agent configuration alist.

Keyword arguments:
- IDENTIFIER: Symbol identifying agent type (e.g., \\='claude-code)
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
- SESSION-META: Optional alist of agent-specific metadata sent as `_meta'
  with session-creating requests (`session/new', `session/load',
  `session/resume', and `session/fork').
- MCP-SERVERS: Optional list of MCP servers for this agent, taking
  precedence over the global `agent-shell-mcp-servers'.  Same shape as
  that variable.
- NOTIFICATION-ADAPTER: Optional function to modify/normalize `notification'
- ICON-NAME: Name of the icon to use
- INSTALL-INSTRUCTIONS: Instructions to show when executable is not found

Returns an alist with all specified values."
  `((:identifier . ,identifier)
    (:mode-line-name . ,mode-line-name)
    (:welcome-function . ,welcome-function)                     ;; function
    (:buffer-name . ,buffer-name)
    (:shell-prompt . ,shell-prompt)
    (:shell-prompt-regexp . ,shell-prompt-regexp)
    (:client-maker . ,client-maker)                             ;; function
    (:needs-authentication . ,needs-authentication)
    (:authenticate-request-maker . ,authenticate-request-maker) ;; function
    (:default-model-id . ,default-model-id)                     ;; function
    (:default-session-mode-id . ,default-session-mode-id)       ;; function
    (:session-meta . ,session-meta)
    (:mcp-servers . ,mcp-servers)
    (:notification-adapter . ,notification-adapter)            ;; function
    (:icon-name . ,icon-name)
    (:install-instructions . ,install-instructions)))

(defun agent-shell-default-agent-config-makers ()
  "Return the list of default agent config maker functions.

Each element is a function that returns a configuration alist when
called.  Keeping makers (rather than pre-built configurations) means
configurations are rebuilt on access and stay current across code
reloads.

This is the default value of `agent-shell-agent-configs'.  It is exposed
so a function value for that variable can build on the defaults, for
example filtering them.  See `agent-shell-agent-configs'."
  (list #'agent-shell-antigravity-make-agent-config
        #'agent-shell-auggie-make-agent-config
        #'agent-shell-anthropic-make-claude-code-config
        #'agent-shell-codebuddy-make-agent-config
        #'agent-shell-cline-make-agent-config
        #'agent-shell-openai-make-codex-config
        #'agent-shell-cursor-make-agent-config
        #'agent-shell-droid-make-agent-config
        #'agent-shell-github-make-copilot-config
        #'agent-shell-google-make-gemini-config
        #'agent-shell-goose-make-agent-config
        #'agent-shell-hermes-make-agent-config
        #'agent-shell-kimi-make-config
        #'agent-shell-kiro-make-config
        #'agent-shell-mistral-make-config
        #'agent-shell-omp-make-agent-config
        #'agent-shell-opencode-make-agent-config
        #'agent-shell-pi-make-agent-config
        #'agent-shell-qwen-make-agent-config
        #'agent-shell-xai-make-grok-config))

(defcustom agent-shell-agent-configs
  (agent-shell-default-agent-config-makers)
  "The known agent configurations.

Either a list of entries, or a function of no arguments returning such a
list.  A function is called on every access, so it can compute the known
agents dynamically, for example keeping only the agents from
`agent-shell-default-agent-config-makers' whose executable is installed:

  (setq agent-shell-agent-configs
        (lambda ()
          (seq-filter
           (lambda (maker)
             (when-let* ((config (funcall maker))
                         (client-maker (map-elt config :client-maker))
                         (client (ignore-errors
                                   (funcall client-maker (current-buffer))))
                         (command (map-elt client :command)))
               (executable-find command)))
           (agent-shell-default-agent-config-makers))))

Each list entry is either a function that returns a configuration alist,
or a configuration alist itself.  Functions are preferred and used by
default: they are called on every access, so agent definitions stay
current across code reloads.  Concrete alists are accepted for
backwards compatibility.

See `agent-shell-*-make-*-config' for details."
  :type '(choice (function :tag "Function returning configs")
                 (repeat :tag "List of configs"
                         (choice function
                                 (alist :key-type symbol :value-type sexp))))
  :group 'agent-shell)

(defun agent-shell--resolved-agent-configs ()
  "Return `agent-shell-agent-configs' with maker entries realized.

`agent-shell-agent-configs' is a list of entries, or a function
returning such a list, in which case it is called first.  Each entry is
a configuration alist or a function (a symbol or lambda) returning one.
Functions are called on every access, so edits to the underlying makers
take effect without rebuilding the list."
  (mapcar (lambda (entry)
            (if (functionp entry)
                (funcall entry)
              entry))
          (if (functionp agent-shell-agent-configs)
              (funcall agent-shell-agent-configs)
            agent-shell-agent-configs)))

(defcustom agent-shell-preferred-agent-config nil
  "Default agent to use for all new shells.

If this is set to an agent identifier (e.g., `claude-code'),
`agent-shell' will unconditionally use that agent and not prompt you
to select one.  A full configuration alist is also accepted for
backwards compatibility.

To keep the picker but have an agent preselected as the default,
wrap the identifier in a cons cell:

  (setq agent-shell-preferred-agent-config \\='(preselect . claude-code))

The equivalent `(auto . claude-code)' spells out the unconditional
behavior explicitly."
  :type '(choice (const :tag "None (prompt each time)" nil)
                 (const :tag "Antigravity" antigravity)
                 (const :tag "Auggie" auggie)
                 (const :tag "Claude Code" claude-code)
                 (const :tag "CodeBuddy" codebuddy)
                 (const :tag "Cline" cline)
                 (const :tag "Codex" codex)
                 (const :tag "Copilot" copilot)
                 (const :tag "Cursor" cursor)
                 (const :tag "Droid" droid)
                 (const :tag "Gemini CLI" gemini-cli)
                 (const :tag "Goose" goose)
                 (const :tag "Grok Build" grok-build)
                 (const :tag "Hermes" hermes)
                 (const :tag "Kimi" kimi)
                 (const :tag "Kiro" kiro)
                 (const :tag "Mistral" le-chat)
                 (const :tag "OpenCode" opencode)
                 (const :tag "Pi" pi)
                 (const :tag "Qwen Code" qwen-code)
                 (symbol :tag "Custom identifier")
                 (cons :tag "Preselect in picker (still prompt)"
                       (const preselect) (symbol :tag "Agent identifier"))
                 (cons :tag "Use unconditionally (explicit)"
                       (const auto) (symbol :tag "Agent identifier"))
                 (alist :tag "Full configuration (legacy)"
                        :key-type symbol :value-type sexp))
  :group 'agent-shell)

(defcustom agent-shell-session-restore-verbosity 'minimal
  "How much prior context to show when restoring a session.

  `minimal': Show only the session title (default).  Uses
             `session/resume' when supported (no message replay),
             so restore is fast and quiet.
  `last':    Use `session/load' and render only the last prompt
             turn (one user prompt and the agent's response).
             When earlier turns exist, a `truncated history'
             separator is shown above the rendered turn.
  `first-last': Use `session/load' and render only the first and
             last prompt turns.  When more than two turns exist,
             a `truncated history' separator is shown between
             them.
  `full':    Use `session/load' and replay the entire conversation.

`last', `first-last', and `full' all require the agent to
advertise `session/load' support.  When unavailable, restore
falls back to `minimal' behavior.

`minimal' uses `session/resume' when available.  When the agent
doesn't support `session/resume' but does support
`session/load', restore falls forward to `first-last' so some
context is shown rather than nothing."
  :type '(choice (const :tag "Title only (minimal)" minimal)
                 (const :tag "Last response (last)" last)
                 (const :tag "First prompt + last response (first-last)" first-last)
                 (const :tag "Full replay" full))
  :group 'agent-shell)

(defvar agent-shell-session-restore-strategy nil
  "Obsolete.  Use `agent-shell-session-restore-verbosity' instead.

Kept bound so init files that `setq' the old name don't get a
`void-variable' error.  `agent-shell--start' detects a non-nil
value and signals a migration error.")

(make-obsolete-variable 'agent-shell-session-restore-strategy
                        'agent-shell-session-restore-verbosity
                        "agent-shell 0.54")

(defun agent-shell--validate-session-strategy (value)
  "Signal an error if VALUE is not a supported `agent-shell-session-strategy'.

`new-deferred' was removed in `agent-shell' 0.54.  Use `new' for a fresh
session without prompting, or `prompt' to choose."
  (unless (memq value '(new latest prompt))
    (user-error
     (concat
      "agent-shell-session-strategy value `%s' is no longer supported.\n"
      "Use `new' for a fresh session, `latest' to load the most recent,\n"
      "or `prompt' to choose.")
     value)))

(defcustom agent-shell-session-strategy 'prompt
  "How to handle sessions when starting a new shell.

Available values:

  `new': Always start a new session.
  `latest': Always load/resume the latest session.
  `prompt': Always prompt to choose a session (or start a new one)."
  :type '(choice (const :tag "Always start new session" new)
                 (const :tag "Load latest session" latest)
                 (const :tag "Prompt for session" prompt))
  :set (lambda (sym value)
         (agent-shell--validate-session-strategy value)
         (set-default sym value))
  :group 'agent-shell)

(defcustom agent-shell-session-choices-function nil
  "Function to transform the choices offered when starting a shell.

When nil, all choices are offered unchanged.

Otherwise called with the list of candidate choices and must return the
choices to actually offer.  Each candidate is a cons cell (LABEL . TOKEN)
where LABEL is the displayed string and TOKEN identifies the choice:

  `:new-shell': Start a new shell.
  `:downloads-shell': Start a new shell in ~/Downloads.
  `:temp-shell': Start a new shell in a temporary directory.
  `:other-shell': Switch to an existing shell buffer.

Resumable session candidates use their session alist as TOKEN.

The function may filter, reorder, or relabel the choices.  Labels are
display-only, so renaming them is fine, but it must not introduce a
choice whose TOKEN was not offered.  Returning a non-list, an empty list,
or a choice with an unknown token signals an error.

For example, to hide the Downloads and temp choices:

  (setq agent-shell-session-choices-function
        (lambda (choices)
          (seq-remove (lambda (choice)
                        (memq (cdr choice) \='(:downloads-shell :temp-shell)))
                      choices)))"
  :type '(choice (const :tag "Offer all choices" nil)
                 (function :tag "Transform function"))
  :group 'agent-shell)

(defun agent-shell--apply-session-choices (choices)
  "Apply `agent-shell-session-choices-function' to CHOICES.

CHOICES is a list of (LABEL . TOKEN) candidate conses.  Returns the
transformed list.  Signals a `user-error' when the configured function
returns something invalid, so a broken configuration is reported against
the setting rather than failing obscurely downstream.

For example, with the default nil value choices pass through unchanged:

  (agent-shell--apply-session-choices \\='((\"New shell\" . :new-shell)))
  => \\='((\"New shell\" . :new-shell))"
  (if agent-shell-session-choices-function
      (let ((result (funcall agent-shell-session-choices-function choices)))
        (unless (listp result)
          (user-error "`agent-shell-session-choices-function' must return a list, got: %S"
                      result))
        (unless result
          (user-error "`agent-shell-session-choices-function' returned no choices"))
        (let ((tokens (mapcar #'cdr choices)))
          (dolist (choice result)
            (unless (member (cdr choice) tokens)
              (user-error "`agent-shell-session-choices-function' returned a choice with an unknown token: %S"
                          (cdr choice)))))
        result)
    choices))

(defvar agent-shell-idle-timeout 30
  "Seconds before an `idle' event is emitted.

When the agent is waiting for user input (after `permission-request'
or `turn-complete'), an `idle' event is emitted after this many
seconds of inactivity.  Activity events (`permission-response',
`tool-call-update', `input-submitted', `clean-up') cancel the timer.

Can be a number (same timeout for all events) or an alist mapping
event symbols to timeouts:

  (setq agent-shell-idle-timeout
        \\='((permission-request . 10)
          (turn-complete . 60)))

Defaults to 30 seconds when nil or when an event has no entry.")

(cl-defun agent-shell-idle-timeout (&key event)
  "Resolve idle timeout in seconds.
When EVENT is non-nil, look it up in variable `agent-shell-idle-timeout'
if it is an alist.  Falls back to 30 seconds."
  (or (if (listp agent-shell-idle-timeout)
          (map-elt agent-shell-idle-timeout event)
        agent-shell-idle-timeout)
      30))

(defcustom agent-shell-outgoing-request-decorator nil
  "Function to decorate outgoing ACP requests before they are sent.

When non-nil, this function is called with each outgoing request alist
and must return the (possibly modified) request.  This is useful for
injecting agent-specific metadata (e.g. system prompt extensions) into
requests.

The function receives the full request alist (with :method, :params, etc.)
and should return the decorated request.  Returning nil is treated as an
error and the original request is sent unchanged.

This is passed through to `acp-make-client' as :outgoing-request-decorator.
The keyword argument to `agent-shell-start' takes precedence over this
variable when both are set."
  :type '(choice (const :tag "None" nil)
                 function)
  :group 'agent-shell)

(defun agent-shell--resolve-config-designator (designator)
  "Resolve DESIGNATOR to a full configuration.

If DESIGNATOR is a symbol, look it up in `agent-shell-agent-configs'.
If it's already an alist (legacy format), return it as-is.
Returns nil if no matching configuration is found."
  (cond
   ((null designator) nil)
   ((symbolp designator)
    (seq-find (lambda (config)
                (eq (map-elt config :identifier) designator))
              (agent-shell--resolved-agent-configs)))
   ((listp designator) designator)))

(defun agent-shell--preferred-config-and-mode ()
  "Return (CONFIG . PRESELECT) for `agent-shell-preferred-agent-config'.

CONFIG is the resolved configuration alist, or nil when none is set.
PRESELECT is non-nil when the picker should still be shown with CONFIG
offered as the default, instead of selecting CONFIG unconditionally."
  (pcase agent-shell-preferred-agent-config
    ('nil nil)
    (`(preselect . ,designator)
     (cons (agent-shell--resolve-config-designator designator) t))
    (`(auto . ,designator)
     (cons (agent-shell--resolve-config-designator designator) nil))
    (designator
     (cons (agent-shell--resolve-config-designator designator) nil))))

(defun agent-shell--resolve-preferred-config ()
  "Resolve `agent-shell-preferred-agent-config' to a full configuration.

Returns the configuration whether it is used automatically or only
preselected in the picker.  Returns nil if none is configured."
  (car (agent-shell--preferred-config-and-mode)))

(defun agent-shell--auto-preferred-config ()
  "Return the preferred configuration only when it should bypass the picker.

Returns nil when no preference is set, or when the preference is
configured to merely preselect (see `agent-shell-preferred-agent-config')."
  (pcase (agent-shell--preferred-config-and-mode)
    (`(,config . nil) config)))

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
           (env . ()))))

Lambdas can be used anywhere in the configuration hierarchy for dynamic
evaluation at session startup time.  This is useful for values that
depend on runtime context like the current working directory
\(`agent-shell-cwd').  Note: only lambdas are evaluated, not named
functions, to avoid accidentally calling external symbols.

For example, using the `claude-code-ide' package (see its documentation
for more details), you can embed a lambda for the URL that registers
the session and returns the appropriate endpoint:

  (setq agent-shell-mcp-servers
        \='(((name . \"emacs\")
           (type . \"http\")
           (headers . ())
           (url . (lambda ()
                    (require \='claude-code-ide-mcp-server)
                    (let* ((project-dir (agent-shell-cwd))
                           (session-id (format \"agent-shell-%s-%s\"
                                         (file-name-nondirectory
                                           (directory-file-name project-dir))
                                         (format-time-string \"%Y%m%d-%H%M%S\"))))
                      (puthash session-id `(:project-dir ,project-dir)
                               claude-code-ide-mcp-server--sessions)
                      (format \"http://localhost:%d/mcp/%s\"
                              (claude-code-ide-mcp-server-ensure-server)
                              session-id)))))))"
  :type '(repeat (choice (alist :key-type symbol :value-type sexp) function))
  :group 'agent-shell)

(cl-defun agent-shell--make-state (&key agent-config buffer client-maker needs-authentication authenticate-request-maker heartbeat outgoing-request-decorator)
  "Construct shell agent state with AGENT-CONFIG and BUFFER.

Shell state is provider-dependent and needs CLIENT-MAKER, NEEDS-AUTHENTICATION,
HEARTBEAT, AUTHENTICATE-REQUEST-MAKER, and optionally
OUTGOING-REQUEST-DECORATOR (passed through to `acp-make-client')."
  (list (cons :agent-config agent-config)
        (cons :buffer buffer)
        (cons :client nil)
        (cons :client-maker client-maker)
        (cons :outgoing-request-decorator outgoing-request-decorator)
        (cons :heartbeat heartbeat)
        (cons :initialized nil)
        (cons :needs-authentication needs-authentication)
        (cons :authenticate-request-maker authenticate-request-maker)
        (cons :authenticated nil)
        (cons :set-model nil)
        (cons :set-session-mode nil)
        (cons :session (list (cons :id nil)
                             (cons :config-options nil)
                             (cons :model-id nil)
                             (cons :models nil)
                             (cons :mode-id nil)
                             (cons :modes nil)
                             (cons :title nil)))
        (cons :config-options nil)
        (cons :last-entry-type nil)
        (cons :last-agent-message-id nil)
        (cons :chunked-group-count 0)
        (cons :activity-group-count 0)
        (cons :activity-thoughts nil)
        (cons :expanded-activity-group nil)
        (cons :request-count 0)
        (cons :last-activity-time nil)
        (cons :tool-calls nil)
        (cons :available-commands nil)
        (cons :available-modes nil)
        (cons :supports-session-list nil)
        (cons :supports-session-load nil)
        (cons :supports-session-resume nil)
        (cons :supports-session-fork nil)
        (cons :supports-steering nil)
        (cons :resume-session-id nil)
        (cons :fork-session-id nil)
        (cons :pending-restore nil)
        (cons :prompt-capabilities nil)
        (cons :event-subscriptions nil)
        (cons :idle-timer nil)
        (cons :sleep-token nil)
        (cons :active-requests nil)
        (cons :pending-prompts nil)
        (cons :usage (list (cons :total-tokens 0)
                           (cons :input-tokens 0)
                           (cons :output-tokens 0)
                           (cons :thought-tokens 0)
                           (cons :cached-read-tokens 0)
                           (cons :cached-write-tokens 0)
                           (cons :context-used 0)
                           (cons :context-size 0)
                           (cons :cost-amount 0.0)
                           (cons :cost-currency nil)))))

(defvar-local agent-shell--state
    (agent-shell--make-state))

(defvar-local agent-shell--transcript-file nil
  "Path to the shell's transcript file.")

(defvar-local agent-shell--pending-directory-cleanup nil
  "Directory to delete when the shell is cleaned up.

Set by shells that created a directory of their own and must dispose of
it, for example the \"/tmp/temp-a1B2c3\" that `agent-shell-new-temp-shell'
makes.  Nil in shells whose working directory belongs to the user.

Recorded on creation rather than read from `default-directory' at cleanup
time, which project detection can resolve to a directory we don't own
\(e.g. \"/tmp\" when \"/tmp/.git\" exists).")

(defvar agent-shell--shell-maker-config nil)

;;;###autoload
(defun agent-shell (&optional arg)
  "Start or reuse an existing agent shell.

`agent-shell' carries some DWIM (do what I mean) behaviour.

If in a project without a shell, offer to create one.

If already in a shell, invoke `agent-shell-toggle'.

If a region is active or point is on relevant context (ie.
`dired' files or image buffers), carry them over to the
shell input.

See `agent-shell-context-sources' on how to control DWIM
behaviour.

With \\[universal-argument] prefix ARG, force start a new shell.

With \\[universal-argument] \\[universal-argument] prefix ARG, prompt to pick an existing shell."
  (interactive "P")
  (cond
   ((equal arg '(16))
    (agent-shell--dwim :switch-to-shell t))
   ((equal arg '(4))
    (agent-shell--dwim :new-shell t))
   (t
    (agent-shell--dwim))))

(defun agent-shell-submit ()
  "Submit the current input to the agent.

The prompt is shown early (before the ACP session is ready) so users
can type while the agent initializes.  Gate the actual send on the
session being ready: when it is not, error with `Busy, please wait'
before the input is committed, so the typed text stays editable
instead of being echoed into the transcript and rejected later.

This owns the `agent-shell-submit' name because shell-maker's
per-start aliasing is disabled (see the `:alias-commands nil' call in
`agent-shell--start')."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent shell"))
  (unless (or (map-nested-elt agent-shell--state '(:session :id))
              (eq agent-shell-session-strategy 'new-deferred))
    (user-error "Busy, please wait"))
  (shell-maker-submit))

(defun agent-shell--display-and-insert-context (shell-buffer text)
  "Display SHELL-BUFFER and insert TEXT into it."
  (if (and (eq (buffer-local-value 'agent-shell-session-strategy shell-buffer) 'prompt)
           (not (map-nested-elt (buffer-local-value 'agent-shell--state shell-buffer)
                                '(:session :id))))
      (progn
        (when text
          (agent-shell--insert-to-shell-buffer :text text
                                               :shell-buffer shell-buffer
                                               :no-focus t))
        (agent-shell-subscribe-to
         :shell-buffer shell-buffer
         :event 'session-selected
         :on-event (lambda (_event)
                     (agent-shell--display-buffer shell-buffer))))
    (agent-shell--display-buffer shell-buffer)
    (when text
      (agent-shell--insert-to-shell-buffer :text text
                                           :shell-buffer shell-buffer))))

(cl-defun agent-shell--display-viewport-when-ready (&key shell-buffer append override edit)
  "Show the viewport for SHELL-BUFFER, deferring until its session is ready.

When SHELL-BUFFER uses the `prompt' session strategy and has no session id
yet, wait for the `session-selected' event before showing the viewport.
Otherwise the session picker `completing-read' races a visible compose
buffer, which is confusing.  APPEND, OVERRIDE and EDIT are forwarded to
`agent-shell-viewport--show-buffer'."
  (if (and (eq (buffer-local-value 'agent-shell-session-strategy shell-buffer) 'prompt)
           (not (map-nested-elt (buffer-local-value 'agent-shell--state shell-buffer)
                                '(:session :id))))
      (agent-shell-subscribe-to
       :shell-buffer shell-buffer
       :event 'session-selected
       :on-event (lambda (_event)
                   (agent-shell-viewport--show-buffer
                    :append append :override override :edit edit :shell-buffer shell-buffer)))
    (agent-shell-viewport--show-buffer
     :append append :override override :edit edit :shell-buffer shell-buffer)))

(cl-defun agent-shell--dwim (&key config new-shell switch-to-shell)
  "Start or reuse an agent shell with DWIM behavior.

CONFIG is the agent configuration to use.
NEW-SHELL when non-nil forces starting a new shell.
SWITCH-TO-SHELL when non-nil prompts to pick an existing shell.

NEW-SHELL and SWITCH-TO-SHELL are mutually exclusive.

This function respects `agent-shell-prefer-viewport-interaction' and
handles viewport mode detection, existing shell reuse, and project context."
  (when (and new-shell switch-to-shell)
    (error ":new-shell and :switch-to-shell are mutually exclusive"))
  (if agent-shell-prefer-viewport-interaction
      (if (and (not new-shell)
               (or (derived-mode-p 'agent-shell-viewport-view-mode)
                   (derived-mode-p 'agent-shell-viewport-edit-mode)))
          (agent-shell-toggle)
        (let* ((shell-buffer
                (cond (switch-to-shell
                       (agent-shell--read-shell-buffer :prompt "Switch to shell: "))
                      (new-shell
                       (agent-shell--start :config (or config
                                                       (agent-shell--auto-preferred-config)
                                                       (agent-shell-select-config
                                                        :prompt "Start new agent: ")
                                                       (error "No agent config found"))
                                           :no-focus t
                                           :new-session t))
                      (t
                       (agent-shell--shell-buffer))))
               (text (agent-shell--context :shell-buffer shell-buffer)))
          (agent-shell--display-viewport-when-ready
           :shell-buffer shell-buffer
           :append text)))
    (cond (switch-to-shell
           (let* ((shell-buffer (agent-shell--read-shell-buffer
                                 :prompt "Switch to shell: "))
                  (text (agent-shell--context :shell-buffer shell-buffer)))
             (agent-shell--display-buffer shell-buffer)
             (when text
               (agent-shell--insert-to-shell-buffer :text text
                                                    :shell-buffer shell-buffer))))
          (new-shell
           (let* ((shell-buffer (agent-shell--start
                                 :config (or config
                                             (agent-shell--auto-preferred-config)
                                             (agent-shell-select-config
                                              :prompt "Start new agent: ")
                                             (error "No agent config found"))
                                 :no-focus t
                                 :new-session t))
                  (text (agent-shell--context :shell-buffer shell-buffer)))
             (agent-shell--display-and-insert-context shell-buffer text)))
          (t
           (if (derived-mode-p 'agent-shell-mode)
               (let* ((shell-buffer (agent-shell--shell-buffer :no-create t))
                      (text (agent-shell--context :shell-buffer shell-buffer)))
                 (agent-shell-toggle)
                 (when text
                   (agent-shell--insert-to-shell-buffer :text text
                                                        :shell-buffer shell-buffer)))
             (let* ((shell-buffer (agent-shell--shell-buffer))
                    (text (agent-shell--context :shell-buffer shell-buffer)))
               (agent-shell--display-and-insert-context shell-buffer text)))))))

;;;###autoload
(defun agent-shell-toggle ()
  "Toggle agent shell display."
  (interactive)
  (let ((shell-buffer (if agent-shell-prefer-viewport-interaction
                          (agent-shell-viewport--buffer)
                        (or (agent-shell--current-shell)
                            (seq-first (agent-shell-project-buffers))
                            (seq-first (agent-shell-buffers))))))
    (unless shell-buffer
      (user-error "No agent shell buffers available for current project"))
    (if-let* ((window (get-buffer-window shell-buffer)))
        (quit-restore-window window 'bury)
      (agent-shell--display-buffer shell-buffer))))

;;;###autoload
(defun agent-shell-new-shell ()
  "Start a new agent shell.

Always prompts for agent selection, even if existing shells are available."
  (interactive)
  (agent-shell '(4)))

;;;###autoload
(cl-defun agent-shell-new-temp-shell (&key config no-display)
  "Start a new agent shell in a temporary directory.

The directory is trashed when the shell buffer is killed.
When CONFIG is non-nil, use it instead of prompting for an agent.
When NO-DISPLAY is non-nil, don't display the shell buffer."
  (interactive)
  (let* ((location (make-temp-file "temp-" t))
         (shell-buffer (agent-shell--new-shell :location location
                                               :config config
                                               :no-display no-display)))
    (with-current-buffer shell-buffer
      (setq-local agent-shell--pending-directory-cleanup location))
    shell-buffer))

;;;###autoload
(cl-defun agent-shell-new-downloads-shell (&key config no-display)
  "Start a new agent shell in ~/Downloads.

When CONFIG is non-nil, use it instead of prompting for an agent.
When NO-DISPLAY is non-nil, don't display the shell buffer."
  (interactive)
  (agent-shell--new-shell :location (expand-file-name "~/Downloads")
                          :config config
                          :no-display no-display))

(cl-defun agent-shell--new-shell (&key location config no-display)
  "Start a new agent shell at LOCATION.

LOCATION is a directory path to use as the shell's working directory.
When CONFIG is non-nil, use it instead of resolving or prompting.
When NO-DISPLAY is non-nil, don't display the shell buffer."
  (let* ((default-directory location)
         (shell-buffer (agent-shell--start
                        :config (or config
                                    (agent-shell--auto-preferred-config)
                                    (agent-shell-select-config
                                     :prompt "Start new agent: ")
                                    (error "No agent config found"))
                        :session-strategy 'new
                        :new-session t
                        :no-focus t)))
    (unless no-display
      (if agent-shell-prefer-viewport-interaction
          (agent-shell-viewport--show-buffer
           :shell-buffer shell-buffer)
        (agent-shell--display-buffer shell-buffer)))
    shell-buffer))

;;;###autoload
(cl-defun agent-shell-restart (&key session-id)
  "Clear conversation by restarting the agent shell in the same project.

Kills the current shell buffer (shutting down the ACP client) and
starts a fresh shell with the same agent configuration.

When SESSION-ID is provided, resume that session instead of starting new.

Works from both shell and viewport buffers."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (let* ((from-viewport (or (derived-mode-p 'agent-shell-viewport-view-mode)
                            (derived-mode-p 'agent-shell-viewport-edit-mode)))
         (shell-buffer (or (agent-shell--current-shell)
                           (user-error "Not in a shell or viewport buffer")))
         (shell-buffer-name (buffer-name shell-buffer))
         (strategy (if (eq (buffer-local-value 'agent-shell-session-strategy shell-buffer)
                           'new-deferred)
                       'new-deferred
                     'new))
         (config (map-elt (buffer-local-value 'agent-shell--state shell-buffer)
                          :agent-config))
         (shell-dir (buffer-local-value 'default-directory shell-buffer))
         (pending-directory-cleanup
          (buffer-local-value 'agent-shell--pending-directory-cleanup shell-buffer))
         ;; Remember where the shell is currently displayed
         (windows (get-buffer-window-list shell-buffer nil t)))
    (with-current-buffer shell-buffer
      (when (and (agent-shell--active-requests-p (agent-shell--state))
                 (not (y-or-n-p "Agent is busy.  Restart anyway?")))
        (user-error "Cancelled"))
      ;; The restarted shell inherits the directory, so unschedule it here to
      ;; keep this buffer's cleanup from deleting it.
      (setq-local agent-shell--pending-directory-cleanup nil))
    (kill-buffer shell-buffer)
    (let* ((default-directory shell-dir)
           (new-shell-buffer (agent-shell--start
                              :config config
                              :session-strategy strategy
                              :session-id session-id
                              :new-session t
                              :no-focus t)))
      (shell-maker-set-buffer-name new-shell-buffer shell-buffer-name)
      (when pending-directory-cleanup
        (with-current-buffer new-shell-buffer
          (setq-local agent-shell--pending-directory-cleanup
                      pending-directory-cleanup)))
      (if (or from-viewport agent-shell-prefer-viewport-interaction)
          (agent-shell-viewport--show-buffer
           :shell-buffer new-shell-buffer)
        ;; Reuse the original window(s) when still live
        (if-let* ((live-windows (seq-filter #'window-live-p windows)))
            (progn
              (dolist (window live-windows)
                (set-window-buffer window new-shell-buffer))
              (select-window (car live-windows)))
          (agent-shell--display-buffer new-shell-buffer))))))

;;;###autoload
(defun agent-shell-reload ()
  "Reload the current session by restarting with the same session ID.

Works from both shell and viewport buffers."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (let* ((shell-buffer (or (agent-shell--current-shell)
                           (user-error "Not in a shell or viewport buffer")))
         (session-id (map-nested-elt (buffer-local-value 'agent-shell--state shell-buffer)
                                     '(:session :id))))
    (unless session-id
      (user-error "No active session to reload"))
    (agent-shell-restart :session-id session-id)))

;;;###autoload
(defun agent-shell-fork ()
  "Fork the current session into a new shell.

Creates a new shell that forks the current session's conversation,
leaving the original shell intact.  The new shell shares conversation
history with the original but diverges from this point forward.

Works from both shell and viewport buffers."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (let* ((from-viewport (or (derived-mode-p 'agent-shell-viewport-view-mode)
                            (derived-mode-p 'agent-shell-viewport-edit-mode)))
         (shell-buffer (or (agent-shell--current-shell)
                           (user-error "Not in a shell or viewport buffer")))
         (session-id (map-nested-elt (buffer-local-value 'agent-shell--state shell-buffer)
                                     '(:session :id)))
         (supports-fork (map-elt (buffer-local-value 'agent-shell--state shell-buffer)
                                 :supports-session-fork))
         (config (map-elt (buffer-local-value 'agent-shell--state shell-buffer)
                          :agent-config)))
    (unless session-id
      (user-error "No active session to fork"))
    (unless supports-fork
      (user-error "Agent does not support session forking"))
    (let ((new-shell-buffer (agent-shell--start
                             :config config
                             :session-strategy 'new
                             :fork-session-id session-id
                             :new-session t
                             :no-focus t)))
      (if (or from-viewport agent-shell-prefer-viewport-interaction)
          (agent-shell-viewport--show-buffer
           :shell-buffer new-shell-buffer)
        (agent-shell--display-buffer new-shell-buffer)))))

;;;###autoload
(defun agent-shell-resume-session (session-id)
  "Resume an existing agent session by SESSION-ID.

Prompts for agent selection and starts a new shell that resumes
the session identified by SESSION-ID."
  (interactive "sSession ID: ")
  (when (string-empty-p (string-trim session-id))
    (user-error "Session ID cannot be empty"))
  (agent-shell--start :config (or (agent-shell--auto-preferred-config)
                                  (agent-shell-select-config
                                   :prompt "Resume with agent: ")
                                  (error "No agent config found"))
                      :session-id session-id
                      :new-session t))

;;;###autoload
(defun agent-shell-prompt-compose ()
  "Compose an `agent-shell' prompt in a dedicated buffer.

If currently visiting an `agent-shell', transfer latest input."
  (interactive)
  (let ((shell-buffer (agent-shell--shell-buffer)))
    ;; Without viewport interaction, compose is a fire-and-forget pop-up:
    ;; sending should dismiss back to the previous layout rather than kill
    ;; the compose buffer and pull focus into the shell.  Scope this to the
    ;; resolved viewport buffer so the global default is left untouched.
    (unless agent-shell-prefer-viewport-interaction
      (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                                    :shell-buffer shell-buffer)))
        (with-current-buffer viewport-buffer
          (setq-local agent-shell-viewport-dismiss-on-send t))))
    (if-let* (((derived-mode-p 'agent-shell-mode))
              ((shell-maker-point-at-last-prompt-p))
              (input (agent-shell--input)))
        (progn
          ;; Clear shell prompt as it's now
          ;; transferred to the compose buffer.
          ;; Use delete-region to point-max rather than comint-kill-input
          ;; which only deletes to point.  Text appended after point
          ;; (e.g. attachments inserted via save-excursion) would otherwise
          ;; survive and get duplicated on viewport submission.
          (delete-region
           (or (marker-position comint-accum-marker)
               (process-mark (get-buffer-process (current-buffer))))
           (point-max))
          ;; Already in a shell, so its session is established.  Show now.
          (agent-shell-viewport--show-buffer :override input
                                             :edit t
                                             :shell-buffer shell-buffer))
      ;; May create a fresh shell.  Defer the viewport so the session picker
      ;; isn't racing a visible compose buffer.
      (agent-shell--display-viewport-when-ready
       :shell-buffer shell-buffer
       :edit t))))

(cl-defun agent-shell-start (&key config session-id outgoing-request-decorator)
  "Programmatically start shell with CONFIG.

See `agent-shell-make-agent-config' for config format.

SESSION-ID resumes an existing session by its id string.
OUTGOING-REQUEST-DECORATOR is an optional function passed through to
`acp-make-client'.  See its docstring for details."
  (agent-shell--start :config config
                      :no-focus nil
                      :new-session t
                      :session-id session-id
                      :outgoing-request-decorator outgoing-request-decorator))

(cl-defun agent-shell--config-icon (&key config)
  "Create icon string for CONFIG if available and icons are enabled.
Returns nil if no icon should be displayed."
  (and-let* ((icon-filename (if (map-elt config :icon-name)
                                (agent-shell--fetch-agent-icon
                                 (map-elt config :icon-name))
                              (agent-shell--make-agent-fallback-icon
                               (map-elt config :buffer-name) 100)))
             ;; Skip the icon when Emacs can't display this image type
             ;; (e.g. SVG fallback icons on a build without SVG support).
             ((image-supported-file-p icon-filename))
             (image (create-image icon-filename nil nil
                                  :ascent 'center
                                  :height (frame-char-height))))
    (with-temp-buffer
      (insert-image image)
      (buffer-string))))

(cl-defun agent-shell-select-config (&key prompt)
  "Display PROMPT to select an agent config from `agent-shell-agent-configs'.

When `agent-shell-preferred-agent-config' is set, its configuration is
listed first and offered as the default selection."
  (let* ((configs (agent-shell--resolved-agent-configs))
         (preferred (agent-shell--resolve-preferred-config))
         (configs (if preferred
                      (cons preferred
                            (seq-remove (lambda (config)
                                          (eq (map-elt config :identifier)
                                              (map-elt preferred :identifier)))
                                        configs))
                    configs))
         (choices (mapcar
                   (lambda (config)
                     (cons (propertize
                            (or (map-elt config :mode-line-name)
                                (map-elt config :buffer-name)
                                "Unknown Agent")
                            'agent-shell--icon
                            (when agent-shell-show-config-icons
                              (agent-shell--config-icon :config config)))
                           config))
                   configs))
         (default-name (when preferred (caar choices)))
         (completion-extra-properties '(:category agent-shell-config))
         (completion-styles (cons 'substring completion-styles))
         (selected-name (completing-read
                         (if default-name
                             (format-prompt
                              (string-remove-suffix ": " (or prompt "Select agent"))
                              default-name)
                           (or prompt "Select agent: "))
                         (lambda (string pred action)
                           (if (eq action 'metadata)
                               '(metadata
                                 (category . agent-shell-config)
                                 (display-sort-function . identity)
                                 (affixation-function
                                  . agent-shell--icon-affixation))
                             (complete-with-action action (mapcar #'car choices)
                                                   string pred)))
                         nil t nil nil default-name)))
    (map-elt choices selected-name)))

(defun agent-shell-buffers ()
  "Return all shell buffers ordered by recent access.
Includes shells accessed via viewport buffers, preserving visited order."
  (let (shell-buffers seen)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when-let* ((shell-buffer
                     (cond ((derived-mode-p 'agent-shell-mode)
                            buffer)
                           ((or (derived-mode-p 'agent-shell-viewport-view-mode)
                                (derived-mode-p 'agent-shell-viewport-edit-mode))
                            (agent-shell-viewport--shell-buffer buffer))))
                    ((buffer-local-value 'shell-maker--config shell-buffer)))
          (unless (memq shell-buffer seen)
            (push shell-buffer seen)
            (push shell-buffer shell-buffers)))))
    (nreverse shell-buffers)))

(defun agent-shell-other-buffer ()
  "Switch to other associated buffer (viewport vs shell).

When switching between a viewport view and its shell, point is carried to
the equivalent location: the same interaction and the same offset within
its prompt or response.  From the shell's live prompt, switch to the
viewport's compose buffer instead (carrying an unsent draft, or empty)."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (cond
   ;; Viewport view -> shell.  The shell buffer's point already tracks the
   ;; shown interaction; capture it and point's response offset before
   ;; switching, since switching restores the shell window's own point.
   ((derived-mode-p 'agent-shell-viewport-view-mode)
    (let* ((shell-buffer (or (agent-shell--shell-buffer
                              :viewport-buffer (current-buffer)
                              :no-create t)
                             (user-error "No shell available")))
           (location (agent-shell--point-location
                      (agent-shell-viewport--prompt-start)
                      (agent-shell-viewport--response-start)))
           (pos (with-current-buffer shell-buffer (point))))
      (switch-to-buffer shell-buffer)
      (goto-char pos)
      (when-let* ((location)
                  (start (if (eq (map-elt location :region) :response)
                             (agent-shell--shell-response-start)
                           (shell-maker--prompt-end-position))))
        (goto-char (min (+ start (map-elt location :offset)) (point-max))))))
   ;; Viewport edit -> shell.  No corresponding interaction, just switch.
   ((derived-mode-p 'agent-shell-viewport-edit-mode)
    (switch-to-buffer (or (agent-shell--shell-buffer
                           :viewport-buffer (current-buffer)
                           :no-create t)
                          (user-error "No shell available"))))
   ;; Shell -> viewport.  A nil response start means point is on the live
   ;; prompt (no completed interaction under it), so compose: carry an unsent
   ;; draft into the edit buffer, or start an empty one.  Otherwise show the
   ;; interaction under point, mapping the response offset 1:1 into the
   ;; viewport.
   ((derived-mode-p 'agent-shell-mode)
    (if-let* ((response-start (agent-shell--shell-response-start)))
        (let ((location (agent-shell--point-location
                         (shell-maker--prompt-end-position)
                         response-start))
              (viewport-buffer (agent-shell-viewport--buffer :shell-buffer (current-buffer))))
          (with-current-buffer viewport-buffer
            ;; Show the interaction unless an in-progress compose draft would
            ;; be lost; an empty edit buffer is switched to view.
            (unless (and (derived-mode-p 'agent-shell-viewport-edit-mode)
                         (> (buffer-size) 0))
              (unless (derived-mode-p 'agent-shell-viewport-view-mode)
                (agent-shell-viewport-view-mode))
              (agent-shell-viewport-refresh)))
          (switch-to-buffer viewport-buffer)
          (when (derived-mode-p 'agent-shell-viewport-view-mode)
            (goto-char (or (when-let* ((location)
                                       (start (if (eq (map-elt location :region) :response)
                                                  (agent-shell-viewport--response-start)
                                                (agent-shell-viewport--prompt-start))))
                             (min (+ start (map-elt location :offset)) (point-max)))
                           (point-min)))))
      (agent-shell-prompt-compose)))
   (t
    (user-error "Not in an agent-shell buffer"))))

(cl-defun agent-shell--read-shell-buffer (&key prompt buffers force-short-names)
  "Read an `agent-shell-mode' buffer via `completing-read'.
Each candidate shows the agent icon, buffer name, status, and
session title in aligned columns.

The agent-name prefix each buffer name carries (for example
\"Claude Agent @ \") is stripped from the displayed names to reduce
clutter when every candidate belongs to the same agent type, or
when FORCE-SHORT-NAMES is non-nil (which strips each candidate's own prefix
regardless of whether the candidates share an agent type).

PROMPT is the prompt string (defaults to \"Agent shell buffer: \").
BUFFERS is the list of buffers to choose from, defaulting to
`agent-shell-buffers'.

Returns the chosen shell buffer.  Signals a `user-error' when no
buffers are available or nothing was selected."
  (let* ((entries (mapcar
                   (lambda (buffer)
                     (with-current-buffer buffer
                       (list (cons :buffer buffer)
                             (cons :icon (when agent-shell-show-config-icons
                                           (agent-shell--config-icon
                                            :config (map-elt agent-shell--state
                                                             :agent-config))))
                             (cons :name (buffer-name buffer))
                             (cons :agent-identifier
                                   (map-nested-elt agent-shell--state
                                                   '(:agent-config :identifier)))
                             ;; The config's `:buffer-name' is the agent's
                             ;; display name (e.g. "Claude"), the same value
                             ;; `agent-shell--format-buffer-name' builds from.
                             (cons :buffer-name-prefix
                                   (agent-shell--buffer-name-prefix
                                    (map-nested-elt agent-shell--state
                                                    '(:agent-config :buffer-name))))
                             (cons :status (symbol-name (agent-shell-status)))
                             (cons :title (let ((title (string-trim
                                                        (car (split-string
                                                              (or (map-nested-elt agent-shell--state
                                                                                  '(:session :title))
                                                                  "")
                                                              "\n")))))
                                            (if (> (length title) 50)
                                                (concat (substring title 0 47) "...")
                                              title))))))
                   (or buffers
                       (agent-shell-buffers)
                       (user-error "No agent-shell buffers"))))
         ;; Strip each candidate's agent-name prefix (e.g. "Claude Agent @ ")
         ;; from the displayed buffer name when SHORTENED is requested or every
         ;; candidate is the same agent type, so only the distinguishing part
         ;; remains.
         (agent-identifiers (mapcar (lambda (e)
                                      (map-elt e :agent-identifier))
                                    entries))
         (homogeneous (seq-every-p (lambda (id)
                                     (eq id (seq-first agent-identifiers)))
                                   agent-identifiers))
         (display-name (lambda (e)
                         (if (and (or force-short-names homogeneous)
                                  (map-elt e :buffer-name-prefix))
                             (string-remove-prefix (map-elt e :buffer-name-prefix)
                                                   (map-elt e :name))
                           (map-elt e :name))))
         (name-width (apply #'max (mapcar (lambda (e)
                                            (length (funcall display-name e)))
                                          entries)))
         (status-width (apply #'max (mapcar (lambda (e)
                                              (length (map-elt e :status)))
                                            entries)))
         ;; Stash the icon as a text property so it can be supplied as an
         ;; affixation prefix later; this keeps the icon out of the candidate
         ;; text so `completing-read' matching doesn't see the leading space.
         (choices (mapcar
                   (lambda (e)
                     (cons (propertize
                            (concat
                             (string-pad (propertize (funcall display-name e)
                                                     'face 'agent-shell-buffer-name)
                                         (1+ name-width))
                             (string-pad (propertize (map-elt e :status)
                                                     'face (pcase (map-elt e :status)
                                                             ("busy" 'agent-shell-warning)
                                                             ("blocked" 'agent-shell-error)
                                                             (_ 'agent-shell-success)))
                                         (1+ status-width))
                             (propertize (map-elt e :title)
                                         'face 'agent-shell-session-title))
                            'agent-shell--icon (map-elt e :icon))
                           (map-elt e :buffer)))
                   entries))
         ;; Bind `this-command' so completion frameworks don't append
         ;; "(nil)" to each candidate (see `agent-shell--prompt-select-session').
         (this-command 'agent-shell--read-shell-buffer)
         (completion-styles (cons 'substring completion-styles))
         (selection (completing-read
                     (or prompt "Agent shell buffer: ")
                     (lambda (string pred action)
                       (if (eq action 'metadata)
                           '(metadata
                             (display-sort-function . identity)
                             (affixation-function
                              . agent-shell--icon-affixation))
                         (complete-with-action action (mapcar #'car choices)
                                               string pred)))
                     nil t)))
    (or (map-elt choices selection)
        (user-error "Nothing selected"))))

(defun agent-shell--icon-affixation (candidates)
  "Return CANDIDATES annotated with the agent icon as a display-only prefix.
Reads the icon from the `agent-shell--icon' text property so leading
icon characters do not participate in `completing-read' matching."
  (mapcar (lambda (candidate)
            (let ((icon (get-text-property 0 'agent-shell--icon candidate)))
              (list candidate
                    (if icon (concat icon " ") "")
                    "")))
          candidates))

(defun agent-shell-switch-buffer ()
  "Switch to another `agent-shell-mode' buffer via `completing-read'.
When `agent-shell-prefer-viewport-interaction' is non-nil and an
associated viewport buffer exists, switch to that instead."
  (interactive)
  (let ((shell-buffer (agent-shell--read-shell-buffer
                       :prompt "Switch to agent-shell buffer: ")))
    (switch-to-buffer (or (when agent-shell-prefer-viewport-interaction
                            (agent-shell-viewport--buffer
                             :shell-buffer shell-buffer
                             :existing-only t))
                          shell-buffer))))

(defun agent-shell-version ()
  "Show `agent-shell' mode version."
  (interactive)
  (message "agent-shell v%s" agent-shell--version))

(defun agent-shell-copy-session-id ()
  "Copy the current session ID to the kill ring."
  (declare (modes agent-shell-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in a shell"))
  (if-let* ((session-id (map-nested-elt (agent-shell--state) '(:session :id))))
      (progn
        (kill-new session-id)
        (message "Copied session ID: %s" session-id))
    (user-error "No active session")))

(defun agent-shell-copy-as-markdown (beg end)
  "Copy the region between BEG and END to the kill ring as markdown.

A plain copy yields exactly the visible (rendered) text, with no
markup, handy for pasting a command straight into a terminal.
This command instead reconstructs the agent's original markdown for
every rendered construct fully contained in the region: `**bold**'
\(including nested `**bold _and italic_**'), `## headings', links,
fenced code blocks with their ```language fences, and tables.  A
construct only partially selected (for example a single line of a
code block) is copied verbatim as shown.

Interactively, operates on the active region."
  (interactive "r")
  (kill-new (agent-shell-markdown-reconstruct beg end))
  (setq deactivate-mark t)
  (message "Copied as markdown"))

(defun agent-shell-copy-link-url-at-point (&optional pos)
  "Copy the rendered Markdown link URL at POS (or point) to the kill ring.

Rendered links show only their title; this recovers the target URL
from the `agent-shell-markdown-url' text property the renderer leaves
behind.  Signals a `user-error' when point is not on a link."
  (interactive)
  (if-let* ((url (agent-shell-markdown-link-url-at-point pos)))
      (progn
        (kill-new url)
        (message "Copied URL: %s" url))
    (user-error "No link at point")))

(defun agent-shell-copy-source-block-at-point (&optional pos)
  "Copy the rendered fenced code block body at POS (or point) to the kill ring.

Copies the code without its fences or language label.  Signals a
`user-error' when point is not on a rendered code block body."
  (interactive)
  (if-let* ((body (agent-shell-markdown-source-block-at-point pos)))
      (progn
        (kill-new body)
        (message "Copied code block"))
    (user-error "No code block at point")))

(cl-defun agent-shell--permission-pending-p (&key shell-buffer tool-call-id)
  "Return non-nil if a permission request is pending.
When SHELL-BUFFER is non-nil, check that buffer instead of the current one.
When TOOL-CALL-ID is non-nil, check only that specific tool call.
When nil, check if any permission request is pending."
  (with-current-buffer (or shell-buffer (current-buffer))
    (if tool-call-id
        (map-nested-elt (map-elt (agent-shell--state) :tool-calls)
                        (list tool-call-id :permission-request-id))
      (seq-some (lambda (entry)
                  (map-elt (cdr entry) :permission-request-id))
                (map-elt (agent-shell--state) :tool-calls)))))

(cl-defun agent-shell-status (&key shell-buffer)
  "Return the status of the agent shell as a symbol.
When SHELL-BUFFER is non-nil, check that buffer instead of the current one.

Returns one of:
  `busy'    - Agent is actively processing.
  `blocked' - Agent is waiting for a permission response.
  `ready'   - Agent is idle and ready for input."
  (with-current-buffer (or shell-buffer (current-buffer))
    (cond
     ((and (shell-maker-busy)
           (agent-shell--permission-pending-p)) 'blocked)
     (t
      (if (shell-maker-busy)
          'busy
        'ready)))))

(cl-defun agent-shell-steering-supported-p (&key shell-buffer)
  "Return non-nil when the agent accepts a prompt steered into a running turn.
When SHELL-BUFFER is non-nil, check that buffer instead of the current one.

Steering is an ACP extension rather than part of the spec, so the agent
advertises it in the `initialize' response's top-level `_meta' as
`steering.supported'.  See `agent-shell-experimental--send-steering'."
  (with-current-buffer (or shell-buffer (current-buffer))
    (map-elt agent-shell--state :supports-steering)))

(defun agent-shell-interrupt (&optional force)
  "Interrupt in-progress request and reject all pending permissions.
When FORCE is non-nil, skip confirmation prompt.
See also `agent-shell-confirm-interrupt'."
  (declare (modes agent-shell-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (cond ((map-nested-elt (agent-shell--state) '(:session :id))
         (when (or force (agent-shell-interrupt-confirmed-p))
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
        (t
         (agent-shell--shutdown)
         (call-interactively #'shell-maker-interrupt))))

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
      :shell-buffer (map-elt shell :buffer)))))

(defun agent-shell--filter-buffer-substring (start end &optional delete)
  "Return visible text between START and END, stripping hidden markup.
If DELETE is non-nil, delete the text between START and END.

START and END may be given in either order: like the stock
`buffer-substring', a reversed range (START > END, e.g. a
right-to-left mouse selection or a kill where mark > point) is
normalized.  Without this, the loop below would never run and the
function would return the empty string, silently breaking mouse
copy depending on selection direction."
  (let* ((beg (min start end))
         (fin (max start end))
         (text "")
         (pos beg))
    (while (< pos fin)
      (let ((next (next-overlay-change pos))
            (exclude (seq-find (lambda (ov)
                                 (memq (overlay-get ov 'markdown-overlays-markup-type)
                                       '(fence language inline-code
                                               bold italic strikethrough header)))
                               (overlays-at pos))))
        (unless exclude
          (setq text (concat text (buffer-substring pos (min next fin)))))
        (setq pos (max next (1+ pos)))))
    (when delete
      (delete-region beg fin))
    (remove-text-properties 0 (length text)
                            '(line-prefix nil wrap-prefix nil)
                            text)
    text))

(defvar-keymap agent-shell-mode-map
  :parent shell-maker-mode-map
  :doc "Keymap for `agent-shell-mode'."
  "TAB" #'agent-shell-next-item
  "<backtab>" #'agent-shell-previous-item
  "n" #'agent-shell-next-item
  "p" #'agent-shell-previous-item
  "C-M-u" #'agent-shell-backward-up-item
  "r" #'agent-shell-quote-region
  "+" #'agent-shell-image-scale-increase
  "-" #'agent-shell-image-scale-decrease
  "0" #'agent-shell-image-scale-reset
  "C-<tab>" #'agent-shell-cycle-session-mode
  "C-c C-c" #'agent-shell-interrupt
  "C-c C-m" #'agent-shell-set-session-mode
  "C-c C-v" #'agent-shell-set-session-model
  "C-c C-t" #'agent-shell-set-session-thought-level
  "C-c C-o" #'agent-shell-other-buffer
  "C-c C-s" #'agent-shell-set-session-config-option
  "<remap> <yank>" #'agent-shell-yank-dwim
  "<remap> <comint-send-input>" #'agent-shell-submit)

(shell-maker-define-major-mode (agent-shell--make-shell-maker-config) agent-shell-mode-map)

(cl-defun agent-shell--handle (&key command shell-buffer)
  "Handle SHELL-BUFFER COMMAND (and lazy initialize the ACP stack).

SHELL-BUFFER is the shell buffer.

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
  (with-current-buffer shell-buffer
    (unless (derived-mode-p 'agent-shell-mode)
      (error "Not in a shell"))
    (when (and command
               (not (eq agent-shell-session-strategy 'new-deferred))
               (not (map-nested-elt (agent-shell--state) '(:session :id))))
      (user-error "Session not ready... please wait"))
    (map-put! (agent-shell--state) :request-count
              ;; TODO: Make public in shell-maker.
              (shell-maker--current-request-id))
    (map-put! (agent-shell--state) :last-activity-time (current-time))
    (cond ((not (map-elt (agent-shell--state) :client))
           ;; Needs a client
           (agent-shell--emit-event :event 'init-started)
           (when (and agent-shell-show-busy-indicator
                      (not command))
             (agent-shell-heartbeat-start
              :heartbeat (map-elt agent-shell--state :heartbeat)))
           (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                                         :shell-buffer shell-buffer
                                         :existing-only t)))
             (with-current-buffer viewport-buffer
               (agent-shell-viewport-view-mode)
               (agent-shell-viewport--initialize
                :prompt  command
                :response (agent-shell-viewport--response))))
           (when (agent-shell--initialize-client)
             (agent-shell--handle :command command :shell-buffer shell-buffer)))
          ;; Needs ACP subscriptions
          ((or (not (map-nested-elt (agent-shell--state) '(:client :request-handlers)))
               (not (map-nested-elt (agent-shell--state) '(:client :notification-handlers)))
               (not (map-nested-elt (agent-shell--state) '(:client :error-handlers))))
           (when (agent-shell--initialize-subscriptions)
             (agent-shell--handle :command command :shell-buffer shell-buffer)))
          ;; Needs to send ACP initialize request
          ((not (map-elt (agent-shell--state) :initialized))
           (agent-shell--initiate-handshake
            :shell-buffer shell-buffer
            :on-initiated (lambda ()
                            (map-put! (agent-shell--state) :initialized t)
                            (agent-shell--handle :command command :shell-buffer shell-buffer))))
          ;; Needs to send ACP authenticate request (optional)
          ((and (map-elt (agent-shell--state) :needs-authentication)
                (not (map-elt (agent-shell--state) :authenticated)))
           (agent-shell--authenticate
            :shell-buffer shell-buffer
            :on-authenticated (lambda ()
                                (map-put! (agent-shell--state) :authenticated t)
                                (agent-shell--handle :command command :shell-buffer shell-buffer))))
          ;; Needs to send ACP new session request
          ((not (map-nested-elt (agent-shell--state) '(:session :id)))
           (agent-shell--initiate-session
            :shell-buffer shell-buffer
            :on-session-init (lambda ()
                               ;; Session is now initiated.
                               ;; Consider bootstrapping/handshake complete.
                               ;; Show shell prompt.
                               (unless command
                                 (agent-shell-heartbeat-stop
                                  :heartbeat (map-elt agent-shell--state :heartbeat))
                                 ;; Place these before `shell-maker-finish-output' so
                                 ;; that late-arriving notifications/responses update
                                 ;; them in place instead of inserting past the prompt.
                                 (agent-shell--create-bootstrapping-placeholders (agent-shell--state))
                                 ;; The prompt is normally shown early (at shell
                                 ;; creation) to enable typing ASAP.
                                 ;;
                                 ;; If there's no prompt already, add one now
                                 ;; that initialization is complete.
                                 (unless comint-last-prompt
                                   (shell-maker-finish-output :config shell-maker--config
                                                              :success nil)
                                   (goto-char (point-max)))
                                 (agent-shell--emit-event :event 'prompt-ready))
                               (agent-shell--handle :command command :shell-buffer shell-buffer))))
          ;; Send ACP request to set default model (optional)
          ((and (map-nested-elt (agent-shell--state) '(:agent-config :default-model-id))
                (funcall (map-nested-elt (agent-shell--state)
                                         '(:agent-config :default-model-id)))
                (not (map-elt (agent-shell--state) :set-model)))
           (agent-shell--set-default-model
            :shell-buffer shell-buffer
            :model-id (funcall (map-nested-elt (agent-shell--state)
                                               '(:agent-config :default-model-id)))
            :on-model-changed (lambda ()
                                (map-put! (agent-shell--state) :set-model t)
                                (agent-shell--handle :command command :shell-buffer shell-buffer))))
          ;; Send ACP request to set default session mode (optional)
          ((and (map-nested-elt (agent-shell--state) '(:agent-config :default-session-mode-id))
                (funcall (map-nested-elt (agent-shell--state) '(:agent-config :default-session-mode-id)))
                (not (map-elt (agent-shell--state) :set-session-mode)))
           (agent-shell--set-default-session-mode
            :shell-buffer shell-buffer
            :mode-id (funcall (map-nested-elt (agent-shell--state) '(:agent-config :default-session-mode-id)))
            :on-mode-changed (lambda ()
                               (map-put! (agent-shell--state) :set-session-mode t)
                               (agent-shell--handle :command command :shell-buffer shell-buffer))))
          ;; Initialization complete
          (t
           (agent-shell--emit-event :event 'init-finished)
           ;; Send ACP prompt request
           (when (and command (not (string-empty-p (string-trim command))))
             (agent-shell--send-command :prompt command :shell-buffer shell-buffer))))))

(cl-defun agent-shell--on-error (&key state acp-error)
  "Handle ACP-ERROR with SHELL an STATE."
  (agent-shell--update-fragment
   :state state
   :block-id "Error"
   :body (or (map-elt acp-error 'message) "Some error ¯\\_ (ツ)_/¯")
   :create-new t
   :navigation 'never))

(defun agent-shell-get-config (buffer)
  "Get the agent configuration for BUFFER.

Returns the agent configuration alist for the given buffer, or nil
if the buffer has no agent configuration."
  (with-current-buffer buffer
    (map-elt agent-shell--state :agent-config)))

(defun agent-shell--build-command-for-execution (command)
  "Build COMMAND for the current buffer's configured execution environment.

COMMAND should be a list of command parts (executable and arguments).

Applies `agent-shell-command-prefix', if set."
  (pcase agent-shell-command-prefix
    ((pred functionp)
     (append (funcall agent-shell-command-prefix (current-buffer)) command))
    ((pred listp)
     (append agent-shell-command-prefix command))
    (_ command)))

(defun agent-shell--tool-call-command-to-string (command)
  "Normalize tool call COMMAND to a display string.

COMMAND, when present, may be a shell command string or an argv vector."
  (cond ((stringp command) command)
        ((vectorp command)
         (combine-and-quote-strings (append command nil)))
        ((null command) nil)
        (t (error "Unexpected tool-call command type: %S" (type-of command)))))

(defun agent-shell--active-requests-p (state)
  "Return non-nil if STATE has in-flight requests awaiting responses."
  (map-elt state :active-requests))

(defun agent-shell--make-out-of-session-turn-notification-body (state acp-notification)
  "Build a fragment body for ACP-NOTIFICATION arriving out of turn using STATE."
  (let ((agent-name (or (map-nested-elt state '(:agent-config :mode-line-name))
                        (map-nested-elt state '(:agent-config :buffer-name))
                        "the ACP server")))
    (format "This `session/update` arrived after the turn ended, with no
request in flight to attach it to.  Per-turn updates are expected
while a `session/prompt', `session/load', or `session/push' is
active.

This is unexpected and likely a bug in %s.  Please report it to the
maintainer with the payload below:

```json
%s
```

"
            agent-name
            (agent-shell-with-work-buffer
              (insert (json-serialize acp-notification))
              (json-pretty-print (point-min) (point-max))
              (buffer-string)))))

(defun agent-shell--make-unhandled-notification-body (acp-notification)
  "Build a fragment body for an ACP-NOTIFICATION we have no handler for.
Includes pretty-printed JSON and a `file a feature request' link.

The body is rendered as markdown, so the link is written as one
rather than propertized by hand."
  (format "Unhandled notification (%s) and include:

```json
%s
```

"
          "[please file a feature request](https://github.com/xenodium/agent-shell/issues/new/choose)"
          (agent-shell-with-work-buffer
            (insert (json-serialize acp-notification))
            (json-pretty-print (point-min) (point-max))
            (buffer-string))))

(cl-defun agent-shell--adapt-notification (&key state acp-notification)
  "Return ACP-NOTIFICATION after optional agent-specific adaptation.

When STATE's agent config defines `:notification-adapter', invoke it
with `:acp-notification' and return the result.  Otherwise return
original ACP-NOTIFICATION."
  (if-let* ((adapter (map-nested-elt state '(:agent-config :notification-adapter)))
            ((functionp adapter)))
      (funcall adapter :acp-notification acp-notification)
    acp-notification))

(defun agent-shell--format-tool-call-input (acp-raw-input)
  "Format ACP-RAW-INPUT from a tool call as a fenced code block.

ACP-RAW-INPUT is the alist parsed from an ACP tool call's `rawInput' field.
If it has exactly one key whose value is a non-empty string, that value
is rendered inside a bare fence.  Otherwise ACP-RAW-INPUT is rendered as
pretty-printed JSON inside a json fence."
  (if-let* (((= (length acp-raw-input) 1))
            (value (cdar acp-raw-input))
            ((and (stringp value) (not (string-empty-p value)))))
      (format "```\n%s\n```" value)
    (format "```json\n%s\n```"
            (agent-shell-with-work-buffer
              (insert (json-encode acp-raw-input))
              (json-pretty-print-buffer)
              (buffer-string)))))

(defconst agent-shell--activity-group-label "Activity"
  "Neutral placeholder heading for an activity group before it is refreshed.
Shown until `agent-shell--refresh-activity-group-header' relabels the
group from its members (e.g. \"✓ Activity 2/2\"), and kept while the
active label function still has nothing to summarize (an empty group).")

(defconst agent-shell--activity-group-run-entry-types
  '("tool_call" "tool_call_update" "session/request_permission"
    "agent_thought_chunk")
  "Entry types that keep the current activity run open.
Consecutive tool calls and thoughts share one activity group; any other
rendered entry between them (e.g. a streamed message) starts a fresh one.
A permission request is part of a tool call's own flow (its dialog is
transient, deleted on completion), so it must not break the run.")

(defun agent-shell--activity-group-current-id (state)
  "Return the current activity group id for STATE, advancing on a new run.

Advances STATE's `:activity-group-count' unless `:last-entry-type' keeps
the run open (see `agent-shell--activity-group-run-entry-types'),
mirroring the `:chunked-group-count' pattern used for message/thought
chunks.  Shared by tool-call and thought rendering so both land in the
same group."
  (unless (member (map-elt state :last-entry-type)
                  agent-shell--activity-group-run-entry-types)
    (map-put! state :activity-group-count
              (1+ (or (map-elt state :activity-group-count) 0))))
  (agent-shell--activity-group-latest-id state))

(defun agent-shell--activity-group-latest-id (state)
  "Return the id of the most recently started activity group in STATE.

Unlike `agent-shell--activity-group-current-id', never advances the run
counter, so callers can tell whether the group they are rendering into is
the agent's current run or an earlier one taking a late update.

  (agent-shell--activity-group-latest-id \\='((:activity-group-count . 2)))
  ;; => \"activity-2\""
  (format "activity-%s" (map-elt state :activity-group-count)))

(defun agent-shell--activity-group-id (state tool-call-id)
  "Return TOOL-CALL-ID's activity group-id in STATE, assigning it on first sight.

The assignment is stored on the tool call and reused by later updates, so
a completion arriving after an interleaving message keeps its original
group.  The run counter is shared with thoughts via
`agent-shell--activity-group-current-id'."
  (or (map-nested-elt state `(:tool-calls ,tool-call-id :group-id))
      (let ((group-id (agent-shell--activity-group-current-id state)))
        (agent-shell--save-tool-call state tool-call-id (list (cons :group-id group-id)))
        group-id)))

(defconst agent-shell--tool-call-status-precedence
  '("failed" "in_progress" "pending" "completed")
  "Tool-call statuses from most to least severe, for group aggregation.
A group header shows `completed' only when every member completed;
otherwise the worst present status surfaces (one failure shows `failed').")

(defun agent-shell--group-tool-statuses (state group-id)
  "Return the statuses of all tool calls in STATE assigned to GROUP-ID.

  (agent-shell--group-tool-statuses
   \\='((:tool-calls . ((\"t1\" . ((:group-id . \"g\") (:status . \"completed\")))
                      (\"t2\" . ((:group-id . \"g\") (:status . \"failed\"))))))
   \"g\")
  ;; => (\"completed\" \"failed\")"
  (seq-keep (lambda (pair)
              (when (equal (map-elt (cdr pair) :group-id) group-id)
                (map-elt (cdr pair) :status)))
            (map-elt state :tool-calls)))

(defun agent-shell--activity-group-status-glyph (status)
  "Return a propertized status icon for a group header, or nil for no STATUS."
  (when-let* ((config (and status (agent-shell--status-config status))))
    (propertize (map-elt config :icon)
                'font-lock-face (map-elt config :face))))

(defun agent-shell--activity-group-header-label (statuses)
  "Return the header label for an activity group with member STATUSES.

Formats as `<glyph> Activity completed/total': the glyph is the dominant
status (the first of `agent-shell--tool-call-status-precedence' present,
so `completed' shows only when every member completed), and the count is
how many members completed out of the total, so a non-completed member
counts toward the total but not the numerator.

  (agent-shell--activity-group-header-label \\='(\"completed\" \"completed\"))
  ;; => \"✓ Activity 2/2\"
  (agent-shell--activity-group-header-label \\='(\"completed\" \"failed\"))
  ;; => \"✗ Activity 1/2\""
  (let ((glyph (agent-shell--activity-group-status-glyph
                (seq-find (lambda (status) (member status statuses))
                          agent-shell--tool-call-status-precedence)))
        (heading (propertize "Activity"
                             'font-lock-face 'agent-shell-section-heading))
        (count (propertize
                (format "%d/%d"
                        (seq-count (lambda (status) (equal status "completed")) statuses)
                        (length statuses))
                'font-lock-face 'agent-shell-section-annotation)))
    (string-join (delq nil (list glyph heading count)) " ")))

(cl-defun agent-shell--group-tool-calls (&key state group-id)
  "Return (ID . TOOL-CALL) pairs in STATE assigned to GROUP-ID, in call order.

STATE's `:tool-calls' alist stores the most recent call first, so the
result is reversed to reflect the order the calls were made."
  (nreverse
   (seq-keep (lambda (pair)
               (when (equal (map-elt (cdr pair) :group-id) group-id)
                 pair))
             (map-elt state :tool-calls))))

(defun agent-shell--count-group-thought (state group-id)
  "Record one more thought for the activity group GROUP-ID in STATE.

Thoughts are not stored in `:tool-calls', so the header label functions
cannot see them from the tool-call members alone.  `:activity-thoughts'
keeps a per-group thought count (an alist of group-id to count) so labels
can report how many thoughts a group holds.  Call once per thought, not
per streamed chunk.

  ;; STATE `:activity-thoughts' = ((\"g\" . 1))
  (agent-shell--count-group-thought state \"g\")
  ;; STATE `:activity-thoughts' = ((\"g\" . 2))"
  (let ((counts (map-elt state :activity-thoughts)))
    (setf (map-elt counts group-id) (1+ (map-elt counts group-id 0)))
    (map-put! state :activity-thoughts counts)))

(defun agent-shell--group-thought-count (state group-id)
  "Return the number of thoughts recorded for GROUP-ID in STATE.

  (agent-shell--group-thought-count
   \\='((:activity-thoughts . ((\"g\" . 2)))) \"g\")
  ;; => 2"
  (map-nested-elt state (list :activity-thoughts group-id) 0))

(defun agent-shell--group-has-thought-p (state group-id)
  "Return non-nil when the activity group GROUP-ID in STATE holds a thought.

  (agent-shell--group-has-thought-p
   \\='((:activity-thoughts . ((\"g\" . 1)))) \"g\")
  ;; => t"
  (> (agent-shell--group-thought-count state group-id) 0))

(defconst agent-shell--tool-call-kind-phrases
  '(("execute" . ((:past . "ran") (:present . "run")
                  (:singular . "command") (:plural . "commands")))
    ("read" . ((:past . "read") (:present . "read")
               (:singular . "file") (:plural . "files")))
    ("edit" . ((:past . "edited") (:present . "edit")
               (:singular . "file") (:plural . "files")))
    ("delete" . ((:past . "deleted") (:present . "delete")
                 (:singular . "file") (:plural . "files")))
    ("move" . ((:past . "moved") (:present . "move")
               (:singular . "file") (:plural . "files")))
    ("search" . ((:past . "ran") (:present . "run")
                 (:singular . "search") (:plural . "searches")))
    ("fetch" . ((:past . "fetched") (:present . "fetch")
                (:singular . "resource") (:plural . "resources"))))
  "Per-kind phrasing for descriptive group headers.
Each entry maps a tool-call kind to an alist with `:past'/`:present'
verbs and `:singular'/`:plural' nouns.  The present verb is used while
any member of that kind is still pending or in progress, the past once
all have finished.  Kinds absent here (including nil and \"other\") fall
back to `agent-shell--tool-call-kind-phrase-default'.  Verbs are
lowercase; the assembled summary capitalizes its first word.  See URL
`https://agentclientprotocol.com/protocol/tool-calls'.")

(defconst agent-shell--tool-call-kind-phrase-default
  '((:past . "ran") (:present . "run")
    (:singular . "tool call") (:plural . "tool calls"))
  "Phrasing for kinds absent from `agent-shell--tool-call-kind-phrases'.")

(cl-defun agent-shell--tool-call-kind-phrase (&key kind count pending)
  "Return a lowercase phrase for KIND repeated COUNT times.
When PENDING is non-nil the present-tense verb is used, so a not-yet-run
action reads \"run a command\" rather than \"ran a command\".  Callers
capitalize as needed.

  (agent-shell--tool-call-kind-phrase :kind \"execute\" :count 2)
  ;; => \"ran 2 commands\"
  (agent-shell--tool-call-kind-phrase :kind \"execute\" :count 1 :pending t)
  ;; => \"run a command\""
  (let ((phrase (or (assoc-default kind agent-shell--tool-call-kind-phrases)
                    agent-shell--tool-call-kind-phrase-default)))
    (format "%s %s %s"
            (map-elt phrase (if pending :present :past))
            (if (= count 1) "a" (number-to-string count))
            (map-elt phrase (if (= count 1) :singular :plural)))))

(cl-defun agent-shell--activity-group-descriptive-text (&key members thought)
  "Return a Claude Code style summary phrase for MEMBERS.

MEMBERS is a list of (ID . TOOL-CALL) pairs in call order.  Kinds are
collapsed into counted phrases joined by commas, e.g. \"Ran 3 commands,
read a file\", in first-seen order.  Only the first word is capitalized.
A kind reads in the present tense (\"Run a command\") while any of its
members is still pending or in progress, past tense once all have
finished.

When THOUGHT is non-nil, or any member is a `think'-kind call, the group
also contains a thought, summarized as a leading \"Thought\" phrase
\(e.g. \"Thought, ran 2 commands\", or just \"Thought\" when there are no
other tool calls).  A `think'-kind call is an act of thinking rather than
a tool action, so it folds into that phrase instead of being counted.
Thoughts are not counted."
  (let* ((member-kind (lambda (member) (or (map-elt (cdr member) :kind) "other")))
         (think-p (lambda (member) (equal (funcall member-kind member) "think")))
         (thought (or thought (seq-some think-p members)))
         (tool-members (seq-remove think-p members))
         (tool-phrases
          (seq-map
           (lambda (kind)
             (let ((of-kind (seq-filter (lambda (member)
                                          (equal (funcall member-kind member) kind))
                                        tool-members)))
               (agent-shell--tool-call-kind-phrase
                :kind kind
                :count (length of-kind)
                :pending (seq-some (lambda (member)
                                     (member (map-elt (cdr member) :status)
                                             '("pending" "in_progress")))
                                   of-kind))))
           (seq-uniq (seq-map member-kind tool-members))))
         (summary (string-join (if thought (cons "thought" tool-phrases) tool-phrases)
                               ", ")))
    (if (string-empty-p summary)
        summary
      (concat (upcase (substring summary 0 1)) (substring summary 1)))))

(defun agent-shell-activity-group-descriptive-label (group)
  "Return a Claude Code style header label for GROUP.

GROUP is an alist with :state and :group-id.  A group with a single tool
call whose title is a description (a task or MCP tool) shows that title
\(like \"Backend modularity and dispatch structure\"); larger groups show
a counted summary such as \"Ran 3 commands, read a file\", prefixed with
\"Thought\" when the group also contains a thought.  A group holding only
thoughts reads \"Thought\".  No status glyph is shown, matching Claude
Code's plain-text grouping.  Returns nil with nothing to summarize yet.
See `agent-shell-activity-group-header-label-function'."
  (let* ((state (map-elt group :state))
         (group-id (map-elt group :group-id))
         (members (agent-shell--group-tool-calls :state state :group-id group-id))
         (thought (agent-shell--group-has-thought-p state group-id)))
    (cond
     ;; A lone call whose title is a genuine description (a task or MCP
     ;; tool, of kind nil/"other") stands in for the summary, as Claude
     ;; Code does.  Standard kinds (execute, read, ...) have titles that
     ;; are just a command or path already shown on the member row below,
     ;; so they use the counted summary instead.
     ((and (= (length members) 1) (not thought)
           (member (map-elt (cdar members) :kind) '(nil "other")))
      (map-elt (agent-shell-make-tool-call-label state (caar members)) :title))
     ((or members thought)
      (propertize (agent-shell--activity-group-descriptive-text
                   :members members :thought thought)
                  'font-lock-face 'agent-shell-secondary)))))

(defun agent-shell-activity-group-count-label (group)
  "Return the count-style header label for GROUP.
GROUP is an alist with :state and :group-id.  Formats as
\"✓ Activity 2/2\", counting each tool call and thought as one item (a
thought is always done), so a group of 2 tool calls and a thought reads
\"✓ Activity 3/3\" and a thought-only group reads \"✓ Activity 1/1\".
Returns nil with nothing in the group yet.
See `agent-shell-activity-group-header-label-function'."
  (let* ((state (map-elt group :state))
         (group-id (map-elt group :group-id))
         (statuses (append (agent-shell--group-tool-statuses state group-id)
                           (make-list (agent-shell--group-thought-count state group-id)
                                      "completed"))))
    (when statuses
      (agent-shell--activity-group-header-label statuses))))

(defun agent-shell-activity-group-tally-label (group)
  "Return a per-category tally header for GROUP.

GROUP is an alist with :state and :group-id.  Counts the group's tool
calls by category (one per ACP tool kind, with untyped/MCP calls under
\"Other\") plus a \"Thinking\" count of its thoughts and any `think'-kind
calls.  Shows only non-zero categories in order, e.g. \"Commands: 1
Reads: 3 Edits: 2 Thinking: 3\".  Each label uses the section-heading
face and its count the default face.  Returns nil with nothing to count
yet.
See `agent-shell-activity-group-header-label-function'."
  (let* ((state (map-elt group :state))
         (group-id (map-elt group :group-id))
         ;; Ordered categories, one per ACP tool kind.  A `think'-kind call
         ;; folds into "Thinking" below instead of getting its own category.
         (categories '(((:label . "Commands") (:kind . "execute"))
                       ((:label . "Reads") (:kind . "read"))
                       ((:label . "Edits") (:kind . "edit"))
                       ((:label . "Moves") (:kind . "move"))
                       ((:label . "Deletes") (:kind . "delete"))
                       ((:label . "Searches") (:kind . "search"))
                       ((:label . "Fetches") (:kind . "fetch"))
                       ((:label . "Other") (:kind . "other"))))
         ;; Normalize untyped/MCP calls (nil kind) to "other" so they land
         ;; under the "Other" category.
         (kinds (seq-map (lambda (member) (or (map-elt (cdr member) :kind) "other"))
                         (agent-shell--group-tool-calls :state state :group-id group-id)))
         (tally (append
                 (seq-map (lambda (category)
                            (list (cons :label (map-elt category :label))
                                  (cons :count (seq-count
                                                (lambda (kind)
                                                  (equal kind (map-elt category :kind)))
                                                kinds))))
                          categories)
                 ;; Thoughts plus any `think'-kind calls fold into "Thinking".
                 (list (list (cons :label "Thinking")
                             (cons :count (+ (agent-shell--group-thought-count state group-id)
                                             (seq-count (lambda (kind) (equal kind "think"))
                                                        kinds)))))))
         (parts (seq-keep (lambda (entry)
                            (when (> (map-elt entry :count 0) 0)
                              (concat (propertize (format "%s: " (map-elt entry :label))
                                                  'font-lock-face 'agent-shell-section-heading)
                                      (propertize (number-to-string (map-elt entry :count))
                                                  'font-lock-face 'default))))
                          tally)))
    (when parts
      (string-join parts " "))))

(defvar agent-shell-activity-group-header-label-function
  #'agent-shell-activity-group-descriptive-label
  "Function that renders an activity group's collapsible header label.

Called with an alist containing:

  :state    - the shell state
  :group-id - the activity group id

Returns the propertized header string, or nil when the group is empty (no
tool calls or thoughts yet).

Built-in options:
- `agent-shell-activity-group-count-label' -- count style,
  e.g. \"✓ Activity 2/2\".
- `agent-shell-activity-group-descriptive-label' (default) -- Claude Code
  style, e.g. \"Ran 3 commands, read a file\".
- `agent-shell-activity-group-tally-label' -- per-category counts,
  e.g. \"Commands: 1 Reads: 3 Edits: 2 Thinking: 3\".

This is a plain variable rather than a `defcustom' while the header
style settles; promote it once the rendering choice is worth exposing.")

(defun agent-shell--refresh-activity-group-header (state group-id)
  "Relabel GROUP-ID's header in STATE from its tool calls and thoughts.
Delegates to `agent-shell-activity-group-header-label-function'.
No-op while that function has nothing to summarize (an empty group)."
  (when-let* ((label (funcall agent-shell-activity-group-header-label-function
                              (list (cons :state state)
                                    (cons :group-id group-id)))))
    (agent-shell--update-fragment
     :state state
     :block-id group-id
     :label-left label
     :above-last-prompt (not (agent-shell--active-requests-p state)))))

(cl-defun agent-shell--sync-activity-group-fold (&key state group-id namespace-id)
  "Leave GROUP-ID the only expanded activity group in STATE.

No-op unless `agent-shell-activity-group-expand-by-default' is
`latest', where the group the agent is working in stays expanded and
earlier ones fold away.  GROUP-ID is ignored unless it is STATE's latest
run, so a late update to an earlier group neither re-expands that group
nor collapses the one the agent is currently in.

NAMESPACE-ID is the fragment namespace GROUP-ID's header was rendered
under (nil for STATE's request count), recorded alongside the group so it
can still be found once the turn ends."
  (when-let* (((eq agent-shell-activity-group-expand-by-default 'latest))
              ((equal group-id (agent-shell--activity-group-latest-id state)))
              ((not (equal group-id (map-nested-elt state '(:expanded-activity-group :group-id))))))
    (agent-shell--collapse-expanded-activity-group state)
    (map-put! state :expanded-activity-group
              (list (cons :namespace-id (or namespace-id (map-elt state :request-count)))
                    (cons :group-id group-id)))))

(defun agent-shell--collapse-expanded-activity-group (state)
  "Collapse the activity group STATE last left expanded, if any.

Called both when the agent moves on to a new group and when the turn ends,
so a `latest' session is left with every activity group folded.
Clears STATE's `:expanded-activity-group'."
  (when-let* ((group (map-elt state :expanded-activity-group)))
    (agent-shell--collapse-fragment-group
     :state state
     :namespace-id (map-elt group :namespace-id)
     :block-id (map-elt group :group-id))
    (map-put! state :expanded-activity-group nil)))

(cl-defun agent-shell--on-notification (&key state acp-notification)
  "Handle incoming ACP-NOTIFICATION using STATE."
  (map-put! state :last-activity-time (current-time))
  (cond ((equal (map-elt acp-notification 'method) "session/update")
         ;; Replayed user_message_chunks aren't followed by
         ;; shell-maker's end-of-prompt marker (no real
         ;; `comint-send-input').  Insert it on the first
         ;; non-user_message_chunk after a user prompt so
         ;; `shell-maker--extract-history' can pair the command with
         ;; its response.  Skipped while pending-restore is buffering
         ;; (nothing is being rendered yet).
         (when (and (not (map-elt state :pending-restore))
                    (equal (map-elt state :last-entry-type) "user_message_chunk")
                    (not (equal (map-nested-elt acp-notification '(params update sessionUpdate))
                                "user_message_chunk")))
           (with-current-buffer (map-elt state :buffer)
             (shell-maker-insert-end-of-prompt-marker)))
         (cond
          ;; Pending-restore: accumulate notifications during
          ;; session/load and suppress normal rendering.  Once the
          ;; load completes, the first and last prompt turns are
          ;; replayed through the normal dispatch path.
          ((map-elt state :pending-restore)
           (agent-shell--append-restore-notification state acp-notification))
          ((equal (map-nested-elt acp-notification '(params update sessionUpdate)) "agent_message_chunk")
           ;; The agent has stopped acting and started answering, which
           ;; breaks the activity run.  Fold the group `latest' left
           ;; expanded now, rather than leaving it open behind a response
           ;; that may stream for a while before the next group starts.
           (agent-shell--collapse-expanded-activity-group state)
           ;; Decide message boundaries by ACP's `messageId' when present:
           ;; distinct messages must never coalesce, even if an interleaved
           ;; entry (e.g. a tool call) failed to advance `:last-entry-type'
           ;; and left it looking like a continuation.  `messageId' is
           ;; optional (older agents omit it), so fall back to the turn
           ;; boundary heuristic: a new run whenever the previous rendered
           ;; entry was not itself a message chunk.
           (let* ((message-id (map-nested-elt acp-notification '(params update messageId)))
                  (new-message (if message-id
                                   (not (equal message-id (map-elt state :last-agent-message-id)))
                                 (not (equal (map-elt state :last-entry-type) "agent_message_chunk"))))
                  (content (agent-shell--content-block-to-markdown
                            (map-nested-elt acp-notification '(params update content)))))
             (when new-message
               (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count)))
               (agent-shell--append-transcript
                :text (format "\n## Agent (%s)\n\n" (format-time-string "%F %T"))
                :file-path agent-shell--transcript-file))
             ;; Indent markdown headers in LLM output so they nest
             ;; below the transcript's ## section headers.  Applied
             ;; per-chunk: if a header is split across chunks it may
             ;; not be indented (graceful degradation).
             (agent-shell--append-transcript
              :text (agent-shell--indent-markdown-headers content)
              :file-path agent-shell--transcript-file)
             (agent-shell--emit-event
              :event 'agent-message-chunk
              :data (list (cons :text-chunk (map-nested-elt acp-notification '(params update content text)))))
             (agent-shell--update-fragment
              :state state
              ;; Out of turn, key under a dedicated namespace so the
              ;; message forms its own fragment rather than coalescing
              ;; into the previous turn's final message.
              :namespace-id (unless (agent-shell--active-requests-p state) "out-of-turn")
              ;; Key on `messageId' when present so distinct messages stay
              ;; distinct; otherwise fall back to the per-run group count.
              :block-id (format "%s-agent_message_chunk"
                                (or message-id (map-elt state :chunked-group-count)))
              :body content
              :create-new new-message
              :append t
              :navigation 'never
              :render-body-images t
              ;; Out of turn (no prompt request in flight) lands the
              ;; message above the fresh prompt rather than after it.
              :above-last-prompt (not (agent-shell--active-requests-p state)))
             (map-put! state :last-agent-message-id message-id))
           (map-put! state :last-entry-type "agent_message_chunk"))
          ((equal (map-nested-elt acp-notification '(params update sessionUpdate)) "tool_call")
           (agent-shell--save-tool-call
            state
            (map-nested-elt acp-notification '(params update toolCallId))
            (append (list (cons :title (cond
                                        ((and (string= (map-nested-elt acp-notification '(params update title)) "Skill")
                                              (map-nested-elt acp-notification '(params update rawInput command)))
                                         (format "Skill: %s"
                                                 (agent-shell--tool-call-command-to-string
                                                  (map-nested-elt acp-notification '(params update rawInput command)))))
                                        (t
                                         (map-nested-elt acp-notification '(params update title)))))
                          (cons :status (map-nested-elt acp-notification '(params update status)))
                          (cons :kind (map-nested-elt acp-notification '(params update kind)))
                          (cons :command (agent-shell--tool-call-command-to-string
                                          (map-nested-elt acp-notification '(params update rawInput command))))
                          (cons :description (map-nested-elt acp-notification '(params update rawInput description)))
                          (cons :content (map-nested-elt acp-notification '(params update content)))
                          (cons :raw-input (map-nested-elt acp-notification '(params update rawInput))))
                    (when-let* ((diffs (agent-shell--make-diff-infos
                                        :acp-tool-call (map-nested-elt acp-notification '(params update)))))
                      (list (cons :diffs diffs)))))
           (agent-shell--cancel-idle-timer)
           (agent-shell--emit-event
            :event 'tool-call-update
            :data (list (cons :tool-call-id (map-nested-elt acp-notification '(params update toolCallId)))
                        (cons :tool-call (map-nested-elt state (list :tool-calls (map-nested-elt acp-notification '(params update toolCallId)))))))
           (let ((tool-call-labels (agent-shell-make-tool-call-label
                                    state (map-nested-elt acp-notification '(params update toolCallId))))
                 (group-id (agent-shell--activity-group-id
                            state (map-nested-elt acp-notification '(params update toolCallId)))))
             (agent-shell--update-fragment
              :state state
              :block-id (map-nested-elt acp-notification '(params update toolCallId))
              :label-left (map-elt tool-call-labels :status)
              :label-right (map-elt tool-call-labels :title)
              :group-id group-id
              :group-label agent-shell--activity-group-label
              :group-expanded (agent-shell--activity-group-initial-expanded-p)
              :expanded agent-shell-tool-use-expand-by-default
              :above-last-prompt (not (agent-shell--active-requests-p state)))
             (agent-shell--refresh-activity-group-header state group-id)
             (agent-shell--sync-activity-group-fold :state state :group-id group-id)
             ;; Display plan as markdown block if present
             (when (map-nested-elt acp-notification '(params update rawInput plan))
               (agent-shell--update-fragment
                :state state
                :block-id (concat (map-nested-elt acp-notification '(params update toolCallId)) "-plan")
                :label-left (propertize "Proposed plan" 'font-lock-face 'agent-shell-section-heading)
                :body (agent-shell--format-plan (map-nested-elt acp-notification '(params update rawInput plan)))
                :expanded t
                :above-last-prompt (not (agent-shell--active-requests-p state)))))
           (map-put! state :last-entry-type "tool_call"))
          ((equal (map-nested-elt acp-notification '(params update sessionUpdate)) "agent_thought_chunk")
           (let ((new-thought-p (not (equal (map-elt state :last-entry-type)
                                            "agent_thought_chunk")))
                 (content (agent-shell--content-block-to-markdown
                           (map-nested-elt acp-notification '(params update content))))
                 ;; Share the tool-call run counter so a thought lands in
                 ;; the same activity group as the surrounding tool calls.
                 ;; Read before `:last-entry-type' is advanced below;
                 ;; stable across a thought's streamed chunks since
                 ;; "agent_thought_chunk" keeps the run open.
                 (group-id (agent-shell--activity-group-current-id state)))
             (when new-thought-p
               (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count)))
               (agent-shell--append-transcript
                :text (format "## Agent's Thoughts (%s)\n\n" (format-time-string "%F %T"))
                :file-path agent-shell--transcript-file))
             (agent-shell--append-transcript
              :text (agent-shell--indent-markdown-headers content)
              :file-path agent-shell--transcript-file)
             (agent-shell--update-fragment
              :state state
              ;; Out of turn, key under a dedicated namespace so the
              ;; thought forms its own fragment rather than coalescing
              ;; into the previous turn's final thought (same request-count
              ;; and group-count).  ACP's ContentChunk.messageId is the
              ;; spec's intended discriminator here, but it is optional and
              ;; only populated by newer agents, so we group by turn
              ;; boundary instead.
              :namespace-id (unless (agent-shell--active-requests-p state) "out-of-turn")
              :block-id (format "%s-agent_thought_chunk"
                                (map-elt state :chunked-group-count))
              :label-left  (concat
                            (when-let* ((icon (agent-shell--thought-process-icon)))
                              (concat icon " "))
                            (propertize "Thinking" 'font-lock-face 'agent-shell-section-heading))
              ;; Base face for the body.  The markdown styling rendered on
              ;; top layers its own faces ahead of it, so bold, links and
              ;; code spans in a thought keep their faces.  Set on both
              ;; properties for the reason
              ;; `agent-shell-markdown--mirror-face-to-font-lock-face'
              ;; gives: fontification clears a plain `face' (it covers a
              ;; collapsed body long before the renderer, and its mirror,
              ;; ever sees it), while `font-lock-face' only renders while
              ;; `font-lock-mode' is on.
              :body (agent-shell--add-text-properties
                     content
                     'face 'agent-shell-thought-body
                     'font-lock-face 'agent-shell-thought-body)
              :append (equal (map-elt state :last-entry-type)
                             "agent_thought_chunk")
              :expanded agent-shell-thought-process-expand-by-default
              :group-id group-id
              :group-label agent-shell--activity-group-label
              :group-expanded (agent-shell--activity-group-initial-expanded-p)
              :render-body-images t
              :above-last-prompt (not (agent-shell--active-requests-p state)))
             ;; Count this thought and relabel the header so a thought-only
             ;; group reads "Thinking"/"Thought" instead of the neutral
             ;; placeholder, and a mixed group can mention it.  Both run once
             ;; per thought run, not per streamed chunk: the header label
             ;; depends only on the thought count (advanced here on
             ;; `new-thought-p') and the group's tool statuses (unchanged
             ;; while a pure thought run streams), so relabeling on every
             ;; chunk repeats an identical update and its buffer scan.
             (when new-thought-p
               (agent-shell--count-group-thought state group-id)
               (agent-shell--refresh-activity-group-header state group-id)
               (agent-shell--sync-activity-group-fold
                :state state :group-id group-id
                :namespace-id (unless (agent-shell--active-requests-p state) "out-of-turn"))))
           (map-put! state :last-entry-type "agent_thought_chunk"))
          ((equal (map-nested-elt acp-notification '(params update sessionUpdate)) "user_message_chunk")
           ;; A user_message_chunk replays a user submission.  Render it
           ;; while a `session/load' or `session/push' is active; with no
           ;; request in flight at all it has nothing to attach to, so
           ;; flag it as anomalous; during a live `session/prompt' it is
           ;; a suppressed no-op.
           (cond
            ((seq-find (lambda (r)
                         (member (map-elt r :method)
                                 (append '("session/load")
                                         (agent-shell-experimental--methods))))
                       (map-elt state :active-requests))
             (let ((new-prompt-p (not (equal (map-elt state :last-entry-type)
                                             "user_message_chunk")))
                   (content-text (or (map-nested-elt acp-notification '(params update content text))
                                     (format "[%s]" (or (map-nested-elt acp-notification '(params update content type))
                                                        "unknown")))))
               (when new-prompt-p
                 (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count)))
                 (agent-shell--append-transcript
                  :text (format "## User (%s)\n\n" (format-time-string "%F %T"))
                  :file-path agent-shell--transcript-file))
               (agent-shell--append-transcript
                :text (format "> %s\n"
                              (agent-shell--indent-markdown-headers content-text))
                :file-path agent-shell--transcript-file)
               (agent-shell--update-text
                :state state
                :block-id (format "%s-user_message_chunk"
                                  (map-elt state :chunked-group-count))
                :text (if new-prompt-p
                          ;; Match the field/face shape comint emits for
                          ;; a live prompt: prefix is `field=output' (so
                          ;; comint-next-prompt treats it as a prompt
                          ;; boundary), user text has no `field' (sits
                          ;; in the input region just like one the user
                          ;; just typed).
                          (concat (propertize
                                   (map-nested-elt
                                    state '(:agent-config :shell-prompt))
                                   'font-lock-face '(agent-shell-prompt comint-highlight-prompt)
                                   'field 'output)
                                  (propertize content-text
                                              'font-lock-face 'agent-shell-input))
                        (propertize content-text
                                    'font-lock-face 'agent-shell-input))
                :create-new new-prompt-p
                :append t))
             (map-put! state :last-entry-type "user_message_chunk"))
            ((not (agent-shell--active-requests-p state))
             ;; No session/load or session/push to attach this echo to,
             ;; and unlike tool calls or message chunks from a background
             ;; subagent, nothing to render.  Surface it above the fresh
             ;; prompt for reporting.
             (agent-shell--update-fragment
              :state state
              :block-id "out-of-turn-user-message-chunk"
              :label-left (propertize "Out of turn user_message_chunk - ACP server bug"
                                      'font-lock-face 'agent-shell-section-heading)
              :body (agent-shell--make-out-of-session-turn-notification-body state acp-notification)
              :append t
              :above-last-prompt t))))
          ((equal (map-nested-elt acp-notification '(params update sessionUpdate)) "plan")
           (agent-shell--update-fragment
            :state state
            :block-id "plan"
            :label-left (propertize "Plan" 'font-lock-face 'agent-shell-section-heading)
            :body (agent-shell--format-plan (map-nested-elt acp-notification '(params update entries)))
            :expanded t
            :above-last-prompt (not (agent-shell--active-requests-p state)))
           (map-put! state :last-entry-type "plan"))
          ((equal (map-nested-elt acp-notification '(params update sessionUpdate)) "tool_call_update")
           ;; Update stored tool call data with new status and content
           (agent-shell--save-tool-call
            state
            (map-nested-elt acp-notification '(params update toolCallId))
            (append (list (cons :status (map-nested-elt acp-notification '(params update status)))
                          (cons :content (map-nested-elt acp-notification '(params update content))))
                    ;; The initial tool_call notification often has a
                    ;; generic title (eg. "grep", "bash", "Read").
                    ;; The tool_call_update may have a more descriptive
                    ;; title (eg. 'grep -i -n "tool" /path/to/file').
                    ;; Upgrade to the more descriptive title when available.
                    ;; See https://github.com/xenodium/agent-shell/issues/182
                    ;; See https://github.com/xenodium/agent-shell/issues/309
                    (when-let* ((new-title (map-nested-elt acp-notification '(params update title)))
                                ((not (string-empty-p new-title))))
                      (list (cons :title new-title)))
                    (when-let* ((description (agent-shell--tool-call-command-to-string
                                              (map-nested-elt acp-notification '(params update rawInput description)))))
                      (list (cons :description description)))
                    (when-let* ((command (agent-shell--tool-call-command-to-string
                                          (map-nested-elt acp-notification '(params update rawInput command)))))
                      (list (cons :command command)))
                    (when-let* ((raw-input (map-nested-elt acp-notification '(params update rawInput))))
                      (list (cons :raw-input raw-input)))
                    (when-let* ((locations (map-nested-elt acp-notification '(params update locations))))
                      (list (cons :locations locations)))
                    (when-let* ((diffs (agent-shell--make-diff-infos
                                        :acp-tool-call (map-nested-elt acp-notification '(params update)))))
                      (list (cons :diffs diffs)))))
           ;; OpenCode sends tool_call_update with the populated rawInput
           ;; after session/request_permission, so an open permission
           ;; dialog needs a re-render to surface the arguments.
           ;; See https://github.com/xenodium/agent-shell/issues/617
           (when-let* ((tool-call-id (map-nested-elt acp-notification '(params update toolCallId)))
                       (tool-call (map-nested-elt state (list :tool-calls tool-call-id)))
                       ((map-elt tool-call :permission-request-id)))
             (agent-shell--update-fragment
              :state state
              :block-id (format "permission-%s" tool-call-id)
              :body (with-current-buffer (map-elt state :buffer)
                      (agent-shell--make-tool-call-permission-text
                       :tool-call tool-call
                       :tool-call-id tool-call-id
                       :client (map-elt state :client)
                       :state state))
              :expanded t
              :navigation 'never
              :above-last-prompt (not (agent-shell--active-requests-p state))))
           (agent-shell--cancel-idle-timer)
           (agent-shell--emit-event
            :event 'tool-call-update
            :data (list (cons :tool-call-id (map-nested-elt acp-notification '(params update toolCallId)))
                        (cons :tool-call (map-nested-elt state `(:tool-calls ,(map-nested-elt acp-notification '(params update toolCallId)))))))
           (let* ((diffs (map-nested-elt state `(:tool-calls ,(map-nested-elt acp-notification '(params update toolCallId)) :diffs)))
                  (output (concat
                           "\n\n"
                           (agent-shell--tool-call-update-output-markdown
                            (map-nested-elt acp-notification '(params update)))
                           "\n\n"))
                  (diff-text (agent-shell--format-diffs-as-text diffs))
                  (body-text (if diff-text
                                 (concat output "\n\n" diff-text)
                               output))
                  ;; Whether this update introduces a new tool call rather than
                  ;; editing an earlier one in place.  Captured before the
                  ;; group-id helper assigns a group, so an in-place update
                  ;; does not look new.
                  (tool-newly-grouped
                   (not (map-nested-elt state `(:tool-calls ,(map-nested-elt acp-notification '(params update toolCallId)) :group-id)))))
             ;; Log tool call to transcript when completed or failed
             (when (and (map-nested-elt acp-notification '(params update status))
                        (member (map-nested-elt acp-notification '(params update status)) '("completed" "failed")))
               (agent-shell--append-transcript
                :text (agent-shell--make-transcript-tool-call-entry
                       :status (map-nested-elt acp-notification '(params update status))
                       :title (map-nested-elt state `(:tool-calls ,(map-nested-elt acp-notification '(params update toolCallId)) :title))
                       :kind (map-nested-elt state `(:tool-calls ,(map-nested-elt acp-notification '(params update toolCallId)) :kind))
                       :description (map-nested-elt state `(:tool-calls ,(map-nested-elt acp-notification '(params update toolCallId)) :description))
                       :command (map-nested-elt state `(:tool-calls ,(map-nested-elt acp-notification '(params update toolCallId)) :command))
                       :parameters (agent-shell--extract-tool-parameters
                                    (map-nested-elt state `(:tool-calls ,(map-nested-elt acp-notification '(params update toolCallId)) :raw-input)))
                       :output body-text)
                :file-path agent-shell--transcript-file))
             ;; Hide permission after sending response.
             ;; Status is completed or failed so the user
             ;; likely selected one of: accepted/rejected/always.
             ;; Remove stale permission dialog.
             (when (member (map-nested-elt acp-notification '(params update status))
                           '("completed" "failed"))
               ;; block-id must be the same as the one used as
               ;; agent-shell--update-fragment param by "session/request_permission".
               (agent-shell--delete-fragment :state state :block-id (format "permission-%s" (map-nested-elt acp-notification '(params update toolCallId)))))
             (let* ((tool-call-labels (agent-shell-make-tool-call-label state (map-nested-elt acp-notification '(params update toolCallId))))
                    (group-id (agent-shell--activity-group-id
                               state (map-nested-elt acp-notification '(params update toolCallId))))
                    (tool-call-id (map-nested-elt acp-notification '(params update toolCallId)))
                    (saved-command (map-nested-elt state `(:tool-calls ,tool-call-id :command)))
                    ;; Prepend fenced command to body for Bash-like
                    ;; tools.
                    (command-block (when saved-command
                                     (concat "```console\n" saved-command "\n```")))
                    ;; For tools without a `command' parameter such
                    ;; as MCP tools, render the input parameters
                    ;; above the body so the user can inspect
                    ;; them.  Standard tool kinds like "read" have
                    ;; their parameters baked into the title, so we
                    ;; only render parameters for non-standard tools
                    ;; like MCP calls.
                    (tool-call-kind (map-nested-elt state `(:tool-calls ,tool-call-id :kind)))
                    (saved-input (map-nested-elt state `(:tool-calls ,tool-call-id :raw-input)))
                    (input-block (when (and (member tool-call-kind '(nil "other"))
                                            saved-input
                                            (not saved-command))
                                   (agent-shell--format-tool-call-input saved-input))))
               (agent-shell--update-fragment
                :state state
                :block-id (map-nested-elt acp-notification '(params update toolCallId))
                :label-left (map-elt tool-call-labels :status)
                :label-right (map-elt tool-call-labels :title)
                :group-id group-id
                :group-label agent-shell--activity-group-label
                :group-expanded (agent-shell--activity-group-initial-expanded-p)
                :body (cond
                       (command-block
                        (concat command-block "\n\n" (string-trim body-text)))
                       (input-block
                        (concat input-block "\n\n" (string-trim body-text)))
                       (t
                        (string-trim body-text)))
                :expanded agent-shell-tool-use-expand-by-default
                :above-last-prompt (not (agent-shell--active-requests-p state)))
               (agent-shell--refresh-activity-group-header state group-id)
               (agent-shell--sync-activity-group-fold :state state :group-id group-id))
             ;; Only advance the run boundary when this update introduced a new
             ;; tool call (appended at the end).  An in-place update of an
             ;; earlier tool must not erase an intervening entry's boundary.
             (when tool-newly-grouped
               (map-put! state :last-entry-type "tool_call_update"))))
          ((equal (map-nested-elt acp-notification '(params update sessionUpdate)) "available_commands_update")
           (map-put! state :available-commands (map-nested-elt acp-notification '(params update availableCommands)))
           (agent-shell--update-bootstrapping-fragment
            :state state
            :block-id "available_commands_update"
            :label-left (propertize "Available /commands" 'font-lock-face 'agent-shell-section-heading)
            :body (agent-shell--format-available-commands (map-nested-elt acp-notification '(params update availableCommands))))
           (map-put! state :last-entry-type "available_commands_update"))
          ((equal (map-nested-elt acp-notification '(params update sessionUpdate)) "current_mode_update")
           (let ((updated-session (map-elt state :session))
                 (new-mode-id (map-nested-elt acp-notification '(params update currentModeId))))
             (map-put! updated-session :mode-id new-mode-id)
             (map-put! state :session updated-session)
             (message "Session mode: %s"
                      (agent-shell--resolve-session-mode-name
                       new-mode-id
                       (agent-shell--get-available-modes state)))
             ;; Note: No need to set :last-entry-type as no text was inserted.
             (agent-shell--update-header-and-mode-line)))
          ((equal (map-nested-elt acp-notification '(params update sessionUpdate)) "session_info_update")
           (with-current-buffer (map-elt state :buffer)
             (agent-shell--set-session-title
              (map-nested-elt acp-notification '(params update title))))
           ;; Note: No need to set :last-entry-type as no text was inserted.
           nil)
          ((equal (map-nested-elt acp-notification '(params update sessionUpdate)) "config_option_update")
           (agent-shell--save-config-options
            :state state
            :acp-config-options (map-nested-elt acp-notification '(params update configOptions)))
           (agent-shell--update-header-and-mode-line)
           (agent-shell--emit-event
            :event 'config-option-update
            :data (list (cons :config-options (agent-shell--config-options state))))
           ;; Note: No need to set :last-entry-type as no text was inserted.
           nil)
          ((equal (map-nested-elt acp-notification '(params update sessionUpdate)) "usage_update")
           ;; Extract context window and cost information
           (agent-shell--update-usage-from-notification
            :state state
            :acp-update (map-nested-elt acp-notification '(params update)))
           ;; Update header to reflect new context usage indicator
           (agent-shell--update-header-and-mode-line)
           ;; Note: This is session-level state, no need to set :last-entry-type
           nil)
          ((equal (map-nested-elt acp-notification '(params update sessionUpdate)) "session_push_end")
           (agent-shell-experimental--on-session-push-end
            :state state
            :on-finished (lambda ()
                           (shell-maker-finish-output :config shell-maker--config
                                                      :success t)
                           (agent-shell--prompt-queue-process-next))))
          (acp-logging-enabled
           (agent-shell--update-fragment
            :state state
            :block-id "unhandled-notification"
            :label-left (propertize "Unhandled"
                                    'font-lock-face 'agent-shell-section-heading)
            :body (agent-shell--make-unhandled-notification-body acp-notification)
            :append t
            :above-last-prompt (not (shell-maker-busy)))
           (map-put! state :last-entry-type nil))))
        (acp-logging-enabled
         (agent-shell--update-fragment
          :state state
          :block-id "unhandled-notification"
          :label-left (propertize "Unhandled"
                                  'font-lock-face 'agent-shell-section-heading)
          :body (agent-shell--make-unhandled-notification-body acp-notification)
          :append t
          :above-last-prompt (not (shell-maker-busy)))
         (map-put! state :last-entry-type nil))))

(cl-defun agent-shell--on-request (&key state acp-request)
  "Handle incoming ACP-REQUEST using STATE."
  (cond ((equal (map-elt acp-request 'method) "session/request_permission")
         (agent-shell--save-tool-call
          state (map-nested-elt acp-request '(params toolCall toolCallId))
          (append (list (cons :title (map-nested-elt acp-request '(params toolCall title)))
                        (cons :status (map-nested-elt acp-request '(params toolCall status)))
                        (cons :kind (map-nested-elt acp-request '(params toolCall kind)))
                        (cons :permission-request-id (map-elt acp-request 'id))
                        (cons :permission-actions (agent-shell--make-permission-actions
                                                   (map-nested-elt acp-request '(params options)))))
                  (when-let* ((raw-input (map-nested-elt acp-request '(params toolCall rawInput))))
                    (list (cons :raw-input raw-input)))
                  (when-let* ((content (map-nested-elt acp-request '(params toolCall content))))
                    (list (cons :content content)))
                  (when-let* ((locations (map-nested-elt acp-request '(params toolCall locations))))
                    (list (cons :locations locations)))
                  (when-let* ((diffs (agent-shell--make-diff-infos
                                      :acp-tool-call (map-nested-elt acp-request '(params toolCall)))))
                    (list (cons :diffs diffs)))))
         (let* ((tool-call-id (map-nested-elt acp-request '(params toolCall toolCallId)))
                (permission-handled
                 (and (functionp agent-shell-permission-responder-function)
                      (funcall agent-shell-permission-responder-function
                               (list (cons :tool-call (map-nested-elt state (list :tool-calls tool-call-id)))
                                     (cons :options (agent-shell--make-permission-actions
                                                     (map-nested-elt acp-request '(params options))))
                                     (cons :respond (lambda (option-id)
                                                      (agent-shell--send-permission-response
                                                       :client (map-elt state :client)
                                                       :request-id (map-elt acp-request 'id)
                                                       :option-id option-id
                                                       :state state
                                                       :tool-call-id tool-call-id)
                                                      t)))))))
           (unless permission-handled
             (when (map-nested-elt acp-request '(params toolCall rawInput plan))
               (agent-shell--update-fragment
                :state state
                :block-id (concat tool-call-id "-plan")
                :label-left (propertize "Proposed plan" 'font-lock-face 'agent-shell-section-heading)
                :body (agent-shell--format-plan (map-nested-elt acp-request '(params toolCall rawInput plan)))
                :expanded t
                :above-last-prompt (not (agent-shell--active-requests-p state))))
             ;; block-id must be the same as the one used
             ;; in agent-shell--delete-fragment param.
             (agent-shell--update-fragment
              :state state
              :block-id (format "permission-%s" tool-call-id)
              :body (with-current-buffer (map-elt state :buffer)
                      (agent-shell--make-tool-call-permission-text
                       :tool-call (map-nested-elt state (list :tool-calls tool-call-id))
                       :tool-call-id tool-call-id
                       :client (map-elt state :client)
                       :state state))
              :expanded t
              :navigation 'never
              :above-last-prompt (not (agent-shell--active-requests-p state)))
             (agent-shell-jump-to-latest-permission-button-row)
             (when-let* (((map-elt state :buffer))
                         (viewport-buffer (agent-shell-viewport--buffer
                                           :shell-buffer (map-elt state :buffer)
                                           :existing-only t)))
               (with-current-buffer viewport-buffer
                 (agent-shell-jump-to-latest-permission-button-row)))
             (let ((data (list (cons :request-id (map-elt acp-request 'id))
                               (cons :tool-call-id tool-call-id)
                               (cons :tool-call (map-nested-elt state (list :tool-calls tool-call-id))))))
               (agent-shell--emit-event
                :event 'permission-request
                :data data)
               (agent-shell--start-idle-timer :event 'permission-request :data data))
             (map-put! state :last-entry-type "session/request_permission"))))
        ((equal (map-elt acp-request 'method) "fs/read_text_file")
         (agent-shell--on-fs-read-text-file-request
          :state state
          :acp-request acp-request))
        ((equal (map-elt acp-request 'method) "fs/write_text_file")
         (agent-shell--on-fs-write-text-file-request
          :state state
          :acp-request acp-request))
        ((equal (map-elt acp-request 'method) "session/push")
         (agent-shell-experimental--on-session-push-request
          :state state
          :acp-request acp-request))
        (t
         (let ((method (map-elt acp-request 'method)))
           (agent-shell--update-fragment
            :state state
            :block-id "Unhandled Incoming Request"
            :body (format "⚠ Unhandled incoming request: \"%s\"" method)
            :create-new t
            :navigation 'never
            :above-last-prompt (not (agent-shell--active-requests-p state)))
           ;; Send error response to prevent client from hanging.
           (acp-send-response
            :client (map-elt state :client)
            :response `((:request-id . ,(map-elt acp-request 'id))
                        (:error . ,(acp-make-error
                                    :code -32601
                                    :message (format "Method not found: %s" method)))))
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

(cl-defun agent-shell--on-fs-read-text-file-request (&key state acp-request)
  "Handle fs/read_text_file ACP-REQUEST with STATE."
  (condition-case err
      (let* ((path (agent-shell--resolve-path (map-nested-elt acp-request '(params path))))
             (line (or (map-nested-elt acp-request '(params line)) 1))
             (limit (map-nested-elt acp-request '(params limit)))
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
                    :request-id (map-elt acp-request 'id)
                    :content content)))
    (quit
     ;; Handle C-g interrupts during file read prompts
     (acp-send-response
      :client (map-elt state :client)
      :response (acp-make-fs-read-text-file-response
                 :request-id (map-elt acp-request 'id)
                 :error (acp-make-error
                         :code -32603
                         :message "Operation cancelled by user"))))
    (file-missing
     ;; File doesn't exist - return RESOURCE_NOT_FOUND (-32002).
     ;; This allows agents to distinguish "file not found" from actual errors.
     (acp-send-response
      :client (map-elt state :client)
      :response (acp-make-fs-read-text-file-response
                 :request-id (map-elt acp-request 'id)
                 :error (acp-make-error
                         :code -32002
                         :message "Resource not found"
                         :data `((path . ,(nth 3 err)))))))
    (error
     (acp-send-response
      :client (map-elt state :client)
      :response (acp-make-fs-read-text-file-response
                 :request-id (map-elt acp-request 'id)
                 :error (acp-make-error
                         :code -32603
                         :message (error-message-string err)))))))

(defun agent-shell--call-with-inhibited-minor-modes (modes thunk)
  "Call THUNK with MODES temporarily disabled in the current buffer.

Disable each mode in MODES that is enabled in the current buffer and has
a buffer-local mode variable.  Re-enable any modes disabled by this
function before returning."
  (let (disabled)
    (unwind-protect
        (progn
          (dolist (mode modes)
            (when (and (symbolp mode)
                       (fboundp mode)
                       (boundp mode)
                       (symbol-value mode)
                       (local-variable-p mode))
              (funcall mode -1)
              (push mode disabled)))
          (funcall thunk))
      (dolist (mode disabled)
        (funcall mode 1)))))

(cl-defun agent-shell--on-fs-write-text-file-request (&key state acp-request)
  "Handle fs/write_text_file ACP-REQUEST with STATE."
  (condition-case err
      (let* ((path (agent-shell--resolve-path (map-nested-elt acp-request '(params path))))
             (content (map-nested-elt acp-request '(params content)))
             (dir (file-name-directory path))
             (buffer (or (find-buffer-visiting path)
                         ;; Prevent auto-insert-mode
                         ;; See issue #170
                         (let ((auto-insert nil))
                           (find-file-noselect path)))))
        (when (and dir (not (file-exists-p dir)))
          (make-directory dir t))
        (with-temp-buffer
          (insert content)
          (let ((content-buffer (current-buffer))
                (inhibit-read-only t))
            (with-current-buffer buffer
              (save-restriction
                (widen)
                ;; Set a time-out to prevent locking up on large files
                ;; https://github.com/xenodium/agent-shell/issues/168
                (agent-shell--call-with-inhibited-minor-modes
                 agent-shell-write-inhibit-minor-modes
                 (lambda ()
                   (replace-buffer-contents content-buffer 1.0)))
                (basic-save-buffer)))))
        (agent-shell--emit-event
         :event 'file-write
         :data (list (cons :path path)
                     (cons :content content)))
        (acp-send-response
         :client (map-elt state :client)
         :response (acp-make-fs-write-text-file-response
                    :request-id (map-elt acp-request 'id))))
    (quit
     ;; Handle C-g interrupts during file save prompts
     (acp-send-response
      :client (map-elt state :client)
      :response (acp-make-fs-write-text-file-response
                 :request-id (map-elt acp-request 'id)
                 :error (acp-make-error
                         :code -32603
                         :message "Operation cancelled by user"))))
    (error
     (acp-send-response
      :client (map-elt state :client)
      :response (acp-make-fs-write-text-file-response
                 :request-id (map-elt acp-request 'id)
                 :error (acp-make-error
                         :code -32603
                         :message (error-message-string err)))))))

(defun agent-shell--resolve-path (path)
  "Resolve PATH using `agent-shell-path-resolver-function'."
  (funcall (or agent-shell-path-resolver-function #'identity) path))

(defun agent-shell-cache-dir (&rest components)
  "Return the `agent-shell' cache directory, creating it if needed.

The base location is system-dependent and honors `XDG_CACHE_HOME'
when set.  COMPONENTS, if given, name a subdirectory beneath the
cache directory, which is created as well.  Return the absolute
path of the resulting directory."
  (let* ((base (or (getenv "XDG_CACHE_HOME")
                   (pcase system-type
                     ('darwin (expand-file-name "Library/Caches" "~"))
                     ('windows-nt (or (getenv "LOCALAPPDATA") (getenv "APPDATA")))
                     ;; Emacs write getCacheDir() into this environment variable
                     ('android (getenv "TMPDIR"))
                     ((or 'ms-dos 'cygwin 'haiku) nil)
                     (_ (expand-file-name ".cache" "~")))
                   (expand-file-name "cache" user-emacs-directory)))
         (cache-dir (apply #'file-name-concat base "agent-shell" components)))
    (make-directory cache-dir t)
    cache-dir))

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
  (string-join
   (seq-map
    (lambda (cmd)
      (concat
       (propertize (concat "/" (map-elt cmd 'name))
                   'font-lock-face 'agent-shell-list-name)
       "\n"
       (propertize (map-elt cmd 'description)
                   'font-lock-face 'agent-shell-secondary)))
    commands)
   "\n\n"))

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
                                  (when-let* ((enabled-items (delq nil (mapcar
                                                                        (lambda (cap-pair)
                                                                          ;; Match (key . t) and (key) forms.
                                                                          ;; eg. promptCapabilities uses (image . t)
                                                                          ;; but sessionCapabilities uses (fork).
                                                                          (when (or (eq (cdr cap-pair) t)
                                                                                    (null (cdr cap-pair)))
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
                                 ;; Top-level capabilities (loadSession)
                                 (t
                                  (cons (downcase group-name) nil)))))
                            capabilities))))
    (agent-shell--align-alist
     :data categories
     :columns (list
               (lambda (pair)
                 (propertize (car pair)
                             'font-lock-face 'agent-shell-list-name))
               (lambda (pair)
                 (when (cdr pair)
                   (propertize (cdr pair)
                               'font-lock-face 'agent-shell-secondary))))
     :joiner "\n")))

(cl-defun agent-shell--make-diff-infos (&key acp-tool-call)
  "Make a list of diff infos from ACP-TOOL-CALL.

A single tool call may carry more than one diff (eg.  Codex editing
several files in one turn), so this returns a list with one entry per
diff.  See https://github.com/xenodium/agent-shell/issues/580

Diff items are extracted by `agent-shell--diff-items' and converted by
`agent-shell--make-diff-info', which documents each entry's schema."
  (let ((locations (map-elt acp-tool-call 'locations)))
    (seq-keep (lambda (diff-item)
                (agent-shell--make-diff-info
                 :acp-diff-item diff-item
                 :locations locations))
              (agent-shell--diff-items acp-tool-call))))

(defun agent-shell--diff-items (acp-tool-call)
  "Return the raw diff items in ACP-TOOL-CALL.

Diffs may arrive as standard ACP content with type \"diff\" containing
oldText/newText/path fields:

  https://agentclientprotocol.com/protocol/schema#toolcallcontent

or, for some agents, in rawInput (eg.  Copilot sends old_str/new_str/path;
see https://github.com/xenodium/agent-shell/issues/217, while goose sends
before/after/path; see https://github.com/xenodium/agent-shell/issues/569).

Returns a list of alists with oldText/newText/path keys, normalizing
rawInput variants to that shape."
  (let ((content (map-elt acp-tool-call 'content))
        (raw-input (map-elt acp-tool-call 'rawInput)))
    (cond
     ;; Single diff object
     ((and content (equal (map-elt content 'type) "diff"))
      (list content))
     ;; Vector/array content - collect all diff items
     ((vectorp content)
      (seq-filter (lambda (item)
                    (equal (map-elt item 'type) "diff"))
                  content))
     ;; List content - collect all diff items
     ((and content (listp content))
      (seq-filter (lambda (item)
                    (equal (map-elt item 'type) "diff"))
                  content))
     ;; Attempt to get from rawInput.
     ((and raw-input (map-elt raw-input 'new_str))
      (list `((oldText . ,(or (map-elt raw-input 'old_str) ""))
              (newText . ,(map-elt raw-input 'new_str))
              (path . ,(map-elt raw-input 'path)))))
     ;; Attempt diff from rawInput (eg. Copilot).
     ((and raw-input (map-elt raw-input 'diff))
      (let ((parsed (agent-shell--parse-unified-diff
                     (map-elt raw-input 'diff))))
        (list `((oldText . ,(car parsed))
                (newText . ,(cdr parsed))
                (path . ,(or (map-elt raw-input 'fileName)
                             (map-elt raw-input 'path)))))))
     ;; Attempt diff from rawInput (eg. goose).
     ((and raw-input (map-elt raw-input 'before))
      (list `((oldText . ,(or (map-elt raw-input 'before) ""))
              (newText . ,(map-elt raw-input 'after))
              (path . ,(map-elt raw-input 'path))))))))

(cl-defun agent-shell--make-diff-info (&key acp-diff-item locations)
  "Convert a single ACP-DIFF-ITEM to a diff info alist.

ACP-DIFF-ITEM is an alist with oldText/newText/path keys, as produced by
`agent-shell--diff-items'.

LOCATIONS is the ACP tool call `locations' array.  When an entry's path
matches the diff, its line is carried as a hint for locating the change.
It is optional (and often absent), but authoritative when present.

Returns an alist of the form:

  ((:old . old-text)
   (:new . new-text)
   (:file . file-path)
   (:line . hint-line))

The :line entry is omitted when no matching location line is available.
Returns nil when the item has no newText or path."
  (when-let* ((new-text (map-elt acp-diff-item 'newText))
              (file-path (map-elt acp-diff-item 'path)))
    (append
     ;; oldText can be nil for Write tools creating new files, default to "".
     ;; TODO: Currently don't have a way to capture overwrites
     (list (cons :old (or (map-elt acp-diff-item 'oldText) ""))
           (cons :new new-text)
           (cons :file file-path))
     (when-let* ((location (seq-find (lambda (location)
                                       (equal (map-elt location 'path) file-path))
                                     locations))
                 (line (map-elt location 'line)))
       (list (cons :line line))))))

;; Based on https://github.com/editor-code-assistant/eca-emacs/blob/298849d1aae3241bf8828b6558c6deb45d75a3c8/eca-diff.el#L22
(defun agent-shell--parse-unified-diff (diff-string)
  "Parse unified DIFF-STRING into old and new text.
Returns a cons cell (OLD-TEXT . NEW-TEXT)."
  (let (old-lines new-lines in-hunk)
    (dolist (line (split-string diff-string "\n"))
      (cond
       ((string-match "^@@.*@@" line)
        (setq in-hunk t))
       ((and in-hunk (string-prefix-p " " line))
        (push (substring line 1) old-lines)
        (push (substring line 1) new-lines))
       ((and in-hunk (string-prefix-p "-" line))
        (push (substring line 1) old-lines))
       ((and in-hunk (string-prefix-p "+" line))
        (push (substring line 1) new-lines))))
    (cons (string-join (nreverse old-lines) "\n")
          (string-join (nreverse new-lines) "\n"))))

(defun agent-shell--format-diff-as-text (diff)
  "Format DIFF info as text suitable for display in tool call body.

DIFF should be a single entry as returned by `agent-shell--make-diff-infos':
  ((:old . old-text) (:new . new-text) (:file . file-path))"
  (when-let* (diff
              (old-file (make-temp-file "old"))
              (new-file (make-temp-file "new")))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert (map-elt diff :old)))
          (with-temp-file new-file (insert (map-elt diff :new)))
          (agent-shell-with-work-buffer
            (call-process diff-command nil t nil "-U3" old-file new-file)
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
            ;; Tag the whole diff as already-rendered output so the
            ;; markdown renderer's avoid-ranges include it — context
            ;; lines like ` # Foo' or ` > Bar' must display verbatim,
            ;; not as a header / blockquote.  See PR #597.
            (add-text-properties (point-min) (point-max)
                                 '(agent-shell-markdown-frozen t
                                                               rear-nonsticky (agent-shell-markdown-frozen)))
            (buffer-string)))
      (delete-file old-file)
      (delete-file new-file))))

(defun agent-shell--diff-box (label)
  "Return a boxed LABEL header string like the tool call changes header.

For example, LABEL \"changes\" returns:

  ╭─────────╮
  │ changes │
  ╰─────────╯"
  (let* ((text (concat " " label " "))
         (line (make-string (length text) ?─)))
    (concat "╭" line "╮\n"
            "│" text "│\n"
            "╰" line "╯")))

(defun agent-shell--format-diffs-as-text (diffs)
  "Format DIFFS as text suitable for display in the tool call body.

DIFFS is a list of diff infos as returned by
`agent-shell--make-diff-infos'.  Each diff is rendered beneath a boxed
header naming its file (or \"changes\" when the file is unknown)."
  (when-let* ((sections
               (seq-keep
                (lambda (diff)
                  (when-let* ((text (agent-shell--format-diff-as-text diff)))
                    (concat (agent-shell--diff-box
                             (if-let* ((file (map-elt diff :file)))
                                 (agent-shell--shorten-paths file)
                               "changes"))
                            "\n\n" text)))
                diffs)))
    (string-join sections "\n\n")))

(defun agent-shell--diff-line-stats (diff)
  "Return added/removed line counts for DIFF, or nil.

DIFF is a single entry as returned by `agent-shell--make-diff-infos'.
Counts come from a unified diff between the old and new text, so
they reflect actual added and removed lines rather than net
line-count change.

For example, replacing 5 old lines with 23 new lines:

  ((:old . \"...5 lines...\") (:new . \"...23 lines...\"))

returns:

  ((:added . 23) (:removed . 5))"
  (when-let* (diff
              (old-file (make-temp-file "agent-shell-old"))
              (new-file (make-temp-file "agent-shell-new")))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert (or (map-elt diff :old) "")))
          (with-temp-file new-file (insert (or (map-elt diff :new) "")))
          (agent-shell-with-work-buffer
            (call-process diff-command nil t nil "-U0" old-file new-file)
            (goto-char (point-min))
            (let ((added 0) (removed 0))
              (while (not (eobp))
                (cond ((looking-at "^\\+\\+\\+"))
                      ((looking-at "^---"))
                      ((looking-at "^\\+") (setq added (1+ added)))
                      ((looking-at "^-") (setq removed (1+ removed))))
                (forward-line 1))
              (list (cons :added added)
                    (cons :removed removed)))))
      (delete-file old-file)
      (delete-file new-file))))

(defun agent-shell--diffs-line-stats (diffs)
  "Return added/removed line counts aggregated across DIFFS, or nil.

DIFFS is a list of diff infos as returned by
`agent-shell--make-diff-infos'.  Counts are summed across every diff.

Returns nil when DIFFS is empty, otherwise:

  ((:added . total-added) (:removed . total-removed))"
  (when diffs
    (let ((added 0) (removed 0))
      (dolist (diff diffs)
        (when-let* ((stats (agent-shell--diff-line-stats diff)))
          (setq added (+ added (map-elt stats :added)))
          (setq removed (+ removed (map-elt stats :removed)))))
      (list (cons :added added)
            (cons :removed removed)))))

(defun agent-shell--format-line-stats (stats)
  "Return a propertized \"+N -M\" summary for STATS, or nil.

STATS is in the form ((:added . N) (:removed . M)).  The added
count is faced with `diff-added' and the removed count with
`diff-removed'.  Returns nil when there are no added or removed
lines.

For example, STATS adding 23 lines and removing 5 returns the
string \"+23 -5\"."
  (when-let* ((added (map-elt stats :added))
              (removed (map-elt stats :removed))
              ((or (> added 0) (> removed 0))))
    (string-join
     (delq nil
           (list (when (> added 0)
                   (propertize (format "+%d" added) 'font-lock-face 'diff-added))
                 (when (> removed 0)
                   (propertize (format "-%d" removed) 'font-lock-face 'diff-removed))))
     " ")))

(defun agent-shell--format-diff-line-stats (diff)
  "Return a propertized \"+N -M\" summary for DIFF, or nil.

DIFF is in the form returned by `agent-shell--make-diff-infos'."
  (agent-shell--format-line-stats (agent-shell--diff-line-stats diff)))

(defun agent-shell--format-diffs-line-stats (diffs)
  "Return a propertized \"+N -M\" summary aggregated across DIFFS, or nil.

DIFFS is a list of diff infos as returned by
`agent-shell--make-diff-infos'."
  (agent-shell--format-line-stats (agent-shell--diffs-line-stats diffs)))

(cl-defun agent-shell--make-error-handler (&key state shell-buffer)
  "Create ACP error handler with SHELL-BUFFER STATE."
  (lambda (acp-error raw-message)
    (agent-shell-heartbeat-stop
     :heartbeat (map-elt state :heartbeat))
    (with-current-buffer (map-elt state :buffer)
      (agent-shell--update-fragment
       :state (agent-shell--state)
       :block-id (format "failed-%s-id:%s-code:%s"
                         (map-elt state :request-count)
                         (or (map-elt acp-error 'id) "?")
                         (or (map-elt acp-error 'code) "?"))
       :body (agent-shell--make-error-dialog-text
              :code (map-elt acp-error 'code)
              :message (map-elt acp-error 'message)
              :raw-message raw-message)
       :create-new t
       :above-last-prompt (not (agent-shell--active-requests-p (agent-shell--state)))))
    ;; TODO: Mark buffer command with shell failure.
    (with-current-buffer shell-buffer
      (agent-shell--emit-event :event 'error
                               :data (list (cons :code (map-elt acp-error 'code))
                                           (cons :message (map-elt acp-error 'message))))
      (shell-maker-finish-output :config shell-maker--config
                                 :success t))))

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

(cl-defun agent-shell--make-boxed-message (&key text width)
  "Return TEXT framed in a rounded Unicode box.
When WIDTH is nil, the box auto-sizes to fit TEXT (respecting any
existing newlines).  Otherwise the box is WIDTH columns wide and TEXT
is word-wrapped to fit — the usable text area is WIDTH - 4 columns
\(two for the `│' borders and two for the one-column padding on each
side)."
  (let* ((lines (if width
                    (agent-shell-with-work-buffer
                      (insert text)
                      (let ((fill-column (max 1 (- width 4))))
                        (fill-region (point-min) (point-max)))
                      (split-string (buffer-string) "\n"))
                  (split-string text "\n")))
         (text-width (apply #'max 1 (mapcar #'string-width lines)))
         (inner-width (+ text-width 2))
         (middle (mapconcat
                  (lambda (line)
                    (concat "│ " line
                            (make-string
                             (- text-width (string-width line)) ?\s)
                            " │"))
                  lines "\n")))
    (concat "╭" (make-string inner-width ?─) "╮\n"
            middle "\n"
            "╰" (make-string inner-width ?─) "╯")))

(cl-defun agent-shell--make-error-dialog-text (&key code message raw-message)
  "Create formatted error dialog text with CODE, MESSAGE, and RAW-MESSAGE."
  (format "╭─

  %s Error (%s) %s

  %s

  %s

╰─"
          (propertize "⚠" 'font-lock-face 'agent-shell-error)
          (or code "?")
          (propertize "⚠" 'font-lock-face 'agent-shell-error)
          (or message "¯\\_ (ツ)_/¯")
          (agent-shell--make-button
           :text "Details" :help "Details" :kind 'error
           :action (lambda ()
                     (interactive)
                     (agent-shell--view-as-error
                      (agent-shell-with-work-buffer
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
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell--cancel-idle-timer)
    (agent-shell--emit-event :event 'clean-up)
    (agent-shell--shutdown)
    ;; Kill any open diff buffers associated with tool calls.
    (map-do (lambda (_tool-call-id tool-call-data)
              (when-let* ((diff-buf (map-elt tool-call-data :diff-buffer)))
                (agent-shell-diff-kill-buffer diff-buf)))
            (map-elt (agent-shell--state) :tool-calls))
    (when-let* (((map-elt (agent-shell--state) :buffer))
                (viewport-buffer (agent-shell-viewport--buffer
                                  :shell-buffer (map-elt (agent-shell--state) :buffer)
                                  :existing-only t))
                (buffer-live-p viewport-buffer))
      (kill-buffer viewport-buffer))
    ;; Last, so the agent is gone before its working directory can be.
    (when (and agent-shell--pending-directory-cleanup
               (file-directory-p agent-shell--pending-directory-cleanup))
      (delete-directory agent-shell--pending-directory-cleanup t t))))

(defun agent-shell--shutdown ()
  "Shut down shell activity."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (when (map-elt (agent-shell--state) :client)
    (acp-shutdown :client (map-elt (agent-shell--state) :client))
    (map-put! (agent-shell--state) :client nil)
    (map-put! (agent-shell--state) :initialized nil)
    (map-put! (agent-shell--state) :authenticated nil)
    (map-put! (agent-shell--state) :set-model nil)
    (map-put! (agent-shell--state) :set-session-mode nil))
  (agent-shell-heartbeat-stop
   :heartbeat (map-elt (agent-shell--state) :heartbeat)))

(defcustom agent-shell-dot-subdir-function #'agent-shell--dot-subdir-in-repo
  "Function used by `agent-shell--dot-subdir' to resolve subdirectory paths.
Called with one argument, SUBDIR (a string such as \"screenshots\" or
\"transcripts\"), and must return the absolute path to that subdirectory.
Directory creation is handled by `agent-shell--dot-subdir', not by this
function."
  :type '(choice (const :tag "In repo (.agent-shell/)" agent-shell--dot-subdir-in-repo)
                 (function :tag "Custom function"))
  :group 'agent-shell)

(defun agent-shell--dot-subdir-in-repo (subdir)
  "Return path to .agent-shell/SUBDIR under the project root.

For example:

  (agent-shell--dot-subdir-in-repo \"screenshots\")
  => \"/path/to/project/.agent-shell/screenshots\""
  (expand-file-name (file-name-concat ".agent-shell" subdir)
                    (agent-shell-cwd)))

(defun agent-shell--dot-subdir (subdir)
  "Return path to SUBDIR for `agent-shell' data, creating it if needed.
Calls `agent-shell-dot-subdir-function' to resolve the path.
When the directory is first created under the project .agent-shell/ directory
inside a git repo and .agent-shell/ is not yet ignored, automatically add it
to .gitignore.  This gitignore update is a one-time operation: if the entry is
later removed from .gitignore it will not be re-added."
  (unless (functionp agent-shell-dot-subdir-function)
    (error "Set agent-shell-dot-subdir-function to a function"))
  (let ((dir (funcall agent-shell-dot-subdir-function subdir)))
    (unless (and (stringp dir) (not (string-empty-p (string-trim dir))))
      (error "Failed to resolve agent-shell data directory (subdir: %s).  Resulting directory is not a non-empty string (dir: %s)" subdir dir))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (when (agent-shell--dot-subdir-in-repo-p dir)
        (agent-shell--ensure-gitignore (agent-shell-cwd))))
    dir))

(defun agent-shell--dot-subdir-in-repo-p (dir)
  "Return non-nil when DIR is under project's `agent-shell' dot directory.

For example:

  (agent-shell--dot-subdir-in-repo-p
    \"/path/to/project/.agent-shell/screenshots\") => t

  (agent-shell--dot-subdir-in-repo-p
   \"/home/user/.emacs.d/agent-shell/project/screenshots\") => nil"
  (file-in-directory-p dir
                       (file-name-as-directory
                        (expand-file-name ".agent-shell" (agent-shell-cwd)))))

(defun agent-shell--ensure-gitignore (project-root)
  "If .agent-shell/ is not ignored under PROJECT-ROOT, add it to .git/info/exclude."
  (condition-case nil
      (when-let* (((eq 'Git (vc-responsible-backend project-root t)))
                  (default-directory project-root)
                  ((not (zerop (process-file "git" nil nil nil
                                             "check-ignore" "-q" ".agent-shell"))))
                  (git-dir (with-temp-buffer
                              (when (zerop (process-file "git" nil t nil
                                                         "rev-parse" "--git-dir"))
                                (string-trim (buffer-string)))))
                  (exclude-file (expand-file-name "info/exclude" git-dir)))
        (make-directory (file-name-directory exclude-file) t)
        (with-temp-buffer
          (when (file-exists-p exclude-file)
            (insert-file-contents exclude-file))
          (goto-char (point-max))
          (unless (bolp)
            (insert "\n"))
          (insert "/.agent-shell/\n")
          (write-region nil nil exclude-file)))
    (error nil)))

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

(cl-defun agent-shell--save-clipboard-image (&key destination-dir no-error)
  "Save clipboard image to DESTINATION-DIR.
Returns the full path to the saved image file on success.
When NO-ERROR is non-nil, return nil instead of signaling errors.

Needs external utilities.  See `agent-shell-clipboard-image-handlers'
for details."
  (unless destination-dir
    (error "Destination-dir is required"))
  (let* ((file-path (expand-file-name
                     (format "clipboard-%s.png"
                             (format-time-string "%Y%m%d-%H%M%S"))
                     destination-dir))
         (handler (seq-find
                   (lambda (h)
                     (executable-find (map-elt h :command)))
                   agent-shell-clipboard-image-handlers)))
    (cond
     ((not handler)
      (unless no-error
        (error "No clipboard image utility found (tried: %s)"
               (mapconcat (lambda (h) (map-elt h :command))
                          agent-shell-clipboard-image-handlers ", "))))
     (t
      (condition-case err
          (funcall (map-elt handler :save) file-path)
        (error
         (unless no-error
           (signal (car err) (cdr err)))))
      (cond
       ((not (file-exists-p file-path))
        (unless no-error
          (error "Clipboard image file was not created")))
       ((zerop (nth 7 (file-attributes file-path)))
        (delete-file file-path)
        (unless no-error
          (error "No image found in clipboard")))
       (t
        file-path))))))

(defcustom agent-shell-status-kind-label-function
  #'agent-shell--icon-and-kind-status-kind-label
  "Function to render status and kind labels.

Called with two arguments: STATUS (string or nil) and KIND (string or nil).
Should return a propertized string or nil.

STATUS is one of: \"pending\", \"in_progress\", \"completed\", \"failed\".
See URL `https://agentclientprotocol.com/protocol/schema#toolcallstatus'.

KIND is the tool call kind string (e.g. \"read\", \"edit\", \"execute\") or nil.
See URL `https://agentclientprotocol.com/protocol/tool-calls'."
  :type 'function
  :group 'agent-shell)

(cl-defun agent-shell--make-status-kind-label (&key status kind)
  "Render STATUS and KIND using `agent-shell-status-kind-label-function'."
  (funcall agent-shell-status-kind-label-function status kind))

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

(defun agent-shell--first-line (text)
  "Return TEXT's first line, ellipsized when more lines follow.

\"cd /tmp\" -> \"cd /tmp\"
\"cd /tmp\\n\" -> \"cd /tmp\"
\"cd /tmp\\ngrep -n foo bar.el\" -> \"cd /tmp…\""
  (when-let* ((text (string-trim-right (or text "")))
              ((not (string-empty-p text))))
    (concat (seq-first (split-string text "\n"))
            (when (string-search "\n" text)
              "…"))))

(defun agent-shell-make-tool-call-label (state tool-call-id)
  "Create tool call label from STATE using TOOL-CALL-ID.

Returns propertized labels in :status and :title propertized."
  (when-let* ((tool-call (map-nested-elt state `(:tool-calls ,tool-call-id))))
    (let* ((title (when-let* ((text (agent-shell--shorten-paths
                                     (map-elt tool-call :title)))
                              ;; Execute commands go to body instead; use description as title.
                              ((not (equal (map-elt tool-call :kind) "execute"))))
                    ;; Strip kind prefix from title to avoid
                    ;; redundancy "[read] Read file.el" becomes
                    ;; "[read] file.el"
                    (if (and (map-elt tool-call :kind)
                             (string-match-p (concat "\\`" (regexp-quote
                                                            (map-elt tool-call :kind)) " ")
                                             (downcase text)))
                        (string-trim-left (substring text (length (map-elt tool-call :kind))))
                      text)))
           (description (or (agent-shell--shorten-paths
                             (map-elt tool-call :description))
                            ;; Fall back to the first line of the command when
                            ;; description is missing for execute tool calls.
                            (when (equal (map-elt tool-call :kind) "execute")
                              (agent-shell--first-line (map-elt tool-call :title)))))
           ;; Append a "+N -M" diff summary to edit titles.
           (stats (agent-shell--format-diffs-line-stats (map-elt tool-call :diffs)))
           (label (cond ((and title description
                              (not (equal (string-remove-prefix "`" (string-remove-suffix "`" (string-trim title)))
                                          (string-remove-prefix "`" (string-remove-suffix "`" (string-trim description))))))
                         (concat
                          (propertize title 'font-lock-face 'default)
                          " "
                          (propertize description 'font-lock-face 'agent-shell-section-annotation)))
                        (title
                         (propertize title 'font-lock-face 'default))
                        (description
                         (propertize description 'font-lock-face 'default)))))
      `((:status . ,(agent-shell--make-status-kind-label
                     :status (map-elt tool-call :status)
                     :kind (map-elt tool-call :kind)))
        (:title . ,(if (and label stats)
                       (concat label " " stats)
                     (or label stats)))))))

(defun agent-shell--format-plan (entries)
  "Format plan ENTRIES for shell rendering.

ENTRIES may be a string or a sequence of alists, for example:

  \\='(((status . \"completed\")
       (content . \"Set up environment\"))
      ((status . \"pending\")
       (content . \"Run tests\")))

Strings are returned as-is.  Each alist entry is expected to have
a `status' key and a `content' or `step' key."
  (cond
   ((stringp entries) entries)
   ((or (vectorp entries) (listp entries))
    (agent-shell--align-alist
     :data entries
     :columns (list
               (lambda (entry)
                 (agent-shell--make-status-kind-label :status (map-elt entry 'status)))
               (lambda (entry)
                 (or (map-elt entry 'content)
                     ;; codex-acp uses non-standard 'step
                     ;; instead of standard 'content.
                     (map-elt entry 'step))))
     :separator " "
     :joiner "\n"))))

(cl-defun agent-shell--make-button (&key text help kind action keymap properties (boxed t))
  "Make button with TEXT, HELP text, KIND, KEYMAP, ACTION, and PROPERTIES.
PROPERTIES is an optional plist of additional text properties to apply.

BOXED draws TEXT as a button and defaults to t.  Pass nil for text
that acts but shouldn't look like a button, such as an inline file
link: TEXT is then used verbatim, with no padding, brackets or box.
Verbatim matters where the text is also content, as in the prompt,
whose buffer text is what gets sent to the agent."
  ;; TODO: Bind a shared keymap to a named command reading text
  ;; properties, rather than a per-call keymap over ACTION.  Anonymous
  ;; closures can't be rebound, which is what issue #759 is about.  See
  ;; `agent-shell-ui-fragment-map' for the shape to follow.
  (let ((button (apply
                 #'agent-shell--add-text-properties
                 (cond ((not boxed) text)
                       ;; Use [ ] brackets in TUI which cannot render the box border.
                       ((display-graphic-p) (format " %s " text))
                       (t (format "[ %s ]" text)))
                 (append
                  ;; Skipped rather than set to nil when unboxed: a nil
                  ;; face would merge into the caller's own face as an
                  ;; invalid spec.
                  (when boxed
                    (list 'font-lock-face '(:box t)
                          'face '(:box t)))
                  (list 'help-echo help
                        'pointer 'hand
                        'keymap (let ((map (make-sparse-keymap)))
                                  (define-key map [mouse-1] action)
                                  (define-key map (kbd "RET") action)
                                  (define-key map [remap self-insert-command] 'ignore)
                                  (when keymap
                                    (set-keymap-parent map keymap))
                                  map)
                        'button kind
                        'rear-nonsticky t)))))
    (if properties
        (apply #'agent-shell--add-text-properties button properties)
      button)))

(cl-defun agent-shell--make-file-link (&key label file line-start line-end (face 'agent-shell-link) hint)
  "Return LABEL as text opening FILE when invoked.

LINE-START and LINE-END, when given, select those lines as the region
on open, reusing a window already showing FILE.  Without them the file
is simply visited.

FACE defaults to `agent-shell-link'.  Pass nil to leave LABEL's own
face alone, as an image preview does.

HINT is echoed when the cursor enters LABEL, for example \"open file\".

These links go into the prompt, whose buffer text is sent to the agent,
so LABEL is used verbatim (see the BOXED argument of
`agent-shell--make-button')."
  (agent-shell--make-button
   :text label
   :boxed nil
   :action (lambda ()
             (interactive)
             (agent-shell-markdown-visit-file :file file
                                              :line-start line-start
                                              :line-end line-end))
   :properties (append
                (when face
                  (list 'font-lock-face face 'face face))
                (when hint
                  (list 'cursor-sensor-functions
                        (list (lambda (_window _old-pos action)
                                (when (eq action 'entered)
                                  (message "Press RET to %s" hint)))))))))

(defun agent-shell--buffer-name-prefix (agent-name)
  "Return the prefix a buffer name for AGENT-NAME places before the project.
Returns nil when `agent-shell-buffer-name-format' is a custom
function, whose output cannot be decomposed into a prefix and a
project name.

For example, with the default format:

  (agent-shell--buffer-name-prefix \"Claude\") => \"Claude Agent @ \""
  (pcase agent-shell-buffer-name-format
    ('kebab-case
     (format "%s-agent @ "
             (downcase (replace-regexp-in-string " " "-" agent-name))))
    ('default
     (format "%s Agent @ " agent-name))))

(defun agent-shell--format-buffer-name (agent-name project-name)
  "Format `agent-shell' buffer name using AGENT-NAME and PROJECT-NAME.

For example, with the default format:

  (agent-shell--format-buffer-name \"Claude\" \"agent-shell\")
    => \"Claude Agent @ agent-shell\""
  (if (functionp agent-shell-buffer-name-format)
      (funcall agent-shell-buffer-name-format agent-name project-name)
    (concat (agent-shell--buffer-name-prefix agent-name) project-name)))

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

(cl-defun agent-shell--start (&key config no-focus new-session session-strategy session-id fork-session-id outgoing-request-decorator)
  "Programmatically start shell with CONFIG.

See `agent-shell-make-agent-config' for config format.

Set NO-FOCUS to start in background.
Set NEW-SESSION to start a separate new session.
SESSION-STRATEGY overrides `agent-shell-session-strategy' buffer-locally.
SESSION-ID resumes an existing session by its id string.
FORK-SESSION-ID forks an existing session by its id string.
OUTGOING-REQUEST-DECORATOR is passed through to `acp-make-client'."
  (unless (version<= "0.91.2" shell-maker-version)
    (error "Please update shell-maker to version 0.91.2 or newer"))
  (unless (version<= "0.13.1" acp-package-version)
    (error "Please update acp.el to version 0.13.1 or newer"))
  (when (boundp 'agent-shell--transcript-file-path-function)
    (user-error "'agent-shell--transcript-file-path-function is retired.

Please use 'agent-shell-transcript-file-path-function and unbind old
variable (see makunbound)"))
  (when agent-shell-session-restore-strategy
    (user-error "Please migrate agent-shell-session-restore-strategy to agent-shell-session-restore-verbosity"))
  (agent-shell--validate-session-strategy
   (or session-strategy agent-shell-session-strategy))
  (let* ((shell-maker-config (agent-shell--make-shell-maker-config
                              :prompt (map-elt config :shell-prompt)
                              :prompt-regexp (map-elt config :shell-prompt-regexp)))
         (agent-shell--shell-maker-config shell-maker-config)
         (default-directory (agent-shell-cwd))
         (shell-buffer
          ;; Suppress mode hook during shell-maker-start since
          ;; agent-shell state isn't ready yet.
          ;;
          ;; Fire it below once state is fully initialised.
          (let ((agent-shell-mode-hook nil))
            (shell-maker-start-v2
             :config agent-shell--shell-maker-config
             :no-focus t  ;; Always use no-focus, handle display below
             :welcome-function nil ;; Defer showing welcome text
             :new-session new-session
             :buffer-name (agent-shell--format-buffer-name (map-elt config :buffer-name) (agent-shell--project-name))
             :mode-line-name (map-elt config :mode-line-name)
             ;; Skip shell-maker's per-start command aliasing so it does not
             ;; alias `agent-shell-submit' internally.  We alias
             ;; agent-shell.el defines its own `agent-shell-submit'.
             :alias-commands nil))))
    ;; While sending the first prompt request would already validate
    ;; finding the ACP agent executable, users have to wait until they
    ;; type a prompt and send it, only to find out that they are missing
    ;; the agent executable. This leaves them with an unsuable shell.
    ;; Better to check on shell creation and bail early (leaving no
    ;; shell behind).
    (with-current-buffer shell-buffer
      ;; Apply dir-local variables in agent-shell buffer
      (hack-dir-local-variables-non-file-buffer)
      ;; Set minimal buffer-local state initialization so `agent-shell-get-config' is available.
      (setq-local agent-shell--state (agent-shell--make-state :agent-config config))
      (unless (and (map-elt config :client-maker)
                   (funcall (map-elt config :client-maker) (current-buffer)))
        (kill-buffer shell-buffer)
        (error "No way to create a new client"))
      (let ((command (map-elt (funcall (map-elt config :client-maker) (current-buffer)) :command)))
        (unless (executable-find command t)
          (kill-buffer shell-buffer)
          (error "%s" (agent-shell--make-missing-executable-error
                       :executable command
                       :install-instructions (map-elt config :install-instructions)))))
      ;; Initialize full buffer-local state (replaces the minimal one above).
      (setq-local agent-shell--state (agent-shell--make-state
                                      :buffer shell-buffer
                                      :heartbeat (agent-shell-heartbeat-make
                                                  :on-heartbeat
                                                  (lambda (_heartbeat status)
                                                    ;; 'ended is the final tick; render
                                                    ;; even if off-screen ensures hidden.
                                                    (when (or (eq status 'ended)
                                                              (get-buffer-window shell-buffer t))
                                                      (with-current-buffer shell-buffer
                                                        (agent-shell--update-header-and-mode-line
                                                         :cache-enabled (eq status 'busy))))
                                                    ;; 'ended is the final tick; render even
                                                    ;; if off-screen to ensure animation is hidden.
                                                    (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                                                                                  :shell-buffer shell-buffer
                                                                                  :existing-only t))
                                                                ;; 'ended is the final tick; render even
                                                                ;; if off-screen to ensure animation is hidden.
                                                                ((or (eq status 'ended)
                                                                     (get-buffer-window viewport-buffer t))))
                                                      (with-current-buffer viewport-buffer
                                                        (agent-shell-viewport--update-header)))))
                                      :client-maker (map-elt config :client-maker)
                                      :needs-authentication (map-elt config :needs-authentication)
                                      :authenticate-request-maker (map-elt config :authenticate-request-maker)
                                      :outgoing-request-decorator (or outgoing-request-decorator
                                                                      agent-shell-outgoing-request-decorator)
                                      :agent-config config))
      ;; Initialize buffer-local shell-maker-config
      (setq-local agent-shell--shell-maker-config shell-maker-config)
      (setq-local filter-buffer-substring-function #'agent-shell--filter-buffer-substring)
      (agent-shell--update-header-and-mode-line)
      (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t)
      (add-hook 'change-major-mode-hook #'agent-shell--clean-up nil t)
      (add-hook 'window-configuration-change-hook #'agent-shell--resize-header nil t)
      (agent-shell-ui-mode +1)
      (add-hook 'agent-shell-ui-post-expand-fragment-at-point-hook
                #'agent-shell--render-markdown nil t)
      (when agent-shell-file-completion-enabled
        (agent-shell-completion-mode +1))
      (agent-shell--setup-modeline)
      (setq-local agent-shell--transcript-file (agent-shell--transcript-file-path))
      ;; We disabled aliasing comint/shell-maker commands
      ;; See `shell-maker-start-v2' above with :alias-commands nil
      ;; Manually alias as needed.
      (defalias 'agent-shell-clear-buffer #'shell-maker-clear-buffer)
      (defalias 'agent-shell-previous-input #'comint-previous-input)
      (defalias 'agent-shell-next-input #'comint-next-input)
      (defalias 'agent-shell-search-history #'shell-maker-search-history)
      (defalias 'agent-shell-newline #'newline)
      (defalias 'agent-shell-rename-buffer #'shell-maker-rename-buffer)
      (defalias 'agent-shell-delete-interaction-at-point #'shell-maker-delete-interaction-at-point)
      (if agent-shell--transcript-file
          ;; Prefer agent-shell--transcript-file over shell-maker's own
          ;; transcript capability, so don't prompt to save on kill.
          (setq-local shell-maker-prompt-before-killing-buffer nil)
        ;; Fall back to shell-maker's transcript.
        (defalias 'agent-shell-save-session-transcript #'shell-maker-save-session-transcript))
      (when session-id
        (map-put! agent-shell--state :resume-session-id session-id))
      (when fork-session-id
        (map-put! agent-shell--state :fork-session-id fork-session-id))
      ;; Snapshot the strategy also in case it was dynamically re-bound
      (setq-local agent-shell-session-strategy
                  (or session-strategy agent-shell-session-strategy))
      ;; Show deferred welcome text,
      ;; but first wipe buffer content.
      (let ((inhibit-read-only t))
        (erase-buffer))
      (set-marker (process-mark (shell-maker--process)) (point-max))
      (when (and agent-shell-show-welcome-message
                 (map-elt config :welcome-function))
        (shell-maker-write-output
         :config shell-maker--config
         :output (funcall (map-elt config :welcome-function)
                          shell-maker--config)))
      ;; TODO: Remove all `new-deferred' code paths.
      ;; The value was removed from `agent-shell-session-strategy' in 0.55.1
      ;; (see `agent-shell--validate-session-strategy'), but the branches
      ;; were left behind temporarily.  Sites to clean up: this branch,
      ;; the `restart' branch (`strategy' let-binding),
      ;; the prompt-readiness guards in `agent-shell--handle' and
      ;; `agent-shell-viewport-compose-send', `agent-shell--shell-buffer'
      ;; (the deferred picker), `agent-shell--initiate-session-list-and-load',
      ;; the `session-strategy' check in `agent-shell--start-acp-session',
      ;; the `prompt'/`new'/`latest' subscriptions below, and
      ;; `agent-shell--insert-to-shell-buffer'.
      ;; Show the prompt immediately, before bootstrapping, so shell
      ;; always has a prompt to type into regardless of strategy.
      (shell-maker-finish-output :config shell-maker--config :success nil)
      ;; Land point on the freshly shown prompt (see #668).  Later context
      ;; insertion / user edits move it from here; bootstrapping does not.
      (goto-char (point-max))
      ;; Kick off ACP session bootstrapping.  `new-deferred' (deprecated)
      ;; defers this until the first prompt is sent.
      (unless (eq agent-shell-session-strategy 'new-deferred)
        (agent-shell--handle :shell-buffer shell-buffer))
      (when (and agent-shell-chat-mode-enabled (not agent-shell-chat-mode))
        (agent-shell-chat-mode 1))
      ;; State should be available after kicking off
      ;; `agent-shell--handle'.  Fire mode hook so initial
      ;; state is available to agent-shell-mode-hook(s).
      (run-hooks 'agent-shell-mode-hook)
      ;; Refresh the session title from the agent. `init-finished' fires
      ;; once the session is established (covers resumed sessions whose
      ;; title is already known) and `turn-complete' covers ongoing
      ;; refinement for agents that summarize as the conversation grows.
      ;; `session-selected' is too early -- it fires synchronously inside
      ;; `agent-shell--handle' before this subscription can register.
      (agent-shell-subscribe-to
       :shell-buffer shell-buffer
       :event 'init-finished
       :on-event #'agent-shell--refresh-session-title)
      (agent-shell-subscribe-to
       :shell-buffer shell-buffer
       :event 'turn-complete
       :on-event #'agent-shell--refresh-session-title)
      ;; Subscribe to session selection events (needed regardless of focus).
      (when (eq agent-shell-session-strategy 'prompt)
        (agent-shell-subscribe-to
         :shell-buffer shell-buffer
         :event 'session-selection-cancelled
         :on-event (lambda (_event)
                     (kill-buffer shell-buffer)))
        (let ((active-message (agent-shell-active-message-show :text "Loading...")))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'session-prompt
           :on-event (lambda (_event)
                       (agent-shell-active-message-hide :active-message active-message)))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'session-selected
           :on-event (lambda (_event)
                       (agent-shell-active-message-hide :active-message active-message)))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'session-selection-cancelled
           :on-event (lambda (_event)
                       (agent-shell-active-message-hide :active-message active-message)))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'error
           :on-event (lambda (_event)
                       (agent-shell-active-message-hide :active-message active-message)))
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'clean-up
           :on-event (lambda (_event)
                       (agent-shell-active-message-hide :active-message active-message)))))
      ;; Display buffer if no-focus was nil, respecting agent-shell-display-action
      (unless no-focus
        (if (eq agent-shell-session-strategy 'prompt)
            ;; Defer display until user selects a session.
            ;; Why? The experience is janky to display a buffer
            ;; and soon after that prompt the user for input.
            ;; Better to prompt the user for input and then
            ;; display the buffer.
            (agent-shell-subscribe-to
             :shell-buffer shell-buffer
             :event 'session-selected
             :on-event (lambda (_event)
                         (agent-shell--display-buffer shell-buffer)))
          (agent-shell--display-buffer shell-buffer))))
    shell-buffer))

(cl-defun agent-shell--delete-fragment (&key state block-id)
  "Delete fragment with STATE and BLOCK-ID."
  (when-let* (((map-elt state :buffer))
              (viewport-buffer (agent-shell-viewport--buffer
                                :shell-buffer (map-elt state :buffer)
                                :existing-only t))
              ;; Fragment deletion only makes sense when viewport is
              ;; displaying conversation, never while it's an active compose buffer.
              ((with-current-buffer viewport-buffer
                 (derived-mode-p 'agent-shell-viewport-view-mode))))
    (with-current-buffer viewport-buffer
      (agent-shell-ui-delete-fragment :namespace-id (map-elt state :request-count) :block-id block-id :no-undo t)))
  (with-current-buffer (map-elt state :buffer)
    (unless (and (derived-mode-p 'agent-shell-mode)
                 (equal (current-buffer)
                        (map-elt state :buffer)))
      (error "Editing the wrong buffer: %s" (current-buffer)))
    (agent-shell-ui-delete-fragment :namespace-id (map-elt state :request-count) :block-id block-id :no-undo t)))

(cl-defun agent-shell--collapse-fragment-group (&key state namespace-id block-id)
  "Collapse group header BLOCK-ID under NAMESPACE-ID in STATE's buffers.

Mirrors `agent-shell--delete-fragment', applying to both the shell buffer
and, when it is displaying the conversation, its viewport buffer.  Does
nothing when BLOCK-ID names no rendered group header."
  (when-let* (((map-elt state :buffer))
              (viewport-buffer (agent-shell-viewport--buffer
                                :shell-buffer (map-elt state :buffer)
                                :existing-only t))
              ;; Folding only makes sense when viewport is displaying
              ;; conversation, never while it's an active compose buffer.
              ((with-current-buffer viewport-buffer
                 (derived-mode-p 'agent-shell-viewport-view-mode))))
    (with-current-buffer viewport-buffer
      (agent-shell-ui-set-group-collapsed-by-id
       :namespace-id namespace-id :block-id block-id :collapsed t :no-undo t)))
  (when-let* ((shell-buffer (map-elt state :buffer))
              ((buffer-live-p shell-buffer)))
    (with-current-buffer shell-buffer
      (agent-shell-ui-set-group-collapsed-by-id
       :namespace-id namespace-id :block-id block-id :collapsed t :no-undo t))))

(defun agent-shell--live-input-prompt-p (prompt)
  "Non-nil when PROMPT is a live input prompt at the end of the buffer.
PROMPT is a `comint-last-prompt' cons of (start . end) markers.  It's
live when nothing follows it (empty input area) or when everything
between its end and `point-max' is user input rather than agent output.
This tells a real prompt awaiting input, possibly with unsubmitted typed
text, apart from a stale prompt left mid-buffer while output streams
below it (where `comint-last-prompt' still points at the previous
prompt).  Output carries a `field' of `output'; typed input does not."
  (let ((end (marker-position (cdr prompt)))
        (max (point-max)))
    ;; When narrowed above the prompt, `end' sits past the accessible
    ;; `point-max' and `text-property-any' would get inverted bounds.
    ;; Treat that as not-live so callers fall back to inserting at the
    ;; narrowed `point-max' (still above the prompt).
    (and (<= end max)
         (or (= end max)
             (not (text-property-any end max 'field 'output))))))

(defun agent-shell--reset-undo-history ()
  "Reset `buffer-undo-list' to undo the active prompt's input only.

Undo entries hold absolute buffer positions, which Emacs does not adjust
when text lands elsewhere in the buffer.  Rendering above the active
prompt (a replayed session or a notification arriving out of turn)
pushes unsubmitted input down, leaving every entry recorded for it
pointing at the wrong text: undoing would delete a stretch of agent
output instead of what was typed.

Everything but the active prompt is either agent output or input frozen
on submission, so drop the history and re-record the input area as a
single insertion.  With `Claude> hello' ending the buffer and `hello'
spanning 30 to 35, leaves `buffer-undo-list' as ((30 . 35)).  Leaves it
empty when no input is pending.

No-op where undo is disabled, either by the buffer (`buffer-disable-undo')
or by a caller rendering under a `buffer-undo-list' bound to t."
  (unless (eq buffer-undo-list t)
    (setq buffer-undo-list
          (when-let* ((prompt comint-last-prompt)
                      ((agent-shell--live-input-prompt-p prompt))
                      (input-start (marker-position (cdr prompt)))
                      ((< input-start (point-max))))
            (list (cons input-start (point-max)))))))

(cl-defun agent-shell--update-bootstrapping-fragment (&rest args)
  "Update a `bootstrapping'-namespace fragment above the shell prompt.

Forwards ARGS to `agent-shell--update-fragment', pinning the fragment
to the `bootstrapping' namespace and landing it above the live prompt.
Keeps session-initialization status above the prompt so it does not
disturb any type-ahead the user entered while the shell bootstraps."
  (apply #'agent-shell--update-fragment
         :namespace-id "bootstrapping"
         :above-last-prompt t
         args))

(defun agent-shell--tag-untagged-output (start end)
  "Mark chars in [START, END) as `field' `output', skipping tagged ones.

Equivalent to `add-text-properties' with `(field output)' over the whole
range, but starts at the first char that isn't tagged yet.  A streamed
block is re-tagged on every chunk, and `add-text-properties' signals a
buffer modification spanning the entire range it was handed as soon as a
single char in it needs the property.  With the whole block as the range,
that means `jit-lock-after-change' marks the whole (by then multi-megabyte)
block unfontified once per chunk, and redisplay refontifies it, the top
entry in the profiles on issue #757.  Tagging only the untagged tail keeps
the reported range down to the newly inserted chars."
  (when-let* ((untagged (text-property-not-all start end 'field 'output)))
    (add-text-properties untagged end '(field output))))

(cl-defun agent-shell--update-fragment (&key state namespace-id block-id label-left label-right
                                             body append create-new navigation expanded
                                             render-body-images above-last-prompt
                                             group-id group-label (group-expanded t))
  "Update fragment in the shell buffer.

Creates or updates existing dialog using STATE's request count as namespace
unless NAMESPACE-ID (rarely needed).  Rely on count is possible.

BLOCK-ID uniquely identifies the block.

Dialog can have LABEL-LEFT, LABEL-RIGHT, and BODY.

Optional flags: APPEND text to existing content, CREATE-NEW block,
NAVIGATION for navigation style, EXPANDED to show block expanded
by default, RENDER-BODY-IMAGES to enable inline image rendering in
body, ABOVE-LAST-PROMPT to land content above the active prompt
instead of after it (typical for notifications arriving out of
turn).  Programmatic fragment updates do not enter undo history.

GROUP-ID nests this block under a collapsible group header, materialized
from GROUP-LABEL on first use (see `agent-shell-ui-make-fragment-model'),
with GROUP-EXPANDED as the group's initial fold state."
  (when label-right
    (setq label-right (string-trim label-right)))
  ;; Convert non-standard multiline single-backtick code spans to fenced
  ;; code blocks so the markdown renderer can recognize them as source
  ;; blocks, but only for labels that start with `.
  (when (and label-right
             (not (string-match-p (rx "```") label-right))
             (string-match-p
              (rx "`" (zero-or-more (not (any "\n`")))
                  "\n")
              label-right))
    (setq label-right
          (replace-regexp-in-string
           (rx "`"
               (group (zero-or-more (not (any "\n`"))) "\n"
                      (*? (seq (zero-or-more (not (any "\n"))) "\n"))
                      (zero-or-more (not (any "\n`"))))
               "`")
           "Snippet\n\n```\n\\1\n```\n"
           label-right)))
  (when-let* (((map-elt state :buffer))
              (viewport-buffer (agent-shell-viewport--buffer
                                :shell-buffer (map-elt state :buffer)
                                :existing-only t))
              ((with-current-buffer viewport-buffer
                 (derived-mode-p 'agent-shell-viewport-view-mode))))
    (with-current-buffer viewport-buffer
      (let ((buffer-undo-list t)
            (inhibit-read-only t)
            (auto-scroll (shell-maker--should-auto-scroll-p)))
        (when-let* ((range (agent-shell-ui-update-fragment
                            (agent-shell-ui-make-fragment-model
                             :namespace-id (or namespace-id
                                               (map-elt state :request-count))
                             :block-id block-id
                             :label-left label-left
                             :label-right label-right
                             :body body
                             :group-id group-id
                             :group-label group-label
                             :group-expanded group-expanded)
                            :navigation navigation
                            :append append
                            :create-new create-new
                            :expanded expanded
                            :no-undo t))
                    (padding-start (map-nested-elt range '(:padding :start)))
                    (padding-end (map-nested-elt range '(:padding :end)))
                    (block-start (map-nested-elt range '(:block :start)))
                    (block-end (map-nested-elt range '(:block :end))))
          ;; Restore point after narrowing to prevent scrolling
          (save-excursion
            ;; Apply markdown to body.
            (save-restriction
              (when-let* ((body-start (map-nested-elt range '(:body :start)))
                          (body-end (map-nested-elt range '(:body :end))))
                (narrow-to-region body-start body-end)
                ;; Skip rendering when body is collapsed; it will be
                ;; rendered on expand via
                ;; `agent-shell-ui-post-expand-fragment-at-point-hook'.
                (unless (agent-shell-ui--body-invisible-p (point-min) (point-max))
                  (agent-shell--render-markdown :render-images render-body-images))))
            ;; Note: For now, we're skipping applying markdown
            ;; on left labels as they currently carry propertized text
            ;; for statuses (ie. boxed).
            ;;
            ;; Apply markdown to right label.
            (save-restriction
              (when-let* ((label-right-start (map-nested-elt range '(:label-right :start)))
                          (label-right-end (map-nested-elt range '(:label-right :end))))
                (narrow-to-region label-right-start label-right-end)
                (agent-shell--render-markdown :render-images nil
                                              :external-renderers nil))))
          (when auto-scroll
            (goto-char (point-max)))))))
  (with-current-buffer (map-elt state :buffer)
    (unless (and (derived-mode-p 'agent-shell-mode)
                 (equal (current-buffer)
                        (map-elt state :buffer)))
      (error "Editing the wrong buffer: %s" (current-buffer)))
    (let* ((buffer-undo-list t)
           (window (get-buffer-window (current-buffer)))
           (auto-scroll (eobp))
           ;; Use a marker to ensure point restoration
           ;; lands point after the inserted text.
           (saved-point (copy-marker (point)))
           (saved-mark (mark t))
           (saved-mark-active mark-active)
           (saved-window-start (and window (window-start window)))
           ;; Caller is asking us to land content above the active
           ;; prompt (typical for notifications arriving after
           ;; `end_turn').  Narrow above the prompt so the fragment
           ;; system inserts there, and flip the prompt-start marker's
           ;; insertion-type so it advances past the new text rather
           ;; than ending up stranded inside it.  Anchor on the
           ;; prompt-start so unsubmitted typed input is pushed down with
           ;; the prompt.  Falls back to the normal in-line path when no
           ;; live input prompt sits at the buffer end.
           (late-prompt-start (and above-last-prompt
                                   comint-last-prompt
                                   (marker-position (car comint-last-prompt))
                                   (agent-shell--live-input-prompt-p comint-last-prompt)
                                   (car comint-last-prompt)))
           (orig-insertion-type (and late-prompt-start
                                     (marker-insertion-type late-prompt-start))))
      (when late-prompt-start
        (set-marker-insertion-type late-prompt-start t))
      (unwind-protect
       (save-restriction
        (when late-prompt-start
          (narrow-to-region (point-min) (marker-position late-prompt-start)))
        (shell-maker-with-auto-scroll-edit
         (when-let* ((range (agent-shell-ui-update-fragment
                             (agent-shell-ui-make-fragment-model
                              :namespace-id (or namespace-id
                                                (map-elt state :request-count))
                              :block-id block-id
                              :label-left label-left
                              :label-right label-right
                              :body body
                              :group-id group-id
                              :group-label group-label
                              :group-expanded group-expanded)
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
             ;; Marking as field output to avoid false positives in
             ;; `agent-shell-next-item' and `agent-shell-previous-item'.
             (agent-shell--tag-untagged-output (or padding-start block-start)
                                               (or padding-end block-end))
             ;; Same for group header (mark as field output).
             (when (map-elt range :group-header)
               (agent-shell--tag-untagged-output
                (map-nested-elt range '(:group-header :start))
                (map-nested-elt range '(:group-header :end))))
             ;; Apply markdown to body.  `inhibit-read-only' must
             ;; wrap the render call too — chars in the body carry
             ;; `read-only t' from `agent-shell-ui--insert-fragment',
             ;; and `agent-shell-markdown' modifies buffer chars
             ;; (unlike the overlay renderer which only adds overlays).
             (when-let* ((body-start (map-nested-elt range '(:body :start)))
                         (body-end (map-nested-elt range '(:body :end))))
               (narrow-to-region body-start body-end)
               ;; Skip rendering when body is collapsed; it will be
               ;; rendered on expand via
               ;; `agent-shell-ui-post-expand-fragment-at-point-hook'.
               (unless (agent-shell-ui--body-invisible-p (point-min) (point-max))
                 (agent-shell--render-markdown))
               (widen))
             ;;
             ;; Note: For now, we're skipping applying markdown
             ;; on left labels as they currently carry propertized text
             ;; for statuses (ie. boxed).
             ;;
             ;; Apply markdown to right label.
             (when-let* ((label-right-start (map-nested-elt range '(:label-right :start)))
                         (label-right-end (map-nested-elt range '(:label-right :end))))
               (narrow-to-region label-right-start label-right-end)
               (agent-shell--render-markdown :render-images nil
                                             :external-renderers nil)
               (widen))))
         (run-hook-with-args 'agent-shell-section-functions range))))
       (when late-prompt-start
         (set-marker-insertion-type late-prompt-start orig-insertion-type)))
      ;; Late-arrival inserts run under a narrow that ends at
      ;; `comint-last-prompt'.  The auto-scroll branch of
      ;; `shell-maker-with-auto-scroll-edit' goes to the narrowed
      ;; `point-max' (= prompt-start position), leaving point stranded
      ;; on the prompt's first char after the narrowing is dropped.
      ;; When the user was at absolute eob (i.e. in the input area),
      ;; restore them there instead.
      (when (and late-prompt-start auto-scroll)
        (goto-char (point-max)))
      (unless auto-scroll
        (goto-char saved-point)
        (when saved-mark
          (set-marker (mark-marker) saved-mark))
        (setq mark-active saved-mark-active)
        (when window
          (set-window-start window saved-window-start t)))
      (set-marker saved-point nil))
    ;; Rendering above the prompt pushed any unsubmitted input down, so the
    ;; undo entries recorded for it now point at the shifted-down text.
    ;; Runs outside the `buffer-undo-list' binding above, which would
    ;; otherwise swallow the reset.
    (when above-last-prompt
      (agent-shell--reset-undo-history))))

(cl-defun agent-shell--update-text (&key state namespace-id block-id text append create-new)
  "Update plain text entry in the shell buffer.

Uses STATE's request count as namespace unless NAMESPACE-ID is given.
BLOCK-ID uniquely identifies the entry.
TEXT is the string to insert or append.
APPEND and CREATE-NEW control update behavior."
  (let ((ns (or namespace-id (map-elt state :request-count))))
    (when-let* (((map-elt state :buffer))
                (viewport-buffer (agent-shell-viewport--buffer
                                  :shell-buffer (map-elt state :buffer)
                                  :existing-only t))
                ((with-current-buffer viewport-buffer
                   (derived-mode-p 'agent-shell-viewport-view-mode))))
      (with-current-buffer viewport-buffer
        (let ((inhibit-read-only t))
          (agent-shell-ui-update-text
           :namespace-id ns
           :block-id block-id
           :text text
           :append append
           :create-new create-new
           :no-undo t))))
    (with-current-buffer (map-elt state :buffer)
      (shell-maker-with-auto-scroll-edit
       (agent-shell-ui-update-text
        :namespace-id ns
        :block-id block-id
        :text text
        :append append
        :create-new create-new
        :no-undo t)))))

(defun agent-shell-toggle-logging ()
  "Toggle logging."
  (declare (modes agent-shell-mode))
  (interactive)
  (setq acp-logging-enabled (not acp-logging-enabled))
  (message "Logging: %s" (if acp-logging-enabled "ON" "OFF")))

(defun agent-shell-reset-logs ()
  "Reset all log buffers."
  (declare (modes agent-shell-mode))
  (interactive)
  (acp-reset-logs :client (map-elt (agent-shell--state) :client))
  (message "Logs reset"))

(defun agent-shell-next-item (&optional leave-table)
  "Go to next item.

Could be a prompt, an expandable item, a displayed image, a rendered
link, a source block, or a rendered markdown table.  A table is entered
at its first cell, then walked one cell (and one link inside a cell) at
a time, moving on to the item after it once past its last one.

With prefix LEAVE-TABLE, carry on from the end of the table point is
in, landing on the item after it rather than on its next cell.  A wide
table is many items to walk, and outside one there's nothing to leave,
so the prefix does nothing there: a table is always entered
deliberately, on its first cell.

If point is at the input prompt and a character key was pressed,
insert the character instead."
  (declare (modes agent-shell-mode))
  (interactive "P")
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (cond
   ;; Check if at prompt and inserting a character
   ;; (Ignore special keys like TAB/Shift-TAB).
   ((agent-shell--typing-at-prompt-p)
    ;; At prompt, insert character.
    (self-insert-command 1))
   (t
    ;; Otherwise navigate, from the end of the table point is leaving
    ;; when there's a prefix: its own cells and the links in them are
    ;; behind the search from there, so it lands past the table.
    (when-let* ((leave-table)
                (table (agent-shell-markdown-table--region-at-point)))
      (goto-char (cdr table)))
    (let* ((current-pos (point))
           (prompt-pos (save-mark-and-excursion
                         (when (comint-next-prompt 1)
                           (point))))
           (block-pos (save-mark-and-excursion
                        (agent-shell-ui-forward-block)))
           (button-pos (save-mark-and-excursion
                         (agent-shell-next-permission-button)))
           (image-pos (save-mark-and-excursion
                        (agent-shell-markdown--next-visible-image)))
           (link-pos (save-mark-and-excursion
                       (agent-shell-markdown--next-visible-link)))
           (source-block-pos (save-mark-and-excursion
                               (agent-shell-markdown--next-visible-source-block)))
           (table-pos (save-mark-and-excursion
                        (agent-shell-markdown--search-visible
                         :property 'agent-shell-markdown-table-cell-start)))
           (positions (seq-filter (lambda (position)
                                    (> position current-pos))
                                  (seq-map (lambda (position)
                                             (agent-shell-markdown-table--entry-position
                                              :position position :from current-pos))
                                           (delq nil (list prompt-pos
                                                           block-pos
                                                           button-pos
                                                           image-pos
                                                           link-pos
                                                           source-block-pos
                                                           table-pos)))))
           (next-pos (when positions
                       (seq-min positions))))
      (when next-pos
        (deactivate-mark)
        (goto-char next-pos)
        (when (eq next-pos prompt-pos)
          (comint-skip-prompt)))))))

(defun agent-shell-previous-item (&optional leave-table)
  "Go to previous item.

Could be a prompt, an expandable item, a displayed image, a rendered
link, a source block, or a rendered markdown table.  A table above is
entered at its first cell, so navigating again from there leaves it for
the item above it.

With prefix LEAVE-TABLE, carry on from the start of the table point is
in, as `agent-shell-next-item' does from its end.

If point is at the input prompt and a character key was pressed,
insert the character instead."
  (declare (modes agent-shell-mode))
  (interactive "P")
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (cond
   ;; Check if at prompt and inserting a character
   ;; (Ignore special keys like TAB/Shift-TAB).
   ((agent-shell--typing-at-prompt-p)
    ;; At prompt, insert character.
    (self-insert-command 1))
   (t
    ;; Otherwise navigate, from the start of the table point is leaving
    ;; when there's a prefix, as `agent-shell-next-item' does from its
    ;; end.
    (when-let* ((leave-table)
                (table (agent-shell-markdown-table--region-at-point)))
      (goto-char (car table)))
    (let* ((current-pos (point))
           (prompt-pos (save-mark-and-excursion
                         (when (comint-next-prompt (- 1))
                           (point))))
           (block-pos (save-mark-and-excursion
                        (agent-shell-ui-backward-block)))
           (button-pos (save-mark-and-excursion
                         (agent-shell-previous-permission-button)))
           (image-pos (save-mark-and-excursion
                        (agent-shell-markdown--previous-visible-image)))
           (link-pos (save-mark-and-excursion
                       (agent-shell-markdown--previous-visible-link)))
           (source-block-pos
            (save-mark-and-excursion
              (agent-shell-markdown--previous-visible-source-block)))
           (table-pos (save-mark-and-excursion
                        (agent-shell-markdown--search-visible
                         :property 'agent-shell-markdown-table-cell-start
                         :backwards t)))
           (positions (seq-filter (lambda (position)
                                    (< position current-pos))
                                  (seq-map (lambda (position)
                                             (agent-shell-markdown-table--entry-position
                                              :position position :from current-pos))
                                           (delq nil (list prompt-pos
                                                           block-pos
                                                           button-pos
                                                           image-pos
                                                           link-pos
                                                           source-block-pos
                                                           table-pos)))))
           (next-pos (when positions
                       (seq-max positions))))
      (when next-pos
        (deactivate-mark)
        (goto-char next-pos)
        (when (eq next-pos prompt-pos)
          (comint-skip-prompt)))))))

(defun agent-shell-backward-up-item ()
  "Go to the start of the item point navigated into.

In a rendered markdown table that's its first cell, whichever cell
`agent-shell-next-item' walked point into, so it is to a table what
`backward-up-list' is to a list.  Anywhere else it runs
`backward-up-list' itself, leaving the key its usual meaning while
composing a prompt."
  (interactive)
  (if-let* ((position (agent-shell-markdown-table--first-cell (point))))
      (goto-char position)
    (call-interactively #'backward-up-list)))

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

(defvar-local agent-shell--header-cache nil
  "Cache for graphical headers (no need for regenerating regularly).

A buffer-local hash table mapping cache keys to header strings.")

(defvar-local agent-shell--header-last-model nil
  "Header model from the last full header update.

Reused by heartbeat ticks, refreshing only the animated frame, so the
model (project lookup, context usage, model/mode names, menu keys,
frame metrics, ...) is not rebuilt on every beat.")


(defun agent-shell--session-id-indicator ()
  "Return a propertized session ID string, or nil if unavailable or disabled."
  (when-let* ((agent-shell-show-session-id)
              (session-id (map-nested-elt (agent-shell--state) '(:session :id)))
              ((not (string-empty-p session-id))))
    (propertize session-id 'font-lock-face 'agent-shell-session-id)))

(defun agent-shell--face-foreground (face)
  "Return the foreground color for FACE, walking `:inherit' chains.
FACE may be a face name symbol, an anonymous face plist (e.g. \\='(:foreground
\"red\")), or a list of either.  Returns the color string or nil when
unspecified."
  (cond
   ((null face) nil)
   ((and (listp face) (keywordp (car face)))
    (let ((val (plist-get face :foreground)))
      (if (and (stringp val) (not (string= val "unspecified")))
          val
        (agent-shell--face-foreground (plist-get face :inherit)))))
   ((listp face)
    (seq-some #'agent-shell--face-foreground face))
   ((symbolp face)
    (let ((val (face-attribute face :foreground nil t)))
      (when (stringp val) val)))))

(defun agent-shell--svg-fill-color (face)
  "Return foreground color for FACE as an `#rrggbb' hex string for SVG.
Resolves FACE's `:inherit' chain and falls back to the `default' face
so the result is always specified, then converts to hex.  The hex
form is important because Emacs face foregrounds are often X11 color
names (e.g., `Green3' for the standard `success' face) that SVG does
not recognize — passing them through unconverted causes the renderer
to fall back to black."
  (let* ((name (or (agent-shell--face-foreground face)
                   (face-attribute 'default :foreground)))
         (rgb (and (stringp name) (color-name-to-rgb name))))
    (if rgb
        (apply #'color-rgb-to-hex (append rgb '(2)))
      "#ffffff")))

(defun agent-shell--header-width (buffer)
  "Return the pixel width the header should be rendered at for BUFFER.

The header image is sized to the widest window showing BUFFER rather than
to the frame.  Nothing is drawn in the padding to the right of the header
text (the image has no background), so a narrower image looks identical
while rasterizing fewer pixels every time the busy indicator animates.  A
window narrower than the header text clips it exactly as the frame-wide
image already did.

Falls back to the frame width when BUFFER is not displayed, which is also
the safe upper bound.

For a buffer shown in two side-by-side windows of 740 and 742 pixels,
returns 742.  For the same buffer in no window, on a 1482 pixel frame,
returns 1482."
  (if-let* ((windows (get-buffer-window-list buffer nil t)))
      (seq-max (seq-map #'window-pixel-width windows))
    (frame-pixel-width)))

(defun agent-shell--svg-text-width (node)
  "Return the pixel width of header `text' NODE's tspans.

Sums each tspan's text width and the `dx' gap preceding it.

Exact rather than estimated: the header SVG names the same font family
and pixel size Emacs is using, so `string-pixel-width' measures what
librsvg will draw.

For example, with \"Claude\" measuring 60 pixels and \"➤\" 15:

  (agent-shell--svg-text-width
   (dom-node \\='text \\='((x . \"79\"))
             (dom-node \\='tspan nil \"Claude\")
             (dom-node \\='tspan \\='((dx . \"8\")) \"➤\")))
  => 83"
  (seq-reduce (lambda (total child)
                (if (and (consp child) (eq (dom-tag child) 'tspan))
                    (+ total
                       (string-to-number (format "%s" (or (dom-attr child 'dx) 0)))
                       (string-pixel-width (or (car (dom-children child)) "")))
                  total))
              (dom-children node)
              0))

(defun agent-shell--svg-content-width (svg)
  "Return the pixel width SVG's text rows reach.

The widest row wins, each measured from its own `x' offset, so the result
is where the rightmost drawn text ends.

For example, an SVG whose top row starts at x 79 and measures 634, and
whose bottom row starts at x 79 and measures 168, returns 713."
  (seq-reduce (lambda (widest node)
                (max widest (+ (string-to-number (format "%s" (dom-attr node 'x)))
                               (agent-shell--svg-text-width node))))
              (dom-by-tag svg 'text)
              0))

(defun agent-shell--resize-header ()
  "Re-render the header when the width it was rendered at no longer applies.

The header image is sized to the window (see `agent-shell--header-width'),
so text too long for the window is clipped by the image itself.  Widening
the window, typically by deleting the one beside it, has to rebuild the
image or that clipping would persist in the wider window.

On `window-configuration-change-hook', so keep the common case, where the
width is unchanged, down to a comparison."
  (when (and (derived-mode-p 'agent-shell-mode)
             agent-shell--header-last-model
             (/= (map-elt agent-shell--header-last-model :width)
                 (agent-shell--header-width (current-buffer))))
    (agent-shell--update-header-and-mode-line)))

(cl-defun agent-shell--make-header-model (state &key position status key-hints menu-keys width)
  "Create a header model alist from STATE and the given header fields.
The model contains all inputs needed to render the header.  POSITION,
STATUS, KEY-HINTS and MENU-KEYS are as documented in
`agent-shell--make-header'.  WIDTH is the pixel width to render at,
defaulting to the frame width."
  `((:buffer-name . ,(map-nested-elt state '(:agent-config :buffer-name)))
    (:icon-name . ,(map-nested-elt state '(:agent-config :icon-name)))
    (:model-id . ,(map-nested-elt state '(:session :model-id)))
    (:model-name . ,(agent-shell-get-model-name state))
    (:thought-level-id . ,(agent-shell--current-thought-level-id state))
    (:thought-level-name . ,(agent-shell-get-thought-level-name state))
    (:mode-id . ,(map-nested-elt state '(:session :mode-id)))
    (:mode-name . ,(agent-shell-get-mode-name state))
    (:project-name . ,(agent-shell--project-name))
    (:session-id . ,(agent-shell--session-id-indicator))
    (:width . ,(or width (frame-pixel-width)))
    (:font-height . ,(frame-char-height))
    (:font-family . ,(face-attribute 'default :family))
    (:font-size . ,(if-let* (((display-graphic-p))
                             (font (face-attribute 'default :font))
                             ((fontp font))
                             (size (font-get font :size))
                             ((> size 0)))
                       size
                     (frame-char-height)))
    (:background-mode . ,(frame-parameter nil 'background-mode))
    (:context-indicator . ,(agent-shell--context-usage-indicator))
    (:busy-indicator-frame . ,(agent-shell--busy-indicator-frame))
    (:position . ,position)
    (:status . ,status)
    (:key-hints . ,key-hints)
    (:menu-keys . ,menu-keys)))

(defun agent-shell--header-cache-key (model)
  "Generate a cache key from header MODEL.
Joins all values from the model alist."
  (mapconcat (lambda (pair) (format "%s" (cdr pair)))
             model "|"))

(cl-defun agent-shell--make-header (state &key position status key-hints menu-keys)
  "Return header text for current STATE.

STATE should contain :agent-config with :icon-name, :buffer-name, and
:session with :mode-id and :modes for displaying the current session mode.

POSITION: Optional string rendered before the project on the bottom line
\(e.g. \"1/3\").  Honors text-property face for foreground color.

STATUS: Optional string rendered at the end of the bottom line (e.g.
propertize \"Edit\" with `success' face).  Honors text-property face for
foreground color.

KEY-HINTS is a list of alists defining the key hint row to display, each with:
  :key         - Key string (e.g., \"n\")
  :description - Description to display (e.g., \"next hunk\")

MENU-KEYS is an alist mapping each clickable label to the key description
string shown in its help-echo tooltip, each with:
  :model         - Key that opens the model menu
  :mode          - Key that opens the session mode menu
  :thought-level - Key that opens the thought level menu"
  (unless state
    (error "STATE is required"))
  (agent-shell--render-header-model
   (agent-shell--make-header-model state :position position :status status
                                   :key-hints key-hints :menu-keys menu-keys)))

(defun agent-shell--svg-header-geometry (header-model)
  "Return pixel geometry for HEADER-MODEL's SVG header as an alist.

Layout is:

  +------+
  | icon | Top text line
  |      | Bottom text line
  +------+
  Key hints row (optional, last row)

For a 21px char height and 16px font, with no key hints:

  ((:char-height . 21)
   (:font-size . 16)
   (:image-height . 63)
   (:image-width . 63)
   (:text-height . 21)
   (:total-height . 79)
   (:icon-x . 6)
   (:icon-y . 8)
   (:icon-text-x . 79)
   (:icon-text-y . 31)
   (:key-hints-x . 6)
   (:key-hints-y . 79))"
  (let* ((char-height (map-elt header-model :font-height))
         (font-size (map-elt header-model :font-size))
         (has-key-hints (and (map-elt header-model :key-hints) t))
         (image-height (* 3 char-height))
         (text-height char-height)
         (top-padding-height (/ font-size 2))
         (bottom-padding-height (if has-key-hints
                                    (+ text-height top-padding-height)
                                  top-padding-height))
         ;; Match the natural inter-line stride between top and bottom
         ;; text rows so the key hints row sits the same vertical
         ;; distance below the bottom row.
         (row-spacing (if has-key-hints (- char-height font-size) 0))
         (icon-x 6))
    `((:char-height . ,char-height)
      (:font-size . ,font-size)
      (:image-height . ,image-height)
      (:image-width . ,image-height)
      (:text-height . ,text-height)
      (:total-height . ,(+ image-height row-spacing top-padding-height bottom-padding-height))
      (:icon-x . ,icon-x)
      (:icon-y . ,top-padding-height)
      (:icon-text-x . ,(+ icon-x image-height 10))
      (:icon-text-y . ,(+ top-padding-height char-height (/ (- char-height font-size) 2)))
      (:key-hints-x . ,icon-x)
      (:key-hints-y . ,(+ image-height font-size row-spacing)))))

(defun agent-shell--render-header-model (header-model)
  "Render HEADER-MODEL to a header string, caching the result.

HEADER-MODEL is an alist produced by `agent-shell--make-header-model',
holding every input needed to render, including the menu keys.

Rendering builds the whole propertized header string, including three
clickable menu keymaps, so it is expensive relative to a heartbeat tick.
The result is cached in `agent-shell--header-cache' keyed on the model.
During busy animation only the frame glyph varies, so ticks cycle through
a small fixed set of keys and become cache hits after the first animation
cycle.  The cache is cleared on every full header update (see
`agent-shell--update-header-and-mode-line'), which bounds its size and
keeps entries fresh."
  (unless (memq agent-shell-header-style '(none nil))
    (unless agent-shell--header-cache
      (setq agent-shell--header-cache (make-hash-table :test #'equal)))
    (let ((cache-key (agent-shell--header-cache-key header-model)))
      (or (map-elt agent-shell--header-cache cache-key)
          (let ((header (agent-shell--render-header-model-uncached header-model)))
            (map-put! agent-shell--header-cache cache-key header)
            header)))))

(defun agent-shell--render-header-model-uncached (header-model)
  "Render HEADER-MODEL to a header string without caching."
  (let* ((key-hints (map-elt header-model :key-hints))
         (menu-keys (map-elt header-model :menu-keys))
         (model-binding (map-elt menu-keys :model))
         (mode-binding (map-elt menu-keys :mode))
         (thought-level-binding (map-elt menu-keys :thought-level))
         (help-hint (seq-find (lambda (b)
                                (equal (map-elt b :description) "Help"))
                              key-hints))
         (help-chunk (when help-hint
                       (concat (propertize (map-elt help-hint :key)
                                           'face 'agent-shell-key-binding)
                               " "
                               (map-elt help-hint :description))))
         (text-header (format " %s%s%s%s%s ➤ %s%s%s%s%s"
                              (cond
                               ((and (map-elt header-model :position)
                                     (map-elt header-model :status))
                                (concat (map-elt header-model :position) " "
                                        (map-elt header-model :status)
                                        (when help-chunk (concat " " help-chunk))
                                        " ➤ "))
                               ((map-elt header-model :position)
                                (concat (map-elt header-model :position)
                                        (when help-chunk (concat " " help-chunk))
                                        " ➤ "))
                               (t ""))
                              (propertize (map-elt header-model :buffer-name)
                                          'font-lock-face 'agent-shell-buffer-name)
                              (if (map-elt header-model :model-name)
                                  (concat " ➤ " (propertize (map-elt header-model :model-name)
                                                            'font-lock-face 'agent-shell-model
                                                            'help-echo (concat "Open LLM model menu "
                                                                               (when model-binding
                                                                                 (propertize model-binding 'face 'agent-shell-key-binding)))
                                                            'mouse-face 'mode-line-highlight
                                                            'local-map (let ((map (make-sparse-keymap)))
                                                                         (define-key map [header-line down-mouse-1] #'ignore)
                                                                         (define-key map [header-line mouse-1]
                                                                                     (agent-shell--mode-line-model-menu))
                                                                         map)))
                                "")
                              (if (map-elt header-model :thought-level-name)
                                  (concat " ➤ " (propertize (map-elt header-model :thought-level-name)
                                                            'font-lock-face 'agent-shell-thought-level
                                                            'help-echo (concat "Open thought level menu "
                                                                               (when thought-level-binding
                                                                                 (propertize thought-level-binding 'face 'agent-shell-key-binding)))
                                                            'mouse-face 'mode-line-highlight
                                                            'local-map (let ((map (make-sparse-keymap)))
                                                                         (define-key map [header-line down-mouse-1] #'ignore)
                                                                         (define-key map [header-line mouse-1]
                                                                                     (agent-shell--mode-line-thought-level-menu))
                                                                         map)))
                                "")
                              (if (map-elt header-model :mode-name)
                                  (concat " ➤ " (propertize (map-elt header-model :mode-name)
                                                            'font-lock-face 'agent-shell-session-mode
                                                            'help-echo (concat "Open session mode menu "
                                                                               (when mode-binding
                                                                                 (propertize mode-binding 'face 'agent-shell-key-binding)))
                                                            'mouse-face 'mode-line-highlight
                                                            'local-map (let ((map (make-sparse-keymap)))
                                                                         (define-key map [header-line down-mouse-1] #'ignore)
                                                                         (define-key map [header-line mouse-1]
                                                                                     (agent-shell--mode-line-mode-menu))
                                                                         map)))
                                "")
                              (propertize (map-elt header-model :project-name) 'font-lock-face 'agent-shell-session-directory)
                              (if (map-elt header-model :session-id)
                                  (concat " ➤ " (map-elt header-model :session-id))
                                "")
                              (if (map-elt header-model :context-indicator)
                                  (concat (if (> (length (map-elt header-model :context-indicator)) 1)
                                              " ➤ "
                                            " ")
                                          (map-elt header-model :context-indicator))
                                "")
                              (if (and (map-elt header-model :status)
                                       (not (map-elt header-model :position)))
                                  (concat " ➤ " (map-elt header-model :status))
                                "")
                              (if (map-elt header-model :busy-indicator-frame)
                                  (map-elt header-model :busy-indicator-frame)
                                ""))))
    (pcase agent-shell-header-style
      ((or 'none (pred null)) nil)
      ('text text-header)
      ('graphical
       (if (image-type-available-p 'svg)
           ;; +------+
           ;; | icon | Top text line
           ;; |      | Bottom text line
           ;; +------+
           ;; Key hints row (optional, last row)
           ;; Caching is handled by `agent-shell--render-header-model'.
           (let* ((geometry (agent-shell--svg-header-geometry header-model))
                  (char-height (map-elt geometry :char-height))
                  (font-size (map-elt geometry :font-size))
                  (font-family (map-elt header-model :font-family))
                  (image-height (map-elt geometry :image-height))
                  (image-width (map-elt geometry :image-width))
                  (text-height (map-elt geometry :text-height))
                  (total-height (map-elt geometry :total-height))
                  (icon-x (map-elt geometry :icon-x))
                  (icon-y (map-elt geometry :icon-y))
                  (icon-text-x (map-elt geometry :icon-text-x))
                  (icon-text-y (map-elt geometry :icon-text-y))
                  (key-hints-x (map-elt geometry :key-hints-x))
                  (key-hints-y (map-elt geometry :key-hints-y))
                  (svg (svg-create (map-elt header-model :width) total-height))
                  (icon-filename
                   (if (map-elt header-model :icon-name)
                       (agent-shell--fetch-agent-icon (map-elt header-model :icon-name))
                     (agent-shell--make-agent-fallback-icon (map-elt header-model :buffer-name) 100)))
                  (image-type (or (agent-shell--image-type-to-mime icon-filename)
                                  "image/png")))
             ;; Icon
             (when (and icon-filename image-type)
               (svg-embed svg icon-filename
                          image-type nil
                          :x icon-x :y icon-y :width image-width :height image-height))
             ;; Top text line
             (svg--append svg (let ((text-node (dom-node 'text
                                                         `((x . ,icon-text-x)
                                                           (y . ,icon-text-y)
                                                           (font-size . ,font-size)
                                                           (font-family . ,font-family)))))
                                ;; Agent name
                                (dom-append-child text-node
                                                  (dom-node 'tspan
                                                            `((fill . ,(agent-shell--svg-fill-color 'agent-shell-buffer-name)))
                                                            (map-elt header-model :buffer-name)))
                                ;; Model name (optional)
                                (when (map-elt header-model :model-name)
                                  ;; Add separator arrow
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color 'default))
                                                                (dx . "8"))
                                                              "➤"))
                                  ;; Add model name
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color 'agent-shell-model))
                                                                (dx . "8"))
                                                              (map-elt header-model :model-name))))
                                ;; Thought level (optional)
                                (when (map-elt header-model :thought-level-id)
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color 'default))
                                                                (dx . "8"))
                                                              "➤"))
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color 'agent-shell-thought-level))
                                                                (dx . "8"))
                                                              (map-elt header-model :thought-level-name))))
                                ;; Session mode (optional)
                                (when (map-elt header-model :mode-name)
                                  ;; Add separator arrow
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color 'default))
                                                                (dx . "8"))
                                                              "➤"))
                                  ;; Add session mode text
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color 'agent-shell-session-mode))
                                                                (dx . "8"))
                                                              (map-elt header-model :mode-name))))
                                (when (map-elt header-model :context-indicator)
                                  (when (> (length (map-elt header-model :context-indicator)) 1)
                                    ;; Add separator arrow
                                    (dom-append-child text-node
                                                      (dom-node 'tspan
                                                                `((fill . ,(agent-shell--svg-fill-color 'default))
                                                                  (dx . "8"))
                                                                "➤")))
                                  ;; Add context indicator
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color
                                                                          (or (get-text-property 0 'face (map-elt header-model :context-indicator))
                                                                              'default)))
                                                                (dx . "8"))
                                                              (format-mode-line (map-elt header-model :context-indicator)))))
                                text-node))
             ;; Bottom text line
             (svg--append svg (let ((text-node (dom-node 'text
                                                         `((x . ,icon-text-x)
                                                           (y . ,(+ icon-text-y text-height (- char-height font-size)))
                                                           (font-size . ,font-size)
                                                           (font-family . ,font-family)))))
                                ;; Position (optional, before project)
                                (when (map-elt header-model :position)
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color
                                                                          (or (get-text-property 0 'face (map-elt header-model :position))
                                                                              'default))))
                                                              (substring-no-properties (map-elt header-model :position))))
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color 'default))
                                                                (dx . "8"))
                                                              "➤")))
                                ;; Directory path
                                (dom-append-child text-node
                                                  (dom-node 'tspan
                                                            `((fill . ,(agent-shell--svg-fill-color 'agent-shell-session-directory))
                                                              ,@(when (map-elt header-model :position) '((dx . "8"))))
                                                            (map-elt header-model :project-name)))
                                ;; Session ID (optional)
                                (when (map-elt header-model :session-id)
                                  ;; Separator arrow (default foreground)
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color 'default))
                                                                (dx . "8"))
                                                              "➤"))
                                  ;; Session ID text
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color 'agent-shell-session-id))
                                                                (dx . "8"))
                                                              (substring-no-properties (map-elt header-model :session-id)))))
                                ;; Status (optional)
                                (when (map-elt header-model :status)
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color 'default))
                                                                (dx . "8"))
                                                              "➤"))
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color
                                                                          (or (get-text-property 0 'face (map-elt header-model :status))
                                                                              'default)))
                                                                (dx . "8"))
                                                              (substring-no-properties (map-elt header-model :status)))))
                                (when (map-elt header-model :busy-indicator-frame)
                                  (dom-append-child text-node
                                                    (dom-node 'tspan
                                                              `((fill . ,(agent-shell--svg-fill-color 'default))
                                                                (dx . "8"))
                                                              (map-elt header-model :busy-indicator-frame))))
                                text-node))
             ;; Key hints row (last row if key hints present)
             (when key-hints
               (svg--append svg (let ((text-node (dom-node 'text
                                                           `((x . ,key-hints-x)
                                                             (y . ,key-hints-y)
                                                             (font-size . ,font-size)
                                                           (font-family . ,font-family))))
                                      (first t))
                                  (dolist (hint key-hints)
                                    (when (map-elt hint :description)
                                      ;; Add key (XML-escape angle brackets)
                                      (dom-append-child text-node
                                                        (dom-node 'tspan
                                                                  `((fill . ,(agent-shell--svg-fill-color 'agent-shell-key-binding))
                                                                    ,@(unless first '((dx . "8"))))
                                                                  (replace-regexp-in-string
                                                                   "<" "&lt;"
                                                                   (replace-regexp-in-string
                                                                    ">" "&gt;"
                                                                    (map-elt hint :key)))))
                                      (setq first nil)
                                      ;; Add space and description
                                      (dom-append-child text-node
                                                        (dom-node 'tspan
                                                                  `((fill . ,(agent-shell--svg-fill-color 'font-lock-comment-face))
                                                                    (dx . "8"))
                                                                  (map-elt hint :description)))))
                                  text-node)))
             ;; Shrink the canvas to what was actually drawn.  Every
             ;; pixel of this image is redrawn on each beat of the busy
             ;; animation, so a frame-wide canvas pays to blit padding
             ;; nothing is drawn on.  Overshooting is harmless (the
             ;; padding is transparent); undershooting would clip text,
             ;; so the margin is generous.
             (dom-set-attribute svg 'width
                                (min (dom-attr svg 'width)
                                     (max (+ icon-x image-width 16)
                                          (+ (agent-shell--svg-content-width svg) 16))))
             (let ((result (propertize
                            (format " %s" (with-temp-buffer
                                            (svg-insert-image svg)
                                            (buffer-string)))
                            'help-echo "Open settings menu"
                            'mouse-face 'mode-line-highlight
                            'local-map (let ((map (make-sparse-keymap)))
                                         (define-key map [header-line down-mouse-1] #'ignore)
                                         (define-key map [header-line mouse-1]
                                                     (agent-shell--mode-line-combined-menu))
                                         map))))
               result))
         text-header))
      (_ text-header))))

(defun agent-shell--image-type-to-mime (filename)
  "Convert image type from FILENAME to MIME type string.
Returns a MIME type like \"image/png\" or \"image/jpeg\"."
  (when-let* ((type (and (stringp filename)
                         (image-supported-file-p filename))))
    (pcase type
      ('svg "image/svg+xml")
      (_ (format "image/%s" type)))))

(cl-defun agent-shell--update-header-and-mode-line (&key cache-enabled)
  "Update header and mode line based on `agent-shell-header-style'.

With CACHE-ENABLED non-nil (used by heartbeat ticks), reuse the model
from the last full update and refresh only the animated busy frame,
avoiding a full rebuild.  With CACHE-ENABLED nil (a full update), rebuild
everything and clear the render cache."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  ;; A full update means some non-animation input changed, so drop the
  ;; render cache.  This bounds its size to the animation's frame count
  ;; and keeps entries fresh.  Cached (busy tick) updates keep it.
  (unless cache-enabled
    (setq agent-shell--header-cache nil))
  (setq header-line-format
        (agent-shell--render-header-model
         (if (and cache-enabled agent-shell--header-last-model)
             (agent-shell--header-model-refresh-frame agent-shell--header-last-model)
           (setq agent-shell--header-last-model
                 (agent-shell--make-header-model
                  (agent-shell--state)
                  :width (agent-shell--header-width (current-buffer))
                  :menu-keys `((:model . ,(key-description (where-is-internal
                                                            'agent-shell-set-session-model
                                                            agent-shell-mode-map t)))
                               (:mode . ,(key-description (where-is-internal
                                                           'agent-shell-set-session-mode
                                                           agent-shell-mode-map t)))
                               (:thought-level . ,(key-description (where-is-internal
                                                                    'agent-shell-set-session-thought-level
                                                                    agent-shell-mode-map t)))))))))
  (when (memq agent-shell-header-style '(text none nil))
    (force-mode-line-update)))

(defun agent-shell--header-model-refresh-frame (model)
  "Return a copy of MODEL with its busy-indicator frame refreshed.
Copies so the cached MODEL is left untouched."
  (let ((copy (copy-alist model)))
    (map-put! copy :busy-indicator-frame (agent-shell--busy-indicator-frame))
    copy))

(defun agent-shell--image-extension-from-content-type ()
  "Return an image file extension for the response `Content-Type', or nil.

Point should be within the HTTP response headers of the current buffer.
Used to name a cached icon whose URL carries no file extension (e.g. a
GitHub avatar), so `image-supported-file-p' can recognize it later."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^Content-Type:[ \t]*\\([^ \t\r\n;]+\\)" nil t)
      (pcase (downcase (match-string 1))
        ("image/png" "png")
        ("image/jpeg" "jpg")
        ("image/gif" "gif")
        ("image/svg+xml" "svg")
        ("image/webp" "webp")
        ((or "image/x-icon" "image/vnd.microsoft.icon") "ico")))))

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
           (cache-dir (agent-shell-cache-dir mode))
           ;; A URL without a recognizable image extension (e.g. a GitHub
           ;; avatar) is cached under a Content-Type-derived extension so
           ;; that `image-supported-file-p' can recognize it. Reuse such a
           ;; copy across calls by globbing for the extension we appended.
           (has-extension (seq-contains-p image-file-name-extensions
                                          (downcase (or (file-name-extension filename) ""))))
           (cache-path (if has-extension
                           (expand-file-name filename cache-dir)
                         (car (file-expand-wildcards
                               (expand-file-name (concat filename ".*")
                                                 cache-dir))))))
      (unless (and cache-path (file-exists-p cache-path))
        (let ((buffer (url-retrieve-synchronously url t t 5.0)))
          (when buffer
            (with-current-buffer buffer
              (goto-char (point-min))
              (if (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
                  (progn
                    (setq cache-path
                          (expand-file-name
                           (if has-extension
                               filename
                             (concat filename
                                     (when-let* ((ext (agent-shell--image-extension-from-content-type)))
                                       (concat "." ext))))
                           cache-dir))
                    (re-search-forward "\r?\n\r?\n")
                    (let ((coding-system-for-write 'no-conversion))
                      (write-region (point) (point-max) cache-path)))
                (message "Icon fetch failed: %s" url)))
            (kill-buffer buffer))))
      (when (and cache-path (file-exists-p cache-path))
        cache-path))))

(defun agent-shell--make-agent-fallback-icon (icon-name width)
  "Create SVG icon with first character of ICON-NAME and WIDTH.
Return file path of the generated SVG."
  (when (and icon-name (not (string-empty-p icon-name)))
    (let* ((icon-text (char-to-string (string-to-char icon-name)))
           (mode (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "light"))
           (filename (format "%s-%s.svg" icon-name width))
           (cache-path (expand-file-name filename (agent-shell-cache-dir mode)))
           (font-size (* 0.7 width))
           (x (/ width 2))
           (y (/ width 2)))
      (unless (file-exists-p cache-path)
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
  (declare (modes agent-shell-mode))
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
  (declare (modes agent-shell-mode))
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

(cl-defun agent-shell--make-missing-executable-error (&key executable install-instructions)
  "Create error message for missing EXECUTABLE.
INSTALL-INSTRUCTIONS is optional installation guidance."
  (concat (format "Executable \"%s\" not found.  Do you need (add-to-list 'exec-path \"another/path/to/consider/\")?" executable)
          (when install-instructions
            (concat "  " install-instructions))))

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

;;; Events

(defvar agent-shell--subscription-counter 0
  "Counter for generating unique subscription tokens.")

(cl-defun agent-shell-subscribe-to (&key shell-buffer event on-event)
  "Subscribe to events in SHELL-BUFFER.

ON-EVENT is a function called with an event alist containing:
  :event - A symbol identifying the event

When EVENT is non-nil, only events matching that symbol are
dispatched.
When EVENT is nil, all events are dispatched.

Initialization events (emitted in order):
  `init-started'        - Initialization pipeline started
  `init-client'         - ACP client created
  `init-subscriptions'  - ACP event subscriptions registered
  `init-handshake'      - ACP initialize/handshake RPC completed
  `init-authenticate'   - ACP authentication completed (optional)
  `init-session'        - ACP session created
  `init-model'          - Default model set (optional)
  `init-session-mode'   - Default session mode set (optional)
  `session-list'        - Session list fetch initiated
  `session-prompt'      - About to prompt user for session selection
  `session-selected'    - Session chosen (new or existing)
    :data contains :session-id (nil when starting new)
  `session-selection-cancelled' - User cancelled session selection
  `init-finished'       - Initialization pipeline completed
  `prompt-ready'        - Shell prompt displayed and ready for input

Session events:
  `tool-call-update'      - Tool call started or updated
    :data contains :tool-call-id and :tool-call
  `config-option-update'  - ACP config option(s) changed
    :data contains :config-options (normalized list)
  `file-write'            - File written via fs/write_text_file
    :data contains :path and :content
  `permission-request'    - Permission prompt displayed to user
    :data contains :request-id, :tool-call-id, :tool-call
  `permission-response'   - Permission response sent
    :data contains :request-id, :tool-call-id, :option-id, :cancelled
  `agent-message-chunk'   - Agent streamed a chunk of message text
    :data contains :text-chunk (the raw text the agent emitted, nil for a
    non-text block such as an image).  Emitted once per streamed chunk, so
    it may fire many times per turn; the shell neither renders nor
    accumulates the text.
  `turn-complete'         - Agent turn finished and prompt ready for input
    :data contains :stop-reason and :usage
  `session-title-changed' - Session title updated
    :data contains :title
  `session-restored'      - Reloaded session fully replayed and settled
  `input-submitted'       - User submitted input to the agent
    :data contains :prompt (the text sent to the agent, with any
    truncated regions expanded)
  `prompt-steered'        - User steered a prompt into the running turn
    :data contains :prompt and :outcome.  :outcome is `injected' when
    the prompt joined the running turn, `prompt-required' or
    `started-new-turn' when the turn had already ended, or `failed'
    when the steer could not be applied and the prompt was queued
    instead.  See `agent-shell-experimental--steering-outcome'.
  `idle'                  - Agent idle for variable `agent-shell-idle-timeout'
    seconds :data contains :idle-event and :buffer

General events:
  `error'               - ACP request failed
    :data contains :code and :message
  `clean-up'            - Buffer being killed, resources cleaned up

Returns a subscription token for use with `agent-shell-unsubscribe'.

Example usage:

  ;; Subscribe to all events
  (agent-shell-subscribe-to
   :shell-buffer shell-buffer
   :on-event (lambda (event)
               (message \"event: %s\" (map-elt event :event))))

  ;; Subscribe to file writes
  (agent-shell-subscribe-to
   :shell-buffer shell-buffer
   :event \\='file-write
   :on-event (lambda (event)
               (let ((data (map-elt event :data)))
                 (message \"wrote: %s\" (map-elt data :path)))))

  ;; Unsubscribe
  (let ((token (agent-shell-subscribe-to
                :shell-buffer shell-buffer
                :on-event #\\='my-handler)))
    (agent-shell-unsubscribe :subscription token))

  ;; Get notified when agent is idle (skip if buffer is visible)
  (agent-shell-subscribe-to
   :shell-buffer shell-buffer
   :event \\='idle
   :on-event
     (lambda (event)
       (unless (get-buffer-window (map-nested-elt event \\='(:data :buffer)))
         (start-process \"notify\" nil \"notify-send\"
                        \"Agent Shell\" \"Agent waiting for input\"))))"
  (unless on-event
    (error "Missing required argument: :on-event"))
  (unless shell-buffer
    (error "Missing required argument: :shell-buffer"))
  (let ((token (cl-incf agent-shell--subscription-counter)))
    (with-current-buffer shell-buffer
      (let ((subscriptions (map-elt (agent-shell--state) :event-subscriptions)))
        (map-put! (agent-shell--state)
                  :event-subscriptions
                  (cons (list (cons :token token)
                              (cons :event event)
                              (cons :on-event on-event))
                        subscriptions))))
    token))

(cl-defun agent-shell-unsubscribe (&key subscription)
  "Remove event SUBSCRIPTION by token.

SUBSCRIPTION is a token returned by `agent-shell-subscribe-to'."
  (unless subscription
    (error "Missing required argument: :subscription"))
  (map-put! (agent-shell--state)
            :event-subscriptions
            (seq-remove (lambda (sub)
                          (equal (map-elt sub :token) subscription))
                        (map-elt (agent-shell--state) :event-subscriptions))))

(defvar agent-shell--system-sleep-load-attempted nil
  "Non-nil after attempting to load the optional `system-sleep' library.")

(defun agent-shell--system-sleep-available-p ()
  "Return non-nil when the optional `system-sleep' API is available.
Attempt to load the library at most once when its API is not already defined."
  (or (fboundp 'system-sleep-block-sleep)
      (unless agent-shell--system-sleep-load-attempted
        (setq agent-shell--system-sleep-load-attempted t)
        (require 'system-sleep nil t)
        (fboundp 'system-sleep-block-sleep))))

(defun agent-shell--inhibit-sleep (state)
  "Block system idle sleep for STATE's shell if so configured.

No-op unless `agent-shell-inhibit-system-sleep' is non-nil and the
`system-sleep' library (Emacs 31.1+) is available.  The block is
recorded in STATE and released by `agent-shell--uninhibit-sleep'."
  ;; Block system idle sleep but allow the display to blank.
  (when-let* ((agent-shell-inhibit-system-sleep)
              ((not (map-elt state :sleep-token)))
              ((agent-shell--system-sleep-available-p)))
    ;; `system-sleep-block-sleep' talks to logind over D-Bus, which can fail
    ;; (e.g. "Permission denied" under WSL where no logind session exists).
    ;; Degrade gracefully instead of letting the error break event dispatch.
    ;; Report the failure so the user can set `agent-shell-inhibit-system-sleep'
    ;; to nil to opt out.
    (condition-case err
        (when-let* ((token (system-sleep-block-sleep "agent-shell (agent busy)" t)))
          (map-put! state :sleep-token token))
      (error
       (message "Sleep inhibit unavailable (%s).  Set `agent-shell-inhibit-system-sleep' to nil to disable."
                (error-message-string err))))))

(defun agent-shell--uninhibit-sleep (state)
  "Release any system sleep block held by STATE's shell."
  (when-let* ((token (map-elt state :sleep-token))
              ((fboundp 'system-sleep-unblock-sleep)))
    (system-sleep-unblock-sleep token)
    (map-put! state :sleep-token nil)))

(defun agent-shell--sync-system-sleep (state)
  "Block or release system sleep to match STATE's shell status.
Blocks only while `agent-shell-status' is `busy' (the agent is actively
processing).  Releases otherwise, including when `blocked' (waiting on a
permission response), since that is waiting on user input rather than
work in progress."
  (when-let* ((buffer (map-elt state :buffer))
              ((buffer-live-p buffer)))
    ;; `agent-shell-status' errors when BUFFER is not a live shell (e.g. a
    ;; killed buffer during teardown).  Treat that as no work in progress
    ;; and release rather than propagating out of event dispatch.
    (if (eq (ignore-errors (agent-shell-status :shell-buffer buffer)) 'busy)
        (agent-shell--inhibit-sleep state)
      (agent-shell--uninhibit-sleep state))))

(cl-defun agent-shell--emit-event (&key event data)
  "Emit an EVENT to matching subscribers.
EVENT is a symbol identifying the event.
DATA is an optional alist of event-specific data."
  (let ((state (agent-shell--state))
        (event-alist (list (cons :event event))))
    (when data
      (push (cons :data data) event-alist))
    ;; Keep the system awake while the agent is working.  `error' and
    ;; `clean-up' are emitted before the shell clears its busy state, so
    ;; release explicitly rather than reading a stale status.
    (pcase event
      ((or 'error 'clean-up) (agent-shell--uninhibit-sleep state))
      (_ (agent-shell--sync-system-sleep state)))
    (dolist (sub (map-elt state :event-subscriptions))
      (when (and (buffer-live-p (map-elt state :buffer))
                 (or (not (map-elt sub :event))
                     (eq (map-elt sub :event) event)))
        (with-current-buffer (map-elt state :buffer)
          (condition-case err
              (funcall (map-elt sub :on-event) event-alist)
            (error
             (message "agent-shell: subscriber for %s errored: %S" event err))))))))

(cl-defun agent-shell--start-idle-timer (&key event data)
  "Start the idle timer for EVENT with DATA.
Cancels any existing idle timer first.  After
variable `agent-shell-idle-timeout' seconds, emits an `idle' event with
the original EVENT as :idle-event."
  (agent-shell--cancel-idle-timer)
  (when-let* ((buffer (map-elt (agent-shell--state) :buffer)))
    (map-put! (agent-shell--state) :idle-timer
              (run-at-time (agent-shell-idle-timeout :event event) nil
                           (lambda ()
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (map-put! (agent-shell--state) :idle-timer nil)
                                 (agent-shell--emit-event
                                  :event 'idle
                                  :data (append (list (cons :idle-event event)
                                                      (cons :buffer buffer))
                                                data)))))))))

(defun agent-shell--cancel-idle-timer ()
  "Cancel any pending idle timer."
  (when-let* ((timer (map-elt (agent-shell--state) :idle-timer))
              ((timerp timer)))
    (cancel-timer timer))
  (map-put! (agent-shell--state) :idle-timer nil))

;;; Initialization

(cl-defun agent-shell--initialize-client ()
  "Initialize ACP client."
  (agent-shell--update-bootstrapping-fragment
   :state (agent-shell--state)
   :block-id "starting"
   :label-left (format "%s %s"
                       (agent-shell--make-status-kind-label :status "in_progress")
                       (propertize "Starting agent" 'font-lock-face 'agent-shell-section-heading))
   :body "Creating client..."
   :create-new t)
  (if (map-elt (agent-shell--state) :client-maker)
      (progn
        (map-put! (agent-shell--state)
                  :client (funcall (map-elt agent-shell--state :client-maker)
                                   (map-elt agent-shell--state :buffer)))
        (agent-shell--emit-event :event 'init-client)
        t)
    (shell-maker-write-output :config shell-maker--config
                              :output "No :client-maker found")
    (shell-maker-finish-output :config shell-maker--config
                               :success nil)
    nil))

(cl-defun agent-shell--initialize-subscriptions ()
  "Initialize ACP client subscriptions."
  (agent-shell--update-bootstrapping-fragment
   :state agent-shell--state
   :block-id "starting"
   :label-left (format "%s %s"
                       (agent-shell--make-status-kind-label :status "in_progress")
                       (propertize "Starting agent" 'font-lock-face 'agent-shell-section-heading))
   :body "\n\nSubscribing..."
   :append t)
  (if (map-elt agent-shell--state :client)
      (progn
        (agent-shell--subscribe-to-client-events :state agent-shell--state)
        (agent-shell--emit-event :event 'init-subscriptions)
        t)
    (shell-maker-write-output :config shell-maker--config
                              :output "No :client found")
    (shell-maker-finish-output :config shell-maker--config
                               :success nil)
    nil))

(cl-defun agent-shell--send-request (&key state client request buffer on-success on-failure sync)
  "Send ACP REQUEST, tracking it in STATE via :active-requests.

Wraps `acp-send-request' so that REQUEST is pushed to
:active-requests while in-flight and removed on success or failure.

CLIENT, REQUEST, BUFFER, ON-SUCCESS, ON-FAILURE, and SYNC are passed
through to `acp-send-request'."
  ;; Migrate state for sessions created before :active-requests existed.
  ;; Without this, map-put! fails on mid-session package updates.
  (unless (assq :active-requests state)
    (nconc state (list (cons :active-requests nil))))
  (map-put! state :active-requests
            (cons request (map-elt state :active-requests)))
  (acp-send-request
   :client client
   :request request
   :buffer buffer
   :on-success (lambda (acp-response)
                 (map-put! state :active-requests
                           (seq-remove (lambda (r)
                                         (equal r request))
                                       (map-elt state :active-requests)))
                 (when on-success
                   (funcall on-success acp-response)))
   :on-failure (lambda (acp-error raw-message)
                 (map-put! state :active-requests
                           (seq-remove (lambda (r)
                                         (equal r request))
                                       (map-elt state :active-requests)))
                 (when on-failure
                   (funcall on-failure acp-error raw-message)))
   :sync sync))

(cl-defun agent-shell--initiate-handshake (&key shell-buffer on-initiated)
  "Initiate ACP handshake with SHELL-BUFFER.

Must provide ON-INITIATED (lambda ())."
  (unless on-initiated
    (error "Missing required argument: :on-initiated"))
  (with-current-buffer (map-elt agent-shell--state :buffer)
    (agent-shell--update-bootstrapping-fragment
     :state agent-shell--state
     :block-id "starting"
     :body "\n\nInitializing..."
     :append t))
  (agent-shell--send-request
   :state agent-shell--state
   :client (map-elt agent-shell--state :client)
   :request (acp-make-initialize-request
             :protocol-version 1
             :client-info `((name . "agent-shell")
                            (title . "Emacs Agent Shell")
                            (version . ,agent-shell--version))
             :read-text-file-capability agent-shell-text-file-capabilities
             :write-text-file-capability agent-shell-text-file-capabilities)
   :on-success (lambda (acp-response)
                 (with-current-buffer shell-buffer
                   (let ((acp-session-capabilities (or (map-elt acp-response 'sessionCapabilities)
                                                       (map-nested-elt acp-response '(agentCapabilities sessionCapabilities)))))
                     (map-put! agent-shell--state :supports-session-list
                               (and (listp acp-session-capabilities)
                                    (assq 'list acp-session-capabilities)
                                    t))
                     (map-put! agent-shell--state :supports-session-resume
                               (and (listp acp-session-capabilities)
                                    (assq 'resume acp-session-capabilities)
                                    t))
                     (map-put! agent-shell--state :supports-session-fork
                               (and (listp acp-session-capabilities)
                                    (assq 'fork acp-session-capabilities)
                                    t)))
                   ;; Save prompt capabilities from agent, converting to internal symbols
                   (when-let* ((prompt-capabilities
                                (map-nested-elt acp-response '(agentCapabilities promptCapabilities))))
                     (map-put! agent-shell--state :prompt-capabilities
                               (list (cons :image (map-elt prompt-capabilities 'image))
                                     (cons :embedded-context (map-elt prompt-capabilities 'embeddedContext)))))
                   ;; Save available modes from agent, converting to internal symbols
                   (when-let* ((modes (map-elt acp-response 'modes)))
                     (map-put! agent-shell--state :available-modes
                               (list (cons :current-mode-id (map-elt modes 'currentModeId))
                                     (cons :modes (mapcar (lambda (mode)
                                                            `((:id . ,(map-elt mode 'id))
                                                              (:name . ,(map-elt mode 'name))
                                                              (:description . ,(map-elt mode 'description))))
                                                          (map-elt modes 'availableModes))))))
                   ;; Steering is an extension, so it is advertised in the
                   ;; response's top-level `_meta' rather than in
                   ;; `agentCapabilities'.  See `agent-shell-steering-supported-p'.
                   (map-put! agent-shell--state :supports-steering
                             (eq (map-nested-elt acp-response '(_meta steering supported)) t))
                   (when-let* ((agent-capabilities (map-elt acp-response 'agentCapabilities)))
                     (map-put! agent-shell--state :supports-session-load
                               (eq (map-elt agent-capabilities 'loadSession) t))
                     (agent-shell--update-bootstrapping-fragment
                      :state agent-shell--state
                      :block-id "agent_capabilities"
                      :label-left (propertize "Agent capabilities" 'font-lock-face 'agent-shell-section-heading)
                      ;; Listed alongside the spec capabilities because that is
                      ;; what it is to a reader of this block, even though the
                      ;; wire carries it elsewhere.
                      :body (agent-shell--format-agent-capabilities
                             (if (map-elt agent-shell--state :supports-steering)
                                 (append agent-capabilities '((steering . t)))
                               agent-capabilities))))
                   (agent-shell--emit-event :event 'init-handshake))
                 (funcall on-initiated))
   :on-failure (agent-shell--make-error-handler
                :state agent-shell--state :shell-buffer shell-buffer)))

(cl-defun agent-shell--authenticate (&key shell-buffer on-authenticated)
  "Initiate ACP authentication with SHELL-BUFFER.

Must provide ON-AUTHENTICATED (lambda ())."
  (with-current-buffer (map-elt agent-shell--state :buffer)
    (agent-shell--update-bootstrapping-fragment
     :state (agent-shell--state)
     :block-id "starting"
     :body "\n\nAuthenticating..."
     :append t))
  (if (map-elt (agent-shell--state) :authenticate-request-maker)
      (agent-shell--send-request
       :state (agent-shell--state)
       :client (map-elt (agent-shell--state) :client)
       :request (funcall (map-elt agent-shell--state :authenticate-request-maker))
       :on-success (lambda (_acp-response)
                     ;; TODO: More to be handled?
                     (with-current-buffer shell-buffer
                       (agent-shell--emit-event :event 'init-authenticate))
                     (funcall on-authenticated))
       :on-failure (agent-shell--make-error-handler
                    :state (agent-shell--state) :shell-buffer shell-buffer))
    (shell-maker-write-output :config shell-maker--config
                              :output "No :authenticate-request-maker")
    (shell-maker-finish-output :config shell-maker--config
                               :success nil)))

(cl-defun agent-shell--set-session-config-option (&key config-id value on-success on-failure)
  "Set session config option CONFIG-ID to VALUE.
Call ON-SUCCESS after state is updated from the response, or ON-FAILURE
on error."
  (agent-shell--send-request
   :state (agent-shell--state)
   :client (map-elt (agent-shell--state) :client)
   :request (acp-make-session-set-config-option-request
             :session-id (map-nested-elt (agent-shell--state) '(:session :id))
             :config-id config-id
             :value value)
   :buffer (current-buffer)
   :on-success (lambda (acp-response)
                 (if (map-elt acp-response 'configOptions)
                     (agent-shell--save-config-options
                      :state (agent-shell--state)
                      :acp-config-options (map-elt acp-response 'configOptions))
                   (agent-shell--config-option-set-value
                    :state (agent-shell--state)
                    :config-id config-id
                    :value value))
                 (agent-shell--update-header-and-mode-line)
                 (when on-success
                   (funcall on-success)))
   :on-failure (or on-failure
                   (lambda (acp-error _raw-message)
                     (message "Failed to change config option: %s" acp-error)))))

(cl-defun agent-shell--config-option-set-model-id (&key model-id on-success on-failure)
  "Set current model to MODEL-ID.
Call ON-SUCCESS on success, or ON-FAILURE on error."
  (if-let* ((model-option (agent-shell--config-option-by-category (agent-shell--state) "model")))
      (agent-shell--set-session-config-option
       :config-id (map-elt model-option :id)
       :value model-id
       :on-success (lambda ()
                     (message "Model: %s"
                              (agent-shell--config-option-value-name model-option model-id))
                     (when on-success
                       (funcall on-success)))
       :on-failure on-failure)
    (agent-shell--send-request
     :state (agent-shell--state)
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-model-request
               :session-id (map-nested-elt (agent-shell--state) '(:session :id))
               :model-id model-id)
     :buffer (current-buffer)
     :on-success (lambda (_acp-response)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :model-id model-id)
                     (map-put! (agent-shell--state) :session updated-session))
                   (message "Model: %s"
                            (or (map-elt (seq-find (lambda (model)
                                                     (string= (map-elt model :model-id) model-id))
                                                   (map-nested-elt (agent-shell--state) '(:session :models)))
                                         :name)
                                model-id))
                   (agent-shell--update-header-and-mode-line)
                   (when on-success
                     (funcall on-success)))
     :on-failure (or on-failure
                     (lambda (acp-error _raw-message)
                       (message "Failed to change model: %s" acp-error))))))

(cl-defun agent-shell--config-option-set-mode-id (&key mode-id on-success on-failure)
  "Set current session mode to MODE-ID.
Call ON-SUCCESS on success, or ON-FAILURE on error."
  (if-let* ((mode-option (agent-shell--config-option-by-category (agent-shell--state) "mode")))
      (agent-shell--set-session-config-option
       :config-id (map-elt mode-option :id)
       :value mode-id
       :on-success (lambda ()
                     (message "Session mode: %s"
                              (agent-shell--config-option-value-name mode-option mode-id))
                     (when on-success
                       (funcall on-success)))
       :on-failure on-failure)
    (agent-shell--send-request
     :state (agent-shell--state)
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-mode-request
               :session-id (map-nested-elt (agent-shell--state) '(:session :id))
               :mode-id mode-id)
     :buffer (current-buffer)
     :on-success (lambda (_acp-response)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :mode-id mode-id)
                     (map-put! (agent-shell--state) :session updated-session))
                   (message "Session mode: %s"
                            (or (agent-shell--resolve-session-mode-name
                                 mode-id
                                 (agent-shell--get-available-modes (agent-shell--state)))
                                mode-id))
                   (agent-shell--update-header-and-mode-line)
                   (when on-success
                     (funcall on-success)))
     :on-failure (or on-failure
                     (lambda (acp-error _raw-message)
                       (message "Failed to change session mode: %s" acp-error))))))

(cl-defun agent-shell--config-option-set-thought-level-id (&key thought-level-id on-success on-failure)
  "Set current thought level to THOUGHT-LEVEL-ID.
Call ON-SUCCESS on success, or ON-FAILURE on error."
  (if-let* ((option (agent-shell--config-option-by-category (agent-shell--state) "thought_level")))
      (agent-shell--set-session-config-option
       :config-id (map-elt option :id)
       :value thought-level-id
       :on-success (lambda ()
                     (message "Thought level: %s"
                              (agent-shell--config-option-value-name option thought-level-id))
                     (when on-success
                       (funcall on-success)))
       :on-failure on-failure)
    ;; In contrast to model and mode, there is no dedicated request type to change the
    ;; thought level, so it is not a config option, it cannot be changed
    (user-error "Agent does not advertise a thought level option for this session")))

(cl-defun agent-shell--set-default-model (&key shell-buffer model-id on-model-changed)
  "Set default model to MODEL-ID in SHELL-BUFFER.
Call ON-MODEL-CHANGED on success."
  (when (map-nested-elt (agent-shell--state) '(:session :id))
    (with-current-buffer (map-elt agent-shell--state :buffer)
      (agent-shell--update-bootstrapping-fragment
       :state (agent-shell--state)
       :block-id "set-model"
       :label-left (propertize "Setting model" 'font-lock-face 'agent-shell-section-heading)
       :body (format "Requesting %s..." model-id)))
    (agent-shell--config-option-set-model-id
     :model-id model-id
     :on-success (lambda ()
                   (agent-shell--update-bootstrapping-fragment
                    :state (agent-shell--state)
                    :block-id "set-model"
                    :body "\n\nDone"
                    :append t)
                   (agent-shell--emit-event :event 'init-model)
                   (when on-model-changed
                     (funcall on-model-changed)))
     :on-failure (agent-shell--make-error-handler
                  :state (agent-shell--state) :shell-buffer shell-buffer))))

(cl-defun agent-shell--set-default-session-mode (&key shell-buffer mode-id on-mode-changed)
  "Set default session mode to MODE-ID in SHELL-BUFFER.
Call ON-MODE-CHANGED on success."
  (when (map-nested-elt (agent-shell--state) '(:session :id))
    (with-current-buffer (map-elt agent-shell--state :buffer)
      (agent-shell--update-bootstrapping-fragment
       :state (agent-shell--state)
       :block-id "set-session-mode"
       :label-left (propertize "Setting session mode" 'font-lock-face 'agent-shell-section-heading)
       :body (format "Requesting %s..." mode-id)))
    (agent-shell--config-option-set-mode-id
     :mode-id mode-id
     :on-success (lambda ()
                   (agent-shell--update-bootstrapping-fragment
                    :state (agent-shell--state)
                    :block-id "set-session-mode"
                    :body "\n\nDone"
                    :append t)
                   (agent-shell--emit-event :event 'init-session-mode)
                   (when on-mode-changed
                     (funcall on-mode-changed)))
     :on-failure (agent-shell--make-error-handler
                  :state (agent-shell--state) :shell-buffer shell-buffer))))

(cl-defun agent-shell--initiate-session (&key shell-buffer on-session-init)
  "Initiate ACP session creation with SHELL-BUFFER.

Must provide ON-SESSION-INIT (lambda ())."
  (unless on-session-init
    (error "Missing required argument: :on-session-init"))
  (with-current-buffer (map-elt (agent-shell--state) :buffer)
    (agent-shell--update-bootstrapping-fragment
     :state (agent-shell--state)
     :block-id "starting"
     :body "\n\nCreating session..."
     :append t))
  ;; User requested forking session with explicit session ID.
  (if-let* ((fork-session-id (map-elt (agent-shell--state) :fork-session-id)))
      (if (map-elt (agent-shell--state) :supports-session-fork)
          (agent-shell--initiate-session-fork-by-id
           :session-id fork-session-id
           :shell-buffer shell-buffer
           :on-session-init on-session-init)
        ;; Forking not supported. Start a new session.
        (message "Forking unsupported by agent. Starting new session.")
        (agent-shell--emit-event :event 'session-selected)
        (agent-shell--initiate-new-session
         :shell-buffer shell-buffer
         :on-session-init on-session-init))
    ;; User requested resuming session with explicit session ID.
    (if-let* ((resume-session-id (map-elt (agent-shell--state) :resume-session-id)))
        (if (or (map-elt (agent-shell--state) :supports-session-load)
                (map-elt (agent-shell--state) :supports-session-resume))
            ;; Agent supports some form of resuming.
            (progn
              (agent-shell--emit-event
               :event 'session-selected
               :data (list (cons :session-id resume-session-id)))
              (agent-shell--initiate-session-resume-by-id
               :session-id resume-session-id
               :shell-buffer shell-buffer
               :on-session-init on-session-init))
          ;; Resuming not supported. Start a new session.
          (message "Resuming unsupported by agent. Starting new session.")
          (agent-shell--emit-event :event 'session-selected)
          (agent-shell--initiate-new-session
           :shell-buffer shell-buffer
           :on-session-init on-session-init))
      ;; Resuming, but must request session list first.
      (if (and (map-elt (agent-shell--state) :supports-session-list)
               (or (map-elt (agent-shell--state) :supports-session-load)
                   (map-elt (agent-shell--state) :supports-session-resume))
               (not (memq agent-shell-session-strategy '(new-deferred new))))
          (agent-shell--initiate-session-list-and-load
           :shell-buffer shell-buffer
           :on-session-init on-session-init)
        (progn
          (agent-shell--emit-event :event 'session-selected)
          (agent-shell--initiate-new-session
           :shell-buffer shell-buffer
           :on-session-init on-session-init))))))

(defun agent-shell--sort-sessions-by-recency (acp-sessions)
  "Return ACP-SESSIONS sorted by recency, newest first.

Sorts by `updatedAt' when present, falling back to `createdAt'.
ISO-8601 timestamps sort lexically the same as chronologically,
so `string>' yields descending time order.

  (agent-shell--sort-sessions-by-recency
   \\='(((sessionId . \"a\") (updatedAt . \"2024-01-01T00:00:00Z\"))
     ((sessionId . \"b\") (updatedAt . \"2024-02-01T00:00:00Z\"))))
  ;; => (((sessionId . \"b\") (updatedAt . \"2024-02-01T00:00:00Z\"))
  ;;     ((sessionId . \"a\") (updatedAt . \"2024-01-01T00:00:00Z\")))"
  (seq-sort (lambda (a b)
              (string> (or (map-elt a 'updatedAt)
                           (map-elt a 'createdAt) "")
                       (or (map-elt b 'updatedAt)
                           (map-elt b 'createdAt) "")))
            acp-sessions))

(defun agent-shell--format-session-date (iso-timestamp)
  "Format ISO-TIMESTAMP as a human-friendly date string.

Returns \"Today, HH:MM\", \"Yesterday, HH:MM\", \"Mon DD, HH:MM\"
for the current year, or \"Mon DD, YYYY\" for other years."
  (condition-case nil
      (let* ((time (date-to-time iso-timestamp))
             (now (current-time))
             (decoded-now (decode-time now))
             (today-start (encode-time 0 0 0
                                       (decoded-time-day decoded-now)
                                       (decoded-time-month decoded-now)
                                       (decoded-time-year decoded-now)))
             (yesterday-start (time-subtract today-start (seconds-to-time (* 24 60 60))))
             (current-year (decoded-time-year (decode-time now)))
             (timestamp-year (decoded-time-year (decode-time time))))
        (cond
         ((not (time-less-p time today-start))
          (format-time-string "Today, %H:%M" time))
         ((not (time-less-p time yesterday-start))
          (format-time-string "Yesterday, %H:%M" time))
         ((= timestamp-year current-year)
          (format-time-string "%b %d, %H:%M" time))
         (t
          (format-time-string "%b %d, %Y" time))))
    (error iso-timestamp)))

(defun agent-shell--session-dir-name (acp-session)
  "Return directory name for ACP-SESSION."
  (file-name-nondirectory
   (directory-file-name (or (map-elt acp-session 'cwd) ""))))

(defun agent-shell--session-title (acp-session)
  "Return display title for ACP-SESSION, truncated to 50 chars."
  (let ((title (or (map-elt acp-session 'title) "Untitled")))
    (if (> (length title) 50)
        (concat (substring title 0 47) "...")
      title)))

(defun agent-shell--session-column-value (column acp-session)
  "Return the string value for COLUMN from ACP-SESSION.

COLUMN is a symbol: `directory', `title', `date', or `session-id'.

  (agent-shell--session-column-value
   \\='directory
   \\='((cwd . \"/home/user/project\")))
  ;; => \"project\""
  (pcase column
    ('directory (agent-shell--session-dir-name acp-session))
    ('title (agent-shell--session-title acp-session))
    ('date (agent-shell--format-session-date
            (or (map-elt acp-session 'updatedAt)
                (map-elt acp-session 'createdAt)
                "unknown-time")))
    ('session-id (or (map-elt acp-session 'sessionId) ""))
    (_ "")))

(defun agent-shell--session-column-face (column)
  "Return the face for COLUMN in the session selection prompt.

  (agent-shell--session-column-face \\='directory)
  ;; => `agent-shell-session-directory'"
  (pcase column
    ('directory 'agent-shell-session-directory)
    ('title 'agent-shell-session-title)
    ('date 'agent-shell-session-date)
    ('session-id 'agent-shell-session-id)
    (_ nil)))

(defun agent-shell--session-selection-columns ()
  "Return the list of columns for session selection.
Always includes directory, title, and date.  Appends session-id
when `agent-shell-show-session-id' is non-nil."
  (if agent-shell-show-session-id
      '(directory title date session-id)
    '(directory title date)))

(cl-defun agent-shell--session-choice-label (&key acp-session max-widths)
  "Return completion label for ACP-SESSION.
MAX-WIDTHS is an alist mapping column symbols to their max widths."
  (let* ((columns (agent-shell--session-selection-columns))
         parts
         (last-col (car (last columns))))
    (dolist (col columns)
      (let* ((value (agent-shell--session-column-value col acp-session))
             (face (agent-shell--session-column-face col))
             (max-width (or (map-elt max-widths col) (length value)))
             (padded (if (eq col last-col)
                         value
                       (let ((padding (make-string
                                       (max 0 (- (+ max-width 1) (length value)))
                                       ?\s)))
                         (concat value padding)))))
        (push (if face (propertize padded 'face face) padded) parts)))
    (apply #'concat (nreverse parts))))

(defun agent-shell--prompt-select-session (acp-sessions)
  "Prompt to choose one from ACP-SESSIONS.

Return selected session alist, nil to start a new session, or
`:other-shell' when the user chose an existing shell (already
displayed and bootstrapping shell killed).
Falls back to latest session in batch mode (e.g. tests)."
  (when (or acp-sessions (agent-shell-buffers))
    (if noninteractive
        (car acp-sessions)
      (let* ((other-shells (seq-remove (lambda (b) (eq b (current-buffer)))
                                       (agent-shell-buffers)))
             (new-session-choice "New shell")
             (columns (agent-shell--session-selection-columns))
             (max-widths (when acp-sessions
                           (mapcar (lambda (col)
                                     (cons col (apply #'max
                                                      (mapcar (lambda (s)
                                                                (length (agent-shell--session-column-value col s)))
                                                              acp-sessions))))
                                   columns)))
             ;; TODO: Consolidate choices with `agent-shell--shell-buffer'.
             (session-choices (agent-shell--apply-session-choices
                               (append (list (cons new-session-choice :new-shell)
                                             (cons "New Downloads shell" :downloads-shell)
                                             (cons "New temp shell" :temp-shell))
                                       (when other-shells
                                         (list (cons "Switch to shell buffer" :other-shell)))
                                       (mapcar (lambda (acp-session)
                                                 (cons (agent-shell--session-choice-label
                                                        :acp-session acp-session
                                                        :max-widths max-widths)
                                                       acp-session))
                                               acp-sessions))))
             ;; Some completion frameworks yielded appended (nil) to each line
             ;; unless this-command was bound.
             ;;
             ;; For example:
             ;;
             ;; Let's build something                 Today, 16:25 (nil)
             ;; Let's optimize the rocket engine      Feb 12, 21:02 (nil)
             (this-command 'agent-shell))
        (agent-shell--emit-event :event 'session-prompt)
        ;; Default to the first surviving choice, since a transform may have
        ;; reordered or dropped new-shell.  Sessions get a generic label
        ;; (their real label is a long column-padded string).
        (let* ((default-choice (caar session-choices))
               (selection (if (length= session-choices 1)
                              ;; Only one choice available; follow it without prompting.
                              default-choice
                            (completing-read
                             (format "Start shell (default: %s): "
                                     (if (keywordp (cdar session-choices))
                                         default-choice
                                       "Resume session"))
                             (lambda (string pred action)
                               (if (eq action 'metadata)
                                   '(metadata
                                     (display-sort-function . identity)
                                     (eager-display . t)
                                     (eager-update . t))
                                 (complete-with-action action session-choices string pred)))
                             nil t nil nil
                             default-choice))))
          (pcase (map-elt session-choices selection)
            (:new-shell nil)
            (:other-shell
             (let ((other-shell (agent-shell--read-shell-buffer
                                 :prompt "Switch to shell buffer: "
                                 :buffers other-shells))
                   (bootstrapping-shell (map-elt (agent-shell--state) :buffer)))
               (agent-shell--display-buffer other-shell)
               (kill-buffer bootstrapping-shell)
               :other-shell))
            (:downloads-shell
             (let ((config (map-elt (agent-shell--state) :agent-config)))
               (kill-buffer (map-elt (agent-shell--state) :buffer))
               (agent-shell-new-downloads-shell :config config))
             :other-shell)
            (:temp-shell
             (let ((config (map-elt (agent-shell--state) :agent-config)))
               (kill-buffer (map-elt (agent-shell--state) :buffer))
               (agent-shell-new-temp-shell :config config))
             :other-shell)
            (choice choice)))))))


(cl-defun agent-shell--session-from-response (&key acp-response acp-session-id)
  "Return internal session state from ACP-RESPONSE and ACP-SESSION-ID."
  (list (cons :id acp-session-id)
        (cons :config-options (agent-shell--normalize-config-options
                               (map-elt acp-response 'configOptions)))
        (cons :mode-id (map-nested-elt acp-response '(modes currentModeId)))
        (cons :modes (mapcar (lambda (mode)
                               `((:id . ,(map-elt mode 'id))
                                 (:name . ,(map-elt mode 'name))
                                 (:description . ,(map-elt mode 'description))))
                             (map-nested-elt acp-response '(modes availableModes))))
        (cons :model-id (map-nested-elt acp-response '(models currentModelId)))
        (cons :models (mapcar (lambda (model)
                                `((:model-id . ,(map-elt model 'modelId))
                                  (:name . ,(map-elt model 'name))
                                  (:description . ,(map-elt model 'description))))
                              (map-nested-elt acp-response '(models availableModels))))
        (cons :title (map-nested-elt agent-shell--state '(:session :title)))))

(cl-defun agent-shell--set-session-from-response (&key acp-response acp-session-id)
  "Set active session state from ACP-RESPONSE and ACP-SESSION-ID."
  (map-put! agent-shell--state
            :session (agent-shell--session-from-response
                      :acp-response acp-response
                      :acp-session-id acp-session-id))
  (agent-shell--save-config-options
   :state agent-shell--state
   :acp-config-options (map-elt acp-response 'configOptions)))

(defun agent-shell--create-bootstrapping-placeholders (state)
  "Create placeholder fragments in STATE's `bootstrapping' namespace.

Ensures fragments exist for notifications and responses arriving
after bootstrapping, so they update existing fragments in place
rather than inserting new ones at point-max.

Idempotent: re-calling with the same STATE either skips or
overwrites an existing fragment with equivalent content."
  (when (seq-empty-p (map-elt state :available-commands))
    (agent-shell--update-bootstrapping-fragment
     :state state
     :block-id "available_commands_update"
     :label-left (propertize "Available /commands"
                             'font-lock-face 'agent-shell-section-heading)))
  (when-let* ((id-fn (map-nested-elt state '(:agent-config :default-model-id)))
              (model-id (funcall id-fn))
              ((not (map-elt state :set-model))))
    (agent-shell--update-bootstrapping-fragment
     :state state
     :block-id "set-model"
     :label-left (propertize "Setting model"
                             'font-lock-face 'agent-shell-section-heading)
     :body (format "Requesting %s..." model-id)))
  (when-let* ((id-fn (map-nested-elt state '(:agent-config :default-session-mode-id)))
              (mode-id (funcall id-fn))
              ((not (map-elt state :set-session-mode))))
    (agent-shell--update-bootstrapping-fragment
     :state state
     :block-id "set-session-mode"
     :label-left (propertize "Setting session mode"
                             'font-lock-face 'agent-shell-section-heading)
     :body (format "Requesting %s..." mode-id))))

(defun agent-shell--display-session-options ()
  "Display available session options during bootstrapping."
  (when (agent-shell--config-options agent-shell--state)
    (agent-shell--update-bootstrapping-fragment
     :state agent-shell--state
     :block-id "available_config_options"
     :label-left (propertize "Available config options" 'font-lock-face 'agent-shell-section-heading)
     :body (agent-shell--format-available-config-options
            (agent-shell--config-options agent-shell--state))))
  (when (agent-shell--get-available-models agent-shell--state)
    (agent-shell--update-bootstrapping-fragment
     :state agent-shell--state
     :block-id "available_models"
     :label-left (propertize "Available models" 'font-lock-face 'agent-shell-section-heading)
     :body (agent-shell--format-available-models
            (agent-shell--get-available-models agent-shell--state))))
  (when (agent-shell--get-available-modes agent-shell--state)
    (agent-shell--update-bootstrapping-fragment
     :state agent-shell--state
     :block-id "available_modes"
     :label-left (propertize "Available modes" 'font-lock-face 'agent-shell-section-heading)
     :body (agent-shell--format-available-modes
            (agent-shell--get-available-modes agent-shell--state)))))

(cl-defun agent-shell--finalize-session-init (&key on-session-init)
  "Finalize session initialization and invoke ON-SESSION-INIT."
  (agent-shell--update-bootstrapping-fragment
   :state agent-shell--state
   :block-id "starting"
   :label-left (format "%s %s"
                       (agent-shell--make-status-kind-label :status "completed")
                       (propertize "Starting agent" 'font-lock-face 'agent-shell-section-heading))
   :body "\n\nReady"
   :append t)
  (agent-shell--update-header-and-mode-line)
  (agent-shell--display-session-options)
  (agent-shell--update-header-and-mode-line)
  (agent-shell--emit-event :event 'init-session)
  (funcall on-session-init))

(cl-defun agent-shell--initiate-new-session (&key shell-buffer on-session-init)
  "Initiate ACP session/new with SHELL-BUFFER and ON-SESSION-INIT."
  (agent-shell--send-request
   :state (agent-shell--state)
   :client (map-elt (agent-shell--state) :client)
   :request (acp-make-session-new-request
             :cwd (agent-shell--resolve-path (agent-shell-cwd))
             :mcp-servers (agent-shell--mcp-servers)
             :meta (map-nested-elt (agent-shell--state) '(:agent-config :session-meta)))
   :buffer (current-buffer)
   :on-success (lambda (acp-response)
                 (map-put! agent-shell--state
                           :session (agent-shell--session-from-response
                                     :acp-response acp-response
                                     :acp-session-id (map-elt acp-response 'sessionId)))
                 (agent-shell--save-config-options
                  :state agent-shell--state
                  :acp-config-options (map-elt acp-response 'configOptions))
                 (agent-shell--update-bootstrapping-fragment
                  :state agent-shell--state
                  :block-id "starting"
                  :label-left (format "%s %s"
                                      (agent-shell--make-status-kind-label :status "completed")
                                      (propertize "Starting agent" 'font-lock-face 'agent-shell-section-heading))
                  :body "\n\nReady"
                  :append t)
                 (agent-shell--update-header-and-mode-line)
                 (agent-shell--display-session-options)
                 (agent-shell--update-header-and-mode-line)
                 (agent-shell--emit-event :event 'init-session)
                 (funcall on-session-init))
   :on-failure (agent-shell--make-error-handler
                :state agent-shell--state :shell-buffer shell-buffer)))

(defun agent-shell--use-session-load-p (state)
  "Return non-nil when STATE should restore via `session/load'.

`agent-shell-session-restore-verbosity' decides the protocol:

  `last', `first-last', and `full' force `session/load' when the
  agent advertises it (so a replay is available to read from);
  they fall back to `session/resume' otherwise.

  `minimal' uses `session/resume' when available, falling back
  to `session/load' only if the agent doesn't support resume."
  (cond
   ((and (memq agent-shell-session-restore-verbosity '(last first-last full))
         (map-elt state :supports-session-load))
    t)
   ((map-elt state :supports-session-resume)
    nil)
   (t
    (map-elt state :supports-session-load))))

(defun agent-shell--effective-restore-verbosity (state)
  "Return the verbosity in effect for STATE's restore.

Falls back from `agent-shell-session-restore-verbosity' when the
agent's protocol support requires it:

  `minimal' without `session/resume' (but with `session/load') →
  `first-last' (replay via load).

  `last', `first-last', or `full' without `session/load' →
  `minimal' (replay unavailable).

  Otherwise returns the configured value."
  (cond
   ((and (eq agent-shell-session-restore-verbosity 'minimal)
         (not (map-elt state :supports-session-resume))
         (map-elt state :supports-session-load))
    'first-last)
   ((and (memq agent-shell-session-restore-verbosity '(last first-last full))
         (not (map-elt state :supports-session-load)))
    'minimal)
   (t agent-shell-session-restore-verbosity)))

(defun agent-shell--has-pending-restore-p (state)
  "Return non-nil when STATE should buffer notifications during restore.

Only true when the effective verbosity (see
`agent-shell--effective-restore-verbosity') is `last', `first-last',
or `full' and the agent supports `session/load' (so the buffered
prompt turns can be replayed after the load completes).  Buffering
matters because a live shell prompt is shown early: rendering the
replay inline would land it on top of that prompt instead of above
it."
  (and (memq (agent-shell--effective-restore-verbosity state) '(last first-last full))
       (map-elt state :supports-session-load)))

(defun agent-shell--make-pending-restore ()
  "Return a fresh pending-restore accumulator.

Buffers `session/update' notifications grouped by prompt turn so
the turns selected by `agent-shell-session-restore-verbosity' can
be replayed once `session/load' completes."
  (list (cons :prompt-turns (list nil))
        (cons :in-agent-response nil)))

(defun agent-shell--append-restore-notification (state acp-notification)
  "Append ACP-NOTIFICATION into STATE's pending-restore accumulator.

Groups notifications by prompt turn.  A new turn begins when a
`user_message_chunk' arrives after agent activity (agent
message, thought, tool call, or plan) — i.e. the user sending a
fresh prompt in reply to the agent.  Notifications without a
boundary effect are appended to the current turn.

Example.  After appending notifications of these `sessionUpdate'
kinds in order (each NAME below stands for the full notification):

  user_message_chunk, agent_message_chunk, user_message_chunk

STATE's `:pending-restore' `:prompt-turns' holds (newest turn
first, newest notification within first):

  ((user_message_chunk)
   (agent_message_chunk user_message_chunk))"
  (let* ((pending (map-elt state :pending-restore))
         (prompt-turns (map-elt pending :prompt-turns))
         (in-agent-response (map-elt pending :in-agent-response))
         (update-type (map-nested-elt acp-notification '(params update sessionUpdate))))
    (cond
     ((equal update-type "user_message_chunk")
      (when in-agent-response
        (push nil prompt-turns)
        (setq in-agent-response nil)))
     ((member update-type '("agent_message_chunk" "agent_thought_chunk"
                            "tool_call" "tool_call_update" "plan"))
      (setq in-agent-response t)))
    (push acp-notification (car prompt-turns))
    (map-put! pending :prompt-turns prompt-turns)
    (map-put! pending :in-agent-response in-agent-response)))

(defun agent-shell--pop-pending-restore (state)
  "Clear STATE's pending-restore and return its prompt turns.

Returns turns in chronological order (oldest first), with each
turn's notifications in arrival order and empty turns filtered
out.  Returns nil when nothing was buffered."
  (prog1 (seq-filter
          #'identity
          (nreverse
           (mapcar #'nreverse
                   (map-nested-elt state '(:pending-restore :prompt-turns)))))
    (map-put! state :pending-restore nil)))

(defun agent-shell--replay-turn (state turn)
  "Dispatch each notification in TURN through STATE's notification handler."
  (dolist (notification turn)
    (agent-shell--on-notification :state state :acp-notification notification)))

(defun agent-shell--render-pending-restore (state)
  "Replay buffered prompt turns in STATE's pending-restore.

Honors `agent-shell-session-restore-verbosity':

  `last': renders the last prompt turn, preceded by a
          `truncated history' separator when earlier turns exist.

  `first-last': renders the first turn, then a separator when
          more than two turns exist, then the last turn (when
          distinct from the first).

  `full': replays every buffered turn in chronological order.

Notifications are dispatched through `agent-shell--on-notification'
so they render as they would during a live turn.  Clears the
pending-restore state once replay completes."
  (when-let* ((prompt-turns (agent-shell--pop-pending-restore state)))
    ;; Pre-create bootstrapping placeholders so replayed
    ;; notifications (e.g. `available_commands_update') update
    ;; them in place instead of inserting at point-max — which
    ;; would land past the restored conversation.
    (agent-shell--create-bootstrapping-placeholders state)
    ;; Temporarily restore the session/load request so handlers
    ;; that gate on `:active-requests' (out-of-turn check,
    ;; `user_message_chunk' rendering, etc.) treat the replayed
    ;; notifications as if they arrived during the live load.
    (let ((saved-active-requests (map-elt state :active-requests))
          (count (length prompt-turns)))
      (map-put! state :active-requests
                (cons (list (cons :method "session/load")) saved-active-requests))
      (unwind-protect
          ;; A replay renders history the user never typed, so keep all of
          ;; it out of undo history (see `agent-shell--reset-undo-history').
          (let ((buffer-undo-list t)
                (prompt-start (and comint-last-prompt
                                   (marker-position (car comint-last-prompt))
                                   (agent-shell--live-input-prompt-p comint-last-prompt)
                                   (car comint-last-prompt))))
            (when prompt-start
              (set-marker-insertion-type prompt-start t))
            (save-restriction
              (when prompt-start
                ;; Narrowing to exclude early prompt so all replayed
                ;; history is rendered above/before the live prompt.
                (narrow-to-region (point-min) (marker-position prompt-start)))
              (pcase (agent-shell--effective-restore-verbosity state)
                ('last
                 (when (> count 1)
                   (agent-shell--update-bootstrapping-fragment
                    :state state
                    :block-id "restore_truncated_history"
                    :body (agent-shell--make-boxed-message
                           :text "Note: truncated history (last only)")))
                 (agent-shell--replay-turn state (car (last prompt-turns))))
                ('first-last
                 (agent-shell--replay-turn state (car prompt-turns))
                 (when (> count 2)
                   (agent-shell--update-bootstrapping-fragment
                    :state state
                    :block-id "restore_truncated_history"
                    :body (agent-shell--make-boxed-message
                           :text "Note: truncated history (first and last only)")))
                 (when (> count 1)
                   (agent-shell--replay-turn state (car (last prompt-turns)))))
                ('full
                 (dolist (turn prompt-turns)
                   (agent-shell--replay-turn state turn))))
              ;; Close a replay that ended on a user prompt.
              ;; `agent-shell--on-notification' terminates a replayed
              ;; `user_message_chunk' only when the next notification
              ;; arrives, emitting shell-maker's end-of-prompt marker so
              ;; `shell-maker--extract-history' can pair the command with
              ;; its response.  A session whose last turn is a user
              ;; message (an interrupted request, whose final entry is the
              ;; interruption notice) has no next notification, leaving
              ;; the turn open: the live prompt renders on the same line
              ;; as the restored user text, and the next unrelated
              ;; notification (say `available_commands_update') emits the
              ;; marker unnarrowed, landing it after the live prompt.
              (when (equal (map-elt state :last-entry-type) "user_message_chunk")
                (shell-maker-insert-end-of-prompt-marker)
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert (propertize "\n\n"
                                      'field 'output
                                      'read-only t
                                      'front-sticky '(read-only)
                                      'rear-nonsticky '(field read-only))))
                (map-put! state :last-entry-type nil))))
        (map-put! state :active-requests saved-active-requests))
      ;; Replay renders history as a live turn would, so the last replayed
      ;; group is left expanded under `latest'.  Nothing is actually
      ;; running, so fold it like a completed turn.
      (agent-shell--collapse-expanded-activity-group state)
      ;; Replayed history renders through the streaming path, which holds
      ;; back an image ending a message in case a `{width=...}' block
      ;; follows.  No further notification is coming for it, so render it
      ;; now, as at the end of a live turn.
      (agent-shell--render-deferred-images)
      ;; Point followed the narrowed history insertions up above the live
      ;; prompt.  Return it to the input area so the cursor lands where the
      ;; user types (matching pre-early-prompt restore behavior).
      (goto-char (point-max))
      ;; The restored history landed above the early prompt, shifting any
      ;; type-ahead down along with the undo entries recorded for it.
      (agent-shell--reset-undo-history)
      ;; The replayed conversation, including the live prompt, is now
      ;; fully laid down; notify observers that the shell has settled.
      (agent-shell--emit-event :event 'session-restored))))

(cl-defun agent-shell--initiate-session-resume-by-id (&key session-id session-title shell-buffer on-session-init)
  "Resume or load session SESSION-ID with SHELL-BUFFER and ON-SESSION-INIT.

SESSION-TITLE is an optional display title for the resumed session."
  (agent-shell--update-bootstrapping-fragment
   :state (agent-shell--state)
   :block-id "starting"
   :body (format "\n\nLoading session %s..." session-id)
   :append t)
  (unless (eq (agent-shell--effective-restore-verbosity (agent-shell--state))
              agent-shell-session-restore-verbosity)
    (agent-shell--update-bootstrapping-fragment
     :state (agent-shell--state)
     :block-id "restore_fallback"
     :body (agent-shell--make-boxed-message
            :text (format "Warning: %s unsupported. Using %s loading"
                          agent-shell-session-restore-verbosity
                          (agent-shell--effective-restore-verbosity (agent-shell--state))))))
  (let ((use-load (agent-shell--use-session-load-p (agent-shell--state))))
    (when (and use-load (agent-shell--has-pending-restore-p (agent-shell--state)))
      (map-put! (agent-shell--state) :pending-restore
                (agent-shell--make-pending-restore)))
    (agent-shell--send-request
     :state (agent-shell--state)
     :client (map-elt (agent-shell--state) :client)
     :request (let ((cwd (agent-shell--resolve-path (agent-shell-cwd)))
                    (mcp-servers (agent-shell--mcp-servers)))
                (if use-load
                    (acp-make-session-load-request
                     :session-id session-id
                     :cwd cwd
                     :mcp-servers mcp-servers
                     :meta (map-nested-elt (agent-shell--state) '(:agent-config :session-meta)))
                  (acp-make-session-resume-request
                   :session-id session-id
                   :cwd cwd
                   :mcp-servers mcp-servers
                   :meta (map-nested-elt (agent-shell--state) '(:agent-config :session-meta)))))
     :buffer (current-buffer)
     :on-success (lambda (acp-load-response)
                   (agent-shell--set-session-from-response
                    :acp-response acp-load-response
                    :acp-session-id session-id)
                   (agent-shell--update-bootstrapping-fragment
                    :state (agent-shell--state)
                    :block-id "resumed_session"
                    :label-left (format "%s %s"
                                        (agent-shell--make-status-kind-label :status "completed")
                                        (propertize "Resuming session" 'font-lock-face 'agent-shell-section-heading))
                    :expanded t
                    :body (or session-title session-id ""))
                   ;; Replay after bootstrapping fragments (e.g. `Available
                   ;; models') so they sit above the restored conversation.
                   (agent-shell--finalize-session-init
                    :on-session-init (lambda ()
                                       (agent-shell--render-pending-restore (agent-shell--state))
                                       (funcall on-session-init))))
     :on-failure (lambda (_acp-error _raw-message)
                   (map-put! (agent-shell--state) :pending-restore nil)
                   (message "Couldn't resume session. Starting a new one.")
                   (agent-shell--update-bootstrapping-fragment
                    :state (agent-shell--state)
                    :block-id "starting"
                    :body "\n\nCouldn't resume session."
                    :append t)
                   (agent-shell--initiate-session-list-and-load
                    :shell-buffer shell-buffer
                    :on-session-init on-session-init)))))

(cl-defun agent-shell--initiate-session-fork-by-id (&key session-id shell-buffer on-session-init)
  "Fork session SESSION-ID with SHELL-BUFFER and ON-SESSION-INIT."
  (agent-shell--update-bootstrapping-fragment
   :state (agent-shell--state)
   :block-id "starting"
   :body (format "\n\nForking session %s..." session-id)
   :append t)
  (agent-shell--send-request
   :state (agent-shell--state)
   :client (map-elt (agent-shell--state) :client)
   :request (acp-make-session-fork-request
             :session-id session-id
             :cwd (agent-shell--resolve-path (agent-shell-cwd))
             :mcp-servers (agent-shell--mcp-servers)
             :meta (map-nested-elt (agent-shell--state) '(:agent-config :session-meta)))
   :buffer (current-buffer)
   :on-success (lambda (acp-fork-response)
                 (let ((new-session-id (map-elt acp-fork-response 'sessionId)))
                   (unless new-session-id
                     (error "Fork response missing sessionId"))
                   (agent-shell--emit-event
                    :event 'session-selected
                    :data (list (cons :session-id new-session-id)))
                   (agent-shell--set-session-from-response
                    :acp-response acp-fork-response
                    :acp-session-id new-session-id)
                   (agent-shell--update-bootstrapping-fragment
                    :state (agent-shell--state)
                    :block-id "forked_session"
                    :label-left (format "%s %s"
                                        (agent-shell--make-status-kind-label :status "completed")
                                        (propertize "Forked session" 'font-lock-face 'agent-shell-section-heading))
                    :expanded t
                    :body (or new-session-id ""))
                   (agent-shell--finalize-session-init :on-session-init on-session-init)))
   :on-failure (agent-shell--make-error-handler
                :state (agent-shell--state) :shell-buffer shell-buffer)))

(cl-defun agent-shell--initiate-session-list-and-load (&key shell-buffer on-session-init)
  "Try loading latest existing session with SHELL-BUFFER and ON-SESSION-INIT."
  (with-current-buffer (map-elt (agent-shell--state) :buffer)
    (agent-shell--update-bootstrapping-fragment
     :state (agent-shell--state)
     :block-id "starting"
     :body "\n\nLooking for existing sessions..."
     :append t))
  (agent-shell--emit-event :event 'session-list)
  (agent-shell--send-request
   :state (agent-shell--state)
   :client (map-elt (agent-shell--state) :client)
   :request (acp-make-session-list-request
             :cwd (agent-shell--resolve-path (agent-shell-cwd)))
   :buffer (current-buffer)
   :on-success (lambda (acp-response)
                 (let ((acp-sessions (agent-shell--sort-sessions-by-recency
                                      (append (or (map-elt acp-response 'sessions) '()) nil))))
                   (condition-case nil
                       (let* ((acp-session
                               (pcase agent-shell-session-strategy
                                 ('new-deferred nil)
                                 ('new nil)
                                 ('latest (car acp-sessions))
                                 ('prompt (agent-shell--prompt-select-session acp-sessions))
                                 (_ (message "Unknown session strategy '%s', starting a new session"
                                             agent-shell-session-strategy)
                                    nil))))
                         (unless (eq acp-session :other-shell)
                           (let ((acp-session-id (and acp-session
                                                      (map-elt acp-session 'sessionId))))
                             (agent-shell--emit-event
                              :event 'session-selected
                              :data (list (cons :session-id acp-session-id)))
                             (if acp-session-id
                                 (let ((use-load (agent-shell--use-session-load-p (agent-shell--state))))
                                   (agent-shell--update-bootstrapping-fragment
                                    :state (agent-shell--state)
                                    :block-id "starting"
                                    :body (format "\n\nLoading session %s..." acp-session-id)
                                    :append t)
                                   (unless (eq (agent-shell--effective-restore-verbosity (agent-shell--state))
                                               agent-shell-session-restore-verbosity)
                                     (agent-shell--update-bootstrapping-fragment
                                      :state (agent-shell--state)
                                      :block-id "restore_fallback"
                                      :body (agent-shell--make-boxed-message
                                             :text (format "Warning: %s unsupported. Using %s loading"
                                                           agent-shell-session-restore-verbosity
                                                           (agent-shell--effective-restore-verbosity (agent-shell--state))))))
                                   (when (and use-load
                                              (agent-shell--has-pending-restore-p (agent-shell--state)))
                                     (map-put! (agent-shell--state) :pending-restore
                                               (agent-shell--make-pending-restore)))
                                   (agent-shell--send-request
                                    :state (agent-shell--state)
                                    :client (map-elt (agent-shell--state) :client)
                                    :request (let ((cwd (agent-shell--resolve-path (agent-shell-cwd)))
                                                   (mcp-servers (agent-shell--mcp-servers)))
                                               (if use-load
                                                   (acp-make-session-load-request
                                                    :session-id acp-session-id
                                                    :cwd cwd
                                                    :mcp-servers mcp-servers
                                                    :meta (map-nested-elt (agent-shell--state) '(:agent-config :session-meta)))
                                                 (acp-make-session-resume-request
                                                  :session-id acp-session-id
                                                  :cwd cwd
                                                  :mcp-servers mcp-servers
                                                  :meta (map-nested-elt (agent-shell--state) '(:agent-config :session-meta)))))
                                    :buffer (current-buffer)
                                    :on-success (lambda (acp-load-response)
                                                  (agent-shell--set-session-from-response
                                                   :acp-response acp-load-response
                                                   :acp-session-id acp-session-id)
                                                  (agent-shell--update-bootstrapping-fragment
                                                   :state (agent-shell--state)
                                                   :block-id "resumed_session"
                                                   :label-left (format "%s %s"
                                                                       (agent-shell--make-status-kind-label :status "completed")
                                                                       (propertize "Resuming session" 'font-lock-face 'agent-shell-section-heading))
                                                   :expanded t
                                                   :body (or (map-elt acp-session 'title) ""))
                                                  ;; Replay after bootstrapping fragments (e.g.
                                                  ;; `Available models') so they sit above the
                                                  ;; restored conversation.
                                                  (agent-shell--finalize-session-init
                                                   :on-session-init (lambda ()
                                                                      (agent-shell--render-pending-restore (agent-shell--state))
                                                                      (funcall on-session-init))))
                                    :on-failure (lambda (_acp-error _raw-message)
                                                  (map-put! (agent-shell--state) :pending-restore nil)
                                                  (agent-shell--update-bootstrapping-fragment
                                                   :state (agent-shell--state)
                                                   :block-id "restore_fallback"
                                                   :body (agent-shell--make-boxed-message
                                                          :text "Warning: Could not load existing session. Starting new"))
                                                  (agent-shell--initiate-new-session
                                                   :shell-buffer shell-buffer
                                                   :on-session-init on-session-init))))
                               (agent-shell--initiate-new-session
                                :shell-buffer shell-buffer
                                :on-session-init on-session-init)))))
                     (quit
                      (agent-shell--emit-event :event 'session-selection-cancelled)))))
   :on-failure (lambda (_acp-error _raw-message)
                 (agent-shell--update-bootstrapping-fragment
                  :state (agent-shell--state)
                  :block-id "restore_fallback"
                  :body (agent-shell--make-boxed-message
                         :text "Warning: Could not list existing sessions. Starting new"))
                 (agent-shell--initiate-new-session
                  :shell-buffer shell-buffer
                  :on-session-init on-session-init))))

(defun agent-shell--eval-dynamic-values (obj)
  "Recursively evaluate any lambda values in OBJ.
Named functions (symbols) are not evaluated to avoid accidentally
calling external symbols."
  (cond
   ((and (functionp obj) (not (symbolp obj))) (agent-shell--eval-dynamic-values (funcall obj)))
   ((consp obj)
    (cons (agent-shell--eval-dynamic-values (car obj))
          (agent-shell--eval-dynamic-values (cdr obj))))
   (t obj)))

(cl-defun agent-shell--make-mcp-server (&key overrides)
  "Construct a normalized MCP server alist for JSON serialization.

OVERRIDES is a user-supplied server alist whose values replace
the schema defaults.  The transport type is inferred from the
`type' key in OVERRIDES: \"http\" or \"sse\" selects the HTTP/SSE
schema (name, type, url, headers); any other value selects the
default stdio schema (name, command, args, env).

Collection fields default to empty vectors so JSON serialization
produces arrays.  List values in OVERRIDES are converted to
vectors; keys not in the schema are appended to preserve any
user-provided fields.

Example:

  (agent-shell--make-mcp-server
   :overrides \\='((name . \"fs\") (command . \"npx\")))
  => ((name . \"fs\") (command . \"npx\") (args . []) (env . []))"
  (let ((normalized (if (member (map-elt overrides 'type) '("http" "sse"))
                        (list (cons 'name nil)
                              (cons 'type nil)
                              (cons 'url nil)
                              (cons 'headers []))
                      (list (cons 'name nil)
                            (cons 'command nil)
                            (cons 'args [])
                            (cons 'env [])))))
    (map-do (lambda (key value)
              (when (listp value)
                (setq value (vconcat value)))
              (if (map-contains-key normalized key)
                  (map-put! normalized key value)
                (setq normalized (append normalized (list (cons key value))))))
            overrides)
    normalized))

(defun agent-shell--mcp-servers ()
  "Return normalized MCP servers configuration for JSON serialization.

The agent config's `:mcp-servers' take precedence over the global
`agent-shell-mcp-servers'.  Each entry is normalized via
`agent-shell--make-mcp-server'."
  (when-let* ((servers (or (map-nested-elt agent-shell--state '(:agent-config :mcp-servers))
                           agent-shell-mcp-servers)))
    (apply #'vector
           (mapcar (lambda (server)
                     (agent-shell--make-mcp-server
                      :overrides (agent-shell--eval-dynamic-values server)))
                   servers))))

(cl-defun agent-shell--subscribe-to-client-events (&key state)
  "Subscribe SHELL and STATE to ACP events."
  (acp-subscribe-to-errors
   :client (map-elt state :client)
   :on-error (lambda (acp-error)
               (agent-shell--update-fragment
                :state state
                :block-id (format "%s-notices"
                                  (map-elt state :request-count))
                :label-left (propertize "Notices" 'font-lock-face 'agent-shell-section-heading)
                :body (or (map-elt acp-error 'message)
                          (map-elt acp-error 'data)
                          "Something is up ¯\\_ (ツ)_/¯")
                :append t
                :above-last-prompt (not (shell-maker-busy)))))
  (acp-subscribe-to-notifications
   :client (map-elt state :client)
   :on-notification (lambda (acp-notification)
                      (agent-shell--on-notification
                       :state state
                       :acp-notification
                       (agent-shell--adapt-notification
                        :state state
                        :acp-notification acp-notification)
                       )))
  (acp-subscribe-to-requests
   :client (map-elt state :client)
   :on-request (lambda (acp-request)
                 (agent-shell--on-request :state state :acp-request acp-request))))

(defun agent-shell--parse-file-mentions (prompt)
  "Parse @ file mentions from PROMPT string.
Returns list of alists with :start, :end, and :path for each mention."
  (let ((mentions '())
        (pos 0))
    (while (string-match (rx (or line-start (not word))
                             "@"
                             (or (seq "\"" (group (+ (not "\""))) "\"")
                                 (group (+ (not space)))))
                         prompt pos)
      (push `((:start . ,(match-beginning 0))
              (:end . ,(match-end 0))
              (:path . ,(when-let* ((path (or (match-string 1 prompt) (match-string 2 prompt))))
                          (substring-no-properties path))))
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
             (relative-path (map-elt mention :path))
             (expanded-path (expand-file-name relative-path (agent-shell-cwd)))
             (resolved-path (agent-shell--resolve-path expanded-path)))
        ;; Add text before mention
        (when (> start pos)
          (push `((type . "text")
                  (text . ,(substring-no-properties prompt pos start)))
                content-blocks))

        ;; Try to embed or link file
        (condition-case nil
            (let ((file (and (file-readable-p expanded-path)
                             (agent-shell--read-file-content :file-path expanded-path))))
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
               ;; Small enough, text file capabilities granted and embeddedContext supported
               ;; Use ContentBlock::Resource
               ((and agent-shell-text-file-capabilities supports-embedded-context (map-elt file :size)
                     (< (map-elt file :size) agent-shell-embed-file-size-limit))
                (push `((type . "resource")
                        (resource . ((uri . ,(concat "file://" resolved-path))
                                     (,(if (map-elt file :base64-p) 'blob 'text) . ,(map-elt file :content))
                                     (mimeType . ,(map-elt file :mime-type)))))
                      content-blocks))
               ;; File too large, no text file capabilities granted or embeddedContext not supported
               ;; Use resource link
               (t
                (push `((type . "resource_link")
                        (uri . ,(concat "file://" resolved-path))
                        (name . ,relative-path)
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
  :base64-p - t if content is base64-encoded (binary file), nil otherwise
  :content - file content (omitted when SHALLOW is non-nil)"
  (let* ((ext (downcase (or (file-name-extension file-path) "")))
         (file-size (file-attribute-size (file-attributes file-path)))
         (mime-type (or (agent-shell--image-type-to-mime file-path)
                        (mailcap-extension-to-mime ext)))
         (content-fields
          (unless shallow
            (let* ((raw-content (with-temp-buffer
                                  (set-buffer-multibyte nil)
                                  (insert-file-contents-literally file-path)
                                  (buffer-string)))
                   ;; Same heuristic that git uses
                   (is-binary (string-search "\0" raw-content))
                   (content (if is-binary
                                (base64-encode-string raw-content t)
                              (decode-coding-string raw-content 'undecided t))))
              ;; Set a better default MIME type for unknown extensions
              ;; based on the content
              (unless mime-type
                (setq mime-type (if is-binary "application/octet-stream" "text/plain")))
              `((:base64-p . ,is-binary)
                (:content . ,content))))))
    `((:size . ,file-size)
      (:extension . ,ext)
      (:mime-type . ,(or mime-type "text/plain"))
      ,@content-fields)))

(cl-defun agent-shell--load-image (&key file-path (max-width 200))
  "Load image from FILE-PATH and return the image object.

MAX-WIDTH specifies the maximum width in pixels for the image (default 200).
If FILE-PATH is not an image, returns nil."
  (when-let* (((display-graphic-p))
              (metadata (agent-shell--read-file-content :file-path file-path :shallow t))
              (mime-type (map-elt metadata :mime-type))
              ;; Check if it's an image type
              (is-image (string-prefix-p "image/" mime-type))
              (type-supported (image-supported-file-p file-path)))
    (create-image file-path nil nil :max-width max-width)))

(defun agent-shell--data-to-cache-file (data extension)
  "Decode base64 DATA to a cache file named by its md5 with EXTENSION.

Returns the file path, or nil when DATA isn't a string or EXTENSION isn't a
plain alphanumeric extension (so an agent-supplied value can't inject a path
or stray characters into the file name).  The md5 name means identical
payloads reuse the same file."
  (when-let* (((stringp data))
              ((stringp extension))
              ((string-match-p "\\`[a-z0-9]+\\'" extension))
              (file (expand-file-name
                     (format "%s.%s" (md5 data) extension)
                     (agent-shell-cache-dir "content"))))
    (unless (file-exists-p file)
      (let ((coding-system-for-write 'binary))
        (write-region (base64-decode-string data) nil file nil 'silent)))
    file))

(defun agent-shell--content-extension (mime-type)
  "Return a plain file extension for MIME-TYPE, or nil.

The extension is the segment after the last `/', lowercased, accepted only
when it is plain alphanumeric -- so vendor/compound types (e.g.
`application/octet-stream' or `image/svg+xml') return nil rather than an
unusable extension.

Examples:

  (agent-shell--content-extension \"audio/wav\")            => \"wav\"
  (agent-shell--content-extension \"application/pdf\")      => \"pdf\"
  (agent-shell--content-extension \"application/octet-stream\") => nil"
  (when (stringp mime-type)
    (let ((tail (downcase (replace-regexp-in-string "\\`.*/" "" mime-type))))
      (and (string-match-p "\\`[a-z0-9]+\\'" tail) tail))))

(defun agent-shell--image-data-to-file (data mime-type)
  "Write base64-encoded image DATA of MIME-TYPE to a cache file.

Returns the file path, or nil when DATA is missing or MIME-TYPE doesn't map
to a known image extension.  The extension is validated against
`image-file-name-extensions'.  The file is written regardless of whether
 Emacs can inline-render the type, so the link remains openable.

Example:

  (agent-shell--image-data-to-file \"iVBORw0KGgo...\" \"image/png\")
  => \"/home/user/.cache/agent-shell/content/<md5>.png\""
  (when-let* (((stringp mime-type))
              (extension (pcase mime-type
                           ("image/svg+xml" "svg")
                           (_ (string-remove-prefix "image/" mime-type))))
              ((seq-contains-p image-file-name-extensions extension)))
    (agent-shell--data-to-cache-file data extension)))

(defun agent-shell--tool-call-update-output-markdown (acp-update)
  "Return markdown output for ACP-UPDATE, a `tool_call_update' update.

Renders the update's `content' blocks (where the ACP spec carries tool
output) via `agent-shell--content-block-to-markdown'.  Falls back to
`rawOutput.formatted_output' when there are no content blocks.

Returns an empty string when neither carries output.

Examples:

  (agent-shell--tool-call-update-output-markdown
   \\='((content . [((type . \"content\")
                  (content . ((type . \"text\") (text . \"hello\"))))])))
  => \"hello\"

  (agent-shell--tool-call-update-output-markdown
   \\='((rawOutput . ((formatted_output . \"total 0\\n\")))))
  => \"total 0\\n\""
  (let ((content (mapconcat #'agent-shell--content-block-to-markdown
                            (seq-keep (lambda (item) (map-elt item 'content))
                                      (map-elt acp-update 'content))
                            "\n\n"))
        ;; Codex reports terminal output via rawOutput.formatted_output
        ;; and leaves content empty, so completed shell command blocks
        ;; render blank without this fallback.
        ;; See https://github.com/xenodium/agent-shell/pull/763
        (raw-output (map-nested-elt acp-update '(rawOutput formatted_output))))
    (cond ((not (string-empty-p content)) content)
          ;; `rawOutput' is agent-defined free-form JSON, so only
          ;; render formatted_output when it's actually a string.
          ((stringp raw-output) raw-output)
          (t ""))))

(defun agent-shell--content-block-to-markdown (acp-content-block)
  "Return markdown for a `session/update' ACP-CONTENT-BLOCK.

Text blocks return their text.  Image blocks (a content block whose `type'
is \"image\", e.g. an agent returning a screenshot) return a markdown image
so the existing image-rendering path (`agent-shell--render-markdown' with
:render-images t) displays them inline rather than dropping them.

An image block may carry its payload as a `uri' or as base64 `data' (the
spec-required field).  A `uri' (local or remote) is emitted verbatim and
resolved by the renderer, which downloads remote uris on demand (see
`agent-shell-markdown--resolve-image-url').  Base64 `data' is decoded to a
local cache file and emitted as a bare path (not a `file://' URI) so the
renderer resolves it without URI parsing.  An image block with no renderable
payload returns an empty string.

A `resource_link' block returns a markdown link (`name' as the label, `uri'
as the target) so the renderer's link machinery makes it clickable.  An
embedded `resource' block carrying text returns that text as a blockquote.

Binary payloads -- `audio', and an embedded `resource' carrying a `blob' --
are decoded to a cache file and returned as a markdown link.  The cache file
is binary, so the renderer opens it externally (with confirmation) when the
link is followed.

A future block type we don't render yet returns a \"[unsupported content:
TYPE]\" placeholder, so unhandled content stays visible rather than being
silently dropped.

Examples:

  (agent-shell--content-block-to-markdown
   \\='((type . \"text\") (text . \"hello\")))
  => \"hello\"

  (agent-shell--content-block-to-markdown
   \\='((type . \"image\") (uri . \"file:///tmp/shot.png\")))
  => \"\\n\\n![image](file:///tmp/shot.png)\\n\\n\""
  (pcase (map-elt acp-content-block 'type)
    ("text" (or (map-elt acp-content-block 'text) ""))
    ("image"
     (if-let* ((source (or (map-elt acp-content-block 'uri)
                           (agent-shell--image-data-to-file
                            (map-elt acp-content-block 'data)
                            (map-elt acp-content-block 'mimeType)))))
         (format "\n\n![%s](%s)\n\n"
                 (or (map-elt acp-content-block 'name) "image")
                 source)
       ""))
    ("resource_link"
     (if-let* ((uri (map-elt acp-content-block 'uri)))
         (format "\n\n[%s](%s)\n\n" (or (map-elt acp-content-block 'name) uri) uri)
       (or (map-elt acp-content-block 'name) "")))
    ("audio"
     ;; Audio carries only base64 `data' (no uri) -> decode to a cache file
     ;; and link it (labelled \"audio (EXT)\"); the link opens externally
     ;; (binary) when clicked.
     (if-let* ((extension (or (agent-shell--content-extension
                               (map-elt acp-content-block 'mimeType))
                              "bin"))
               (file (agent-shell--data-to-cache-file
                      (map-elt acp-content-block 'data) extension)))
         (format "\n\n[audio (%s)](%s)\n\n" extension file)
       ""))
    ("resource"
     (if-let* ((text (map-nested-elt acp-content-block '(resource text))))
         ;; Embedded text resource -> a blockquote so the content is set apart
         ;; from the agent's prose rather than dropped.
         (concat "\n\n"
                 (mapconcat (lambda (line) (concat "> " line))
                            (split-string text "\n")
                            "\n")
                 "\n\n")
       ;; Embedded binary (blob) resource -> a link to a decoded cache file
       ;; (opens externally when clicked); otherwise a placeholder.
       (if-let* ((file (agent-shell--data-to-cache-file
                        (map-nested-elt acp-content-block '(resource blob))
                        (or (agent-shell--content-extension
                             (map-nested-elt acp-content-block '(resource mimeType)))
                            "bin"))))
           (format "\n\n[%s](%s)\n\n"
                   (if-let* ((uri (map-nested-elt acp-content-block '(resource uri))))
                       (file-name-nondirectory uri)
                     "resource")
                   file)
         (format "[unsupported content: %s]"
                 (or (map-elt acp-content-block 'type) "resource")))))
    (type (format "[unsupported content: %s]" (or type "unknown")))))

(cl-defun agent-shell--collect-attached-files (content-blocks)
  "Collect attached resource uris from CONTENT-BLOCKS."
  (mapcan
   (lambda (content-block)
     (let ((type (map-elt content-block 'type)))
       (cond
        ((equal type "resource") (list (map-nested-elt content-block '(resource uri))))
        ((equal type "resource_link") (list (map-elt content-block 'uri)))
        ((equal type "image") (list (map-elt content-block 'uri)))
        (t nil))))
   content-blocks))

(cl-defun agent-shell--display-attached-files (uris)
  "Display the attached URIS in the buffer."
  (with-current-buffer (map-elt agent-shell--state :buffer)
    (let ((follow (eobp)))
      (agent-shell--update-fragment
       :state agent-shell--state
       :block-id "attached-files"
       :label-left (format "%d file%s attached"
                           (length uris)
                           (if (= (length uris) 1) "" "s"))
       :body (mapconcat (lambda (f) (format "• %s" f))
                        (nreverse uris)
                        "\n")
       :create-new t)
      (when follow
        (goto-char (point-max))))))

(defun agent-shell--set-session-title (title)
  "Set the current session's title to TITLE and emit `session-title-changed'.
Does nothing if TITLE is empty or matches the current value."
  (when (and (stringp title)
             (not (string-empty-p title))
             (not (equal (map-nested-elt agent-shell--state '(:session :title)) title)))
    (map-put! (map-elt agent-shell--state :session) :title title)
    (agent-shell--emit-event :event 'session-title-changed
                             :data (list (cons :title title)))))

(defun agent-shell--refresh-session-title (&optional _event)
  "Refresh `(:session :title)' by fetching from agent.

Sends a `session/list' ACP request and writes any non-empty `title'
field on the matching session via `agent-shell--set-session-title'.  Agents
that don't supply a title (e.g. Claude Code) are no-ops; the seeded
first-prompt title is left in place.

Does nothing if the agent doesn't advertise the `list' session
capability, since the `session/list' request would otherwise fail."
  (when-let* ((_ (map-elt agent-shell--state :supports-session-list))
              (client (map-elt agent-shell--state :client))
              (session-id (map-nested-elt agent-shell--state '(:session :id))))
    (acp-send-request
     :client client
     :request (acp-make-session-list-request
               :cwd (agent-shell--resolve-path default-directory))
     :buffer (current-buffer)
     :on-success
     (lambda (acp-response)
       (when-let* ((acp-session (seq-find
                                 (lambda (acp-session)
                                   (equal (map-elt acp-session 'sessionId) session-id))
                                 (append (or (map-elt acp-response 'sessions) '()) nil))))
         (agent-shell--set-session-title (map-elt acp-session 'title)))))))

(defun agent-shell--expand-truncated-regions (prompt)
  "Expand truncated regions in PROMPT marked with `agent-shell-region-id'.
Each marked span is replaced by its `agent-shell-region-text' value."
  (agent-shell-with-work-buffer
    (insert prompt)
    (goto-char (point-min))
    (let (match)
      (while (setq match (text-property-search-forward
                          'agent-shell-region-id nil
                          (lambda (_ val) val)))
        (when-let* ((full-text (get-text-property
                                (prop-match-beginning match)
                                'agent-shell-region-text))
                    (beg (prop-match-beginning match)))
          (delete-region beg (prop-match-end match))
          (goto-char beg)
          (insert full-text))))
    (buffer-string)))

(defconst agent-shell--steered-entry-type "steered_user_message"
  "`:last-entry-type' left by a steered prompt.

Deliberately not \"user_message_chunk\": that value asks the replay
path to insert the end-of-prompt marker on the next notification, and
`agent-shell--render-steered-prompt' has already inserted its own.")

(cl-defun agent-shell--render-steered-prompt (&key state prompt)
  "Render PROMPT into STATE's shell as the user prompt it is.

A steered prompt is never echoed back: neither the Claude nor the Codex
adapter forwards it as a `user_message_chunk' while the turn runs, so
the client renders it or it does not appear at all.

Uses the field/face shape comint gives a live prompt, so
`comint-next-prompt', `agent-shell-next-item', copying and screen-reader
prompt navigation treat it as a user prompt rather than as a new kind of
entry.  `shell-maker-insert-end-of-prompt-marker' then closes it, which
is what bounds the prompt for anything measuring it by searching forward
for the marker; without it such a search runs to the next submission and
takes the rest of the turn's output as this prompt's body.

The `[steered]' prefix carries the one thing this shape would otherwise
lose -- that the prompt was injected into a running turn rather than
typed before one."
  (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count)))
  (agent-shell--append-transcript
   :text (format "## User (steered) (%s)\n\n%s\n\n"
                 (format-time-string "%F %T")
                 (agent-shell--indent-markdown-headers prompt))
   :file-path agent-shell--transcript-file)
  (agent-shell--update-text
   :state state
   :block-id (format "%s-steered-user_message_chunk"
                     (map-elt state :chunked-group-count))
   :text (concat (propertize (map-nested-elt state '(:agent-config :shell-prompt))
                             'font-lock-face '(agent-shell-prompt comint-highlight-prompt)
                             'field 'output)
                 (propertize (concat "[steered] " (substring-no-properties prompt))
                             'font-lock-face 'agent-shell-input))
   :create-new t)
  (with-current-buffer (map-elt state :buffer)
    (shell-maker-insert-end-of-prompt-marker))
  (map-put! state :last-entry-type agent-shell--steered-entry-type))

(cl-defun agent-shell--send-command (&key prompt shell-buffer)
  "Send PROMPT to agent using SHELL-BUFFER."
  (let* ((expanded-prompt (agent-shell--expand-truncated-regions prompt))
         (content-blocks (condition-case nil
                             (agent-shell--build-content-blocks expanded-prompt)
                           (error `[((type . "text")
                                     (text . ,(substring-no-properties expanded-prompt)))])))
         (attached-files (agent-shell--collect-attached-files content-blocks)))
    (when attached-files
      (agent-shell--display-attached-files attached-files))
    (when agent-shell-show-busy-indicator
      (agent-shell-heartbeat-start
       :heartbeat (map-elt agent-shell--state :heartbeat)))

    (agent-shell--cancel-idle-timer)
    (agent-shell--emit-event :event 'input-submitted
                             :data (list (cons :prompt (substring-no-properties expanded-prompt))))

    (map-put! agent-shell--state :last-entry-type nil)

    ;; Seed the session title with the first user prompt so consumers
    ;; (e.g. agent-shell-manager) have something to display before any
    ;; agent-supplied title arrives.
    (unless (map-nested-elt agent-shell--state '(:session :title))
      (agent-shell--set-session-title (substring-no-properties prompt)))

    (agent-shell--append-transcript
     :text (format "## User (%s)\n\n%s\n\n"
                   (format-time-string "%F %T")
                   (agent-shell--indent-markdown-headers expanded-prompt))
     :file-path agent-shell--transcript-file)

    (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                                  :shell-buffer shell-buffer
                                  :existing-only t)))
      (with-current-buffer viewport-buffer
        ;; Refresh the viewport to show the just-sent prompt, but only
        ;; when it's displaying the conversation. Don't interrupt
        ;; any potential prompt crafting (ie. edit mode).
        (when (derived-mode-p 'agent-shell-viewport-view-mode)
          (agent-shell-viewport--initialize
           :prompt prompt))))

    (agent-shell--send-request
     :state agent-shell--state
     :client (map-elt agent-shell--state :client)
     :request (acp-make-session-prompt-request
               :session-id (map-nested-elt agent-shell--state '(:session :id))
               :prompt content-blocks)
     :buffer (current-buffer)
     :on-success (lambda (acp-response)
                   (agent-shell--separate-transcript-after-agent-message
                    :last-entry-type (map-elt (agent-shell--state) :last-entry-type)
                    :file-path agent-shell--transcript-file)
                   ;; Tool call details are no longer needed after
                   ;; a session prompt request is finished.
                   ;; Avoid accumulating them unnecessarily.
                   (map-put! (agent-shell--state) :tool-calls nil)
                   ;; The turn is over, so nothing is active any more: fold
                   ;; the last activity group `latest' left expanded.
                   (agent-shell--collapse-expanded-activity-group (agent-shell--state))
                   ;; Extract usage information from response
                   (when (map-elt acp-response 'usage)
                     (agent-shell--save-usage :state (agent-shell--state) :acp-usage (map-elt acp-response 'usage)))
                   (let ((success (equal (map-elt acp-response 'stopReason)
                                         "end_turn")))
                     ;; Display usage box at end of turn if enabled and data available
                     (when (and success
                                agent-shell-show-usage-at-turn-end
                                (agent-shell--usage-has-data-p (map-elt (agent-shell--state) :usage)))
                       (agent-shell--update-fragment
                        :state (agent-shell--state)
                        :block-id (format "%s-usage" (map-elt (agent-shell--state) :request-count))
                        :label-left (propertize "Usage" 'font-lock-face 'agent-shell-section-heading)
                        :body (agent-shell--format-usage (map-elt (agent-shell--state) :usage) t)
                        :create-new t))
                     (unless success
                       (agent-shell--update-fragment
                        :state (agent-shell--state)
                        :block-id (format "%s-stop-reason"
                                          (map-elt (agent-shell--state) :request-count))
                        :body (agent-shell--stop-reason-description
                               (map-elt acp-response 'stopReason))
                        :create-new t))
                     (agent-shell-heartbeat-stop
                      :heartbeat (map-elt agent-shell--state :heartbeat))
                     (unless success
                       (agent-shell--prompt-queue-display))
                     ;; No more chunks are coming, so markup the streaming
                     ;; passes held back for one (a trailing image) can
                     ;; render now.  Runs whatever the stop reason: an
                     ;; interrupted turn leaves the same markup raw.
                     (agent-shell--render-deferred-images)
                     (shell-maker-finish-output :config shell-maker--config
                                                :success t)
                     (let ((data (list (cons :stop-reason (map-elt acp-response 'stopReason))
                                       (cons :usage (map-elt (agent-shell--state) :usage)))))
                       (agent-shell--emit-event
                        :event 'turn-complete
                        :data data)
                       (agent-shell--start-idle-timer :event 'turn-complete :data data))
                     ;; Update viewport header (longer busy)
                     (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                                                   :shell-buffer shell-buffer
                                                   :existing-only t)))
                       (with-current-buffer viewport-buffer
                         (agent-shell-viewport--update-header)))
                     (when success
                       (agent-shell--prompt-queue-process-next))))
     :on-failure (lambda (acp-error raw-message)
                   ;; A failed/interrupted turn may have stopped mid
                   ;; agent_message_chunk, leaving the transcript body
                   ;; without a trailing newline.  Separate it so the
                   ;; next section header lands on its own line.
                   (agent-shell--separate-transcript-after-agent-message
                    :last-entry-type (map-elt agent-shell--state :last-entry-type)
                    :file-path agent-shell--transcript-file)
                   ;; An interrupted turn leaves no group active either.
                   (agent-shell--collapse-expanded-activity-group agent-shell--state)
                   ;; Display pending requests on failure.
                   (agent-shell--prompt-queue-display)
                   (funcall (agent-shell--make-error-handler :state agent-shell--state :shell-buffer shell-buffer)
                            acp-error raw-message)
                   (agent-shell-heartbeat-stop
                    :heartbeat (map-elt agent-shell--state :heartbeat))
                   ;; Update viewport header (longer busy)
                   (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                                                 :shell-buffer shell-buffer
                                                 :existing-only t)))
                     (with-current-buffer viewport-buffer
                       (agent-shell-viewport--update-header)))))))

;;; Projects

(defun agent-shell-project-buffers ()
  "Return all shell buffers in the same project as current buffer."
  (let ((project-root (agent-shell-cwd)))
    (seq-filter (lambda (buffer)
                  (equal project-root
                         (with-current-buffer buffer
                           (agent-shell-cwd))))
                (agent-shell-buffers))))

(cl-defun agent-shell--shell-buffer (&key viewport-buffer no-error no-create)
  "Get an `agent-shell' buffer for the current project.

Resolution order:
1. If VIEWPORT-BUFFER is provided, derive shell buffer from its name.
2. If inside of a viewport buffer, derive shell buffer from its name.
3. If currently in an `agent-shell-mode' buffer, return it.
4. If there are shells in current project, return the first one found.
5. Otherwise, ask user to pick one.

When NO-CREATE is nil (default), prompt to create a new shell if none exists.
When NO-CREATE is non-nil, return existing shell or nil/error if none exists.
When NO-ERROR is non-nil, return nil instead of raising an error.

Returns a buffer object or nil."
  (let ((shell-buffer (or (agent-shell-viewport--shell-buffer
                           (or viewport-buffer (current-buffer)))
                          (if (derived-mode-p 'agent-shell-mode)
                              (current-buffer)
                            (seq-first (agent-shell-project-buffers))))))
    (if shell-buffer
        shell-buffer
      (if no-create
          (unless no-error
            (user-error "No agent shell buffers available for current project"))
        (if (and (eq agent-shell-session-strategy 'new-deferred)
                 (agent-shell-buffers))
            ;; TODO: Consolidate choices with `agent-shell--prompt-select-session'.
            (let* ((choices (agent-shell--apply-session-choices
                             (list (cons "New shell" :new-shell)
                                   (cons "New Downloads shell" :downloads-shell)
                                   (cons "New temp shell" :temp-shell)
                                   (cons "Switch to shell buffer" :other-shell))))
                   (selection (if (length= choices 1)
                                  ;; Only one choice available; follow it without prompting.
                                  (caar choices)
                                (completing-read "Start shell (default: new): " choices nil t))))
              (pcase (map-elt choices selection)
                (:other-shell
                 (agent-shell--read-shell-buffer :prompt "Switch to shell buffer: "))
                (:downloads-shell
                 (agent-shell-new-downloads-shell :no-display t))
                (:temp-shell
                 (agent-shell-new-temp-shell :no-display t))
                (_
                 (agent-shell--start :config (or (agent-shell--auto-preferred-config)
                                                 (agent-shell-select-config
                                                  :prompt "Start new agent: ")
                                                 (error "No agent config found"))
                                     :no-focus t
                                     :new-session t))))
          (agent-shell--start :config (or (agent-shell--auto-preferred-config)
                                          (agent-shell-select-config
                                           :prompt "Start new agent: ")
                                          (error "No agent config found"))
                              :no-focus t
                              :new-session t
                              :session-strategy agent-shell-session-strategy))))))

(defun agent-shell-goto-last-interaction ()
  "Move point to the last interaction in the shell buffer."
  (when-let* ((shell-buffer (agent-shell--shell-buffer)))
    (with-current-buffer shell-buffer
      (goto-char comint-last-input-start))))

(defun agent-shell--shell-response-start ()
  "Return where the response of the interaction at point begins.
Return nil when point is not on an interaction with a response.  The
position sits right after the `<shell-maker-end-of-prompt>' delimiter, so
it aligns with the start of the response text copied into the viewport."
  (save-excursion
    (when-let* ((begin (ignore-errors (shell-maker--prompt-begin-position))))
      (goto-char begin)
      (when (re-search-forward "<shell-maker-end-of-prompt>" nil t)
        (point)))))

(defun agent-shell--point-location (prompt-start response-start)
  "Return point's location as an alist, or nil.
The alist has :region (:prompt or :response) and :offset (point's distance
from that region's start).  :region is :response when point is at or past
RESPONSE-START, else :prompt when at or past PROMPT-START.  PROMPT-START and
RESPONSE-START are buffer positions or nil.

The prompt and response text is shared between a shell and its viewport, so
a location captured in one maps 1:1 into the other.  For example, point 5
characters into the response returns:

  ((:region . :response) (:offset . 5))"
  (cond ((and response-start (>= (point) response-start))
         `((:region . :response) (:offset . ,(- (point) response-start))))
        ((and prompt-start (>= (point) prompt-start))
         `((:region . :prompt) (:offset . ,(- (point) prompt-start))))))

(defun agent-shell-interaction-at-point ()
  "Return the interaction at point in the shell buffer.
Result is of the form ((:prompt . PROMPT) (:response . RESPONSE))."
  (when-let* ((shell-buffer (agent-shell--shell-buffer))
              (result (with-current-buffer shell-buffer
                        (or (shell-maker--command-and-response-at-point :trimmed nil)
                            (shell-maker-next-command-and-response t :trimmed nil)))))
    `((:prompt . ,(car result))
      (:response . ,(cdr result)))))

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

;;;###autoload
(cl-defun agent-shell-shell-buffer (&key viewport-buffer no-error no-create)
  "Return an `agent-shell' buffer for the current context.

A stable public API wrapping the internal resolver, intended for
packages that integrate with `agent-shell' programmatically.

Resolution order: viewport → current buffer → project buffers → prompt user.

VIEWPORT-BUFFER resolves from that viewport's shell buffer.  NO-ERROR
returns nil instead of signaling when none is found.  NO-CREATE skips
creating a buffer.

Example:
  (agent-shell-shell-buffer)
  (agent-shell-shell-buffer :no-error t)"
  (agent-shell--shell-buffer :viewport-buffer viewport-buffer
                             :no-error no-error
                             :no-create no-create))

(defun agent-shell--input ()
  "Return shell input (not yet submitted).
Return nil while the shell is busy and the region past the process mark
holds read-only agent-rendered output (e.g. a permission request) rather
than editable user input."
  (when-let* ((shell-buffer (agent-shell--shell-buffer)))
    (with-current-buffer shell-buffer
      ;; Based on `comint-kill-input' to get latest input.
      (when-let* ((start (or (marker-position comint-accum-marker)
                             (process-mark (get-buffer-process (current-buffer)))))
                  (input (buffer-substring start (point-max)))
                  ((not (string-empty-p (string-trim input))))
                  ;; Editable input only; agent-rendered UI here is read-only.
                  ((not (text-property-not-all start (point-max) 'read-only nil))))
        input))))

;;; Shell

(defun agent-shell-insert-shell-command-output ()
  "Execute a shell command and insert output as a code block.

The command executes asynchronously.  When finished, the output is
inserted into the shell buffer prompt."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (unless (or (derived-mode-p 'agent-shell-viewport-view-mode)
              (derived-mode-p 'agent-shell-viewport-edit-mode)
              (derived-mode-p 'agent-shell-mode))
    (user-error "Not in an `agent-shell' buffer"))
  (let* ((command (read-string "insert command output: "))
         (shell-buffer (or (agent-shell--current-shell)
                           (user-error "No shell available")))
         (destination-buffer (if (or (derived-mode-p 'agent-shell-viewport-view-mode)
                                     (derived-mode-p 'agent-shell-viewport-edit-mode))
                                 (agent-shell-viewport--buffer
                                  :shell-buffer shell-buffer)
                               shell-buffer))
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
                :command (with-current-buffer shell-buffer
                           (agent-shell--build-command-for-execution
                            (list shell-file-name
                                  shell-command-switch
                                  ;; Merge stderr into stdout output
                                  ;; (all into output buffer)
                                  (format "%s 2>&1" command))))
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
                    (let ((code-block (format "```shell
%s
```" (with-current-buffer output-buffer
       (buffer-string)))))
                      (if (with-current-buffer shell-buffer (shell-maker-busy))
                          (with-current-buffer shell-buffer
                            (agent-shell-prompt-queue
                             (agent-shell--prompt-queue-read
                              :initial (concat code-block "\n\n"))))
                        (with-current-buffer destination-buffer
                          (save-excursion
                            (goto-char (point-max))
                            (insert "\n\n" code-block))
                          (agent-shell--render-markdown))))
                    (when (buffer-live-p output-buffer)
                      (kill-buffer output-buffer)))))))
    (set-process-query-on-exit-flag proc nil)
    (run-at-time "0.2 sec" nil
                 (lambda ()
                   (unless (equal (process-status proc) 'exit)
                     (agent-shell--display-buffer output-buffer))))))

;;; Completion

(cl-defun agent-shell--get-files-context (&key files agent-cwd)
  "Process FILES into sendable text with image preview if applicable.

Uses AGENT-CWD to shorten file paths where necessary."
  (when files
    (mapconcat (lambda (file)
                 (when agent-cwd
                   (setq file (expand-file-name file agent-cwd)))
                 (if-let* ((image-display (agent-shell--load-image
                                           :file-path file
                                           :max-width 200)))
                     ;; Propertize text to display the image
                     (agent-shell--make-file-link
                      :label (propertize (concat "@" file)
                                         'display image-display
                                         'pointer 'hand
                                         'agent-shell-context-image t
                                         'modification-hooks
                                         ;; Delete entire image if any of it is deleted.
                                         (list (lambda (edit-start edit-end)
                                                 (when-let* (((get-text-property edit-start 'agent-shell-context-image))
                                                             (image-start (or (previous-single-property-change
                                                                               (1+ edit-start) 'agent-shell-context-image)
                                                                              (point-min)))
                                                             (image-end (or (next-single-property-change
                                                                             edit-start 'agent-shell-context-image)
                                                                            (point-max)))
                                                             (inhibit-modification-hooks t))
                                                   (when (> image-end edit-end)
                                                     (delete-region edit-end image-end))
                                                   (when (< image-start edit-start)
                                                     (delete-region image-start edit-start))))))
                      :file file
                      ;; No link face for image (no underline).
                      :face nil
                      :hint "open image")
                   ;; Not an image, insert as normal text
                   (agent-shell--make-file-link
                    :label (if (and agent-cwd (file-in-directory-p file agent-cwd))
                               ;; File within project, shorten path.
                               (propertize (concat "@" (file-relative-name file agent-cwd))
                                           'pointer 'hand)
                             (propertize (concat "@" file)
                                         'pointer 'hand))
                    :file file
                    :hint "open file")))
               files
               "\n\n")))

(defun agent-shell-send-file (&optional prompt-for-file pick-shell)
  "Insert a file into `agent-shell'.

If visiting a file, send this file.

If invoked from shell, select a project file.

If invoked from `dired', use selection or region files.

With prefix argument PROMPT-FOR-FILE, always prompt for file selection.

When PICK-SHELL is non-nil, prompt for which shell buffer to use."
  (interactive "P")
  (if (and (region-active-p)
           (buffer-file-name))
      (agent-shell-send-region)
    (let* ((in-shell (derived-mode-p 'agent-shell-mode))
           (files (if (or in-shell prompt-for-file)
                      (list (completing-read "Send file: " (agent-shell--project-files)))
                    (or (agent-shell--buffer-files)
                        (when (buffer-file-name)
                          (list (buffer-file-name)))
                        (list (completing-read "Send file: " (agent-shell--project-files)))
                        (user-error "No file to send"))))
           (shell-buffer (when pick-shell
                           (agent-shell--read-shell-buffer
                            :prompt "Send file to shell: "))))
      (agent-shell-insert :text (agent-shell--get-files-context :files files)
                          :shell-buffer shell-buffer))))

(defun agent-shell-send-file-to (&optional prompt-for-file)
  "Like `agent-shell-send-file' but prompt for which shell to use.

With prefix argument PROMPT-FOR-FILE, always prompt for file selection."
  (interactive "P")
  (agent-shell-send-file prompt-for-file t))

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

(defun agent-shell-send-screenshot (&optional pick-shell)
  "Capture a screenshot and insert it into `agent-shell'.

The screenshot is saved to .agent-shell/screenshots in the project root.
The captured screenshot file path is then inserted into the shell prompt.

When PICK-SHELL is non-nil, prompt for which shell buffer to use."
  (interactive)
  (let* ((screenshots-dir (agent-shell--dot-subdir "screenshots"))
         (screenshot-path (agent-shell--capture-screenshot :destination-dir screenshots-dir))
         (shell-buffer (when pick-shell
                         (agent-shell--read-shell-buffer
                          :prompt "Send screenshot to shell: "))))
    (agent-shell-insert
     :text (agent-shell--get-files-context :files (list screenshot-path))
     :shell-buffer shell-buffer)))

(defun agent-shell-send-screenshot-to ()
  "Like `agent-shell-send-screenshot' but prompt for which shell to use."
  (interactive)
  (agent-shell-send-screenshot t))

(defun agent-shell-send-clipboard-image (&optional pick-shell)
  "Paste clipboard image and insert it into `agent-shell'.

Needs external utilities.  See `agent-shell-clipboard-image-handlers'
for details.

The image is saved to .agent-shell/screenshots in the project root.
The saved image file path is then inserted into the shell prompt.

When PICK-SHELL is non-nil, prompt for which shell buffer to use."
  (interactive "P")
  (unless (window-system)
    (user-error "Clipboard image requires a window system"))
  (let* ((screenshots-dir (agent-shell--dot-subdir "screenshots"))
         (image-path (agent-shell--save-clipboard-image :destination-dir screenshots-dir))
         (shell-buffer (when pick-shell
                         (agent-shell--read-shell-buffer
                          :prompt "Send image to shell: "))))
    (agent-shell-insert
     :text (agent-shell--get-files-context :files (list image-path))
     :shell-buffer shell-buffer)))

(defun agent-shell-send-clipboard-image-to ()
  "Like `agent-shell-send-clipboard-image' but prompt for which shell to use."
  (interactive)
  (agent-shell-send-clipboard-image t))

;; Inherit yank's `delete-selection' property so
;; `delete-selection-mode' replaces the active region on paste.
(put 'agent-shell-yank-dwim 'delete-selection 'yank)
(defun agent-shell-yank-dwim (&optional arg)
  "Yank or paste clipboard image into `agent-shell'.

If the clipboard contains an image, save it and insert as file context.
Otherwise, invoke `yank' with ARG as usual.

Needs external utilities.  See `agent-shell-clipboard-image-handlers'
for details."
  (interactive "*P")
  (if-let* (((window-system))
            (screenshots-dir (agent-shell--dot-subdir "screenshots"))
            (image-path (agent-shell--save-clipboard-image :destination-dir screenshots-dir
                                                           :no-error t)))
      (agent-shell-insert
       :text (agent-shell--get-files-context :files (list image-path))
       :shell-buffer (agent-shell--shell-buffer))
    (yank arg)))

;;; Permissions

(cl-defun agent-shell--append-title-detail (&key text detail)
  "Return TEXT with DETAIL appended, or DETAIL when TEXT is nil.

TEXT is returned as-is when it ends in a fenced code block.  Appending to
the closing fence line would leave the block unterminated and the
markdown renderer would show the fences verbatim.  See
https://github.com/xenodium/agent-shell/issues/767.

For example:

  TEXT \"edit\" and DETAIL \"foo.rs\"
  => \"edit (foo.rs)\"

  TEXT \"```console\\nls -la\\n```\" and DETAIL \"/home/user\"
  => \"```console\\nls -la\\n```\""
  (cond ((null text)
         detail)
        ((string-suffix-p "```" (string-trim-right text))
         text)
        (t
         (concat (string-trim-right text) " (" detail ")"))))

(cl-defun agent-shell--permission-title (&key tool-call)
  "Build a display title for a permission dialog from TOOL-CALL.

Combines the ACP ToolCall \\='title, \\='rawInput-derived command and
filepath, and the structured \\='content and \\='locations fields
\(when the agent provides them) into a user-facing string.

Simple substring deduplication avoids showing the same info
twice when an agent populates both \\='rawInput and \\='content,
or when a \\='locations path is already mentioned in title or
\\='content.

For example:

  TOOL-CALL with title \"edit\" and filepath \"/home/user/foo.rs\"
  => \"edit (foo.rs)\"

  TOOL-CALL with title \"Bash\" and command \"ls -la\"
  => \"```console\\nls -la\\n```\"

  TOOL-CALL with title \"emacs_eval-elisp\", kind \"other\",
  and rawInput ((expression . \"(+ 1 2 3)\"))
  => \"emacs_eval-elisp\\n\\n```\\n(+ 1 2 3)\\n```\"

  TOOL-CALL with title \"Bash\", command \"ls -la\"
  and locations ((path . \"/home/user\"))
  => \"```console\\nls -la\\n```\""
  (let* ((title (map-elt tool-call :title))
         (raw-input (map-elt tool-call :raw-input))
         (command (agent-shell--tool-call-command-to-string
                   (map-elt raw-input 'command)))
         ;; Some tools put a non-string under `path' (e.g. an HTTP API's
         ;; path params), so pick the first string, like the `locations'
         ;; paths guard below.
         (filepath (seq-find #'stringp
                             (list (map-elt raw-input 'filepath)
                                   (map-elt raw-input 'fileName)
                                   (map-elt raw-input 'path)
                                   (map-elt raw-input 'file_path))))
         ;; Fetch tools (eg. OpenCode's webfetch) put the target URL
         ;; under `url'.  Surface it in full below, since the basename
         ;; alone isn't enough to decide whether to allow the request.
         (url (seq-find #'stringp (list (map-elt raw-input 'url))))
         (content-texts
          (delq nil
                (mapcar (lambda (item)
                          (when-let* ((item-text (map-nested-elt item '(content text)))
                                      ((stringp item-text))
                                      ((not (string-empty-p item-text))))
                            item-text))
                        (append (map-elt tool-call :content) nil))))
         (location-paths
          (delq nil
                (mapcar (lambda (loc)
                          (when-let* ((path (map-elt loc 'path))
                                      ((stringp path))
                                      ((not (string-empty-p path))))
                            path))
                        (append (map-elt tool-call :locations) nil))))
         ;; Some agents don't include the command in the
         ;; permission/tool call title, so it's hard to know
         ;; what the permission is actually allowing.
         ;; Display command if needed.
         (text (if (and (stringp title)
                        (stringp command)
                        (not (string-empty-p command))
                        (string-match-p (regexp-quote command) title))
                   title
                 (or command title))))
    ;; Append filename to title when available and not
    ;; already included, so the user can see which file
    ;; the permission applies to.
    (when-let* ((filename (and filepath
                               (file-name-nondirectory filepath)))
                ((not (string-empty-p filename)))
                ((or (not text)
                     (not (string-match-p (regexp-quote filename) text)))))
      (setq text (agent-shell--append-title-detail :text text :detail filename)))
    ;; Append the URL to the title when available and not already
    ;; included, so the user can see which URL the permission applies
    ;; to.  Unlike filepaths, keep the full URL (not just its basename).
    ;; See https://github.com/xenodium/agent-shell/issues/745
    (when-let* ((url)
                ((not (string-empty-p url)))
                ((or (not text)
                     (not (string-match-p (regexp-quote url) text)))))
      (setq text (agent-shell--append-title-detail :text text :detail url)))
    ;; Fence execute commands so the markdown renderer
    ;; renders them verbatim, not as markdown.
    (when (and text
               (equal text command)
               (equal (map-elt tool-call :kind) "execute"))
      (setq text (concat "```console\n" text "\n```")))
    ;; For "other"/unspecified-kind tools (typically MCP), surface
    ;; the `rawInput' value when there is a single stringy field —
    ;; agents like OpenCode put the meaningful detail there
    ;; (e.g. emacs_eval-elisp's `expression') without populating
    ;; the structured `content' channel that Claude Code uses.
    (when-let* (((member (map-elt tool-call :kind) '(nil "other")))
                ((null command))
                ((null filepath))
                ((= (length raw-input) 1))
                (input-value (cdar raw-input))
                ((stringp input-value))
                ((not (string-empty-p input-value)))
                ((or (not text)
                     (not (string-match-p (regexp-quote input-value) text))))
                ((not (seq-find (lambda (c-text)
                                  (string-match-p (regexp-quote input-value) c-text))
                                content-texts))))
      (let ((fenced (agent-shell--format-tool-call-input raw-input)))
        (setq text (if text (concat text "\n\n" fenced) fenced))))
    ;; Fold in ACP `content' text blocks attached to the tool call.
    ;; Skip blocks whose text is already a substring of what we
    ;; have (e.g. Claude mirrors `rawInput.description' in `content').
    (dolist (content-text content-texts)
      (unless (and text
                   (string-match-p (regexp-quote content-text) text))
        (setq text (if text
                       (concat text "\n\n" content-text)
                     content-text))))
    ;; Fold in ACP `locations' paths.  Skip paths (or their
    ;; basenames) already present in the displayed text — covers
    ;; both filesystem paths the `rawInput' branch already
    ;; surfaced and URLs/commands embedded in `content' text.
    (dolist (path location-paths)
      (when-let* ((basename (file-name-nondirectory path))
                  ((or (not text)
                       (not (string-match-p (regexp-quote path) text))))
                  ((or (not text)
                       (equal basename path)
                       (string-empty-p basename)
                       (not (string-match-p (regexp-quote basename) text)))))
        (setq text (agent-shell--append-title-detail :text text :detail path))))
    text))

(cl-defun agent-shell--make-tool-call-permission-text (&key tool-call tool-call-id client state)
  "Create text to render permission dialog for TOOL-CALL.

TOOL-CALL is the saved tool-call alist; TOOL-CALL-ID identifies it
within STATE.  CLIENT is the ACP client used to send the response.

For example:

   ╭─

       ⚠ Tool Permission ⚠

       Add more cowbell

       [ View (v) ] [ Allow (y) ] [ Reject (n) ] [ Always Allow (!) ]


   ╰─"
  (let* ((actions (map-elt tool-call :permission-actions))
         (shell-buffer (map-elt state :buffer))
         (keymap (let ((map (make-sparse-keymap)))
                   (dolist (action actions)
                     (when-let* ((char (map-elt action :char)))
                       (define-key map (kbd char)
                                   (lambda ()
                                     (interactive)
                                     (agent-shell--send-permission-response
                                      :client client
                                      :request-id (map-elt tool-call :permission-request-id)
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
                   (when (map-elt tool-call :diffs)
                     (define-key map "v" (agent-shell--make-diff-viewing-function
                                          :diffs (map-elt tool-call :diffs)
                                          :actions actions
                                          :client client
                                          :request-id (map-elt tool-call :permission-request-id)
                                          :state state
                                          :tool-call-id tool-call-id)))
                   ;; Add interrupt keybinding
                   (define-key map (kbd "C-c C-c")
                               (lambda ()
                                 (interactive)
                                 (with-current-buffer shell-buffer
                                   (agent-shell-interrupt t))))
                   map))
         (title (agent-shell--permission-title :tool-call tool-call))
         (diff-button (when (map-elt tool-call :diffs)
                        (agent-shell--make-permission-button
                         :text "View (v)"
                         :help "Press v to view diff"
                         :action (agent-shell--make-diff-viewing-function
                                  :diffs (map-elt tool-call :diffs)
                                  :actions actions
                                  :client client
                                  :request-id (map-elt tool-call :permission-request-id)
                                  :state state
                                  :tool-call-id tool-call-id)
                         :keymap keymap
                         :navigatable t
                         :char "v"
                         :option "view diff"))))
    (format "╭─

    %s %s %s%s


    %s%s


╰─"
            (propertize agent-shell-permission-icon
                        'font-lock-face 'agent-shell-warning)
            (propertize "Tool Permission" 'font-lock-face 'agent-shell-permission-title)
            (propertize agent-shell-permission-icon
                        'font-lock-face 'agent-shell-warning)
            (if title
                (propertize
                 (format "\n\n\n    %s" title)
                 'font-lock-face 'agent-shell-input)
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
                                     :request-id (map-elt tool-call :permission-request-id)
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
  ;; Kill any diff buffer opened for this tool call, suppressing the
  ;; on-exit callback since the permission is already being resolved.
  (when-let* ((diff-buf (map-nested-elt state (list :tool-calls tool-call-id :diff-buffer))))
    (agent-shell-diff-kill-buffer diff-buf))
  ;; Ensure in the shell buffer for state operations, as this
  ;; function may be invoked from a viewport buffer.
  (with-current-buffer (map-elt state :buffer)
    ;; Hide permission after sending response.
    ;; block-id must be the same as the one used as
    ;; agent-shell--update-fragment param by "session/request_permission".
    (agent-shell--delete-fragment :state state :block-id (format "permission-%s" tool-call-id))
    ;; Note: Tool call data is no longer deleted here intentionally.
    ;; Subsequent tool_call_update notifications still need the data.
    ;; It gets cleared at end of turn with all tool calls.
    ;;
    ;; Do clear :permission-request-id so consumers can distinguish
    ;; between a pending permission request and one already answered.
    (when-let* ((tool-calls (map-elt state :tool-calls))
                (tool-call (map-elt tool-calls tool-call-id)))
      (map-put! tool-calls tool-call-id
                (map-delete tool-call :permission-request-id)))
    (agent-shell--cancel-idle-timer)
    (agent-shell--emit-event
     :event 'permission-response
     :data (list (cons :request-id request-id)
                 (cons :tool-call-id tool-call-id)
                 (cons :option-id option-id)
                 (cons :cancelled cancelled)))
    (when message-text
      (message "%s" message-text))
    ;; Jump to any remaining permission buttons, or go to end of buffer.
    (or (agent-shell-jump-to-latest-permission-button-row)
        (goto-char (point-max)))
    (when-let* (((map-elt state :buffer))
                (viewport-buffer (agent-shell-viewport--buffer
                                  :shell-buffer (map-elt state :buffer)
                                  :existing-only t)))
      (with-current-buffer viewport-buffer
        (or (agent-shell-jump-to-latest-permission-button-row)
            (goto-char (point-max)))))))

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

(defun agent-shell--diffs-title (diffs)
  "Return a header-line title for DIFFS.

Returns the file name when DIFFS has a single entry, or a \"N files\"
summary when it has several."
  (cond ((null diffs) nil)
        ((= (length diffs) 1)
         (when-let* ((file (map-elt (car diffs) :file)))
           (file-name-nondirectory file)))
        (t (format "%d files" (length diffs)))))

(cl-defun agent-shell--make-diff-viewing-function (&key diffs actions client request-id state tool-call-id)
  "Create a diffing handler for the ACP CLIENT's REQUEST-ID and TOOL-CALL-ID.

DIFFS as per `agent-shell--make-diff-infos'.
ACTIONS as per `agent-shell--make-permission-action'."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (let ((shell-buffer (current-buffer)))
    (lambda ()
      (interactive)
      (if-let* ((existing (map-nested-elt state (list :tool-calls tool-call-id :diff-buffer)))
                ((buffer-live-p existing)))
          (pop-to-buffer existing '((display-buffer-reuse-window
                                     display-buffer-use-some-window
                                     display-buffer-same-window)))
        (let ((diff-buffer
               (agent-shell-diff
                :diffs diffs
                :title (agent-shell--diffs-title diffs)
                :on-accept (lambda ()
                             (interactive)
                             (let ((action (agent-shell--resolve-permission-choice-to-action
                                            :choice 'accept
                                            :actions actions)))
                               (with-current-buffer shell-buffer
                                 (agent-shell--send-permission-response
                                  :client client
                                  :request-id request-id
                                  :option-id (map-elt action :option-id)
                                  :state state
                                  :tool-call-id tool-call-id
                                  :message-text (map-elt action :option)))))
                :on-reject (lambda ()
                             (interactive)
                             (when (agent-shell-interrupt-confirmed-p)
                               (with-current-buffer shell-buffer
                                 (agent-shell-interrupt t))))
                :on-exit (lambda ()
                           (if-let* ((choice (condition-case nil
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
                             (message "Ignored"))))))
          ;; Track the diff buffer in tool-call state so it can be
          ;; cleaned up when the permission is resolved externally.
          (when-let* ((tool-calls (map-elt state :tool-calls)))
            (map-put! tool-calls tool-call-id
                      (map-insert (map-elt tool-calls tool-call-id)
                                  :diff-buffer diff-buffer))))))))

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
      ;; Graphical: " Allow (y) "
      ;;
      ;; Terminal: "[ Allow (y) ]"
      ;;
      ;; so adjust the offsets accordingly.
      (let ((trailing (if (display-graphic-p) 2 3)))
        (put-text-property (- (length button) (+ trailing 1))
                           (- (length button) trailing)
                           'agent-shell-permission-button t button)
        (put-text-property (- (length button) (+ trailing 1))
                           (- (length button) trailing)
                           'cursor-sensor-functions
                           (list (lambda (_window _old-pos sensor-action)
                                   (when (eq sensor-action 'entered)
                                     (if char
                                         (message "Press RET or %s to %s" char option)
                                       (message "Press RET to %s" option)))))
                           button)))
    button))

(defconst agent-shell--permission-kind-order
  '("allow_once" "reject_once" "allow_always" "reject_always")
  "Display order for permission options, by ACP kind.

Agents send options in whichever order they like (some list rejection
first), but the dialog always offers allowing before rejecting.")

(defun agent-shell--permission-action-rank (action)
  "Return the sort rank of ACTION, derived from its ACP kind.

Unknown kinds sort last.  See `agent-shell--permission-kind-order'."
  (or (seq-position agent-shell--permission-kind-order (map-elt action :kind))
      (length agent-shell--permission-kind-order)))

(defun agent-shell--make-permission-actions (acp-options)
  "Make actions from ACP-OPTIONS for shell rendering.

Actions are sorted by `agent-shell--permission-kind-order', ignoring the
order the agent sent them in.

See `agent-shell--make-permission-action' for ACP-OPTION and return schema."
  (let (acp-seen-kinds)
    (seq-sort (lambda (a b)
                (< (agent-shell--permission-action-rank a)
                   (agent-shell--permission-action-rank b)))
              (delq nil (mapcar (lambda (acp-option)
                                  (let ((action (agent-shell--make-permission-action
                                                 :acp-option acp-option
                                                 :acp-seen-kinds acp-seen-kinds)))
                                    (push (map-elt acp-option 'kind) acp-seen-kinds)
                                    action))
                                acp-options)))))

(cl-defun agent-shell--make-permission-action (&key acp-option acp-seen-kinds)
  "Convert a single ACP-OPTION to an action alist.

ACP-OPTION should be a PermissionOption per ACP spec:

  https://agentclientprotocol.com/protocol/schema#permissionoption

  An alist of the form:

  ((\='kind . \"allow_once\")
   (\='name . \"Allow\")
   (\='optionId . \"allow\"))

ACP-SEEN-KINDS is a list of kinds already processed.  If kind is in
ACP-SEEN-KINDS, omit the keybinding to avoid duplicates.

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
         (char (unless (member kind acp-seen-kinds)
                 (map-elt char-map kind)))
         (name (map-elt acp-option 'name)))
    (when (map-elt char-map kind)
      (map-into `((:label . ,(if char (format "%s (%s)" name char) name))
                  (:option . ,name)
                  (:char . ,char)
                  (:kind . ,kind)
                  (:option-id . ,(map-elt acp-option 'optionId)))
                'alist))))

(defun agent-shell-jump-to-latest-permission-button-row ()
  "Jump to the latest permission button row.

Moves point to the first button of the latest permission row and syncs
that position into every window showing the buffer, across frames.  The
row is thus revealed even when the shell window is not the selected one
\(for example when a prompt bar has focus); a bare `goto-char' would only
move point in the selected window.  When the buffer is not displayed,
only its point moves, so a later display still shows the row.

Returns non-nil if a permission button was found, nil otherwise."
  (declare (modes agent-shell-mode))
  (interactive)
  (when-let* ((found (save-mark-and-excursion
                       (goto-char (point-max))
                       (agent-shell-previous-permission-button))))
    (deactivate-mark)
    (let ((target (save-excursion
                    (goto-char found)
                    (beginning-of-line)
                    (agent-shell-next-permission-button)
                    (point))))
      (goto-char target)
      (dolist (window (get-buffer-window-list (current-buffer) nil t))
        (set-window-point window target)))
    t))

(defun agent-shell-next-permission-button ()
  "Jump to the next button."
  (declare (modes agent-shell-mode))
  (interactive)
  (when-let* ((found (save-mark-and-excursion
                       (when (get-text-property (point) 'agent-shell-permission-button)
                         (when-let* ((next-change (next-single-property-change (point) 'agent-shell-permission-button)))
                           (goto-char next-change)))
                       (when-let* ((next (text-property-search-forward
                                          'agent-shell-permission-button t t)))
                         (prop-match-beginning next)))))
    (deactivate-mark)
    (goto-char found)
    found))

(defun agent-shell-previous-permission-button ()
  "Jump to the previous button."
  (declare (modes agent-shell-mode))
  (interactive)
  (when-let* ((found (save-mark-and-excursion
                       (when (get-text-property (point) 'agent-shell-permission-button)
                         (when-let* ((prev-change (previous-single-property-change (point) 'agent-shell-permission-button)))
                           (goto-char prev-change)))
                       (when-let* ((prev (text-property-search-backward
                                          'agent-shell-permission-button t t)))
                         (prop-match-beginning prev)))))
    (deactivate-mark)
    (goto-char found)
    found))

;;; Region

(cl-defun agent-shell--insert-to-shell-buffer (&key shell-buffer text submit no-focus)
  "Insert TEXT into the agent shell buffer at `point-max'.

SHELL-BUFFER, when non-nil, specifies the target shell buffer.
Otherwise, uses `agent-shell--shell-buffer' to find one.

SUBMIT, when non-nil, submits the shell buffer after insertion.

NO-FOCUS, when non-nil, avoid focusing shell on insertion.

Returns an alist with insertion details or nil otherwise:

  ((:buffer . BUFFER)
   (:start . START)
   (:end . END))"
  (unless text
    (user-error "No text provided to insert"))
  (let* ((shell-buffer (or shell-buffer
                           (agent-shell--shell-buffer :no-create t))))
    (if (with-current-buffer shell-buffer
          (or (map-nested-elt agent-shell--state '(:session :id))
              (eq agent-shell-session-strategy 'new-deferred)
              ;; Inserting text now is also possible if there's
              ;; a prompt available to insert text into.
              (and (not submit) comint-last-prompt)))
        ;; Displaying before with-current-buffer below
        ;; ensures window is selected, thus window-point
        ;; is also updated after insertion.
        (let* ((inhibit-read-only t)
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
                ;; TODO: Render prompt markdown?
                ))
            (when submit
              (shell-maker-submit)))
          `((:buffer . ,shell-buffer)
            (:start . ,insert-start)
            (:end . ,insert-end)))
      (let ((token nil))
        (setq token
              (agent-shell-subscribe-to
               :shell-buffer shell-buffer
               :event 'prompt-ready
               :on-event (lambda (_event)
                           (agent-shell-unsubscribe :subscription token)
                           (agent-shell--insert-to-shell-buffer
                            :text text :submit submit
                            :no-focus no-focus :shell-buffer shell-buffer))))))))

(cl-defun agent-shell-insert (&key text submit no-focus shell-buffer)
  "Insert TEXT into the agent shell at `point-max'.

SUBMIT, when non-nil, submits the shell buffer after insertion.

NO-FOCUS, when non-nil, avoid focusing shell on insertion.

Use SHELL-BUFFER for insertion.

When `agent-shell-prefer-viewport-interaction' is non-nil, prefer inserting
into the viewport compose buffer instead of the shell buffer.  If no compose
buffer exists, one will be created.

Returns an alist with insertion details or nil otherwise:

  ((:buffer . BUFFER)
   (:start . START)
   (:end . END))

Uses optional SHELL-BUFFER to make paths relative to shell project."
  (if (and (not (derived-mode-p 'agent-shell-mode))
           (or agent-shell-prefer-viewport-interaction
               (derived-mode-p 'agent-shell-viewport-edit-mode)
               (derived-mode-p 'agent-shell-viewport-view-mode)))
      (agent-shell-viewport--show-buffer :append text :submit submit
                                         :no-focus no-focus :shell-buffer shell-buffer)
    (agent-shell--insert-to-shell-buffer :text text :submit submit
                                         :no-focus no-focus :shell-buffer shell-buffer)))

(cl-defun agent-shell-send-region (&optional pick-shell)
  "Send region to last accessed shell buffer in project.

When PICK-SHELL is non-nil, prompt for which shell buffer to use."
  (interactive)
  (let* ((shell-buffer (or (when pick-shell
                             (agent-shell--read-shell-buffer
                              :prompt "Send region to shell: "))
                           (agent-shell--shell-buffer)))
         (text (agent-shell--get-region-context
                :deactivate t
                :agent-cwd (with-current-buffer shell-buffer
                             (agent-shell-cwd)))))
    (if (with-current-buffer shell-buffer (shell-maker-busy))
        (with-current-buffer shell-buffer
          (agent-shell-prompt-queue
           (agent-shell--prompt-queue-read :initial (concat text "\n\n"))))
      (agent-shell-insert :text text :shell-buffer shell-buffer))))

(defun agent-shell-send-region-to ()
  "Like `agent-shell-send-region' but prompt for which shell to use."
  (interactive)
  (agent-shell-send-region t))

(cl-defun agent-shell-send-dwim (&optional arg)
  "Send region or error at point to last accessed shell buffer in project.

With \\[universal-argument] prefix ARG, force start a new shell.

With \\[universal-argument] \\[universal-argument] prefix ARG, prompt to pick an existing shell."
  (interactive "P")
  (cond
   ;; `agent-shell--dwim' already carries the context to the chosen shell
   ;; (deferring until the session is selected when needed), so let it own
   ;; the send rather than inserting a second time here.
   ((equal arg '(16))
    (agent-shell--dwim :switch-to-shell t))
   ((equal arg '(4))
    (agent-shell--dwim :new-shell t))
   (t
    (let* ((shell-buffer (agent-shell--shell-buffer))
           (text (agent-shell--context :shell-buffer shell-buffer)))
      (if (with-current-buffer shell-buffer (shell-maker-busy))
          (with-current-buffer shell-buffer
            (agent-shell-prompt-queue
             (agent-shell--prompt-queue-read :initial (concat text "\n\n"))))
        (agent-shell-insert :text text :shell-buffer shell-buffer))))))

(cl-defun agent-shell--get-region-context (&key deactivate no-error agent-cwd)
  "Get region as insertable text, ready for sending to agent.

When DEACTIVATE is non-nil, deactivate region.

When NO-ERROR is non-nil, return nil and continue without error.

Uses AGENT-CWD to shorten file paths where necessary."
  (let* ((region (or (agent-shell--get-region :deactivate deactivate)
                     (unless no-error
                       (user-error "No region selected"))))
         (processed-text (if (map-elt region :file)
                             (let ((file-link (agent-shell--make-file-link
                                               :label (format "%s:%d-%d"
                                                              (if (and agent-cwd (file-in-directory-p (map-elt region :file) agent-cwd))
                                                                  (file-relative-name (map-elt region :file) agent-cwd)
                                                                (map-elt region :file))
                                                              (map-elt region :line-start)
                                                              (map-elt region :line-end))
                                               :file (map-elt region :file)
                                               :line-start (map-elt region :line-start)
                                               :line-end (map-elt region :line-end)
                                               :hint "open file"))
                                   (numbered-preview
                                    (when-let* ((buffer (get-file-buffer (map-elt region :file))))
                                      (let ((char-start (map-elt region :char-start))
                                            (char-end (map-elt region :char-end))
                                            (max-preview-lines 5))
                                        (if (= (count-lines char-start char-end) 1)
                                            ;; Same line region? Avoid numbering.
                                            (agent-shell--buffer-substring-with-faces
                                             char-start char-end)
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

(defun agent-shell--buffer-substring-with-faces (start end)
  "Return text between START and END, preserving only face properties."
  (let ((text (buffer-substring start end))
        (pos 0))
    (while (< pos (length text))
      (let ((next (or (next-property-change pos text) (length text)))
            (props (text-properties-at pos text))
            remove-props)
        (while props
          (unless (memq (car props) '(face font-lock-face))
            (setq remove-props (plist-put remove-props (car props) nil)))
          (setq props (cddr props)))
        (when remove-props
          (remove-text-properties pos next remove-props text))
        (setq pos next)))
    text))

(cl-defun agent-shell--get-numbered-region (&key buffer from to cap trim)
  "Get region from BUFFER between FROM and TO locations.

Expands to include entire lines.

When TRIM is non-nil, trim empty lines from beginning and end.

If CAP is non-nil, truncate at CAP."
  (with-current-buffer buffer
    (save-excursion
      (goto-char from)
      (let* ((start-line (line-number-at-pos from))
             (end-line (save-excursion
                         (goto-char to)
                         (when (and (bolp) (not (bobp)))
                           (backward-char))
                         (line-number-at-pos)))
             (lines '())
             (current-line start-line))
        (goto-char (point-min))
        (forward-line (1- start-line))
        (while (<= current-line end-line)
          (let ((line-content (agent-shell--buffer-substring-with-faces
                               (line-beginning-position)
                               (line-end-position))))
            (push (concat (format "   %d: " current-line) line-content)
                  lines))
          (forward-line 1)
          (setq current-line (1+ current-line)))
        (let ((reversed-lines (nreverse lines)))
          (when trim
            ;; Trim empty lines from the beginning
            (while (and reversed-lines
                        (string-match-p "^   [0-9]+:[[:space:]]*$" (car reversed-lines)))
              (setq reversed-lines (cdr reversed-lines)))
            ;; Trim empty lines from the end
            (setq reversed-lines (nreverse reversed-lines))
            (while (and reversed-lines
                        (string-match-p "^   [0-9]+:[[:space:]]*$" (car reversed-lines)))
              (setq reversed-lines (cdr reversed-lines)))
            (setq reversed-lines (nreverse reversed-lines)))
          ;; Apply cap before final join
          (let ((final-lines reversed-lines))
            (if-let* (((and cap (> (length final-lines) cap)))
                      (full-text (string-join final-lines "\n"))
                      (id (gensym "agent-shell-region-")))
                (agent-shell--add-text-properties
                 (concat (string-join (seq-take final-lines cap) "\n")
                         "\n\n   "
                         (agent-shell--make-button
                          :text "Expand..."
                          :help "RET to expand"
                          :action
                          (lambda ()
                            (interactive)
                            (save-excursion
                              (goto-char (point-min))
                              (when-let* ((match (text-property-search-forward
                                                  'agent-shell-region-id id t))
                                          (inhibit-read-only t))
                                (delete-region (prop-match-beginning match)
                                               (prop-match-end match))
                                (goto-char (prop-match-beginning match))
                                (insert full-text))))))
                 'agent-shell-region-id id
                 'agent-shell-region-text full-text)
              (string-join final-lines "\n"))))))))


(cl-defun agent-shell--format-diagnostic (&key buffer beg end line col type text)
  "Format a diagnostic error with context.
BUFFER is the buffer containing the error.
BEG and END are the error region positions.
LINE and COL are the line and column numbers.
TYPE is the error type/level.
TEXT is the error message."
  (let* ((file (agent-shell--shorten-paths (buffer-file-name buffer) t))
         (code (when (and beg end)
                 (with-current-buffer buffer
                   (buffer-substring beg end))))
         (context-lines 3)
         (context (when beg
                    (with-current-buffer buffer
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
                                                 :to context-end
                                                 :trim t))
                               ;; Replace the line number prefix for the error line
                               (error-line-prefix (format "   %d:" line))
                               (highlight-prefix (format "-> %d:" line)))
                          (replace-regexp-in-string
                           (regexp-quote error-line-prefix)
                           highlight-prefix
                           numbered-region
                           nil 'literal)))))))
    (if (or (not code) (string-empty-p (string-trim code)))
        (format "%s:%d:%d: %s: %s"
                (or file (buffer-name buffer))
                line (or col 0) type text)
      (format "%s:%d:%d: %s: %s\n\n%s"
              (or file (buffer-name buffer))
              line (or col 0) type text context))))

(defun agent-shell--get-flymake-error-context ()
  "Get flymake error at point, ready for sending to agent."
  (when-let* ((diagnostics (flymake-diagnostics (point))))
    (mapconcat
     (lambda (diagnostic)
       (let* ((buffer (flymake-diagnostic-buffer diagnostic))
              (beg (flymake-diagnostic-beg diagnostic))
              (end (flymake-diagnostic-end diagnostic))
              (type (flymake-diagnostic-type diagnostic))
              (text (flymake-diagnostic-text diagnostic))
              (line (with-current-buffer buffer
                      (line-number-at-pos beg)))
              (col (with-current-buffer buffer
                     (save-excursion
                       (goto-char beg)
                       (current-column)))))
         (agent-shell--format-diagnostic
          :buffer buffer
          :beg beg
          :end end
          :line line
          :col col
          :type type
          :text text)))
     diagnostics
     "\n\n")))

(defun agent-shell--get-flycheck-error-context ()
  "Get flycheck error at point, ready for sending to agent."
  (when-let* (((bound-and-true-p flycheck-mode))
              ((fboundp 'flycheck-overlay-errors-at))
              (errors (flycheck-overlay-errors-at (point))))
    (mapconcat
     (lambda (err)
       (let* ((buffer (current-buffer))
              (beg (flycheck-error-pos err))
              (end (when beg
                     (save-excursion
                       (goto-char beg)
                       (if-let* ((end-line (flycheck-error-end-line err))
                                 (end-col (flycheck-error-end-column err)))
                           (progn
                             (forward-line (- end-line (line-number-at-pos)))
                             (move-to-column end-col)
                             (point))
                         beg))))
              (type (flycheck-error-level err))
              (text (flycheck-error-message err))
              (line (flycheck-error-line err))
              (col (flycheck-error-column err)))
         (agent-shell--format-diagnostic
          :buffer buffer
          :beg beg
          :end end
          :line line
          :col col
          :type type
          :text text)))
     errors
     "\n\n")))

(defun agent-shell--get-error-context ()
  "Get error at point from either flymake or flycheck, whichever is available.
Tries flymake first, then flycheck."
  (or (agent-shell--get-flymake-error-context)
      (agent-shell--get-flycheck-error-context)))

(cl-defun agent-shell--get-current-line-context (&key agent-cwd)
  "Get the current line as insertable text, ready for sending to agent.

Uses AGENT-CWD to shorten file paths where necessary."
  (save-excursion
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (goto-char start)
      (set-mark end)
      (activate-mark)
      (agent-shell--get-region-context :deactivate t :no-error t :agent-cwd agent-cwd))))

(cl-defun agent-shell--context (&key shell-buffer)
  "Return context (if available).  Nil otherwise.

Uses optional SHELL-BUFFER to make paths relative to shell project.

Context could be either a region or error at point or files.
The sources checked are controlled by `agent-shell-context-sources'."
  (unless (and (derived-mode-p 'agent-shell-mode)
               (not (region-active-p)))
    (let ((agent-cwd (when shell-buffer
                       (with-current-buffer shell-buffer
                         (agent-shell-cwd)))))
      (seq-some
       (lambda (source)
         (pcase source
           ('files (agent-shell--get-files-context
                    :files (agent-shell--buffer-files :obvious t)
                    :agent-cwd agent-cwd))
           ('region (agent-shell--get-region-context
                     :deactivate t :no-error t
                     :agent-cwd agent-cwd))
           ('error (agent-shell--get-error-context))
           ('line (agent-shell--get-current-line-context
                   :agent-cwd agent-cwd))
           ((pred functionp) (funcall source))))
       agent-shell-context-sources))))

(cl-defun agent-shell--get-region (&key deactivate)
  "Get the active region as an alist.

When DEACTIVATE is non-nil, deactivate region/selection.

Available values:

 :file :language :char-start :char-end :line-start :line-end and :content."
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end))
          (content (buffer-substring-no-properties (region-beginning) (region-end)))
          (language (string-remove-suffix "-mode" (string-remove-suffix "-ts-mode" (symbol-name major-mode))))
          (file (buffer-file-name)))
      (when deactivate
        (deactivate-mark))
      `((:file . ,file)
        (:language . ,language)
        (:char-start . ,start)
        (:char-end . ,end)
        (:line-start . ,(line-number-at-pos start))
        (:line-end . ,(save-excursion
                        (goto-char end)
                        (when (and (bolp) (not (bobp)))
                          (backward-char))
                        (line-number-at-pos)))
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
                           (string-trim-right
                            (string-join
                             (seq-mapn (lambda (cell width)
                                         (format (format "%%-%ds" width) (or cell "")))
                                       row
                                       widths)
                             separator)))
                         rows)))
    (if joiner
        (string-join result joiner)
      result)))

(cl-defun agent-shell--get-decorated-region (&key deactivate)
  "Get the active region decorated with file path and Markdown code block.

When DEACTIVATE is non-nil, deactivate region/selection."
  (when-let* ((region-data (agent-shell--get-region :deactivate deactivate)))
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

(defun agent-shell--block-quote (text)
  "Return TEXT with each line prefixed by \"> \", displayed as a bar.

Underlying text keeps the \"> \" so it remains valid markdown;
the bar is a display-only override.  Yanks strip both the bar
styling and the leading \"> \" so paste gives plain text."
  (let* ((bar      (propertize "▌" 'face 'agent-shell-markdown-blockquote))
         (wrap     (propertize "▌ " 'face 'agent-shell-markdown-blockquote))
         (quoted   (concat "> " (replace-regexp-in-string
                                 (rx "\n") "\n> " text)))
         (rendered (copy-sequence quoted))
         (pos      0))
    (add-text-properties
     0 (length rendered)
     (list 'wrap-prefix wrap
           'face 'agent-shell-markdown-blockquote
           'yank-handler
           (list (lambda (s)
                   (insert
                    (replace-regexp-in-string
                     (rx line-start "> ") ""
                     (substring-no-properties s))))))
     rendered)
    (while (string-match (rx line-start ">") rendered pos)
      (put-text-property (match-beginning 0) (match-end 0)
                         'display bar rendered)
      (setq pos (match-end 0)))
    rendered))

;;; Session modes

(defun agent-shell--get-available-modes (state)
  "Get available modes list, preferring session modes over agent modes.

STATE is the agent shell state.

Returns the modes list from session if available, otherwise from
the agent's available modes."
  (if-let* ((mode-option (agent-shell--config-option-by-category state "mode")))
      (agent-shell--config-option-as-modes mode-option)
    (or (map-nested-elt state '(:session :modes))
        ;; Use agent-level availability as fallback.
        (map-nested-elt state '(:available-modes :modes)))))

(defun agent-shell--resolve-session-mode-name (mode-id available-session-modes)
  "Get the name of the session mode with MODE-ID from AVAILABLE-SESSION-MODES.

AVAILABLE-SESSION-MODES is the list of mode objects from the ACP
session/new response.  Each mode has an `:id' and `:name' field.
We look up the mode by ID to get its display name.

See https://agentclientprotocol.com/protocol/session-modes for details."
  (when-let* ((mode (seq-find (lambda (m)
                                (string= mode-id (map-elt m :id)))
                              available-session-modes)))
    (map-elt mode :name)))

(defun agent-shell-get-model-name (state)
  "Get the current model name from STATE.

Returns the model name if available, otherwise returns nil.
Prefers config option data when available."
  (let ((model-id (agent-shell--current-model-id state)))
    (or (map-elt (seq-find (lambda (model)
                             (string= (map-elt model :model-id)
                                      model-id))
                           (agent-shell--get-available-models state))
                 :name)
        model-id)))

(defun agent-shell-get-mode-name (state)
  "Get the current session mode name from STATE.

Returns the mode name if available, otherwise returns nil.
Prefers config option data when available."
  (when-let* ((mode-id (agent-shell--current-mode-id state)))
    (or (agent-shell--resolve-session-mode-name
         mode-id
         (agent-shell--get-available-modes state))
        mode-id)))

(defun agent-shell-get-thought-level-name (state)
  "Return current thought level display name from STATE or nil."
  (when-let* ((option (agent-shell--config-option-by-category state "thought_level"))
              (current (map-elt option :current-value)))
    (agent-shell--config-option-value-name option current)))

(defun agent-shell--busy-indicator-frame ()
  "Return busy frame string or nil if not busy."
  (when-let* ((agent-shell-show-busy-indicator)
              ((eq 'busy (map-nested-elt (agent-shell--state) '(:heartbeat :status))))
              (frames (pcase agent-shell-busy-indicator-frames
                        ('circle '("●" "●" "●" "●" "●" " " " " " " " "  " "))
                        ('wave '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" "▇" "▆" "▅" "▄" "▃" "▂"))
                        ('dots-block '("⣷" "⣯" "⣟" "⡿" "⢿" "⣻" "⣽" "⣾"))
                        ('dots-round '("⢎⡰" "⢎⡡" "⢎⡑" "⢎⠱" "⠎⡱" "⢊⡱" "⢌⡱" "⢆⡱"))
                        ('wide '("░   " "░░  " "░░░ " "░░░░" "░░░ " "░░  " "░   " "    "))
                        ((pred listp) agent-shell-busy-indicator-frames)
                        (_ '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" "▇" "▆" "▅" "▄" "▃" "▂"))))
              (value (map-nested-elt (agent-shell--state) '(:heartbeat :value))))
    (concat " " (seq-elt frames (mod value (length frames))))))

(defun agent-shell--mode-line-model-menu ()
  "Build a menu keymap for selecting a model from the mode line.

For example: clicking \"[Sonnet]\" shows a popup with all available models."
  (let ((menu (make-sparse-keymap "LLM model"))
        (shell-buffer (agent-shell--shell-buffer)))
    (seq-do
     (lambda (model)
       (define-key menu (vector (intern (concat "model-" (map-elt model :model-id))))
                   `(menu-item ,(map-elt model :name)
                               (lambda () (interactive)
                                 (with-current-buffer ,shell-buffer
                                   (agent-shell--config-option-set-model-id
                                    :model-id ,(map-elt model :model-id))))
                               :button (:toggle . ,(equal (map-elt model :model-id)
                                                          (agent-shell--current-model-id (agent-shell--state)))))))
     (reverse (agent-shell--get-available-models (agent-shell--state))))
    menu))

(defun agent-shell--mode-line-mode-menu ()
  "Build a menu keymap for selecting a session mode from the mode line.

For example: clicking \"[Accept Edits]\" shows a popup with all available modes."
  (let ((menu (make-sparse-keymap "Session mode"))
        (shell-buffer (agent-shell--shell-buffer)))
    (seq-do
     (lambda (mode)
       (define-key menu (vector (intern (concat "mode-" (map-elt mode :id))))
                   `(menu-item ,(map-elt mode :name)
                               (lambda () (interactive)
                                 (with-current-buffer ,shell-buffer
                                   (agent-shell--config-option-set-mode-id
                                    :mode-id ,(map-elt mode :id))))
                               :button (:toggle . ,(equal (map-elt mode :id)
                                                          (agent-shell--current-mode-id (agent-shell--state)))))))
     (reverse (agent-shell--get-available-modes (agent-shell--state))))
    menu))

(defun agent-shell--mode-line-thought-level-menu ()
  "Build a menu keymap for selecting a thought level from the mode line.

Clicking the thought level segment in the header/mode-line shows this
popup."
  (let ((menu (make-sparse-keymap "Thought level"))
        (shell-buffer (agent-shell--shell-buffer))
        (current-id (agent-shell--current-thought-level-id (agent-shell--state))))
    (seq-do
     (lambda (value)
       (define-key menu (vector (intern (concat "thought-level-" (map-elt value :value))))
                   `(menu-item ,(map-elt value :name)
                               (lambda () (interactive)
                                 (with-current-buffer ,shell-buffer
                                   (agent-shell--config-option-set-thought-level-id
                                    :thought-level-id ,(map-elt value :value))))
                               :button (:toggle . ,(equal (map-elt value :value) current-id)))))
     (reverse (agent-shell--get-available-thought-levels (agent-shell--state))))
    menu))

(defun agent-shell--mode-line-combined-menu ()
  "Build a combined menu keymap exposing all session settings.

The graphical SVG header renders as a single image, so clicks cannot
target individual segments.  Clicking anywhere on it pops up this menu,
which groups the per-segment menus (LLM model, thought level, session
mode) into submenus."
  (let ((menu (make-sparse-keymap "Agent shell")))
    (when (agent-shell-get-mode-name (agent-shell--state))
      (define-key menu [mode]
                  `(menu-item "Session mode" ,(agent-shell--mode-line-mode-menu))))
    (when (agent-shell-get-thought-level-name (agent-shell--state))
      (define-key menu [thought-level]
                  `(menu-item "Thought level" ,(agent-shell--mode-line-thought-level-menu))))
    (when (agent-shell-get-model-name (agent-shell--state))
      (define-key menu [model]
                  `(menu-item "LLM model" ,(agent-shell--mode-line-model-menu))))
    menu))

(defun agent-shell--mode-line-format ()
  "Return `agent-shell''s mode-line format.

Typically includes the container indicator, model, session mode and activity
or nil if unavailable.

For example: \" ⧉ ➤ Sonnet ➤ Accept Edits ░░░ \".
Shows \" ⧉\" when a command prefix is used."
  (when-let* (((derived-mode-p 'agent-shell-mode))
              ((memq agent-shell-header-style '(none nil))))
    (concat (when agent-shell-command-prefix
              (propertize " ⧉ ➤"
                          'face 'agent-shell-container-indicator
                          'help-echo "Running in container"))
            (when-let* ((model-name (or (map-elt (seq-find (lambda (model)
                                                             (string= (map-elt model :model-id)
                                                                      (agent-shell--current-model-id (agent-shell--state))))
                                                           (agent-shell--get-available-models (agent-shell--state)))
                                                 :name)
                                        (agent-shell--current-model-id (agent-shell--state)))))
              (concat " " (propertize model-name
                                      'face 'agent-shell-model
                                      'help-echo (concat "Open LLM model menu "
                                                         (propertize (key-description (where-is-internal
                                                                                       'agent-shell-set-session-model
                                                                                       agent-shell-mode-map t))
                                                                     'face 'agent-shell-key-binding))
                                      'mouse-face 'mode-line-highlight
                                      'local-map (let ((map (make-sparse-keymap)))
                                                   (define-key map [mode-line mouse-1]
                                                               (agent-shell--mode-line-model-menu))
                                                   map))))
            (when-let* ((thought-level-name (agent-shell-get-thought-level-name (agent-shell--state))))
              (concat " ➤ " (propertize thought-level-name
                                        'face 'agent-shell-thought-level
                                        'help-echo (concat "Open thought level menu "
                                                           (propertize (key-description (where-is-internal
                                                                                         'agent-shell-set-session-thought-level
                                                                                         agent-shell-mode-map t))
                                                                       'face 'agent-shell-key-binding))
                                        'mouse-face 'mode-line-highlight
                                        'local-map (let ((map (make-sparse-keymap)))
                                                     (define-key map [mode-line mouse-1]
                                                                 (agent-shell--mode-line-thought-level-menu))
                                                     map))))
            (when-let* ((mode-name (agent-shell--resolve-session-mode-name
                                    (agent-shell--current-mode-id (agent-shell--state))
                                    (agent-shell--get-available-modes (agent-shell--state)))))
              (concat " ➤ " (propertize mode-name
                                        'face 'agent-shell-session-mode
                                        'help-echo (concat "Open session mode menu "
                                                           (propertize (key-description (where-is-internal
                                                                                         'agent-shell-set-session-mode
                                                                                         agent-shell-mode-map t))
                                                                       'face 'agent-shell-key-binding))
                                        'mouse-face 'mode-line-highlight
                                        'local-map (let ((map (make-sparse-keymap)))
                                                     (define-key map [mode-line mouse-1]
                                                                 (agent-shell--mode-line-mode-menu))
                                                     map))))
            (when-let* ((indicator (agent-shell--context-usage-indicator)))
              (concat " ➤ " indicator))
            (agent-shell--busy-indicator-frame))))

(defun agent-shell--setup-modeline ()
  "Set up the modeline to display session mode.
Uses :eval so the mode updates automatically when state changes."
  (setq-local mode-line-misc-info
              (append mode-line-misc-info
                      '((:eval (agent-shell--mode-line-format))))))

(defun agent-shell-cycle-session-mode (&optional on-success)
  "Cycle through available session modes for the current `agent-shell' session.

Optionally, get notified of completion with ON-SUCCESS function."
  (declare (modes agent-shell-mode))
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
                                     (agent-shell--current-mode-id (agent-shell--state))
                                     #'string=) -1))
         (next-mode-idx (mod (1+ mode-idx) (length mode-ids)))
         (next-mode-id (nth next-mode-idx mode-ids)))
    (agent-shell--config-option-set-mode-id
     :mode-id next-mode-id
     :on-success on-success)))

(defun agent-shell-set-session-mode (&optional on-success)
  "Set session mode (if any available).

Optionally, get notified of completion with ON-SUCCESS function."
  (declare (modes agent-shell-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (user-error "No active session"))
  (unless (agent-shell--get-available-modes (agent-shell--state))
    (user-error "No session modes available"))
  (let* ((current-mode-id (agent-shell--current-mode-id (agent-shell--state)))
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
    (agent-shell--config-option-set-mode-id
     :mode-id selected-mode-id
     :on-success on-success)))

(defun agent-shell-set-session-model (&optional on-success)
  "Set session model.

Optionally, get notified of completion with ON-SUCCESS function."
  (declare (modes agent-shell-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (user-error "No active session"))
  (unless (agent-shell--get-available-models (agent-shell--state))
    (user-error "No session models available"))
  (let* ((current-model-id (agent-shell--current-model-id (agent-shell--state)))
         (available-models (agent-shell--get-available-models (agent-shell--state)))
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
    (agent-shell--config-option-set-model-id
     :model-id selected-model-id
     :on-success on-success)))

(defun agent-shell-set-session-thought-level (&optional on-success)
  "Set thought level (reasoning effort) for the current session.

Optionally, get notified of completion with ON-SUCCESS function."
  (declare (modes agent-shell-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (user-error "No active session"))
  (unless (agent-shell--get-available-thought-levels (agent-shell--state))
    (user-error "Agent does not advertise a thought level option for this session"))
  (let* ((current-id (agent-shell--current-thought-level-id (agent-shell--state)))
         (option (agent-shell--config-option-by-category (agent-shell--state) "thought_level"))
         (default-name (and current-id
                            (agent-shell--config-option-value-name option current-id)))
         (choices (mapcar (lambda (value)
                            (cons (map-elt value :name)
                                  (map-elt value :value)))
                          (agent-shell--get-available-thought-levels (agent-shell--state))))
         (selection (completing-read "Set thought level: "
                                     (mapcar #'car choices)
                                     nil t nil nil default-name))
         (selected-id (cdr (seq-find (lambda (choice)
                                       (string= selection (car choice)))
                                     choices))))
    (unless selected-id
      (user-error "Unknown thought level: %s" selection))
    (when (and current-id (string= selected-id current-id))
      (error "Thought level already %s" selection))
    (agent-shell--config-option-set-thought-level-id
     :thought-level-id selected-id
     :on-success on-success)))

(defun agent-shell-set-session-config-option (&optional on-success)
  "Set a session config option.

Only `select' options are offered.  Optionally, get notified of completion
with ON-SUCCESS function."
  (declare (modes agent-shell-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (user-error "No active session"))
  (unless (agent-shell--select-config-options (agent-shell--state))
    (user-error "No session config options available"))
  (let* ((config-choices (mapcar (lambda (option)
                                   (cons (map-elt option :name)
                                         option))
                                 (agent-shell--select-config-options (agent-shell--state))))
         (config-selection
          (completing-read
           "Set session option: "
           (lambda (string pred action)
             (if (eq action 'metadata)
                 `(metadata
                   (annotation-function
                    . ,(lambda (name)
                         (when-let* ((option (map-elt config-choices name)))
                           (concat
                            "  (current: "
                            (or (agent-shell--config-option-value-name
                                 option
                                 (map-elt option :current-value))
                                "")
                            ") "
                            (or (map-elt option :description) "")))))
                   (eager-display . t))
               (complete-with-action action config-choices string pred)))
           nil t))
         (selected-config-option (cdr (seq-find (lambda (choice)
                                                  (string= config-selection (car choice)))
                                                config-choices))))
    (unless selected-config-option
      (user-error "Unknown session config option: %s" config-selection))
    (let* ((value-choices (mapcar (lambda (value)
                                    (cons (map-elt value :name)
                                          value))
                                  (map-elt selected-config-option :options)))
           (current-value (map-elt selected-config-option :current-value))
           (default-value-name (agent-shell--config-option-value-name
                                selected-config-option
                                current-value))
           (value-selection
            (completing-read
             "Set value: "
             (lambda (string pred action)
               (if (eq action 'metadata)
                   `(metadata
                     (annotation-function
                      . ,(lambda (name)
                           (when-let* ((value (map-elt value-choices name)))
                             (concat
                              (if (equal (map-elt value :value) current-value)
                                  "  [current]"
                                "   ")
                              (when-let* ((description (map-elt value :description)))
                                (concat " " description))))))
                     (eager-display . t))
                 (complete-with-action action value-choices string pred)))
             nil t nil nil default-value-name))
           (selected-value (map-elt (cdr (seq-find (lambda (choice)
                                                     (string= value-selection (car choice)))
                                                   value-choices))
                                    :value)))
      (unless selected-value
        (user-error "Unknown session config value: %s" value-selection))
      (when (equal selected-value current-value)
        (error "%s already %s" config-selection value-selection))
      (agent-shell--set-session-config-option
       :config-id (map-elt selected-config-option :id)
       :value selected-value
       :on-success (lambda ()
                     (message "%s: %s" config-selection value-selection)
                     (when on-success
                       (funcall on-success)))))))

(defun agent-shell--format-available-modes (modes)
  "Format MODES for shell rendering."
  (string-join
   (seq-map
    (lambda (mode)
      (let ((name (when (map-elt mode :name)
                    (propertize (format "%s (id: %s)"
                                        (map-elt mode :name)
                                        (map-elt mode :id))
                                'font-lock-face 'agent-shell-list-name)))
            (desc (when (map-elt mode :description)
                    (propertize (map-elt mode :description)
                                'font-lock-face 'agent-shell-secondary))))
        (if desc
            (concat name "\n" desc)
          name)))
    modes)
   "\n\n"))

(defun agent-shell--format-available-models (models)
  "Format MODELS for shell rendering."
  (string-join
   (seq-map
    (lambda (model)
      (let ((name (concat
                   (when (map-elt model :name)
                     (propertize (map-elt model :name)
                                 'font-lock-face 'agent-shell-list-name))
                   (when (map-elt model :model-id)
                     (propertize (format " (id: %s)" (map-elt model :model-id))
                                 'font-lock-face 'agent-shell-list-name))))
            (desc (when (map-elt model :description)
                    (propertize (map-elt model :description)
                                'font-lock-face 'agent-shell-secondary))))
        (if desc
            (concat name "\n" desc)
          name)))
    models)
   "\n\n"))

;;; Transient

(transient-define-prefix agent-shell-help-menu ()
  "Transient menu for `agent-shell' commands."
  [["Navigation"
    ("<tab>" "Next item" agent-shell-next-item :transient t)
    ("<backtab>" "Previous item" agent-shell-previous-item :transient t)
    ("C-M-u" "Up to table's first cell" agent-shell-backward-up-item :transient t)]
   ["Insert"
    ("!" "Shell command" agent-shell-insert-shell-command-output :transient t)
    ("@" "File" agent-shell-insert-file :transient t)
    ("d" "Dwim" agent-shell-send-dwim :transient t)
    ]]
  [["Session"
    ("m" "Cycle modes" agent-shell-cycle-session-mode :transient t)
    ("M" "Set mode" agent-shell-set-session-mode :transient t)
    ("v" "Set model" agent-shell-set-session-model :transient t)
    ("t" "Set thought level" agent-shell-set-session-thought-level :transient t)
    ("o" "Set option" agent-shell-set-session-config-option :transient t)
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
  (let* ((dir (agent-shell--dot-subdir "transcripts"))
         (filename (format-time-string "%F-%H-%M-%S.md"))
         (filepath (expand-file-name filename dir)))
    filepath))

(defun agent-shell--transcript-file-path ()
  "Return the transcript file path, or nil if disabled."
  (when-let* ((path-fn agent-shell-transcript-file-path-function))
    (condition-case err
        (funcall path-fn)
      (error
       (message "Failed to generate transcript path: %S" err)
       nil))))

(defun agent-shell--ensure-transcript-file ()
  "Ensure the transcript file exists, creating it with header if needed.
Returns the file path, or nil if disabled."
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (when-let* ((filepath agent-shell--transcript-file)
              (dir (file-name-directory filepath)))
    (unless (file-exists-p filepath)
      (condition-case err
          (let ((agent-name (or (map-nested-elt agent-shell--state '(:agent-config :mode-line-name))
                                (map-nested-elt agent-shell--state '(:agent-config :buffer-name))
                                "Unknown Agent"))
                (session-id (map-nested-elt agent-shell--state '(:session :id)))
                (model-id (map-nested-elt agent-shell--state '(:session :model-id))))
            (write-region
             (format "# Agent Shell Transcript

**Agent:** %s
**Started:** %s
**Working Directory:** %s%s%s

---

"
                     agent-name
                     (format-time-string "%F %T")
                     (agent-shell-cwd)
                     (if session-id
                         (format "\n**Session ID:** %s" session-id)
                       "")
                     (if model-id
                         (format "\n**Model:** %s" model-id)
                       ""))
             nil filepath nil 'no-message)
            (message "Created %s"
                     (agent-shell--shorten-paths filepath t)))
        (error
         (message "Failed to initialize transcript: %S" err))))
    filepath))

(defun agent-shell--indent-markdown-headers (text)
  "Indent markdown headers in TEXT by 2 levels for transcript hierarchy.

Increases the level of all markdown headers while leaving content
inside code blocks unchanged.  Headers are capped at level 6
since markdown doesn't support deeper levels.

For example:

  (agent-shell--indent-markdown-headers \"# Foo\")
    => \"### Foo\"
  (agent-shell--indent-markdown-headers \"##### Deep\")
    => \"###### Deep\""
  (unless (stringp text)
    (setq text (or text "")))
  (let ((lines (split-string text "\n"))
        (in-code-block nil)
        (result nil))
    (dolist (line lines)
      (cond
       ;; Toggle code block state on fence lines (3+ backticks).
       ((string-match "\\`\\(```+\\)" line)
        (if in-code-block
            (when (>= (length (match-string 1 line)) in-code-block)
              (setq in-code-block nil))
          (setq in-code-block (length (match-string 1 line))))
        (push line result))
       ;; Outside code blocks, indent header lines.
       ((and (not in-code-block)
             (string-match "\\`\\(#+\\) " line))
        (let* ((hashes (match-string 1 line))
               (new-level (min 6 (+ (length hashes) 2)))
               (new-hashes (make-string new-level ?#)))
          (push (replace-regexp-in-string "\\`#+ " (concat new-hashes " ") line)
                result)))
       (t (push line result))))
    (mapconcat #'identity (nreverse result) "\n")))


(cl-defun agent-shell--append-transcript (&key text file-path)
  "Append TEXT to the transcript at FILE-PATH."
  (when (and file-path (agent-shell--ensure-transcript-file))
    (condition-case err
        (write-region text nil file-path t 'no-message)
      (error
       (message "Error writing to transcript: %S" err)))))

(cl-defun agent-shell--separate-transcript-after-agent-message (&key last-entry-type file-path)
  "Append a blank-line separator to the transcript at FILE-PATH.

Write the separator only when LAST-ENTRY-TYPE is
\"agent_message_chunk\", i.e. the turn ended while streaming an
agent message.  An agent message chunk may end without a trailing
newline (for example when interrupted), and without this separator
the next transcript section header lands on the same line as the
message body.  Call this at turn end on both the success and
failure paths."
  (when (equal last-entry-type "agent_message_chunk")
    (agent-shell--append-transcript :text "\n\n" :file-path file-path)))

(defun agent-shell--extract-tool-parameters (raw-input)
  "Extract and format tool parameters from RAW-INPUT.
Returns a formatted string of key parameters, or nil if no relevant
parameters found.  Excludes `command' and `description' as these are
already shown separately in transcript entries.

For example, given RAW-INPUT:

  \\='((filePath . \"/home/user/project/file.el\")
    (offset . 10)
    (limit . 20)
    (command . \"grep -r foo\")
    (description . \"Search for foo\"))

returns:

  \"filePath: /home/user/project/file.el
  offset: 10
  limit: 20\""
  (when-let* (((listp raw-input))
              (excluded-keys '(command description plan))
              (params (seq-remove
                       (lambda (pair)
                         (or (not (consp pair))
                             (let ((key (car pair))
                                   (value (cdr pair)))
                               (or (memq key excluded-keys)
                                   (null value)
                                   (and (stringp value) (string-empty-p value))))))
                       raw-input)))
    (mapconcat (lambda (pair)
                 (format "%s: %s"
                         (symbol-name (car pair))
                         (cond
                          ((stringp (cdr pair)) (cdr pair))
                          ((numberp (cdr pair)) (number-to-string (cdr pair)))
                          ((eq (cdr pair) t) "true")
                          (t (prin1-to-string (cdr pair))))))
               params
               "\n")))

(defun agent-shell--longest-backtick-run (text)
  "Return the length of the longest consecutive backtick sequence in TEXT.

For example:

  (agent-shell--longest-backtick-run \"no backticks\")
    => 0
  (agent-shell--longest-backtick-run \"has ``` three\")
    => 3
  (agent-shell--longest-backtick-run \"has ```` four and ``` three\")
    => 4"
  (let ((pos 0)
        (max-run 0))
    (while (string-match "`+" text pos)
      (setq max-run (max max-run (- (match-end 0) (match-beginning 0)))
            pos (match-end 0)))
    max-run))

(cl-defun agent-shell--make-transcript-tool-call-entry (&key status title kind description command parameters output)
  "Create a formatted transcript entry for a tool call.

Includes STATUS, TITLE, KIND, DESCRIPTION, COMMAND, PARAMETERS, and OUTPUT."
  (let* ((trimmed (string-trim output))
         (fence (make-string (max 3 (1+ (agent-shell--longest-backtick-run trimmed))) ?`)))
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
     (when parameters
       (format "\n**Parameters:**\n%s" parameters))
     "\n\n"
     fence
     "\n"
     trimmed
     "\n"
     fence
     "\n")))

(defun agent-shell-open-transcript ()
  "Open the transcript file for the current `agent-shell' buffer."
  (declare (modes agent-shell-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in an agent-shell buffer"))
  (unless agent-shell--transcript-file
    (error "No transcript file available for this buffer"))
  (unless (file-exists-p agent-shell--transcript-file)
    (error "Transcript file does not exist: %s" agent-shell--transcript-file))
  (find-file agent-shell--transcript-file))

(defun agent-shell--echo (format-string &rest args)
  "Echo FORMAT-STRING and ARGS to the echo area without logging.

Like `message', but binds `message-log-max' to nil so the text is shown
transiently and not recorded in the *Messages* buffer.  Useful for
ephemeral status such as the pending prompt queue."
  (let ((message-log-max nil))
    (apply #'message format-string args)))

(defun agent-shell-narrow-to-block (count)
  "Narrow to the last COUNT navigatable blocks in the current buffer.
The buffer must be an `agent-shell-mode' buffer.  Narrow from the
start of the COUNTth-from-last navigatable block to `point-max'."
  (interactive "P")
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in an agent-shell buffer"))
  (save-excursion
    (widen)
    (goto-char (point-max))
    (dotimes (_ (or count 1)) (agent-shell-ui-backward-block))
    (when-let* ((block (agent-shell-ui--block-range :position (point))))
      (narrow-to-region (map-elt block :start) (point-max)))))

(defun agent-shell-quote-region ()
  "Quote the active region into the shell's latest prompt.

If point is at the last prompt, behave as regular editing (typing
the originating key) so the user can type `r' as plain input.

Otherwise, when a region is active, wrap it as a Markdown block quote.
If the shell is not busy, insert the quote at the latest prompt with
point left below it, ready to type.  If the shell is busy, read a
follow-up prompt in the minibuffer prefilled with the block quote
and queue it via `agent-shell-prompt-queue'."
  (declare (modes agent-shell-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (cond
   ;; At prompt + not busy: behave as regular editing.
   ((agent-shell--typing-at-prompt-p)
    (self-insert-command 1))
   ;; Region active and not at prompt: quote into prompt or queue.
   ((and (not (shell-maker-point-at-last-prompt-p))
         (region-active-p))
    (let ((quoted (agent-shell--block-quote
                   (string-trim
                    (map-elt (agent-shell--get-region :deactivate t) :content)))))
      (if (shell-maker-busy)
          (agent-shell-prompt-queue
           (agent-shell--prompt-queue-read :initial (concat "\n\n" quoted "\n\n")))
        (goto-char (point-max))
        (insert "\n\n" quoted "\n\n"))))
   ;; Otherwise: fall back to self-insert.
   (t
    (self-insert-command 1))))

(defun agent-shell--typing-at-prompt-p ()
  "Return non-nil when a character key was typed at the latest prompt.
Single-character bindings in `agent-shell-mode-map' (`n', `+', ...)
consult this to insert the character while a prompt is being
composed, acting as commands anywhere else in the shell."
  (and (not (shell-maker-busy))
       (shell-maker-point-at-last-prompt-p)
       (integerp last-command-event)
       (> (length (this-command-keys-vector)) 0)
       ;; Ensure invoked using a key binding.
       (eq (key-binding (this-command-keys-vector)) this-command)))

(defun agent-shell-image-scale-increase ()
  "Widen the image at point, or every image in the buffer, by one step.

If point is at the last prompt, behave as regular editing (typing
the originating key) so the user can type `+' as plain input.

See `agent-shell-markdown-image-scale-increase' for the sizing."
  (declare (modes agent-shell-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (if (agent-shell--typing-at-prompt-p)
      (self-insert-command 1)
    (agent-shell-markdown-image-scale-increase)))

(defun agent-shell-image-scale-decrease ()
  "Narrow the image at point, or every image in the buffer, by one step.

If point is at the last prompt, behave as regular editing (typing
the originating key) so the user can type `-' as plain input.

See `agent-shell-markdown-image-scale-decrease' for the sizing."
  (declare (modes agent-shell-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (if (agent-shell--typing-at-prompt-p)
      (self-insert-command 1)
    (agent-shell-markdown-image-scale-decrease)))

(defun agent-shell-image-scale-reset ()
  "Reset the image at point, or every image in the buffer, to its rendered size.

If point is at the last prompt, behave as regular editing (typing
the originating key) so the user can type `0' as plain input.

See `agent-shell-markdown-image-scale-reset', which errors when
there's nothing to reset."
  (declare (modes agent-shell-mode))
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (if (agent-shell--typing-at-prompt-p)
      (self-insert-command 1)
    (agent-shell-markdown-image-scale-reset)))

(defun agent-shell-trim (text)
  "Strip surrounding whitespace from TEXT, preserving renderer padding.

Like `string-trim', but whitespace chars carrying the
`agent-shell-non-trimmable' text property are treated as
intentional padding (e.g. the top/bottom vpad `\\n's the
source-block renderer inserts inside a fragment body) and left
alone.  A blind `string-trim' would consume those chars on the
first / last block of a response and visibly clip the panel.

For example:

  (agent-shell-trim \"\\n\\n  hello  \\n\\n\")
  => \"hello\"

  (agent-shell-trim
   (concat \"\\n\\nhello\\n\"
           (propertize \"\\n\" \\='agent-shell-non-trimmable t)
           \"\\n\\n\"))
  => \"hello\\n\\n\"  ;; tagged trailing `\\n' preserved"
  (and-let* ((text text)
             (start 0)
             (end (length text)))
    (while (and (< start end)
                (memq (seq-elt text start) '(?\s ?\t ?\n ?\r))
                (not (get-text-property
                      start 'agent-shell-non-trimmable text)))
      (setq start (1+ start)))
    (while (and (< start end)
                (memq (seq-elt text (1- end)) '(?\s ?\t ?\n ?\r))
                (not (get-text-property
                      (1- end) 'agent-shell-non-trimmable text)))
      (setq end (1- end)))
    (substring text start end)))

(defvar-local agent-shell--realign-timer nil
  "Pending idle timer that re-aligns this buffer's rendered markdown.")

(defun agent-shell--realign-rendered (buffer)
  "Re-align BUFFER's window-relative markdown: tables and images.
Deferred worker for `agent-shell--realign-on-change'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq agent-shell--realign-timer nil)
      (agent-shell-markdown-rerender-tables)
      (agent-shell-markdown-rerender-images))))

(defun agent-shell--realign-on-change (window)
  "Re-align the current buffer's window-relative markdown shown in WINDOW.

Installed buffer-locally on `window-size-change-functions' and
`window-buffer-change-functions' (whose buffer-local values are
called with the window, the buffer current), so it runs only for
shell windows.  Table column widths and percentage image sizes are
measured against the display, so a resize or first display can change
them; schedule a re-render on any such change.  What actually needs
re-doing is decided per-item by `agent-shell-markdown-rerender-tables'
and `agent-shell-markdown-rerender-images' (via their stored widths),
so this can fire freely: it also catches content rendered while the
buffer was off-screen and then brought back into a same-width window.
Deferred to an idle timer, which also debounces a drag-resize into a
single re-render, because modifying a buffer from within these
redisplay hooks is unsafe."
  (when (window-live-p window)
    (when (timerp agent-shell--realign-timer)
      (cancel-timer agent-shell--realign-timer))
    (setq agent-shell--realign-timer
          (run-with-idle-timer 0.15 nil #'agent-shell--realign-rendered
                               (current-buffer)))))

(defun agent-shell--enable-realign ()
  "Keep the current buffer's rendered markdown aligned to its window.

Installs buffer-local window-change handlers (see
`agent-shell--realign-on-change').  Being buffer-local they run only
for this buffer's windows and are removed automatically when the
buffer is killed.  Added to `agent-shell-mode-hook' and
`agent-shell-viewport-view-mode-hook' so both the shell and its
viewport realign; a same-size window switch also fires the
buffer-change hook, so first display is covered too."
  (add-hook 'window-size-change-functions
            #'agent-shell--realign-on-change nil t)
  (add-hook 'window-buffer-change-functions
            #'agent-shell--realign-on-change nil t))

(add-hook 'agent-shell-mode-hook #'agent-shell--enable-realign)
(add-hook 'agent-shell-viewport-view-mode-hook
          #'agent-shell--enable-realign)

(provide 'agent-shell)

;;; agent-shell.el ends here
