;;; agent-shell-faces.el --- Customizable faces for agent-shell UI. -*- lexical-binding: t; -*-

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
;; Faces for `agent-shell''s user interface.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'comint)

(defgroup agent-shell-faces nil
  "Faces for `agent-shell''s user interface."
  :group 'agent-shell)

(defface agent-shell-steer-label
  '((t :inherit shadow :height 0.9))
  "Face for the label above a steered prompt."
  :group 'agent-shell-faces)


;;; Mode-line and header

(defface agent-shell-model
  '((t :inherit font-lock-negation-char-face))
  "Face for the model name shown in the mode-line and header."
  :group 'agent-shell-faces)

(defface agent-shell-thought-level
  '((t :inherit font-lock-keyword-face))
  "Face for the thought level shown in the mode-line and header."
  :group 'agent-shell-faces)

(defface agent-shell-container-indicator
  '((t :inherit font-lock-constant-face))
  "Face for the container indicator shown in the mode-line."
  :group 'agent-shell-faces)

(defface agent-shell-buffer-name
  '((t :inherit font-lock-variable-name-face))
  "Face for the buffer name shown in the header."
  :group 'agent-shell-faces)


;;; Session

(defface agent-shell-session-id
  '((t :inherit font-lock-constant-face))
  "Face for a session's id."
  :group 'agent-shell-faces)

(defface agent-shell-session-mode
  '((t :inherit font-lock-type-face))
  "Face for a session's mode."
  :group 'agent-shell-faces)

(defface agent-shell-session-title
  '((t :inherit font-lock-doc-markup-face))
  "Face for a session's title."
  :group 'agent-shell-faces)

(defface agent-shell-session-directory
  '((t :inherit font-lock-string-face))
  "Face for a session's working directory."
  :group 'agent-shell-faces)

(defface agent-shell-session-date
  '((t :inherit font-lock-comment-face))
  "Face for a session's date."
  :group 'agent-shell-faces)


;;; Collapsible sections (magit-like sections)

(defface agent-shell-section-heading
  '((t :inherit font-lock-doc-markup-face))
  "Face for a section's heading (e.g. \"Plan\", \"Thinking\", a tool call's kind)."
  :group 'agent-shell-faces)

(defface agent-shell-section-annotation
  '((t :inherit font-lock-doc-face))
  "Face for a section's inline annotation (e.g. a tool call's description)."
  :group 'agent-shell-faces)

(defface agent-shell-thought-body
  '((t))
  "Face for the body of a thought process section.
Unstyled by default; customize it (for example to inherit `italic') to
set thoughts apart from the agent's final output.  Applied as a base
face, under the markdown styling the body carries, so bold, links and
code spans in a thought keep their own faces on top."
  :group 'agent-shell-faces)

(defface agent-shell-secondary
  '((t :inherit font-lock-comment-face))
  "Face for de-emphasized secondary text.
Shared muted style for supporting text such as activity-group summaries,
listing descriptions and usage readouts."
  :group 'agent-shell-faces)


;;; Status (semantic states)

(defface agent-shell-success
  '((t :inherit success))
  "Face for success states (e.g. a completed tool call)."
  :group 'agent-shell-faces)

(defface agent-shell-warning
  '((t :inherit warning))
  "Face for warning and in-progress states."
  :group 'agent-shell-faces)

(defface agent-shell-error
  '((t :inherit error))
  "Face for error and failure states (e.g. a failed tool call)."
  :group 'agent-shell-faces)

(defface agent-shell-pending
  '((t :inherit font-lock-comment-face))
  "Face for pending and waiting states."
  :group 'agent-shell-faces)


;;; Listings (\"Available commands/models/modes/config options\", capabilities)

(defface agent-shell-list-name
  '((t :inherit font-lock-function-name-face))
  "Face for an entry's name in an \"Available ...\" listing.
Used for command, capability, config option, model and mode names."
  :group 'agent-shell-faces)

(defface agent-shell-list-value
  '((t :inherit font-lock-constant-face))
  "Face for a config option's current value in a listing."
  :group 'agent-shell-faces)


;;; Prompt and input

(defface agent-shell-prompt
  '((t :inherit comint-highlight-prompt))
  "Face for the shell prompt."
  :group 'agent-shell-faces)

(defface agent-shell-input
  '((t :inherit comint-highlight-input))
  "Face for user input."
  :group 'agent-shell-faces)

(defface agent-shell-key-binding
  '((t :inherit help-key-binding))
  "Face for key binding hints shown in the header and mode-line."
  :group 'agent-shell-faces)

(defface agent-shell-link
  '((t :inherit link))
  "Face for clickable links (e.g. file paths and URLs)."
  :group 'agent-shell-faces)

(defface agent-shell-permission-title
  '((t :inherit bold))
  "Face for the tool permission dialog title."
  :group 'agent-shell-faces)


;;; Viewport

(defface agent-shell-viewport-prompt
  '((t :inherit font-lock-doc-face))
  "Face for the prompt echoed in a viewport."
  :group 'agent-shell-faces)

(defface agent-shell-viewport-status-edit
  '((t :inherit success))
  "Face for the viewport edit status indicator."
  :group 'agent-shell-faces)

(defface agent-shell-viewport-status-busy
  '((t :inherit warning))
  "Face for the viewport busy status indicator."
  :group 'agent-shell-faces)

(defface agent-shell-viewport-status-view
  '((t :inherit default))
  "Face for the viewport view status indicator."
  :group 'agent-shell-faces)


;;; Diff

(defface agent-shell-diff-changes-label
  '((((type graphic)) :inherit diff-hunk-header :box t)
    (t :inherit diff-hunk-header :inverse-video t))
  "Face for the \"changes\" label shown for each hunk in diff buffers.
On graphical frames the label is drawn with a box; on terminals,
where boxes are not rendered, it falls back to inverse video so the
label stays visible."
  :group 'agent-shell-faces)

(provide 'agent-shell-faces)

;;; agent-shell-faces.el ends here
