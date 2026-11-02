;;; agent-shell-config.el --- Session config option helpers for agent-shell. -*- lexical-binding: t; -*-

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
;; Normalization, querying, and conversion of ACP session config options.
;;
;; ACP agents may advertise config options (model, mode, or custom) via
;; `configOptions' in session responses.  This file converts the camelCase
;; ACP wire format into :kebab-case internal alists and provides accessors
;; for the rest of agent-shell.
;;
;; See https://agentclientprotocol.com/protocol/session-config-options
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'agent-shell-faces)

;;; Normalization

(defun agent-shell--normalize-config-option (acp-option)
  "Normalize ACP-OPTION (an ACP config option) to an internal alist.

ACP `options' may be a flat array of values or an array of groups,
each holding its own values.  Groups are flattened into a single
list, with each value carrying its group's name under :group.

For example:

  (agent-shell--normalize-config-option
   \\='((id . \"mode\") (type . \"select\") (currentValue . \"ask\")))
  => \\='((:id . \"mode\") (:type . \"select\") (:current-value . \"ask\") ...)

  (agent-shell--normalize-config-option
   \\='((id . \"model\")
     (options . [((group . \"recommended\")
                  (name . \"Recommended\")
                  (options . [((value . \"model-1\") (name . \"Model 1\"))]))])))
  => \\='((:id . \"model\") ...
       (:options . (((:value . \"model-1\") (:name . \"Model 1\")
                     (:description . nil) (:group . \"Recommended\")))))"
  `((:id . ,(map-elt acp-option 'id))
    (:name . ,(map-elt acp-option 'name))
    (:description . ,(map-elt acp-option 'description))
    (:category . ,(map-elt acp-option 'category))
    (:type . ,(map-elt acp-option 'type))
    (:current-value . ,(map-elt acp-option 'currentValue))
    (:options . ,(seq-mapcat
                  (lambda (acp-entry)
                    (if (map-elt acp-entry 'group)
                        (mapcar (lambda (acp-value)
                                  `((:value . ,(map-elt acp-value 'value))
                                    (:name . ,(map-elt acp-value 'name))
                                    (:description . ,(map-elt acp-value 'description))
                                    (:group . ,(map-elt acp-entry 'name))))
                                (append (map-elt acp-entry 'options) nil))
                      (list `((:value . ,(map-elt acp-entry 'value))
                              (:name . ,(map-elt acp-entry 'name))
                              (:description . ,(map-elt acp-entry 'description))))))
                  (append (map-elt acp-option 'options) nil)))))

(defun agent-shell--normalize-config-options (acp-config-options)
  "Normalize ACP-CONFIG-OPTIONS (ACP `configOptions') to internal alists.

For example:

  (agent-shell--normalize-config-options
   \\='[((id . \"mode\") (type . \"select\") (currentValue . \"ask\"))])
  => \\='(((:id . \"mode\") (:type . \"select\") (:current-value . \"ask\") ...))"
  (mapcar #'agent-shell--normalize-config-option
          (append acp-config-options nil)))

;;; State management

(cl-defun agent-shell--save-config-options (&key state acp-config-options)
  "Save ACP-CONFIG-OPTIONS in STATE as normalized session config state.

Stores normalized options at both top-level :config-options and inside
the :session alist for consistency."
  (let ((normalized-options (agent-shell--normalize-config-options acp-config-options)))
    (setf (map-elt state :config-options) normalized-options)
    (when-let* ((session (map-elt state :session)))
      (setf (map-elt session :config-options) normalized-options)
      (setf (map-elt state :session) session))))

(cl-defun agent-shell--config-option-set-value (&key state config-id value)
  "In STATE, set the :current-value of config option CONFIG-ID to VALUE.

Updates the option in place in both top-level :config-options and the
copy under :session.  Used to keep local state in sync when an agent
acknowledges a `session/set_config_option' without echoing the full
configOptions list."
  (dolist (option (agent-shell--config-options state))
    (when (equal config-id (map-elt option :id))
      (setf (map-elt option :current-value) value))))

;;; Accessors

(defun agent-shell--config-options (state)
  "Return current config options from STATE.

For example:

  (agent-shell--config-options
   \\='((:session . ((:config-options . (((:id . \"model\")))))))
  => \\='(((:id . \"model\")))"
  (or (map-nested-elt state '(:session :config-options))
      (map-elt state :config-options)))

(cl-defun agent-shell--config-option-get (&key state id)
  "Return config option with ID from STATE, or nil.

For example:

  (agent-shell--config-option-get :state state :id \"model\")
  => \\='((:id . \"model\") (:type . \"select\") ...)"
  (seq-find (lambda (option)
              (equal id (map-elt option :id)))
            (agent-shell--config-options state)))

(defun agent-shell--config-option-by-category (state category)
  "Return a config option in STATE matching CATEGORY, or nil.

CATEGORY may be nil for uncategorized options.  Uses `equal' for
nil-safe comparison.

When several options share CATEGORY, prefers the one whose `:id'
equals CATEGORY, falling back to the first match.  Some agents (e.g.
Cline) tag multiple options with the same category -- both their
`provider' and `model' options use category \"model\" -- so matching
on category alone could otherwise resolve \"model\" to the provider
option.

For example:

  (agent-shell--config-option-by-category state \"model\")
  => \\='((:id . \"model\") (:category . \"model\") ...)"
  (let ((matches (seq-filter (lambda (option)
                               (equal category (map-elt option :category)))
                             (agent-shell--config-options state))))
    (or (seq-find (lambda (option)
                    (equal category (map-elt option :id)))
                  matches)
        (car matches))))

(defun agent-shell--resolve-config-option (state option)
  "Return the config option in STATE addressed by OPTION, or nil.

OPTION is matched against advertised ids first, then ACP categories, so
both what a shell lists under \"Available config options\" (\"effort\")
and the spec's category names (\"thought_level\") reach the same option.
Ids cast the wider net: an option outside the spec's categories, say
\"fast\", is only addressable by id.

For example, against an agent advertising an \"effort\" option
categorized as \"thought_level\":

  (agent-shell--resolve-config-option state \"effort\")
  => \\='((:id . \"effort\") (:category . \"thought_level\") ...)

  (agent-shell--resolve-config-option state \"thought_level\")
  => \\='((:id . \"effort\") (:category . \"thought_level\") ...)"
  (or (agent-shell--config-option-get :state state :id option)
      (agent-shell--config-option-by-category state option)))

(cl-defun agent-shell--config-option-find (&key state id category)
  "Return the config option in STATE with ID or in CATEGORY, or nil.

Exactly one of ID and CATEGORY must be given.  Unlike
`agent-shell--resolve-config-option', CATEGORY never guesses: when
several options share it, signal an error naming their ids, so the
caller can pass one as ID instead.

For example, against an agent advertising \"effort\" and \"reasoning\"
options, both categorized as \"thought_level\":

  (agent-shell--config-option-find :state state :id \"effort\")
  => \\='((:id . \"effort\")
       (:category . \"thought_level\")
       ...)

  (agent-shell--config-option-find :state state :category \"thought_level\")
  => error: Several thought_level options, pass :id with one of:
     effort, reasoning"
  (when (eq (null id) (null category))
    (error "Pass either :id or :category"))
  (if id
      (agent-shell--config-option-get :state state :id id)
    (let ((matches (seq-filter (lambda (option)
                                 (equal category (map-elt option :category)))
                               (agent-shell--config-options state))))
      (when (cdr matches)
        (error "Several %s options, pass :id with one of: %s"
               category
               (string-join (seq-map (lambda (option)
                                       (map-elt option :id))
                                     matches)
                            ", ")))
      (car matches))))

(defun agent-shell--select-config-options (state)
  "Return selectable (type = \"select\") config options from STATE."
  (seq-filter (lambda (option)
                (equal (map-elt option :type) "select"))
              (agent-shell--config-options state)))

(defun agent-shell--more-config-options (state)
  "Return selectable config options in STATE lacking their own header segment.

The header shows the model, thought level and session mode.  Every
other selectable option, whatever its category, is returned, including
a second option sharing one of those categories (e.g. Cline's
`provider', tagged \"model\").

For example, with model, provider, effort and speed options:

  (agent-shell--more-config-options state)
  => \\='(((:id . \"provider\") (:category . \"model\") ...)
       ((:id . \"fast-mode\") (:category . \"model_config\") ...))"
  (let ((shown (seq-keep (lambda (category)
                           (agent-shell--config-option-by-category state category))
                         '("model" "thought_level" "mode"))))
    (seq-remove (lambda (option)
                  (memq option shown))
                (agent-shell--select-config-options state))))

(defun agent-shell--config-option-value-name (option value)
  "Return display name for VALUE in OPTION, falling back to VALUE itself.

For example:

  (agent-shell--config-option-value-name
   \\='((:options . (((:value . \"ask\") (:name . \"Ask\"))))) \"ask\")
  => \"Ask\""
  (or (map-elt (seq-find (lambda (candidate)
                           (equal value (map-elt candidate :value)))
                         (map-elt option :options))
               :name)
      value))

(defun agent-shell--config-option-value-choices (option)
  "Return (LABEL . VALUE) completion choices for OPTION's values.

LABEL is VALUE's :name, qualified with its :group when another value
in OPTION shares that name, so every LABEL stays selectable.

For example:

  (agent-shell--config-option-value-choices
   \\='((:options . (((:value . \"a/sonnet\") (:name . \"Sonnet\") (:group . \"A\"))
                    ((:value . \"b/sonnet\") (:name . \"Sonnet\") (:group . \"B\"))
                    ((:value . \"opus\") (:name . \"Opus\"))))))
  => \\='((\"Sonnet (A)\" . ...) (\"Sonnet (B)\" . ...) (\"Opus\" . ...))"
  (mapcar (lambda (value)
            (cons (if (and (map-elt value :group)
                           (< 1 (seq-count (lambda (other)
                                             (equal (map-elt other :name)
                                                    (map-elt value :name)))
                                           (map-elt option :options))))
                      (format "%s (%s)" (map-elt value :name) (map-elt value :group))
                    (map-elt value :name))
                  value))
          (map-elt option :options)))

;;; Legacy shape conversion

(defun agent-shell--config-option-as-models (option)
  "Convert OPTION values to legacy model display shape.

Each value becomes an alist with :model-id, :name, and :description
so existing model UI code works unchanged.

For example:

  (agent-shell--config-option-as-models
   \\='((:options . (((:value . \"sonnet\")
                    (:name . \"Sonnet\")
                    (:description . nil))))))
  => \\='(((:model-id . \"sonnet\")
        (:name . \"Sonnet\")
        (:description . nil)))"
  (mapcar (lambda (value)
            `((:model-id . ,(map-elt value :value))
              (:name . ,(map-elt value :name))
              (:description . ,(map-elt value :description))))
          (map-elt option :options)))

(defun agent-shell--config-option-as-modes (option)
  "Convert OPTION values to legacy mode display shape.

Each value becomes an alist with :id, :name, and :description
so existing mode UI code works unchanged.

For example:

  (agent-shell--config-option-as-modes
   \\='((:options . (((:value . \"ask\") (:name . \"Ask\") (:description . nil))))))
  => \\='(((:id . \"ask\") (:name . \"Ask\") (:description . nil)))"
  (mapcar (lambda (value)
            `((:id . ,(map-elt value :value))
              (:name . ,(map-elt value :name))
              (:description . ,(map-elt value :description))))
          (map-elt option :options)))

;;; Current value helpers

(defun agent-shell--current-model-id (state)
  "Return current model ID from STATE.

Prefers config option with category \"model\", falls back to
session :model-id."
  (or (map-elt (agent-shell--config-option-by-category state "model") :current-value)
      (map-nested-elt state '(:session :model-id))))

(defun agent-shell--current-mode-id (state)
  "Return current mode ID from STATE.

Prefers config option with category \"mode\", falls back to
session :mode-id."
  (or (map-elt (agent-shell--config-option-by-category state "mode") :current-value)
      (map-nested-elt state '(:session :mode-id))))

(defun agent-shell--current-thought-level-id (state)
  "Return current thought level ID from STATE or nil if not advertised.

The option is identified by ACP category \"thought_level\" per the spec:
https://agentclientprotocol.com/protocol/session-config-options"
  (map-elt (agent-shell--config-option-by-category state "thought_level")
           :current-value))

(defun agent-shell--get-available-models (state)
  "Return available models from STATE, preferring config options.

When a config option with category \"model\" exists, converts its
values to legacy model shape.  Otherwise returns session :models."
  (if-let* ((model-option (agent-shell--config-option-by-category state "model")))
      (agent-shell--config-option-as-models model-option)
    (map-nested-elt state '(:session :models))))

(defun agent-shell--get-available-thought-levels (state)
  "Return available thought level values from STATE.

Each value is an alist with :value, :name, optional :description where
:value is the agent value and :name is a human-readable name."
  (map-elt (agent-shell--config-option-by-category state "thought_level")
           :options))

;;; Formatting

(defun agent-shell--format-available-config-options (config-options)
  "Format CONFIG-OPTIONS for shell rendering.

Returns a propertized string with one block per option showing the
name (with any description inline), the current value, and the
selectable values.  Both the current value and each selectable value
show the id sent to the agent alongside its display name.

The name carries the listing's emphasis; the `current:' and `values:'
labels are muted so they do not read as headings."
  (string-join
   (seq-map
    (lambda (option)
      (let* ((values-prefix "values: ")
             (name (concat
                    (propertize (format "%s (id: %s)"
                                        (map-elt option :name)
                                        (map-elt option :id))
                                'font-lock-face 'agent-shell-list-name)
                    (when (map-elt option :description)
                      (concat ": " (map-elt option :description)))))
             (current (concat
                       (propertize "current: " 'font-lock-face 'agent-shell-secondary)
                       (agent-shell--config-option-value-label
                        (agent-shell--config-option-value-name
                         option (map-elt option :current-value))
                        (map-elt option :current-value))))
             (values (when-let* ((options (map-elt option :options)))
                       (concat
                        (propertize values-prefix 'font-lock-face 'agent-shell-secondary)
                        (string-join
                         (seq-map (lambda (value)
                                    (agent-shell--config-option-value-label
                                     (map-elt value :name)
                                     (map-elt value :value)))
                                  options)
                         (concat "\n" (make-string (length values-prefix) ?\s)))))))
        (string-join (delq nil (list name current values)) "\n")))
    config-options)
   "\n\n"))

(defun agent-shell--config-option-value-label (name id)
  "Return a display label combining NAME with its raw ID.

Shows the human NAME annotated with the ID sent to the agent, as in
\"High (id: high)\".  Falls back to the bare ID when NAME is nil or
adds no information.

For example:

  (agent-shell--config-option-value-label \"High\" \"high\")
  => \"High (id: high)\""
  (if (and name (not (equal name id)))
      (format "%s (id: %s)" name id)
    (format "%s" id)))

(provide 'agent-shell-config)

;;; agent-shell-config.el ends here
