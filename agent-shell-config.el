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

;;; Normalization

(defun agent-shell--normalize-config-option (acp-option)
  "Normalize ACP-OPTION (an ACP config option) to an internal alist.

For example:

  (agent-shell--normalize-config-option
   \\='((id . \"mode\") (type . \"select\") (currentValue . \"ask\")))
  => \\='((:id . \"mode\") (:type . \"select\") (:current-value . \"ask\") ...)"
  `((:id . ,(map-elt acp-option 'id))
    (:name . ,(map-elt acp-option 'name))
    (:description . ,(map-elt acp-option 'description))
    (:category . ,(map-elt acp-option 'category))
    (:type . ,(map-elt acp-option 'type))
    (:current-value . ,(map-elt acp-option 'currentValue))
    (:options . ,(mapcar (lambda (acp-value)
                           `((:value . ,(map-elt acp-value 'value))
                             (:name . ,(map-elt acp-value 'name))
                             (:description . ,(map-elt acp-value 'description))))
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
  "Return first config option in STATE matching CATEGORY, or nil.

CATEGORY may be nil for uncategorized options.  Uses `equal' for
nil-safe comparison.

For example:

  (agent-shell--config-option-by-category state \"model\")
  => \\='((:id . \"model\") (:category . \"model\") ...)"
  (seq-find (lambda (option)
              (equal category (map-elt option :category)))
            (agent-shell--config-options state)))

(defun agent-shell--select-config-options (state)
  "Return selectable (type = \"select\") config options from STATE."
  (seq-filter (lambda (option)
                (equal (map-elt option :type) "select"))
              (agent-shell--config-options state)))

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
  (if-let ((model-option (agent-shell--config-option-by-category state "model")))
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

Returns a propertized string with one block per option showing
name, id, current value, and optional description."
  (string-join
   (seq-map
    (lambda (option)
      (let ((name (propertize (format "%s (id: %s)"
                                      (map-elt option :name)
                                      (map-elt option :id))
                              'font-lock-face 'font-lock-function-name-face))
            (current (propertize (format "current: %s"
                                         (agent-shell--config-option-value-name
                                          option
                                          (map-elt option :current-value)))
                                 'font-lock-face 'font-lock-constant-face))
            (desc (when (map-elt option :description)
                    (propertize (map-elt option :description)
                                'font-lock-face 'font-lock-comment-face))))
        (string-join (delq nil (list name current desc)) "\n")))
    config-options)
   "\n\n"))

(provide 'agent-shell-config)

;;; agent-shell-config.el ends here
