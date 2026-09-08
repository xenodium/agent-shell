;;; agent-shell-elicitation.el --- Inline ACP elicitation forms -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Render ACP form elicitations as ordinary agent-shell fragments.  The
;; shell state owns each request record and its shell buffer owns the
;; canonical controls.  The generic fragment renderer may mirror them
;; into a viewing viewport, as it does for permission prompts; a compose
;; viewport remains untouched because its contents are the user's next
;; prompt.
;;
;; Elicitations are independent Agent-to-Client requests.  They can
;; arrive before a session exists, during a turn, or between turns, and
;; the Agent may continue working while one is pending.  Records are
;; therefore scoped to their ACP request, session, tool call, and
;; connection rather than to the shell's current turn.
;;
;; Settlement removes that live record before sending a response, then
;; replaces its controls with display-only history.  The visible summary
;; cannot extend the protocol request's lifetime or answer it twice.
;;
;; Unknown extension field types stay opaque while pending.  Optional
;; fields can then be omitted, while a required unknown field leaves only
;; Decline and Cancel; treating unknown data as a known control would
;; invent wire semantics.
;;
;; Only form mode is implemented.  agent-shell does not advertise URL
;; mode, whose consent and browser lifecycle are separate concerns.

;;; Code:

(require 'acp)
(eval-when-compile
  (require 'cl-lib))
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'text-property-search)

(declare-function agent-shell--delete-fragment "agent-shell")
(declare-function agent-shell--make-button "agent-shell")
(declare-function agent-shell--present-user-input-request "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")

(defconst agent-shell-elicitation--namespace "elicitations"
  "Fragment namespace for pending form elicitations.")

(defconst agent-shell-elicitation--integer-min (- (expt 2 63))
  "Smallest integer representable by ACP elicitation content.")

(defconst agent-shell-elicitation--integer-max (1- (expt 2 63))
  "Largest integer representable by ACP elicitation content.")

(defconst agent-shell-elicitation--json-schema-types
  '("array" "boolean" "integer" "null" "number" "object" "string")
  "JSON Schema types known independently of ACP's restricted subset.")

(defun agent-shell-elicitation--integer-p (value)
  "Return non-nil when VALUE is an ACP signed 64-bit integer."
  (and (integerp value)
       (<= agent-shell-elicitation--integer-min
           value
           agent-shell-elicitation--integer-max)))

(defun agent-shell-elicitation--name-string (name)
  "Return protocol property NAME as a display string."
  (cond ((symbolp name) (symbol-name name))
        ((stringp name) name)
        (t (error "Elicitation property names must be strings"))))

(defun agent-shell-elicitation--agent-name (state)
  "Return the name of the Agent represented by STATE."
  (or (map-nested-elt state '(:agent-config :mode-line-name))
      (map-nested-elt state '(:agent-config :buffer-name))
      "ACP Agent"))

(defun agent-shell-elicitation--entry (state request-id)
  "Return STATE's pending elicitation with REQUEST-ID."
  (map-nested-elt state (list :elicitations request-id)))

(defun agent-shell-elicitation--field (entry name)
  "Return ENTRY's normalized field named NAME."
  (seq-find (lambda (field)
              (equal (map-elt field :name) name))
            (map-elt entry :fields)))

(defun agent-shell-elicitation--active-request-p (state request-id)
  "Return non-nil when REQUEST-ID names an active request in STATE."
  (seq-some
   (lambda (request)
     (equal (map-elt request :wire-request-id) request-id))
   (map-elt state :active-requests)))

(defun agent-shell-elicitation--scope (state params)
  "Validate PARAMS' scope against STATE and return its normalized form.

For example, `(sessionId . \"s\")' becomes `(:scope . session)' and
`(:scope-id . \"s\")' when \"s\" is STATE's active session."
  (let ((has-session (map-contains-key params 'sessionId))
        (has-request (map-contains-key params 'requestId))
        (session-id (map-elt params 'sessionId))
        (request-id (map-elt params 'requestId))
        (tool-call-id (map-elt params 'toolCallId)))
    (when (eq has-session has-request)
      (error "An elicitation must have exactly one scope"))
    (when (and tool-call-id (not has-session))
      (error "The toolCallId field requires session scope"))
    (when (and tool-call-id (not (stringp tool-call-id)))
      (error "The toolCallId field must be a string"))
    (cond
     (has-session
      (unless (and (stringp session-id)
                   (equal session-id
                          (map-nested-elt state '(:session :id))))
        (error "Elicitation sessionId is not the active session"))
      (when-let* ((tool-call-id)
                  (tool-call
                   (or (map-nested-elt
                        state (list :tool-calls tool-call-id))
                       (error
                        "Elicitation toolCallId is not active")))
                  ((seq-contains-p
                    '("completed" "failed" "cancelled")
                    (map-elt tool-call :status))))
        (error "Elicitation toolCallId has already finished"))
      (list (cons :scope 'session)
            (cons :scope-id session-id)
            (cons :tool-call-id tool-call-id)))
     (t
      (unless (or (stringp request-id)
                  (agent-shell-elicitation--integer-p request-id))
        (error "Elicitation requestId must be a string or signed 64-bit integer"))
      (unless (agent-shell-elicitation--active-request-p state request-id)
        (error "Elicitation requestId is not active"))
      (list (cons :scope 'request)
            (cons :scope-id request-id))))))

(defun agent-shell-elicitation--required-names (schema)
  "Return SCHEMA's required property names."
  (let ((required (map-elt schema 'required)))
    (cond ((null required) nil)
          ((and (vectorp required)
                (seq-every-p #'stringp required))
           (append required nil))
          (t
           (error "The requestedSchema.required field must be an array of strings")))))

(defun agent-shell-elicitation--options (schema plain-key titled-key)
  "Return options from SCHEMA's PLAIN-KEY or TITLED-KEY.

For example:

  (agent-shell-elicitation--options
   \\='((enum . [\"red\" \"blue\"])) \\='enum \\='oneOf)
  => (((:value . \"red\") (:title . \"red\"))
      ((:value . \"blue\") (:title . \"blue\")))"
  (let* ((plain (map-elt schema plain-key))
         (titled (map-elt schema titled-key))
         ;; ACP treats null option collections like omitted ones.
         (plain-present
          (and plain (map-contains-key schema plain-key)))
         (titled-present
          (and titled (map-contains-key schema titled-key))))
    (when (and plain-present titled-present)
      (error "An elicitation field cannot use two option forms"))
    (cond
     (plain-present
      (unless (and (vectorp plain)
                   (> (length plain) 0)
                   (seq-every-p #'stringp plain))
        (error "Elicitation enum must contain string values"))
      (seq-map
       (lambda (value)
         (list (cons :value value)
               (cons :title value)))
       plain))
     (titled-present
      (unless (and (vectorp titled)
                   (> (length titled) 0))
        (error "Elicitation titled options must be a nonempty array"))
      (seq-map
       (lambda (option)
         (unless (and (listp option)
                      (stringp (map-elt option 'const))
                      (stringp (map-elt option 'title)))
           (error "Elicitation titled options need const and title"))
         (list
          (cons :value (map-elt option 'const))
          (cons :title (map-elt option 'title))
          (cons :description
                (when (stringp (map-elt option 'description))
                  (map-elt option 'description)))))
       titled))
     (t nil))))

(cl-defun agent-shell-elicitation--constraint
    (&key schema key predicate description)
  "Return SCHEMA's KEY after checking PREDICATE.

Nil means the optional constraint is absent.  DESCRIPTION names the
constraint in errors."
  (when-let* ((value (map-elt schema key)))
    (unless (funcall predicate value)
      (error "Elicitation %s has the wrong type" description))
    value))

(defun agent-shell-elicitation--field-kind-and-options (schema)
  "Return SCHEMA's field kind and normalized options.

For example, `((type . \"integer\"))' becomes
`((:kind . integer))'."
  (let ((type (map-elt schema 'type)))
    (cond
     ((equal type "string")
      (let ((options
             (agent-shell-elicitation--options schema 'enum 'oneOf)))
        (list (cons :kind (if options 'single-select 'string))
              (cons :options options))))
     ((equal type "number")
      '((:kind . number)))
     ((equal type "integer")
      '((:kind . integer)))
     ((equal type "boolean")
      '((:kind . boolean)))
     ((equal type "array")
      (unless (listp (map-elt schema 'items))
        (error "Elicitation multi-select requires items"))
      (let* ((items (map-elt schema 'items))
             (item-type (map-elt items 'type)))
        (when (and item-type (not (stringp item-type)))
          (error "Elicitation multi-select items.type must be a string"))
        (cond
         ((and item-type (not (equal item-type "string")))
          (if (seq-contains-p
               agent-shell-elicitation--json-schema-types item-type)
              (error "Elicitation multi-select items must be strings")
            (list (cons :kind 'unsupported)
                  (cons :raw-type type)
                  (cons :raw-item-type item-type))))
         (t
          (let ((options
                 (agent-shell-elicitation--options
                  items 'enum 'anyOf)))
            (when (and (map-elt items 'enum)
                       (not (equal item-type "string")))
              (error "Plain multi-select items require string type"))
            (unless options
              (error "Elicitation multi-select has no options"))
            (list (cons :kind 'multi-select)
                  (cons :options options)))))))
     ((and (stringp type)
           (not (seq-contains-p
                 agent-shell-elicitation--json-schema-types type)))
      ;; Unknown extension types remain opaque.  The type name is sufficient
      ;; for display: agent-shell does not store, replay, proxy, or forward
      ;; elicitation schemas.
      (list (cons :kind 'unsupported)
            (cons :raw-type type)))
     (t
      (error "Unsupported elicitation property type %S" type)))))

(defun agent-shell-elicitation--default (schema kind options)
  "Return normalized default information for SCHEMA, KIND, and OPTIONS.

For example:

  (agent-shell-elicitation--default
   \\='((default . \"blue\")) \\='single-select
   \\='(((:value . \"red\")) ((:value . \"blue\"))))
  => ((:present . t) (:value . \"blue\"))"
  (let* ((raw (map-elt schema 'default))
         (present (map-contains-key schema 'default))
         (allowed (seq-map (lambda (option)
                             (map-elt option :value))
                           options))
         (missing 'agent-shell-elicitation--missing-default)
         ;; The live parser retains acp.el's established nil
         ;; representation for JSON false, while recorded traffic uses
         ;; :false.  Presence is checked separately, so a false default
         ;; remains distinct from an omitted default without changing every
         ;; ACP boolean consumer.  Live JSON null is indistinguishable here;
         ;; treating it as false is the safe, editable boolean value.
         (value
          (cond
           ((not present)
            missing)
           ((and (eq kind 'boolean)
                 (or (eq raw t) (null raw) (eq raw :false)))
            (eq raw t))
           ((and (eq kind 'string) (stringp raw))
            raw)
           ((and (eq kind 'number) (numberp raw))
            (format "%s" raw))
           ((and (eq kind 'integer)
                 (agent-shell-elicitation--integer-p raw))
            (format "%s" raw))
           ((and (eq kind 'single-select)
                 (stringp raw)
                 (seq-contains-p allowed raw))
            raw)
           ((and (eq kind 'multi-select) (vectorp raw))
            ;; ACP explicitly asks readers to skip malformed or unknown
            ;; default selections rather than rejecting the whole form.
            (seq-filter
             (lambda (item)
               (and (stringp item)
                    (seq-contains-p allowed item)))
             raw))
           ;; Primitive defaults are tolerant fields in ACP.  A malformed
           ;; value is equivalent to no default and remains user-editable.
           (t missing))))
    (list
     (cons :present (not (eq value missing)))
     (cons :value
           (if (eq value missing)
               (pcase kind
                 ((or 'string 'number 'integer) "")
                 ('boolean nil)
                 ('single-select nil)
                 ('multi-select nil))
             value)))))

(defun agent-shell-elicitation--validate-bounds (field)
  "Return FIELD after validating its normalized lower and upper bounds."
  (when (and (map-elt field :minimum)
             (map-elt field :maximum)
             (> (map-elt field :minimum)
                (map-elt field :maximum)))
    (error "Elicitation minimum exceeds maximum"))
  field)

(defun agent-shell-elicitation--normalize-field
    (name schema required-names)
  "Normalize field NAME from SCHEMA using REQUIRED-NAMES.

For example, NAME `enabled' with SCHEMA `((type . \"boolean\"))'
produces a field containing `(:kind . boolean)'."
  (unless (listp schema)
    (error "Elicitation property %S must be an object" name))
  (let* ((kind-and-options
          (agent-shell-elicitation--field-kind-and-options schema))
         (kind (map-elt kind-and-options :kind))
         (options (map-elt kind-and-options :options))
         (default
          (if (eq kind 'unsupported)
              '((:present . nil) (:value . nil))
            (agent-shell-elicitation--default schema kind options)))
         (required
          (and (seq-contains-p
                required-names
                (agent-shell-elicitation--name-string name))
               t))
         (uint32
          (lambda (value)
            (and (integerp value)
                 (<= 0 value (1- (expt 2 32))))))
         (uint64
          (lambda (value)
            (and (integerp value)
                 (<= 0 value (1- (expt 2 64))))))
         (number-value
          (if (eq kind 'integer)
              #'agent-shell-elicitation--integer-p
            #'numberp)))
    (agent-shell-elicitation--validate-bounds
     (append
      (list
       (cons :name name)
       (cons :kind kind)
       (cons :title
             (or (when (stringp (map-elt schema 'title))
                   (map-elt schema 'title))
                 (agent-shell-elicitation--name-string name)))
       (cons :description
             (when (stringp (map-elt schema 'description))
               (map-elt schema 'description)))
       (cons :required required)
       (cons :included (or required (map-elt default :present)))
       (cons :has-value
             (or (map-elt default :present)
                 (seq-contains-p
                  '(string boolean multi-select) kind)))
       (cons :value (map-elt default :value))
       (cons :options options)
       (cons :raw-type (map-elt kind-and-options :raw-type))
       (cons :raw-item-type
             (map-elt kind-and-options :raw-item-type)))
      (when (eq kind 'string)
        (list
         (cons :minimum
               (agent-shell-elicitation--constraint
                :schema schema :key 'minLength
                :predicate uint32
                :description "minLength"))
         (cons :maximum
               (agent-shell-elicitation--constraint
                :schema schema :key 'maxLength
                :predicate uint32
                :description "maxLength"))
         (cons :pattern
               (agent-shell-elicitation--constraint
                :schema schema :key 'pattern
                :predicate #'stringp :description "pattern"))
         (cons :format
               (agent-shell-elicitation--constraint
                :schema schema :key 'format
                :predicate #'stringp :description "format"))))
      (when (seq-contains-p '(number integer) kind)
        (list
         (cons :minimum
               (agent-shell-elicitation--constraint
                :schema schema :key 'minimum
                :predicate number-value :description "minimum"))
         (cons :maximum
               (agent-shell-elicitation--constraint
                :schema schema :key 'maximum
                :predicate number-value :description "maximum"))))
      (when (eq kind 'multi-select)
        (list
         (cons :minimum
               (agent-shell-elicitation--constraint
                :schema schema :key 'minItems
                :predicate uint64
                :description "minItems"))
         (cons :maximum
               (agent-shell-elicitation--constraint
                :schema schema :key 'maxItems
                :predicate uint64
                :description "maxItems"))))))))

(defun agent-shell-elicitation--normalize-schema (schema)
  "Normalize an ACP form SCHEMA into editable field records.

For example:

  (map-elt
   (seq-first
    (map-elt
     (agent-shell-elicitation--normalize-schema
      \\='((properties (name (type . \"string\")))))
     :fields))
   :kind)
  => string"
  (unless (listp schema)
    (error "The requestedSchema field must be an object"))
  (when (and (stringp (map-elt schema 'type))
             (not (equal (map-elt schema 'type) "object")))
    (error "The requestedSchema.type field must be \"object\""))
  (let ((properties (map-elt schema 'properties))
        (required (agent-shell-elicitation--required-names schema)))
    (unless (listp properties)
      (error "The requestedSchema.properties field must be an object"))
    (seq-do
     (lambda (name)
       (unless (seq-some
                (lambda (property-name)
                  (equal
                   (agent-shell-elicitation--name-string property-name)
                   name))
                (map-keys properties))
         (error "Required elicitation property %S is not defined" name)))
     required)
    (list
     (cons :title
           (when (stringp (map-elt schema 'title))
             (map-elt schema 'title)))
     (cons :description
           (when (stringp (map-elt schema 'description))
             (map-elt schema 'description)))
     (cons :fields
           (map-apply
            (lambda (name property-schema)
              (agent-shell-elicitation--normalize-field
               name property-schema required))
            properties)))))

(defun agent-shell-elicitation--normalize-request (state acp-request)
  "Validate ACP-REQUEST against STATE and return a pending record.

For example, request id 7 scoped by `(sessionId . \"s\")' produces
`(:request-id . 7)', `(:scope . session)', and `(:scope-id . \"s\")'
when \"s\" is STATE's active session."
  (let ((params (map-elt acp-request 'params))
        (request-id (map-elt acp-request 'id)))
    (unless (or (stringp request-id)
                (agent-shell-elicitation--integer-p request-id))
      (error "Elicitation JSON-RPC id must be a string or signed 64-bit integer"))
    (unless (listp params)
      (error "Elicitation params must be an object"))
    (unless (equal (map-elt params 'mode) "form")
      (error "Only form elicitation is supported"))
    (unless (stringp (map-elt params 'message))
      (error "An elicitation message is required"))
    (unless (map-contains-key params 'requestedSchema)
      (error "An elicitation requestedSchema is required"))
    (when (agent-shell-elicitation--entry state request-id)
      (error "An elicitation with this request id is already pending"))
    (let ((scope (agent-shell-elicitation--scope state params))
          (form
           (agent-shell-elicitation--normalize-schema
            (map-elt params 'requestedSchema))))
      (append
       (list
        (cons :request-id request-id)
        (cons :message (map-elt params 'message))
        (cons :error nil))
       scope
       form))))

(cl-defun agent-shell-elicitation--button
    (&key state entry action text help field option (boxed t))
  "Return an inline control for ENTRY in STATE.

ACTION selects the command behavior.  TEXT and HELP are displayed.
FIELD and OPTION identify an optional field or choice.  BOXED controls
whether the control is drawn as a button."
  (agent-shell--make-button
   :text text
   :help help
   :kind 'elicitation
   :action #'agent-shell-elicitation-activate
   :boxed boxed
   :properties
   (list
    'agent-shell-elicitation-control t
    'agent-shell-elicitation-shell-buffer (map-elt state :buffer)
    'agent-shell-elicitation-request-id (map-elt entry :request-id)
    'agent-shell-elicitation-action action
    'agent-shell-elicitation-field field
    'agent-shell-elicitation-option option)))

(defun agent-shell-elicitation--constraint-text (field)
  "Return FIELD's constraints as display text.

For example, `((:minimum . 1) (:maximum . 2))' becomes
\"minimum 1, maximum 2\"."
  (string-join
   (seq-filter
    #'identity
    (list
     (when (map-elt field :minimum)
       (format "minimum %s" (map-elt field :minimum)))
     (when (map-elt field :maximum)
       (format "maximum %s" (map-elt field :maximum)))
     (when (map-elt field :format)
       (format "format %s" (map-elt field :format)))
     (when (map-elt field :pattern)
       (format "pattern %s" (map-elt field :pattern)))))
   ", "))

(defun agent-shell-elicitation--insert-field (state entry field)
  "Insert FIELD from ENTRY's inline representation using STATE."
  (insert (propertize (map-elt field :title) 'face 'bold))
  (when (map-elt field :required)
    (insert " (required)"))
  (insert "\n")
  (when (map-elt field :description)
    (insert (map-elt field :description) "\n"))
  (when-let* ((constraints
               (agent-shell-elicitation--constraint-text field))
              ((not (string-empty-p constraints))))
    (insert (propertize constraints 'face 'shadow) "\n"))
  (unless (or (map-elt field :required)
              (eq (map-elt field :kind) 'unsupported))
    (insert
     (agent-shell-elicitation--button
      :state state :entry entry :action 'toggle-include
      :field (map-elt field :name)
      :text (if (map-elt field :included) "Included" "Omitted")
      :help "RET to include or omit this field")
     " "))
  (pcase (map-elt field :kind)
    ((or 'string 'number 'integer)
     (insert
      (agent-shell-elicitation--button
       :state state :entry entry :action 'edit
       :field (map-elt field :name)
       :text (if (map-elt field :has-value)
                 (if (eq (map-elt field :kind) 'string)
                     (prin1-to-string (map-elt field :value))
                   (map-elt field :value))
               "<unset>")
       :help "RET to edit this value")
      "\n"))
    ('boolean
     (insert
      (string-join
       (seq-map
        (lambda (choice)
          (agent-shell-elicitation--button
           :state state :entry entry :action 'set-option
           :field (map-elt field :name)
           :option (car choice)
           :text (format "%s %s"
                         (if (eq (map-elt field :value) (car choice))
                             "(*)"
                           "( )")
                         (cdr choice))
           :boxed nil
           :help "RET to select this value"))
        '((t . "Yes") (nil . "No")))
       "  ")
      "\n"))
    ((or 'single-select 'multi-select)
     (insert "\n")
     (seq-do
      (lambda (option)
        (let ((selected
               (if (eq (map-elt field :kind) 'single-select)
                   (equal (map-elt field :value)
                          (map-elt option :value))
                 (seq-contains-p
                  (map-elt field :value)
                  (map-elt option :value)))))
          (insert
           (agent-shell-elicitation--button
            :state state
            :entry entry
            :action (if (eq (map-elt field :kind) 'single-select)
                        'set-option
                      'toggle-option)
            :field (map-elt field :name)
            :option (map-elt option :value)
            :text (format "%s %s"
                          (if selected "[x]" "[ ]")
                          (map-elt option :title))
            :help "RET to select this option")
           "\n")
          (when (map-elt option :description)
            (insert "  "
                    (propertize (map-elt option :description)
                                'face 'shadow)
                    "\n"))))
      (map-elt field :options)))
    ('unsupported
     (insert
      (propertize
       (if (map-elt field :raw-item-type)
           (format
            "Unsupported array item type %S; no value can be entered."
            (map-elt field :raw-item-type))
         (format
          "Unsupported field type %S; no value can be entered."
          (map-elt field :raw-type)))
       'face 'warning)
      "\n")))
  (insert "\n"))

(defun agent-shell-elicitation--required-unsupported-field (entry)
  "Return ENTRY's first required field with an unsupported type."
  (seq-find
   (lambda (field)
     (and (map-elt field :required)
          (eq (map-elt field :kind) 'unsupported)))
   (map-elt entry :fields)))

(defun agent-shell-elicitation--body (state entry)
  "Return the propertized fragment body for ENTRY in STATE."
  (with-temp-buffer
    (insert (map-elt entry :message) "\n\n")
    (when (map-elt entry :title)
      (insert (propertize (map-elt entry :title) 'face 'bold) "\n"))
    (when (map-elt entry :description)
      (insert (map-elt entry :description) "\n"))
    (when (or (map-elt entry :title)
              (map-elt entry :description))
      (insert "\n"))
    (insert
     (propertize
      "Do not enter passwords, API keys, or other credentials in this form."
      'face 'warning)
     "\n\n")
    (seq-do
     (lambda (field)
       (agent-shell-elicitation--insert-field state entry field))
     (map-elt entry :fields))
    (when (map-elt entry :error)
      (insert (propertize (map-elt entry :error) 'face 'error) "\n\n"))
    (let ((unsupported
           (agent-shell-elicitation--required-unsupported-field entry)))
      (when unsupported
        (insert
         (propertize
          (format
           "Cannot submit: required field %S has an unsupported type."
           (map-elt unsupported :title))
          'face 'error)
         "\n\n"))
      (unless unsupported
        (insert
         (agent-shell-elicitation--button
          :state state :entry entry :action 'submit
          :text "Submit" :help "RET to submit this form")
         " "))
      (insert
       (agent-shell-elicitation--button
        :state state :entry entry :action 'decline
        :text "Decline" :help "RET to explicitly decline")
       " "
       (agent-shell-elicitation--button
        :state state :entry entry :action 'cancel
        :text "Cancel" :help "RET to dismiss without choosing")))
    (buffer-string)))

(defun agent-shell-elicitation--option-title (field value)
  "Return FIELD's display title for option VALUE, or nil.

For example, VALUE \"s\" in an option titled \"Small\" produces
\"Small\"."
  (map-elt
   (seq-find
    (lambda (option)
      (equal (map-elt option :value) value))
    (map-elt field :options))
   :title))

(defun agent-shell-elicitation--summary-value (field value)
  "Return VALUE from FIELD as literal settled-summary text.

For example, JSON false in a boolean field produces \"No\"."
  (pcase (map-elt field :kind)
    ('boolean
     (if (eq value t) "Yes" "No"))
    ('single-select
     (or (agent-shell-elicitation--option-title field value)
         (prin1-to-string value)))
    ('multi-select
     (string-join
      (seq-map
       (lambda (item)
         (or (agent-shell-elicitation--option-title field item)
             (prin1-to-string item)))
       value)
      ", "))
    ('string
     (prin1-to-string value))
    (_
     (format "%s" value))))

(cl-defun agent-shell-elicitation--settled-body
    (&key entry action content detail)
  "Return inert summary text for settled ENTRY.

ACTION is \"accept\", \"decline\", or \"cancel\".  CONTENT contains
the accepted values, and DETAIL optionally explains automatic
cancellation.

For example, ACTION \"decline\" produces a summary ending in
\"Declined\"."
  (with-temp-buffer
    (insert (map-elt entry :message) "\n\n")
    (pcase action
      ("accept"
       (if content
           (progn
             (insert (propertize "Submitted" 'face 'bold) "\n\n")
             (map-do
              (lambda (name value)
                (let ((field
                       (agent-shell-elicitation--field entry name)))
                  (insert
                   (format
                    "%s: %s\n"
                    (or (map-elt field :title)
                        (agent-shell-elicitation--name-string name))
                    (agent-shell-elicitation--summary-value
                     field value)))))
              content))
         (insert (propertize
                  "Submitted with no values."
                  'face 'bold))))
      ("decline"
       (insert (propertize "Declined" 'face 'bold)))
      (_
       (insert (propertize "Cancelled" 'face 'bold))))
    (when detail
      (insert "\n\n" (propertize detail 'face 'shadow)))
    (buffer-string)))

(defun agent-shell-elicitation--render-body (state entry body)
  "Render BODY for ENTRY in STATE's shell."
  (agent-shell--update-fragment
   :state state
   :namespace-id agent-shell-elicitation--namespace
   :block-id (map-elt entry :block-id)
   :label-left
   (propertize
    (format "Input requested by %s"
            (agent-shell-elicitation--agent-name state))
    'font-lock-face 'agent-shell-section-heading)
   :body body
   :navigation 'never
   :expanded t
   :render-markdown nil
   :above-last-prompt t))

(defun agent-shell-elicitation--render (state entry)
  "Render pending ENTRY in STATE's shell."
  (agent-shell-elicitation--render-body
   state entry (agent-shell-elicitation--body state entry)))

(cl-defun agent-shell-elicitation--render-settled
    (&key state entry action content detail)
  "Replace ENTRY's controls with an inert settlement summary in STATE."
  (agent-shell-elicitation--render-body
   state entry
   (agent-shell-elicitation--settled-body
    :entry entry
    :action action
    :content content
    :detail detail)))

(defun agent-shell-elicitation--take (state request-id)
  "Remove and return REQUEST-ID's live record from STATE.

Rendered text is intentionally untouched so successful settlement can
replace it with an inert summary."
  (when-let* ((entry (agent-shell-elicitation--entry state request-id)))
    (map-put! state :elicitations
              (map-delete (map-elt state :elicitations) request-id))
    entry))

(defun agent-shell-elicitation--delete-entry-fragment-safely (state entry)
  "Best-effort removal of ENTRY's rendered fragment from STATE."
  (condition-case err
      (when-let* ((buffer (map-elt state :buffer))
                  ((buffer-live-p buffer)))
        (agent-shell--delete-fragment
         :state state
         :namespace-id agent-shell-elicitation--namespace
         :block-id (map-elt entry :block-id)))
    (error
     (message "Could not remove ACP form UI: %s"
              (error-message-string err)))))

(defun agent-shell-elicitation--remove (state request-id)
  "Remove REQUEST-ID from STATE and both rendered views."
  (when-let* ((entry
               (agent-shell-elicitation--take state request-id)))
    (agent-shell-elicitation--delete-entry-fragment-safely
     state entry)
    entry))

(defun agent-shell-elicitation--control-property (property)
  "Return PROPERTY from the inline control at point."
  (or (get-text-property (point) property)
      (when (> (point) (point-min))
        (get-text-property (1- (point)) property))))

(cl-defun agent-shell-elicitation--goto-control
    (&key request-id action field option)
  "Move to REQUEST-ID's control matching ACTION, FIELD, and OPTION."
  (goto-char (point-min))
  (let (found)
    (while (and (not found)
                (setq found
                      (text-property-search-forward
                       'agent-shell-elicitation-request-id
                       request-id t)))
      (goto-char (prop-match-beginning found))
      (unless
          (and
           (or (null action)
               (eq (get-text-property
                    (point) 'agent-shell-elicitation-action)
                   action))
           (or (null field)
               (equal
                (get-text-property
                 (point) 'agent-shell-elicitation-field)
                field))
           (or (null option)
               (equal
                (get-text-property
                 (point) 'agent-shell-elicitation-option)
                option)))
        (goto-char (prop-match-end found))
        (setq found nil)))
    (when found
      (goto-char (prop-match-beginning found))
      t)))

(defun agent-shell-elicitation--focus-request (request-id)
  "Move this buffer and its windows to REQUEST-ID's first control."
  (when (agent-shell-elicitation--goto-control
         :request-id request-id)
    (seq-do
     (lambda (window)
       (set-window-point window (point)))
     (get-buffer-window-list (current-buffer) nil t))
    t))

(defun agent-shell-elicitation--parse-number (text integer)
  "Parse TEXT as a JSON number, requiring INTEGER when non-nil.

For example:

  (agent-shell-elicitation--parse-number \"1.5\" nil)
  => 1.5"
  (let ((value
         (condition-case nil
             (json-parse-string text)
           (error
            (user-error "Enter a valid JSON number")))))
    (unless (numberp value)
      (user-error "Enter a valid JSON number"))
    (when (and integer
               (not (agent-shell-elicitation--integer-p value)))
      (user-error "Enter a signed 64-bit integer"))
    value))

(defun agent-shell-elicitation--validate-value (field value)
  "Validate FIELD's VALUE and return it."
  (pcase (map-elt field :kind)
    ('string
     (when (and (map-elt field :minimum)
                (< (length value) (map-elt field :minimum)))
       (user-error "%s is shorter than its minimum length"
                   (map-elt field :title)))
     (when (and (map-elt field :maximum)
                (> (length value) (map-elt field :maximum)))
       (user-error "%s is longer than its maximum length"
                   (map-elt field :title)))
     ;; ACP uses ECMA-262 patterns.  Emacs regular expressions have
     ;; different semantics and no evaluation timeout, so evaluating an
     ;; untrusted pattern here would be both inaccurate and able to block
     ;; the UI.  The constraint is displayed and the Agent validates it.
     )
    ((or 'number 'integer)
     (when (and (map-elt field :minimum)
                (< value (map-elt field :minimum)))
       (user-error "%s is below its minimum"
                   (map-elt field :title)))
     (when (and (map-elt field :maximum)
                (> value (map-elt field :maximum)))
       (user-error "%s is above its maximum"
                   (map-elt field :title))))
    ('multi-select
     (when (and (map-elt field :minimum)
                (< (length value) (map-elt field :minimum)))
       (user-error "%s has too few selections"
                   (map-elt field :title)))
     (when (and (map-elt field :maximum)
                (> (length value) (map-elt field :maximum)))
       (user-error "%s has too many selections"
                   (map-elt field :title)))))
  value)

(defun agent-shell-elicitation--content (entry)
  "Return validated ACP content from pending ENTRY.

For example, an included false boolean field named `authorize' becomes:

  ((authorize . :false))"
  (when-let* ((field
               (agent-shell-elicitation--required-unsupported-field
                entry)))
    (user-error
     "%s has an unsupported field type"
     (map-elt field :title)))
  (seq-keep
   (lambda (field)
     (when (and (map-elt field :included)
                (not (eq (map-elt field :kind) 'unsupported)))
       (unless (map-elt field :has-value)
         (user-error "%s needs a value" (map-elt field :title)))
       (let ((value
              (pcase (map-elt field :kind)
                ('number
                 (agent-shell-elicitation--parse-number
                  (map-elt field :value) nil))
                ('integer
                 (agent-shell-elicitation--parse-number
                  (map-elt field :value) t))
                ('boolean
                 (if (map-elt field :value) t :false))
                ('multi-select
                 (vconcat (map-elt field :value)))
                (_
                 (map-elt field :value)))))
         (cons (map-elt field :name)
               (agent-shell-elicitation--validate-value field value)))))
   (map-elt entry :fields)))

(cl-defun agent-shell-elicitation--settle-ui
    (&key state entry action content detail)
  "Replace ENTRY with an inert ACTION summary in STATE.

CONTENT contains accepted values and DETAIL explains automatic
cancellation.  Rendering cannot change the already completed protocol
operation; if it fails, remove the stale controls instead."
  (let ((buffer (map-elt state :buffer)))
    (if (and buffer
             (buffer-live-p buffer)
             (with-current-buffer buffer
               (derived-mode-p 'agent-shell-mode)))
        (condition-case err
            (agent-shell-elicitation--render-settled
             :state state
             :entry entry
             :action action
             :content content
             :detail detail)
          (error
           (agent-shell-elicitation--delete-entry-fragment-safely
            state entry)
           (message "Could not render settled ACP form: %s"
                    (error-message-string err))))
      (agent-shell-elicitation--delete-entry-fragment-safely
       state entry))))

(cl-defun agent-shell-elicitation--send-user-response
    (&key state request-id action content)
  "Settle REQUEST-ID in STATE with user ACTION and optional CONTENT."
  (when-let* ((entry
               (agent-shell-elicitation--take state request-id)))
    (condition-case err
        (acp-send-response
         :client (map-elt state :client)
         :response
         (acp-make-elicitation-response
          :request-id request-id
          :action action
          :content content))
      (error
       (agent-shell-elicitation--delete-entry-fragment-safely
        state entry)
       (signal (car err) (cdr err))))
    (agent-shell-elicitation--settle-ui
     :state state
     :entry entry
     :action action
     :content content)
    entry))

(cl-defun agent-shell-elicitation--send-cancelled-error
    (&key state request-id message)
  "Settle REQUEST-ID in STATE as protocol cancellation with MESSAGE."
  (when-let* ((entry
               (agent-shell-elicitation--take state request-id)))
    ;; Automatic owner cleanup runs inside unrelated request and
    ;; notification callbacks.  A dead transport must not prevent those
    ;; owners from completing their own state transitions.
    (condition-case err
        (acp-send-response
         :client (map-elt state :client)
         :response
         `((:request-id . ,request-id)
           (:error . ,(acp-make-error
                       :code -32800
                       :message message))))
      (error
       (message "Could not cancel ACP form request: %s"
                (error-message-string err))))
    (agent-shell-elicitation--settle-ui
     :state state
     :entry entry
     :action "cancel"
     :detail message)
    entry))

(defun agent-shell-elicitation-submit (state request-id)
  "Validate and submit REQUEST-ID from STATE."
  (when-let* ((entry (agent-shell-elicitation--entry state request-id)))
    (condition-case err
        (agent-shell-elicitation--send-user-response
         :state state
         :request-id request-id
         :action "accept"
         :content (agent-shell-elicitation--content entry))
      (user-error
       (map-put! entry :error (error-message-string err))
       (agent-shell-elicitation--render state entry)))))

(defun agent-shell-elicitation-activate ()
  "Activate the inline elicitation control at point."
  (interactive)
  (let ((shell-buffer
         (agent-shell-elicitation--control-property
          'agent-shell-elicitation-shell-buffer))
        (request-id
         (agent-shell-elicitation--control-property
          'agent-shell-elicitation-request-id))
        (action
         (agent-shell-elicitation--control-property
          'agent-shell-elicitation-action))
        (field-name
         (agent-shell-elicitation--control-property
          'agent-shell-elicitation-field))
        (option
         (agent-shell-elicitation--control-property
          'agent-shell-elicitation-option))
        (origin (current-buffer)))
    (unless (buffer-live-p shell-buffer)
      (user-error "The elicitation's shell is no longer available"))
    (with-current-buffer shell-buffer
      (let* ((state (agent-shell--state))
             (entry
              (or (agent-shell-elicitation--entry state request-id)
                  (user-error "This elicitation is no longer pending")))
             (field
              (when field-name
                (or (agent-shell-elicitation--field entry field-name)
                    (user-error "This elicitation field no longer exists")))))
        (pcase action
          ('edit
           (map-put! field :value
                     (read-string
                      (format "%s: " (map-elt field :title))
                      (map-elt field :value)))
           (map-put! field :has-value t)
           (map-put! field :included t))
          ('toggle-include
           (map-put! field :included
                     (not (map-elt field :included))))
          ('set-option
           (map-put! field :value option)
           (map-put! field :has-value t)
           (map-put! field :included t))
          ('toggle-option
           (map-put!
            field :value
            (if (seq-contains-p (map-elt field :value) option)
                (seq-remove
                 (lambda (value)
                   (equal value option))
                 (map-elt field :value))
              (append (map-elt field :value) (list option))))
           (map-put! field :included t))
          ('submit
           (agent-shell-elicitation-submit state request-id))
          ('decline
           (agent-shell-elicitation--send-user-response
            :state state :request-id request-id :action "decline"))
          ('cancel
           (agent-shell-elicitation--send-user-response
            :state state :request-id request-id :action "cancel"))
          (_
           (user-error "Unknown elicitation action")))
        (when (agent-shell-elicitation--entry state request-id)
          (unless (eq action 'submit)
            (map-put! entry :error nil)
            (agent-shell-elicitation--render state entry))
          (when (buffer-live-p origin)
            (with-current-buffer origin
              (agent-shell-elicitation--goto-control
               :request-id request-id
               :action action
               :field field-name
               :option option))))))))

(cl-defun agent-shell-elicitation--on-create-request
    (&key state acp-request)
  "Handle an incoming `elicitation/create' ACP-REQUEST using STATE."
  (let ((request-id (map-elt acp-request 'id))
        entry)
    (condition-case err
        (setq entry
              (agent-shell-elicitation--normalize-request
               state acp-request))
      (error
       (acp-send-response
        :client (map-elt state :client)
        :response
        `((:request-id . ,request-id)
          (:error . ,(acp-make-error
                      :code -32602
                      :message (error-message-string err)))))))
    (when entry
      ;; JSON-RPC IDs need only be unique while outstanding.  A local
      ;; sequence keeps an inert summary distinct if the Agent later
      ;; reuses the same wire ID for another elicitation.
      (map-put! state :elicitation-count
                (1+ (map-elt state :elicitation-count)))
      (setq entry
            (append
             entry
             (list
              (cons
               :block-id
               (format
                "request-%s-%S"
                (map-elt state :elicitation-count)
                request-id)))))
      (map-put! state :elicitations
                (cons (cons request-id entry)
                      (map-elt state :elicitations)))
      (condition-case err
          (progn
            (agent-shell-elicitation--render state entry)
            (agent-shell--present-user-input-request
             :state state
             :focus
             (lambda ()
               (agent-shell-elicitation--focus-request request-id))))
        (error
         (agent-shell-elicitation--remove state request-id)
         (acp-send-response
          :client (map-elt state :client)
          :response
          `((:request-id . ,request-id)
            (:error . ,(acp-make-error
                        :code -32603
                        :message (error-message-string err))))))))))

(cl-defun agent-shell-elicitation--on-cancel-request
    (&key state request-id)
  "Handle Agent cancellation of incoming REQUEST-ID in STATE."
  (agent-shell-elicitation--send-cancelled-error
   :state state
   :request-id request-id
   :message "Elicitation cancelled by Agent"))

(cl-defun agent-shell-elicitation--request-finished
    (&key state request-id)
  "Cancel forms whose owning client REQUEST-ID has finished in STATE."
  (map-do
   (lambda (elicitation-id entry)
     (when (and (eq (map-elt entry :scope) 'request)
                (equal (map-elt entry :scope-id) request-id))
       (agent-shell-elicitation--send-cancelled-error
        :state state
        :request-id elicitation-id
        :message "Owning ACP request finished")))
   (copy-sequence (map-elt state :elicitations))))

(cl-defun agent-shell-elicitation--tool-finished
    (&key state tool-call-id)
  "Cancel forms whose owning TOOL-CALL-ID has finished in STATE."
  (map-do
   (lambda (elicitation-id entry)
     (when (equal (map-elt entry :tool-call-id) tool-call-id)
       (agent-shell-elicitation--send-cancelled-error
        :state state
        :request-id elicitation-id
        :message "Owning tool call finished")))
   (copy-sequence (map-elt state :elicitations))))

(cl-defun agent-shell-elicitation--session-finished
    (&key state session-id)
  "Cancel forms owned by SESSION-ID during a STATE session change."
  (map-do
   (lambda (elicitation-id entry)
     (when (and (eq (map-elt entry :scope) 'session)
                (equal (map-elt entry :scope-id) session-id))
       (agent-shell-elicitation--send-cancelled-error
        :state state
        :request-id elicitation-id
        :message "Owning ACP session finished")))
   (copy-sequence (map-elt state :elicitations))))

(cl-defun agent-shell-elicitation--dismiss-all (&key state)
  "Dismiss all pending forms in STATE before intentional shutdown."
  (map-do
   (lambda (request-id _entry)
     (condition-case err
         (agent-shell-elicitation--send-user-response
          :state state
          :request-id request-id
          :action "cancel")
       (error
        (agent-shell-elicitation--remove state request-id)
        (message "Could not cancel ACP form: %s"
                 (error-message-string err)))))
   (copy-sequence (map-elt state :elicitations))))

(cl-defun agent-shell-elicitation--abandon-all (&key state)
  "Settle STATE's forms after their ACP connection has disappeared."
  (map-do
   (lambda (request-id _entry)
     (when-let* ((entry
                  (agent-shell-elicitation--take state request-id)))
       (agent-shell-elicitation--settle-ui
        :state state
        :entry entry
        :action "cancel"
        :detail "Agent connection closed")))
   (copy-sequence (map-elt state :elicitations))))

(defun agent-shell-next-elicitation-control ()
  "Move to the next inline elicitation control."
  (interactive)
  (when-let* ((found
               (save-mark-and-excursion
                 (when-let* (((get-text-property
                               (point)
                               'agent-shell-elicitation-control))
                             (next-change
                              (next-single-property-change
                               (point)
                               'agent-shell-elicitation-control)))
                   (goto-char next-change))
                 (when-let* ((match
                              (text-property-search-forward
                               'agent-shell-elicitation-control t t)))
                   (prop-match-beginning match)))))
    (deactivate-mark)
    (goto-char found)
    (point)))

(defun agent-shell-previous-elicitation-control ()
  "Move to the previous inline elicitation control."
  (interactive)
  (when-let* ((found
               (save-mark-and-excursion
                 (when-let* (((get-text-property
                               (point)
                               'agent-shell-elicitation-control))
                             (previous-change
                              (previous-single-property-change
                               (point)
                               'agent-shell-elicitation-control)))
                   (goto-char previous-change))
                 (when-let* ((match
                              (text-property-search-backward
                               'agent-shell-elicitation-control t t)))
                   (prop-match-beginning match)))))
    (deactivate-mark)
    (goto-char found)
    (point)))

(provide 'agent-shell-elicitation)

;;; agent-shell-elicitation.el ends here
