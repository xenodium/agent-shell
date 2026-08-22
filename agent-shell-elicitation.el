;;; agent-shell-elicitation.el --- ACP elicitation forms -*- lexical-binding: t; -*-

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
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; Renders `elicitation/create' requests as an in-buffer form.
;;
;; An agent uses an elicitation to ask a structured question mid-turn
;; and wait for the answer.  Support is opt-in per client: an agent
;; cannot tell an unimplemented elicitation from a declined one, so it
;; adapts by not asking at all.  The Claude adapter drops its
;; `AskUserQuestion' tool from the model's toolset when the client stays
;; silent, and auto-declines elicitations coming from MCP servers.
;;
;; Advertising `elicitation.form' is therefore a promise to render any
;; form the schema permits, so every `ElicitationPropertySchema' variant
;; is covered here.  What is not covered is constraint validation
;; (`minLength', `pattern', `minimum', `minItems' and friends): only a
;; required-presence gate runs on submit, since without it we would send
;; a response the schema forbids.  An agent that cares about the rest
;; rejects the answer and asks again.
;;
;; URL mode (`elicitation.url') is a separate capability and is
;; deliberately not advertised: agents must not send a mode the client
;; did not ask for, so an agent will not send it.
;;
;; Form state lives in `agent-shell--state' under `:elicitations', keyed
;; by the JSON-RPC request id -- the only identifier every elicitation
;; carries.  Rendered text holds identity only (which elicitation, which
;; field, which option); the named commands below read those properties
;; at point and look the authoritative entry up.  Fragment-local text
;; properties would not do: `agent-shell--update-fragment' writes into
;; both the shell buffer and the viewport buffer, so a fragment's own
;; state exists in two copies, which for field values is a correctness
;; bug rather than the harmless divergence it is for fold state.
;;
;; See https://github.com/xenodium/agent-shell/issues/792

;;; Code:

(require 'agent-shell-faces)
(require 'agent-shell-ui)
(require 'map)
(require 'seq)
(require 'text-property-search)
(eval-when-compile
  (require 'cl-lib))

(declare-function acp-make-error "acp")
(declare-function acp-send-response "acp")
(declare-function agent-shell--append-transcript "agent-shell")
(declare-function agent-shell--cancel-idle-timer "agent-shell")
(declare-function agent-shell--start-idle-timer "agent-shell")
(declare-function agent-shell--active-requests-p "agent-shell")
(declare-function agent-shell--delete-fragment "agent-shell")
(declare-function agent-shell--emit-event "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell-viewport--shell-buffer "agent-shell-viewport")

(defvar agent-shell--transcript-file)

(defun agent-shell-elicitation--questionnaire-p (raw-input)
  "Return non-nil when RAW-INPUT is an ask-the-user questionnaire.

Agents bridging an ask-the-user tool onto elicitation call it with the
questions as its input, so the tool call carries the whole questionnaire
and its `content\=' later carries the answers.  Recognising that shape is
what lets such a call render as prose rather than as a JSON dump, live
and on a restored session alike -- restore replays notifications but
never requests, so the form itself never comes back and the tool call is
all there is.

Read from the input rather than from a mark left when the form was
raised, since nothing marks it on restore.

For example:

  (agent-shell-elicitation--questionnaire-p
   \='((questions . [((question . \"Which colour?\"))])))
  => t"
  (when-let* ((questions (map-elt raw-input 'questions))
              ((not (seq-empty-p questions))))
    (seq-every-p (lambda (question) (map-elt question 'question)) questions)))

(cl-defun agent-shell-elicitation--pending-for-tool-call-p (&key state tool-call-id)
  "Return non-nil when a form raised for TOOL-CALL-ID in STATE awaits an answer.

While it does, that tool call renders no body of its own: the form is
showing the same questions interactively just below it."
  (when tool-call-id
    (seq-find (lambda (entry)
                (and (eq (map-elt (cdr entry) :status) 'pending)
                     (equal (map-elt (cdr entry) :tool-call-id) tool-call-id)))
              (map-elt state :elicitations))))

(defconst agent-shell-elicitation--advertised-modes '("form")
  "Elicitation modes this client advertises in `initialize'.

An agent must not send a mode we did not advertise, so anything else
is answered with an invalid-params error rather than rendered.")


(defun agent-shell-elicitation--assoc-put (alist key value)
  "Return ALIST with KEY bound to VALUE, replacing any existing binding.

`map-insert' would prepend a second binding rather than replace the
first, so an alist edited once per interaction would grow an entry per
keystroke while `map-elt' quietly read the newest.

For example:

  (agent-shell-elicitation--assoc-put \='((a . 1) (b . 2)) \='a 3)
  => ((a . 3) (b . 2))"
  (cons (cons key value) (map-delete (copy-alist alist) key)))

;;; Schema parsing

(defun agent-shell-elicitation--enum-options (values)
  "Normalize VALUES, an untitled enum, into option alists.

VALUES arrives as a JSON array, so a vector.  Each option becomes:

  ((:value . \"red\") (:title . \"red\") (:description . nil))

For example:

  (agent-shell-elicitation--enum-options [\"red\" \"blue\"])
  => (((:value . \"red\") (:title . \"red\") (:description . nil))
      ((:value . \"blue\") (:title . \"blue\") (:description . nil)))"
  (seq-map (lambda (value)
             (list (cons :value value)
                   (cons :title value)
                   (cons :description nil)))
           (append values nil)))

(defun agent-shell-elicitation--titled-options (options)
  "Normalize OPTIONS, a sequence of ACP `EnumOption', into option alists.

Each `EnumOption' carries a `const' value, a human-readable `title' and
an optional `description'.

For example:

  (agent-shell-elicitation--titled-options
   [((const . \"a\") (title . \"Approach A\") (description . \"Safer\"))])
  => (((:value . \"a\") (:title . \"Approach A\") (:description . \"Safer\")))"
  (seq-map (lambda (option)
             (list (cons :value (map-elt option 'const))
                   (cons :title (or (map-elt option 'title)
                                    (map-elt option 'const)))
                   (cons :description (map-elt option 'description))
                   (cons :preview (agent-shell-elicitation--option-preview option))))
           (append options nil)))

(defconst agent-shell-elicitation--option-meta-key '_claude/askUserQuestionOption
  "`_meta\=' key an option's preview travels under.

Unlike the free-text companion marker, this one is namespaced to the
agent that sends it, so only Claude sends a preview today.  An option
without it renders without a preview to open.")

(defun agent-shell-elicitation--option-preview (option)
  "Return OPTION's preview text, or nil when there is none.

A preview is the longer form of an answer -- a mockup, a snippet, an
outline of what picking it would do -- that an agent attaches for the
client to show on demand.

For example:

  (agent-shell-elicitation--option-preview
   \='((const . \"a\") (_meta (_claude/askUserQuestionOption (preview . \"…\")))))
  => \"…\""
  (map-nested-elt option (list '_meta
                               agent-shell-elicitation--option-meta-key
                               'preview)))

(defun agent-shell-elicitation--field-options (schema)
  "Return SCHEMA's selectable options, or nil when it has none.

Both spellings of each enum shape are folded into one option list:
`oneOf' and `enum' for a single-select string, `items.anyOf' and
`items.enum' for a multi-select array."
  (cond ((map-elt schema 'oneOf)
         (agent-shell-elicitation--titled-options (map-elt schema 'oneOf)))
        ((map-elt schema 'enum)
         (agent-shell-elicitation--enum-options (map-elt schema 'enum)))
        ((map-nested-elt schema '(items anyOf))
         (agent-shell-elicitation--titled-options (map-nested-elt schema '(items anyOf))))
        ((map-nested-elt schema '(items enum))
         (agent-shell-elicitation--enum-options (map-nested-elt schema '(items enum))))))

(defun agent-shell-elicitation--field-type (schema)
  "Return SCHEMA's field type as a symbol.

One of `single-select', `multi-select', `boolean', `string', `number',
`integer', or `unsupported' for the schema's forward-compatibility
variant, whose `type' this client has no way to render.

A string is a single-select only once it offers options, so the two
share a `type' and are told apart by that:

  (agent-shell-elicitation--field-type \='((type . \"string\")))
  => string

  (agent-shell-elicitation--field-type
   \='((type . \"string\") (enum . [\"a\"])))
  => single-select"
  (pcase (map-elt schema 'type)
    ("string" (if (agent-shell-elicitation--field-options schema)
                  'single-select
                'string))
    ("array" 'multi-select)
    ("boolean" 'boolean)
    ("number" 'number)
    ("integer" 'integer)
    (_ 'unsupported)))

(defun agent-shell-elicitation--field-default (type schema)
  "Return SCHEMA's initial value for a field of TYPE, or nil when unset.

A multi-select default is a JSON array, so it arrives as a vector and is
returned as a list.  A boolean `false' default is indistinguishable from
an absent one, since acp.el parses JSON `false' and `null' alike as nil;
such a field simply starts unchecked.

For example:

  (agent-shell-elicitation--field-default
   \='multi-select \='((default . [\"a\" \"b\"])))
  => (\"a\" \"b\")"
  (when-let* ((default (map-elt schema 'default)))
    (pcase type
      ('multi-select (append default nil))
      ('boolean t)
      (_ default))))

(defconst agent-shell-elicitation--custom-answer-meta-key '_askUserQuestionCustomAnswer
  "`_meta\=' key marking a text field as another field\='s free-text companion.

Agents bridging an ask-the-user tool onto elicitation send a select
field and a free-text \"Other\" field per question, and mark the second
as belonging to the first.  The key is deliberately un-namespaced so the
same marker is recognisable across Claude, Codex and any other bridge.

Unlike agent-namespaced `_meta\=' decoration, acting on this is what makes
the form readable: rendered as two independent fields, one question
looks like two, and a user can answer it twice in contradictory ways.")

(defun agent-shell-elicitation--custom-answer-for (schema)
  "Return the field key SCHEMA declares itself the free-text companion of.

Returns nil for an ordinary field.  See
`agent-shell-elicitation--custom-answer-meta-key\='."
  (map-nested-elt schema (list '_meta
                               agent-shell-elicitation--custom-answer-meta-key
                               'questionId)))

(defun agent-shell-elicitation--link-custom-answers (fields)
  "Fold each free-text companion in FIELDS into the select it answers for.

The select gains `:custom-key\=', naming the companion; the companion gains
`:folded-into\=', naming the select.  The renderer then offers the companion
as one more option of that select rather than as a field of its own, so
picking an option and typing an answer are alternatives rather than both
being answerable at once.

A companion whose named field is missing, or is not a select, is left
alone and renders as the ordinary text field it is.

For example, given a select `question_0' and a companion naming it:

  => the select gains (:custom-key . \"question_0_custom\")
     and the companion gains (:folded-into . \"question_0\")"
  (let ((links (seq-keep
                (lambda (field)
                  (when-let* (((eq (map-elt field :type) 'string))
                              (parent-key (map-elt field :custom-answer-for))
                              (parent (seq-find (lambda (each)
                                                  (equal (map-elt each :key) parent-key))
                                                fields))
                              ((memq (map-elt parent :type) '(single-select multi-select))))
                    (cons parent-key (map-elt field :key))))
                fields)))
    (seq-map (lambda (field)
               (cond ((map-elt links (map-elt field :key))
                      (agent-shell-elicitation--assoc-put
                       field :custom-key (map-elt links (map-elt field :key))))
                     ((rassoc (map-elt field :key) links)
                      (agent-shell-elicitation--assoc-put
                       field :folded-into (car (rassoc (map-elt field :key) links))))
                     (t field)))
             fields)))

(cl-defun agent-shell-elicitation--parse-schema (&key requested-schema)
  "Return REQUESTED-SCHEMA's fields as an ordered list of descriptors.

REQUESTED-SCHEMA is an ACP `ElicitationSchema': an object schema whose
`properties' are the form's fields and whose `required' array names the
ones that must carry a value.  Property order is the order the agent
sent, which is the order the form renders in.

Each descriptor is an alist:

  ((:key . \"colour\")
   (:type . single-select)
   (:raw-type . \"string\")
   (:title . \"Colour\")
   (:description . \"Pick one.\")
   (:required . t)
   (:options . (((:value . \"red\") (:title . \"Red\") (:description . nil))))
   (:default . nil)
   (:custom-answer-for . nil)
   (:custom-key . nil)
   (:folded-into . nil))

`:custom-answer-for\=' through `:folded-into\=' carry the free-text
companion link, see `agent-shell-elicitation--link-custom-answers\='.

For example:

  (agent-shell-elicitation--parse-schema
   :requested-schema \\='((type . \"object\")
                       (properties . ((note . ((type . \"string\")))))
                       (required . [\"note\"])))
  => (((:key . \"note\") (:type . string) (:raw-type . \"string\")
       (:title . nil) (:description . nil) (:required . t)
       (:options . nil) (:default . nil) (:custom-answer-for . nil)
       (:custom-key . nil) (:folded-into . nil)))"
  (let ((required (append (map-elt requested-schema 'required) nil)))
    (agent-shell-elicitation--link-custom-answers
     (seq-map (lambda (property)
                (let ((type (agent-shell-elicitation--field-type (cdr property))))
                  (list (cons :key (symbol-name (car property)))
                        (cons :type type)
                        (cons :raw-type (map-elt (cdr property) 'type))
                        (cons :title (map-elt (cdr property) 'title))
                        (cons :description (map-elt (cdr property) 'description))
                        (cons :required (and (member (symbol-name (car property)) required) t))
                        (cons :options (agent-shell-elicitation--field-options (cdr property)))
                        (cons :default (agent-shell-elicitation--field-default
                                        type (cdr property)))
                        (cons :custom-answer-for
                              (agent-shell-elicitation--custom-answer-for (cdr property))))))
              (map-elt requested-schema 'properties)))))

(defun agent-shell-elicitation--initial-values (fields)
  "Return the starting value alist for FIELDS, seeded from their defaults.

Fields with no default are left out entirely, so an untouched optional
field is omitted from the response rather than sent as null.

For example:

  (agent-shell-elicitation--initial-values
   \='(((:key . \"n\") (:default . 3)) ((:key . \"note\") (:default . nil))))
  => ((\"n\" . 3))"
  (seq-reduce (lambda (values field)
                (if-let* ((value (map-elt field :default)))
                    (cons (cons (map-elt field :key) value) values)
                  values))
              fields nil))


;;; Response encoding

(defun agent-shell-elicitation--encode-value (type value)
  "Encode VALUE for a field of TYPE as an ACP `ElicitationContentValue'.

Booleans serialize as t/`:false' rather than t/nil, matching acp.el's
convention: nil is how it spells JSON null.  Integers are floored so
`1' never goes out as `1.0'.  A multi-select becomes a vector, since
`json-serialize' reads a plain list as an alist.

For example:

  (agent-shell-elicitation--encode-value \\='integer 3.0) => 3
  (agent-shell-elicitation--encode-value \\='boolean nil) => :false
  (agent-shell-elicitation--encode-value \\='multi-select \\='(\"a\")) => [\"a\"]"
  (pcase type
    ('boolean (if (eq value t) t :false))
    ('integer (truncate value))
    ('number (float value))
    ('multi-select (vconcat value))
    (_ value)))

(defun agent-shell-elicitation--value-present-p (type value)
  "Return non-nil when VALUE counts as an answer for a field of TYPE.

An empty multi-select is no answer, nor is an empty string.  Booleans
are not decided here: an unticked checkbox is indistinguishable from an
absent one, so whether it answers anything depends on the field rather
than the value -- see `agent-shell-elicitation--checkbox-answers-p'."
  (pcase type
    ('multi-select (and value t))
    ('string (and (stringp value) (not (string-empty-p value))))
    (_ (and value t))))

(defun agent-shell-elicitation--checkbox-answers-p (field value)
  "Return non-nil when checkbox FIELD holding VALUE has an answer to send.

An unticked checkbox is indistinguishable on screen from one the user
never reached, so it is left out rather than reported as `no'.  It does
answer when ticked, when the schema marked it required (a required
checkbox has to be answerable `no'), or when a default gave it a
starting position.

Zed's elicitation form applies the same rule.

One caveat is specific to this client: acp.el decodes JSON `false' and
`null' alike as nil, so a schema default of `false' is indistinguishable
from no default at all, and such a checkbox is treated as having none."
  (or (eq value t)
      (map-elt field :required)
      (and (map-elt field :default) t)))

(cl-defun agent-shell-elicitation--field-answer (&key field values)
  "Return what FIELD answers given VALUES, or nil when it answers nothing.

Resolves a select and its free-text companion into the one answer they
jointly make, since only one of the two can carry it:

  ((:key . \"question_0_custom\")  ; the field it is sent under
   (:type . string)                ; how to encode it
   (:value . \"Red, teal\")         ; what goes on the wire
   (:display . \"Red, teal\"))      ; what a settled form shows

A typed answer to a single-select replaces the pick, matching how such
a question reads: one choice, of which \"my own\" is one.

A typed answer to a multi-select joins the ticks instead, because
ticking boxes and adding one of your own is a single answer.  It has to
travel as one string: agents bridging their ask-the-user tool onto
elicitation read the companion first and return early, so a response
carrying both loses the ticks -- verified against the Claude adapter,
which reported back only the typed word.  Comma-joined is the shape
that adapter builds from an array of ticks on its own, so the model
sees no difference."
  (let ((type (map-elt field :type))
        (value (map-elt values (map-elt field :key)))
        (custom (map-elt values (map-elt field :custom-key))))
    (cond
     ((eq type 'unsupported) nil)
     ((eq type 'boolean)
      (when (agent-shell-elicitation--checkbox-answers-p field value)
        (list (cons :key (map-elt field :key))
              (cons :type 'boolean)
              (cons :value value)
              (cons :display (agent-shell-elicitation--render-value field value)))))
     ((agent-shell-elicitation--value-present-p 'string custom)
      (let ((answer (if (and (eq type 'multi-select) value)
                        (string-join (append value (list custom)) ", ")
                      custom)))
        (list (cons :key (map-elt field :custom-key))
              (cons :type 'string)
              (cons :value answer)
              (cons :display answer))))
     ((agent-shell-elicitation--value-present-p type value)
      (list (cons :key (map-elt field :key))
            (cons :type type)
            (cons :value value)
            (cons :display (agent-shell-elicitation--render-value field value)))))))

(defun agent-shell-elicitation--answerable (fields)
  "Return FIELDS minus every companion folded into a select.
A companion contributes through its select, so walking both would count
one question twice."
  (seq-remove (lambda (field) (map-elt field :folded-into)) fields))

(cl-defun agent-shell-elicitation--make-content (&key fields values)
  "Encode VALUES for FIELDS as an elicitation response `content' map.

Fields with no value are omitted rather than sent as null, and so are
fields whose type this client could not render.  Returns nil when
nothing was answered, which serializes as an empty JSON object.

A select and its free-text companion contribute one entry between them,
see `agent-shell-elicitation--field-answer'.

For example:

  (agent-shell-elicitation--make-content
   :fields \='(((:key . \"n\") (:type . integer)) ((:key . \"m\") (:type . string)))
   :values \='((\"n\" . 2)))
  => ((n . 2))"
  (seq-reduce (lambda (content field)
                (if-let* ((answer (agent-shell-elicitation--field-answer
                                   :field field :values values)))
                    (append content
                            (list (cons (intern (map-elt answer :key))
                                        (agent-shell-elicitation--encode-value
                                         (map-elt answer :type)
                                         (map-elt answer :value)))))
                  content))
              (agent-shell-elicitation--answerable fields) nil))

(cl-defun agent-shell-elicitation--make-response (&key request-id action fields values)
  "Instantiate an \"elicitation/create\" response.

REQUEST-ID is the id of the incoming request this answers.  ACTION is
one of `accept', `decline' or `cancel'.  FIELDS and VALUES are only
read for `accept', to build the response `content'.

For example:

  (agent-shell-elicitation--make-response :request-id 7 :action \\='decline)
  => ((:request-id . 7) (:result . ((action . \"decline\"))))"
  (unless request-id
    (error ":request-id is required"))
  `((:request-id . ,request-id)
    (:result . ,(if (eq action 'accept)
                    `((action . "accept")
                      (content . ,(agent-shell-elicitation--make-content
                                   :fields fields :values values)))
                  `((action . ,(symbol-name action)))))))


;;; State

(defun agent-shell-elicitation--ensure-state (state)
  "Add the keys elicitation needs to STATE when a session predates them.
Without this, `map-put!' fails on a mid-session package update."
  (dolist (key '(:elicitations :active-requests))
    (unless (assq key state)
      (nconc state (list (cons key nil)))))
  state)

(cl-defun agent-shell-elicitation--clear (&key state)
  "Drop STATE's forms at the end of a turn.

A form settles within the turn that raised it -- the agent is blocked
on the answer until it does -- so nothing here is still awaiting one.
Settled entries are cleared alongside `:tool-calls' rather than kept:
the rendered text is already correct and the transcript already has the
answer, so there is nothing left to look up."
  (agent-shell-elicitation--ensure-state state)
  (map-put! state :elicitations nil)
  (map-put! state :active-requests
            (seq-remove (lambda (request)
                          (equal (map-elt request :method) "elicitation/create"))
                        (map-elt state :active-requests))))

(defun agent-shell-elicitation--preview-open-p (elicitation key value)
  "Return non-nil when the preview for KEY's VALUE option is open in ELICITATION.

Kept in the elicitation rather than in the rendered text for the same
reason field values are: the form is drawn into the shell buffer and the
viewport buffer both, so text-local state would exist in two copies that
could disagree."
  (member (cons key value) (map-elt elicitation :previews)))

(defun agent-shell-elicitation--field (elicitation key)
  "Return ELICITATION's field named KEY, or nil when it has none."
  (seq-find (lambda (field) (equal (map-elt field :key) key))
            (map-elt elicitation :fields)))

(defun agent-shell-elicitation--get (state id)
  "Return the elicitation ID's entry in STATE, or nil when it is gone."
  (map-nested-elt state (list :elicitations id)))

(defun agent-shell-elicitation--put (state id elicitation)
  "Store ELICITATION under ID in STATE's `:elicitations'."
  (agent-shell-elicitation--ensure-state state)
  (map-put! state :elicitations
            (agent-shell-elicitation--assoc-put
             (map-elt state :elicitations) id elicitation)))

(cl-defun agent-shell-elicitation--pending-p (&key shell-buffer)
  "Return non-nil if an elicitation is awaiting an answer.
When SHELL-BUFFER is non-nil, check that buffer instead of the current one."
  (with-current-buffer (or shell-buffer (current-buffer))
    (seq-some (lambda (entry)
                (eq (map-elt (cdr entry) :status) 'pending))
              (map-elt (agent-shell--state) :elicitations))))

(defun agent-shell-elicitation--shell-buffer ()
  "Return the shell buffer backing the current buffer.

A form renders into both the shell buffer and the viewport buffer, but
only the shell buffer holds the authoritative state, so acting on a
control resolves back to it either way."
  (if (derived-mode-p 'agent-shell-viewport-view-mode)
      (agent-shell-viewport--shell-buffer)
    (current-buffer)))


;;; Rendering

(defvar agent-shell-elicitation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-shell-elicitation-act)
    (define-key map [mouse-1] #'agent-shell-elicitation-act)
    (define-key map [remap self-insert-command] #'ignore)
    (define-key map (kbd "C-c C-c") #'agent-shell-elicitation-interrupt)
    map)
  "Keymap active on an elicitation form's controls.

Applied as a `keymap' text property by
`agent-shell-elicitation--make-control', so it reaches the options,
fields and buttons and nothing else.  Scoping the keys to that text is
what leaves the rest of the buffer alone: RET still submits the prompt
and typing still inserts everywhere outside a control.

Typing on a control runs `ignore' rather than erroring, since that text
is read-only.

Rebind with `define-key' rather than `setq': already rendered forms hold
on to this keymap object.")

(defvar agent-shell-elicitation-preview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map agent-shell-elicitation-map)
    (define-key map (kbd "?") #'agent-shell-elicitation-toggle-preview)
    map)
  "Keymap active on a control whose answer carries a preview.

Everything `agent-shell-elicitation-map' binds, plus \\`?' to open the
preview.  Applied only where there is one to open, so on every other
control \\`?' is left to whatever the buffer already binds it to -- the
viewport, for one, opens its help menu with it.

Only \\`?' lives here; RET and the rest are looked up in the parent, so
rebinding them there still reaches these controls.  That is what
`agent-shell--make-button' cannot do (see
`agent-shell-elicitation--box'): it binds RET in the child itself, where
it shadows the parent.

Rebind with `define-key' rather than `setq': already rendered forms hold
on to this keymap object.")

(defconst agent-shell-elicitation--indent "    "
  "Indentation shared by every line of a rendered form.")

(defconst agent-shell-elicitation--empty-value "(empty)"
  "Placeholder shown in place of a text field with no value yet.")

(cl-defun agent-shell-elicitation--make-control (&key text id key value action hint help
                                                     (navigable t)
                                                     (keymap agent-shell-elicitation-map))
  "Return TEXT propertized as an elicitation control.

ID identifies the elicitation, KEY the field within it and VALUE the
option within that field, so `agent-shell-elicitation-act' can look the
authoritative entry up rather than close over it.  ACTION names what
invoking the control does: `select', `toggle', `read', `custom',
`preview', `submit' or `decline'.  HINT is a verb echoed when the cursor enters
TEXT.

HELP is offered as `help-echo', which `display-local-help' reads out, so
a preview can be read with \\[display-local-help] without expanding it.

The first character carries `agent-shell-elicitation-navigable', which
is what TAB stops on, putting point at the start of the control where a
screen reader reads the whole line from.  Pass NAVIGABLE nil for chrome
that acts but should not be a stop of its own, so a question with a
preview to open still takes one TAB rather than two.

KEYMAP is applied as the `keymap' property and defaults to
`agent-shell-elicitation-map'.  Pass
`agent-shell-elicitation-preview-map' for a control that has a preview,
which is the only place \\`?' should be taken over."
  (let ((control (apply #'agent-shell--add-text-properties
                        text
                        (append
                         (list 'agent-shell-elicitation-control
                               (list (cons :id id)
                                     (cons :key key)
                                     (cons :value value)
                                     (cons :action action))
                               'keymap keymap
                               'pointer 'hand
                               'rear-nonsticky t)
                         (when help
                           (list 'help-echo help))
                         (when hint
                           (list 'cursor-sensor-functions
                                 (list (lambda (_window _old-pos sensor-action)
                                         (when (eq sensor-action 'entered)
                                           (agent-shell-ui--echo-action-hint
                                            hint #'agent-shell-elicitation-act
                                            keymap))))))))))
    (when navigable
      (put-text-property 0 1 'agent-shell-elicitation-navigable t control))
    control))

(defun agent-shell-elicitation--box (text)
  "Return TEXT drawn as a button, matching `agent-shell--make-button'.
Uses [ ] brackets on displays that cannot draw a box border.

Only the drawing is borrowed, not the helper itself.
`agent-shell--make-button' binds RET in a fresh keymap per call and
keeps the shared map as its parent, so a child binding shadows the
parent and rebinding RET in `agent-shell-elicitation-map' would not
reach the buttons -- the problem its own TODO records as issue #759.
Applying that map directly as a `keymap' property instead leaves every
key in one rebindable place, and this is the only piece of
`agent-shell--make-button' left to reproduce."
  (if (display-graphic-p)
      (agent-shell--add-text-properties (format " %s " text)
                                        'font-lock-face '(:box t)
                                        'face '(:box t))
    (format "[ %s ]" text)))

(defun agent-shell-elicitation--field-heading (field)
  "Return FIELD's display heading, falling back to its key."
  (or (map-elt field :title) (map-elt field :key)))

(defun agent-shell-elicitation--option-title (field value)
  "Return the title FIELD gives VALUE, or VALUE itself when it has none.

An option's `const' is what travels on the wire, but its `title' is what
the user picked by name, so that is what a settled form reads back."
  (or (map-elt (seq-find (lambda (option)
                           (equal (map-elt option :value) value))
                         (map-elt field :options))
               :title)
      value))

(defun agent-shell-elicitation--render-value (field value)
  "Return VALUE for FIELD as the text of a settled form.

Selected options read back by title rather than by the `const' sent on
the wire, a multi-select renders its picks comma-joined, and a boolean
renders as yes or no.

For example, a single-select holding \"a\" whose option is titled
\"Approach A\" renders as \"Approach A\"."
  (pcase (map-elt field :type)
    ('single-select (agent-shell-elicitation--option-title field value))
    ('multi-select (string-join
                    (seq-map (lambda (each)
                               (agent-shell-elicitation--option-title field each))
                             value)
                    ", "))
    ('boolean (if (eq value t) "yes" "no"))
    (_ (format "%s" value))))

(defun agent-shell-elicitation--marker (multi selected)
  "Return the marker for an option, given MULTI and whether it is SELECTED.
Checkboxes for a multi-select, radio buttons otherwise."
  (cond ((and multi selected) "[x]")
        (multi "[ ]")
        (selected "(*)")
        (t "( )")))

(cl-defun agent-shell-elicitation--make-option-line (&key id field value title description
                                                         action multi selected preview
                                                         preview-open)
  "Return one marked option line of FIELD in elicitation ID.

TITLE and DESCRIPTION label it, VALUE identifies which option it is (nil
for the free-text companion, which has no option value of its own),
ACTION is what invoking it does, and MULTI and SELECTED pick the marker.

PREVIEW is the longer form of the answer, when the agent sent one.  It
hangs off this option rather than becoming a control of its own: the
disclosure glyph opens it, so the option is still a single TAB stop, and
`display-local-help' reads the preview out without opening it.
PREVIEW-OPEN says whether it is currently open."
  (concat
   agent-shell-elicitation--indent
   (agent-shell-elicitation--make-control
    :text (format "%s %s" (agent-shell-elicitation--marker multi selected) title)
    :id id
    :key (map-elt field :key)
    :value value
    :action action
    :help preview
    :keymap (if preview
                agent-shell-elicitation-preview-map
              agent-shell-elicitation-map)
    :hint (pcase action
            ('custom "type your own answer")
            ('toggle "toggle this option")
            (_ "pick this option")))
   (when preview
     ;; The same glyph every fold in the buffer uses, and no navigable
     ;; property, so TAB passes over it to the next option.
     (concat " " (agent-shell-elicitation--make-control
                  :text (if preview-open "▼" "▶")
                  :id id :key (map-elt field :key) :value value
                  :action 'preview :navigable nil
                  :help preview
                  :keymap agent-shell-elicitation-preview-map
                  :hint (if preview-open "hide this preview" "show this preview"))))
   (when description
     (format "\n%s    %s" agent-shell-elicitation--indent description))
   (when (and preview preview-open)
     (concat "\n" (agent-shell-elicitation--indent-block preview)))))

(defun agent-shell-elicitation--indent-block (text)
  "Return TEXT with every line indented to sit under an option."
  (mapconcat (lambda (line)
               (concat agent-shell-elicitation--indent "    " line))
             (split-string text "\n")
             "\n"))

(cl-defun agent-shell-elicitation--make-select-lines (&key id field value multi custom custom-value
                                                          elicitation)
  "Return FIELD's option lines for elicitation ID given its current VALUE.

With MULTI, options are checkboxes toggled independently and VALUE is a
list; otherwise they are radio buttons and VALUE is the single pick.

CUSTOM is FIELD's free-text companion, when it has one, and CUSTOM-VALUE
whatever has been typed into it.  It renders as one more option of this
field rather than as a field of its own, and shows what was typed, since
for that option the answer is the option."
  (append
   (seq-map (lambda (option)
              (agent-shell-elicitation--make-option-line
               :id id :field field
               :value (map-elt option :value)
               :title (map-elt option :title)
               :description (map-elt option :description)
               :action (if multi 'toggle 'select)
               :multi multi
               :selected (if multi
                             (and (member (map-elt option :value) value) t)
                           (equal (map-elt option :value) value))
               :preview (map-elt option :preview)
               :preview-open (agent-shell-elicitation--preview-open-p
                              elicitation (map-elt field :key) (map-elt option :value))))
            (map-elt field :options))
   (when custom
     (list (agent-shell-elicitation--make-option-line
            :id id :field custom
            :title (if custom-value
                       (format "%s: %s"
                               (agent-shell-elicitation--field-heading custom)
                               custom-value)
                     (agent-shell-elicitation--field-heading custom))
            :description (map-elt custom :description)
            :action 'custom
            :multi multi
            :selected (and custom-value t))))))

(cl-defun agent-shell-elicitation--make-field-text (&key id field value custom custom-value
                                                        elicitation)
  "Return FIELD's rendered lines for elicitation ID given its current VALUE.

Selects render as a heading, an optional description, then one marked
line per option.  A boolean renders as a lone checkbox carrying its own
heading, since a single checkbox reads correctly without the extra
line.  Text-ish fields render as `Heading: value', preceded by their
description so it is read before the minibuffer opens.

CUSTOM is FIELD's free-text companion and CUSTOM-VALUE what has been
typed into it, both nil unless FIELD is a select that has one.

For example, a single-select renders as:

    Colour
    Pick one.
    (*) Red
    ( ) Blue"
  (let ((heading (agent-shell-elicitation--field-heading field))
        (description (map-elt field :description)))
    (string-join
     (delq nil
           (pcase (map-elt field :type)
             ((or 'single-select 'multi-select)
              (append (list (concat agent-shell-elicitation--indent heading)
                            (when description
                              (concat agent-shell-elicitation--indent description)))
                      (agent-shell-elicitation--make-select-lines
                       :id id :field field :value value
                       :multi (eq (map-elt field :type) 'multi-select)
                       :custom custom :custom-value custom-value
                       :elicitation elicitation)))
             ('boolean
              (list (concat agent-shell-elicitation--indent
                            (agent-shell-elicitation--make-control
                             :text (format "%s %s" (if (eq value t) "[x]" "[ ]") heading)
                             :id id :key (map-elt field :key) :action 'toggle
                             :hint "toggle this checkbox"))
                    (when description
                      (concat agent-shell-elicitation--indent "    " description))))
             ('unsupported
              (list (concat agent-shell-elicitation--indent heading)
                    (format "%sUnsupported field type \"%s\", left unanswered."
                            agent-shell-elicitation--indent
                            (map-elt field :raw-type))))
             (_
              (list (when description
                      (concat agent-shell-elicitation--indent description))
                    (concat agent-shell-elicitation--indent
                            (agent-shell-elicitation--make-control
                             :text (format "%s: %s" heading
                                           (if value
                                               (agent-shell-elicitation--render-value field value)
                                             agent-shell-elicitation--empty-value))
                             :id id :key (map-elt field :key) :action 'read
                             :hint "edit this field"))))))
     "\n")))

(defun agent-shell-elicitation--answered-p (elicitation field)
  "Return non-nil when FIELD of ELICITATION carries an answer.

Asks the same question the response encoder asks, so the gate on Submit
and what actually goes out can never disagree: a select answered by its
free-text companion counts, and so does a checkbox the schema requires.

See `agent-shell-elicitation--field-answer'."
  (and (agent-shell-elicitation--field-answer
        :field field :values (map-elt elicitation :values))
       t))

(defun agent-shell-elicitation--blocker (elicitation)
  "Return why ELICITATION cannot be submitted, or nil when it can.

A required field this client cannot render blocks submission outright,
since no value we could send would satisfy the schema; Decline is then
the only answer left.  A required field merely left empty blocks until
it is filled in."
  (when-let* ((field (seq-find
                      (lambda (field)
                        (and (map-elt field :required)
                             (or (eq (map-elt field :type) 'unsupported)
                                 (not (agent-shell-elicitation--answered-p
                                       elicitation field)))))
                      (map-elt elicitation :fields))))
    (if (eq (map-elt field :type) 'unsupported)
        (cons 'unsupported
              (format "\"%s\" is required but uses field type \"%s\", which this client cannot render."
                      (agent-shell-elicitation--field-heading field)
                      (map-elt field :raw-type)))
      (cons 'missing
            (format "\"%s\" is required."
                    (agent-shell-elicitation--field-heading field))))))

(defun agent-shell-elicitation--make-buttons (elicitation)
  "Return ELICITATION's button row, plus the reason Submit is missing.

Submit is dropped when a required field uses a type this client cannot
render, since no answer we could send would satisfy the schema.  A
required field that is merely empty keeps Submit, which then reports the
missing field rather than sending an invalid response."
  (let* ((blocker (agent-shell-elicitation--blocker elicitation))
         (unanswerable (eq (car blocker) 'unsupported)))
    (string-join
     (delq nil
           (list
            (when unanswerable
              (format "%sCannot submit: %s" agent-shell-elicitation--indent (cdr blocker)))
            (concat
             agent-shell-elicitation--indent
             (unless unanswerable
               (concat (agent-shell-elicitation--make-control
                        :text (agent-shell-elicitation--box "Submit")
                        :id (map-elt elicitation :id) :action 'submit
                        :hint "submit this form")
                       " "))
             (agent-shell-elicitation--make-control
              :text (agent-shell-elicitation--box "Decline")
              :id (map-elt elicitation :id) :action 'decline
              :hint "decline this form"))))
     "\n\n")))

(defun agent-shell-elicitation--make-answer-lines (elicitation)
  "Return one \"Question: answer\" line per question ELICITATION answered.

Read from the same answers the response carries, so a settled form and
the wire can never disagree."
  (seq-keep (lambda (field)
              (when-let* ((answer (agent-shell-elicitation--field-answer
                                   :field field
                                   :values (map-elt elicitation :values))))
                (format "%s%s: %s"
                        agent-shell-elicitation--indent
                        (agent-shell-elicitation--field-heading field)
                        (map-elt answer :display))))
            (agent-shell-elicitation--answerable (map-elt elicitation :fields))))

(defun agent-shell-elicitation--make-settled-text (elicitation)
  "Return ELICITATION's body once it has been answered, declined or cancelled.

Unlike a permission dialog, which deletes its fragment on answer, a
settled form stays on screen showing what was sent."
  (pcase (map-elt elicitation :status)
    ('answered
     (if-let* ((answers (agent-shell-elicitation--make-answer-lines elicitation)))
         (concat agent-shell-elicitation--indent "Answered\n\n"
                 (string-join answers "\n"))
       (concat agent-shell-elicitation--indent "Answered with no input")))
    ('declined (concat agent-shell-elicitation--indent "Declined"))
    (_ (concat agent-shell-elicitation--indent "Cancelled"))))

(defun agent-shell-elicitation--make-field-texts (elicitation)
  "Return ELICITATION's questions, one rendered block each.

Companions are left out: each renders inside the select it belongs to,
so walking both would render one question twice."
  (seq-map (lambda (field)
             (agent-shell-elicitation--make-field-text
              :id (map-elt elicitation :id)
              :field field
              :value (map-nested-elt elicitation (list :values (map-elt field :key)))
              :custom (agent-shell-elicitation--field
                       elicitation (map-elt field :custom-key))
              :custom-value (map-nested-elt
                             elicitation (list :values (map-elt field :custom-key)))
              :elicitation elicitation))
           (agent-shell-elicitation--answerable (map-elt elicitation :fields))))

(defun agent-shell-elicitation--mark-form-start (text)
  "Return TEXT with its first character marked as a form's landing point.

`agent-shell-elicitation-jump-to-latest-form' looks for this mark, so a
form arriving puts point on the first line that says something -- the
question when the agent sent one, otherwise the first field's heading --
rather than on the box border above it.  A screen reader then reads the
question on arrival, and the whole form lies forward of point.

Marked here rather than found by searching the rendered text, so it
cannot drift out of step with the layout."
  (when text
    (let ((marked (copy-sequence text)))
      (put-text-property 0 1 'agent-shell-elicitation-form-start t marked)
      marked)))

(defun agent-shell-elicitation--make-text (elicitation)
  "Create text to render the form for ELICITATION.

ELICITATION is the entry `agent-shell--state' holds under
`:elicitations'.  Pure: every interaction mutates that entry and calls
this again, and the fresh body is pushed through
`agent-shell--update-fragment'.

For example:

   ╭─

       ? Agent Question ?

       Which approach should I take?

       Approach
       (*) Refactor first
       ( ) Add first

        Submit   Decline

   ╰─"
  (let* ((question (when-let* ((message (map-elt elicitation :message)))
                     (propertize (concat agent-shell-elicitation--indent message)
                                 'font-lock-face 'agent-shell-input)))
         (sections (if (eq (map-elt elicitation :status) 'pending)
                       (append (agent-shell-elicitation--make-field-texts elicitation)
                               (list (agent-shell-elicitation--make-buttons elicitation)))
                     (list (agent-shell-elicitation--make-settled-text elicitation))))
         (body (delq nil (cons question sections))))
    (format "╭─

    %s %s %s

%s

╰─"
            (propertize "?" 'font-lock-face 'agent-shell-warning)
            (propertize "Agent Question" 'font-lock-face 'agent-shell-permission-title)
            (propertize "?" 'font-lock-face 'agent-shell-warning)
            (string-join (cons (agent-shell-elicitation--mark-form-start (car body))
                               (cdr body))
                         "\n\n"))))


;;; Requests

(cl-defun agent-shell-elicitation--on-create-request (&key state acp-request)
  "Handle an incoming \"elicitation/create\" ACP-REQUEST with STATE.

Parses the requested schema into fields, stores them under the request
id and renders the form.  A mode we never advertised is answered with
an invalid-params error rather than rendered, since responding to it
would claim a capability we do not have."
  (unless (member (map-nested-elt acp-request '(params mode))
                  agent-shell-elicitation--advertised-modes)
    (acp-send-response
     :client (map-elt state :client)
     :response `((:request-id . ,(map-elt acp-request 'id))
                 (:error . ,(acp-make-error
                             :code -32602
                             :message (format "Unsupported elicitation mode: %s"
                                              (map-nested-elt acp-request '(params mode)))))))
    (cl-return-from agent-shell-elicitation--on-create-request))
  (let ((request-id (map-elt acp-request 'id))
        (tool-call-id (map-nested-elt acp-request '(params toolCallId)))
        (question (map-nested-elt acp-request '(params message)))
        (fields (agent-shell-elicitation--parse-schema
                 :requested-schema (map-nested-elt acp-request '(params requestedSchema)))))
    (agent-shell-elicitation--put
     state request-id
     (list (cons :id request-id)
           (cons :tool-call-id tool-call-id)
           ;; The adapter emits the `tool_call' before the elicitation
           ;; referencing it, so a question already showing as that
           ;; call's title would otherwise be rendered twice.
           (cons :message (unless (and tool-call-id
                                       (equal question
                                              (map-nested-elt
                                               state (list :tool-calls tool-call-id :title))))
                            question))
           (cons :fields fields)
           (cons :values (agent-shell-elicitation--initial-values fields))
           (cons :status 'pending)))
    ;; Track as active so out-of-turn gating treats the form as in-turn
    ;; content, the way an in-flight `session/prompt' does.
    (agent-shell-elicitation--ensure-state state)
    (map-put! state :active-requests
              (cons (list (cons :method "elicitation/create")
                          (cons :id request-id))
                    (map-elt state :active-requests)))
    (agent-shell-elicitation--render :state state :id request-id)
    (agent-shell-elicitation--jump-in-all-buffers :state state)
    ;; A form left unanswered blocks the turn exactly as an unanswered
    ;; permission does, so arm the same idle notification the permission
    ;; path arms.  Cancelled in `agent-shell-elicitation--respond'.
    (let ((data (list (cons :request-id request-id)
                      (cons :tool-call-id tool-call-id)
                      (cons :message question))))
      (agent-shell--emit-event :event 'elicitation-request :data data)
      (agent-shell--start-idle-timer :event 'elicitation-request :data data))
    (map-put! state :last-entry-type "elicitation/create")))

(cl-defun agent-shell-elicitation--render (&key state id)
  "Re-render elicitation ID's form from STATE.

The only supported re-render path: the UI layer keeps no content model,
so every interaction mutates state and pushes a freshly built body."
  (when-let* ((elicitation (agent-shell-elicitation--get state id)))
    (agent-shell--update-fragment
     :state state
     :block-id (format "elicitation-%s" id)
     :body (with-current-buffer (map-elt state :buffer)
             (agent-shell-elicitation--make-text elicitation))
     :expanded t
     :navigation 'never
     :above-last-prompt (not (agent-shell--active-requests-p state)))))

(cl-defun agent-shell-elicitation--settle (&key state id)
  "Leave elicitation ID's answer in STATE on screen, as form or tool call.

A questionnaire bridged from an ask-the-user tool lives on in the tool
call that raised it, whose `content\=' spells the answers out in prose.
The form is removed there, so the answer reads the same whether it was
just given or replayed from a restored session -- the same tool call
rendering either way, since restore never brings the form back.

Anything else -- an elicitation from an MCP server, or one scoped to a
request rather than a session -- has no tool call behind it, so the
settled form stays as the only record of what was asked."
  (if (agent-shell-elicitation--tool-call-records-p :state state :id id)
      (agent-shell--delete-fragment
       :state state :block-id (format "elicitation-%s" id))
    (agent-shell-elicitation--render :state state :id id)))

(cl-defun agent-shell-elicitation--tool-call-records-p (&key state id)
  "Return non-nil when elicitation ID's tool call in STATE stands in for its form."
  (when-let* ((tool-call-id (map-nested-elt state (list :elicitations id :tool-call-id))))
    (agent-shell-elicitation--questionnaire-p
     (map-nested-elt state (list :tool-calls tool-call-id :raw-input)))))

(cl-defun agent-shell-elicitation--jump-in-all-buffers (&key state)
  "Reveal STATE's latest form in the shell buffer and its viewport."
  (with-current-buffer (map-elt state :buffer)
    (agent-shell-elicitation-jump-to-latest-form))
  (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                               :shell-buffer (map-elt state :buffer)
                               :existing-only t)))
    (with-current-buffer viewport-buffer
      (agent-shell-elicitation-jump-to-latest-form))))

(cl-defun agent-shell-elicitation--respond (&key state id action)
  "Answer elicitation ID in STATE with ACTION and settle its form.

ACTION is one of `accept', `decline' or `cancel'.  Answering an
elicitation that already settled is a no-op, so a racing interrupt
cannot respond to one request twice."
  (when-let* ((elicitation (agent-shell-elicitation--get state id))
              ((eq (map-elt elicitation :status) 'pending)))
    (acp-send-response
     :client (map-elt state :client)
     :response (agent-shell-elicitation--make-response
                :request-id id
                :action action
                :fields (map-elt elicitation :fields)
                :values (map-elt elicitation :values)))
    (agent-shell-elicitation--put
     state id
     (agent-shell-elicitation--assoc-put
      elicitation :status (pcase action
                            ('accept 'answered)
                            ('decline 'declined)
                            (_ 'cancelled))))
    (map-put! state :active-requests
              (seq-remove (lambda (request)
                            (and (equal (map-elt request :method) "elicitation/create")
                                 (equal (map-elt request :id) id)))
                          (map-elt state :active-requests)))
    (agent-shell-elicitation--settle :state state :id id)
    ;; Both of these read buffer-local state (the transcript path, the
    ;; event subscriptions), and answering can be driven from the
    ;; viewport buffer, so resolve them against the shell.
    (with-current-buffer (map-elt state :buffer)
      (agent-shell--cancel-idle-timer)
      (agent-shell--append-transcript
       :text (format "\n## Question (%s)\n\n%s\n"
                     (format-time-string "%F %T")
                     (substring-no-properties
                      (agent-shell-elicitation--make-settled-text
                       (agent-shell-elicitation--get state id))))
       :file-path agent-shell--transcript-file)
      (agent-shell--emit-event
       :event 'elicitation-response
       :data (list (cons :request-id id)
                   (cons :action action)
                   (cons :values (map-elt elicitation :values)))))
    t))

(cl-defun agent-shell-elicitation--cancel-pending (&key state)
  "Cancel every pending elicitation in STATE.

Called from `agent-shell-interrupt': without it, interrupting a shell
that is sitting on a form leaves the agent waiting for an answer that
is never coming."
  (seq-reduce (lambda (cancelled entry)
                ;; `or' with the answer first, so every form is visited
                ;; rather than stopping at the first one cancelled.
                (or (agent-shell-elicitation--respond
                     :state state :id (car entry) :action 'cancel)
                    cancelled))
              ;; Copied, since responding rewrites `:elicitations'.
              (copy-sequence (map-elt state :elicitations))
              nil))


;;; Commands

(defun agent-shell-elicitation--control-at-point ()
  "Return the elicitation control at point, or nil when there is none."
  (get-text-property (point) 'agent-shell-elicitation-control))

(defun agent-shell-elicitation--goto-control (control)
  "Move point back to CONTROL, when it is still rendered in this buffer.

Every interaction replaces the whole form body, so without this point
would land wherever the replacement left it rather than on the option
just toggled.  CONTROL carries identity only -- which elicitation,
field and option -- so it survives the value change that triggered the
re-render and still matches.

Returns the position moved to, or nil when CONTROL is gone (a settled
form has no controls left)."
  (when-let* ((found (save-mark-and-excursion
                       (goto-char (point-min))
                       (when-let* ((match (text-property-search-forward
                                           'agent-shell-elicitation-control
                                           control #'equal)))
                         (prop-match-beginning match)))))
    (goto-char found)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-point window found))
    found))

(defun agent-shell-elicitation--read-string (prompt initial)
  "Read a string with PROMPT, prefilled with INITIAL and point at its end.

Prefilled by inserting rather than through `read-string\='s INITIAL-INPUT
argument, which is deprecated.  Follows
`agent-shell--prompt-queue-read\=' for the shape."
  (minibuffer-with-setup-hook
      (lambda ()
        (when initial
          (insert initial)))
    (read-string prompt)))

(cl-defun agent-shell-elicitation--read-value (&key field value)
  "Read a new value for FIELD from the minibuffer, starting at VALUE.

A number or integer field loops until the input parses, so bad input is
rejected while it can still be corrected rather than at submit time.
An integer field rejects a fractional value rather than truncating it:
answering 1.9 with 1 would report something the user did not type.
Each retry is prefilled with the text that was rejected, so the
correction starts from what was typed.  Returns nil when the field is
left empty, which omits it from the response.

Quitting the minibuffer leaves the field alone: this runs as an argument
to the call that stores the value, so nothing is stored."
  (let ((prompt (format "%s: " (agent-shell-elicitation--field-heading field)))
        (initial (when value (agent-shell-elicitation--render-value field value))))
    (pcase (map-elt field :type)
      ((or 'number 'integer)
       (let ((wholep (eq (map-elt field :type) 'integer))
             (input nil)
             (complaint ""))
         (while (not input)
           (let ((read (string-trim (agent-shell-elicitation--read-string
                                     (concat complaint prompt) initial))))
             (cond ((string-empty-p read) (setq input 'empty))
                   ((string-match-p (if wholep
                                        (rx bos (opt (any "+-")) (+ digit) eos)
                                      (rx bos (opt (any "+-"))
                                          (or (seq (+ digit) (opt "." (* digit)))
                                              (seq "." (+ digit)))
                                          eos))
                                    read)
                    (setq input (string-to-number read)))
                   ;; Complain in the prompt, which the re-opened minibuffer
                   ;; would otherwise cover, and hand back the rejected text:
                   ;; prefilling the original would prepend it to the retry.
                   (t (setq complaint (if wholep
                                          "Whole number needed.  "
                                        "Not a number.  "))
                      (setq initial read)))))
         (unless (eq input 'empty) input)))
      (_ (let ((read (agent-shell-elicitation--read-string prompt initial)))
           (unless (string-empty-p read) read))))))

(defun agent-shell-elicitation--set-values (state id pairs)
  "Store PAIRS against elicitation ID in STATE, then re-render once.

PAIRS is an alist of field key to value.  A nil value removes the
field, so it is omitted from the response.  Applied together because
picking an option and typing your own answer replace one another: doing
that in two steps would render an in-between form showing both."
  (when-let* ((elicitation (agent-shell-elicitation--get state id)))
    (agent-shell-elicitation--put
     state id
     (agent-shell-elicitation--assoc-put
      elicitation :values
      (seq-reduce (lambda (values pair)
                    (if (cdr pair)
                        (agent-shell-elicitation--assoc-put values (car pair) (cdr pair))
                      (map-delete (copy-alist values) (car pair))))
                  pairs
                  (map-elt elicitation :values))))
    (agent-shell-elicitation--render :state state :id id)))

(defun agent-shell-elicitation--set-value (state id key value)
  "Set field KEY of elicitation ID in STATE to VALUE and re-render.
A nil VALUE removes the field, so it is omitted from the response."
  (agent-shell-elicitation--set-values state id (list (cons key value))))

(defun agent-shell-elicitation--multi-answer-p (elicitation field)
  "Return non-nil when FIELD is part of a multi-select question in ELICITATION.
True for the select itself and for the free-text companion answering it."
  (eq 'multi-select
      (map-elt (or (agent-shell-elicitation--field
                    elicitation (map-elt field :folded-into))
                   field)
               :type)))

(defun agent-shell-elicitation--clearing (elicitation key value)
  "Return ELICITATION field/value pairs binding KEY to VALUE.

Also clears whatever KEY replaces, for a single-select: picking an
option drops the answer typed in its place, and typing an answer drops
the option picked in its place, so exactly one of the two is ever sent.
A multi-select clears nothing, since its ticks and its typed answer are
one answer together.  Feed the result to
`agent-shell-elicitation--set-values\='.

For example, given a select `question_0\=' whose free-text companion is
`question_0_custom\=':

  (agent-shell-elicitation--clearing elicitation \"question_0\" \"Red\")
  => ((\"question_0\" . \"Red\") (\"question_0_custom\"))"
  (let ((field (agent-shell-elicitation--field elicitation key)))
    (cons (cons key value)
          ;; A multi-select and its typed answer are not alternatives:
          ;; ticking boxes and adding one of your own is one answer, and
          ;; they are combined on the way out rather than clearing here.
          (unless (agent-shell-elicitation--multi-answer-p elicitation field)
            (seq-keep (lambda (other) (when other (cons other nil)))
                      (list (map-elt field :custom-key)
                            (map-elt field :folded-into)))))))

(defun agent-shell-elicitation--option-preview-at (elicitation key value)
  "Return the preview KEY's VALUE option carries in ELICITATION, or nil.

Reads the parsed option rather than the raw schema, so it answers the
same question the renderer asked when it decided whether to draw a
disclosure glyph."
  (map-elt (seq-find (lambda (option)
                       (equal (map-elt option :value) value))
                     (map-elt (agent-shell-elicitation--field elicitation key) :options))
           :preview))

(cl-defun agent-shell-elicitation--toggle-preview (&key state id key value)
  "Open or close the preview of KEY's VALUE option of elicitation ID in STATE."
  (when-let* ((elicitation (agent-shell-elicitation--get state id))
              (preview (cons key value)))
    (agent-shell-elicitation--put
     state id
     (agent-shell-elicitation--assoc-put
      elicitation :previews
      (if (member preview (map-elt elicitation :previews))
          (remove preview (map-elt elicitation :previews))
        (cons preview (map-elt elicitation :previews)))))
    (agent-shell-elicitation--render :state state :id id)))

(defun agent-shell-elicitation-toggle-preview ()
  "Open or close the preview of the option at point.

Reached through `agent-shell-elicitation-preview-map', which covers the
option itself as well as its disclosure glyph, so an answer offering a
preview can be opened without first moving onto the glyph.
`end-of-line' lands after the glyph rather than on it, and RET past the
end of a line keeps its usual meaning.

That map is applied only where there is a preview, so a control without
one never binds the key.  Reaching this command anyway -- through
\\[execute-extended-command], or a binding of your own -- signals a
`user-error' rather than redrawing an identical form."
  (declare (modes agent-shell-mode))
  (interactive)
  (let ((control (agent-shell-elicitation--control-at-point))
        (acted-in (current-buffer)))
    (unless control
      (user-error "No question control at point"))
    (with-current-buffer (agent-shell-elicitation--shell-buffer)
      (let ((state (agent-shell--state)))
        (unless (agent-shell-elicitation--option-preview-at
                 (agent-shell-elicitation--get state (map-elt control :id))
                 (map-elt control :key) (map-elt control :value))
          (user-error "No preview for this answer"))
        (agent-shell-elicitation--toggle-preview
         :state state
         :id (map-elt control :id)
         :key (map-elt control :key)
         :value (map-elt control :value))))
    (with-current-buffer acted-in
      (agent-shell-elicitation--goto-control control))))

(defun agent-shell-elicitation-act ()
  "Act on the elicitation control at point.

Reads which elicitation, field and option the control names from its
text properties, then mutates the authoritative entry in
`agent-shell--state' and re-renders.  Named rather than a per-control
closure so it stays rebindable (see issue #759)."
  (declare (modes agent-shell-mode))
  (interactive)
  (let ((control (agent-shell-elicitation--control-at-point))
        (shell-buffer (agent-shell-elicitation--shell-buffer))
        (acted-in (current-buffer)))
    (unless control
      (user-error "No question control at point"))
    (with-current-buffer shell-buffer
      (let* ((state (agent-shell--state))
             (id (map-elt control :id))
             (elicitation (agent-shell-elicitation--get state id)))
        (unless (and elicitation (eq (map-elt elicitation :status) 'pending))
          (user-error "This question is no longer awaiting an answer"))
        (pcase (map-elt control :action)
          ('select
           (agent-shell-elicitation--set-values
            state id (agent-shell-elicitation--clearing
                      elicitation (map-elt control :key) (map-elt control :value))))
          ('toggle
           (let ((current (map-nested-elt elicitation
                                          (list :values (map-elt control :key)))))
             (agent-shell-elicitation--set-values
              state id
              (agent-shell-elicitation--clearing
               elicitation (map-elt control :key)
               (if (map-elt control :value)
                   (if (member (map-elt control :value) current)
                       (remove (map-elt control :value) current)
                     (append current (list (map-elt control :value))))
                 (if (eq current t) :false t))))))
          ;; A free-text field and a free-text answer to a select are the
          ;; same minibuffer read; only what it clears differs.
          ((or 'read 'custom)
           (agent-shell-elicitation--set-values
            state id
            (agent-shell-elicitation--clearing
             elicitation (map-elt control :key)
             (agent-shell-elicitation--read-value
              :field (agent-shell-elicitation--field elicitation (map-elt control :key))
              :value (map-nested-elt elicitation
                                     (list :values (map-elt control :key)))))))
          ('preview (agent-shell-elicitation--toggle-preview
                     :state state :id id :key (map-elt control :key)
                     :value (map-elt control :value)))
          ('submit (agent-shell-elicitation--submit :state state :id id))
          ('decline
           (agent-shell-elicitation--respond :state state :id id :action 'decline)
           (message "Declined")))))
    (with-current-buffer acted-in
      (agent-shell-elicitation--goto-control control))))

(cl-defun agent-shell-elicitation--submit (&key state id)
  "Submit elicitation ID in STATE, or say why it cannot be submitted."
  (if-let* ((blocker (agent-shell-elicitation--blocker
                      (agent-shell-elicitation--get state id))))
      (user-error "Cannot submit: %s" (cdr blocker))
    (agent-shell-elicitation--respond :state state :id id :action 'accept)
    (message "Answer sent")))

(defun agent-shell-elicitation-interrupt ()
  "Interrupt the shell backing the form at point.

Bound on the form's own controls so the usual interrupt key still works
there, where `agent-shell-mode-map' is shadowed by the form's keymap."
  (declare (modes agent-shell-mode))
  (interactive)
  (with-current-buffer (agent-shell-elicitation--shell-buffer)
    (agent-shell-interrupt t)))


;;; Navigation

(defun agent-shell-elicitation-next-field ()
  "Jump to the next elicitation form control."
  (declare (modes agent-shell-mode))
  (interactive)
  (when-let* ((found (save-mark-and-excursion
                       (when (get-text-property (point) 'agent-shell-elicitation-navigable)
                         (when-let* ((next-change (next-single-property-change
                                                   (point) 'agent-shell-elicitation-navigable)))
                           (goto-char next-change)))
                       (when-let* ((next (text-property-search-forward
                                          'agent-shell-elicitation-navigable t t)))
                         (prop-match-beginning next)))))
    (deactivate-mark)
    (goto-char found)
    found))

(defun agent-shell-elicitation-previous-field ()
  "Jump to the previous elicitation form control."
  (declare (modes agent-shell-mode))
  (interactive)
  (when-let* ((found (save-mark-and-excursion
                       (when (get-text-property (point) 'agent-shell-elicitation-navigable)
                         (when-let* ((prev-change (previous-single-property-change
                                                   (point) 'agent-shell-elicitation-navigable)))
                           (goto-char prev-change)))
                       (when-let* ((prev (text-property-search-backward
                                          'agent-shell-elicitation-navigable t t)))
                         (prop-match-beginning prev)))))
    (deactivate-mark)
    (goto-char found)
    found))

(defun agent-shell-elicitation-jump-to-latest-form ()
  "Jump to the top of the latest elicitation form.

Point lands on the first line of the form that says something -- the
question when the agent sent one, otherwise the first field's heading --
so a screen reader reads the question on arrival and the whole form,
every option and both buttons, lies forward of point.  Landing on a
control instead would leave the question above point, where TAB does not
reach it: only controls are navigable, so the question text and field
headings are not stops in either direction.

Syncs that position into every window showing the buffer, across
frames, so the form is revealed even when the shell window is not the
selected one.  A bare `goto-char' would only move point in the selected
window.

Returns non-nil if a form was found, nil otherwise."
  (declare (modes agent-shell-mode))
  (interactive)
  (when-let* ((found (save-mark-and-excursion
                       (goto-char (point-max))
                       (when-let* ((match (text-property-search-backward
                                           'agent-shell-elicitation-form-start t t)))
                         (prop-match-beginning match)))))
    (deactivate-mark)
    (goto-char found)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-point window found))
    t))

(provide 'agent-shell-elicitation)

;;; agent-shell-elicitation.el ends here
