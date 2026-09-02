;;; agent-shell-elicitation-tests.el --- Tests for agent-shell-elicitation -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for ACP `elicitation/create' handling: schema parsing, response
;; encoding, request handling and keymap scoping.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
;; `agent-shell-elicitation' only declares what it borrows from these.
(require 'agent-shell)
(require 'agent-shell-elicitation)

(defun agent-shell-elicitation-tests--state ()
  "Return a plain state alist standing in for a shell's."
  (list (cons :buffer (current-buffer))
        (cons :client 'test-client)
        (cons :tool-calls nil)
        (cons :elicitations nil)
        (cons :active-requests nil)
        (cons :event-subscriptions nil)
        (cons :idle-timer nil)
        (cons :last-entry-type nil)))

(cl-defun agent-shell-elicitation-tests--request (&key (id 1) (mode "form") tool-call-id
                                                       (message "Pick one") schema)
  "Return an `elicitation/create' request as it arrives off the wire.

ID, MODE, TOOL-CALL-ID, MESSAGE and SCHEMA fill the request in.  Note
repeated JSON fields are vectors, matching how acp.el decodes them."
  `((id . ,id)
    (method . "elicitation/create")
    (params . ((mode . ,mode)
               (sessionId . "session-1")
               ,@(when tool-call-id `((toolCallId . ,tool-call-id)))
               (message . ,message)
               (requestedSchema . ,schema)))))

(cl-defmacro agent-shell-elicitation-tests--with-shell ((state-var sent-var &optional bodies-var)
                                                        &rest body)
  "Run BODY with a stubbed shell, binding STATE-VAR, SENT-VAR and BODIES-VAR.

STATE-VAR holds the fake state, SENT-VAR collects every response handed
to `acp-send-response', and BODIES-VAR collects every fragment body
pushed, newest first."
  (declare (indent 1) (debug t))
  (let ((bodies (or bodies-var (gensym "bodies"))))
    `(with-temp-buffer
       (let ((,state-var (agent-shell-elicitation-tests--state))
             (,sent-var nil)
             (,bodies nil))
         (cl-letf (((symbol-function 'agent-shell--state)
                    (lambda () ,state-var))
                   ((symbol-function 'agent-shell--update-fragment)
                    (lambda (&rest args) (push (plist-get args :body) ,bodies)))
                   ((symbol-function 'agent-shell-viewport--buffer)
                    (lambda (&rest _) nil))
                   ((symbol-function 'agent-shell--append-transcript)
                    (lambda (&rest _)))
                   ((symbol-function 'acp-send-response)
                    (lambda (&rest args) (push (plist-get args :response) ,sent-var))))
           ,@body)))))


;;; Schema parsing

(ert-deftest agent-shell-elicitation-parse-schema-covers-every-variant-test ()
  "Every `ElicitationPropertySchema' variant parses, in both spellings.

Advertising `elicitation.form' promises to render any form the schema
permits, so a variant that parsed to nothing would leave a question the
user cannot answer."
  (dolist (case '((((type . "string")
                    (oneOf . [((const . "a") (title . "A") (description . "First"))]))
                   . (single-select (("a" "A" "First"))))
                  (((type . "string") (enum . ["a" "b"]))
                   . (single-select (("a" "a" nil) ("b" "b" nil))))
                  (((type . "array") (items . ((anyOf . [((const . "a") (title . "A"))]))))
                   . (multi-select (("a" "A" nil))))
                  (((type . "array") (items . ((enum . ["a"]))))
                   . (multi-select (("a" "a" nil))))
                  (((type . "string")) . (string nil))
                  (((type . "number")) . (number nil))
                  (((type . "integer")) . (integer nil))
                  (((type . "boolean")) . (boolean nil))
                  (((type . "gizmo")) . (unsupported nil))))
    (let ((field (seq-first (agent-shell-elicitation--parse-schema
                             :requested-schema `((type . "object")
                                                 (properties . ((f . ,(car case)))))))))
      (should (equal (map-elt field :type) (nth 0 (cdr case))))
      (should (equal (map-elt field :key) "f"))
      (should (equal (seq-map (lambda (option)
                                (list (map-elt option :value)
                                      (map-elt option :title)
                                      (map-elt option :description)))
                              (map-elt field :options))
                     (nth 1 (cdr case)))))))

(ert-deftest agent-shell-elicitation-parse-schema-keeps-property-order-test ()
  "Fields render in the order the agent sent them."
  (should (equal (seq-map (lambda (field) (map-elt field :key))
                          (agent-shell-elicitation--parse-schema
                           :requested-schema '((type . "object")
                                               (properties . ((zebra . ((type . "string")))
                                                              (apple . ((type . "string")))
                                                              (mango . ((type . "string"))))))))
                 '("zebra" "apple" "mango"))))

(ert-deftest agent-shell-elicitation-parse-schema-marks-required-test ()
  "`required' names which fields must carry a value, and arrives as a vector."
  (let ((fields (agent-shell-elicitation--parse-schema
                 :requested-schema '((type . "object")
                                     (properties . ((a . ((type . "string")))
                                                    (b . ((type . "string")))))
                                     (required . ["b"])))))
    (should-not (map-elt (seq-first fields) :required))
    (should (map-elt (seq-elt fields 1) :required))))

(ert-deftest agent-shell-elicitation-parse-schema-seeds-defaults-test ()
  "Defaults seed the starting values, with array defaults arriving as vectors."
  (let ((fields (agent-shell-elicitation--parse-schema
                 :requested-schema '((type . "object")
                                     (properties
                                      . ((pick . ((type . "string") (enum . ["a" "b"])
                                                  (default . "b")))
                                         (tags . ((type . "array") (items . ((enum . ["a" "b"])))
                                                  (default . ["a" "b"])))
                                         (n . ((type . "integer") (default . 3)))
                                         (flag . ((type . "boolean") (default . t)))
                                         (note . ((type . "string")))))))))
    (should (equal (map-elt fields "pick") nil))
    (let ((values (agent-shell-elicitation--initial-values fields)))
      (should (equal (map-elt values "pick") "b"))
      (should (equal (map-elt values "tags") '("a" "b")))
      (should (equal (map-elt values "n") 3))
      (should (equal (map-elt values "flag") t))
      ;; No default and not a checkbox, so it starts absent and is
      ;; omitted from the response rather than sent as null.
      (should-not (assoc "note" values)))))

(ert-deftest agent-shell-elicitation-checkbox-answers-only-when-it-means-something-test ()
  "A checkbox answers when ticked, when required, or when it has a default.

Otherwise it is left out: an unticked optional box is indistinguishable
on screen from one the user never reached.  Required is the exception,
since omitting it would leave a box that can neither be submitted nor
answered in the negative."
  (dolist (case '((;; optional, no default: silent either way
                   nil nil ((nil . nil) (("flag" . t) . ((flag . t)))
                            (("flag" . :false) . nil)))
                  (;; required: answers, including `no\=', and never blocks
                   t nil ((nil . ((flag . :false)))
                          (("flag" . t) . ((flag . t)))))
                  (;; defaulted: confirming or overturning both count
                   nil t ((nil . ((flag . t)))
                          (("flag" . :false) . ((flag . :false)))))))
    (let* ((required (nth 0 case))
           (default (nth 1 case))
           (fields (agent-shell-elicitation--parse-schema
                    :requested-schema
                    `((type . "object")
                      (properties . ((flag . ((type . "boolean")
                                              ,@(when default '((default . t)))))))
                      ,@(when required '((required . ["flag"]))))))
           (seeded (agent-shell-elicitation--initial-values fields)))
      ;; A required checkbox must never be what stops a form submitting.
      (should-not (agent-shell-elicitation--blocker
                   (list (cons :fields fields) (cons :values seeded))))
      (dolist (expectation (nth 2 case))
        (should (equal (agent-shell-elicitation--make-content
                        :fields fields
                        :values (if (car expectation)
                                    (list (car expectation))
                                  seeded))
                       (cdr expectation)))))))

;;; Free-text companions

(defconst agent-shell-elicitation-tests--ask-schema
  '((type . "object")
    (properties
     (question_0 (type . "string") (title . "Colour")
                 (oneOf . [((const . "Red") (title . "Red"))
                           ((const . "Blue") (title . "Blue"))]))
     (question_0_custom
      (type . "string") (title . "Other")
      (description . "Type your own answer instead of choosing an option above (optional).")
      (_meta (_askUserQuestionCustomAnswer
              (questionId . "question_0") (isCustomAnswer . t))))))
  "The schema Claude's `AskUserQuestion' bridge really sends, captured live.")

(ert-deftest agent-shell-elicitation-folds-custom-answer-into-its-select-test ()
  "A free-text companion renders as one more option of the question it answers.

Left as a field of its own, it renders as a second question and can be
answered alongside the first in contradictory ways."
  (let ((fields (agent-shell-elicitation--parse-schema
                 :requested-schema agent-shell-elicitation-tests--ask-schema)))
    (should (equal (map-elt (seq-first fields) :custom-key) "question_0_custom"))
    (should (equal (map-elt (seq-elt fields 1) :folded-into) "question_0"))
    (should-not (map-elt (seq-first fields) :folded-into)))
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5 :schema agent-shell-elicitation-tests--ask-schema))
    (let ((body (substring-no-properties (car bodies))))
      ;; One question, one list, with the free-text answer among the options.
      (should (string-match-p "Colour" body))
      (should (string-match-p "( ) Red" body))
      (should (string-match-p "( ) Other" body))
      ;; Not also standing alone as a `Other: (empty)' text field.
      (should-not (string-match-p "Other: (empty)" body)))))

(ert-deftest agent-shell-elicitation-option-and-custom-answer-replace-each-other-test ()
  "Picking an option drops a typed answer, and typing one drops the pick.

They are alternatives, so exactly one of them is ever sent."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5 :schema agent-shell-elicitation-tests--ask-schema))
    (cl-flet ((set-to (key value)
                (agent-shell-elicitation--set-values
                 state 5 (agent-shell-elicitation--clearing
                          (agent-shell-elicitation--get state 5) key value))))
      (set-to "question_0" "Red")
      (should (equal (map-elt (agent-shell-elicitation--get state 5) :values)
                     '(("question_0" . "Red"))))
      (should (string-match-p "(\\*) Red" (substring-no-properties (car bodies))))
      (set-to "question_0_custom" "teal, actually")
      (should (equal (map-elt (agent-shell-elicitation--get state 5) :values)
                     '(("question_0_custom" . "teal, actually"))))
      (let ((body (substring-no-properties (car bodies))))
        (should (string-match-p "( ) Red" body))
        (should (string-match-p "(\\*) Other: teal, actually" body)))
      (set-to "question_0" "Blue")
      (should (equal (map-elt (agent-shell-elicitation--get state 5) :values)
                     '(("question_0" . "Blue")))))))

(defconst agent-shell-elicitation-tests--multi-ask-schema
  '((type . "object")
    (properties
     (question_0 (type . "array") (title . "Colours")
                 (items . ((anyOf . [((const . "Red") (title . "Red"))
                                     ((const . "Blue") (title . "Blue"))
                                     ((const . "Green") (title . "Green"))]))))
     (question_0_custom
      (type . "string") (title . "Other")
      (description . "Type your own answer instead of choosing an option above (optional).")
      (_meta (_askUserQuestionCustomAnswer
              (questionId . "question_0") (isCustomAnswer . t))))))
  "The multi-select shape Claude's `AskUserQuestion' bridge sends, captured live.")

(ert-deftest agent-shell-elicitation-multi-select-keeps-ticks-and-typed-answer-test ()
  "Ticking boxes and typing your own answer coexist on a multi-select.

Unlike a single-select, where the two are alternatives, here they are
one answer: the user picked these and also wants to add that.  The
companion folds in as one more checkbox rather than a field of its own."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5 :schema agent-shell-elicitation-tests--multi-ask-schema))
    (let ((body (substring-no-properties (car bodies))))
      (should (string-match-p "\\[ \\] Red" body))
      (should (string-match-p "\\[ \\] Green" body))
      ;; A checkbox, not a radio button, and not standing alone.
      (should (string-match-p "\\[ \\] Other" body))
      (should-not (string-match-p "Other: (empty)" body)))
    (cl-flet ((set-to (key value)
                (agent-shell-elicitation--set-values
                 state 5 (agent-shell-elicitation--clearing
                          (agent-shell-elicitation--get state 5) key value))))
      (set-to "question_0" '("Red" "Blue"))
      (set-to "question_0_custom" "teal"))
    ;; Neither cleared the other.
    (should (equal (map-nested-elt (agent-shell-elicitation--get state 5)
                                   '(:values "question_0"))
                   '("Red" "Blue")))
    (should (equal (map-nested-elt (agent-shell-elicitation--get state 5)
                                   '(:values "question_0_custom"))
                   "teal"))
    (let ((body (substring-no-properties (car bodies))))
      (should (string-match-p "\\[x\\] Red" body))
      (should (string-match-p "\\[x\\] Blue" body))
      (should (string-match-p "\\[x\\] Other: teal" body)))))

(ert-deftest agent-shell-elicitation-multi-select-wire-encoding-test ()
  "Ticks alone travel as an array; ticks plus typed text as one string.

Sent as two fields the ticks are lost: an agent bridging its
ask-the-user tool onto elicitation reads the companion first and
returns early.  Verified against the Claude adapter, which reported back
only the typed word when both were sent."
  (dolist (case '(((("question_0" . ("Red" "Green")))
                   . ((question_0 . ["Red" "Green"])))
                  ((("question_0" . ("Red" "Blue")) ("question_0_custom" . "teal"))
                   . ((question_0_custom . "Red, Blue, teal")))))
    (agent-shell-elicitation-tests--with-shell (state sent bodies)
      (agent-shell--on-request
       :state state
       :acp-request (agent-shell-elicitation-tests--request
                     :id 5 :schema agent-shell-elicitation-tests--multi-ask-schema))
      (dolist (pair (car case))
        (agent-shell-elicitation--set-value state 5 (car pair) (cdr pair)))
      (agent-shell-elicitation--submit :state state :id 5)
      (should (equal (map-nested-elt (seq-first sent) '(:result content)) (cdr case)))
      ;; The settled form says exactly what went out, on one line.
      (should (= 1 (seq-count (lambda (line) (string-match-p "Colours:" line))
                              (split-string (substring-no-properties (car bodies)) "\n")))))))

(ert-deftest agent-shell-elicitation-sends-custom-answer-alone-test ()
  "A typed answer goes out on its own, with no option alongside it.

The agent reads the companion first and returns early, so the option it
replaced must be absent rather than merely ignored."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5 :schema agent-shell-elicitation-tests--ask-schema))
    (agent-shell-elicitation--set-values
     state 5 (agent-shell-elicitation--clearing
              (agent-shell-elicitation--get state 5) "question_0_custom" "teal"))
    (agent-shell-elicitation--submit :state state :id 5)
    (should (equal (seq-first sent)
                   '((:request-id . 5)
                     (:result . ((action . "accept")
                                 (content . ((question_0_custom . "teal"))))))))
    ;; Settled, the answer reads under the question it answered, not "Other".
    (should (string-match-p "Colour: teal" (substring-no-properties (car bodies))))))

(ert-deftest agent-shell-elicitation-custom-answer-satisfies-required-select-test ()
  "Typing your own answer answers a required question.

The select itself stays empty, so a naive presence check would refuse to
submit a form the user has in fact filled in."
  (agent-shell-elicitation-tests--with-shell (state sent)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5
                   :schema (append agent-shell-elicitation-tests--ask-schema
                                   '((required . ["question_0"])))))
    (should-error (agent-shell-elicitation--submit :state state :id 5) :type 'user-error)
    (agent-shell-elicitation--set-values
     state 5 (agent-shell-elicitation--clearing
              (agent-shell-elicitation--get state 5) "question_0_custom" "teal"))
    (agent-shell-elicitation--submit :state state :id 5)
    (should (equal (map-nested-elt (seq-first sent) '(:result content))
                   '((question_0_custom . "teal"))))))

(ert-deftest agent-shell-elicitation-dangling-custom-answer-stays-standalone-test ()
  "A companion naming a field that is not a select renders on its own.

Folding it away would hide a field the user can still be asked to fill
in, so an unrecognised link is left alone rather than trusted."
  (dolist (schema '(;; Names a field that is not in this schema at all.
                    ((type . "object")
                     (properties
                      (stray (type . "string") (title . "Other")
                             (_meta (_askUserQuestionCustomAnswer
                                     (questionId . "nonexistent"))))))
                    ;; Names a field that exists but is not a select.
                    ((type . "object")
                     (properties
                      (plain (type . "string") (title . "Plain"))
                      (stray (type . "string") (title . "Other")
                             (_meta (_askUserQuestionCustomAnswer
                                     (questionId . "plain"))))))))
    (let ((fields (agent-shell-elicitation--parse-schema :requested-schema schema)))
      (should-not (seq-find (lambda (field) (map-elt field :folded-into)) fields))
      (should-not (seq-find (lambda (field) (map-elt field :custom-key)) fields)))))


;;; Response encoding

(ert-deftest agent-shell-elicitation-encodes-content-for-the-wire-test ()
  "Booleans, integers and multi-selects serialize the way the schema wants.

Emacs has two spellings to get wrong here: nil is JSON null rather than
false, and a plain list serializes as an object rather than an array."
  (should (equal (json-serialize
                  (agent-shell-elicitation--make-content
                   ;; `no' is required, which is what keeps an unticked
                   ;; checkbox on the wire at all.
                   :fields '(((:key . "yes") (:type . boolean))
                             ((:key . "no") (:type . boolean) (:required . t))
                             ((:key . "n") (:type . integer))
                             ((:key . "x") (:type . number))
                             ((:key . "tags") (:type . multi-select))
                             ((:key . "note") (:type . string)))
                   :values '(("yes" . t) ("no" . :false) ("n" . 1) ("x" . 1.5)
                             ("tags" . ("a" "b")) ("note" . "hi"))))
                 "{\"yes\":true,\"no\":false,\"n\":1,\"x\":1.5,\"tags\":[\"a\",\"b\"],\"note\":\"hi\"}"))
  ;; An integer field never goes out as a float, however the value arrived.
  (should (equal (json-serialize
                  (agent-shell-elicitation--make-content
                   :fields '(((:key . "n") (:type . integer)))
                   :values '(("n" . 1.0))))
                 "{\"n\":1}")))

(ert-deftest agent-shell-elicitation-omits-unanswered-fields-test ()
  "Fields with no value are left out rather than sent as null.

An unsupported field is left out too, since it was never rendered and
so was never answered."
  (should (equal (agent-shell-elicitation--make-content
                  :fields '(((:key . "a") (:type . string))
                            ((:key . "b") (:type . string))
                            ((:key . "empty") (:type . string))
                            ((:key . "none") (:type . multi-select))
                            ((:key . "gizmo") (:type . unsupported)))
                  :values '(("a" . "kept") ("empty" . "") ("none" . nil)
                            ("gizmo" . "ignored")))
                 '((a . "kept"))))
  ;; Nothing answered serializes as an empty object, not null.
  (should (equal (json-serialize
                  `((content . ,(agent-shell-elicitation--make-content
                                 :fields '(((:key . "a") (:type . string)))
                                 :values nil))))
                 "{\"content\":{}}")))

(ert-deftest agent-shell-elicitation-make-response-shapes-test ()
  "Accept carries content; decline and cancel carry only their action."
  (should (equal (agent-shell-elicitation--make-response
                  :request-id 3 :action 'accept
                  :fields '(((:key . "a") (:type . string)))
                  :values '(("a" . "x")))
                 '((:request-id . 3)
                   (:result . ((action . "accept") (content . ((a . "x"))))))))
  (should (equal (agent-shell-elicitation--make-response :request-id 3 :action 'decline)
                 '((:request-id . 3) (:result . ((action . "decline"))))))
  (should (equal (agent-shell-elicitation--make-response :request-id 3 :action 'cancel)
                 '((:request-id . 3) (:result . ((action . "cancel"))))))
  (should-error (agent-shell-elicitation--make-response :action 'cancel)))


;;; Request handling

(ert-deftest agent-shell-elicitation-renders-and-blocks-the-shell-test ()
  "A form arrives pending, renders its fields, and reports the shell blocked.

Without the blocked status the shell reports `ready' and the prompt
queue steers a prompt into a turn that is sitting on a question."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5
                   :schema '((type . "object")
                             (properties . ((pick . ((type . "string")
                                                     (title . "Pick")
                                                     (oneOf . [((const . "a") (title . "Apple"))
                                                               ((const . "b") (title . "Banana"))]))))))))
    (should-not sent)
    (should (eq (map-elt (agent-shell-elicitation--get state 5) :status) 'pending))
    (should (agent-shell-elicitation--pending-p))
    (cl-letf (((symbol-function 'shell-maker-busy) (lambda (&rest _) t)))
      (should (eq (agent-shell-status) 'blocked)))
    (let ((body (substring-no-properties (car bodies))))
      (should (string-match-p "Pick one" body))
      (should (string-match-p "( ) Apple" body))
      (should (string-match-p "( ) Banana" body))
      (should (string-match-p "Submit" body))
      (should (string-match-p "Decline" body)))
    ;; Tracked as active so out-of-turn gating treats it as in-turn content.
    (should (seq-find (lambda (request)
                        (equal (map-elt request :method) "elicitation/create"))
                      (map-elt state :active-requests)))))

(ert-deftest agent-shell-elicitation-accept-sends-the-answers-test ()
  "Selecting an option and submitting sends it, then settles the form."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5
                   :schema '((type . "object")
                             (properties . ((pick . ((type . "string")
                                                     (oneOf . [((const . "a") (title . "Apple"))]))))))))
    (agent-shell-elicitation--set-value state 5 "pick" "a")
    (should (string-match-p "(\\*) Apple" (substring-no-properties (car bodies))))
    (agent-shell-elicitation--submit :state state :id 5)
    (should (equal (seq-first sent)
                   '((:request-id . 5)
                     (:result . ((action . "accept") (content . ((pick . "a"))))))))
    (should (eq (map-elt (agent-shell-elicitation--get state 5) :status) 'answered))
    (should-not (agent-shell-elicitation--pending-p))
    ;; Settled rather than deleted: the form stays, showing what was sent.
    (should (string-match-p "Answered" (substring-no-properties (car bodies))))
    (should (string-match-p "Apple" (substring-no-properties (car bodies))))
    (should-not (map-elt state :active-requests))))

(ert-deftest agent-shell-elicitation-decline-sends-decline-test ()
  "Declining answers the request without any content."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5 :schema '((type . "object")
                                   (properties . ((note . ((type . "string"))))))))
    (agent-shell-elicitation--respond :state state :id 5 :action 'decline)
    (should (equal (seq-first sent)
                   '((:request-id . 5) (:result . ((action . "decline"))))))
    (should (string-match-p "Declined" (substring-no-properties (car bodies))))))

(ert-deftest agent-shell-elicitation-interrupt-cancels-pending-test ()
  "Interrupting cancels the form, or the agent waits on an answer forever."
  (agent-shell-elicitation-tests--with-shell (state sent)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5 :schema '((type . "object")
                                   (properties . ((note . ((type . "string"))))))))
    (agent-shell-elicitation--cancel-pending :state state)
    (should (equal (seq-first sent)
                   '((:request-id . 5) (:result . ((action . "cancel"))))))
    (should (eq (map-elt (agent-shell-elicitation--get state 5) :status) 'cancelled))
    (should-not (agent-shell-elicitation--pending-p))))

(ert-deftest agent-shell-elicitation-responds-once-per-request-test ()
  "A settled form is never answered a second time.

An interrupt racing a submit would otherwise send two responses for one
JSON-RPC id."
  (agent-shell-elicitation-tests--with-shell (state sent)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5 :schema '((type . "object")
                                   (properties . ((note . ((type . "string"))))))))
    (should (agent-shell-elicitation--respond :state state :id 5 :action 'accept))
    (should-not (agent-shell-elicitation--respond :state state :id 5 :action 'cancel))
    (should-not (agent-shell-elicitation--cancel-pending :state state))
    (should (= (length sent) 1))
    ;; The entry carries one status, not one per attempt.
    (should (= 1 (seq-count (lambda (pair) (eq (car pair) :status))
                            (agent-shell-elicitation--get state 5))))))

(ert-deftest agent-shell-elicitation-rejects-unadvertised-mode-test ()
  "A mode we never advertised is an invalid-params error, not a form.

We advertise `form' only, so `url' is one the agent should not have
sent; answering it would claim a capability we do not have."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request :id 5 :mode "url"))
    (should (equal (map-elt (seq-first sent) :request-id) 5))
    (should (equal (map-nested-elt (seq-first sent) '(:error code)) -32602))
    (should-not bodies)
    (should-not (agent-shell-elicitation--get state 5))))

(ert-deftest agent-shell-elicitation-submit-gate-test ()
  "Submit is gated by required fields, and says which way it is stuck.

A field left empty keeps Submit, which then refuses and names the field,
rather than sending a response the schema forbids.  A field whose type
this client cannot render drops Submit altogether, since no answer we
could send would satisfy the schema; Decline is then the only way out."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    ;; Merely empty: Submit stays, and refuses until the field is filled.
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5
                   :schema '((type . "object")
                             (properties . ((note . ((type . "string") (title . "Note")))))
                             (required . ["note"]))))
    (should-error (agent-shell-elicitation--submit :state state :id 5) :type 'user-error)
    (should-not sent)
    (agent-shell-elicitation--set-value state 5 "note" "filled in")
    (agent-shell-elicitation--submit :state state :id 5)
    (should (equal (map-nested-elt (seq-first sent) '(:result content))
                   '((note . "filled in"))))
    ;; Unrenderable: Submit is gone, Decline remains, and the form says why.
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 6
                   :schema '((type . "object")
                             (properties . ((gizmo . ((type . "gizmo") (title . "Gizmo")))))
                             (required . ["gizmo"]))))
    (let ((body (substring-no-properties (car bodies))))
      (should (string-match-p "Unsupported field type \"gizmo\"" body))
      (should (string-match-p "Cannot submit" body))
      (should-not (string-match-p "Submit \\]" body))
      (should (string-match-p "Decline" body)))
    (should-error (agent-shell-elicitation--submit :state state :id 6) :type 'user-error)))

(ert-deftest agent-shell-elicitation-skips-question-already-shown-test ()
  "A question already rendered as the tool call's title is not repeated.

The adapter emits the `tool_call' before the elicitation referencing it,
so the question is on screen before the form is."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (map-put! state :tool-calls '(("tc-1" . ((:title . "Which approach?")))))
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5 :tool-call-id "tc-1" :message "Which approach?"
                   :schema '((type . "object")
                             (properties . ((note . ((type . "string"))))))))
    (should-not (string-match-p "Which approach\\?" (substring-no-properties (car bodies))))
    ;; A question the tool call does not already carry still shows.
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 6 :tool-call-id "tc-1" :message "Something else?"
                   :schema '((type . "object")
                             (properties . ((note . ((type . "string"))))))))
    (should (string-match-p "Something else\\?" (substring-no-properties (car bodies))))))

(ert-deftest agent-shell-elicitation-clear-drops-settled-forms-test ()
  "Turn end clears forms and their active-request entries alike."
  (agent-shell-elicitation-tests--with-shell (state sent)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5 :schema '((type . "object")
                                   (properties . ((note . ((type . "string"))))))))
    (agent-shell-elicitation--clear :state state)
    (should-not (map-elt state :elicitations))
    (should-not (map-elt state :active-requests)))
  ;; A session predating `:elicitations' migrates rather than erroring.
  (let ((state (list (cons :buffer (current-buffer)))))
    (agent-shell-elicitation--clear :state state)
    (should (assq :elicitations state))
    (should (assq :active-requests state))))


;;; Option previews

(defconst agent-shell-elicitation-tests--preview-schema
  '((type . "object")
    (properties
     (q (type . "string") (title . "Approach")
        (oneOf . [((const . "a") (title . "Refactor first")
                   (description . "Clean up before adding.")
                   (_meta (_claude/askUserQuestionOption (preview . "mockup one\nmockup two"))))
                  ((const . "b") (title . "Add first"))]))))
  "A question whose first option carries a preview, as Claude sends it.")

(ert-deftest agent-shell-elicitation-parses-an-option-preview-test ()
  "A preview is read off the option, and its absence is not an error."
  (let ((options (agent-shell-elicitation--titled-options
                  (map-nested-elt agent-shell-elicitation-tests--preview-schema
                                  '(properties q oneOf)))))
    (should (equal (map-elt (seq-first options) :preview) "mockup one\nmockup two"))
    (should-not (map-elt (seq-elt options 1) :preview))))

(ert-deftest agent-shell-elicitation-preview-costs-no-extra-tab-stop-test ()
  "An option with a preview is still one TAB stop, glyph and all.

The disclosure glyph acts but is not navigable, so walking the form
stops once per option rather than twice for the ones offering a
preview."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5 :schema agent-shell-elicitation-tests--preview-schema))
    (with-temp-buffer
      (insert (car bodies))
      (goto-char (point-min))
      (let (stops)
        (while (agent-shell-elicitation-next-field)
          (push (map-elt (get-text-property (point) 'agent-shell-elicitation-control) :action)
                stops))
        ;; Two options and the two buttons; no stop for the glyph.
        (should (equal (nreverse stops) '(select select submit decline)))))))

(ert-deftest agent-shell-elicitation-preview-opens-and-closes-test ()
  "The preview opens and closes, from the option as well as from its glyph.

`end-of-line' lands after the glyph rather than on it, so requiring the
glyph would mean stepping back a character first, and RET past the end
of a line would stop meaning what it means elsewhere.

Which previews are open is kept in the elicitation, not in the rendered
text, so the shell buffer and the viewport cannot disagree about it."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5 :schema agent-shell-elicitation-tests--preview-schema))
    (should (string-match-p "Refactor first \u25b6" (substring-no-properties (car bodies))))
    (should-not (string-match-p "mockup" (substring-no-properties (car bodies))))
    (with-temp-buffer
      (insert (car bodies))
      (goto-char (point-min))
      (should (agent-shell-elicitation-next-field))
      ;; Point is on the option itself, not on the glyph.
      (should (eq (map-elt (get-text-property (point) 'agent-shell-elicitation-control) :action)
                  'select))
      (should (eq agent-shell-elicitation-preview-map
                  (get-text-property (point) 'keymap)))
      (should (eq #'agent-shell-elicitation-toggle-preview (key-binding (kbd "?")))))
    (agent-shell-elicitation--toggle-preview :state state :id 5 :key "q" :value "a")
    (let ((body (substring-no-properties (car bodies))))
      (should (string-match-p "Refactor first \u25bc" body))
      (should (string-match-p "mockup one" body))
      (should (string-match-p "mockup two" body)))
    (agent-shell-elicitation--toggle-preview :state state :id 5 :key "q" :value "a")
    (should (string-match-p "Refactor first \u25b6" (substring-no-properties (car bodies))))
    (should-not (string-match-p "mockup" (substring-no-properties (car bodies))))))

(ert-deftest agent-shell-elicitation-preview-is-readable-without-opening-test ()
  "The preview is offered as `help-echo', so \\[display-local-help] reads it out.

That needs no keybinding of our own, and works on the option itself
rather than only on the glyph.  An option carrying no preview offers no
help, keeps the plain control keymap so \\`?' stays free for whatever the
buffer binds it to, and says so when asked to open one anyway."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5 :schema agent-shell-elicitation-tests--preview-schema))
    (with-temp-buffer
      (insert (car bodies))
      (goto-char (point-min))
      (should (agent-shell-elicitation-next-field))
      (should (equal (get-text-property (point) 'help-echo) "mockup one\nmockup two"))
      ;; The option without a preview offers no help, and does not take
      ;; over `?' -- the viewport binds it to its help menu.
      (should (agent-shell-elicitation-next-field))
      (should-not (get-text-property (point) 'help-echo))
      (should (eq agent-shell-elicitation-map (get-text-property (point) 'keymap)))
      (should-not (eq #'agent-shell-elicitation-toggle-preview (key-binding (kbd "?"))))
      (cl-letf (((symbol-function 'agent-shell-elicitation--shell-buffer)
                 (lambda () (current-buffer))))
        (should-error (agent-shell-elicitation-toggle-preview) :type 'user-error)))
    (should-not (map-elt (agent-shell-elicitation--get state 5) :previews))))


;;; Tool calls behind a questionnaire

(defconst agent-shell-elicitation-tests--questionnaire
  '((questions . [((question . "Which colour?")
                   (header . "Colour")
                   (options . [((label . "Red")) ((label . "Blue"))]))]))
  "A tool call's input when it was bridged from an ask-the-user tool.")

(ert-deftest agent-shell-elicitation-recognises-a-questionnaire-test ()
  "A questionnaire is told from its input, not from a mark left when asked.

Restore replays notifications but never requests, so the form never
comes back and nothing marks the tool call.  Reading the shape is what
makes a restored session render like a live one."
  (should (agent-shell-elicitation--questionnaire-p
           agent-shell-elicitation-tests--questionnaire))
  ;; An ordinary tool's arguments are not a questionnaire.
  (should-not (agent-shell-elicitation--questionnaire-p '((path . "/tmp/x"))))
  (should-not (agent-shell-elicitation--questionnaire-p nil))
  ;; Nor is something merely carrying the key.
  (should-not (agent-shell-elicitation--questionnaire-p '((questions . []))))
  (should-not (agent-shell-elicitation--questionnaire-p
               '((questions . [((header . "no question text"))])))))

(ert-deftest agent-shell-elicitation-tool-call-reads-as-a-question-test ()
  "A call carrying a questionnaire is labelled a question, not `other'.

Agents bridging an ask-the-user tool give it the catch-all `other'
kind, which says nothing about what it is."
  (cl-flet ((kind-label (raw-input)
              (substring-no-properties
               (map-elt (agent-shell-make-tool-call-label
                         (list (cons :tool-calls
                                     (list (cons "t1" (list (cons :status "completed")
                                                            (cons :kind "other")
                                                            (cons :title "Which colour?")
                                                            (cons :raw-input raw-input))))))
                         "t1")
                        :status))))
    (should (equal (kind-label agent-shell-elicitation-tests--questionnaire) "✓ Question"))
    (should (equal (kind-label '((path . "/tmp/x"))) "✓ Other"))))

(ert-deftest agent-shell-elicitation-pending-form-owns-the-questions-test ()
  "A tool call stays quiet while its form is up, and speaks again after.

Both render the same questions, so showing them at once reads as being
asked twice."
  (cl-flet ((pending-p (status)
              (and (agent-shell-elicitation--pending-for-tool-call-p
                    :state (list (cons :elicitations
                                       (list (cons 5 (list (cons :status status)
                                                           (cons :tool-call-id "t1"))))))
                    :tool-call-id "t1")
                   t)))
    (should (pending-p 'pending))
    (should-not (pending-p 'answered))
    (should-not (pending-p 'declined))))

(ert-deftest agent-shell-elicitation-settles-into-its-tool-call-test ()
  "An answered questionnaire leaves its form behind for the tool call.

The call's own content spells the answers out, so the form would only
repeat what is already there -- and only the tool call survives a
restore, so letting it be the record keeps both readings identical."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (let (deleted)
      (cl-letf (((symbol-function 'agent-shell--delete-fragment)
                 (lambda (&rest args) (push (plist-get args :block-id) deleted))))
        (map-put! state :tool-calls
                  (list (cons "t1" (list (cons :raw-input
                                               agent-shell-elicitation-tests--questionnaire)))))
        (agent-shell--on-request
         :state state
         :acp-request (agent-shell-elicitation-tests--request
                       :id 5 :tool-call-id "t1"
                       :schema '((type . "object")
                                 (properties . ((note . ((type . "string"))))))))
        (agent-shell-elicitation--respond :state state :id 5 :action 'accept)
        (should (equal deleted '("elicitation-5")))))))

(ert-deftest agent-shell-elicitation-without-a-questionnaire-keeps-its-form-test ()
  "An elicitation with no questionnaire behind it stays as its own record.

An MCP server's form is raised by a tool whose input says nothing about
it, so removing the form would erase the only account of what was asked."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (let (deleted)
      (cl-letf (((symbol-function 'agent-shell--delete-fragment)
                 (lambda (&rest args) (push (plist-get args :block-id) deleted))))
        (map-put! state :tool-calls
                  (list (cons "t1" (list (cons :raw-input '((path . "/tmp/x")))))))
        (agent-shell--on-request
         :state state
         :acp-request (agent-shell-elicitation-tests--request
                       :id 5 :tool-call-id "t1"
                       :schema '((type . "object")
                                 (properties . ((note . ((type . "string"))))))))
        (agent-shell-elicitation--set-value state 5 "note" "kept")
        (agent-shell-elicitation--respond :state state :id 5 :action 'accept)
        (should-not deleted)
        (should (string-match-p "Answered" (substring-no-properties (car bodies))))))))


;;; Capability

(ert-deftest agent-shell-elicitation-capability-advertises-form-only-test ()
  "The handshake advertises `form' and stays silent about `url'.

An empty `elicitation' object would mean zero modes, so the `form' key
has to be there for the capability to say anything at all."
  (should (equal (json-serialize
                  (map-nested-elt (agent-shell--make-initialize-request)
                                  '(:params clientCapabilities elicitation)))
                 "{\"form\":{}}")))


;;; Keys and navigation

(ert-deftest agent-shell-elicitation-numeric-input-is-corrected-not-coerced-test ()
  "Bad numeric input comes back to be fixed; it is never quietly reshaped.

Each retry is prefilled with the text that was rejected, so a correction
starts from what was typed -- prefilling the original value again would
prepend it, turning a 2 corrected to 5 into 25.  An integer field
refuses a fraction rather than truncating it, since answering 1.9 with 1
reports a number the user never typed and leaves a plausible-looking
form behind."
  ;; An integer field names its requirement whatever is wrong with the
  ;; input, which reads better than "not a number" for `2 apples\='.
  (dolist (case '((integer ("1.9" "2 apples" "2") 2
                   ("N: " "Whole number needed.  N: " "Whole number needed.  N: "))
                  (number ("1.9") 1.9 ("N: "))
                  (number ("nope" ".5") 0.5 ("N: " "Not a number.  N: "))))
    (let ((reads (nth 1 case))
          (prompts nil)
          (prefills nil))
      (cl-letf (((symbol-function 'agent-shell-elicitation--read-string)
                 (lambda (prompt initial)
                   (push prompt prompts)
                   (push initial prefills)
                   (pop reads))))
        (should (equal (agent-shell-elicitation--read-value
                        :field `((:key . "n") (:title . "N") (:type . ,(nth 0 case))))
                       (nth 2 case)))
        (should (equal (nreverse prompts) (nth 3 case)))
        ;; Nothing to start from, then whatever each retry rejected.
        (should (equal (nreverse prefills)
                       (cons nil (butlast (nth 1 case)))))))))

(ert-deftest agent-shell-elicitation-arms-and-cancels-the-idle-timer-test ()
  "A pending form arms the idle notification, and answering cancels it.

An unanswered form blocks the turn exactly as an unanswered permission
does, so subscribers watching for `idle' must hear about it."
  (agent-shell-elicitation-tests--with-shell (state sent)
    (let (armed cancelled)
      (cl-letf (((symbol-function 'agent-shell--start-idle-timer)
                 (lambda (&rest args) (setq armed args)))
                ((symbol-function 'agent-shell--cancel-idle-timer)
                 (lambda () (setq cancelled t))))
        (agent-shell--on-request
         :state state
         :acp-request (agent-shell-elicitation-tests--request
                       :id 5 :schema '((type . "object")
                                       (properties . ((note . ((type . "string"))))))))
        (should (equal (plist-get armed :event) 'elicitation-request))
        (should (equal (map-elt (plist-get armed :data) :request-id) 5))
        (should-not cancelled)
        (agent-shell-elicitation--respond :state state :id 5 :action 'decline)
        (should cancelled)))))

(ert-deftest agent-shell-elicitation-keys-drive-the-whole-form-test ()
  "Pressing RET on each control does what the control says.

Driven through `execute-kbd-macro\=' rather than by calling the commands,
so a control whose action reaches no branch of
`agent-shell-elicitation-act\=' shows up as the silent no-op it is.

The buffer is displayed rather than temporary: key dispatch goes
through the selected window, so keys pressed at a buffer nobody is
showing resolve against something else entirely."
  (let ((buffer (generate-new-buffer "*agent-shell-elicitation-keys-test*"))
        (sent nil))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (let ((state (list (cons :buffer buffer)
                             (cons :client 'test-client)
                             (cons :tool-calls nil)
                             (cons :elicitations nil)
                             (cons :active-requests nil)
                             (cons :event-subscriptions nil)
                             (cons :idle-timer nil)
                             (cons :last-entry-type nil))))
            (cl-letf (((symbol-function 'agent-shell--state)
                       (lambda () state))
                      ((symbol-function 'agent-shell--update-fragment)
                       ;; Render for real, so point sits on live text.
                       (lambda (&rest args)
                         (let ((inhibit-read-only t))
                           (erase-buffer)
                           (insert (plist-get args :body)))))
                      ((symbol-function 'agent-shell-viewport--buffer)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-shell--append-transcript)
                       (lambda (&rest _)))
                      ((symbol-function 'acp-send-response)
                       (lambda (&rest args) (push (plist-get args :response) sent))))
              (agent-shell--on-request
               :state state
               :acp-request (agent-shell-elicitation-tests--request
                             :id 5 :schema agent-shell-elicitation-tests--ask-schema))
              (cl-flet ((press-on (action keys)
                          (goto-char (point-min))
                          (let ((match (text-property-search-forward
                                        'agent-shell-elicitation-control action
                                        (lambda (wanted control)
                                          (eq (map-elt control :action) wanted)))))
                            (should match)
                            (goto-char (prop-match-beginning match)))
                          (execute-kbd-macro keys)))
                ;; RET on an option picks it.
                (press-on 'select (kbd "RET"))
                (should (equal (map-elt (agent-shell-elicitation--get state 5) :values)
                               '(("question_0" . "Red"))))
                ;; RET on the free-text answer opens the minibuffer; what
                ;; follows in the macro is what gets typed into it.  The
                ;; pick it replaces goes.
                (press-on 'custom (vconcat (kbd "RET") (string-to-vector "teal") (kbd "RET")))
                (should (equal (map-elt (agent-shell-elicitation--get state 5) :values)
                               '(("question_0_custom" . "teal"))))
                ;; RET on Submit sends exactly the typed answer.
                (press-on 'submit (kbd "RET"))
                (should (equal (map-nested-elt (seq-first sent) '(:result content))
                               '((question_0_custom . "teal"))))))))
      (kill-buffer buffer))))

(ert-deftest agent-shell-elicitation-keys-reach-controls-only-test ()
  "The form keys sit on its controls, leaving the rest of the buffer alone.

A `keymap' text property over a wide region has shadowed
`agent-shell-mode-map' before, so assert on `key-binding' at a position
rather than driving keys."
  (with-temp-buffer
    (insert (agent-shell-elicitation--make-control
             :text "(*) Apple" :id 1 :key "pick" :value "a" :action 'select))
    (insert "plain")
    (goto-char (point-min))
    (should (eq #'agent-shell-elicitation-act (key-binding (kbd "RET"))))
    (should (eq #'ignore (key-binding [?x])))
    ;; Off the control nothing is shadowed.  Here RET is `newline'; in a
    ;; shell buffer it submits the prompt.
    (goto-char (point-max))
    (should (eq #'newline (key-binding (kbd "RET"))))
    (should (eq #'self-insert-command (key-binding [?x])))))

(ert-deftest agent-shell-elicitation-navigation-walks-controls-test ()
  "Navigation stops once per control, at its first character.

Point lands on the marker rather than mid-label, so a screen reader
reads the whole option from its start."
  (with-temp-buffer
    (dolist (option '("a" "b"))
      (insert "  ")
      (insert (agent-shell-elicitation--make-control
               :text (format "( ) %s" option)
               :id 1 :key "pick" :value option :action 'select))
      (insert "\n"))
    (goto-char (point-min))
    (should (equal (char-after (agent-shell-elicitation-next-field)) ?\())
    (should (equal (char-after (agent-shell-elicitation-next-field)) ?\())
    (should-not (agent-shell-elicitation-next-field))
    (should (agent-shell-elicitation-previous-field))
    (should-not (agent-shell-elicitation-previous-field))))

(ert-deftest agent-shell-elicitation-jump-lands-at-the-top-of-the-form-test ()
  "A form arriving puts point on its question, with the rest forward.

Landing on a control would leave the question and the field headings
above point, and those are not navigable, so TAB would never reach
them and a keyboard user would have to walk back up by line."
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 5 :message "Which approach should I take?"
                   :schema '((type . "object")
                             (properties . ((approach . ((type . "string")
                                                         (title . "Approach")
                                                         (enum . ["Refactor first" "Add first"])))))
                             (required . ["approach"]))))
    (with-temp-buffer
      (insert "earlier transcript content\n\n")
      (insert (car bodies))
      (goto-char (point-max))
      (should (agent-shell-elicitation-jump-to-latest-form))
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))
                     "    Which approach should I take?"))
      ;; Everything the form offers is forward of point.
      (should (agent-shell-elicitation-next-field))
      (should (equal (buffer-substring-no-properties
                      (point) (line-end-position))
                     "( ) Refactor first"))))

  ;; With no message of its own, the form lands on its first heading
  ;; instead -- a questionnaire whose question is already the tool
  ;; call's title sends none.
  (agent-shell-elicitation-tests--with-shell (state sent bodies)
    (agent-shell--on-request
     :state state
     :acp-request (agent-shell-elicitation-tests--request
                   :id 6 :message nil
                   :schema '((type . "object")
                             (properties . ((approach . ((type . "string")
                                                         (title . "Approach")
                                                         (enum . ["Refactor first"]))))))))
    (with-temp-buffer
      (insert (car bodies))
      (goto-char (point-max))
      (should (agent-shell-elicitation-jump-to-latest-form))
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))
                     "    Approach")))))

(ert-deftest agent-shell-elicitation-goto-control-survives-rerender-test ()
  "Point returns to the control just acted on after the body is replaced.

Controls carry identity only -- which elicitation, field and option --
so the same control still matches once its value has changed."
  (let ((control (list (cons :id 1) (cons :key "pick")
                       (cons :value "b") (cons :action 'select))))
    (with-temp-buffer
      (insert "preamble\n")
      (dolist (option '("a" "b"))
        (insert (agent-shell-elicitation--make-control
                 ;; Rendered selected this time, as a re-render would.
                 :text (format "(*) %s" option)
                 :id 1 :key "pick" :value option :action 'select))
        (insert "\n"))
      (goto-char (point-min))
      (should (agent-shell-elicitation--goto-control control))
      (should (equal (buffer-substring-no-properties (point) (+ (point) 5)) "(*) b"))
      ;; A control that is gone (a settled form) leaves point alone.
      (should-not (agent-shell-elicitation--goto-control
                   (map-insert control :value "gone"))))))

(provide 'agent-shell-elicitation-tests)

;;; agent-shell-elicitation-tests.el ends here
