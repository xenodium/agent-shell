;;; agent-shell-elicitation-tests.el --- Tests for inline ACP forms -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for ACP form normalization, inline rendering, and ownership.

;;; Code:

(require 'agent-shell)
(require 'agent-shell-elicitation)
(require 'ert)

(defun agent-shell-elicitation-tests--state (&optional buffer)
  "Return fresh shell state using BUFFER."
  (let ((state
         (agent-shell--make-state
          :agent-config '((:mode-line-name . "Loki"))
          :buffer buffer)))
    (map-put! state :client 'client)
    (map-put! state :session '((:id . "session-1")))
    state))

(cl-defun agent-shell-elicitation-tests--request
    (&key (id 1) (message "Choose") schema request-id
          (session-id "session-1") tool-call-id (mode "form"))
  "Return an elicitation request.

ID identifies the reverse request.  MESSAGE, SCHEMA, and MODE are its
form data.  REQUEST-ID selects request scope; otherwise SESSION-ID and
optional TOOL-CALL-ID select session scope."
  `((id . ,id)
    (method . "elicitation/create")
    (params
     ,@(if request-id
           `((requestId . ,request-id))
         `((sessionId . ,session-id)
           ,@(when tool-call-id
               `((toolCallId . ,tool-call-id)))))
     (mode . ,mode)
     (message . ,message)
     (requestedSchema . ,schema))))

(defun agent-shell-elicitation-tests--boolean-schema ()
  "Return Loki's saved-connection authorization schema."
  '((type . "object")
    (properties
     (authorize
      (type . "boolean")
      (title . "Use saved connection")
      (description . "Allow this connection.")
      (default)))
    (required . ["authorize"])))

(defun agent-shell-elicitation-tests--all-fields-schema ()
  "Return one constrained schema containing every supported field kind."
  '((type . "object")
    (properties
     (text (type . "string") (default . "hello")
           (minLength . 2) (maxLength . 8)
           (pattern . "^[a-z]+$") (format . "text"))
     (amount (type . "number") (default . 1.5)
             (minimum . 1) (maximum . 2))
     (count (type . "integer") (default . 2)
            (minimum . 1) (maximum . 3))
     (enabled (type . "boolean") (default))
     (color (type . "string")
            (enum . ["red" "blue"])
            (default . "blue"))
     (size (type . "string")
           (oneOf . [((const . "s") (title . "Small"))
                     ((const . "l") (title . "Large"))])
           (default . "l"))
     (tags (type . "array")
           (items
            (type . "string")
            (anyOf . [((const . "one") (title . "One"))
                      ((const . "two") (title . "Two"))]))
           (default . ["two"])
           (minItems . 1)
           (maxItems . 2)))
    (required . ["text" "amount" "count" "enabled"
                 "color" "size" "tags"])))

(defun agent-shell-elicitation-tests--entry (schema)
  "Normalize SCHEMA and return its form data."
  (agent-shell-elicitation--normalize-schema schema))

(defmacro agent-shell-elicitation-tests--with-shell
    (shell state &rest body)
  "Run BODY with initialized SHELL and STATE bindings."
  (declare (indent 2) (debug (symbolp symbolp body)))
  `(let* ((agent-shell-header-style nil)
          (,shell (generate-new-buffer " *elicitation-shell*"))
          (,state (agent-shell-elicitation-tests--state ,shell)))
     (unwind-protect
         (progn
           (with-current-buffer ,shell
             (comint-mode)
             (setq major-mode 'agent-shell-mode)
             (agent-shell-ui-mode 1)
             (setq-local agent-shell--state ,state
                         kill-buffer-query-functions nil
                         shell-maker--config
                         (make-shell-maker-config
                          :name "Agent"
                          :prompt "> "
                          :prompt-regexp "^> ")))
           ,@body)
       (when (buffer-live-p ,shell)
         (kill-buffer ,shell)))))

(ert-deftest agent-shell-elicitation-normalizes-and-collects-fields-test ()
  "Normalize and collect all supported field kinds and constraints."
  (let* ((form
          (agent-shell-elicitation-tests--entry
           (agent-shell-elicitation-tests--all-fields-schema)))
         (fields (map-elt form :fields)))
    (should
     (equal (seq-map (lambda (field) (map-elt field :kind)) fields)
            '(string number integer boolean
                     single-select single-select multi-select)))
    (should (equal (map-elt (seq-first fields) :minimum) 2))
    (should (equal (map-elt (seq-first fields) :maximum) 8))
    (should (equal (map-elt (seq-first fields) :pattern) "^[a-z]+$"))
    (should
     (equal
      (agent-shell-elicitation--content form)
      '((text . "hello")
        (amount . 1.5)
        (count . 2)
        (enabled . :false)
        (color . "blue")
        (size . "l")
        (tags . ["two"]))))))

(ert-deftest agent-shell-elicitation-preserves-unset-and-false-test ()
  "Keep omission distinct from JSON false on the live parse path."
  (let* ((form
          (agent-shell-elicitation-tests--entry
           (map-nested-elt
            (acp--parse-json
             "{\"requestedSchema\":{\"type\":\"object\",\
\"properties\":{\"note\":{\"type\":\"string\"},\
\"authorize\":{\"type\":\"boolean\",\"default\":false}},\
\"required\":[\"authorize\"]}}")
            '(requestedSchema))))
         (fields (map-elt form :fields)))
    (should-not (map-elt (seq-first fields) :included))
    (should (map-elt (seq-elt fields 1) :included))
    (should
     (equal (agent-shell-elicitation--content form)
            '((authorize . :false))))))

(ert-deftest agent-shell-elicitation-rejects-malformed-schema-test ()
  "Reject schemas whose supported forms cannot be represented faithfully."
  (seq-do
   (lambda (schema)
     (should-error (agent-shell-elicitation-tests--entry schema)))
   '(((type . "object")
      (properties (nested (type . "object"))))
     ((type . "object")
      (properties (choice (type . "string") (enum . []))))
     ((type . "object")
      (properties
       (value (type . "number") (minimum . 2) (maximum . 1))))
     ((type . "object")
      (properties
       (choices
        (type . "array")
        (items (type . "number") (enum . ["one"])))))
     ((type . "object")
      (properties
       (choices
        (type . "array")
        (items (enum . ["one"])))))
     ((type . "object")
      (properties
       (text
        (type . "string")
        (minLength . 4294967296)))))))

(ert-deftest agent-shell-elicitation-preserves-unknown-field-types-test ()
  "Keep unknown types opaque without preventing valid optional answers."
  (let* ((unknown
          '((type . "_future")
            (title . "Future value")
            (_meta (extension . t))))
         (form
          (agent-shell-elicitation-tests--entry
           (list
            (cons 'type "object")
            (cons
             'properties
             (list
              (cons
               'known
               '((type . "string") (default . "value")))
              (cons 'future unknown)))
            (cons 'required ["known"]))))
         (field
          (agent-shell-elicitation--field form 'future)))
    (should (eq (map-elt field :kind) 'unsupported))
    (should
     (equal (agent-shell-elicitation--content form)
            '((known . "value"))))
    (let ((required
           (agent-shell-elicitation-tests--entry
            (list
             (cons 'type "object")
             (cons 'properties
                   (list (cons 'future unknown)))
             (cons 'required ["future"])))))
      (should
       (eq
        (map-elt
         (agent-shell-elicitation--required-unsupported-field
          required)
         :kind)
        'unsupported))
      (should-error
       (agent-shell-elicitation--content required)
       :type 'user-error)
      (let* ((body
              (agent-shell-elicitation--body
               (agent-shell-elicitation-tests--state)
               (append
                '((:message . "Question"))
                required)))
             (plain (substring-no-properties body)))
        (should (string-match-p
                 "Unsupported field type \"_future\"" plain))
        (should (string-match-p "Cannot submit" plain))
        (should-not
         (text-property-any
          0 (length body)
          'agent-shell-elicitation-action 'submit body))
        (should (string-match-p "Decline" plain))
        (should (string-match-p "Cancel" plain))))
  (let* ((form
          (agent-shell-elicitation-tests--entry
           '((type . "object")
             (properties
              (choices
               (type . "array")
               (items
                (type . "_future")
                (enum . ["one"])))))))
         (field (seq-first (map-elt form :fields))))
    (should (eq (map-elt field :kind) 'unsupported))
    (should (equal (map-elt field :raw-type) "array"))
    (should (equal (map-elt field :raw-item-type) "_future")))))

(ert-deftest agent-shell-elicitation-tolerates-defaulted-schema-fields-test ()
  "Apply ACP's reader defaults without inventing a choice."
  (should-not
   (map-elt (agent-shell-elicitation-tests--entry nil) :fields))
  (let* ((form
          (agent-shell-elicitation-tests--entry
           '((type . 23)
             (properties
              (text (type . "string") (default . 42))
              (choice (type . "string")
                      (enum . ["a" "b"])
                      (default . "missing"))
              (tags (type . "array")
                    (items (type . "string")
                           (enum . ["a" "b"]))
                    (default . ["a" 2 "missing"]))))))
         (fields (map-elt form :fields)))
    (should-not (map-elt (seq-elt fields 0) :included))
    (should-not (map-elt (seq-elt fields 1) :included))
    (should-not (map-elt (seq-elt fields 1) :has-value))
    (should (equal (map-elt (seq-elt fields 2) :value) '("a")))))

(ert-deftest agent-shell-elicitation-validates-edited-values-test ()
  "Validate length, numeric, integer, and selection-count constraints."
  (let* ((form
          (agent-shell-elicitation-tests--entry
           (agent-shell-elicitation-tests--all-fields-schema)))
         (fields (map-elt form :fields))
         (text (seq-elt fields 0))
         (amount (seq-elt fields 1))
         (count (seq-elt fields 2))
         (tags (seq-elt fields 6)))
    (map-put! text :value "x")
    (should-error (agent-shell-elicitation--content form)
                  :type 'user-error)
    (map-put! text :value "hello")
    (map-put! amount :value "3")
    (should-error (agent-shell-elicitation--content form)
                  :type 'user-error)
    (map-put! amount :value "1.5")
    (map-put! count :value "2.5")
    (should-error (agent-shell-elicitation--content form)
                  :type 'user-error)
    (map-put! count :value "2")
    (map-put! tags :value nil)
    (should-error (agent-shell-elicitation--content form)
                  :type 'user-error)))

(ert-deftest agent-shell-elicitation-validates-json-numbers-test ()
  "Require complete JSON numbers and signed 64-bit integers."
  (should (equal 1.5
                 (agent-shell-elicitation--parse-number "1.5" nil)))
  (should (equal 2
                 (agent-shell-elicitation--parse-number "2" t)))
  (should-error
   (agent-shell-elicitation--parse-number "2 trailing" nil))
  (should-error
   (agent-shell-elicitation--parse-number "2.5" t))
  (should-error
   (agent-shell-elicitation--parse-number "9223372036854775808" t)))

(ert-deftest agent-shell-elicitation-renders-inline-in-shell-test ()
  "Render and settle one authoritative form in its shell."
  (agent-shell-elicitation-tests--with-shell shell state
    (let (sent)
      (cl-letf (((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (push (plist-get args :response) sent))))
        (with-current-buffer shell
          (agent-shell-elicitation--on-create-request
           :state state
           :acp-request
           (agent-shell-elicitation-tests--request
            :id "form-1"
            :message "Use *literal* [site](https://example.test)"
            :schema (agent-shell-elicitation-tests--boolean-schema))))
        (with-current-buffer shell
          (should
           (string-match-p
            (regexp-quote
             "Use *literal* [site](https://example.test)")
            (buffer-string)))
          (goto-char (point-min))
          (search-forward "https://example.test")
          (should
           (get-text-property
            (1- (point)) 'agent-shell-literal-content))
          (should-not (get-text-property (1- (point)) 'button))
          (should
           (string-match-p
            (concat (regexp-quote "( ) Yes")
                    ".*"
                    (regexp-quote "(*) No"))
            (buffer-string)))
          ;; Generic expansion and deferred rendering revisit fragment
          ;; bodies; literal form text must remain inert in those passes.
          (let ((before (buffer-string)))
            (save-restriction
              (narrow-to-region
               (previous-single-property-change
                (point) 'agent-shell-literal-content nil (point-min))
               (next-single-property-change
                (point) 'agent-shell-literal-content nil (point-max)))
              (agent-shell--render-markdown))
            (should (equal before (buffer-string))))
          (should
           (agent-shell-elicitation--goto-control
            :request-id "form-1"
            :action 'set-option
            :field 'authorize
            :option t))
          (agent-shell-elicitation-activate))
        (let* ((entry
                (agent-shell-elicitation--entry state "form-1"))
               (field
                (agent-shell-elicitation--field entry 'authorize)))
          (should (map-elt field :value)))
        (with-current-buffer shell
          (should
           (string-match-p
            (concat (regexp-quote "(*) Yes")
                    ".*"
                    (regexp-quote "( ) No"))
            (buffer-string)))
          (goto-char (point-min))
          (search-forward "( ) No")
          (agent-shell-elicitation-activate))
        (let* ((entry
                (agent-shell-elicitation--entry state "form-1"))
               (field
                (agent-shell-elicitation--field entry 'authorize)))
          (should-not (map-elt field :value)))
        (with-current-buffer shell
          (should
           (string-match-p
            (concat (regexp-quote "( ) Yes")
                    ".*"
                    (regexp-quote "(*) No"))
            (buffer-string)))
          (should
           (agent-shell-elicitation--goto-control
            :request-id "form-1"
            :action 'set-option
            :field 'authorize
            :option t))
          (agent-shell-elicitation-activate)
          (should
           (agent-shell-elicitation--goto-control
            :request-id "form-1" :action 'submit))
          (agent-shell-elicitation-activate))
        (should-not (map-elt state :elicitations))
        (should
         (equal
          (seq-first sent)
          '((:request-id . "form-1")
            (:result . ((action . "accept")
                        (content . ((authorize . t))))))))
        (with-current-buffer shell
          (should (string-match-p "Submitted" (buffer-string)))
          (should
           (string-match-p "Use saved connection: Yes" (buffer-string)))
          (goto-char (point-min))
          (should-not
           (text-property-search-forward
            'agent-shell-elicitation-control t t)))))))

(ert-deftest agent-shell-elicitation-cancellation-leaves-summary-test ()
  "Replace an automatically cancelled form with its inert reason."
  (agent-shell-elicitation-tests--with-shell shell state
    (let (response)
      (cl-letf (((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (setq response (plist-get args :response)))))
        (with-current-buffer shell
          (agent-shell-elicitation--on-create-request
           :state state
           :acp-request
           (agent-shell-elicitation-tests--request
            :id "form"
            :schema (agent-shell-elicitation-tests--boolean-schema)))
          (agent-shell-elicitation--send-cancelled-error
           :state state
           :request-id "form"
           :message "Owning tool call finished")))
      (should
       (equal -32800
              (map-nested-elt response '(:error code))))
      (should-not (map-elt state :elicitations))
      (with-current-buffer shell
        (should (string-match-p "Cancelled" (buffer-string)))
        (should
         (string-match-p "Owning tool call finished" (buffer-string)))
        (goto-char (point-min))
        (should-not
         (text-property-search-forward
          'agent-shell-elicitation-control t t))
        (agent-shell-elicitation--on-create-request
         :state state
         :acp-request
         (agent-shell-elicitation-tests--request
          :id "form"
          :message "Second request"
          :schema (agent-shell-elicitation-tests--boolean-schema))))
      (with-current-buffer shell
        (should
         (string-match-p "Owning tool call finished" (buffer-string)))
        (should (string-match-p "Second request" (buffer-string)))))))

(ert-deftest agent-shell-elicitation-stable-namespace-and-navigation-test ()
  "Keep forms addressable across turns and expose each inline control."
  (agent-shell-elicitation-tests--with-shell shell state
    (cl-letf (((symbol-function 'acp-send-response) #'ignore))
      (with-current-buffer shell
        (agent-shell-elicitation--on-create-request
         :state state
         :acp-request
         (agent-shell-elicitation-tests--request
          :id 1
          :schema (agent-shell-elicitation-tests--boolean-schema)))
        (map-put! state :request-count 99)
        (agent-shell-elicitation--render
         state (agent-shell-elicitation--entry state 1))
        (goto-char (point-min))
        (let ((first (agent-shell-next-elicitation-control)))
          (should first)
          (should (> (agent-shell-next-elicitation-control) first)))
        (agent-shell-elicitation--send-user-response
         :state state :request-id 1 :action "cancel")
        (should-not
         (text-property-search-forward
          'agent-shell-elicitation-request-id 1 t))))))

(ert-deftest agent-shell-elicitation-focuses-form-above-live-draft-test ()
  "Show a form above the prompt without changing its draft.

The selected shell window must actually include the form control in its
displayed screen-line range, not merely store that position as its
possibly off-screen window point."
  (agent-shell-elicitation-tests--with-shell shell state
    (cl-letf (((symbol-function 'acp-send-response) #'ignore))
      (save-window-excursion
        (let ((window (selected-window)))
          (with-current-buffer shell
            (rename-buffer "elicitation-visible-shell" t))
          (set-window-buffer window shell)
          (select-window window)
          (set-buffer shell)
          (insert (make-string 100 ?\n))
          (let ((prompt-start (copy-marker (point) nil)))
            (insert
             (propertize
              "> "
              'font-lock-face
              '(comint-highlight-prompt comint-highlight-prompt)))
            (setq-local comint-last-prompt
                        (cons prompt-start (copy-marker (point) nil))))
          (insert "draft text")
          (setq-local agent-shell-chat-mode t
                      agent-shell-chat--labeled t)
          (agent-shell-chat--relabel)
          (goto-char (point-max))
          (agent-shell-elicitation--on-create-request
           :state state
           :acp-request
           (agent-shell-elicitation-tests--request
            :id "form"
            :schema (agent-shell-elicitation-tests--boolean-schema)))
          (should
           (get-text-property
            (point) 'agent-shell-elicitation-control))
          (should (eq (window-buffer window) shell))
          (should (= (window-point window) (point)))
          (should-not (get-char-property (point) 'invisible))
          (should (<= (window-start window) (point)))
          (should
           (< (count-screen-lines
               (window-start window) (point) nil window)
              (window-body-height window)))
          (should (string-suffix-p "> draft text" (buffer-string)))
          (should
           (equal "draft text"
                  (buffer-substring-no-properties
                   (marker-position (cdr comint-last-prompt))
                   (point-max)))))))))

(ert-deftest agent-shell-permission-displays-shell-without-changing-viewport-test ()
  "Reveal a permission in its shell without modifying a viewport draft."
  (agent-shell-elicitation-tests--with-shell shell state
    (let ((viewport (agent-shell-viewport--buffer :shell-buffer shell)))
      (unwind-protect
          (save-window-excursion
            (with-current-buffer shell
              (insert (make-string 100 ?\n)))
            (with-current-buffer viewport
              (insert "unfinished draft")
              (goto-char 5))
            (set-window-buffer (selected-window) viewport)
            (with-current-buffer shell
              (agent-shell--on-request
               :state state
               :acp-request
               '((id . 7)
                 (method . "session/request_permission")
                 (params
                  (options
                   . [((optionId . "allow")
                       (name . "Allow")
                       (kind . "allow_once"))])
                  (toolCall
                   (toolCallId . "tool")
                   (title . "Run command")
                   (kind . "execute")
                   (status . "pending"))))))
            (should (eq (window-buffer (selected-window)) shell))
            (with-current-buffer shell
              (should
               (get-text-property
                (point) 'agent-shell-permission-button))
              (should-not (get-char-property (point) 'invisible))
              (should
               (<= (window-start (selected-window)) (point)))
              (should
               (< (count-screen-lines
                   (window-start (selected-window))
                   (point)
                   nil
                   (selected-window))
                  (window-body-height (selected-window)))))
            (with-current-buffer viewport
              (should
               (derived-mode-p 'agent-shell-viewport-edit-mode))
              (should (equal "unfinished draft" (buffer-string)))
              (should (= 5 (point)))))
        (when (buffer-live-p viewport)
          (let ((agent-shell-viewport--clean-up nil))
            (kill-buffer viewport)))))))

(ert-deftest agent-shell-elicitation-session-load-focuses-shell-form-test ()
  "Focus a request that arrives while an existing session is loading.

This follows the non-viewport `session/list' then `session/load' path
used when a user selects a saved session.  The displayed chat shell must
land on the form control while preserving type-ahead at its live prompt."
  (let ((agent-shell-prefer-viewport-interaction nil)
        (agent-shell-session-strategy 'prompt)
        (agent-shell-session-restore-verbosity 'first-last)
        (agent-shell-file-completion-enabled nil)
        (agent-shell-show-busy-indicator nil)
        (agent-shell-show-welcome-message t)
        (agent-shell-chat-mode-enabled t)
        (next-request-id 0)
        shell
        list-success
        (client (acp-make-client :command "cat")))
    (let ((config
           (agent-shell-make-agent-config
            :mode-line-name "Mock Agent"
            :buffer-name "Mock Agent"
            :shell-prompt "Mock> "
            :shell-prompt-regexp "^Mock> "
            :client-maker
            (lambda (buffer)
              (setq shell buffer)
              (map-put! client :context-buffer buffer)
              client))))
      (unwind-protect
          (cl-letf
              (((symbol-function 'agent-shell--context)
                (lambda (&rest _)
                  (concat (make-string 100 ?\n) "draft text")))
               ((symbol-function 'acp-send-response) #'ignore)
               ((symbol-function 'acp-send-request)
                (lambda (&rest args)
                  (let* ((request (plist-get args :request))
                         (method (map-elt request :method))
                         (request-id
                          (setq next-request-id (1+ next-request-id))))
                    (when-let* ((on-sent (plist-get args :on-sent)))
                      (funcall on-sent
                               `((:request-id . ,request-id))))
                    (pcase method
                      ("initialize"
                       (funcall
                        (plist-get args :on-success)
                        '((agentCapabilities
                           (loadSession . t)
                           (sessionCapabilities (list))))))
                      ("session/list"
                       (setq list-success (plist-get args :on-success)))
                      ("session/load"
                       (should (eq (window-buffer (selected-window))
                                   shell))
                       (with-current-buffer shell
                         (should (derived-mode-p 'agent-shell-mode))
                         (should agent-shell-chat-mode)
                         (seq-do
                          (lambda (handler)
                            (funcall
                             handler
                             (agent-shell-elicitation-tests--request
                              :id "authorization"
                              :message
                              "Authorize Loki to use this saved connection?
Provider: \"OpenAI ChatGPT subscription [endpoint supplied by Loki]\"
Model: \"gpt-5.6-sol\"
Chat endpoint: \"https://chatgpt.com/backend-api/codex/responses\"
Models endpoint: \"https://chatgpt.com/backend-api/codex/models?client_version=0.144.0\"
Credential: \"openai-subscription:openai\"
Streaming: \"yes\"

Do not enter passwords, API keys, or other credentials in this form."
                              :request-id request-id
                              :schema
                              (agent-shell-elicitation-tests--boolean-schema))))
                          (map-elt client :request-handlers))
                         (should
                          (get-text-property
                           (point) 'agent-shell-elicitation-control))
                         (should
                          (= (window-point (selected-window)) (point)))
                         (should-not
                          (get-char-property (point) 'invisible))
                         (should
                          (<= (window-start (selected-window))
                              (point)))
                         (should
                          (< (count-screen-lines
                              (window-start (selected-window))
                              (point)
                              nil
                              (selected-window))
                             (window-body-height (selected-window))))
                         (should
                          (string-match-p
                           "Authorize Loki"
                           (buffer-string)))
                         (let ((visible
                                (agent-shell-chat--displayed-substring
                                 (point-min) (point-max))))
                           (should
                            (string-match-p
                             "Input requested by Mock Agent"
                             visible))
                           (should
                            (string-match-p "Authorize Loki" visible))
                           (should
                            (< (string-match
                                "Input requested by Mock Agent"
                                visible)
                               (string-match "\n Me \n" visible))))
                         (should
                          (agent-shell-elicitation--goto-control
                           :request-id "authorization"
                           :action 'set-option
                           :field 'authorize
                           :option t))
                         (agent-shell-elicitation-activate)
                         (goto-char (point-min))
                         (search-forward "Authorize Loki")
                         (should-not
                          (equal
                           ""
                           (get-char-property
                            (match-beginning 0) 'display)))
                         (let ((visible
                                (agent-shell-chat--displayed-substring
                                 (point-min) (point-max))))
                           (should
                            (string-match-p
                             "Input requested by Mock Agent"
                             visible))
                           (should
                            (string-match-p "Authorize Loki" visible))
                           (should
                            (< (string-match
                                "Input requested by Mock Agent"
                                visible)
                               (string-match "\n Me \n" visible))))
                         (should
                          (string-suffix-p
                           "draft text" (buffer-string))))
                       (funcall
                        (plist-get args :on-success)
                        '((modes
                           (currentModeId . "default")
                           (availableModes . []))
                          (models
                           (currentModelId . "model")
                           (availableModels . [])))))
                      (_
                       (ert-fail
                        (format "Unexpected ACP request: %s"
                                method))))))))
            (agent-shell--dwim :config config :new-shell t)
            (should (buffer-live-p shell))
            (should list-success)
            (with-current-buffer shell
              (funcall
               list-success
               '((sessions
                  . [((sessionId . "saved-session")
                      (cwd . "/tmp")
                      (title . "Saved session"))])))))
        (when (buffer-live-p shell)
          (with-current-buffer shell
            (setq-local kill-buffer-query-functions nil))
          (kill-buffer shell))))))

(ert-deftest agent-shell-elicitation-request-scope-follows-wire-request-test ()
  "Bind a pre-session form to the actual outgoing JSON-RPC request id."
  (with-temp-buffer
    (let ((state (agent-shell-elicitation-tests--state (current-buffer)))
          sent)
      (cl-letf (((symbol-function 'acp-send-request)
                 (lambda (&rest args)
                   (funcall (plist-get args :on-sent)
                            '((:request-id . 41)))
                   (should
                    (agent-shell-elicitation--active-request-p state 41))
                   (agent-shell-elicitation--on-create-request
                    :state state
                    :acp-request
                    (agent-shell-elicitation-tests--request
                     :id "authorization"
                     :request-id 41
                     :schema
                     (agent-shell-elicitation-tests--boolean-schema)))
                   (funcall (plist-get args :on-success) '((ok . t)))))
                ((symbol-function 'agent-shell-elicitation--render)
                 #'ignore)
                ((symbol-function 'agent-shell--delete-fragment)
                 #'ignore)
                ((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (push (plist-get args :response) sent))))
        (agent-shell--send-request
         :state state
         :client 'client
         :request '((:method . "session/load")))
        (should-not (map-elt state :active-requests))
        (should-not (map-elt state :elicitations))
        (should
         (equal -32800
                (map-nested-elt
                 (seq-first sent) '(:error code))))))))

(ert-deftest agent-shell-elicitation-request-send-failure-cleans-owner-test ()
  "Remove request ownership when transmission fails after id assignment."
  (with-temp-buffer
    (let ((state (agent-shell-elicitation-tests--state (current-buffer))))
      (cl-letf (((symbol-function 'acp-send-request)
                 (lambda (&rest args)
                   (funcall (plist-get args :on-sent)
                            '((:request-id . 42)))
                   (error "Write failed"))))
        (should-error
         (agent-shell--send-request
          :state state
          :client 'client
          :request '((:method . "session/load")))))
      (should-not (map-elt state :active-requests)))))

(ert-deftest agent-shell-elicitation-scope-validation-test ()
  "Reject nonexistent, conflicting, expired, and malformed owners."
  (with-temp-buffer
    (let ((state (agent-shell-elicitation-tests--state (current-buffer)))
          responses)
      (agent-shell--save-tool-call
       state "done" '((:status . "completed")))
      (agent-shell--save-tool-call
       state "cancelled" '((:status . "cancelled")))
      (cl-letf (((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (push (plist-get args :response) responses))))
        (seq-do
         (lambda (request)
           (agent-shell-elicitation--on-create-request
            :state state :acp-request request))
         (list
          (agent-shell-elicitation-tests--request
           :id 1 :request-id 404
           :schema (agent-shell-elicitation-tests--boolean-schema))
          (agent-shell-elicitation-tests--request
           :id 2 :session-id "other"
           :schema (agent-shell-elicitation-tests--boolean-schema))
          (agent-shell-elicitation-tests--request
           :id 3 :tool-call-id "missing"
           :schema (agent-shell-elicitation-tests--boolean-schema))
          (agent-shell-elicitation-tests--request
           :id 4 :tool-call-id "done"
           :schema (agent-shell-elicitation-tests--boolean-schema))
          (agent-shell-elicitation-tests--request
           :id 6 :tool-call-id "cancelled"
           :schema (agent-shell-elicitation-tests--boolean-schema))
          '((id . 5)
            (method . "elicitation/create")
            (params
             (requestId . 1)
             (sessionId . "session-1")
             (mode . "form")
             (message . "bad")
             (requestedSchema
              (type . "object")
              (properties)))))))
      (should (= (length responses) 6))
      (seq-do
       (lambda (response)
         (should
          (equal -32602
                 (map-nested-elt response '(:error code)))))
       responses))))

(ert-deftest agent-shell-elicitation-cancellation-is-exactly-once-test ()
  "Settle an Agent-cancelled form once even if actions race."
  (with-temp-buffer
    (let ((state (agent-shell-elicitation-tests--state (current-buffer)))
          responses)
      (cl-letf (((symbol-function 'agent-shell-elicitation--render)
                 #'ignore)
                ((symbol-function 'agent-shell--delete-fragment)
                 #'ignore)
                ((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (push (plist-get args :response) responses))))
        (agent-shell-elicitation--on-create-request
         :state state
         :acp-request
         (agent-shell-elicitation-tests--request
          :id "form"
          :schema (agent-shell-elicitation-tests--boolean-schema)))
        (agent-shell-elicitation--on-cancel-request
         :state state :request-id "form")
        (agent-shell-elicitation--send-user-response
         :state state :request-id "form" :action "cancel"))
      (should (= (length responses) 1))
      (should
       (equal -32800
              (map-nested-elt
               (seq-first responses) '(:error code)))))))

(ert-deftest agent-shell-elicitation-lifecycle-ownership-test ()
  "Apply tool, session, shutdown, and connection lifetimes independently."
  (with-temp-buffer
    (let ((state (agent-shell-elicitation-tests--state (current-buffer)))
          responses)
      (agent-shell--save-tool-call state "running" '((:status . "pending")))
      (cl-letf (((symbol-function 'agent-shell-elicitation--render)
                 #'ignore)
                ((symbol-function 'agent-shell--delete-fragment)
                 #'ignore)
                ((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (push (plist-get args :response) responses))))
        (seq-do
         (lambda (request)
           (agent-shell-elicitation--on-create-request
            :state state :acp-request request))
         (list
          (agent-shell-elicitation-tests--request
           :id "tool" :tool-call-id "running"
           :schema (agent-shell-elicitation-tests--boolean-schema))
          (agent-shell-elicitation-tests--request
           :id "session"
           :schema (agent-shell-elicitation-tests--boolean-schema))))
        (agent-shell-elicitation--tool-finished
         :state state :tool-call-id "running")
        (should
         (agent-shell-elicitation--entry state "session"))
        ;; Finishing an unrelated Client-to-Agent request or prompt turn
        ;; does not own a session-scoped form.
        (agent-shell-elicitation--request-finished
         :state state :request-id 41)
        (should
         (agent-shell-elicitation--entry state "session"))
        (agent-shell-elicitation--session-finished
         :state state :session-id "session-1")
        (should-not (map-elt state :elicitations))
        (agent-shell-elicitation--on-create-request
         :state state
         :acp-request
         (agent-shell-elicitation-tests--request
          :id "shutdown"
          :schema (agent-shell-elicitation-tests--boolean-schema)))
        (agent-shell-elicitation--dismiss-all :state state)
        (should
         (equal "cancel"
                (map-nested-elt (seq-first responses)
                                '(:result action))))
        (agent-shell-elicitation--on-create-request
         :state state
         :acp-request
         (agent-shell-elicitation-tests--request
          :id "lost"
          :schema (agent-shell-elicitation-tests--boolean-schema)))
        (let ((before (length responses)))
          (agent-shell-elicitation--abandon-all :state state)
          (should (= before (length responses)))))
      (should-not (map-elt state :elicitations)))))

(ert-deftest agent-shell-elicitation-tool-notification-ends-owner-test ()
  "Settle a tool-scoped form when its tool reaches a terminal status."
  (with-temp-buffer
    (let ((state (agent-shell-elicitation-tests--state (current-buffer)))
          response)
      (agent-shell--save-tool-call
       state "tool" '((:status . "in_progress")))
      (cl-letf (((symbol-function 'agent-shell-elicitation--render)
                 #'ignore)
                ((symbol-function 'agent-shell--delete-fragment)
                 #'ignore)
                ((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (setq response (plist-get args :response))))
                ((symbol-function 'agent-shell--cancel-idle-timer)
                 #'ignore)
                ((symbol-function 'agent-shell--emit-event)
                 #'ignore)
                ((symbol-function 'agent-shell-make-tool-call-label)
                 (lambda (&rest _)
                   '((:status . "") (:title . ""))))
                ((symbol-function 'agent-shell--activity-group-id)
                 #'ignore)
                ((symbol-function 'agent-shell--update-fragment)
                 #'ignore)
                ((symbol-function 'agent-shell--refresh-activity-group-header)
                 #'ignore)
                ((symbol-function 'agent-shell--sync-activity-group-fold)
                 #'ignore))
        (agent-shell-elicitation--on-create-request
         :state state
         :acp-request
         (agent-shell-elicitation-tests--request
          :id "form"
          :tool-call-id "tool"
          :schema (agent-shell-elicitation-tests--boolean-schema)))
        (agent-shell--on-notification
         :state state
         :acp-notification
         '((method . "session/update")
           (params
            (update
             (sessionUpdate . "tool_call")
             (toolCallId . "tool")
             (status . "cancelled"))))))
      (should-not (map-elt state :elicitations))
      (should
       (equal -32800
              (map-nested-elt response '(:error code)))))))

(ert-deftest agent-shell-elicitation-session-change-ends-owner-test ()
  "Settle session-scoped forms before replacing the active session."
  (with-temp-buffer
    (let ((state (agent-shell-elicitation-tests--state (current-buffer)))
          response)
      (setq-local agent-shell--state state)
      (cl-letf (((symbol-function 'agent-shell-elicitation--render)
                 #'ignore)
                ((symbol-function 'agent-shell--delete-fragment)
                 #'ignore)
                ((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (setq response (plist-get args :response))))
                ((symbol-function 'agent-shell--save-config-options)
                 #'ignore))
        (agent-shell-elicitation--on-create-request
         :state state
         :acp-request
         (agent-shell-elicitation-tests--request
          :id "form"
          :schema (agent-shell-elicitation-tests--boolean-schema)))
        (agent-shell--set-session-from-response
         :acp-response nil
         :acp-session-id "session-2"))
      (should-not (map-elt state :elicitations))
      (should
       (equal -32800
              (map-nested-elt response '(:error code))))
      (should
       (equal "session-2"
              (map-nested-elt state '(:session :id)))))))

(ert-deftest agent-shell-elicitation-connection-subscription-cleans-ui-test ()
  "Abandon connection-owned forms when the ACP transport disappears."
  (with-temp-buffer
    (let ((state (agent-shell-elicitation-tests--state (current-buffer)))
          (client (acp-make-client :command "cat"))
          responses)
      (map-put! state :client client)
      (map-put! state :elicitations
                '(("form" . ((:request-id . "form")
                             (:message . "Question")
                             (:block-id . "request-form")))))
      (cl-letf (((symbol-function 'agent-shell--delete-fragment)
                 #'ignore)
                ((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (push (plist-get args :response) responses))))
        (agent-shell--subscribe-to-client-events :state state)
        (acp--notify-process-exit :client client :event "exited"))
      (should-not (map-elt state :elicitations))
      (should-not responses))))

(ert-deftest agent-shell-elicitation-shutdown-answers-before-close-test ()
  "Dismiss reverse requests before deliberate ACP shutdown closes transport."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (let ((state (agent-shell-elicitation-tests--state (current-buffer)))
          events)
      (setq-local agent-shell--state state)
      (map-put! state :elicitations
                '(("form" . ((:request-id . "form")
                             (:message . "Question")
                             (:block-id . "request-form")))))
      (cl-letf (((symbol-function 'agent-shell--delete-fragment)
                 #'ignore)
                ((symbol-function
                  'agent-shell-elicitation--render-settled)
                 #'ignore)
                ((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (push
                    (list 'response (plist-get args :response))
                    events)))
                ((symbol-function 'acp-shutdown)
                 (lambda (&rest _)
                   (push '(shutdown) events)))
                ((symbol-function 'agent-shell-heartbeat-stop)
                 #'ignore))
        (agent-shell--shutdown))
      (should
       (equal
        (seq-reverse events)
        '((response
           ((:request-id . "form")
            (:result . ((action . "cancel")))))
          (shutdown)))))))

(ert-deftest agent-shell-elicitation-turn-cancel-does-not-own-form-test ()
  "Do not infer that session/cancel cancels an independent elicitation."
  (with-temp-buffer
    (let ((state (agent-shell-elicitation-tests--state (current-buffer)))
          notifications
          responses)
      (map-put! state :elicitations
                '(("form" . ((:request-id . "form")
                             (:message . "Question")
                             (:scope . session)
                             (:scope-id . "session-1")))))
      (cl-letf (((symbol-function 'derived-mode-p)
                 (lambda (&rest _) t))
                ((symbol-function 'agent-shell--state)
                 (lambda () state))
                ((symbol-function 'acp-send-notification)
                 (lambda (&rest args)
                   (push (plist-get args :notification)
                         notifications)))
                ((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (push (plist-get args :response) responses))))
        (agent-shell-interrupt t))
      (should (= (length notifications) 1))
      (should-not responses)
      (should (map-elt state :elicitations)))))

(ert-deftest agent-shell-elicitation-status-is-not-inferred-test ()
  "A pending form alone does not establish that the Agent is blocked."
  (with-temp-buffer
    (let ((state (agent-shell-elicitation-tests--state (current-buffer))))
      (map-put! state :elicitations '(("form" . t)))
      (cl-letf (((symbol-function 'agent-shell--state)
                 (lambda () state))
                ((symbol-function 'shell-maker-busy)
                 (lambda () nil)))
        (should (eq (agent-shell-status) 'ready))))))

(ert-deftest agent-shell-elicitation-agent-cancel-notification-test ()
  "Route generic JSON-RPC cancellation to the reverse request."
  (with-temp-buffer
    (let ((state (agent-shell-elicitation-tests--state (current-buffer)))
          response)
      (map-put! state :elicitations
                '(("form" . ((:request-id . "form")
                             (:message . "Question")
                             (:block-id . "request-form")))))
      (cl-letf (((symbol-function 'agent-shell--delete-fragment)
                 #'ignore)
                ((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (setq response (plist-get args :response)))))
        (agent-shell--on-notification
         :state state
         :acp-notification
         '((method . "$/cancel_request")
           (params (requestId . "form")))))
      (should
       (equal -32800
              (map-nested-elt response '(:error code)))))))

(ert-deftest agent-shell-elicitation-handshake-advertises-form-test ()
  "Promise form handling in the initialize request."
  (with-temp-buffer
    (let ((agent-shell--state
           (agent-shell-elicitation-tests--state (current-buffer)))
          request)
      (cl-letf (((symbol-function
                  'agent-shell--update-bootstrapping-fragment)
                 #'ignore)
                ((symbol-function 'agent-shell--send-request)
                 (lambda (&rest args)
                   (setq request (plist-get args :request)))))
        (agent-shell--initiate-handshake
         :shell-buffer (current-buffer)
         :on-initiated #'ignore)
        (let ((form
               (map-nested-elt
                request
                '(:params clientCapabilities elicitation form))))
          (should (hash-table-p form))
          (should (equal 0 (hash-table-count form))))))))

(provide 'agent-shell-elicitation-tests)

;;; agent-shell-elicitation-tests.el ends here
