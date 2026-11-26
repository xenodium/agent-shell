;;; agent-shell-subagent-tests.el --- Tests for sub-agent sessions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for agent-shell-subagent.el.

;;; Code:

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-subagent)

(declare-function agent-shell-fakes--drive "agent-shell-fakes")
(declare-function agent-shell-fakes--settle "agent-shell-fakes")
(declare-function agent-shell-fakes-start-agent "agent-shell-fakes")

(ert-deftest agent-shell-subagent--add-client-capability-test ()
  "Advertise `clientCapabilities.subagents' as an empty object when enabled."
  ;; Opt-in.
  (should-not (default-toplevel-value 'agent-shell-subagent-sessions-enabled))
  (let ((request (acp-make-initialize-request :protocol-version 1)))
    (let ((agent-shell-subagent-sessions-enabled t))
      (should (equal (json-serialize
                      (map-nested-elt (agent-shell-subagent--add-client-capability request)
                                      '(:params clientCapabilities subagents)))
                     "{}"))
      (should (assq 'subagents
                    (map-nested-elt (agent-shell-subagent--add-client-capability request)
                                    '(:params clientCapabilities))))
      ;; Existing capabilities are kept.
      (should (map-nested-elt (agent-shell-subagent--add-client-capability request)
                              '(:params clientCapabilities fs))))
    (let ((agent-shell-subagent-sessions-enabled nil))
      (should (equal (agent-shell-subagent--add-client-capability request) request)))))

(ert-deftest agent-shell-subagent--apply-update-test ()
  "Patch records as per spec: absent fields unchanged, nulls clear."
  (let ((record (agent-shell-subagent--make-record :session-id "c1" :parent-session-id "p1")))
    (agent-shell-subagent--apply-update
     record '((sessionId . "c1")
              (title . "Investigator")
              (description . "Looks into things")
              (capabilities (cancel))
              (state (state . "running"))))
    (should (equal (map-elt record :title) "Investigator"))
    (should (equal (map-elt record :description) "Looks into things"))
    (should (map-elt record :can-cancel))
    (should (equal (map-elt record :state) "running"))
    ;; Absent fields are left as they were.
    (agent-shell-subagent--apply-update
     record '((sessionId . "c1")
              (state (state . "idle") (stopReason . "error")
                     (error (code . -32000) (message . "Boom")))))
    (should (equal (map-elt record :title) "Investigator"))
    (should (map-elt record :can-cancel))
    (should (equal (map-elt record :state) "idle"))
    (should (equal (map-elt record :stop-reason) "error"))
    (should (equal (map-elt record :error) "Boom"))
    ;; Present nulls clear.
    (agent-shell-subagent--apply-update
     record '((sessionId . "c1") (description) (capabilities)))
    (should-not (map-elt record :description))
    (should-not (map-elt record :can-cancel))))

(ert-deftest agent-shell-subagent--status-text-test ()
  "Describe sub-agent work states."
  (should (equal (agent-shell-subagent--status-text '((:state . nil))) "started"))
  (should (equal (agent-shell-subagent--status-text '((:state . "running"))) "running"))
  (should (equal (agent-shell-subagent--status-text '((:state . "requires_action")))
                 "needs input"))
  (should (equal (agent-shell-subagent--status-text '((:state . "idle"))) "done"))
  (should (equal (agent-shell-subagent--status-text
                  '((:state . "idle") (:stop-reason . "end_turn")))
                 "done"))
  (should (equal (agent-shell-subagent--status-text
                  '((:state . "idle") (:stop-reason . "cancelled")))
                 "cancelled"))
  (should (equal (agent-shell-subagent--status-text
                  '((:state . "idle") (:stop-reason . "error")))
                 "failed"))
  (should (equal (agent-shell-subagent--status-text
                  '((:state . "idle") (:stop-reason . "max_tokens")))
                 "max tokens"))
  (should (equal (agent-shell-subagent--status-text '((:state . "_paused"))) "paused")))

(ert-deftest agent-shell-subagent--record-message-test ()
  "Track inter-session messages, and sub-agent tasks and results from them."
  (let ((state (agent-shell--make-state)))
    (agent-shell-subagent--register
     :state state :parent-session-id "p1"
     :update '((sessionUpdate . "subagent_update") (sessionId . "c1")))
    ;; The parent's outgoing view of the task.
    (agent-shell-subagent--record-message
     :state state :session-id "p1"
     :update '((sessionUpdate . "session_message")
               (messageId . "m1")
               (senderSessionId . "p1")
               (recipientSessionId . "c1")
               (content . [((type . "text") (text . "Find the bug"))])))
    (should (equal (map-elt (agent-shell-subagent--record state "c1") :task) "Find the bug"))
    ;; The sub-agent's answer, upserted then streamed.
    (agent-shell-subagent--record-message
     :state state :session-id "p1"
     :update '((sessionUpdate . "session_message")
               (messageId . "m2")
               (senderSessionId . "c1")
               (recipientSessionId . "p1")
               (content . [((type . "text") (text . "Found"))])))
    (agent-shell-subagent--record-message
     :state state :session-id "p1"
     :update '((sessionUpdate . "session_message_chunk")
               (messageId . "m2")
               (content (type . "text") (text . " it"))))
    (should (equal (map-elt (agent-shell-subagent--record state "c1") :result) "Found it"))
    (should (equal (map-elt (agent-shell-subagent--message state "p1" "m2") :text) "Found it"))
    ;; Message ids are scoped to their session.
    (should-not (agent-shell-subagent--message state "c1" "m2"))
    ;; Delegating onwards to a nested sub-agent is not a result.
    (agent-shell-subagent--register
     :state state :parent-session-id "c1"
     :update '((sessionUpdate . "subagent_update") (sessionId . "c2")))
    (agent-shell-subagent--record-message
     :state state :session-id "c1"
     :update '((sessionUpdate . "session_message")
               (messageId . "m3")
               (senderSessionId . "c1")
               (recipientSessionId . "c2")
               (content . [((type . "text") (text . "Read the logs"))])))
    (should (equal (map-elt (agent-shell-subagent--record state "c2") :task) "Read the logs"))
    (should (equal (map-elt (agent-shell-subagent--record state "c1") :result) "Found it"))
    ;; The nested sub-agent reporting back is its result, not a task
    ;; for its parent.
    (agent-shell-subagent--record-message
     :state state :session-id "c1"
     :update '((sessionUpdate . "session_message")
               (messageId . "m4")
               (senderSessionId . "c2")
               (recipientSessionId . "c1")
               (content . [((type . "text") (text . "Logs read"))])))
    (should (equal (map-elt (agent-shell-subagent--record state "c2") :result) "Logs read"))
    (should (equal (map-elt (agent-shell-subagent--record state "c1") :task) "Find the bug"))
    ;; Nor is a message without a known sender.
    (agent-shell-subagent--record-message
     :state state :session-id "c2"
     :update '((sessionUpdate . "session_message")
               (messageId . "m5")
               (recipientSessionId . "c2")
               (content . [((type . "text") (text . "Who knows"))])))
    (should (equal (map-elt (agent-shell-subagent--record state "c2") :task) "Read the logs"))
    ;; Message ids that only differ in where the delimiter would go
    ;; stay apart.
    (agent-shell-subagent--record-message
     :state state :session-id "a/b"
     :update '((sessionUpdate . "session_message") (messageId . "c")
               (content . [((type . "text") (text . "first"))])))
    (agent-shell-subagent--record-message
     :state state :session-id "a"
     :update '((sessionUpdate . "session_message") (messageId . "b/c")
               (content . [((type . "text") (text . "second"))])))
    (should (equal (map-elt (agent-shell-subagent--message state "a/b" "c") :text) "first"))))

(ert-deftest agent-shell-subagent--candidates-test ()
  "List sub-agents for picking, nested ones indented, duplicates told apart."
  (let ((state (agent-shell--make-state)))
    (agent-shell-subagent--register
     :state state :parent-session-id "p1"
     :update '((sessionId . "c1") (title . "Reviewer") (state (state . "running"))))
    (agent-shell-subagent--register
     :state state :parent-session-id "c1"
     :update '((sessionId . "c2") (title . "Reader")))
    (agent-shell-subagent--register
     :state state :parent-session-id "p1"
     :update '((sessionId . "c3") (title . "Reviewer") (state (state . "running"))))
    (should (equal (agent-shell-subagent--candidates state)
                   '(("Reviewer  [running]" . "c1")
                     ("  Reader  [started]" . "c2")
                     ("Reviewer  [running]  <c3>" . "c3"))))))

;;; Listing

(ert-deftest agent-shell-subagent--tree-records-test ()
  "List sub-agents parent first, siblings oldest first, orphans last."
  (let ((state (agent-shell--make-state)))
    (map-put! (map-elt state :session) :id "p1")
    (agent-shell-subagent--register
     :state state :parent-session-id "p1" :update '((sessionId . "c1")))
    (agent-shell-subagent--register
     :state state :parent-session-id "elsewhere" :update '((sessionId . "orphan")))
    (agent-shell-subagent--register
     :state state :parent-session-id "p1" :update '((sessionId . "c3")))
    (agent-shell-subagent--register
     :state state :parent-session-id "c1" :update '((sessionId . "c2")))
    (should (equal (mapcar (lambda (record) (map-elt record :session-id))
                           (agent-shell-subagent--tree-records state))
                   '("c1" "c2" "c3" "orphan")))))

;;; End to end

(defconst agent-shell-subagent-tests--parent "parent-session")

(defun agent-shell-subagent-tests--update (session-id update)
  "Return an incoming `session/update' traffic item for SESSION-ID with UPDATE."
  `((:direction . incoming) (:kind . notification)
    (:object (jsonrpc . "2.0") (method . "session/update")
             (params (sessionId . ,session-id) (update . ,update)))))

(defun agent-shell-subagent-tests--traffic ()
  "Return traffic for a turn delegating to one sub-agent session."
  (let ((parent agent-shell-subagent-tests--parent))
    (list
     `((:direction . outgoing) (:kind . request)
       (:object (jsonrpc . "2.0") (method . "session/prompt") (id . 1)
                (params (sessionId . ,parent)
                        (prompt . [((type . "text") (text . "Investigate the tests"))]))))
     (agent-shell-subagent-tests--update
      parent '((sessionUpdate . "subagent_update")
               (sessionId . "child-1")
               (title . "Test investigator")
               (description . "Investigates failing tests")
               (capabilities (cancel))
               (state (state . "running"))))
     (agent-shell-subagent-tests--update
      parent '((sessionUpdate . "session_message")
               (messageId . "to-child")
               (senderSessionId . "parent-session")
               (recipientSessionId . "child-1")
               (content . [((type . "text") (text . "Look into the flaky test"))])))
     (agent-shell-subagent-tests--update
      "child-1" '((sessionUpdate . "session_message")
                  (messageId . "from-parent")
                  (senderSessionId . "parent-session")
                  (recipientSessionId . "child-1")
                  (content . [((type . "text") (text . "Look into the flaky test"))])))
     (agent-shell-subagent-tests--update
      "child-1" '((sessionUpdate . "tool_call")
                  (toolCallId . "child-tool-1")
                  (title . "Read child-only-file.el")
                  (kind . "read")
                  (status . "completed")))
     (agent-shell-subagent-tests--update
      "child-1" '((sessionUpdate . "agent_message_chunk")
                  (messageId . "child-answer")
                  (content (type . "text") (text . "Child-only reasoning text"))))
     (agent-shell-subagent-tests--update
      parent '((sessionUpdate . "subagent_update")
               (sessionId . "child-1")
               (capabilities)
               (state (state . "idle") (stopReason . "end_turn"))))
     (agent-shell-subagent-tests--update
      parent '((sessionUpdate . "session_message")
               (messageId . "from-child")
               (senderSessionId . "child-1")
               (recipientSessionId . "parent-session")
               (content . [((type . "text") (text . "The test races on a timer"))])))
     (agent-shell-subagent-tests--update
      parent '((sessionUpdate . "agent_message_chunk")
               (messageId . "parent-answer")
               (content (type . "text") (text . "Parent summary text"))))
     `((:direction . incoming) (:kind . response)
       (:object (jsonrpc . "2.0") (id . 1) (result (stopReason . "end_turn")))))))

(defmacro agent-shell-subagent-tests--with-shell (shell &rest body)
  "Run BODY with SHELL bound to a fake shell that replayed sub-agent traffic."
  (declare (indent 1))
  `(agent-shell-subagent-tests--with-traffic ,shell (agent-shell-subagent-tests--traffic)
     ,@body))

(defmacro agent-shell-subagent-tests--with-traffic (shell traffic &rest body)
  "Run BODY with SHELL bound to a fake shell that replayed TRAFFIC."
  (declare (indent 2))
  `(let* ((agent-shell-subagent-sessions-enabled t)
          (agent-shell-transcript-file-path-function nil)
          (agent-shell-show-welcome-message nil)
          (agent-shell-display-action '(display-buffer-same-window))
          (default-directory (file-name-as-directory temporary-file-directory))
          (agent-shell-cwd-function (lambda () default-directory))
          (,shell (agent-shell-fakes-start-agent ,traffic)))
     (unwind-protect
         (progn
           (agent-shell-fakes--settle)
           (agent-shell-fakes--drive
            (map-elt (buffer-local-value 'agent-shell--state ,shell) :client)
            ,shell)
           ,@body)
       (when (buffer-live-p ,shell)
         (let ((kill-buffer-query-functions nil))
           (kill-buffer ,shell))))))

(ert-deftest agent-shell-subagent-end-to-end-test ()
  "Route sub-agent traffic away from the parent and into an openable buffer."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell)))
      ;; Bookkeeping.
      (should (equal (map-keys (map-elt state :subagents)) '("child-1")))
      (let ((record (agent-shell-subagent--record state "child-1")))
        (should (equal (map-elt record :title) "Test investigator"))
        (should (equal (map-elt record :state) "idle"))
        (should-not (map-elt record :can-cancel))
        (should (equal (map-elt record :task) "Look into the flaky test"))
        (should (equal (map-elt record :result) "The test races on a timer"))
        (should-not (map-elt record :buffer)))
      (should (equal (buffer-local-value 'agent-shell-subagent-count shell) 1))
      (should (equal (buffer-local-value 'agent-shell-subagent-running-count shell) 0))
      (should (equal (agent-shell-subagent--indicator state) "1 sub-agent"))
      ;; The parent shows a card, but none of the child's own activity.
      (with-current-buffer shell
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-search "Test investigator" text))
          (should (string-search "Parent summary text" text))
          (should-not (string-search "child-only-file.el" text))
          (should-not (string-search "Child-only reasoning text" text))))
      ;; Opening replays the child's traffic into its own buffer.
      (let ((child (agent-shell-subagent--ensure-buffer :state state :session-id "child-1")))
        (unwind-protect
            (progn
              (should (agent-shell-subagent-p child))
              (should-not (agent-shell-subagent-p shell))
              (should (eq (map-elt (agent-shell-subagent--record state "child-1") :buffer)
                          child))
              (should (eq (agent-shell-subagent--ensure-buffer :state state :session-id "child-1")
                          child))
              (should (equal (map-nested-elt (buffer-local-value 'agent-shell--state child)
                                             '(:session :id))
                             "child-1"))
              ;; Shares the parent's client.
              (should (eq (map-elt (buffer-local-value 'agent-shell--state child) :client)
                          (map-elt state :client)))
              (should (string-search "Test investigator" (buffer-name child)))
              (with-current-buffer child
                (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                  (should (string-search "Task from parent" text))
                  (should (string-search "Look into the flaky test" text))
                  (should (string-search "child-only-file.el" text))
                  (should (string-search "Child-only reasoning text" text))
                  (should-not (string-search "Parent summary text" text)))
                ;; Read-only, and not cancellable once idle.
                (should-error (agent-shell-submit) :type 'user-error)
                (should-error (agent-shell-interrupt t) :type 'user-error)
                (should (equal (agent-shell-subagent--indicator agent-shell--state)
                               "sub-agent: done")))
              ;; Kept out of shell lists.
              (should (memq shell (agent-shell-buffers)))
              (should-not (memq child (agent-shell-buffers)))
              ;; Back to the parent.
              (with-current-buffer child
                (agent-shell-subagent-parent))
              (should (eq (window-buffer (selected-window)) shell))
              ;; Killing the child keeps the parent's client and the record.
              (kill-buffer child)
              (should (map-elt state :client))
              (should-not (map-elt (agent-shell-subagent--record state "child-1") :buffer))
              ;; Reopening replays again.
              (setq child (agent-shell-subagent--ensure-buffer :state state :session-id "child-1"))
              (with-current-buffer child
                (should (string-search "Child-only reasoning text"
                                       (buffer-substring-no-properties (point-min) (point-max)))))
              ;; Killing the parent takes its sub-agents along.
              (let ((kill-buffer-query-functions nil))
                (kill-buffer shell))
              (should-not (buffer-live-p child)))
          (when (buffer-live-p child)
            (kill-buffer child)))))))

(ert-deftest agent-shell-subagent-live-traffic-test ()
  "Render traffic arriving after a sub-agent's buffer is open live."
  (agent-shell-subagent-tests--with-shell shell
    (let* ((state (buffer-local-value 'agent-shell--state shell))
           (child (agent-shell-subagent--ensure-buffer :state state :session-id "child-1")))
      (with-current-buffer shell
        (agent-shell-subagent--route-notification
         :state state
         :acp-notification
         `((method . "session/update")
           (params (sessionId . "child-1")
                   (update (sessionUpdate . "agent_message_chunk")
                           (messageId . "late")
                           (content (type . "text") (text . "Late child text")))))))
      (agent-shell-fakes--settle)
      (with-current-buffer child
        (should (string-search "Late child text"
                               (buffer-substring-no-properties (point-min) (point-max)))))
      (with-current-buffer shell
        (should-not (string-search "Late child text"
                                   (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest agent-shell-subagent-route-request-test ()
  "Handle a sub-agent's requests in the main shell, without opening it."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          handled-in)
      (cl-letf (((symbol-function 'agent-shell--on-request)
                 (lambda (&rest args)
                   (setq handled-in (map-elt (plist-get args :state) :buffer)))))
        (with-current-buffer shell
          (should (agent-shell-subagent--route-request
                   :state state
                   :acp-request '((id . 9)
                                  (method . "fs/read_text_file")
                                  (params (sessionId . "child-1") (path . "/tmp/x")))))
          (should (eq handled-in shell))
          (should-not (map-elt (agent-shell-subagent--record state "child-1") :buffer))
          ;; Requests for the main session are left alone.
          (should-not (agent-shell-subagent--route-request
                       :state state
                       :acp-request `((id . 10)
                                      (method . "fs/read_text_file")
                                      (params (sessionId . ,agent-shell-subagent-tests--parent)
                                              (path . "/tmp/x"))))))))))

;;; Permissions

(cl-defun agent-shell-subagent-tests--request-permission (shell request-id
                                                                &key (session-id "child-1")
                                                                tool-call)
  "Route a sub-agent permission request with REQUEST-ID through SHELL.

SESSION-ID is the sub-agent's.  TOOL-CALL replaces the request's tool
call."
  (with-current-buffer shell
    (agent-shell-subagent--route-request
     :state agent-shell--state
     :acp-request `((id . ,request-id)
                    (method . "session/request_permission")
                    (params (sessionId . ,session-id)
                            (toolCall . ,(or tool-call
                                             `((toolCallId . ,(format "perm-tool-%s" request-id))
                                               (title . "Run make test")
                                               (kind . "execute"))))
                            (options . [((optionId . "allow") (name . "Allow") (kind . "allow_once"))
                                        ((optionId . "reject") (name . "Reject") (kind . "reject_once"))]))))))

(defun agent-shell-subagent-tests--route (shell session-id update)
  "Route `session/update' UPDATE sent on SESSION-ID through SHELL."
  (with-current-buffer shell
    (agent-shell-subagent--route-notification
     :state agent-shell--state
     :acp-notification (map-elt (agent-shell-subagent-tests--update session-id update)
                                :object))))

(defun agent-shell-subagent-tests--permission-key (shell key &optional after)
  "Return the command KEY runs on SHELL's permission buttons.

AFTER, if given, is text preceding the dialog's buttons."
  (with-current-buffer shell
    (save-excursion
      (goto-char (point-min))
      (when after
        (search-forward after))
      (search-forward "Allow (y)")
      (lookup-key (get-text-property (1- (point)) 'keymap) (kbd key)))))

(defun agent-shell-subagent-tests--text (buffer)
  "Return BUFFER's text."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defmacro agent-shell-subagent-tests--with-responses (responses &rest body)
  "Run BODY collecting sent permission responses into RESPONSES.

Each is (REQUEST-ID . OUTCOME), newest first."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'acp-send-response)
              (lambda (&rest args)
                (let ((response (plist-get args :response)))
                  (push (cons (map-elt response :request-id)
                              (map-nested-elt response '(:result outcome outcome)))
                        ,responses))))
             ((symbol-function 'acp-send-notification) #'ignore))
     ,@body))

(defmacro agent-shell-subagent-tests--with-notifications (notifications &rest body)
  "Run BODY collecting sent notification methods into NOTIFICATIONS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'acp-send-notification)
              (lambda (&rest args)
                (push (cons (map-elt (plist-get args :notification) :method)
                            (map-nested-elt (plist-get args :notification)
                                            '(:params sessionId)))
                      ,notifications))))
     ,@body))

(ert-deftest agent-shell-subagent-permission-in-main-shell-test ()
  "Ask a sub-agent's permission in the main shell, with a link to the sub-agent."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (responses nil))
      (agent-shell-subagent-tests--with-responses responses
        (agent-shell-subagent-tests--request-permission shell 41)
        ;; Shown in the main shell, tagged with its sub-agent.
        (should-not (map-elt (agent-shell-subagent--record state "child-1") :buffer))
        (should (equal (map-nested-elt state '(:tool-calls ("child-1" . "perm-tool-41")
                                                           :subagent-session-id))
                       "child-1"))
        (should (agent-shell--permission-pending-p :shell-buffer shell))
        (with-current-buffer shell
          (should (string-search "From sub-agent: Test investigator"
                                 (buffer-substring-no-properties (point-min) (point-max)))))
        ;; Opening and killing the sub-agent leaves the request alone.
        (funcall (agent-shell-subagent-tests--permission-key shell "o"))
        (let ((child (map-elt (agent-shell-subagent--record state "child-1") :buffer)))
          (should (buffer-live-p child))
          (kill-buffer child))
        (should (agent-shell--permission-pending-p :shell-buffer shell))
        (should-not responses)
        ;; Answered from the main shell.
        (funcall (agent-shell-subagent-tests--permission-key shell "y"))
        (should (equal responses '((41 . "selected"))))
        (should-not (agent-shell--permission-pending-p :shell-buffer shell))
        (with-current-buffer shell
          (should-not (string-search "From sub-agent"
                                     (buffer-substring-no-properties (point-min) (point-max)))))
        ;; The entry goes once the sub-agent's tool call settles.
        (should (map-nested-elt state '(:tool-calls ("child-1" . "perm-tool-41"))))
        (agent-shell-subagent--route-notification
         :state state
         :acp-notification (map-elt (agent-shell-subagent-tests--update
                                     "child-1" '((sessionUpdate . "tool_call_update")
                                                 (toolCallId . "perm-tool-41")
                                                 (status . "completed")))
                                    :object))
        (should-not (map-nested-elt state '(:tool-calls ("child-1" . "perm-tool-41"))))))))

(ert-deftest agent-shell-subagent-cancel-requests-test ()
  "Cancel a sub-agent's pending requests once it stops or is interrupted."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (responses nil))
      (agent-shell-subagent-tests--with-responses responses
        ;; The sub-agent stopping.
        (agent-shell-subagent-tests--request-permission shell 42)
        (agent-shell-subagent--route-notification
         :state state
         :acp-notification (map-elt (agent-shell-subagent-tests--update
                                     agent-shell-subagent-tests--parent
                                     '((sessionUpdate . "subagent_update")
                                       (sessionId . "child-1")
                                       (state (state . "idle") (stopReason . "cancelled"))))
                                    :object))
        (should (equal responses '((42 . "cancelled"))))
        (should-not (map-nested-elt state '(:tool-calls ("child-1" . "perm-tool-42"))))
        ;; Interrupting the sub-agent from its own buffer.
        (setq responses nil)
        (agent-shell-subagent-tests--request-permission shell 43)
        (map-put! (agent-shell-subagent--record state "child-1") :can-cancel t)
        (with-current-buffer (agent-shell-subagent--ensure-buffer
                              :state state :session-id "child-1")
          (agent-shell-interrupt t))
        (should (equal responses '((43 . "cancelled"))))
        (should-not (agent-shell--permission-pending-p :shell-buffer shell))))))

(ert-deftest agent-shell-subagent-reject-leaves-main-turn-test ()
  "Reject a sub-agent's request alone, leaving the main turn and siblings be."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (responses nil)
          (notifications nil))
      (agent-shell-subagent-tests--route
       shell agent-shell-subagent-tests--parent
       '((sessionUpdate . "subagent_update") (sessionId . "child-2")
         (title . "Sibling") (state (state . "running"))))
      (agent-shell-subagent-tests--with-responses responses
        (agent-shell-subagent-tests--with-notifications notifications
          (agent-shell-subagent-tests--request-permission shell 51)
          (agent-shell-subagent-tests--request-permission shell 52 :session-id "child-2")
          ;; Reject shares the interrupt key.
          (funcall (agent-shell-subagent-tests--permission-key
                    shell "C-c C-c" "From sub-agent: Test investigator"))
          (should (equal responses '((51 . "selected"))))
          (should-not notifications)
          ;; Interrupting the main session cancels its own turn only.
          (with-current-buffer shell
            (agent-shell-interrupt t))
          (should (equal responses '((51 . "selected"))))
          (should (equal notifications
                         `(("session/cancel" . ,agent-shell-subagent-tests--parent))))
          (should (map-nested-elt state '(:tool-calls ("child-2" . "perm-tool-52")
                                                      :permission-request-id))))))))

(ert-deftest agent-shell-subagent-permission-details-test ()
  "Fill in a permission request naming only a tool call reported earlier."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell)))
      (agent-shell-subagent-tests--route
       shell "child-1"
       '((sessionUpdate . "tool_call") (toolCallId . "build")
         (title . "Run build") (kind . "execute") (status . "pending")
         (rawInput (command . "make test"))))
      (cl-letf (((symbol-function 'acp-send-response) #'ignore))
        (agent-shell-subagent-tests--request-permission
         shell 61 :tool-call '((toolCallId . "build"))))
      (let ((tool-call (map-nested-elt state '(:tool-calls ("child-1" . "build")))))
        (should (equal (map-elt tool-call :title) "Run build"))
        (should (equal (map-nested-elt tool-call '(:raw-input command)) "make test"))
        (should (equal (map-elt tool-call :subagent-session-id) "child-1")))
      (should (string-search "make test" (agent-shell-subagent-tests--text shell))))))

(ert-deftest agent-shell-subagent-settle-answers-pending-test ()
  "Answer a still pending request once its tool call fails, exactly once."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (responses nil))
      (agent-shell-subagent-tests--with-responses responses
        (agent-shell-subagent-tests--request-permission shell 71)
        (agent-shell-subagent-tests--route
         shell "child-1"
         '((sessionUpdate . "tool_call_update") (toolCallId . "perm-tool-71")
           (status . "failed")))
        (should (equal responses '((71 . "cancelled"))))
        (should-not (map-nested-elt state '(:tool-calls ("child-1" . "perm-tool-71"))))
        (should-not (string-search "From sub-agent" (agent-shell-subagent-tests--text shell)))
        ;; Stopping afterwards has nothing left to answer.
        (agent-shell-subagent-tests--route
         shell agent-shell-subagent-tests--parent
         '((sessionUpdate . "subagent_update") (sessionId . "child-1")
           (state (state . "idle") (stopReason . "cancelled"))))
        (should (equal responses '((71 . "cancelled"))))))))

(ert-deftest agent-shell-subagent-metadata-keeps-requests-test ()
  "Keep pending requests through updates that don't report a cancellation."
  (agent-shell-subagent-tests--with-shell shell
    (let ((responses nil))
      (agent-shell-subagent-tests--with-responses responses
        ;; Already idle from the traffic.
        (agent-shell-subagent-tests--request-permission shell 81)
        (agent-shell-subagent-tests--route
         shell agent-shell-subagent-tests--parent
         '((sessionUpdate . "subagent_update") (sessionId . "child-1") (title . "Renamed")))
        (agent-shell-subagent-tests--route
         shell agent-shell-subagent-tests--parent
         '((sessionUpdate . "subagent_update") (sessionId . "child-1")
           (state (state . "idle") (stopReason . "end_turn"))))
        (should-not responses)
        (should (agent-shell--permission-pending-p :shell-buffer shell))))))

(ert-deftest agent-shell-subagent-permission-across-turns-test ()
  "Remove an answered dialog even when answered in a later turn."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (responses nil))
      (agent-shell-subagent-tests--with-responses responses
        (agent-shell-subagent-tests--request-permission shell 91)
        (map-put! state :request-count (1+ (map-elt state :request-count)))
        (funcall (agent-shell-subagent-tests--permission-key shell "y"))
        (should (equal responses '((91 . "selected"))))
        (should-not (string-search "From sub-agent" (agent-shell-subagent-tests--text shell)))))))

(ert-deftest agent-shell-subagent-tool-call-keys-test ()
  "Keep a sub-agent's tool calls apart from the main session's own."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell)))
      (agent-shell--save-tool-call state "child-1/build" '((:title . "Main build")
                                                           (:status . "in_progress")))
      (cl-letf (((symbol-function 'acp-send-response) #'ignore))
        (agent-shell-subagent-tests--request-permission
         shell 95 :tool-call '((toolCallId . "build") (title . "Child build"))))
      (agent-shell-subagent-tests--route
       shell "child-1"
       '((sessionUpdate . "tool_call_update") (toolCallId . "build") (status . "completed")))
      (should (equal (map-nested-elt state '(:tool-calls "child-1/build" :title)) "Main build"))
      (should-not (map-nested-elt state '(:tool-calls ("child-1" . "build")))))))

(ert-deftest agent-shell-subagent-elicitation-test ()
  "Attribute a sub-agent's question, and keep it answerable past the turn."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (agent-shell-elicitation--experimental-feature-enabled t)
          (responses nil)
          (pending-at-event nil))
      (agent-shell-subscribe-to
       :shell-buffer shell
       :event 'elicitation-request
       :on-event (lambda (_event)
                   (setq pending-at-event
                         (agent-shell-subagent--pending-request-count state "child-1"))))
      (agent-shell-subagent-tests--with-responses responses
        (with-current-buffer shell
          (agent-shell-subagent--route-request
           :state state
           :acp-request '((id . 101)
                          (method . "elicitation/create")
                          (params (sessionId . "child-1")
                                  (mode . "form")
                                  (message . "Which test?")
                                  (requestedSchema (type . "object")
                                                   (properties (name (type . "string"))))))))
        (should (equal pending-at-event 1))
        (should (string-search "From sub-agent: Test investigator"
                               (agent-shell-subagent-tests--text shell)))
        ;; The main shell's turn ends, the question stays.
        (agent-shell-elicitation--clear :state state)
        (should (equal (agent-shell-subagent--pending-request-count state "child-1") 1))
        ;; Interrupting the main shell leaves it too.
        (with-current-buffer shell
          (agent-shell-interrupt t))
        (should-not responses)
        (should (agent-shell-elicitation--respond :state state :id 101 :action 'decline))
        (should (length= responses 1))
        (should-not (agent-shell-elicitation--respond :state state :id 101 :action 'decline))
        (should (length= responses 1))))))

;;; Restore

(ert-deftest agent-shell-subagent-restore-history-test ()
  "Show replayed sub-agent state as history, granting no live controls."
  (let ((state (agent-shell--make-state)))
    (map-put! (map-elt state :session) :id "p1")
    (map-put! state :active-requests '(((:method . "session/load"))))
    (agent-shell-subagent--route-notification
     :state state
     :acp-notification (map-elt (agent-shell-subagent-tests--update
                                 "p1" '((sessionUpdate . "subagent_update")
                                        (sessionId . "c1")
                                        (capabilities (cancel))
                                        (state (state . "running"))))
                                :object))
    (let ((record (agent-shell-subagent--record state "c1")))
      (should-not (map-elt record :can-cancel))
      (should-not (agent-shell-subagent--working-p record))
      (should (equal (agent-shell-subagent--status-text record) "unconfirmed"))
      ;; The load completes, and the agent reports current state.
      (map-put! state :active-requests nil)
      (agent-shell-subagent--route-notification
       :state state
       :acp-notification (map-elt (agent-shell-subagent-tests--update
                                   "p1" '((sessionUpdate . "subagent_update")
                                          (sessionId . "c1")
                                          (capabilities (cancel))
                                          (state (state . "running"))))
                                  :object))
      (should (map-elt record :can-cancel))
      (should (agent-shell-subagent--working-p record)))
    ;; A failed restore leaves nothing behind.
    (agent-shell-subagent--discard-restore state)
    (should-not (agent-shell-subagent--records state))))

(ert-deftest agent-shell-subagent-ensure-state-test ()
  "Clean up a shell whose state predates sub-agents."
  (let ((state (list (cons :buffer nil)
                     (cons :session (list (cons :id "p1"))))))
    (agent-shell-subagent--clean-up state)
    (should (assq :subagents state))))

;;; Read-only buffers

(ert-deftest agent-shell-subagent-read-only-test ()
  "Refuse session requests from a sub-agent's buffer, and reload from memory."
  (agent-shell-subagent-tests--with-shell shell
    (let* ((state (buffer-local-value 'agent-shell--state shell))
           (child (agent-shell-subagent--ensure-buffer :state state :session-id "child-1"))
           (sent nil))
      (cl-letf (((symbol-function 'acp-send-request)
                 (lambda (&rest args) (push args sent))))
        (with-current-buffer child
          (should-error (agent-shell--send-request
                         :state agent-shell--state
                         :client (map-elt agent-shell--state :client)
                         :request (acp-make-session-set-config-option-request
                                   :session-id "child-1" :config-id "model" :value "x"))
                        :type 'user-error)
          (should-error (agent-shell-fork) :type 'user-error)
          (should-error (agent-shell-restart) :type 'user-error))
        (should-not sent)
        (let ((reloaded (with-current-buffer child
                          (agent-shell-reload))))
          (should-not (buffer-live-p child))
          (should (buffer-live-p reloaded))
          (should (agent-shell-subagent-p reloaded))
          (should (string-search "Child-only reasoning text"
                                 (agent-shell-subagent-tests--text reloaded)))
          (should-not sent)
          (kill-buffer reloaded))))))

;;; Live updates

(ert-deftest agent-shell-subagent-live-card-test ()
  "Refresh the parent's card when a result arrives on the sub-agent's session."
  (agent-shell-subagent-tests--with-shell shell
    (let ((events nil))
      (agent-shell-subscribe-to
       :shell-buffer shell
       :event 'subagent-update
       :on-event (lambda (event) (push (map-elt event :data) events)))
      (agent-shell-subagent-tests--route
       shell "child-1"
       '((sessionUpdate . "session_message") (messageId . "late-result")
         (senderSessionId . "child-1") (recipientSessionId . "parent-session")
         (content . [((type . "text") (text . "Second opinion"))])))
      (should (string-search "Second opinion" (agent-shell-subagent-tests--text shell)))
      (should (equal (map-nested-elt (car events) '(:subagent :result)) "Second opinion")))))

(ert-deftest agent-shell-subagent-open-test ()
  "Open a sub-agent in its resolved directory, without a new client."
  (agent-shell-subagent-tests--with-shell shell
    (let* ((state (buffer-local-value 'agent-shell--state shell))
           (local (file-name-as-directory (make-temp-file "subagent-cwd" t)))
           (agent-shell-path-resolver-function
            (lambda (path)
              (if (equal path "/container/work") local path)))
           (client-makers 0)
           (config (map-elt state :agent-config))
           (client-maker (map-elt config :client-maker)))
      (unwind-protect
          (progn
            (map-put! config :client-maker
                      (lambda (buffer)
                        (setq client-makers (1+ client-makers))
                        (funcall client-maker buffer)))
            (agent-shell-subagent-tests--route
             shell agent-shell-subagent-tests--parent
             '((sessionUpdate . "subagent_update") (sessionId . "child-1")
               (cwd . "/container/work")))
            (let ((child (agent-shell-subagent--ensure-buffer :state state :session-id "child-1")))
              (should (equal (buffer-local-value 'default-directory child) local))
              (should (equal client-makers 0))
              (kill-buffer child)))
        (map-put! config :client-maker client-maker)
        (delete-directory local)))))

(ert-deftest agent-shell-subagent-session-reference-test ()
  "Name sub-agent sessions referred to in tool call output."
  (agent-shell-subagent-tests--with-shell shell
    (should (equal (agent-shell--tool-call-update-output-markdown
                    '((content . [((type . "session") (sessionId . "child-1"))
                                  ((type . "session") (sessionId . "unknown"))]))
                    (buffer-local-value 'agent-shell--state shell))
                   "Sub-agent: Test investigator\n\nSession: unknown"))))

;;; Round two

(cl-defun agent-shell-subagent-tests--ask (shell request-id &key (session-id "child-1")
                                                 tool-call-id)
  "Raise a form with REQUEST-ID from SESSION-ID in SHELL, for TOOL-CALL-ID."
  (with-current-buffer shell
    (let ((agent-shell-elicitation--experimental-feature-enabled t)
          (request `((id . ,request-id)
                     (method . "elicitation/create")
                     (params (sessionId . ,session-id)
                             (mode . "form")
                             (message . "Which test?")
                             ,@(when tool-call-id
                                 `((toolCallId . ,tool-call-id)))
                             (requestedSchema (type . "object")
                                              (properties (name (type . "string"))))))))
      (unless (agent-shell-subagent--route-request :state agent-shell--state
                                                   :acp-request request)
        (agent-shell--on-request :state agent-shell--state :acp-request request)))))

(defun agent-shell-subagent-tests--count (needle text)
  "Return how many times NEEDLE occurs in TEXT."
  (let ((count 0)
        (start 0))
    (while (setq start (string-search needle text start))
      (setq count (1+ count)
            start (1+ start)))
    count))

(ert-deftest agent-shell-subagent-failed-restore-requests-test ()
  "Answer and remove a discarded sub-agent's requests and card."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (responses nil))
      (agent-shell-subagent-tests--with-responses responses
        (agent-shell-subagent-tests--request-permission shell 301)
        (agent-shell-subagent-tests--ask shell 302)
        (let ((allow (agent-shell-subagent-tests--permission-key shell "y")))
          (agent-shell-subagent--discard-restore state)
          (should (equal (sort (mapcar #'car responses) #'<) '(301 302)))
          (should-not (string-search "Test investigator" (agent-shell-subagent-tests--text shell)))
          (should-not (agent-shell--permission-pending-p :shell-buffer shell))
          ;; A stale control answers nothing.
          (funcall allow)
          (should (length= responses 2))
          (should-error (agent-shell-subagent--open :root-buffer shell :session-id "child-1")
                        :type 'user-error))))))

(ert-deftest agent-shell-subagent-disconnect-test ()
  "Stop showing sub-agents as working once the agent process ends."
  (agent-shell-subagent-tests--with-shell shell
    (let* ((state (buffer-local-value 'agent-shell--state shell))
           (client (map-elt state :client))
           (process (make-process :name "subagent-test" :command '("sleep" "30")
                                  :noquery t))
           (responses nil)
           (notifications nil)
           (started 0))
      (unwind-protect
          (progn
            (if (assq :process client)
                (map-put! client :process process)
              (nconc client (list (cons :process process))))
            (agent-shell-subagent-tests--route
             shell agent-shell-subagent-tests--parent
             '((sessionUpdate . "subagent_update") (sessionId . "child-1")
               (capabilities (cancel)) (state (state . "running"))))
            (should (equal (buffer-local-value 'agent-shell-subagent-running-count shell) 1))
            (agent-shell-subagent-tests--with-responses responses
              (agent-shell-subagent-tests--request-permission shell 141))
            (kill-process process)
            (while (process-live-p process)
              (accept-process-output nil 0.05))
            (accept-process-output nil 0.05)
            (should (equal (buffer-local-value 'agent-shell-subagent-running-count shell) 0))
            (should (equal (map-elt (car (agent-shell-subagents :shell-buffer shell)) :status)
                           "disconnected"))
            (should-not (map-elt (car (agent-shell-subagents :shell-buffer shell)) :can-cancel))
            ;; Its request can't be answered any more, so it's gone.
            (should-not (agent-shell--permission-pending-p :shell-buffer shell))
            (should-not responses)
            ;; Cancelling starts nothing.
            (cl-letf (((symbol-function 'acp--start-client)
                       (lambda (&rest _) (setq started (1+ started)))))
              (agent-shell-subagent-tests--with-notifications notifications
                (with-current-buffer (agent-shell-subagent--ensure-buffer
                                      :state state :session-id "child-1")
                  (should-error (agent-shell-interrupt t) :type 'user-error))))
            (should (equal started 0))
            (should-not notifications))
        (when (process-live-p process)
          (delete-process process))
        (map-put! client :process nil)))))

(ert-deftest agent-shell-subagent-resume-boundary-test ()
  "Allow cancelling a resumed sub-agent only once the resume succeeds."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (notifications nil))
      (map-put! state :active-requests
                (cons '((:method . "session/resume")) (map-elt state :active-requests)))
      (agent-shell-subagent-tests--route
       shell agent-shell-subagent-tests--parent
       '((sessionUpdate . "subagent_update") (sessionId . "child-1")
         (capabilities (cancel)) (state (state . "running"))))
      (let ((child (agent-shell-subagent--ensure-buffer :state state :session-id "child-1")))
        (agent-shell-subagent-tests--with-notifications notifications
          ;; Resume reports current state, but nothing may be sent yet.
          (should (agent-shell-subagent--working-p (agent-shell-subagent--record state "child-1")))
          (with-current-buffer child
            (should-error (agent-shell-interrupt t) :type 'user-error))
          (should-not notifications)
          ;; The resume succeeds.
          (map-put! state :active-requests
                    (seq-remove (lambda (request)
                                  (equal (map-elt request :method) "session/resume"))
                                (map-elt state :active-requests)))
          (with-current-buffer child
            (agent-shell-interrupt t))
          (should (equal notifications '(("session/cancel" . "child-1")))))))))

(ert-deftest agent-shell-subagent-late-responder-test ()
  "Ignore a saved responder answering a request already answered."
  (agent-shell-subagent-tests--with-shell shell
    (let* ((responses nil)
           (respond nil)
           (agent-shell-permission-responder-function
            (lambda (request)
              (setq respond (map-elt request :respond))
              t)))
      (agent-shell-subagent-tests--with-responses responses
        (agent-shell-subagent-tests--request-permission shell 204)
        (agent-shell-subagent-tests--route
         shell "child-1"
         '((sessionUpdate . "tool_call_update") (toolCallId . "perm-tool-204")
           (status . "failed")))
        (should (equal responses '((204 . "cancelled"))))
        (funcall respond "allow")
        (should (equal responses '((204 . "cancelled"))))))))

(ert-deftest agent-shell-subagent-form-tool-call-test ()
  "Tie a sub-agent's form to its own tool call, not the main shell's."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (responses nil))
      (agent-shell-subagent-tests--with-responses responses
        ;; The main shell's own questionnaire, under the same tool id.
        (agent-shell--save-tool-call state "ask" '((:title . "Ask")
                                                   (:raw-input (questions . [((question . "Pick?"))]))))
        (agent-shell-subagent-tests--ask shell 131 :session-id agent-shell-subagent-tests--parent
                                         :tool-call-id "ask")
        (agent-shell-subagent-tests--ask shell 132 :tool-call-id "ask")
        (should (equal (map-nested-elt state '(:elicitations 132 :tool-call-id))
                       '("child-1" . "ask")))
        ;; Declining keeps the settled form, as the main shell doesn't
        ;; show the sub-agent's tool call.
        (should (agent-shell-elicitation--respond :state state :id 132 :action 'decline))
        (should (string-search "From sub-agent" (agent-shell-subagent-tests--text shell)))
        ;; A form whose tool call fails is answered.
        (agent-shell-subagent-tests--ask shell 133 :tool-call-id "ask2")
        (agent-shell-subagent-tests--route
         shell "child-1"
         '((sessionUpdate . "tool_call_update") (toolCallId . "ask2") (status . "failed")))
        (should (equal (mapcar #'car responses) '(133 132)))
        (should (eq (map-nested-elt state '(:elicitations 133 :status)) 'cancelled))
        ;; The main shell's own form is left be.
        (should (eq (map-nested-elt state '(:elicitations 131 :status)) 'pending))))))

(ert-deftest agent-shell-subagent-form-across-turns-test ()
  "Settle a form answered in a later turn where it was drawn."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (responses nil))
      (agent-shell-subagent-tests--with-responses responses
        (agent-shell-subagent-tests--ask shell 171)
        (map-put! state :request-count (1+ (map-elt state :request-count)))
        (should (agent-shell-elicitation--respond :state state :id 171 :action 'decline))
        (let ((text (agent-shell-subagent-tests--text shell)))
          (should (equal (agent-shell-subagent-tests--count "Agent Question" text) 1))
          (should-not (string-search "Submit" text)))))))

(ert-deftest agent-shell-subagent-restore-config-test ()
  "Leave configuration replayed by a load out of the sub-agent's current one."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (update (lambda (value)
                    `((sessionUpdate . "config_option_update")
                      (configOptions . [((id . "model") (name . "Model") (type . "select")
                                         (currentValue . ,value)
                                         (options . [((value . ,value) (name . ,value))]))])))))
      (map-put! state :active-requests '(((:method . "session/load"))))
      (agent-shell-subagent-tests--route shell "child-1" (funcall update "past-model"))
      (map-put! state :active-requests nil)
      (let ((child (agent-shell-subagent--ensure-buffer :state state :session-id "child-1")))
        (should-not (string-search "past-model"
                                   (format "%S" (agent-shell--config-options
                                                 (buffer-local-value 'agent-shell--state child)))))
        (agent-shell-subagent-tests--route shell "child-1" (funcall update "live-model"))
        (should (string-search "live-model"
                               (format "%S" (agent-shell--config-options
                                             (buffer-local-value 'agent-shell--state child)))))))))

(ert-deftest agent-shell-subagent-permission-identity-test ()
  "Give dialogs for distinct sub-agent tool calls distinct identities."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (responses nil))
      (dolist (session-id '("a" "a . b"))
        (agent-shell-subagent-tests--route
         shell agent-shell-subagent-tests--parent
         `((sessionUpdate . "subagent_update") (sessionId . ,session-id))))
      (agent-shell-subagent-tests--with-responses responses
        (agent-shell-subagent-tests--request-permission
         shell 121 :session-id "a" :tool-call '((toolCallId . "b . c") (title . "First")))
        (agent-shell-subagent-tests--request-permission
         shell 122 :session-id "a . b" :tool-call '((toolCallId . "c") (title . "Second")))
        (should (equal (agent-shell-subagent-tests--count
                        "Allow (y)" (agent-shell-subagent-tests--text shell))
                       2))
        (funcall (agent-shell-subagent-tests--permission-key
                  shell "y" "From sub-agent: Sub-agent a . b"))
        (should (equal responses '((122 . "selected"))))
        (should (equal (agent-shell-subagent-tests--count
                        "Allow (y)" (agent-shell-subagent-tests--text shell))
                       1))
        (should (map-nested-elt state '(:tool-calls ("a" . "b . c") :permission-request-id)))))))

(ert-deftest agent-shell-subagent-message-patch-test ()
  "Keep the newest task when an older message is patched."
  (let ((state (agent-shell--make-state)))
    (agent-shell-subagent--register
     :state state :parent-session-id "p1" :update '((sessionId . "c1")))
    (dolist (message '(("m1" . "First assignment") ("m2" . "Second assignment")))
      (agent-shell-subagent--record-message
       :state state :session-id "p1"
       :update `((sessionUpdate . "session_message") (messageId . ,(car message))
                 (senderSessionId . "p1") (recipientSessionId . "c1")
                 (content . [((type . "text") (text . ,(cdr message)))]))))
    (should-not (agent-shell-subagent--record-message
                 :state state :session-id "p1"
                 :update '((sessionUpdate . "session_message") (messageId . "m1")
                           (recipientMessageId . "c1-m1"))))
    (should (equal (map-elt (agent-shell-subagent--record state "c1") :task)
                   "Second assignment"))
    ;; The newest one can still be revised.
    (agent-shell-subagent--record-message
     :state state :session-id "p1"
     :update '((sessionUpdate . "session_message_chunk") (messageId . "m2")
               (content (type . "text") (text . ", revised"))))
    (should (equal (map-elt (agent-shell-subagent--record state "c1") :task)
                   "Second assignment, revised"))))

(ert-deftest agent-shell-subagent-inherited-cwd-test ()
  "Move an open nested sub-agent along with its parent's directory."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (first (file-name-as-directory (make-temp-file "subagent-a" t)))
          (second (file-name-as-directory (make-temp-file "subagent-b" t))))
      (unwind-protect
          (progn
            (agent-shell-subagent-tests--route
             shell agent-shell-subagent-tests--parent
             `((sessionUpdate . "subagent_update") (sessionId . "child-1") (cwd . ,first)))
            (agent-shell-subagent-tests--route
             shell "child-1"
             '((sessionUpdate . "subagent_update") (sessionId . "grandchild")))
            (let ((grandchild (agent-shell-subagent--ensure-buffer
                               :state state :session-id "grandchild")))
              (should (equal (buffer-local-value 'default-directory grandchild) first))
              (agent-shell-subagent-tests--route
               shell agent-shell-subagent-tests--parent
               `((sessionUpdate . "subagent_update") (sessionId . "child-1") (cwd . ,second)))
              (should (equal (buffer-local-value 'default-directory grandchild) second))
              (kill-buffer grandchild)))
        (delete-directory first)
        (delete-directory second)))))

(ert-deftest agent-shell-subagent-tool-call-cache-test ()
  "Keep one entry per running sub-agent tool call, and none once it's done."
  (agent-shell-subagent-tests--with-shell shell
    (let ((record (agent-shell-subagent--record
                   (buffer-local-value 'agent-shell--state shell) "child-1")))
      (dotimes (step 20)
        (agent-shell-subagent-tests--route
         shell "child-1"
         `((sessionUpdate . "tool_call_update") (toolCallId . "busy")
           (title . ,(format "Step %d" step)) (status . "in_progress"))))
      (should (length= (map-elt record :tool-calls) 1))
      (should (equal (map-nested-elt record '(:tool-calls "busy" title)) "Step 19"))
      (agent-shell-subagent-tests--route
       shell "child-1"
       '((sessionUpdate . "tool_call_update") (toolCallId . "busy") (status . "completed")))
      (should-not (map-elt record :tool-calls)))))

(ert-deftest agent-shell-subagent-late-permission-details-test ()
  "Redraw a sub-agent's permission dialog with details reported after it."
  (agent-shell-subagent-tests--with-shell shell
    (cl-letf (((symbol-function 'acp-send-response) #'ignore))
      (agent-shell-subagent-tests--request-permission
       shell 151 :tool-call '((toolCallId . "late"))))
    (should-not (string-search "make integration-test" (agent-shell-subagent-tests--text shell)))
    (agent-shell-subagent-tests--route
     shell "child-1"
     '((sessionUpdate . "tool_call_update") (toolCallId . "late") (status . "pending")
       (rawInput (command . "make integration-test"))))
    (should (string-search "make integration-test" (agent-shell-subagent-tests--text shell)))
    (should (agent-shell--permission-pending-p :shell-buffer shell))))

(ert-deftest agent-shell-subagent-form-open-key-test ()
  "Open a question's sub-agent with o, from its controls or Open button."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell)))
      (cl-letf (((symbol-function 'acp-send-response) #'ignore))
        (agent-shell-subagent-tests--ask shell 161))
      (with-current-buffer shell
        (goto-char (point-min))
        (search-forward "Submit")
        (backward-char)
        (should (eq (lookup-key (get-text-property (point) 'keymap) (kbd "o"))
                    #'agent-shell-elicitation-open-source))
        (agent-shell-elicitation-open-source))
      (let ((child (map-elt (agent-shell-subagent--record state "child-1") :buffer)))
        (should (buffer-live-p child))
        (kill-buffer child))
      (with-current-buffer shell
        (goto-char (point-min))
        (search-forward "From sub-agent: Test investigator")
        (search-forward "Open")
        (funcall (lookup-key (get-text-property (1- (point)) 'keymap) (kbd "o"))))
      (should (buffer-live-p (map-elt (agent-shell-subagent--record state "child-1") :buffer))))))

(ert-deftest agent-shell-subagent-session-reference-link-test ()
  "Link a tool call's sub-agent session reference to its conversation."
  (agent-shell-subagent-tests--with-shell shell
    (let* ((state (buffer-local-value 'agent-shell--state shell))
           (text (agent-shell-subagent--session-reference-text state "child-1"))
           (action (lookup-key (get-text-property (length "Sub-agent: ") 'keymap text)
                               (kbd "RET"))))
      (with-current-buffer shell
        (funcall action))
      (should (buffer-live-p (map-elt (agent-shell-subagent--record state "child-1") :buffer))))))

(ert-deftest agent-shell-subagent-cycle-test ()
  "Ignore an announcement that would make a session its own ancestor."
  (let ((state (agent-shell--make-state)))
    (agent-shell-subagent--register
     :state state :parent-session-id "c1" :update '((sessionId . "c1")))
    (agent-shell-subagent--register
     :state state :parent-session-id "x" :update '((sessionId . "y")))
    (agent-shell-subagent--register
     :state state :parent-session-id "y" :update '((sessionId . "x")))
    (should (equal (map-keys (map-elt state :subagents)) '("y")))))

(ert-deftest agent-shell-subagent-cancel-decoding-test ()
  "Grant cancelling for an advertised `cancel', as decoded by acp.el."
  (let ((record (agent-shell-subagent--make-record :session-id "c1")))
    (agent-shell-subagent--apply-update
     record (acp--parse-json "{\"sessionId\":\"c1\",\"capabilities\":{\"cancel\":{}}}"))
    (should (map-elt record :can-cancel))))

(ert-deftest agent-shell-subagent-null-cancel-test ()
  "Deny cancelling for a null `cancel'.

Expected to fail while acp.el decodes {} and null alike (both nil)."
  :expected-result :failed
  (let ((record (agent-shell-subagent--make-record :session-id "c1")))
    (agent-shell-subagent--apply-update
     record (acp--parse-json "{\"sessionId\":\"c1\",\"capabilities\":{\"cancel\":null}}"))
    (should-not (map-elt record :can-cancel))))

;;; Round three

(ert-deftest agent-shell-subagent-dead-restore-test ()
  "Withdraw a discarded sub-agent's requests when the process has ended."
  (agent-shell-subagent-tests--with-shell shell
    (let* ((state (buffer-local-value 'agent-shell--state shell))
           (client (map-elt state :client))
           (process (make-process :name "subagent-test" :command '("sleep" "30")
                                  :noquery t))
           (sent 0))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'acp-send-response) #'ignore))
              (agent-shell-subagent-tests--request-permission shell 311)
              (agent-shell-subagent-tests--ask shell 312))
            (if (assq :process client)
                (map-put! client :process process)
              (nconc client (list (cons :process process))))
            (delete-process process)
            ;; As when sending to the ended process.
            (cl-letf (((symbol-function 'acp-send-response)
                       (lambda (&rest _)
                         (setq sent (1+ sent))
                         (error "Process not running"))))
              (agent-shell-subagent--discard-restore state))
            (should (equal sent 0))
            (should-not (agent-shell-subagents :shell-buffer shell))
            (should-not (agent-shell--permission-pending-p :shell-buffer shell))
            (should (eq (map-nested-elt state '(:elicitations 312 :status)) 'cancelled))
            (should-not (string-search "Submit" (agent-shell-subagent-tests--text shell))))
        (map-put! client :process nil)))))

(ert-deftest agent-shell-subagent-late-permission-content-test ()
  "Show text a sub-agent's tool call reports after asking permission."
  (agent-shell-subagent-tests--with-shell shell
    (cl-letf (((symbol-function 'acp-send-response) #'ignore))
      (agent-shell-subagent-tests--request-permission
       shell 321 :tool-call '((toolCallId . "build") (title . "Build"))))
    (should-not (string-search "mobile manifest" (agent-shell-subagent-tests--text shell)))
    (agent-shell-subagent-tests--route
     shell "child-1"
     '((sessionUpdate . "tool_call_update") (toolCallId . "build")
       (content . [((type . "content")
                    (content (type . "text") (text . "Build the mobile manifest")))])))
    (should (string-search "Build the mobile manifest"
                           (agent-shell-subagent-tests--text shell)))
    (should (agent-shell--permission-pending-p :shell-buffer shell))))

(ert-deftest agent-shell-subagent-claimed-permission-test ()
  "Draw no dialog for a request a responder claimed, even with later details."
  (agent-shell-subagent-tests--with-shell shell
    (let* ((responses nil)
           (respond nil)
           (agent-shell-permission-responder-function
            (lambda (request)
              (setq respond (map-elt request :respond))
              t)))
      (agent-shell-subagent-tests--with-responses responses
        (agent-shell-subagent-tests--request-permission shell 331)
        (agent-shell-subagent-tests--route
         shell "child-1"
         '((sessionUpdate . "tool_call_update") (toolCallId . "perm-tool-331")
           (rawInput (command . "make release"))))
        (should-not (string-search "Allow (y)" (agent-shell-subagent-tests--text shell)))
        (should (funcall respond "allow"))
        (should-not (funcall respond "allow"))
        (should (equal responses '((331 . "selected"))))))))

(ert-deftest agent-shell-subagent-withdraw-viewer-test ()
  "Close a withdrawn request's diff viewer and drop its reminder."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (viewer (generate-new-buffer "*subagent-diff-test*")))
      (cl-letf (((symbol-function 'acp-send-response) #'ignore))
        (agent-shell-subagent-tests--request-permission shell 341))
      (agent-shell--save-tool-call state '("child-1" . "perm-tool-341")
                                   (list (cons :diff-buffer viewer)))
      (with-current-buffer shell
        (should (timerp (map-elt state :idle-timer))))
      (agent-shell-subagent--retire-requests state)
      (should-not (buffer-live-p viewer))
      (should-not (map-elt state :idle-timer)))))

(ert-deftest agent-shell-subagent-restore-events-test ()
  "Tell subscribers what a restore's success or failure changed."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (events nil))
      (agent-shell-subscribe-to
       :shell-buffer shell
       :event 'subagent-update
       :on-event (lambda (event)
                   (push (map-elt event :data) events)))
      (map-put! state :active-requests
                (cons '((:method . "session/resume")) (map-elt state :active-requests)))
      (agent-shell-subagent-tests--route
       shell agent-shell-subagent-tests--parent
       '((sessionUpdate . "subagent_update") (sessionId . "child-1")
         (capabilities (cancel)) (state (state . "running"))))
      (should-not (map-nested-elt (car events) '(:subagent :can-cancel)))
      (map-put! state :active-requests
                (seq-remove (lambda (request)
                              (equal (map-elt request :method) "session/resume"))
                            (map-elt state :active-requests)))
      (setq events nil)
      (agent-shell-subagent--on-restore-success state)
      (should (map-nested-elt (car events) '(:subagent :can-cancel)))
      ;; Discarded, as by a failed restore.
      (setq events nil)
      (agent-shell-subagent--discard-restore state)
      (should (length= events 1))
      (should (map-elt (car events) :removed))
      (should (equal (map-nested-elt (car events) '(:subagent :session-id)) "child-1"))
      (should (equal (map-elt (car events) :count) 0)))))

(ert-deftest agent-shell-subagent-linked-copy-test ()
  "Date a late copy of an assignment from the original it links to."
  (let ((state (agent-shell--make-state)))
    (agent-shell-subagent--register
     :state state :parent-session-id "p1" :update '((sessionId . "c1")))
    (dolist (message '(("a" . "Assignment A") ("b" . "Assignment B")))
      (agent-shell-subagent--record-message
       :state state :session-id "p1"
       :update `((sessionUpdate . "session_message") (messageId . ,(car message))
                 (senderSessionId . "p1") (recipientSessionId . "c1")
                 (content . [((type . "text") (text . ,(cdr message)))]))))
    ;; The child's copy of A, reported last.
    (agent-shell-subagent--record-message
     :state state :session-id "c1"
     :update '((sessionUpdate . "session_message") (messageId . "c1-a")
               (senderSessionId . "p1") (recipientSessionId . "c1")
               (senderMessageId . "a")
               (content . [((type . "text") (text . "Assignment A"))])))
    (should (equal (map-elt (agent-shell-subagent--record state "c1") :task)
                   "Assignment B"))
    ;; B turns out not to be from the parent.
    (should (agent-shell-subagent--record-message
             :state state :session-id "p1"
             :update '((sessionUpdate . "session_message") (messageId . "b")
                       (senderSessionId))))
    (should (equal (map-elt (agent-shell-subagent--record state "c1") :task)
                   "Assignment A"))))

(ert-deftest agent-shell-subagent-initial-session-reference-test ()
  "Show a session reference sent only in the initial tool call."
  (agent-shell-subagent-tests--with-shell shell
    (with-current-buffer shell
      (dolist (update '(((sessionUpdate . "tool_call") (toolCallId . "wait-1")
                         (title . "Wait") (kind . "other") (status . "in_progress")
                         (content . [((type . "session") (sessionId . "child-1"))]))
                        ((sessionUpdate . "tool_call_update") (toolCallId . "wait-1")
                         (status . "completed"))))
        (agent-shell--dispatch-notification
         :state agent-shell--state
         :acp-notification (map-elt (agent-shell-subagent-tests--update
                                     agent-shell-subagent-tests--parent update)
                                    :object))))
    (should (string-search "Sub-agent: Test investigator"
                           (agent-shell-subagent-tests--text shell)))))

(ert-deftest agent-shell-subagent-discarded-answer-test ()
  "Keep an answered question readable, but unlinked, once its sub-agent is discarded."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell)))
      (cl-letf (((symbol-function 'acp-send-response) #'ignore))
        (agent-shell-subagent-tests--ask shell 361)
        (agent-shell-elicitation--respond :state state :id 361 :action 'decline)
        (should (string-search "From sub-agent" (agent-shell-subagent-tests--text shell)))
        (agent-shell-subagent--discard-restore state))
      (should (string-search "Which test?" (agent-shell-subagent-tests--text shell)))
      (should-not (string-search "From sub-agent" (agent-shell-subagent-tests--text shell))))))

(ert-deftest agent-shell-subagent-null-marker-cancel-test ()
  "Deny cancelling for a `cancel' decoded as a null marker."
  (let ((record (agent-shell-subagent--make-record :session-id "c1")))
    (agent-shell-subagent--apply-update
     record '((sessionId . "c1") (capabilities (cancel . :null))))
    (should-not (map-elt record :can-cancel))))

;;; Round four

(defun agent-shell-subagent-tests--receive (shell session-id update)
  "Have SHELL receive `session/update' UPDATE sent on SESSION-ID.

As the client does: routed first, and rendered in SHELL unless it's a
sub-agent's."
  (with-current-buffer shell
    (let ((notification (map-elt (agent-shell-subagent-tests--update session-id update)
                                 :object)))
      (unless (agent-shell-subagent--route-notification
               :state agent-shell--state :acp-notification notification)
        (agent-shell--dispatch-notification
         :state agent-shell--state :acp-notification notification)))))

(ert-deftest agent-shell-subagent-reference-output-test ()
  "Show a tool's output alongside a session reference sent earlier."
  (agent-shell-subagent-tests--with-shell shell
    (let ((transcript nil))
      (cl-letf (((symbol-function 'agent-shell--append-transcript)
                 (lambda (&rest args)
                   (push (plist-get args :text) transcript))))
        (agent-shell-subagent-tests--receive
         shell agent-shell-subagent-tests--parent
         '((sessionUpdate . "tool_call") (toolCallId . "wait-2")
           (title . "Wait") (kind . "other") (status . "in_progress")
           (content . [((type . "session") (sessionId . "child-1"))])))
        (agent-shell-subagent-tests--receive
         shell agent-shell-subagent-tests--parent
         '((sessionUpdate . "tool_call_update") (toolCallId . "wait-2")
           (status . "completed")
           (rawOutput (formatted_output . "42 tests passed")))))
      (let ((text (agent-shell-subagent-tests--text shell))
            (saved (string-join transcript)))
        (should (string-search "Sub-agent: Test investigator" text))
        (should (string-search "42 tests passed" text))
        (should (string-search "Sub-agent: Test investigator" saved))
        (should (string-search "42 tests passed" saved))))))

(ert-deftest agent-shell-subagent-empty-copy-test ()
  "Keep a task's text when its linked copy reports no content."
  (let ((state (agent-shell--make-state)))
    (agent-shell-subagent--register
     :state state :parent-session-id "p1" :update '((sessionId . "c1")))
    (agent-shell-subagent--record-message
     :state state :session-id "p1"
     :update '((sessionUpdate . "session_message") (messageId . "m1")
               (senderSessionId . "p1") (recipientSessionId . "c1")
               (content . [((type . "text") (text . "Package the launch report"))])))
    (agent-shell-subagent--record-message
     :state state :session-id "c1"
     :update '((sessionUpdate . "session_message") (messageId . "c1-m1")
               (senderSessionId . "p1") (recipientSessionId . "c1")
               (senderMessageId . "m1")))
    (should (equal (map-elt (agent-shell-subagent--record state "c1") :task)
                   "Package the launch report"))
    (agent-shell-subagent--record-message
     :state state :session-id "p1"
     :update '((sessionUpdate . "session_message_chunk") (messageId . "m1")
               (content (type . "text") (text . " today"))))
    (should (equal (map-elt (agent-shell-subagent--record state "c1") :task)
                   "Package the launch report today"))
    ;; Or the other way around: only the copy has text.
    (agent-shell-subagent--register
     :state state :parent-session-id "p1" :update '((sessionId . "c2")))
    (agent-shell-subagent--record-message
     :state state :session-id "p1"
     :update '((sessionUpdate . "session_message") (messageId . "m2")
               (senderSessionId . "p1") (recipientSessionId . "c2")))
    (agent-shell-subagent--record-message
     :state state :session-id "c2"
     :update '((sessionUpdate . "session_message") (messageId . "c2-m2")
               (senderSessionId . "p1") (recipientSessionId . "c2")
               (senderMessageId . "m2")
               (content . [((type . "text") (text . "Review the changelog"))])))
    (should (equal (map-elt (agent-shell-subagent--record state "c2") :task)
                   "Review the changelog"))))

(ert-deftest agent-shell-subagent-message-attribution-test ()
  "Show a message on its own until its card can, then only there."
  (agent-shell-subagent-tests--with-shell shell
    ;; Only the recipient is known, so it isn't the card's.
    (agent-shell-subagent-tests--receive
     shell agent-shell-subagent-tests--parent
     '((sessionUpdate . "session_message") (messageId . "note")
       (recipientSessionId . "child-1")
       (content . [((type . "text") (text . "Draft release notes"))])))
    (should (string-search "Draft release notes" (agent-shell-subagent-tests--text shell)))
    ;; No participants at first, both (and new text) later.
    (agent-shell-subagent-tests--receive
     shell agent-shell-subagent-tests--parent
     '((sessionUpdate . "session_message") (messageId . "late")
       (content . [((type . "text") (text . "Original wording"))])))
    (should (string-search "Original wording" (agent-shell-subagent-tests--text shell)))
    (agent-shell-subagent-tests--receive
     shell agent-shell-subagent-tests--parent
     `((sessionUpdate . "session_message") (messageId . "late")
       (senderSessionId . ,agent-shell-subagent-tests--parent)
       (recipientSessionId . "child-1")
       (content . [((type . "text") (text . "Replacement wording"))])))
    (let ((text (agent-shell-subagent-tests--text shell)))
      (should-not (string-search "Original wording" text))
      (should (equal (agent-shell-subagent-tests--count "Replacement wording" text) 1)))))

(ert-deftest agent-shell-subagent-withdraw-form-reminder-test ()
  "Drop a withdrawn question's reminder, and only its own."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell)))
      (cl-letf (((symbol-function 'acp-send-response) #'ignore))
        (agent-shell-subagent-tests--ask shell 401))
      (should (timerp (map-elt state :idle-timer)))
      (agent-shell-subagent--retire-requests state)
      (should-not (map-elt state :idle-timer))
      ;; Another request's reminder stays.
      (cl-letf (((symbol-function 'acp-send-response) #'ignore))
        (agent-shell-subagent-tests--ask shell 402))
      (with-current-buffer shell
        (agent-shell--start-idle-timer :event 'permission-request
                                       :data '((:request-id . 999))))
      (agent-shell-subagent--retire-requests state)
      (should (timerp (map-elt state :idle-timer)))
      (with-current-buffer shell
        (agent-shell--cancel-idle-timer)))))

;;; Round five

(ert-deftest agent-shell-subagent-reference-order-test ()
  "Keep session references in place among a tool's output."
  (agent-shell-subagent-tests--with-shell shell
    (let ((transcript nil)
          (expected "Primary owner\n\nSub-agent: Test investigator\n\nBackup owner\n\nSub-agent: Release checker"))
      (agent-shell-subagent-tests--route
       shell agent-shell-subagent-tests--parent
       '((sessionUpdate . "subagent_update") (sessionId . "child-2")
         (title . "Release checker") (state (state . "running"))))
      (cl-letf (((symbol-function 'agent-shell--append-transcript)
                 (lambda (&rest args)
                   (push (plist-get args :text) transcript))))
        (agent-shell-subagent-tests--receive
         shell agent-shell-subagent-tests--parent
         '((sessionUpdate . "tool_call") (toolCallId . "owners")
           (title . "Owners") (kind . "other") (status . "in_progress")))
        (agent-shell-subagent-tests--receive
         shell agent-shell-subagent-tests--parent
         '((sessionUpdate . "tool_call_update") (toolCallId . "owners")
           (status . "completed")
           (content . [((type . "content") (content (type . "text") (text . "Primary owner")))
                       ((type . "session") (sessionId . "child-1"))
                       ((type . "content") (content (type . "text") (text . "Backup owner")))
                       ((type . "session") (sessionId . "child-2"))]))))
      (should (string-search expected (agent-shell-subagent-tests--text shell)))
      (should (string-search expected (string-join transcript))))))

(ert-deftest agent-shell-subagent-late-link-test ()
  "Pick the newest task once a late link dates an older copy."
  (let ((state (agent-shell--make-state)))
    (agent-shell-subagent--register
     :state state :parent-session-id "p1" :update '((sessionId . "c1")))
    ;; The child's view of A, its recipient unreported.
    (agent-shell-subagent--record-message
     :state state :session-id "c1"
     :update '((sessionUpdate . "session_message") (messageId . "c1-a")
               (senderSessionId . "p1")
               (content . [((type . "text") (text . "Assignment A"))])))
    (dolist (message '(("b" . "Assignment B") ("a" . "Assignment A")))
      (agent-shell-subagent--record-message
       :state state :session-id "p1"
       :update `((sessionUpdate . "session_message") (messageId . ,(car message))
                 (senderSessionId . "p1") (recipientSessionId . "c1")
                 (content . [((type . "text") (text . ,(cdr message)))]))))
    (should (equal (map-elt (agent-shell-subagent--record state "c1") :task)
                   "Assignment A"))
    ;; The link names neither sub-agent, but dates the parent's A.
    (should (equal (agent-shell-subagent--record-message
                    :state state :session-id "c1"
                    :update '((sessionUpdate . "session_message") (messageId . "c1-a")
                              (senderMessageId . "a")))
                   (list (agent-shell-subagent--record state "c1"))))
    (should (equal (map-elt (agent-shell-subagent--record state "c1") :task)
                   "Assignment B"))))

(ert-deftest agent-shell-subagent-split-views-test ()
  "Take a task's participants and text from either of its views."
  (agent-shell-subagent-tests--with-shell shell
    (let ((parent agent-shell-subagent-tests--parent))
      ;; The parent's view has the text, but no recipient.
      (agent-shell-subagent-tests--receive
       shell parent
       `((sessionUpdate . "session_message") (messageId . "m1")
         (senderSessionId . ,parent)
         (content . [((type . "text") (text . "Audit the release"))])))
      (should (string-search "Message sent" (agent-shell-subagent-tests--text shell)))
      ;; The child's view names both, but has no text.
      (agent-shell-subagent-tests--receive
       shell "child-1"
       `((sessionUpdate . "session_message") (messageId . "c1-m1")
         (senderSessionId . ,parent) (recipientSessionId . "child-1")
         (senderMessageId . "m1")))
      (should (equal (map-elt (agent-shell-subagent--record
                               (buffer-local-value 'agent-shell--state shell) "child-1")
                              :task)
                     "Audit the release"))
      ;; Now the card's, so shown there only.
      (let ((text (agent-shell-subagent-tests--text shell)))
        (should-not (string-search "Message sent" text))
        (should (equal (agent-shell-subagent-tests--count "Audit the release" text) 1)))
      ;; The same, with the child's view first.
      (agent-shell-subagent-tests--receive
       shell "child-1"
       `((sessionUpdate . "session_message") (messageId . "c1-m2")
         (senderSessionId . ,parent) (recipientSessionId . "child-1")
         (senderMessageId . "m2")))
      (agent-shell-subagent-tests--receive
       shell parent
       `((sessionUpdate . "session_message") (messageId . "m2")
         (senderSessionId . ,parent)
         (content . [((type . "text") (text . "Check the changelog"))])))
      (let ((text (agent-shell-subagent-tests--text shell)))
        (should-not (string-search "Message sent" text))
        (should (equal (agent-shell-subagent-tests--count "Check the changelog" text) 1))))))

(ert-deftest agent-shell-subagent-split-views-unlink-test ()
  "Show a parent's view on its own again once its linked view lets go."
  (agent-shell-subagent-tests--with-shell shell
    (let ((parent agent-shell-subagent-tests--parent))
      (dolist (message '(("a" . "Keep the log") ("b" . "Rotate the keys")))
        (agent-shell-subagent-tests--receive
         shell parent
         `((sessionUpdate . "session_message") (messageId . ,(car message))
           (senderSessionId . ,parent)
           (content . [((type . "text") (text . ,(cdr message)))]))))
      (agent-shell-subagent-tests--receive
       shell "child-1"
       `((sessionUpdate . "session_message") (messageId . "c1-a")
         (senderSessionId . ,parent) (recipientSessionId . "child-1")
         (senderMessageId . "a")))
      (let ((text (agent-shell-subagent-tests--text shell)))
        (should (equal (agent-shell-subagent-tests--count "Message sent" text) 1))
        (should (equal (agent-shell-subagent-tests--count "Keep the log" text) 1)))
      ;; Redirected: A is on its own again, and B in the card.
      (agent-shell-subagent-tests--receive
       shell "child-1"
       '((sessionUpdate . "session_message") (messageId . "c1-a")
         (senderMessageId . "b")))
      (let ((text (agent-shell-subagent-tests--text shell)))
        (should (equal (agent-shell-subagent-tests--count "Message sent" text) 1))
        (should (equal (agent-shell-subagent-tests--count "Keep the log" text) 1))
        (should (equal (agent-shell-subagent-tests--count "Rotate the keys" text) 1)))
      ;; Cleared: both on their own.
      (agent-shell-subagent-tests--receive
       shell "child-1"
       '((sessionUpdate . "session_message") (messageId . "c1-a")
         (senderMessageId)))
      (let ((text (agent-shell-subagent-tests--text shell)))
        (should (equal (agent-shell-subagent-tests--count "Message sent" text) 2))
        (should (equal (agent-shell-subagent-tests--count "Keep the log" text) 1))
        (should (equal (agent-shell-subagent-tests--count "Rotate the keys" text) 1))))))

(ert-deftest agent-shell-subagent-unlink-turn-test ()
  "Restore an unlinked message in the turn it arrived in."
  (let ((parent agent-shell-subagent-tests--parent))
    (agent-shell-subagent-tests--with-traffic shell
        (list
         `((:direction . outgoing) (:kind . request)
           (:object (jsonrpc . "2.0") (method . "session/prompt") (id . 1)
                    (params (sessionId . ,parent)
                            (prompt . [((type . "text") (text . "First turn"))]))))
         (agent-shell-subagent-tests--update
          parent '((sessionUpdate . "subagent_update") (sessionId . "child-1")
                   (title . "Logger") (state (state . "running"))))
         (agent-shell-subagent-tests--update
          parent `((sessionUpdate . "session_message") (messageId . "a")
                   (senderSessionId . ,parent)
                   (content . [((type . "text") (text . "Keep the log"))])))
         (agent-shell-subagent-tests--update
          "child-1" `((sessionUpdate . "session_message") (messageId . "c1-a")
                      (senderSessionId . ,parent) (recipientSessionId . "child-1")
                      (senderMessageId . "a")))
         `((:direction . incoming) (:kind . response)
           (:object (jsonrpc . "2.0") (id . 1) (result (stopReason . "end_turn"))))
         `((:direction . outgoing) (:kind . request)
           (:object (jsonrpc . "2.0") (method . "session/prompt") (id . 2)
                    (params (sessionId . ,parent)
                            (prompt . [((type . "text") (text . "Second turn"))]))))
         (agent-shell-subagent-tests--update
          parent '((sessionUpdate . "agent_message_chunk") (messageId . "reply")
                   (content (type . "text") (text . "Second reply"))))
         (agent-shell-subagent-tests--update
          "child-1" '((sessionUpdate . "session_message") (messageId . "c1-a")
                      (senderMessageId)))
         `((:direction . incoming) (:kind . response)
           (:object (jsonrpc . "2.0") (id . 2) (result (stopReason . "end_turn")))))
      (let ((history (with-current-buffer shell
                       (shell-maker-history))))
        (should (equal (mapcar #'car history) '("First turn" "Second turn")))
        (should (string-search "Keep the log" (cdr (nth 0 history))))
        (should-not (string-search "Keep the log" (cdr (nth 1 history))))))))

(ert-deftest agent-shell-subagent-unlink-viewport-test ()
  "Show a message restored in the turn on screen in the open viewport."
  (let ((parent agent-shell-subagent-tests--parent))
    ;; The turn never ends.
    (agent-shell-subagent-tests--with-traffic shell
        (list
         `((:direction . outgoing) (:kind . request)
           (:object (jsonrpc . "2.0") (method . "session/prompt") (id . 1)
                    (params (sessionId . ,parent)
                            (prompt . [((type . "text") (text . "First turn"))]))))
         (agent-shell-subagent-tests--update
          parent '((sessionUpdate . "subagent_update") (sessionId . "child-1")
                   (title . "Logger") (state (state . "running")))))
      (let ((viewport (agent-shell-viewport--buffer :shell-buffer shell)))
        (unwind-protect
            (progn
              (with-current-buffer viewport
                (agent-shell-viewport-view-mode)
                (agent-shell-viewport-refresh))
              (agent-shell-subagent-tests--receive
               shell parent
               `((sessionUpdate . "session_message") (messageId . "a")
                 (senderSessionId . ,parent)
                 (content . [((type . "text") (text . "Keep the log"))])))
              (should (string-search "Keep the log" (agent-shell-subagent-tests--text viewport)))
              (agent-shell-subagent-tests--receive
               shell "child-1"
               `((sessionUpdate . "session_message") (messageId . "c1-a")
                 (senderSessionId . ,parent) (recipientSessionId . "child-1")
                 (senderMessageId . "a")))
              (should-not (string-search "Message sent" (agent-shell-subagent-tests--text viewport)))
              (agent-shell-subagent-tests--receive
               shell "child-1"
               '((sessionUpdate . "session_message") (messageId . "c1-a")
                 (senderMessageId)))
              (dolist (buffer (list shell viewport))
                (let ((text (agent-shell-subagent-tests--text buffer)))
                  (should (equal (agent-shell-subagent-tests--count "Message sent" text) 1))
                  (should (equal (agent-shell-subagent-tests--count "Keep the log" text) 1)))))
          (kill-buffer viewport))))))

(ert-deftest agent-shell-subagent-root-message-labels-test ()
  "Never label the main session's messages as to or from a parent."
  (agent-shell-subagent-tests--with-shell shell
    (let ((parent agent-shell-subagent-tests--parent))
      (dolist (message `(("in" (recipientSessionId . ,parent))
                         ("out" (senderSessionId . ,parent))
                         ("unknown")))
        (agent-shell-subagent-tests--receive
         shell parent
         `((sessionUpdate . "session_message") (messageId . ,(car message))
           ,@(cdr message)
           (content . [((type . "text") (text . ,(format "Body %s" (car message))))]))))
      (let ((text (agent-shell-subagent-tests--text shell)))
        (should-not (string-search "Task from parent" text))
        (should-not (string-search "Reported to parent" text))
        (should (string-search "Message received" text))
        (should (string-search "Message sent" text))
        (should (string-search "Session message" text))))))

(ert-deftest agent-shell-subagent-sibling-reminder-test ()
  "Keep a pending question's reminder when another request is cancelled."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (responses nil))
      (agent-shell-subagent-tests--with-responses responses
        (agent-shell-subagent-tests--request-permission shell 501)
        (agent-shell-subagent-tests--ask shell 502)
        (agent-shell-subagent-tests--route
         shell "child-1"
         '((sessionUpdate . "tool_call_update") (toolCallId . "perm-tool-501")
           (status . "failed"))))
      (should (equal responses '((501 . "cancelled"))))
      (should (timerp (map-elt state :idle-timer)))
      (should (equal (map-elt state :idle-timer-request-id) 502))
      ;; Likewise for a form raised for the tool call.
      (cl-letf (((symbol-function 'acp-send-response) #'ignore))
        (agent-shell-subagent-tests--ask shell 503 :tool-call-id "tool-503")
        (agent-shell-subagent-tests--request-permission shell 504)
        (agent-shell-subagent-tests--route
         shell "child-1"
         '((sessionUpdate . "tool_call_update") (toolCallId . "tool-503")
           (status . "completed"))))
      (should-not (eq (map-nested-elt state '(:elicitations 503 :status)) 'pending))
      (should (timerp (map-elt state :idle-timer)))
      (should (equal (map-elt state :idle-timer-request-id) 504))
      (with-current-buffer shell
        (agent-shell--cancel-idle-timer)))))

;;; Round six


(ert-deftest agent-shell-subagent-reloaded-reminder-test ()
  "Drop a withdrawn request's reminder armed by code loaded earlier."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (responses nil))
      (agent-shell-subagent-tests--with-responses responses
        (agent-shell-subagent-tests--request-permission shell 601)
        (should (timerp (map-elt state :idle-timer)))
        ;; As armed before the shell's state had the key.
        (with-current-buffer shell
          (setq agent-shell--state (assq-delete-all :idle-timer-request-id agent-shell--state)))
        (agent-shell-subagent-tests--route
         shell "child-1"
         '((sessionUpdate . "tool_call_update") (toolCallId . "perm-tool-601")
           (status . "failed"))))
      (should (equal responses '((601 . "cancelled"))))
      (should-not (map-elt state :idle-timer)))))

(defun agent-shell-subagent-tests--armed-for (state)
  "Return the request STATE's live idle reminder is for, if armed."
  (and (timerp (map-elt state :idle-timer))
       (map-elt state :idle-timer-request-id)))

(ert-deftest agent-shell-subagent-reminder-scope-test ()
  "Keep another request's reminder through cleanup, not through the user's answer."
  (agent-shell-subagent-tests--with-shell shell
    (let ((state (buffer-local-value 'agent-shell--state shell))
          (responses nil))
      (agent-shell-subagent-tests--route
       shell agent-shell-subagent-tests--parent
       '((sessionUpdate . "subagent_update") (sessionId . "child-2")
         (title . "Sibling") (state (state . "running"))))
      (cl-flet ((ask-both (permission form)
                  ;; Child 1's permission, then child 2's newer form.
                  (agent-shell-subagent-tests--request-permission shell permission)
                  (agent-shell-subagent-tests--ask shell form :session-id "child-2")
                  (should (equal (agent-shell-subagent-tests--armed-for state) form))))
        (agent-shell-subagent-tests--with-responses responses
          ;; Reported as cancelled.
          (ask-both 701 702)
          (agent-shell-subagent-tests--route
           shell agent-shell-subagent-tests--parent
           '((sessionUpdate . "subagent_update") (sessionId . "child-1")
             (state (state . "idle") (stopReason . "cancelled"))))
          (should (equal responses '((701 . "cancelled"))))
          (should (equal (agent-shell-subagent-tests--armed-for state) 702))
          ;; Answered by the user.
          (setq responses nil)
          (agent-shell-subagent-tests--route
           shell agent-shell-subagent-tests--parent
           '((sessionUpdate . "subagent_update") (sessionId . "child-1")
             (state (state . "running"))))
          (ask-both 703 704)
          (funcall (agent-shell-subagent-tests--permission-key
                    shell "y" "From sub-agent: Test investigator"))
          (should (equal responses '((703 . "selected"))))
          (should-not (map-elt state :idle-timer))
          ;; Interrupted by the user, from its own buffer.
          (setq responses nil)
          (ask-both 705 706)
          (map-put! (agent-shell-subagent--record state "child-1") :can-cancel t)
          (with-current-buffer (agent-shell-subagent--ensure-buffer
                                :state state :session-id "child-1")
            (agent-shell-interrupt t))
          (should (equal responses '((705 . "cancelled"))))
          (should-not (map-elt state :idle-timer))
          ;; A failed restore, connected, with the main session waiting.
          (setq responses nil)
          (agent-shell-subagent-tests--request-permission shell 707)
          (with-current-buffer shell
            (agent-shell--start-idle-timer :event 'permission-request
                                           :data '((:request-id . 999))))
          (agent-shell-subagent--discard-restore state)
          (should (assoc 707 responses))
          (should (equal (agent-shell-subagent-tests--armed-for state) 999))))
      (with-current-buffer shell
        (agent-shell--cancel-idle-timer)))))

;;; Public API

(ert-deftest agent-shell-subagents-test ()
  "List, open, and follow sub-agents through the public API."
  (agent-shell-subagent-tests--with-shell shell
    (let ((events nil))
      (agent-shell-subscribe-to
       :shell-buffer shell
       :event 'subagent-update
       :on-event (lambda (event)
                   (push (map-elt event :data) events)))
      (let ((subagent (car (agent-shell-subagents :shell-buffer shell))))
        (should (length= (agent-shell-subagents :shell-buffer shell) 1))
        (should (equal (map-elt subagent :session-id) "child-1"))
        (should (equal (map-elt subagent :parent-session-id)
                       agent-shell-subagent-tests--parent))
        (should (equal (map-elt subagent :title) "Test investigator"))
        (should (equal (map-elt subagent :status) "done"))
        (should (equal (map-elt subagent :result) "The test races on a timer"))
        (should (equal (map-elt subagent :depth) 1))
        (should (equal (map-elt subagent :pending-requests) 0))
        (should (eq (map-elt subagent :shell-buffer) shell))
        (should-not (map-elt subagent :buffer)))
      ;; Requests held by the main shell count as pending.
      (cl-letf (((symbol-function 'acp-send-response) #'ignore))
        (agent-shell-subagent-tests--request-permission shell 44))
      (should (equal (map-elt (car (agent-shell-subagents :shell-buffer shell))
                              :pending-requests)
                     1))
      ;; Opening, in the background.
      (let ((child (agent-shell-subagent-buffer :shell-buffer shell
                                                :session-id "child-1")))
        (should (buffer-live-p child))
        (should-not (get-buffer-window child t))
        (should (eq (map-nested-elt (car events) '(:subagent :buffer)) child))
        (should (equal (map-elt (car events) :count) 1))
        ;; Listed the same from the sub-agent's own buffer.
        (should (eq (map-elt (car (agent-shell-subagents :shell-buffer child)) :buffer)
                    child))
        (should (eq (agent-shell-subagent-buffer :shell-buffer child
                                                 :session-id "child-1")
                    child))
        ;; Killing.
        (setq events nil)
        (kill-buffer child)
        (should (length= events 1))
        (should (equal (map-nested-elt (car events) '(:subagent :session-id)) "child-1"))
        (should-not (map-nested-elt (car events) '(:subagent :buffer)))
        (should-not (map-elt (car (agent-shell-subagents :shell-buffer shell)) :buffer)))
      (should-error (agent-shell-subagent-buffer :shell-buffer shell :session-id "nope"))
      (should-error (agent-shell-subagents :shell-buffer (get-buffer-create " *not a shell*"))))))

(provide 'agent-shell-subagent-tests)

;;; agent-shell-subagent-tests.el ends here
