;;; agent-shell-cursor-tests.el --- Tests for agent-shell-cursor -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-cursor)

;;; Code:

(ert-deftest agent-shell-cursor--make-text-content-block-test ()
  "Test `agent-shell-cursor--make-text-content-block'."
  (should (equal (agent-shell-cursor--make-text-content-block "hello")
                 '((type . "content")
                   (content (type . "text") (text . "hello")))))
  (should-not (agent-shell-cursor--make-text-content-block ""))
  (should-not (agent-shell-cursor--make-text-content-block nil)))

(ert-deftest agent-shell-cursor--content-from-raw-output-test ()
  "Test `agent-shell-cursor--content-from-raw-output'."
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((error . "permission denied")))
                 (list '((type . "content")
                         (content (type . "text") (text . "permission denied"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((content . "file contents")))
                 (list '((type . "content")
                         (content (type . "text") (text . "file contents"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((stdout . "hello")
                    (stderr . "warning")
                    (exitCode . 0)))
                 (list '((type . "content")
                         (content (type . "text")
                                  (text . "```\nExit code: 0\n\nhello\n\nwarning\n```"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((stdout . "done")))
                 (list '((type . "content")
                         (content (type . "text") (text . "```\ndone\n```"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((exitCode . 1)))
                 (list '((type . "content")
                         (content (type . "text") (text . "```\nExit code: 1\n```"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((totalMatches . 42)))
                 (list '((type . "content")
                         (content (type . "text") (text . "42 matches"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((totalMatches . 42)
                    (truncated . t)))
                 (list '((type . "content")
                         (content (type . "text") (text . "42 matches (truncated)"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((resultCount . 7)))
                 (list '((type . "content")
                         (content (type . "text") (text . "7 results"))))))
  (should-not (agent-shell-cursor--content-from-raw-output nil))
  (should-not (agent-shell-cursor--content-from-raw-output '((unknown . "value")))))

(ert-deftest agent-shell-cursor--notification-adapter-test ()
  "Test `agent-shell-cursor--notification-adapter'."
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (rawOutput (stdout . "hello")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "```\nhello\n```")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (content . ((type . "content")
                                       (content (type . "text")
                                                (text . "keep me"))))
                           (rawOutput (stdout . "ignored")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   '((type . "content")
                     (content (type . "text") (text . "keep me"))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (content . ())
                           (rawOutput (stdout . "hello")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "```\nhello\n```")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "in_progress")
                           (rawOutput (error . "permission denied")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "permission denied")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (rawOutput (content . "file contents")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "file contents")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (rawOutput (totalMatches . 42)
                                      (truncated . t)))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "42 matches (truncated)")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (rawOutput (unknown . "value")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (null (map-nested-elt notification '(params update content)))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed"))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (null (map-nested-elt notification '(params update content)))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "agent_message_chunk")
                           (content . "hello"))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   "hello")))
  (let ((notification
         '((method . "session/request")
           (params (foo . "bar")))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal notification
                   '((method . "session/request")
                     (params (foo . "bar")))))))

(ert-deftest agent-shell--adapt-notification-cursor-integration-test ()
  "Test `agent-shell--adapt-notification' with Cursor config."
  (let* ((config (agent-shell-cursor-make-agent-config))
         (state (agent-shell--make-state :agent-config config))
         (notification
          '((method . "session/update")
            (params (update (sessionUpdate . "tool_call_update")
                            (status . "completed")
                            (rawOutput (stdout . "hello")))))))
    (agent-shell--adapt-notification :state state :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "```\nhello\n```"))))))))


(ert-deftest agent-shell-cursor--make-extension-response-test ()
  "Test `agent-shell-cursor--make-extension-response'."
  (let ((accepted (agent-shell-cursor--make-extension-response
                   :request-id "req-1"
                   :outcome '((outcome . "accepted"))))
        (rejected (agent-shell-cursor--make-extension-response
                   :request-id 7
                   :outcome '((outcome . "rejected")
                              (reason . "nope")))))
    (should (equal (map-elt accepted :request-id) "req-1"))
    (should (equal (map-nested-elt accepted '(:result outcome outcome))
                   "accepted"))
    (should (equal (map-elt rejected :request-id) 7))
    (should (equal (map-nested-elt rejected '(:result outcome outcome))
                   "rejected"))
    (should (equal (map-nested-elt rejected '(:result outcome reason))
                   "nope"))))

(ert-deftest agent-shell-cursor--format-create-plan-body-test ()
  "Test `agent-shell-cursor--format-create-plan-body'."
  (let* ((todos (vector
                 '((id . "t1") (content . "Inspect") (status . "completed"))
                 '((id . "t2") (content . "Update") (status . "pending"))))
         (body (agent-shell-cursor--format-create-plan-body
                `((name . "Refactor")
                  (overview . "Tighten layout.")
                  (plan . "1. Inspect\n2. Update")
                  (todos . ,todos)))))
    (should (string-match-p "Refactor" body))
    (should (string-match-p "Tighten layout" body))
    (should (string-match-p "1\\. Inspect" body))
    (should (string-match-p "Inspect" body))
    (should (string-match-p "Update" body))))

(ert-deftest agent-shell-cursor--on-request-create-plan-test ()
  "Test `agent-shell-cursor--on-request' handles cursor/create_plan."
  (with-temp-buffer
    (let* ((captured-response nil)
           (fragments nil)
           (state `((:buffer . ,(current-buffer))
                    (:client . test-client)
                    (:event-subscriptions . nil)
                    (:idle-timer . nil)
                    (:last-entry-type . nil))))
      (cl-letf (((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest args)
                   (push (plist-get args :block-id) fragments)))
                ((symbol-function 'agent-shell-cursor--make-create-plan-dialog)
                 (lambda (&rest _) "dialog"))
                ((symbol-function 'agent-shell-jump-to-latest-permission-button-row)
                 (lambda ()))
                ((symbol-function 'agent-shell-viewport--buffer)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-shell--emit-event)
                 (lambda (&rest _)))
                ((symbol-function 'agent-shell--start-idle-timer)
                 (lambda (&rest _)))
                ((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (setq captured-response (plist-get args :response)))))
        (should (agent-shell-cursor--on-request
                 :state state
                 :acp-request
                 '((id . "req-plan")
                   (method . "cursor/create_plan")
                   (params . ((toolCallId . "call-1")
                              (name . "Plan")
                              (overview . "Do the thing")
                              (plan . "Step one")
                              (todos . []))))))
        (should (equal (map-elt state :last-entry-type) "cursor/create_plan"))
        (should (member "cursor-create-plan-call-1" fragments))
        (should (member "cursor-create-plan-call-1-body" fragments))
        (should-not captured-response)))))

(ert-deftest agent-shell--on-request-cursor-create-plan-integration-test ()
  "Test `agent-shell--on-request' dispatches cursor/create_plan."
  (with-temp-buffer
    (let* ((handled nil)
           (state `((:buffer . ,(current-buffer))
                    (:client . test-client)
                    (:last-entry-type . nil))))
      (cl-letf (((symbol-function 'agent-shell-cursor--on-request)
                 (lambda (&rest _)
                   (setq handled t)
                   t))
                ((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest _)))
                ((symbol-function 'acp-send-response)
                 (lambda (&rest _)
                   (error "Should not send method-not-found for create_plan"))))
        (agent-shell--on-request
         :state state
         :acp-request '((id . "req-plan")
                        (method . "cursor/create_plan")
                        (params . ((plan . "x")))))
        (should handled)))))

(ert-deftest agent-shell-cursor--on-request-unknown-method-test ()
  "Test `agent-shell-cursor--on-request' ignores unknown cursor methods."
  (should-not (agent-shell-cursor--on-request
               :state nil
               :acp-request '((id . "1")
                              (method . "cursor/unknown_method")))))

(ert-deftest agent-shell-cursor--merge-todos-test ()
  "Test `agent-shell-cursor--merge-todos'."
  (let* ((existing (list '((id . "1") (content . "A") (status . "pending"))
                         '((id . "2") (content . "B") (status . "pending"))))
         (incoming (vector '((id . "2") (content . "B2") (status . "completed"))
                           '((id . "3") (content . "C") (status . "pending"))))
         (replaced (agent-shell-cursor--merge-todos
                    :existing existing
                    :incoming incoming
                    :merge nil))
         (merged (agent-shell-cursor--merge-todos
                  :existing existing
                  :incoming incoming
                  :merge t)))
    (should (equal replaced (append incoming nil)))
    (should (equal merged
                   (list '((id . "1") (content . "A") (status . "pending"))
                         '((id . "2") (content . "B2") (status . "completed"))
                         '((id . "3") (content . "C") (status . "pending")))))))

(ert-deftest agent-shell-cursor--on-request-update-todos-test ()
  "Test `agent-shell-cursor--on-request' handles cursor/update_todos."
  (with-temp-buffer
    (let* ((captured-response nil)
           (fragment-body nil)
           (todos (vector
                   '((id . "1") (content . "New") (status . "completed"))
                   '((id . "2") (content . "Next") (status . "pending"))))
           (request `((id . "req-todos")
                      (method . "cursor/update_todos")
                      (params (toolCallId . "call-t")
                              (merge . t)
                              (todos . ,todos))))
           (state `((:buffer . ,(current-buffer))
                    (:client . test-client)
                    (:event-subscriptions . nil)
                    (:cursor-todos . (((id . "1")
                                       (content . "Old")
                                       (status . "pending"))))
                    (:last-entry-type . nil))))
      (cl-letf (((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest args)
                   (setq fragment-body (plist-get args :body))))
                ((symbol-function 'agent-shell--active-requests-p)
                 (lambda (_) t))
                ((symbol-function 'agent-shell--emit-event)
                 (lambda (&rest _)))
                ((symbol-function 'agent-shell--cancel-idle-timer)
                 (lambda ()))
                ((symbol-function 'agent-shell-jump-to-latest-permission-button-row)
                 (lambda ()))
                ((symbol-function 'agent-shell-viewport--buffer)
                 (lambda (&rest _) nil))
                ((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (setq captured-response (plist-get args :response)))))
        (should (agent-shell-cursor--on-request
                 :state state
                 :acp-request request))
        (should (equal (map-elt state :last-entry-type) "cursor/update_todos"))
        (should (equal (map-elt state :cursor-todos)
                       (list '((id . "1") (content . "New") (status . "completed"))
                             '((id . "2") (content . "Next") (status . "pending")))))
        (should (string-match-p "New" fragment-body))
        (should (string-match-p "Next" fragment-body))
        (should (equal (map-elt captured-response :request-id) "req-todos"))
        (should (equal (map-nested-elt captured-response '(:result outcome outcome))
                       "accepted"))
        (should (= (length (map-nested-elt captured-response
                                           '(:result outcome todos)))
                   2))))))

(provide 'agent-shell-cursor-tests)
;;; agent-shell-cursor-tests.el ends here
