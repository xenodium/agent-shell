;;; agent-shell-openai-tests.el --- Tests for agent-shell-openai -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-openai)

;;; Code:

(ert-deftest agent-shell-openai-default-model-id-test ()
  "Test that Codex config exposes default model id."
  (let ((default-model-id-fn
         (map-elt (agent-shell-openai-make-codex-config) :default-model-id)))

    (let ((agent-shell-openai-default-model-id nil))
      (should (null (funcall default-model-id-fn))))

    (let ((agent-shell-openai-default-model-id "gpt-5.4/low"))
      (should (string= (funcall default-model-id-fn) "gpt-5.4/low")))

    (let ((agent-shell-openai-default-model-id (lambda () "gpt-5.4/low")))
      (should (string= (funcall default-model-id-fn) "gpt-5.4/low")))))

(ert-deftest agent-shell-openai-default-config-options-test ()
  "Test that Codex config exposes default config options."
  (let ((default-config-options-fn
         (map-elt (agent-shell-openai-make-codex-config) :default-config-options)))

    (let ((agent-shell-openai-default-config-options nil))
      (should (null (funcall default-config-options-fn))))

    (let ((agent-shell-openai-default-config-options
           '(("model" . "gpt-5.4")
             ("reasoning_effort" . "high"))))
      (should (equal (funcall default-config-options-fn)
                     '(("model" . "gpt-5.4")
                       ("reasoning_effort" . "high")))))))

(ert-deftest agent-shell-openai-default-session-mode-id-test ()
  "Test that Codex config exposes default session mode id."
  (let ((default-session-mode-id-fn
         (map-elt (agent-shell-openai-make-codex-config) :default-session-mode-id)))

    (let ((agent-shell-openai-default-session-mode-id nil))
      (should (null (funcall default-session-mode-id-fn))))

    (let ((agent-shell-openai-default-session-mode-id "full-access"))
      (should (string= (funcall default-session-mode-id-fn) "full-access")))))

(ert-deftest agent-shell-openai-codex-does-not-eagerly-authenticate-test ()
  "Test that Codex lets codex-acp decide when auth is needed."
  (let ((config (agent-shell-openai-make-codex-config)))
    (should-not (map-elt config :needs-authentication))
    (should-not (map-elt config :authenticate-request-maker))))

(ert-deftest agent-shell-openai-codex-login-default-auth-request-test ()
  "Test that Codex login auth uses the current chat-gpt method id."
  (let* ((agent-shell-openai-authentication
          (agent-shell-openai-make-authentication :login t))
         (request (json-parse-string (agent-shell-openai--codex-default-auth-request)
                                     :object-type 'alist)))
    (should (string= (map-elt request 'methodId) "chat-gpt"))))

(ert-deftest agent-shell-openai-codex-api-key-default-auth-request-test ()
  "Test that Codex API key auth sends key metadata."
  (let* ((agent-shell-openai-authentication
          (agent-shell-openai-make-authentication :api-key "openai-secret"))
         (request (json-parse-string (agent-shell-openai--codex-default-auth-request)
                                     :object-type 'alist)))
    (should (string= (map-elt request 'methodId) "api-key"))
    (should (string= (map-nested-elt request '(_meta api-key apiKey))
                     "openai-secret"))))

(ert-deftest agent-shell-openai-codex-key-default-auth-request-test ()
  "Test that Codex-specific API key auth sends key metadata."
  (let* ((agent-shell-openai-authentication
          (agent-shell-openai-make-authentication :codex-api-key "codex-secret"))
         (request (json-parse-string (agent-shell-openai--codex-default-auth-request)
                                     :object-type 'alist)))
    (should (string= (map-elt request 'methodId) "api-key"))
    (should (string= (map-nested-elt request '(_meta api-key apiKey))
                     "codex-secret"))))

(defun agent-shell-openai-tests--initialize-state ()
  "Initialize Codex shell state in the current test buffer."
  (setq major-mode 'agent-shell-mode)
  (setq-local agent-shell--state
              (agent-shell--make-state
               :buffer (current-buffer)
               :agent-config (agent-shell-openai-make-codex-config))))

(cl-defun agent-shell-openai-tests--on-notification (&key state acp-notification)
  "Adapt ACP-NOTIFICATION and dispatch it to the core handler with STATE."
  (agent-shell--on-notification
   :state state
   :acp-notification (agent-shell--adapt-notification
                      :state state :acp-notification (copy-tree acp-notification t))))

(defun agent-shell-openai-tests--adapt-update (update)
  "Return a fresh adapted Codex tool-call UPDATE."
  (map-nested-elt
   (agent-shell--adapt-notification
    :state agent-shell--state
    :acp-notification
    (copy-tree `((method . "session/update")
                 (params (update (sessionUpdate . "tool_call_update")
                                 (toolCallId . "one") ,@update))) t))
   '(params update)))

(ert-deftest agent-shell-openai-notification-adapter-config-test ()
  "Codex configuration installs its notification adapter."
  (should (eq (map-elt (agent-shell-openai-make-codex-config) :notification-adapter)
              #'agent-shell-openai--notification-adapter)))

(ert-deftest agent-shell-openai-tool-output-deltas-survive-completion-test ()
  "Keep interleaved command output in both fragments and the transcript."
  (dolist (status '("completed" "failed"))
    (with-temp-buffer
      (let ((state (agent-shell-openai-tests--initialize-state))
            fragments transcripts)
        (cl-letf (((symbol-function 'agent-shell--update-fragment)
                   (lambda (&rest args)
                     (setf (map-elt fragments (plist-get args :block-id))
                           (plist-get args :body))))
                  ((symbol-function 'agent-shell--append-transcript)
                   (lambda (&rest args) (push (plist-get args :text) transcripts)))
                  ((symbol-function 'agent-shell--refresh-activity-group-header) #'ignore)
                  ((symbol-function 'agent-shell--sync-activity-group-fold) #'ignore)
                  ((symbol-function 'agent-shell--delete-fragment) #'ignore)
                  ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
                  ((symbol-function 'agent-shell-make-tool-call-label)
                   (lambda (&rest _) '((:status . "done") (:title . "command")))))
          (dolist (update
                   `(((toolCallId . "one") (status . "in_progress")
                      (rawInput (command . "printf one"))
                      (_meta (terminal_output_delta (terminal_id . "one") (data . "first\n"))))
                     ((toolCallId . "two") (status . "in_progress")
                      (_meta (terminal_output_delta (terminal_id . "two") (data . "other\n"))))
                     ((toolCallId . "one")
                      (_meta (terminal_output_delta (terminal_id . "one") (data . "second\n"))
                             (terminal_exit (terminal_id . "one") (exit_code . 0))))
                     ((toolCallId . "one") (status . ,status))
                     ((toolCallId . "two") (status . "completed"))))
            (agent-shell-openai-tests--on-notification
             :state state
             :acp-notification
             `((method . "session/update")
               (params (update (sessionUpdate . "tool_call_update") ,@update)))))
          (should (equal (map-elt fragments "one")
                         "```console\nprintf one\n```\n\n```\nfirst\nsecond\n```"))
          (should (equal (map-elt fragments "two") "```\nother\n```"))
          (should (string-search "first\nsecond" (seq-elt transcripts 1)))
          (should (string-search "other" (seq-first transcripts)))
          ;; A later metadata-only update must not erase the completed output.
          (agent-shell-openai-tests--on-notification
           :state state
           :acp-notification
           '((method . "session/update")
             (params (update (sessionUpdate . "tool_call_update")
                             (toolCallId . "one") (title . "Renamed command")))))
          (should (string-suffix-p "first\nsecond\n```" (map-elt fragments "one"))))))))

(ert-deftest agent-shell-openai-tool-output-completed-delta-test ()
  "Display output delivered only with a command's completion."
  (with-temp-buffer
    (let ((state (agent-shell-openai-tests--initialize-state))
          chat transcript)
      (cl-letf (((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest args) (setq chat (plist-get args :body))))
                ((symbol-function 'agent-shell--append-transcript)
                 (lambda (&rest args) (setq transcript (plist-get args :text))))
                ((symbol-function 'agent-shell--refresh-activity-group-header) #'ignore)
                ((symbol-function 'agent-shell--sync-activity-group-fold) #'ignore)
                ((symbol-function 'agent-shell--delete-fragment) #'ignore)
                ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
                ((symbol-function 'agent-shell-make-tool-call-label)
                 (lambda (&rest _) '((:status . "done") (:title . "command")))))
        (agent-shell-openai-tests--on-notification
         :state state
         :acp-notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (toolCallId . "one") (status . "completed")
                           (_meta (terminal_output_delta (terminal_id . "one")
                                                         (data . "visible output\n"))
                                  (terminal_exit (terminal_id . "one") (exit_code . 0)))))))
        (should (equal chat "```\nvisible output\n```"))
        (should (string-search "visible output" transcript))))))

(ert-deftest agent-shell-openai-output-snapshots-replace-deltas-test ()
  "Complete snapshots replace deltas and survive subsequent updates."
  (with-temp-buffer
    (agent-shell-openai-tests--initialize-state)
    (dolist (case '((((_meta (terminal_output_delta (data . "again\n")))) . "```\nagain\n```")
                    (((_meta (terminal_output_delta (data . "again\n"))))
                     . "```\nagain\nagain\n```")
                    (((rawOutput (formatted_output . "complete\n"))
                      (_meta (terminal_output_delta (data . "ignored")))) . "complete\n")
                    (nil . "```\ncomplete\n```")
                    (((content . [((type . "content")
                                   (content (type . "text") (text . "snapshot")))])
                      (rawOutput (formatted_output . "ignored"))
                      (_meta (terminal_output_delta (data . "ignored")))) . "snapshot")
                    (((_meta (terminal_output_delta (data . 42)))) . "```\nsnapshot\n```")
                    (((rawOutput (formatted_output . ""))) . "")
                    (nil . "")
                    (((_meta (terminal_output_delta (data . "fresh")))) . "```\nfresh\n```")))
      (should (equal
               (agent-shell--tool-call-update-output-markdown
                (agent-shell-openai-tests--adapt-update (car case)))
               (cdr case))))))

(ert-deftest agent-shell-openai-output-content-preserved-test ()
  "Keep structured snapshots and non-text content alongside deltas."
  (with-temp-buffer
    (agent-shell-openai-tests--initialize-state)
    (let* ((content [((type . "content")
                      (content (type . "image") (mimeType . "image/png") (data . "YWJj")))])
           (update (agent-shell-openai-tests--adapt-update
                    `((content . ,content)
                      (_meta (terminal_output_delta (data . "ignored")))))))
      (should (equal (map-elt update 'content) content)))
    (agent-shell--emit-event :event 'turn-complete)
    (let* ((diff '((type . "diff") (path . "file") (oldText . "a") (newText . "b")))
           (update (agent-shell-openai-tests--adapt-update
                    `((content . [,diff])
                      (_meta (terminal_output_delta (data . "output")))))))
      (should (equal (seq-first (map-elt update 'content)) diff))
      (should (equal (agent-shell--tool-call-update-output-markdown update) "```\noutput\n```")))))

(ert-deftest agent-shell-openai-output-buffer-isolation-test ()
  "Identical tool-call IDs in separate shells do not share output."
  (with-temp-buffer
    (agent-shell-openai-tests--initialize-state)
    (agent-shell-openai-tests--adapt-update
     '((_meta (terminal_output_delta (data . "outer")))))
    (with-temp-buffer
      (agent-shell-openai-tests--initialize-state)
      (should (equal
               (agent-shell--tool-call-update-output-markdown
                (agent-shell-openai-tests--adapt-update
                 '((_meta (terminal_output_delta (data . "inner"))))))
               "```\ninner\n```")))
    (should (equal
             (agent-shell--tool-call-update-output-markdown
              (agent-shell-openai-tests--adapt-update nil))
             "```\nouter\n```"))))

(ert-deftest agent-shell-openai-output-unrelated-notifications-test ()
  "Leave other notifications, malformed deltas and non-streamed calls unchanged."
  (with-temp-buffer
    (agent-shell-openai-tests--initialize-state)
    (dolist (notification
             '(nil
               ((method . "other"))
               ((method . "session/update")
                (params (update (sessionUpdate . "agent_message_chunk")
                                (content (type . "text") (text . "hello")))))
               ((method . "session/update")
                (params (update (sessionUpdate . "tool_call_update")
                                (_meta (terminal_output_delta (data . "missing ID"))))))
               ((method . "session/update")
                (params (update (sessionUpdate . "tool_call_update")
                                (toolCallId . "one")
                                (_meta (terminal_output_delta (data . 42))))))
               ((method . "session/update")
                (params (update (sessionUpdate . "tool_call_update")
                                (toolCallId . "read") (status . "completed")
                                (content . [((type . "content")
                                             (content (type . "text") (text . "file")))]))))))
      (should (equal (agent-shell-openai--notification-adapter
                      :acp-notification (copy-tree notification t))
                     notification)))
    (should-not agent-shell-openai--terminal-output-snapshots)
    (should-not (map-elt agent-shell--state :event-subscriptions))))

(ert-deftest agent-shell-openai-output-cleanup-test ()
  "Clear snapshots and their subscription when a turn or session ends."
  (dolist (event '(turn-complete session-restored session-selected init-session error clean-up))
    (with-temp-buffer
      (agent-shell-openai-tests--initialize-state)
      (dotimes (_ 2)
        (let ((update (agent-shell-openai-tests--adapt-update
                       '((_meta (terminal_output_delta (data . "retained")))))))
          (agent-shell-openai-tests--adapt-update '((status . "completed")))
          (should (= (length (map-elt agent-shell--state :event-subscriptions)) 1))
          (agent-shell--emit-event :event event :data '((:stop-reason . "cancelled")))
          (should-not agent-shell-openai--terminal-output-snapshots)
          (should-not (map-elt agent-shell--state :event-subscriptions))
          (should (equal (agent-shell--tool-call-update-output-markdown update)
                         "```\nretained\n```"))
          (should (equal (agent-shell--tool-call-update-output-markdown
                          (agent-shell-openai-tests--adapt-update nil)) "")))))))

(ert-deftest agent-shell-openai-output-restored-history-test ()
  "Adapt streamed output before buffering and replaying restored history."
  (with-temp-buffer
    (comint-mode)
    (agent-shell-openai-tests--initialize-state)
    (map-put! agent-shell--state :supports-session-load t)
    (map-put! agent-shell--state :pending-restore (agent-shell--make-pending-restore))
    (setq-local shell-maker--config
                (agent-shell--make-shell-maker-config :prompt "Test> "))
    (setq-local comint-last-output-start (copy-marker (point)))
    (insert (propertize "Test> " 'field 'prompt))
    (setq-local comint-last-prompt (cons (copy-marker 1) (copy-marker (point))))
    (let ((agent-shell-tool-use-expand-by-default nil)
          (agent-shell-activity-group-expand-by-default t)
          (process (make-pipe-process :name "agent-shell-output-test"
                                      :buffer (current-buffer) :noquery t
                                      :sentinel #'ignore)))
      (unwind-protect
          (cl-letf (((symbol-function 'shell-maker--process) (lambda () process))
                    ((symbol-function 'agent-shell--append-transcript) #'ignore)
                    ((symbol-function 'agent-shell--create-bootstrapping-placeholders) #'ignore))
            (dolist (update
                     '(((status . "in_progress") (kind . "execute")
                        (title . "Run command") (rawInput (command . "printf result"))
                        (_meta (terminal_output_delta (data . "first output line\n"))))
                       ((status . "completed")
                        (_meta (terminal_output_delta (data . "last output line\n"))))))
              (agent-shell-openai-tests--on-notification
               :state agent-shell--state
               :acp-notification
               `((method . "session/update")
                 (params (update (sessionUpdate . "tool_call_update")
                                 (toolCallId . "command") ,@update)))))
            (should-not (string-search "first output line" (buffer-string)))
            (agent-shell--render-pending-restore agent-shell--state)
            (should (string-search "first output line\nlast output line" (buffer-string)))
            (should-not (map-elt agent-shell--state :pending-restore))
            (should-not agent-shell-openai--terminal-output-snapshots))
        (delete-process process)))))

(provide 'agent-shell-openai-tests)
;;; agent-shell-openai-tests.el ends here
