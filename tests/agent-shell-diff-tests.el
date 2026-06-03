;;; agent-shell-diff-tests.el --- Tests for agent-shell-diff -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-diff)

;;; Code:

(ert-deftest agent-shell-diff-returns-buffer-test ()
  "Test that `agent-shell-diff' returns the newly created diff buffer."
  (let ((buf (agent-shell-diff
              :old "hello\n"
              :new "world\n"
              :file "test.el")))
    (unwind-protect
        (progn
          (should (bufferp buf))
          (should (buffer-live-p buf))
          (should (with-current-buffer buf
                    (derived-mode-p 'diff-mode))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest agent-shell-diff-on-exit-fires-on-kill-test ()
  "Test that ON-EXIT callback fires when the diff buffer is killed."
  (let* ((on-exit-called nil)
         (buf (agent-shell-diff
               :old "hello\n"
               :new "world\n"
               :file "test.el"
               :on-exit (lambda () (setq on-exit-called t)))))
    (unwind-protect
        (progn
          (should (buffer-live-p buf))
          (kill-buffer buf)
          (should on-exit-called))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest agent-shell-diff-kill-buffer-suppresses-on-exit-test ()
  "Test that `agent-shell-diff-kill-buffer' kills without calling ON-EXIT."
  (let* ((on-exit-called nil)
         (buf (agent-shell-diff
               :old "hello\n"
               :new "world\n"
               :file "test.el"
               :on-exit (lambda () (setq on-exit-called t)))))
    (unwind-protect
        (progn
          (should (buffer-live-p buf))
          (agent-shell-diff-kill-buffer buf)
          (should-not (buffer-live-p buf))
          (should-not on-exit-called))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest agent-shell-diff-kill-buffer-noop-when-dead-test ()
  "Test that `agent-shell-diff-kill-buffer' is safe on already-dead buffers."
  (let ((buf (generate-new-buffer "*test-diff*")))
    (kill-buffer buf)
    ;; Should not error.
    (agent-shell-diff-kill-buffer buf)))

(ert-deftest agent-shell-diff-kill-buffer-noop-when-nil-test ()
  "Test that `agent-shell-diff-kill-buffer' is safe when called with nil."
  ;; Should not error.
  (agent-shell-diff-kill-buffer nil))

(ert-deftest agent-shell-diff-on-exit-skipped-when-calling-buffer-dead-test ()
  "Test that ON-EXIT is skipped without error when calling buffer is dead."
  (let ((on-exit-called nil)
        (calling-buf (generate-new-buffer " *test-calling*")))
    (let ((buf (with-current-buffer calling-buf
                 (agent-shell-diff
                  :old "hello\n"
                  :new "world\n"
                  :file "test.el"
                  :on-exit (lambda () (setq on-exit-called t))))))
      (unwind-protect
          (progn
            (kill-buffer calling-buf)
            (should (buffer-live-p buf))
            (kill-buffer buf)
            (should-not on-exit-called))
        (when (buffer-live-p buf)
          (kill-buffer buf))
        (when (buffer-live-p calling-buf)
          (kill-buffer calling-buf))))))

;;; Integration tests — diff buffer lifecycle in agent-shell state

(require 'agent-shell)

(ert-deftest agent-shell-diff-tracked-in-tool-call-state-test ()
  "Test that invoking the diff viewer stores the buffer in tool-call state."
  (let* ((shell-buf (generate-new-buffer " *test-shell*"))
         (tool-data (list (cons :status "pending")))
         (state (list (cons :buffer shell-buf)
                      (cons :tool-calls
                            (list (cons "tc-1" tool-data)))))
         (diff (list (cons :old "hello\n")
                     (cons :new "world\n")
                     (cons :file "test.el")))
         (view-fn (with-current-buffer shell-buf
                    (setq major-mode 'agent-shell-mode)
                    (agent-shell--make-diff-viewing-function
                     :diff diff
                     :actions nil
                     :client nil
                     :request-id "req-1"
                     :state state
                     :tool-call-id "tc-1"))))
    (unwind-protect
        (let ((diff-buf (progn (funcall view-fn)
                               (map-nested-elt state '(:tool-calls "tc-1" :diff-buffer)))))
          (should (bufferp diff-buf))
          (should (buffer-live-p diff-buf)))
      (when-let* ((diff-buf (map-nested-elt state '(:tool-calls "tc-1" :diff-buffer))))
        (when (buffer-live-p diff-buf)
          (agent-shell-diff-kill-buffer diff-buf)))
      (when (buffer-live-p shell-buf)
        (kill-buffer shell-buf)))))

(ert-deftest agent-shell-diff-reuses-existing-buffer-test ()
  "Test that invoking the diff viewer twice reuses the same buffer."
  (let* ((shell-buf (generate-new-buffer " *test-shell*"))
         (state (list (cons :buffer shell-buf)
                      (cons :tool-calls
                            (list (cons "tc-1" (list (cons :status "pending")))))))
         (diff (list (cons :old "hello\n")
                     (cons :new "world\n")
                     (cons :file "test.el")))
         (view-fn (with-current-buffer shell-buf
                    (setq major-mode 'agent-shell-mode)
                    (agent-shell--make-diff-viewing-function
                     :diff diff
                     :actions nil
                     :client nil
                     :request-id "req-1"
                     :state state
                     :tool-call-id "tc-1"))))
    (unwind-protect
        (progn
          (funcall view-fn)
          (let ((first-buf (map-nested-elt state '(:tool-calls "tc-1" :diff-buffer))))
            (should (buffer-live-p first-buf))
            (funcall view-fn)
            (should (eq first-buf (map-nested-elt state '(:tool-calls "tc-1" :diff-buffer))))))
      (when-let* ((diff-buf (map-nested-elt state '(:tool-calls "tc-1" :diff-buffer))))
        (when (buffer-live-p diff-buf)
          (agent-shell-diff-kill-buffer diff-buf)))
      (when (buffer-live-p shell-buf)
        (kill-buffer shell-buf)))))

(ert-deftest agent-shell-diff-killed-on-permission-response-test ()
  "Test that `agent-shell--send-permission-response' kills a tracked diff buffer."
  (let* ((diff-buf (agent-shell-diff
                    :old "hello\n"
                    :new "world\n"
                    :file "test.el"))
         (shell-buf (generate-new-buffer " *test-shell*"))
         (tool-data (list (cons :status "pending")
                          (cons :diff-buffer diff-buf)))
         (state (list (cons :buffer shell-buf)
                      (cons :tool-calls
                            (list (cons "tc-1" tool-data))))))
    (unwind-protect
        (progn
          (should (buffer-live-p diff-buf))
          (cl-letf (((symbol-function 'acp-send-response) #'ignore)
                    ((symbol-function 'acp-make-session-request-permission-response) #'ignore)
                    ((symbol-function 'agent-shell--delete-fragment) #'ignore)
                    ((symbol-function 'agent-shell--emit-event) #'ignore)
                    ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
                    ((symbol-function 'agent-shell-jump-to-latest-permission-button-row) #'ignore)
                    ((symbol-function 'agent-shell-viewport--buffer) (lambda (&rest _) nil)))
            (with-current-buffer shell-buf
              (agent-shell--send-permission-response
               :client nil
               :request-id "req-1"
               :option-id "opt-1"
               :state state
               :tool-call-id "tc-1")))
          (should-not (buffer-live-p diff-buf)))
      (when (buffer-live-p diff-buf)
        (kill-buffer diff-buf))
      (when (buffer-live-p shell-buf)
        (kill-buffer shell-buf)))))

(ert-deftest agent-shell-diff-killed-on-shell-clean-up-test ()
  "Test that `agent-shell--clean-up' kills tracked diff buffers."
  (let* ((diff-buf (agent-shell-diff
                    :old "hello\n"
                    :new "world\n"
                    :file "test.el"))
         (shell-buf (generate-new-buffer " *test-shell*"))
         (tool-data (list (cons :status "pending")
                          (cons :diff-buffer diff-buf)))
         (state (list (cons :buffer shell-buf)
                      (cons :tool-calls
                            (list (cons "tc-1" tool-data)))
                      (cons :idle-timer nil))))
    (unwind-protect
        (progn
          (should (buffer-live-p diff-buf))
          (with-current-buffer shell-buf
            (setq major-mode 'agent-shell-mode)
            (setq-local agent-shell--state state)
            (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t))
          (cl-letf (((symbol-function 'agent-shell--shutdown) #'ignore)
                    ((symbol-function 'agent-shell-viewport--buffer) (lambda (&rest _) nil)))
            (kill-buffer shell-buf))
          (should-not (buffer-live-p diff-buf)))
      (when (buffer-live-p diff-buf)
        (kill-buffer diff-buf))
      (when (buffer-live-p shell-buf)
        (kill-buffer shell-buf)))))

(provide 'agent-shell-diff-tests)
;;; agent-shell-diff-tests.el ends here
