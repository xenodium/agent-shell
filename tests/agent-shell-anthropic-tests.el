;;; agent-shell-anthropic-tests.el --- Tests for agent-shell-anthropic -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-anthropic)

(defvar agent-shell-anthropic-default-model-name)
(defvar agent-shell-anthropic-default-model-id)

(declare-function agent-shell-anthropic--find-model-id-by-name "agent-shell-anthropic")
(declare-function agent-shell-anthropic--resolve-default-model-id "agent-shell-anthropic")

(ert-deftest agent-shell-anthropic-make-claude-client-test ()
  "Test agent-shell-anthropic-make-claude-client function."
  ;; Mock executable-find to always return the command path
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "/usr/bin/claude-agent-acp")))
    ;; Test with API key authentication
    (let* ((agent-shell-anthropic-authentication '(:api-key "test-api-key"))
           (agent-shell-anthropic-claude-command '("claude-agent-acp" "--json"))
           (agent-shell-anthropic-claude-environment '("DEBUG=1"))
           (test-buffer (get-buffer-create "*test-buffer*"))
           (client (agent-shell-anthropic-make-claude-client :buffer test-buffer)))
      (unwind-protect
          (progn
            (should (listp client))
            (should (equal (map-elt client :command) "claude-agent-acp"))
            (should (equal (map-elt client :command-params) '("--json")))
            (should (member "ANTHROPIC_API_KEY=test-api-key" (map-elt client :environment-variables)))
            (should (member "DEBUG=1" (map-elt client :environment-variables))))
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))

    ;; Test with login authentication
    (let* ((agent-shell-anthropic-authentication '(:login t))
           (agent-shell-anthropic-claude-command '("claude-agent-acp" "--interactive"))
           (agent-shell-anthropic-claude-environment '("VERBOSE=true"))
           (test-buffer (get-buffer-create "*test-buffer*"))
           (client (agent-shell-anthropic-make-claude-client :buffer test-buffer)))
      (unwind-protect
          (progn
            ;; Verify environment variables include empty API key for login
            (should (member "ANTHROPIC_API_KEY=" (map-elt client :environment-variables)))
            (should (member "VERBOSE=true" (map-elt client :environment-variables))))
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))

    ;; Test with function-based API key
    (let* ((agent-shell-anthropic-authentication `(:api-key ,(lambda () "dynamic-key")))
           (agent-shell-anthropic-claude-command '("claude-agent-acp"))
           (agent-shell-anthropic-claude-environment '())
           (test-buffer (get-buffer-create "*test-buffer*"))
           (client (agent-shell-anthropic-make-claude-client :buffer test-buffer))
           (env-vars (map-elt client :environment-variables)))
      (unwind-protect
          (should (member "ANTHROPIC_API_KEY=dynamic-key" env-vars))
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))

    ;; Test error on invalid authentication
    (let* ((agent-shell-anthropic-authentication '())
           (agent-shell-anthropic-claude-command '("claude-agent-acp"))
           (test-buffer (get-buffer-create "*test-buffer*")))
      (unwind-protect
          (should-error (agent-shell-anthropic-make-claude-client :buffer test-buffer)
                        :type 'error)
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))

    ;; Test with agent-shell-make-environment-variables and :inherit-env t
    (let* ((agent-shell-anthropic-authentication '(:api-key "test-key"))
           (agent-shell-anthropic-claude-command '("claude-agent-acp"))
           (process-environment '("EXISTING_VAR=existing_value"))
           (agent-shell-anthropic-claude-environment (agent-shell-make-environment-variables
                                                      "NEW_VAR" "new_value"
                                                      :inherit-env t))
           (test-buffer (get-buffer-create "*test-buffer*"))
           (client (agent-shell-anthropic-make-claude-client :buffer test-buffer))
           (env-vars (map-elt client :environment-variables)))
      (unwind-protect
          (progn
            (should (member "ANTHROPIC_API_KEY=test-key" env-vars))
            (should (member "NEW_VAR=new_value" env-vars))
            (should (member "EXISTING_VAR=existing_value" env-vars)))
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))))

(ert-deftest agent-shell-anthropic-find-model-id-by-name-test ()
  "Test agent-shell-anthropic--find-model-id-by-name function."
  (let ((mock-models '(((:name . "Default (recommended)")
                        (:model-id . "claude-3-5-sonnet-20241022")
                        (:description . "Use the default model"))
                       ((:name . "Sonnet")
                        (:model-id . "claude-3-6-sonnet-20250115")
                        (:description . "Sonnet 4.6"))
                       ((:name . "Opus")
                        (:model-id . "claude-3-6-opus-20250109")
                        (:description . "Opus 4.6"))
                       ((:name . "Opus 4.1")
                        (:model-id . "claude-opus-4-1-legacy")
                        (:description . "Opus 4.1 Legacy"))
                       ((:name . "Haiku")
                        (:model-id . "claude-3-6-haiku-20250107")
                        (:description . "Haiku 4.5")))))

    ;; Test exact match
    (should (string= (agent-shell-anthropic--find-model-id-by-name "Opus" mock-models)
                     "claude-3-6-opus-20250109"))

    ;; Test case-insensitive match
    (should (string= (agent-shell-anthropic--find-model-id-by-name "opus" mock-models)
                     "claude-3-6-opus-20250109"))
    (should (string= (agent-shell-anthropic--find-model-id-by-name "OPUS" mock-models)
                     "claude-3-6-opus-20250109"))

    ;; Test other models
    (should (string= (agent-shell-anthropic--find-model-id-by-name "Sonnet" mock-models)
                     "claude-3-6-sonnet-20250115"))
    (should (string= (agent-shell-anthropic--find-model-id-by-name "Haiku" mock-models)
                     "claude-3-6-haiku-20250107"))

    ;; Test non-existent model
    (should (null (agent-shell-anthropic--find-model-id-by-name "NonExistent" mock-models)))

    ;; Test nil inputs
    (should (null (agent-shell-anthropic--find-model-id-by-name nil mock-models)))
    (should (null (agent-shell-anthropic--find-model-id-by-name "Opus" nil)))
    (should (null (agent-shell-anthropic--find-model-id-by-name nil nil)))))

(ert-deftest agent-shell-anthropic-resolve-default-model-id-test ()
  "Test agent-shell-anthropic--resolve-default-model-id function."
  ;; Test when only model ID is configured
  (let ((agent-shell-anthropic-default-model-name nil)
        (agent-shell-anthropic-default-model-id "test-model-id")
        (agent-shell--state nil))
    (should (string= (agent-shell-anthropic--resolve-default-model-id) "test-model-id")))

  ;; Test when only model name is configured but no state yet
  (let ((agent-shell-anthropic-default-model-name "Opus")
        (agent-shell-anthropic-default-model-id nil)
        (agent-shell--state nil))
    (should (eq (agent-shell-anthropic--resolve-default-model-id) 'resolve-by-name)))

  ;; Test when model name is configured and state has models
  (let ((agent-shell-anthropic-default-model-name "Opus")
        (agent-shell-anthropic-default-model-id nil)
        (agent-shell--state '((:session . ((:models . (((:name . "Opus")
                                                        (:model-id . "claude-opus-id")
                                                        (:description . "Opus model"))
                                                       ((:name . "Sonnet")
                                                        (:model-id . "claude-sonnet-id")
                                                        (:description . "Sonnet model")))))))))
    (should (string= (agent-shell-anthropic--resolve-default-model-id) "claude-opus-id")))

  ;; Test when model name doesn't match any model - should error
  (let ((agent-shell-anthropic-default-model-name "NonExistent")
        (agent-shell-anthropic-default-model-id nil)
        (agent-shell--state '((:session . ((:models . (((:name . "Opus")
                                                        (:model-id . "claude-opus-id")
                                                        (:description . "Opus model")))))))))
    (should-error (agent-shell-anthropic--resolve-default-model-id)
                  :type 'error))

  ;; Test when neither name nor ID is configured
  (let ((agent-shell-anthropic-default-model-name nil)
        (agent-shell-anthropic-default-model-id nil)
        (agent-shell--state nil))
    (should (null (agent-shell-anthropic--resolve-default-model-id))))

  ;; Test error when both name and ID are configured
  (let ((agent-shell-anthropic-default-model-name "Sonnet")
        (agent-shell-anthropic-default-model-id "some-model-id")
        (agent-shell--state '((:session . ((:models . (((:name . "Opus")
                                                        (:model-id . "claude-opus-id"))
                                                       ((:name . "Sonnet")
                                                        (:model-id . "claude-sonnet-id")))))))))
    (should-error (agent-shell-anthropic--resolve-default-model-id)
                  :type 'error)))

(provide 'agent-shell-anthropic-tests)
;;; agent-shell-anthropic-tests.el ends here
