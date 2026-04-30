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
         (env (car (agent-shell-openai--codex-default-auth-environment)))
         (request (json-parse-string (string-remove-prefix "DEFAULT_AUTH_REQUEST=" env)
                                     :object-type 'alist)))
    (should (string= (map-elt request 'methodId) "chat-gpt"))))

(ert-deftest agent-shell-openai-codex-api-key-default-auth-request-test ()
  "Test that Codex API key auth sends key metadata."
  (let* ((agent-shell-openai-authentication
          (agent-shell-openai-make-authentication :api-key "openai-secret"))
         (env (car (agent-shell-openai--codex-default-auth-environment)))
         (request (json-parse-string (string-remove-prefix "DEFAULT_AUTH_REQUEST=" env)
                                     :object-type 'alist)))
    (should (string= (map-elt request 'methodId) "api-key"))
    (should (string= (map-nested-elt request '(_meta api-key apiKey))
                     "openai-secret"))))

(ert-deftest agent-shell-openai-codex-key-default-auth-request-test ()
  "Test that Codex-specific API key auth sends key metadata."
  (let* ((agent-shell-openai-authentication
          (agent-shell-openai-make-authentication :codex-api-key "codex-secret"))
         (env (car (agent-shell-openai--codex-default-auth-environment)))
         (request (json-parse-string (string-remove-prefix "DEFAULT_AUTH_REQUEST=" env)
                                     :object-type 'alist)))
    (should (string= (map-elt request 'methodId) "api-key"))
    (should (string= (map-nested-elt request '(_meta api-key apiKey))
                     "codex-secret"))))

(provide 'agent-shell-openai-tests)
;;; agent-shell-openai-tests.el ends here
