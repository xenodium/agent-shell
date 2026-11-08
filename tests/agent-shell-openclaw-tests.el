;;; agent-shell-openclaw-tests.el --- Tests for agent-shell-openclaw -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-openclaw)

;;; Code:

(ert-deftest agent-shell-openclaw-make-agent-config-identifier-test ()
  "Test that OpenClaw config is identified as openclaw."
  (should (eq (map-elt (agent-shell-openclaw-make-agent-config) :identifier)
              'openclaw)))

(ert-deftest agent-shell-openclaw-default-model-id-test ()
  "Test that OpenClaw config exposes default model id."
  (let ((default-model-id-fn
         (map-elt (agent-shell-openclaw-make-agent-config) :default-model-id)))

    (let ((agent-shell-openclaw-default-model-id nil))
      (should (null (funcall default-model-id-fn))))

    (let ((agent-shell-openclaw-default-model-id "anthropic/claude-opus-4-5"))
      (should (string= (funcall default-model-id-fn) "anthropic/claude-opus-4-5")))))

(ert-deftest agent-shell-openclaw-make-client-uses-acp-command-test ()
  "Test that OpenClaw client is started with the configured ACP command."
  (let ((agent-shell-openclaw-acp-command '("openclaw" "acp" "--session" "main"))
        (agent-shell-openclaw-environment '("MY_VAR=some-value"))
        (captured nil))
    (cl-letf (((symbol-function 'agent-shell--make-acp-client)
               (lambda (&rest args) (setq captured args))))
      (with-temp-buffer
        (agent-shell-openclaw-make-client :buffer (current-buffer))))
    (should (equal (plist-get captured :command) "openclaw"))
    (should (equal (plist-get captured :command-params)
                   '("acp" "--session" "main")))
    (should (equal (plist-get captured :environment-variables)
                   '("MY_VAR=some-value")))))

(ert-deftest agent-shell-openclaw-make-client-requires-buffer-test ()
  "Test that creating an OpenClaw client without a buffer is an error."
  (should-error (agent-shell-openclaw-make-client)))

(provide 'agent-shell-openclaw-tests)
;;; agent-shell-openclaw-tests.el ends here
