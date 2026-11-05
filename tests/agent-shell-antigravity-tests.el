;;; agent-shell-antigravity-tests.el --- Tests for Antigravity -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-antigravity)

;;; Code:

(ert-deftest agent-shell-antigravity-default-model-id-test ()
  "Test that Antigravity config exposes default model id."
  (let ((default-model-id-fn
         (map-elt (agent-shell-antigravity-make-agent-config) :default-model-id)))
    (let ((agent-shell-antigravity-default-model-id nil))
      (should (null (funcall default-model-id-fn))))
    (let ((agent-shell-antigravity-default-model-id "Gemini 3.8 Pro"))
      (should (string= (funcall default-model-id-fn) "Gemini 3.8 Pro")))
    (let ((agent-shell-antigravity-default-model-id (lambda () "Gemini 3.8 Flash (Medium)")))
      (should (string= (funcall default-model-id-fn) "Gemini 3.8 Flash (Medium)")))))

(ert-deftest agent-shell-antigravity-default-session-mode-id-test ()
  "Test that Antigravity config exposes default session mode id."
  (let ((default-session-mode-id-fn
         (map-elt (agent-shell-antigravity-make-agent-config) :default-session-mode-id)))
    (let ((agent-shell-antigravity-default-session-mode-id nil))
      (should (null (funcall default-session-mode-id-fn))))
    (let ((agent-shell-antigravity-default-session-mode-id "auto"))
      (should (string= (funcall default-session-mode-id-fn) "auto")))))

(provide 'agent-shell-antigravity-tests)
;;; agent-shell-antigravity-tests.el ends here
