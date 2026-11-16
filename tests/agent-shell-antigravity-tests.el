;;; agent-shell-antigravity-tests.el --- Tests for Antigravity -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-antigravity)

;;; Code:

(ert-deftest agent-shell-antigravity-default-config-options-test ()
  "Test that Antigravity config exposes default config options."
  (let ((default-config-options-fn
         (map-elt (agent-shell-antigravity-make-agent-config) :default-config-options)))

    (let ((agent-shell-antigravity-default-config-options nil))
      (should (null (funcall default-config-options-fn))))

    (let ((agent-shell-antigravity-default-config-options
           '(("model" . "gemini-2.5-pro")
             ("mode" . "auto"))))
      (should (equal (funcall default-config-options-fn)
                     '(("model" . "gemini-2.5-pro")
                       ("mode" . "auto")))))))

(provide 'agent-shell-antigravity-tests)
;;; agent-shell-antigravity-tests.el ends here
