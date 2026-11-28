;;; agent-shell-devin-tests.el --- Tests for agent-shell-devin -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-devin)

;;; Code:

(ert-deftest agent-shell-devin-make-authentication-test ()
  "Test `agent-shell-devin-make-authentication'."
  (should (equal (agent-shell-devin-make-authentication :api-key "key")
                 '((:api-key . "key"))))
  (should (equal (agent-shell-devin-make-authentication :login t)
                 '((:login . t))))
  (should (equal (agent-shell-devin-make-authentication :none t)
                 '((:none . t))))
  (should-error (agent-shell-devin-make-authentication))
  (should-error (agent-shell-devin-make-authentication :api-key "key" :none t))
  (should-error (agent-shell-devin-make-authentication :login t :none t)))

(ert-deftest agent-shell-devin-default-config-options-test ()
  "Test that Devin config exposes default config options."
  (let ((default-config-options-fn
         (map-elt (agent-shell-devin-make-agent-config) :default-config-options)))

    (let ((agent-shell-devin-default-config-options nil))
      (should (null (funcall default-config-options-fn))))

    (let ((agent-shell-devin-default-config-options
           '(("model" . "swe-2-high")
             ("mode" . "plan"))))
      (should (equal (funcall default-config-options-fn)
                     '(("model" . "swe-2-high")
                       ("mode" . "plan")))))))

(ert-deftest agent-shell-devin--resolve-secret-test ()
  "Test `agent-shell-devin--resolve-secret'."
  (should (equal (agent-shell-devin--resolve-secret "key") "key"))
  (should (equal (agent-shell-devin--resolve-secret (lambda () "key")) "key"))
  (should-not (agent-shell-devin--resolve-secret nil))
  (should-error (agent-shell-devin--resolve-secret (lambda () (error "no key")))))

(provide 'agent-shell-devin-tests)
;;; agent-shell-devin-tests.el ends here
