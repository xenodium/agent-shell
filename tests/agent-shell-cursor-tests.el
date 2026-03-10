;;; agent-shell-cursor-tests.el --- Tests for agent-shell-cursor -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-cursor)

(ert-deftest agent-shell-cursor-make-client-api-key-test ()
  "Test API key auth injects --api-key into CLI command."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "/usr/bin/cursor-agent")))
    (let* ((agent-shell-cursor-authentication (agent-shell-cursor-make-authentication :api-key "test-api-key"))
           (agent-shell-cursor-acp-command '("cursor-agent" "acp"))
           (agent-shell-cursor-environment '("DEBUG=1"))
           (test-buffer (get-buffer-create "*test-cursor-buffer*"))
           (client (agent-shell-cursor-make-client :buffer test-buffer)))
      (unwind-protect
          (progn
            (should (listp client))
            (should (equal (map-elt client :command) "cursor-agent"))
            (should (equal (map-elt client :command-params) '("--api-key" "test-api-key" "acp")))
            (should (member "DEBUG=1" (map-elt client :environment-variables))))
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))))

(ert-deftest agent-shell-cursor-make-client-api-key-function-test ()
  "Test function-based API key injection."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "/usr/bin/cursor-agent")))
    (let* ((agent-shell-cursor-authentication (agent-shell-cursor-make-authentication :api-key (lambda () "dynamic-key")))
           (agent-shell-cursor-acp-command '("cursor-agent" "acp"))
           (agent-shell-cursor-environment '())
           (test-buffer (get-buffer-create "*test-cursor-buffer*"))
           (client (agent-shell-cursor-make-client :buffer test-buffer)))
      (unwind-protect
          (should (equal (map-elt client :command-params) '("--api-key" "dynamic-key" "acp")))
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))))

(ert-deftest agent-shell-cursor-make-client-login-test ()
  "Test login-based authentication."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "/usr/bin/cursor-agent")))
    (let* ((agent-shell-cursor-authentication (agent-shell-cursor-make-authentication :login t))
           (agent-shell-cursor-acp-command '("cursor-agent" "acp"))
           (agent-shell-cursor-environment '("DEBUG=1"))
           (test-buffer (get-buffer-create "*test-cursor-buffer*"))
           (client (agent-shell-cursor-make-client :buffer test-buffer)))
      (unwind-protect
          (progn
            (should (equal (map-elt client :command-params) '("acp")))
            (should (member "DEBUG=1" (map-elt client :environment-variables))))
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))))

(ert-deftest agent-shell-cursor-make-client-invalid-auth-test ()
  "Test error on invalid authentication configuration."
  (let* ((agent-shell-cursor-authentication '())
         (agent-shell-cursor-acp-command '("cursor-agent" "acp"))
         (test-buffer (get-buffer-create "*test-cursor-buffer*")))
    (unwind-protect
        (should-error (agent-shell-cursor-make-client :buffer test-buffer)
                      :type 'error)
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

(provide 'agent-shell-cursor-tests)
;;; agent-shell-cursor-tests.el ends here
