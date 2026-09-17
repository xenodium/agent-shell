;;; agent-shell-experimental-tests.el --- Tests for agent-shell-experimental -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for server-initiated `session/push' handling.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'comint)
;; `agent-shell-experimental' only declares what it borrows from these.
(require 'agent-shell)
(require 'agent-shell-experimental)

(defun agent-shell-experimental-tests--push (&optional preceding-output)
  "Return the buffer text after a `session/push' request is handled.

PRECEDING-OUTPUT is inserted first, standing for the turn the push
arrives after.  The shell is faked the way `agent-shell-tests' does it:
a `cat' process the stubbed `shell-maker--process' hands back, so
`shell-maker-insert-end-of-prompt-marker' has a process mark to advance.

A prompt is printed last, since a shell always has one waiting for input
\(see `agent-shell--persistent-prompt'), and the push renders above it."
  (let* ((buffer (generate-new-buffer " *agent-shell-push-test*"))
         (fake-process (start-process "fake-agent" buffer "cat")))
    (set-process-query-on-exit-flag fake-process nil)
    (unwind-protect
        (with-current-buffer buffer
          (comint-mode)
          (setq-local comint-prompt-regexp "^Claude> ")
          (when preceding-output
            (insert preceding-output))
          (shell-maker--output-filter fake-process "Claude> ")
          (let ((agent-shell-show-busy-indicator nil)
                (state (list (cons :buffer (current-buffer))
                             (cons :client nil)
                             (cons :heartbeat nil)
                             (cons :active-requests nil)
                             (cons :last-entry-type nil))))
            (cl-letf (((symbol-function 'shell-maker--process) (lambda () fake-process)))
              (agent-shell-experimental--on-session-push-request
               :state state
               :acp-request '((jsonrpc . "2.0") (id . 100) (method . "session/push")))))
          (buffer-string))
      (kill-buffer buffer))))

(ert-deftest agent-shell-experimental-push-inserts-turn-boundary-test ()
  "A `session/push' gives the turn it pushes its own end-of-prompt boundary.

Without one the pushed content runs on from whatever preceded it, and
chat mode, which anchors the agent label on that boundary, renders the
pushed turn under the user's `Me' label (issue 37).

The boundary lands above the prompt waiting for input, which the push
leaves alone: it is where the pushed content renders, and it holds
anything the user has typed and not submitted."
  (should (string-suffix-p "<shell-maker-end-of-prompt>Claude> "
                           (agent-shell-experimental-tests--push
                            "A previous turn's output\n"))))

(ert-deftest agent-shell-experimental-push-boundary-follows-output-test ()
  "The boundary is appended, leaving the preceding turn's output intact."
  (should (string-prefix-p "A previous turn's output\n"
                           (agent-shell-experimental-tests--push
                            "A previous turn's output\n"))))

(provide 'agent-shell-experimental-tests)

;;; agent-shell-experimental-tests.el ends here
