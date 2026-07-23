;;; agent-shell-retry-tests.el --- Tests for agent-shell-retry -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-retry)

;;; Code:

(ert-deftest agent-shell--retry-backoff-for-attempt-test ()
  "Test `agent-shell--retry-backoff-for-attempt' delay lookup and clamping."
  (let ((agent-shell-retry-backoff-seconds '(2 5 10)))
    (should (equal (agent-shell--retry-backoff-for-attempt 1) 2))
    (should (equal (agent-shell--retry-backoff-for-attempt 2) 5))
    (should (equal (agent-shell--retry-backoff-for-attempt 3) 10))
    ;; Beyond the list length clamps to the last entry.
    (should (equal (agent-shell--retry-backoff-for-attempt 4) 10))
    (should (equal (agent-shell--retry-backoff-for-attempt 99) 10))))

(ert-deftest agent-shell--retry-prompt-test ()
  "Test `agent-shell--retry-prompt' includes the failed user prompt."
  (let ((agent-shell-retry-prompt "Continue from the failure."))
    (should (equal (agent-shell--retry-prompt nil)
                   "Continue from the failure."))
    (should (equal (agent-shell--retry-prompt '((:last-user-prompt . "   ")))
                   "Continue from the failure."))
    (should (equal (agent-shell--retry-prompt
                    '((:last-user-prompt . "Patch the retry tests.")))
                   "Continue from the failure.

Last user prompt before the failure:

Patch the retry tests."))))

(ert-deftest agent-shell--retriable-turn-tail-p-test ()
  "Test `agent-shell--retriable-turn-tail-p' anchors to the final line."
  ;; The reported Cursor case: error emitted as the turn's final line, the
  ;; matched line is returned so it can be reused as the error message.
  (should (equal (agent-shell--retriable-turn-tail-p
                  "Let me continue waiting for the save/upload.\nError: RetriableError: [canceled] http/2 stream closed with error code CANCEL (0x8)")
                 "Error: RetriableError: [canceled] http/2 stream closed with error code CANCEL (0x8)"))
  ;; Trailing blank lines are ignored.
  (should (equal (agent-shell--retriable-turn-tail-p
                  "Error: RetriableError: [canceled] stream closed\n\n")
                 "Error: RetriableError: [canceled] stream closed"))
  ;; A turn that merely analyzes error logs mid-message is not retried:
  ;; the error is not the final line.
  (should-not (agent-shell--retriable-turn-tail-p
               "Error: RetriableError: x\nThat log line is expected; all good now."))
  ;; Non-matching / empty input.
  (should-not (agent-shell--retriable-turn-tail-p "All done, no problems."))
  (should-not (agent-shell--retriable-turn-tail-p ""))
  (should-not (agent-shell--retriable-turn-tail-p nil)))

(ert-deftest agent-shell--accumulate-turn-agent-message-test ()
  "Test `agent-shell--accumulate-turn-agent-message' appends a bounded tail."
  (let ((state (list (cons :turn-agent-message ""))))
    (agent-shell--accumulate-turn-agent-message state "Hello ")
    (agent-shell--accumulate-turn-agent-message state "world")
    (should (equal (map-elt state :turn-agent-message) "Hello world"))
    ;; The stored string is capped so it never grows with the whole message,
    ;; while preserving the trailing text used for tail detection.
    (agent-shell--accumulate-turn-agent-message state (make-string 5000 ?x))
    (should (<= (length (map-elt state :turn-agent-message)) 4000))
    (should (string-suffix-p "xxxx" (map-elt state :turn-agent-message))))
  ;; A no-op (no error) on sessions predating the field.
  (let ((state (list (cons :other t))))
    (agent-shell--accumulate-turn-agent-message state "content")
    (should-not (map-elt state :turn-agent-message))))

(ert-deftest agent-shell--retry-completed-turn-p-test ()
  "Test `agent-shell--retry-completed-turn-p' gating of success-path retries."
  (let ((agent-shell-auto-retry t)
        (agent-shell-retry-max-retries 2)
        (tail "Working...\nError: RetriableError: [canceled] stream closed")
        (clean "All done."))
    ;; A normally-finished turn whose final line is a retriable error.
    (should (agent-shell--retry-completed-turn-p
             :state '((:retry-attempt . 0)) :stop-reason "end_turn" :message tail))
    (should (agent-shell--retry-completed-turn-p
             :state '((:retry-attempt . 1)) :stop-reason "end_turn" :message tail))
    ;; Retry budget exhausted.
    (should-not (agent-shell--retry-completed-turn-p
                 :state '((:retry-attempt . 2)) :stop-reason "end_turn" :message tail))
    ;; Non-end_turn stop reasons go through the normal (failure) paths.
    (should-not (agent-shell--retry-completed-turn-p
                 :state '((:retry-attempt . 0)) :stop-reason "cancelled" :message tail))
    ;; Clean turn is never retried.
    (should-not (agent-shell--retry-completed-turn-p
                 :state '((:retry-attempt . 0)) :stop-reason "end_turn" :message clean))
    ;; Auto-retry disabled.
    (let ((agent-shell-auto-retry nil))
      (should-not (agent-shell--retry-completed-turn-p
                   :state '((:retry-attempt . 0)) :stop-reason "end_turn" :message tail)))))

(provide 'agent-shell-retry-tests)
;;; agent-shell-retry-tests.el ends here
