;;; agent-shell-invariants-tests.el --- Tests for agent-shell-invariants -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the invariant checking and event tracing system.

;;; Code:

(require 'ert)
(require 'agent-shell-invariants)
(require 'agent-shell-ui)

;;; --- Event ring tests -----------------------------------------------------

(ert-deftest agent-shell-invariants--record-populates-ring-test ()
  "Test that recording events populates the ring buffer."
  (with-temp-buffer
    (let ((agent-shell-invariants-enabled t)
          (agent-shell-invariants--ring nil)
          (agent-shell-invariants--seq 0))
      (agent-shell-invariants--record 'test-op :foo "bar")
      (agent-shell-invariants--record 'test-op-2 :baz 42)
      (should (= (ring-length agent-shell-invariants--ring) 2))
      (let ((events (agent-shell-invariants--events)))
        (should (= (length events) 2))
        ;; Oldest first
        (should (eq (plist-get (car events) :op) 'test-op))
        (should (eq (plist-get (cadr events) :op) 'test-op-2))
        ;; Sequence numbers increment
        (should (= (plist-get (car events) :seq) 1))
        (should (= (plist-get (cadr events) :seq) 2))))))

(ert-deftest agent-shell-invariants--record-noop-when-disabled-test ()
  "Test that recording does nothing when invariants are disabled."
  (with-temp-buffer
    (let ((agent-shell-invariants-enabled nil)
          (agent-shell-invariants--ring nil)
          (agent-shell-invariants--seq 0))
      (agent-shell-invariants--record 'test-op :foo "bar")
      (should-not agent-shell-invariants--ring))))

(ert-deftest agent-shell-invariants--ring-wraps-test ()
  "Test that the ring drops oldest events when full."
  (with-temp-buffer
    (let ((agent-shell-invariants-enabled t)
          (agent-shell-invariants--ring nil)
          (agent-shell-invariants--seq 0)
          (agent-shell-invariants-ring-size 3))
      (dotimes (i 5)
        (agent-shell-invariants--record 'test-op :i i))
      (should (= (ring-length agent-shell-invariants--ring) 3))
      (let ((events (agent-shell-invariants--events)))
        ;; Should have events 3, 4, 5 (seq 3, 4, 5)
        (should (= (plist-get (car events) :seq) 3))
        (should (= (plist-get (car (last events)) :seq) 5))))))

;;; --- Invariant check tests ------------------------------------------------

(ert-deftest agent-shell-invariants--check-ui-state-contiguity-clean-test ()
  "Test that contiguity check passes for well-formed fragments."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (state (list (cons :qualified-id "ns-1") (cons :collapsed nil))))
      (insert "fragment content")
      (add-text-properties 1 (point) (list 'agent-shell-ui-state state)))
    (should-not (agent-shell-invariants--check-ui-state-contiguity))))

(ert-deftest agent-shell-invariants--check-ui-state-contiguity-gap-test ()
  "Test that contiguity check detects gaps within a fragment."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (state (list (cons :qualified-id "ns-1") (cons :collapsed nil))))
      ;; First span
      (insert "part1")
      (add-text-properties 1 (point) (list 'agent-shell-ui-state state))
      ;; Gap with no property
      (insert "gap")
      ;; Second span with same fragment id
      (let ((start (point)))
        (insert "part2")
        (add-text-properties start (point) (list 'agent-shell-ui-state state))))
    (should (agent-shell-invariants--check-ui-state-contiguity))))

;;; --- Violation handler tests ----------------------------------------------

(ert-deftest agent-shell-invariants--on-violation-creates-bundle-buffer-test ()
  "Test that violation handler creates a debug bundle buffer."
  (with-temp-buffer
    (rename-buffer "*agent-shell test-inv*" t)
    (let ((agent-shell-invariants-enabled t)
          (agent-shell-invariants--ring nil)
          (agent-shell-invariants--seq 0)
          (bundle-buf-name (format "*agent-shell invariant [%s]*"
                                   (buffer-name))))
      ;; Record a couple events
      (agent-shell-invariants--record 'test-op :detail "setup")
      ;; Trigger violation
      (agent-shell-invariants--on-violation
       'test-trigger
       '((test-check . "something went wrong")))
      ;; Bundle buffer should exist
      (should (get-buffer bundle-buf-name))
      (with-current-buffer bundle-buf-name
        (should (string-match-p "INVARIANT VIOLATION" (buffer-string)))
        (should (string-match-p "something went wrong" (buffer-string)))
        (should (string-match-p "test-trigger" (buffer-string)))
        (should (string-match-p "Recommended Prompt" (buffer-string))))
      (kill-buffer bundle-buf-name))))

(ert-deftest agent-shell-invariants--on-violation-snapshots-head-and-tail-test ()
  "Bundle includes both head and tail snippets when buffer exceeds the window.
The 2000-char window from `point-min' alone misses violations
that fire near `point-max' on long sessions; both snippets must
appear in the bundle."
  (with-temp-buffer
    (rename-buffer "*agent-shell test-inv-long*" t)
    ;; Build a buffer with distinctive head and tail markers separated
    ;; by enough filler that neither end-snippet alone would contain
    ;; both markers.
    (insert "HEAD-MARKER ")
    (insert (make-string 5000 ?x))
    (insert " TAIL-MARKER")
    (let ((agent-shell-invariants-enabled t)
          (agent-shell-invariants--ring nil)
          (agent-shell-invariants--seq 0)
          (bundle-buf-name (format "*agent-shell invariant [%s]*"
                                   (buffer-name))))
      (agent-shell-invariants--on-violation
       'long-buffer-trigger
       '((test-check . "long-buffer test")))
      (with-current-buffer bundle-buf-name
        (let ((bundle (buffer-string)))
          (should (string-match-p "Buffer Snapshot Head" bundle))
          (should (string-match-p "Buffer Snapshot Tail" bundle))
          (should (string-match-p "HEAD-MARKER" bundle))
          (should (string-match-p "TAIL-MARKER" bundle))))
      (kill-buffer bundle-buf-name))))

(ert-deftest agent-shell-invariants--on-violation-single-snapshot-when-short-test ()
  "Short buffers fit in a single snapshot section, no head/tail split."
  (with-temp-buffer
    (rename-buffer "*agent-shell test-inv-short*" t)
    (insert "ONLY-MARKER plus a little filler text")
    (let ((agent-shell-invariants-enabled t)
          (agent-shell-invariants--ring nil)
          (agent-shell-invariants--seq 0)
          (bundle-buf-name (format "*agent-shell invariant [%s]*"
                                   (buffer-name))))
      (agent-shell-invariants--on-violation
       'short-buffer-trigger
       '((test-check . "short")))
      (with-current-buffer bundle-buf-name
        (let ((bundle (buffer-string)))
          (should (string-match-p "ONLY-MARKER" bundle))
          (should-not (string-match-p "Snapshot Head" bundle))
          (should-not (string-match-p "Snapshot Tail" bundle))))
      (kill-buffer bundle-buf-name))))

;;; --- Mutation hook tests --------------------------------------------------

(ert-deftest agent-shell-invariants-on-notification-records-event-test ()
  "Test that notification hook records to the event ring."
  (with-temp-buffer
    (let ((agent-shell-invariants-enabled t)
          (agent-shell-invariants--ring nil)
          (agent-shell-invariants--seq 0))
      (agent-shell-invariants-on-notification "tool_call" "tc-123")
      (let ((events (agent-shell-invariants--events)))
        (should (= (length events) 1))
        (should (eq (plist-get (car events) :op) 'notification))
        (should (equal (plist-get (car events) :update-type) "tool_call"))
        (should (equal (plist-get (car events) :detail) "tc-123"))))))

(ert-deftest agent-shell-invariants--format-events-test ()
  "Test that event formatting produces readable output."
  (with-temp-buffer
    (let ((agent-shell-invariants-enabled t)
          (agent-shell-invariants--ring nil)
          (agent-shell-invariants--seq 0))
      (agent-shell-invariants--record 'test-op :detail "hello")
      (let ((formatted (agent-shell-invariants--format-events)))
        (should (string-match-p "\\[1\\]" formatted))
        (should (string-match-p "test-op" formatted))
        (should (string-match-p "hello" formatted))))))

;;; --- Rate-limiting tests ---------------------------------------------------

(ert-deftest agent-shell-invariants--violation-reported-once-test ()
  "Violation handler should only fire once per buffer until flag is cleared."
  (with-temp-buffer
    (rename-buffer "*agent-shell rate-limit-test*" t)
    (let ((agent-shell-invariants-enabled t)
          (agent-shell-invariants--ring nil)
          (agent-shell-invariants--seq 0)
          (agent-shell-invariants--violation-reported nil)
          (call-count 0)
          (bundle-buf-name (format "*agent-shell invariant [%s]*"
                                   (buffer-name))))
      ;; Override one check to always fail
      (let ((agent-shell-invariants--all-checks
             (list (lambda () "always fails"))))
        ;; First run should report
        (agent-shell-invariants--run-checks 'test-op)
        (should agent-shell-invariants--violation-reported)
        (should (get-buffer bundle-buf-name))
        (kill-buffer bundle-buf-name)
        ;; Second run should be suppressed
        (agent-shell-invariants--run-checks 'test-op-2)
        (should-not (get-buffer bundle-buf-name))
        ;; After clearing the flag, it should report again
        (agent-shell-invariants--clear-violation-flag)
        (agent-shell-invariants--run-checks 'test-op-3)
        (should (get-buffer bundle-buf-name))
        (kill-buffer bundle-buf-name)))))

(provide 'agent-shell-invariants-tests)

;;; agent-shell-invariants-tests.el ends here
