;;; agent-shell-ui-tests.el --- Tests for agent-shell-ui -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-ui)

;;; Code:

(ert-deftest agent-shell-ui-body-invisible-p-handles-whitespace-only-body ()
  ;; Regression for PR #597 (pi-acp): the markdown renderer strips
  ;; an empty `\\`\\`\\`console' fence down to a body of only
  ;; newlines.  On the next `agent-shell-ui--replace-body',
  ;; `--body-invisible-p' must still report the body as hidden when
  ;; its chars carry `invisible t' — otherwise new chars come in
  ;; visible and the fragment "expands" on every subsequent update
  ;; while still showing the `▶' collapsed indicator.
  (with-temp-buffer
    (insert "\n\n")
    (add-text-properties (point-min) (point-max) '(invisible t))
    (should (agent-shell-ui--body-invisible-p (point-min) (point-max))))
  (with-temp-buffer
    (insert "\n\n")
    (should-not (agent-shell-ui--body-invisible-p (point-min) (point-max)))))

(ert-deftest agent-shell-ui-indent-text-preserves-caller-text-properties ()
  ;; A pre-rendered body (eg. a diff tagged `agent-shell-markdown-frozen')
  ;; passes through `--indent-text' on its way into the fragment buffer.
  ;; Every char of the indented result — including the inter-line `\\n's
  ;; — must keep the caller's text properties, otherwise the markdown
  ;; renderer's contiguous frozen-range collapses per-line and the
  ;; header / blockquote passes match across the now-bare line breaks.
  ;; See PR #597.
  (let* ((input (propertize "line one\nline two\nline three"
                            'agent-shell-markdown-frozen t))
         (out (agent-shell-ui--indent-text input "  ")))
    (dotimes (i (length out))
      (should (eq t (get-text-property i 'agent-shell-markdown-frozen out)))
      (should (equal "  " (get-text-property i 'line-prefix out)))
      (should (equal "  " (get-text-property i 'wrap-prefix out))))))

(provide 'agent-shell-ui-tests)

;;; agent-shell-ui-tests.el ends here
