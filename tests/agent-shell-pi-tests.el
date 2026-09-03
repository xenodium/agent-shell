;;; agent-shell-pi-tests.el --- Tests for agent-shell-pi -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-pi)
(require 'map)

;;; Code:

(ert-deftest agent-shell-pi--notification-adapter-terminal-output-test ()
  "Test `agent-shell-pi--notification-adapter' accumulates terminal output.
Pi sends terminal output as deltas in `_meta.terminal_output'.  Its current
final update also includes `_meta.terminal_exit'; a status-only final update
is accepted as well so it cannot replace the accumulated output with empty
content."
  (with-temp-buffer
    (setq-local agent-shell-pi--terminal-output-snapshots nil)
    (let ((notification
           '((method . "session/update")
             (params
              (update
               (sessionUpdate . "tool_call_update")
               (toolCallId . "call-1")
               (status . "in_progress")
               (_meta
                (terminal_output
                 (terminal_id . "call-1")
                 (data . "hello\n"))))))))
      (agent-shell-pi--notification-adapter :acp-notification notification)
      (should (equal
               (map-nested-elt notification '(params update content))
               '(((type . "content")
                  (content (type . "text") (text . "```\nhello\n\n```")))))))
    (let ((notification
           '((method . "session/update")
             (params
              (update
               (sessionUpdate . "tool_call_update")
               (toolCallId . "call-1")
               (status . "completed")
               (_meta
                (terminal_output
                 (terminal_id . "call-1")
                 (data . "world"))
                (terminal_exit
                 (terminal_id . "call-1")
                 (exit_code . 0)
                 (signal . nil))))))))
      (agent-shell-pi--notification-adapter :acp-notification notification)
      (should (equal
               (map-nested-elt notification '(params update content))
               '(((type . "content")
                  (content (type . "text")
                           (text . "```\nhello\nworld\n```")))))))
    (should-not agent-shell-pi--terminal-output-snapshots))

  (with-temp-buffer
    (setq-local agent-shell-pi--terminal-output-snapshots nil)
    (let ((notification
           '((method . "session/update")
             (params
              (update
               (sessionUpdate . "tool_call_update")
               (toolCallId . "call-2")
               (status . "in_progress")
               (_meta
                (terminal_output
                 (terminal_id . "call-2")
                 (data . "kept"))))))))
      (agent-shell-pi--notification-adapter :acp-notification notification))
    (let ((notification
           '((method . "session/update")
             (params
              (update
               (sessionUpdate . "tool_call_update")
               (toolCallId . "call-2")
               (status . "completed"))))))
      (agent-shell-pi--notification-adapter :acp-notification notification)
      (should (equal
               (map-nested-elt notification '(params update content))
               '(((type . "content")
                  (content (type . "text") (text . "```\nkept\n```")))))))
    (should-not agent-shell-pi--terminal-output-snapshots)))

(ert-deftest agent-shell-pi-make-agent-config-notification-adapter-test ()
  "Test Pi's agent configuration installs its notification adapter."
  (should (eq (map-elt (agent-shell-pi-make-agent-config)
                       :notification-adapter)
              #'agent-shell-pi--notification-adapter)))

(provide 'agent-shell-pi-tests)
;;; agent-shell-pi-tests.el ends here
