;;; agent-shell-streaming-tests.el --- Tests for streaming/dedup -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-meta)

;;; Code:

(ert-deftest agent-shell--tool-call-meta-response-text-test ()
  "Extract toolResponse text from meta updates."
  (let ((update '((_meta . ((agent . ((toolResponse . ((content . "ok"))))))))))
    (should (equal (agent-shell--tool-call-meta-response-text update) "ok")))
  (let ((update '((_meta . ((toolResponse . [((type . "text") (text . "one"))
                                             ((type . "text") (text . "two"))]))))))
    (should (equal (agent-shell--tool-call-meta-response-text update)
                   "one\n\ntwo"))))

(ert-deftest agent-shell--tool-call-normalize-output-strips-fences-test ()
  "Backtick fence lines should be stripped from output.

For example:
  (agent-shell--tool-call-normalize-output \"```elisp\\n(+ 1 2)\\n```\")
    => \"(+ 1 2)\\n\""
  ;; Plain fence
  (should (equal (agent-shell--tool-call-normalize-output "```\nhello\n```")
                 "hello\n"))
  ;; Fence with language
  (should (equal (agent-shell--tool-call-normalize-output "```elisp\n(+ 1 2)\n```")
                 "(+ 1 2)\n"))
  ;; Fence with leading whitespace
  (should (equal (agent-shell--tool-call-normalize-output "  ```\nindented\n  ```")
                 "indented\n"))
  ;; Non-fence backticks preserved
  (should (string-match-p "`inline`"
                          (agent-shell--tool-call-normalize-output "`inline` code\n"))))

(ert-deftest agent-shell--tool-call-normalize-output-trailing-newline-test ()
  "Normalized output should always end with a newline."
  (should (string-suffix-p "\n" (agent-shell--tool-call-normalize-output "hello")))
  (should (string-suffix-p "\n" (agent-shell--tool-call-normalize-output "hello\n")))
  (should (equal (agent-shell--tool-call-normalize-output "") ""))
  (should (equal (agent-shell--tool-call-normalize-output nil) nil)))

(ert-deftest agent-shell--tool-call-normalize-output-persisted-output-test ()
  "Persisted-output tags should be stripped and content fontified."
  (let ((result (agent-shell--tool-call-normalize-output
                 "<persisted-output>\nOutput saved to: /tmp/foo.txt\n\nPreview:\nline 0\n</persisted-output>")))
    ;; Tags stripped
    (should-not (string-match-p "<persisted-output>" result))
    (should-not (string-match-p "</persisted-output>" result))
    ;; Content preserved
    (should (string-match-p "Output saved to" result))
    (should (string-match-p "line 0" result))
    ;; Fontified as comment
    (should (eq (get-text-property 1 'font-lock-face result) 'font-lock-comment-face))))

(ert-deftest agent-shell--tool-call-update-writes-output-test ()
  "Tool call updates should write output to the shell buffer."
  (let* ((buffer (get-buffer-create " *agent-shell-tool-call-output*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer)))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update . ((sessionUpdate . "tool_call_update")
                                                   (toolCallId . "call-1")
                                                   (status . "completed")
                                                   (content . [((content . ((text . "stream chunk"))))]))))))))
          (with-current-buffer buffer
            (should (string-match-p "stream chunk" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--tool-call-meta-response-stdout-no-duplication-test ()
  "Meta toolResponse.stdout must not produce duplicate output.
Simplified replay without terminal notifications: sends tool_call
\(pending), tool_call_update with _meta stdout, then tool_call_update
\(completed).  A distinctive line must appear exactly once."
  (let* ((buffer (get-buffer-create " *agent-shell-dedup-test*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (tool-id "toolu_replay_dedup")
         (stdout-text "line 0\nline 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            ;; Notification 1: tool_call (pending)
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call")
                                            (rawInput)
                                            (status . "pending")
                                            (title . "Bash")
                                            (kind . "execute")))))))
            ;; Notification 2: tool_call_update with toolResponse.stdout
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((_meta (claudeCode (toolResponse (stdout . ,stdout-text)
                                                                             (stderr . "")
                                                                             (interrupted)
                                                                             (isImage)
                                                                             (noOutputExpected))
                                                               (toolName . "Bash")))
                                            (toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call_update")))))))
            ;; Notification 3: tool_call_update completed
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call_update")
                                            (status . "completed"))))))))
          (with-current-buffer buffer
            (let* ((buf-text (buffer-substring-no-properties (point-min) (point-max)))
                   (count-line5 (let ((c 0) (s 0))
                                  (while (string-match "line 5" buf-text s)
                                    (setq c (1+ c) s (match-end 0)))
                                  c)))
              ;; "line 9" must be present (output was rendered)
              (should (string-match-p "line 9" buf-text))
              ;; "line 5" must appear exactly once (no duplication)
              (should (= count-line5 1)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--tool-call-meta-response-cumulative-no-duplication-test ()
  "Cumulative meta toolResponse.stdout across multiple updates must not duplicate.
Some agents re-send the full accumulated stdout on every
tool_call_update before the final notification.  Without delta
detection, every revision concatenates into the rendered output."
  (let* ((buffer (get-buffer-create " *agent-shell-cumulative-dedup-test*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (tool-id "toolu_replay_cumulative")
         (line1 "line 0\nline 1\nline 2")
         (line2 (concat line1 "\nline 3\nline 4\nline 5"))
         (line3 (concat line2 "\nline 6\nline 7\nline 8\nline 9")))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((toolCallId . ,tool-id)
                                                (sessionUpdate . "tool_call")
                                                (rawInput)
                                                (status . "pending")
                                                (title . "Bash")
                                                (kind . "execute")))))))
            (dolist (cumulative (list line1 line2 line3))
              (agent-shell--on-notification
               :state agent-shell--state
               :acp-notification `((method . "session/update")
                                   (params . ((update
                                               . ((_meta (claudeCode (toolResponse (stdout . ,cumulative))))
                                                  (toolCallId . ,tool-id)
                                                  (sessionUpdate . "tool_call_update")))))))
)
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((toolCallId . ,tool-id)
                                                (sessionUpdate . "tool_call_update")
                                                (status . "completed"))))))))
          (with-current-buffer buffer
            (let* ((buf-text (buffer-substring-no-properties (point-min) (point-max)))
                   (count-line2 (let ((c 0) (s 0))
                                  (while (string-match "line 2" buf-text s)
                                    (setq c (1+ c) s (match-end 0)))
                                  c)))
              (should (string-match-p "line 9" buf-text))
              ;; Without delta dedup, "line 2" appears 3× (once per cumulative).
              (should (= count-line2 1)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--initialize-request-includes-terminal-output-meta-test ()
  "Initialize request should include terminal_output meta capability.
Without this, agents like claude-agent-acp will not send
toolResponse.stdout streaming updates."
  (let* ((buffer (get-buffer-create " *agent-shell-init-request*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer)))
    (map-put! agent-shell--state :client 'test-client)
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode)
      (setq-local agent-shell--state agent-shell--state))
    (unwind-protect
        (let ((captured-request nil))
          (cl-letf (((symbol-function 'acp-send-request)
                     (lambda (&rest args)
                       (setq captured-request (plist-get args :request)))))
            (agent-shell--initiate-handshake
             :shell-buffer buffer
             :on-initiated (lambda () nil)))
          (should (eq t (map-nested-elt captured-request
                                        '(:params clientCapabilities _meta terminal_output)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--codex-terminal-output-streams-without-duplication-test ()
  "Codex-acp streams via terminal_output.data; output must not duplicate.
Replays the codex notification pattern: tool_call with terminal content,
incremental terminal_output.data chunks, then completed update."
  (let* ((buffer (get-buffer-create " *agent-shell-codex-dedup*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (tool-id "call_codex_test"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            ;; Notification 1: tool_call (in_progress, terminal content)
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((sessionUpdate . "tool_call")
                                            (toolCallId . ,tool-id)
                                            (title . "Run echo test")
                                            (kind . "execute")
                                            (status . "in_progress")
                                            (content . [((type . "terminal")
                                                         (terminalId . ,tool-id))])
                                            (_meta (terminal_info
                                                    (terminal_id . ,tool-id)))))))))
            ;; Notification 2: first terminal_output.data chunk
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((sessionUpdate . "tool_call_update")
                                            (toolCallId . ,tool-id)
                                            (_meta (terminal_output
                                                    (terminal_id . ,tool-id)
                                                    (data . "alpha\n")))))))))
            ;; Notification 3: second terminal_output.data chunk
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((sessionUpdate . "tool_call_update")
                                            (toolCallId . ,tool-id)
                                            (_meta (terminal_output
                                                    (terminal_id . ,tool-id)
                                                    (data . "bravo\n")))))))))
            ;; Notification 4: completed
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((sessionUpdate . "tool_call_update")
                                            (toolCallId . ,tool-id)
                                            (status . "completed")
                                            (_meta (terminal_exit
                                                    (terminal_id . ,tool-id)
                                                    (exit_code . 0)))))))))))
          (with-current-buffer buffer
            (let* ((buf-text (buffer-substring-no-properties (point-min) (point-max)))
                   (count-alpha (let ((c 0) (s 0))
                                  (while (string-match "alpha" buf-text s)
                                    (setq c (1+ c) s (match-end 0)))
                                  c)))
              ;; Both chunks rendered
              (should (string-match-p "alpha" buf-text))
              (should (string-match-p "bravo" buf-text))
              ;; No duplication
              (should (= count-alpha 1))
              ;; Streamed-append text must carry the same comint /
              ;; tooltip properties the initial body insert applies, or
              ;; comint field navigation and prompt-boundary detection
              ;; degrade across the streamed region.  Walk every "bravo"
              ;; position (the second streamed chunk, inserted via the
              ;; bypass path).
              (save-excursion
                (goto-char (point-min))
                (let ((found nil))
                  (while (search-forward "bravo" nil t)
                    (setq found t)
                    (let ((p (match-beginning 0)))
                      (should (eq (get-text-property p 'field) 'output))
                      (should (eq (get-text-property p 'agent-shell-ui-section) 'body))
                      (should (stringp (get-text-property p 'help-echo)))))
                  (should found)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--mixed-source-output-no-duplication-test ()
  "Tool output streamed via terminal_output and finalized via _meta.toolResponse must dedup.
Some agents stream incremental terminal_output.data while in
progress, then send the full stdout via _meta.toolResponse on the
final update.  The accumulator must recognize the cumulative
re-delivery and not re-emit text already present."
  (let* ((buffer (get-buffer-create " *agent-shell-mixed-source*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (agent-shell--transcript-file nil)
         (tool-id "call_mixed_source"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            ;; tool_call: in_progress with terminal content placeholder.
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((sessionUpdate . "tool_call")
                                                (toolCallId . ,tool-id)
                                                (title . "Run mixed test")
                                                (kind . "execute")
                                                (status . "in_progress")
                                                (content . [((type . "terminal")
                                                             (terminalId . ,tool-id))])))))))
            ;; Two streamed terminal_output chunks.
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((sessionUpdate . "tool_call_update")
                                                (toolCallId . ,tool-id)
                                                (_meta (terminal_output
                                                        (terminal_id . ,tool-id)
                                                        (data . "hello\n")))))))))
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((sessionUpdate . "tool_call_update")
                                                (toolCallId . ,tool-id)
                                                (_meta (terminal_output
                                                        (terminal_id . ,tool-id)
                                                        (data . "world\n")))))))))
            ;; Final completed update carries _meta.toolResponse.stdout
            ;; with the full output (no terminal_output.data this time).
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((sessionUpdate . "tool_call_update")
                                                (toolCallId . ,tool-id)
                                                (status . "completed")
                                                (_meta (claudeCode
                                                        (toolResponse
                                                         (stdout . "hello\nworld\n"))))))))))
            (let* ((buf-text (buffer-substring-no-properties (point-min) (point-max)))
                   (count-occurrences (lambda (needle)
                                        (let ((c 0) (s 0))
                                          (while (string-match (regexp-quote needle) buf-text s)
                                            (setq c (1+ c) s (match-end 0)))
                                          c))))
              (should (string-match-p "hello" buf-text))
              (should (string-match-p "world" buf-text))
              (should (= 1 (funcall count-occurrences "hello")))
              (should (= 1 (funcall count-occurrences "world"))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--mixed-source-output-extends-with-final-tail-test ()
  "If _meta.toolResponse on the final update carries text the stream missed, append it."
  (let* ((buffer (get-buffer-create " *agent-shell-mixed-source-tail*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (agent-shell--transcript-file nil)
         (tool-id "call_mixed_source_tail"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((sessionUpdate . "tool_call")
                                                (toolCallId . ,tool-id)
                                                (title . "Run extends test")
                                                (kind . "execute")
                                                (status . "in_progress")
                                                (content . [((type . "terminal")
                                                             (terminalId . ,tool-id))])))))))
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((sessionUpdate . "tool_call_update")
                                                (toolCallId . ,tool-id)
                                                (_meta (terminal_output
                                                        (terminal_id . ,tool-id)
                                                        (data . "first chunk\n")))))))))
            ;; Final brings the full stdout — the second line was never
            ;; streamed via terminal_output.
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((sessionUpdate . "tool_call_update")
                                                (toolCallId . ,tool-id)
                                                (status . "completed")
                                                (_meta (claudeCode
                                                        (toolResponse
                                                         (stdout . "first chunk\nsecond chunk\n"))))))))))
            (let ((buf-text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "first chunk" buf-text))
              (should (string-match-p "second chunk" buf-text))
              (let ((c 0) (s 0))
                (while (string-match "first chunk" buf-text s)
                  (setq c (1+ c) s (match-end 0)))
                (should (= 1 c))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))


;;; Thought chunk dedup tests

(ert-deftest agent-shell--thought-chunk-delta-incremental-test ()
  "Incremental tokens with no prefix overlap pass through unchanged."
  (should (equal (agent-shell--thought-chunk-delta "AB" "CD") "CD"))
  (should (equal (agent-shell--thought-chunk-delta nil "hello") "hello"))
  (should (equal (agent-shell--thought-chunk-delta "" "hello") "hello")))

(ert-deftest agent-shell--thought-chunk-delta-cumulative-test ()
  "Cumulative re-delivery returns only the new tail."
  (should (equal (agent-shell--thought-chunk-delta "AB" "ABCD") "CD"))
  (should (equal (agent-shell--thought-chunk-delta "hello " "hello world") "world")))

(ert-deftest agent-shell--thought-chunk-delta-exact-duplicate-test ()
  "Exact duplicate returns empty string."
  (should (equal (agent-shell--thought-chunk-delta "ABCD" "ABCD") "")))

(ert-deftest agent-shell--thought-chunk-delta-suffix-test ()
  "Chunk already present as suffix of accumulated returns empty string.
This handles the case where leading whitespace tokens were streamed
incrementally but the re-delivery omits them."
  (should (equal (agent-shell--thought-chunk-delta "\n\nABCD" "ABCD") ""))
  (should (equal (agent-shell--thought-chunk-delta "\n\n**bold**" "**bold**") "")))

(ert-deftest agent-shell--thought-chunk-delta-partial-overlap-test ()
  "Partial overlap between tail of accumulated and head of chunk.
When an agent re-delivers text that partially overlaps with what
was already accumulated, only the genuinely new portion is returned."
  (should (equal (agent-shell--thought-chunk-delta "ABCD" "CDEF") "EF"))
  (should (equal (agent-shell--thought-chunk-delta "hello world" "world!") "!"))
  (should (equal (agent-shell--thought-chunk-delta "abc" "cde") "de"))
  ;; No overlap falls through to full chunk
  (should (equal (agent-shell--thought-chunk-delta "AB" "CD") "CD")))

(ert-deftest agent-shell--thought-chunk-no-duplication-test ()
  "Thought chunks must not produce duplicate output in the buffer.
Replays the codex doubling pattern: incremental tokens followed by
a cumulative re-delivery of the complete thought text."
  (let* ((buffer (get-buffer-create " *agent-shell-thought-dedup*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (agent-shell--transcript-file nil)
         (thought-text "**Checking beads**\n\nLooking for .beads directory."))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf ()
          (with-current-buffer buffer
            ;; Send incremental tokens
            (dolist (token (list "\n\n" "**Checking" " beads**" "\n\n"
                                 "Looking" " for" " .beads" " directory."))
              (agent-shell--on-notification
               :state agent-shell--state
               :acp-notification `((method . "session/update")
                                   (params . ((update
                                               . ((sessionUpdate . "agent_thought_chunk")
                                                  (content (type . "text")
                                                           (text . ,token)))))))))
            ;; Cumulative re-delivery of the complete text
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((sessionUpdate . "agent_thought_chunk")
                                                (content (type . "text")
                                                         (text . ,thought-text))))))))
            (let* ((buf-text (buffer-substring-no-properties (point-min) (point-max)))
                   (count (let ((c 0) (s 0))
                            (while (string-match "Checking beads" buf-text s)
                              (setq c (1+ c) s (match-end 0)))
                            c)))
              ;; Content must be present
              (should (string-match-p "Checking beads" buf-text))
              ;; Must appear exactly once (no duplication)
              (should (= count 1)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-ui-update-fragment-append-preserves-point-test ()
  "Appending body text must not displace point.
The append-in-place path inserts at the body end without
delete-and-reinsert, so markers (and thus point via save-excursion)
remain stable."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Create a fragment with initial body
      (let ((model (list (cons :namespace-id "1")
                         (cons :block-id "pt")
                         (cons :label-left "Status")
                         (cons :body "first chunk"))))
        (agent-shell-ui-update-fragment model :expanded t))
      ;; Place point inside the body text
      (goto-char (point-min))
      (search-forward "first")
      (let ((saved (point)))
        ;; Append more body text
        (let ((model2 (list (cons :namespace-id "1")
                            (cons :block-id "pt")
                            (cons :body " second chunk"))))
          (agent-shell-ui-update-fragment model2 :append t :expanded t))
        ;; Point must not have moved
        (should (= (point) saved))
        ;; Both chunks present in correct order
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "first chunk second chunk" text)))))))

(ert-deftest agent-shell-ui-update-fragment-append-with-label-change-test ()
  "Appending body with a new label must update the label.
The in-place append path must fall back to a full rebuild when the
caller provides a new :label-left or :label-right alongside :append t,
otherwise the label change is silently dropped."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Create a fragment with initial label and body
      (let ((model (list (cons :namespace-id "1")
                         (cons :block-id "boot")
                         (cons :label-left "[busy] Starting")
                         (cons :body "Initializing..."))))
        (agent-shell-ui-update-fragment model :expanded t))
      ;; Verify initial label
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "\\[busy\\] Starting" text)))
      ;; Append body AND change label
      (let ((model2 (list (cons :namespace-id "1")
                          (cons :block-id "boot")
                          (cons :label-left "[done] Starting")
                          (cons :body "\n\nReady"))))
        (agent-shell-ui-update-fragment model2 :append t :expanded t))
      ;; Label must now say [done], not [busy]
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "\\[done\\] Starting" text))
        (should-not (string-match-p "\\[busy\\]" text))
        ;; Body should contain both chunks
        (should (string-match-p "Initializing" text))
        (should (string-match-p "Ready" text))))))

(ert-deftest agent-shell-ui-update-fragment-append-preserves-single-newline-test ()
  "Appending a chunk whose text starts with a single newline must
preserve that newline.  Regression: the append-in-place path
previously stripped leading newlines from each chunk, collapsing
markdown list item separators (e.g. \"&&.\\n2.\" became \"&&.2.\")."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (let ((model (list (cons :namespace-id "1")
                         (cons :block-id "nl")
                         (cons :label-left "Agent")
                         (cons :body "1. First item"))))
        (agent-shell-ui-update-fragment model :expanded t))
      (let ((model2 (list (cons :namespace-id "1")
                          (cons :block-id "nl")
                          (cons :body "\n2. Second item"))))
        (agent-shell-ui-update-fragment model2 :append t :expanded t))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "First item\n.*2\\. Second item" text))))))

(ert-deftest agent-shell-ui-update-fragment-append-preserves-double-newline-test ()
  "Appending a chunk starting with a double newline (paragraph break)
must preserve both newlines."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (let ((model (list (cons :namespace-id "1")
                         (cons :block-id "dnl")
                         (cons :label-left "Agent")
                         (cons :body "Paragraph one."))))
        (agent-shell-ui-update-fragment model :expanded t))
      (let ((model2 (list (cons :namespace-id "1")
                          (cons :block-id "dnl")
                          (cons :body "\n\nParagraph two."))))
        (agent-shell-ui-update-fragment model2 :append t :expanded t))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Paragraph one\\.\n.*\n.*Paragraph two" text))))))

(ert-deftest agent-shell-ui-update-fragment-append-caps-boundary-newlines-test ()
  "Boundary newlines between existing body and appended chunk cap at two.
When the existing body already ends in newline(s) and the appended chunk
starts with newline(s), naive concatenation yields three or more
consecutive newlines (an extra blank line).  Cap the run at two."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Existing body ends with one trailing \n.
      (let ((model (list (cons :namespace-id "1")
                         (cons :block-id "cap")
                         (cons :label-left "Agent")
                         (cons :body "First line.\n"))))
        (agent-shell-ui-update-fragment model :expanded t))
      ;; Appended chunk leads with two newlines (paragraph break).
      (let ((model2 (list (cons :namespace-id "1")
                          (cons :block-id "cap")
                          (cons :body "\n\nSecond line."))))
        (agent-shell-ui-update-fragment model2 :append t :expanded t))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        ;; Expect exactly two newlines between "First line." and "Second line.".
        (should (string-match-p "First line\\.\n\nSecond line\\." text))
        (should-not (string-match-p "First line\\.\n\n\n" text))))))

;;; Insert-before tests (content above prompt)

(ert-deftest agent-shell-ui-update-fragment-insert-before-test ()
  "New fragment with :insert-before inserts above that position.
Simulates a prompt at the end of the buffer; the new fragment
must appear before the prompt text, not after it."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Simulate existing output followed by a prompt.
      (insert "previous output\n\nClaude Code> ")
      (let ((prompt-start (- (point) (length "Claude Code> "))))
        ;; Insert a notice fragment before the prompt.
        (let ((model (list (cons :namespace-id "1")
                           (cons :block-id "notice")
                           (cons :label-left "Notices")
                           (cons :body "Something happened"))))
          (agent-shell-ui-update-fragment model
                                          :expanded t
                                          :insert-before prompt-start))
        ;; The prompt must still be at the end.
        (should (string-suffix-p "Claude Code> "
                                 (buffer-substring-no-properties (point-min) (point-max))))
        ;; The notice body must appear before the prompt.
        (let ((notice-pos (save-excursion
                            (goto-char (point-min))
                            (search-forward "Something happened" nil t)))
              (prompt-pos (save-excursion
                            (goto-char (point-min))
                            (search-forward "Claude Code> " nil t))))
          (should notice-pos)
          (should prompt-pos)
          (should (< notice-pos prompt-pos)))))))

(ert-deftest agent-shell-ui-update-text-insert-before-test ()
  "New text entry with :insert-before inserts above that position."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert "previous output\n\nClaude Code> ")
      (let ((prompt-start (- (point) (length "Claude Code> "))))
        (agent-shell-ui-update-text
         :namespace-id "1"
         :block-id "user-msg"
         :text "yes"
         :insert-before prompt-start)
        ;; Prompt must remain at the end.
        (should (string-suffix-p "Claude Code> "
                                 (buffer-substring-no-properties (point-min) (point-max))))
        ;; User message must appear before the prompt.
        (let ((msg-pos (save-excursion
                         (goto-char (point-min))
                         (search-forward "yes" nil t)))
              (prompt-pos (save-excursion
                            (goto-char (point-min))
                            (search-forward "Claude Code> " nil t))))
          (should msg-pos)
          (should prompt-pos)
          (should (< msg-pos prompt-pos)))))))

(ert-deftest agent-shell-ui-update-fragment-insert-before-nil-test ()
  "When :insert-before is nil, new fragment inserts at end (default)."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert "previous output")
      (let ((model (list (cons :namespace-id "1")
                         (cons :block-id "msg")
                         (cons :label-left "Agent")
                         (cons :body "hello"))))
        (agent-shell-ui-update-fragment model :expanded t :insert-before nil))
      (should (string-suffix-p "hello\n\n"
                               (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest agent-shell--mark-tool-calls-cancelled-with-nil-transcript-test ()
  "Cancelling in-flight tool calls must not signal when transcript-file is nil.
agent-shell--mark-tool-calls-cancelled invokes handle-tool-call-final
which appends transcript entries; the transcript helper must tolerate
a nil file-path or interrupting an unsaved session would crash."
  (let* ((buffer (get-buffer-create " *agent-shell-cancel-nil-transcript*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (agent-shell--transcript-file nil)
         (tool-id "toolu_cancel_test"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            ;; Send a tool_call so there's an in-flight entry to cancel.
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((sessionUpdate . "tool_call")
                                                (toolCallId . ,tool-id)
                                                (title . "Run cancel test")
                                                (kind . "execute")
                                                (status . "pending")))))))
            ;; Cancel must not signal even with nil transcript-file.
            (agent-shell--mark-tool-calls-cancelled agent-shell--state)
            (let ((status (map-nested-elt agent-shell--state
                                          `(:tool-calls ,tool-id :status))))
              (should (equal status "cancelled")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--tool-call-update-overrides-nil-title-test ()
  "Overrides must not signal when existing title is nil.
When a tool_call_update arrives before the initial tool_call has
set a title, the title-upgrade path must not crash on string=."
  (let* ((state (list (cons :tool-calls
                            (list (cons "tc-1" (list (cons :status "pending")))))))
         (update '((toolCallId . "tc-1")
                   (status . "in_progress"))))
    (should (listp (agent-shell--tool-call-update-overrides
                    state update nil nil)))))

(ert-deftest agent-shell--tool-call-update-overrides-upgrades-title-test ()
  "A non-empty title in tool_call_update replaces the existing one.
Mirrors the non-streaming dispatcher in agent-shell.el so a generic
initial title (\"Bash\") is upgraded when a richer one arrives."
  (let* ((state (list (cons :tool-calls
                            (list (cons "tc-1" (list (cons :title "Bash")
                                                     (cons :status "pending")))))))
         (update '((toolCallId . "tc-1")
                   (status . "in_progress")
                   (title . "grep -i -n pattern /path/to/file"))))
    (should (equal "grep -i -n pattern /path/to/file"
                   (map-elt (agent-shell--tool-call-update-overrides
                             state update nil nil)
                            :title)))))

(ert-deftest agent-shell--tool-call-update-overrides-empty-title-test ()
  "An empty-string title in tool_call_update is ignored.
Otherwise the existing descriptive title would be clobbered."
  (let* ((state (list (cons :tool-calls
                            (list (cons "tc-1" (list (cons :title "Bash")
                                                     (cons :status "pending")))))))
         (update '((toolCallId . "tc-1")
                   (status . "in_progress")
                   (title . ""))))
    (should-not (map-elt (agent-shell--tool-call-update-overrides
                          state update nil nil)
                         :title))))

;;; Label status transition tests

(ert-deftest agent-shell--tool-call-update-overrides-uses-correct-keyword-test ()
  "Overrides with include-diff must use :acp-tool-call keyword.
Previously used :tool-call which caused a cl-defun keyword error,
aborting handle-tool-call-final before the label update."
  (let* ((state (list (cons :tool-calls
                            (list (cons "tc-1" (list (cons :title "Read")
                                                     (cons :status "pending")))))))
         (update '((toolCallId . "tc-1")
                   (status . "completed")
                   (content . [((content . ((text . "ok"))))]))))
    ;; With include-diff=t, this must not signal
    ;; "Keyword argument :tool-call not one of (:acp-tool-call)"
    (should (listp (agent-shell--tool-call-update-overrides
                    state update t t)))))

(ert-deftest agent-shell--tool-call-label-transitions-to-done-test ()
  "Tool call label must transition from pending to done on completion.
Replays tool_call (pending) then tool_call_update (completed) and
verifies the buffer contains the done label, not wait."
  (let* ((buffer (get-buffer-create " *agent-shell-label-done*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (tool-id "toolu_label_done"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            ;; tool_call (pending)
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call")
                                            (rawInput)
                                            (status . "pending")
                                            (title . "Read")
                                            (kind . "read")))))))
            ;; Verify initial label is wait (pending)
            (let ((buf-text (buffer-string)))
              (should (string-match-p "wait" buf-text)))
            ;; tool_call_update (completed)
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call_update")
                                            (status . "completed")
                                            (content . [((content . ((text . "file contents"))))])))))))
            ;; Label must now be done, not wait
            (let ((buf-text (buffer-string)))
              (should (string-match-p "done" buf-text))
              (should-not (string-match-p "wait" buf-text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--tool-call-label-updates-on-in-progress-test ()
  "Non-final tool_call_update must update label from wait to busy.
Upstream updates labels on every tool_call_update, not just final."
  (let* ((buffer (get-buffer-create " *agent-shell-label-busy*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (tool-id "toolu_label_busy"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            ;; tool_call (pending)
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call")
                                            (rawInput)
                                            (status . "pending")
                                            (title . "Bash")
                                            (kind . "execute")))))))
            (let ((buf-text (buffer-string)))
              (should (string-match-p "wait" buf-text)))
            ;; tool_call_update (in_progress, no content)
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call_update")
                                            (status . "in_progress")))))))
            ;; Label must now be busy, not wait
            (let ((buf-text (buffer-string)))
              (should (string-match-p "busy" buf-text))
              (should-not (string-match-p "wait" buf-text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--tool-call-command-block-in-body-test ()
  "Completed execute tool call must show saved command as fenced console block.
Upstream commit 75cc736 prepends a ```console block to the body when the
tool call has a saved :command.  Verify the fenced block appears."
  (let* ((buffer (get-buffer-create " *agent-shell-cmd-block*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (tool-id "toolu_cmd_block"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            ;; tool_call (pending) with rawInput command
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call")
                                            (rawInput (command . "echo hello world"))
                                            (status . "pending")
                                            (title . "Bash")
                                            (kind . "execute")))))))
            ;; tool_call_update (completed) with output
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call_update")
                                            (status . "completed")
                                            (content . [((content . ((text . "hello world"))))])))))))
            ;; Buffer must contain the fenced console command block
            (let ((buf-text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "```console" buf-text))
              (should (string-match-p "echo hello world" buf-text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--tool-call-meta-response-on-final-only-test ()
  "Meta toolResponse arriving only on the final update must render output.
Some agents send stdout exclusively on the completed tool_call_update
with no prior meta chunks.  The output must not be dropped."
  (let* ((buffer (get-buffer-create " *agent-shell-meta-final*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (tool-id "toolu_meta_final"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            ;; tool_call (pending)
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call")
                                            (rawInput)
                                            (status . "pending")
                                            (title . "Bash")
                                            (kind . "execute")))))))
            ;; tool_call_update (completed) with _meta stdout only, no prior chunks
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((_meta (claudeCode (toolResponse (stdout . "final-only-output")
                                                                             (stderr . "")
                                                                             (interrupted)
                                                                             (isImage)
                                                                             (noOutputExpected))
                                                               (toolName . "Bash")))
                                            (toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call_update")
                                            (status . "completed")))))))
            ;; Output must be rendered, not dropped
            (let ((buf-text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "final-only-output" buf-text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--empty-chunk-inserts-paragraph-break-test ()
  "An empty agent_message_chunk mid-stream inserts a paragraph break.
Regression: when the model produces two content blocks in the same
turn (e.g. a description followed by a background-task result),
the ACP sends an empty chunk at the boundary.  Without converting
that to a paragraph break, the end of the first block and the
start of the second get merged: \"pipeline.Full test suite passed\"."
  (let* ((buffer (get-buffer-create " *agent-shell-empty-chunk-para*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (agent-shell--transcript-file nil)
         (tool-id "toolu_empty_chunk_test"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            ;; Completed tool call (background task)
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call")
                                            (rawInput)
                                            (status . "pending")
                                            (title . "Bash")
                                            (kind . "execute")))))))
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call_update")
                                            (status . "completed")))))))
            ;; First content block: empty start chunk + text
            (dolist (token (list "" "First paragraph" " ending."))
              (agent-shell--on-notification
               :state agent-shell--state
               :acp-notification `((method . "session/update")
                               (params . ((update
                                           . ((sessionUpdate . "agent_message_chunk")
                                              (content (type . "text")
                                                       (text . ,token)))))))))
            ;; Second content block: empty boundary chunk + text
            (dolist (token (list "" "Second paragraph" " starting."))
              (agent-shell--on-notification
               :state agent-shell--state
               :acp-notification `((method . "session/update")
                               (params . ((update
                                           . ((sessionUpdate . "agent_message_chunk")
                                              (content (type . "text")
                                                       (text . ,token)))))))))
            ;; The two paragraphs must NOT be merged.
            (let ((visible-text (agent-shell-test--visible-buffer-string)))
              (should (string-match-p "First paragraph ending\\." visible-text))
              (should (string-match-p "Second paragraph starting\\." visible-text))
              ;; The boundary must include whitespace, not "ending.Second"
              (should-not (string-match-p "ending\\.Second" visible-text))
              ;; And the boundary must be exactly one blank line (two
              ;; consecutive newlines) — not a triple-newline regression
              ;; if the existing chunk already trailed with a newline.
              (should (string-match-p "ending\\.\n\nSecond paragraph" visible-text))
              (should-not (string-match-p "ending\\.\n\n\nSecond paragraph" visible-text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--agent-message-chunks-fully-visible-test ()
  "All agent_message_chunk tokens must be visible in the buffer.
Regression: label-less fragments defaulted to :collapsed t.  The
in-place append path used `insert-and-inherit', which inherited the
`invisible t' property from the trailing-whitespace-hiding step of
the previous body text, making every appended chunk invisible.

Replays the traffic captured in the debug log: a completed tool call
followed by streaming agent_message_chunk tokens.  The full message
\"All 10 tests pass.\" must be visible, not just \"All\"."
  (let* ((buffer (get-buffer-create " *agent-shell-msg-chunk-visible*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (agent-shell--transcript-file nil)
         (tool-id "toolu_msg_chunk_test"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            ;; tool_call (pending)
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call")
                                            (rawInput)
                                            (status . "pending")
                                            (title . "Bash")
                                            (kind . "execute")))))))
            ;; tool_call_update with toolResponse.stdout
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((_meta (claudeCode (toolResponse (stdout . "Ran 10 tests, 10 results as expected")
                                                                             (stderr . "")
                                                                             (interrupted)
                                                                             (isImage)
                                                                             (noOutputExpected))
                                                               (toolName . "Bash")))
                                            (toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call_update")))))))
            ;; tool_call_update completed
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call_update")
                                            (status . "completed")))))))
            ;; Now stream agent_message_chunk tokens (the agent's
            ;; conversational response).  This is label-less text.
            (dolist (token (list "All " "10 tests pass" "." " Now"
                                 " let me prepare" " the PR."))
              (agent-shell--on-notification
               :state agent-shell--state
               :acp-notification `((method . "session/update")
                               (params . ((update
                                           . ((sessionUpdate . "agent_message_chunk")
                                              (content (type . "text")
                                                       (text . ,token)))))))))
            ;; The full message must be present AND visible.
            (let ((visible-text (agent-shell-test--visible-buffer-string)))
              (should (string-match-p "All 10 tests pass" visible-text))
              (should (string-match-p "let me prepare the PR" visible-text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--post-turn-end-chunks-render-test ()
  "Notifications streamed after the turn appears finished must render.
Regression: when an ACP agent (e.g.  Claude Code under a Stop-hook
bounce) sends more session/update notifications after the
session/prompt request has resolved, agent-shell would treat them as
stale and silently drop them — the buffer froze on the previous
message while the agent kept streaming.  Replays a realistic
post-bounce sequence (thought chunk, tool_call, tool_call_update,
agent_message_chunk) and asserts every piece appears in the buffer."
  (let* ((buffer (get-buffer-create " *agent-shell-post-turn-end*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (agent-shell--transcript-file nil)
         (post-tool-id "toolu_post_bounce_test"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            ;; Pre-bounce: request is active, chunk renders normally.
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((sessionUpdate . "agent_message_chunk")
                                                (content (type . "text")
                                                         (text . "Pre-bounce reply.")))))))) ;
            ;; Simulate the session/prompt response arriving — request
            ;; is no longer active.
            (map-put! agent-shell--state :active-requests nil)
            ;; Post-bounce regen turn: thought chunk first.
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((sessionUpdate . "agent_thought_chunk")
                                                (content (type . "text")
                                                         (text . "post-bounce thought")))))))) ;
            ;; Post-bounce tool_call (pending).
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((sessionUpdate . "tool_call")
                                                (toolCallId . ,post-tool-id)
                                                (rawInput)
                                                (status . "pending")
                                                (title . "Bash")
                                                (kind . "execute"))))))) ;
            ;; Post-bounce tool_call_update (completed).
            (agent-shell--on-notification
             :state agent-shell--state
             :acp-notification `((method . "session/update")
                                 (params . ((update
                                             . ((sessionUpdate . "tool_call_update")
                                                (toolCallId . ,post-tool-id)
                                                (status . "completed")
                                                (content . [((content . ((text . "post-bounce-tool-output")))) ])))))))
            ;; Post-bounce assistant message chunks.
            (dolist (token (list "Post-bounce " "regen " "content."))
              (agent-shell--on-notification
               :state agent-shell--state
               :acp-notification `((method . "session/update")
                                   (params . ((update
                                               . ((sessionUpdate . "agent_message_chunk")
                                                  (content (type . "text")
                                                           (text . ,token))))))))) ;
            (let ((visible-text (agent-shell-test--visible-buffer-string))
                  (buf-text (buffer-substring-no-properties (point-min) (point-max))))
              ;; Label-less message chunks must be visible (no
              ;; collapsing).  Pre- and post-bounce content both render.
              (should (string-match-p "Pre-bounce reply" visible-text))
              (should (string-match-p "Post-bounce regen content" visible-text))
              ;; Thought chunks and tool calls render under collapsed
              ;; drawers — present in the buffer even though invisible.
              (should (string-match-p "Thinking" buf-text))
              (should (string-match-p "post-bounce thought" buf-text))
              (should (string-match-p "Bash" buf-text))
              (should (string-match-p "post-bounce-tool-output" buf-text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun agent-shell-test--visible-buffer-string ()
  "Return buffer text with invisible regions removed."
  (let ((result "")
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((next-change (next-single-property-change pos 'invisible nil (point-max))))
        (unless (get-text-property pos 'invisible)
          (setq result (concat result (buffer-substring-no-properties pos next-change))))
        (setq pos next-change)))
    result))

(ert-deftest agent-shell-ui--split-qualified-id-no-hyphen-in-block-id-test ()
  (should (equal (agent-shell-ui--split-qualified-id "1-toolu123")
                 '("1" . "toolu123"))))

(ert-deftest agent-shell-ui--split-qualified-id-hyphenated-block-id-test ()
  ;; Block-ids commonly carry hyphens; greedy-first parsing would
  ;; misattribute them to the namespace.
  (should (equal (agent-shell-ui--split-qualified-id "1-toolu123-plan")
                 '("1" . "toolu123-plan")))
  (should (equal (agent-shell-ui--split-qualified-id "1-permission-toolu123")
                 '("1" . "permission-toolu123")))
  (should (equal (agent-shell-ui--split-qualified-id "2-failed-x-id:y-code:z")
                 '("2" . "failed-x-id:y-code:z"))))

(ert-deftest agent-shell-ui--split-qualified-id-no-hyphen-test ()
  (should (null (agent-shell-ui--split-qualified-id "single"))))

(provide 'agent-shell-streaming-tests)
;;; agent-shell-streaming-tests.el ends here
