;;; agent-shell-table-tests.el --- Tests for markdown table rendering -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)

;;; Code:

;; Reuse the visible-buffer-string helper if available, otherwise define it.
(unless (fboundp 'agent-shell-test--visible-buffer-string)
  (defun agent-shell-test--visible-buffer-string ()
    "Return buffer text with invisible regions removed."
    (let ((result "")
          (pos (point-min)))
      (while (< pos (point-max))
        (let ((next-change (next-single-property-change pos 'invisible nil (point-max))))
          (unless (get-text-property pos 'invisible)
            (setq result (concat result (buffer-substring-no-properties pos next-change))))
          (setq pos next-change)))
      result)))

(defun agent-shell-table-test--setup-buffer ()
  "Create and return a test buffer with agent-shell-mode initialized."
  (let ((buffer (get-buffer-create " *agent-shell-table-test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    buffer))

(defun agent-shell-table-test--fire-debounce ()
  "Fire pending markdown overlay debounce timer if present."
  (when (and (boundp 'agent-shell--markdown-overlay-timer)
             (timerp agent-shell--markdown-overlay-timer))
    (timer-event-handler agent-shell--markdown-overlay-timer)))

(defun agent-shell-table-test--table-overlays ()
  "Return table overlays in the current buffer, sorted by position.
Each element is an alist with :start, :end, and :before-string."
  (let ((result nil))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'markdown-overlays-tables)
        (push (list (cons :start (overlay-start ov))
                    (cons :end (overlay-end ov))
                    (cons :before-string
                          (when-let ((bs (overlay-get ov 'before-string)))
                            (substring-no-properties bs))))
              result)))
    (sort result (lambda (a b) (< (map-elt a :start) (map-elt b :start))))))

(defun agent-shell-table-test--send-tool-call (state tool-id)
  "Send a complete tool_call lifecycle (pending → meta → completed).
STATE is agent-shell--state, TOOL-ID is the tool call identifier."
  (agent-shell--on-notification
   :state state
   :acp-notification `((method . "session/update")
                    (params . ((update
                                . ((toolCallId . ,tool-id)
                                   (sessionUpdate . "tool_call")
                                   (rawInput)
                                   (status . "pending")
                                   (title . "Bash")
                                   (kind . "execute")))))))
  (agent-shell--on-notification
   :state state
   :acp-notification `((method . "session/update")
                    (params . ((update
                                . ((_meta (claudeCode (toolResponse (stdout . "tool output")
                                                                    (stderr . "")
                                                                    (interrupted)
                                                                    (isImage)
                                                                    (noOutputExpected))
                                                      (toolName . "Bash")))
                                   (toolCallId . ,tool-id)
                                   (sessionUpdate . "tool_call_update")))))))
  (agent-shell--on-notification
   :state state
   :acp-notification `((method . "session/update")
                    (params . ((update
                                . ((toolCallId . ,tool-id)
                                   (sessionUpdate . "tool_call_update")
                                   (status . "completed"))))))))

(defun agent-shell-table-test--send-message-chunks (state tokens)
  "Send agent_message_chunk notifications for each token in TOKENS.
STATE is agent-shell--state."
  (dolist (token tokens)
    (agent-shell--on-notification
     :state state
     :acp-notification `((method . "session/update")
                      (params . ((update
                                  . ((sessionUpdate . "agent_message_chunk")
                                     (content (type . "text")
                                              (text . ,token))))))))))

;;; The real-world chunks from the debug session, split exactly as ACP
;;; delivered them.  The table has 4 columns and ~10 data rows.
(defconst agent-shell-table-test--chunks
  (list
   "Here's the comparison:\n\n| Policy"
   " | lab-green-003 | prod-green-003 | lab-v6.4-003 |\n|---|---|---|---|\n| dd_pastebin | `removed"
   "` | `removed` | `removed` |\n| onepassword_scim | `removed` | `removed` | `removed` |\n| us1_prod_dog_incidents_app | `removed` | `removed` | `removed` |"
   "\n| us1_prod_dog_pagerbeauty | `removed` | `removed` | `removed` |\n| us1_prod_dog_support_eng_access | `removed` | `removed` | `removed` |\n| us1-"
   "staging-fed-ssh | `removed` | `removed` | `removed` |\n| us1-staging-fed-dns | `removed` | `removed` | `removed` |\n| pci | `removed` | `removed` | `removed` |\n|"
   " production_ga | `removed` | `removed` | `removed` |\n| production_common_services | `removed` | `removed` | `removed` |"
   "\n\nAll 18 policies are aligned.")
  "Chunk sequence from a real debug session containing a markdown table.")


(ert-deftest agent-shell--table-rows-not-split-across-lines-test ()
  "Markdown table rows must render with pipe-delimited cells on single lines.
Regression test: table rows with backtick-wrapped content like `removed`
were being split so that cell content appeared on separate lines below
each row.

Replays actual agent_message_chunk traffic from a debug session where
a 4-column table (Policy / lab-green-003 / prod-green-003 / lab-v6.4-003)
was streamed across multiple chunks with cell boundaries split mid-chunk."
  (let* ((buffer (agent-shell-table-test--setup-buffer))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (agent-shell--transcript-file nil)
         (tool-id "toolu_table_test"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            (agent-shell-table-test--send-tool-call agent-shell--state tool-id)
            (agent-shell-table-test--send-message-chunks
             agent-shell--state agent-shell-table-test--chunks)
            (agent-shell-table-test--fire-debounce)
            ;; Verify: the table content is visible in the raw text.
            (let ((visible-text (agent-shell-test--visible-buffer-string)))
              ;; Header row must be intact on one line.
              (should (string-match-p
                       "| Policy.*| lab-green-003.*| prod-green-003.*| lab-v6.4-003 |"
                       visible-text))
              ;; Separator row.
              (should (string-match-p "|---|---|---|---|" visible-text))
              ;; Data rows: policy name and all three `removed` cells
              ;; must appear on the same logical line.
              (should (string-match-p
                       "| dd_pastebin.*|.*removed.*|.*removed.*|.*removed.*|"
                       visible-text))
              (should (string-match-p
                       "| pci.*|.*removed.*|.*removed.*|.*removed.*|"
                       visible-text))
              ;; Post-table text must be visible.
              (should (string-match-p "All 18 policies are aligned" visible-text)))
            ;; No line should consist of just "removed" — the regression
            ;; symptom of cell content breaking out of the table.
            (let ((visible-text (agent-shell-test--visible-buffer-string)))
              (dolist (line (split-string visible-text "\n"))
                (should-not (string-match-p
                             "\\`[[:space:]]*\\(?:`\\)?removed\\(?:`\\)?[[:space:]]*\\'"
                             line))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--table-overlay-structure-test ()
  "Each table row must have exactly one overlay with correct before-string.
After all chunks arrive and markdown overlays are applied, the overlay
structure should show:
  - 1 header row overlay containing all column names
  - 1 separator overlay
  - N data row overlays, each containing the policy name and all cells"
  (let* ((buffer (agent-shell-table-test--setup-buffer))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (agent-shell--transcript-file nil)
         (tool-id "toolu_overlay_test"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            (agent-shell-table-test--send-tool-call agent-shell--state tool-id)
            (agent-shell-table-test--send-message-chunks
             agent-shell--state agent-shell-table-test--chunks)
            (agent-shell-table-test--fire-debounce)
            (let ((table-ovs (agent-shell-table-test--table-overlays)))
              ;; 1 header + 1 separator + 10 data rows = 12 overlays
              (should (= 12 (length table-ovs)))
              ;; First overlay is the header row.
              (let ((header-bs (map-elt (car table-ovs) :before-string)))
                (should (string-match-p "Policy" header-bs))
                (should (string-match-p "lab-green-003" header-bs))
                (should (string-match-p "prod-green-003" header-bs))
                (should (string-match-p "lab-v6.4-003" header-bs)))
              ;; Each data row overlay (index 2+) must contain "removed"
              ;; and the cell content must be on a single line.
              (dolist (ov (nthcdr 2 table-ovs))
                (let ((bs (map-elt ov :before-string)))
                  (should (string-match-p "removed" bs))
                  ;; The before-string for a single-line row should
                  ;; NOT contain newlines (multi-line wrapping aside).
                  ;; If it does, cells are being split.
                  (should-not (string-match-p "\n" bs)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--table-mid-stream-overlay-cleanup-test ()
  "Overlays from partial table rendering must be cleaned up after full table arrives.
Simulates the debounce timer firing mid-stream (when only part of the
table has been received), then checks that the final overlay state is
correct after all chunks arrive."
  (let* ((buffer (agent-shell-table-test--setup-buffer))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (agent-shell--transcript-file nil)
         (tool-id "toolu_midstream_test")
         (all-chunks agent-shell-table-test--chunks)
         ;; Split: first 3 chunks = partial table, rest = completion.
         (early-chunks (seq-take all-chunks 3))
         (late-chunks (seq-drop all-chunks 3)))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (map-put! agent-shell--state :active-requests (list t))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (cl-function (lambda (&key acp-tool-call) (ignore acp-tool-call)))))
          (with-current-buffer buffer
            (agent-shell-table-test--send-tool-call agent-shell--state tool-id)
            ;; Stream partial table
            (agent-shell-table-test--send-message-chunks
             agent-shell--state early-chunks)
            ;; Fire debounce mid-stream (partial table gets overlaid)
            (agent-shell-table-test--fire-debounce)
            (let ((partial-ovs (agent-shell-table-test--table-overlays)))
              ;; Partial table should have SOME overlays (header + sep + rows so far).
              (should (< 0 (length partial-ovs))))
            ;; Stream remaining chunks
            (agent-shell-table-test--send-message-chunks
             agent-shell--state late-chunks)
            ;; Fire debounce again (full table)
            (agent-shell-table-test--fire-debounce)
            (let ((final-ovs (agent-shell-table-test--table-overlays)))
              ;; Full table: 1 header + 1 separator + 10 data rows = 12
              (should (= 12 (length final-ovs)))
              ;; Every data row overlay should contain "removed".
              (dolist (ov (nthcdr 2 final-ovs))
                (should (string-match-p "removed"
                                        (map-elt ov :before-string)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'agent-shell-table-tests)
;;; agent-shell-table-tests.el ends here
