;;; agent-shell-diff-tests.el --- Tests for agent-shell-diff -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-diff)

;;; Code:

(ert-deftest agent-shell-diff-returns-buffer-test ()
  "Test that `agent-shell-diff' returns the newly created diff buffer."
  (let ((buf (agent-shell-diff
              :diffs (list (list (cons :old "hello\n")
                                 (cons :new "world\n")
                                 (cons :file "test.el"))))))
    (unwind-protect
        (progn
          (should (bufferp buf))
          (should (buffer-live-p buf))
          (should (with-current-buffer buf
                    (derived-mode-p 'diff-mode))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest agent-shell-diff-on-exit-fires-on-kill-test ()
  "Test that ON-EXIT callback fires when the diff buffer is killed."
  (let* ((on-exit-called nil)
         (buf (agent-shell-diff
               :diffs (list (list (cons :old "hello\n")
                                  (cons :new "world\n")
                                  (cons :file "test.el")))
               :on-exit (lambda () (setq on-exit-called t)))))
    (unwind-protect
        (progn
          (should (buffer-live-p buf))
          (kill-buffer buf)
          (should on-exit-called))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest agent-shell-diff-kill-buffer-suppresses-on-exit-test ()
  "Test that `agent-shell-diff-kill-buffer' kills without calling ON-EXIT."
  (let* ((on-exit-called nil)
         (buf (agent-shell-diff
               :diffs (list (list (cons :old "hello\n")
                                  (cons :new "world\n")
                                  (cons :file "test.el")))
               :on-exit (lambda () (setq on-exit-called t)))))
    (unwind-protect
        (progn
          (should (buffer-live-p buf))
          (agent-shell-diff-kill-buffer buf)
          (should-not (buffer-live-p buf))
          (should-not on-exit-called))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest agent-shell-diff-kill-buffer-noop-when-dead-test ()
  "Test that `agent-shell-diff-kill-buffer' is safe on already-dead buffers."
  (let ((buf (generate-new-buffer "*test-diff*")))
    (kill-buffer buf)
    ;; Should not error.
    (agent-shell-diff-kill-buffer buf)))

(ert-deftest agent-shell-diff-kill-buffer-noop-when-nil-test ()
  "Test that `agent-shell-diff-kill-buffer' is safe when called with nil."
  ;; Should not error.
  (agent-shell-diff-kill-buffer nil))

(ert-deftest agent-shell-diff--target-at-point-test ()
  "Test `agent-shell-diff--target-at-point' anchor deduction."
  (with-temp-buffer
    (insert "one.el\n"
            "--- /tmp/a\n"
            "+++ /tmp/b\n"
            "@@ -10,4 +10,4 @@\n"
            " ctx1\n"
            " ctx2\n"
            "-old3\n"
            "+new3\n"
            " ctx4\n"
            "\n")
    (put-text-property (point-min) (point-max) 'agent-shell-diff-file "one.el")

    ;; Point on the removed line targets that line's old-side offset.
    (goto-char (point-min))
    (search-forward "-old3")
    (beginning-of-line)
    (should (equal (agent-shell-diff--target-at-point)
                   '((:file . "one.el")
                     (:hint-line)
                     (:old-block . "ctx1\nctx2\nold3\nctx4")
                     (:new-block . "ctx1\nctx2\nnew3\nctx4")
                     (:offset . 2))))

    ;; Point on a context line targets that context line.
    (goto-char (point-min))
    (search-forward " ctx1")
    (beginning-of-line)
    (should (equal (map-elt (agent-shell-diff--target-at-point) :offset) 0))

    ;; Point on the hunk header targets the hunk's first change.
    (goto-char (point-min))
    (search-forward "@@ -10")
    (beginning-of-line)
    (should (equal (map-elt (agent-shell-diff--target-at-point) :offset) 2))

    ;; Point on the header banner uses the nearest change below.
    (goto-char (point-min))
    (should (equal (map-elt (agent-shell-diff--target-at-point) :offset) 2))

    ;; Point past the last hunk uses the nearest change above.
    (goto-char (point-max))
    (should (equal (map-elt (agent-shell-diff--target-at-point) :offset) 2))))

(ert-deftest agent-shell-diff--target-at-point-new-file-test ()
  "Test `agent-shell-diff--target-at-point' with an addition-only hunk."
  (with-temp-buffer
    (insert "new.el\n"
            "--- /tmp/a\n"
            "+++ /tmp/b\n"
            "@@ -0,0 +1,2 @@\n"
            "+line one\n"
            "+line two\n")
    (put-text-property (point-min) (point-max) 'agent-shell-diff-file "new.el")
    (goto-char (point-min))
    (search-forward "+line two")
    (beginning-of-line)
    (let ((target (agent-shell-diff--target-at-point)))
      ;; A new file has no old-side text to search.
      (should (equal (map-elt target :old-block) nil))
      (should (equal (map-elt target :new-block) "line one\nline two")))))

(ert-deftest agent-shell-diff-open-file-jumps-to-changed-content-test ()
  "Test that opening a diff jumps to the change, not the fragment line.

The diff's oldText/newText are only a fragment of the file, so the
hunk's line numbers do not match the file.  The jump must locate the
change by content."
  (let* ((body (mapconcat (lambda (n) (format "line %d" n))
                          (number-sequence 1 30) "\n"))
         (file (make-temp-file "agent-shell-jump" nil ".txt" (concat body "\n"))))
    (unwind-protect
        ;; Fragment around line 21; its diff hunk is fragment-relative.
        (let ((buf (agent-shell-diff
                    :diffs (list (list (cons :old "line 20\nline 21\nline 22")
                                       (cons :new "line 20\nline 21 CHANGED\nline 22")
                                       (cons :file file))))))
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (search-forward "-line 21")
                (beginning-of-line)
                (agent-shell-diff-open-file)
                ;; find-file switched the current buffer to the visited file.
                (should (equal (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position))
                               "line 21")))
            (when (buffer-live-p buf) (kill-buffer buf))
            (when (find-buffer-visiting file) (kill-buffer (find-buffer-visiting file)))))
      (delete-file file))))

(ert-deftest agent-shell-diff-open-file-falls-back-to-new-side-test ()
  "Test that an already-applied addition is found via the new-side text.

When the change is applied, the old-side block is no longer contiguous
in the file (the addition sits in the middle of it), so the jump must
fall back to searching the new-side text.  Mirrors the real Claude Code
`* Welcome' prepend in welcome.traffic."
  (let* ((file (make-temp-file "agent-shell-jump" nil ".txt"
                               "alpha\nADDED\nbeta\n")))
    (unwind-protect
        (let ((buf (agent-shell-diff
                    :diffs (list (list (cons :old "alpha\nbeta")
                                       (cons :new "alpha\nADDED\nbeta")
                                       (cons :file file))))))
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (search-forward "+ADDED")
                (beginning-of-line)
                (agent-shell-diff-open-file)
                (should (equal (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position))
                               "ADDED")))
            (when (buffer-live-p buf) (kill-buffer buf))
            (when (find-buffer-visiting file) (kill-buffer (find-buffer-visiting file)))))
      (delete-file file))))

(ert-deftest agent-shell-diff-open-file-drops-stale-region-test ()
  "Test that jumping into a file drops a region left active in it.

A range reference elsewhere (`file:2-4', say) leaves its lines
selected, and this jump moves point out from under that selection, so
what it would otherwise span afterwards is neither the range nor the
change."
  (let ((file (make-temp-file "agent-shell-jump" nil ".txt"
                              "alpha\nbeta\ngamma\ndelta\n")))
    (unwind-protect
        (let ((buf (agent-shell-diff
                    :diffs (list (list (cons :old "gamma")
                                       (cons :new "gamma CHANGED")
                                       (cons :file file))))))
          (unwind-protect
              (progn
                (with-current-buffer (find-file-noselect file)
                  (goto-char (point-min))
                  (push-mark (line-end-position 2) t t)
                  ;; `mark-active' rather than `region-active-p': the latter
                  ;; is nil whenever `transient-mark-mode' is off (as under
                  ;; batch), passing whether or not the mark was cleared.
                  (should mark-active))
                (with-current-buffer buf
                  (goto-char (point-min))
                  (search-forward "-gamma")
                  (beginning-of-line)
                  (agent-shell-diff-open-file))
                (with-current-buffer (find-buffer-visiting file)
                  (should (equal (line-number-at-pos) 3))
                  (should-not mark-active)))
            (when (buffer-live-p buf) (kill-buffer buf))
            (when (find-buffer-visiting file) (kill-buffer (find-buffer-visiting file)))))
      (delete-file file))))

(ert-deftest agent-shell-diff-open-file-disambiguates-with-hint-test ()
  "Test that the ACP `locations' line picks between duplicate matches.

The changed content appears twice in the file, so a plain search would
land on the first.  The hint line steers it to the intended occurrence."
  (let* ((file (make-temp-file "agent-shell-jump" nil ".txt"
                               (concat "context\ntarget\nfiller\n"
                                       "context\ntarget\nfiller\n"))))
    (unwind-protect
        ;; :line 4 points at the second occurrence.
        (let ((buf (agent-shell-diff
                    :diffs (list (list (cons :old "context\ntarget")
                                       (cons :new "context\ntarget CHANGED")
                                       (cons :file file)
                                       (cons :line 4))))))
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (search-forward "-target")
                (beginning-of-line)
                (agent-shell-diff-open-file)
                ;; Second "target" is at line 5, not the first at line 2.
                (should (equal (line-number-at-pos) 5)))
            (when (buffer-live-p buf) (kill-buffer buf))
            (when (find-buffer-visiting file) (kill-buffer (find-buffer-visiting file)))))
      (delete-file file))))

(ert-deftest agent-shell-diff-open-file-navigates-later-hunk-test ()
  "Test that open-file resolves a change in a later hunk.

With more than one hunk, blank spacer lines surround each `changes'
label (the one below it is tagged `agent-shell-diff-spacer').  The jump
must skip that spacer and still land on the changed line of the hunk at
point."
  (let* ((old (string-join
               '("alpha" "one" "two" "three" "four" "five" "six" "seven"
                 "OLD_A" "eight" "nine" "ten" "eleven" "twelve" "thirteen"
                 "fourteen" "OLD_B" "omega")
               "\n"))
         (new (string-join
               '("alpha" "one" "two" "three" "four" "five" "six" "seven"
                 "NEW_A" "eight" "nine" "ten" "eleven" "twelve" "thirteen"
                 "fourteen" "NEW_B" "omega")
               "\n"))
         (file (make-temp-file "agent-shell-jump" nil ".txt" (concat old "\n"))))
    (unwind-protect
        (let ((buf (agent-shell-diff
                    :diffs (list (list (cons :old old)
                                       (cons :new new)
                                       (cons :file file))))))
          (unwind-protect
              (with-current-buffer buf
                ;; Two changes far enough apart to form separate hunks.
                (goto-char (point-min))
                (search-forward "-OLD_B")
                (beginning-of-line)
                (agent-shell-diff-open-file)
                (should (equal (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position))
                               "OLD_B")))
            (when (buffer-live-p buf) (kill-buffer buf))
            (when (find-buffer-visiting file) (kill-buffer (find-buffer-visiting file)))))
      (delete-file file))))

(ert-deftest agent-shell-diff-on-exit-skipped-when-calling-buffer-dead-test ()
  "Test that ON-EXIT is skipped without error when calling buffer is dead."
  (let ((on-exit-called nil)
        (calling-buf (generate-new-buffer " *test-calling*")))
    (let ((buf (with-current-buffer calling-buf
                 (agent-shell-diff
                  :diffs (list (list (cons :old "hello\n")
                                     (cons :new "world\n")
                                     (cons :file "test.el")))
                  :on-exit (lambda () (setq on-exit-called t))))))
      (unwind-protect
          (progn
            (kill-buffer calling-buf)
            (should (buffer-live-p buf))
            (kill-buffer buf)
            (should-not on-exit-called))
        (when (buffer-live-p buf)
          (kill-buffer buf))
        (when (buffer-live-p calling-buf)
          (kill-buffer calling-buf))))))

;;; Integration tests — diff buffer lifecycle in agent-shell state

(require 'agent-shell)

(ert-deftest agent-shell-diff-tracked-in-tool-call-state-test ()
  "Test that invoking the diff viewer stores the buffer in tool-call state."
  (let* ((shell-buf (generate-new-buffer " *test-shell*"))
         (tool-data (list (cons :status "pending")))
         (state (list (cons :buffer shell-buf)
                      (cons :tool-calls
                            (list (cons "tc-1" tool-data)))))
         (diff (list (cons :old "hello\n")
                     (cons :new "world\n")
                     (cons :file "test.el")))
         (view-fn (with-current-buffer shell-buf
                    (setq major-mode 'agent-shell-mode)
                    (agent-shell--make-diff-viewing-function
                     :diffs (list diff)
                     :actions nil
                     :client nil
                     :request-id "req-1"
                     :state state
                     :tool-call-id "tc-1"))))
    (unwind-protect
        (let ((diff-buf (progn (funcall view-fn)
                               (map-nested-elt state '(:tool-calls "tc-1" :diff-buffer)))))
          (should (bufferp diff-buf))
          (should (buffer-live-p diff-buf)))
      (when-let* ((diff-buf (map-nested-elt state '(:tool-calls "tc-1" :diff-buffer))))
        (when (buffer-live-p diff-buf)
          (agent-shell-diff-kill-buffer diff-buf)))
      (when (buffer-live-p shell-buf)
        (kill-buffer shell-buf)))))

(ert-deftest agent-shell-diff-reuses-existing-buffer-test ()
  "Test that invoking the diff viewer twice reuses the same buffer."
  (let* ((shell-buf (generate-new-buffer " *test-shell*"))
         (state (list (cons :buffer shell-buf)
                      (cons :tool-calls
                            (list (cons "tc-1" (list (cons :status "pending")))))))
         (diff (list (cons :old "hello\n")
                     (cons :new "world\n")
                     (cons :file "test.el")))
         (view-fn (with-current-buffer shell-buf
                    (setq major-mode 'agent-shell-mode)
                    (agent-shell--make-diff-viewing-function
                     :diffs (list diff)
                     :actions nil
                     :client nil
                     :request-id "req-1"
                     :state state
                     :tool-call-id "tc-1"))))
    (unwind-protect
        (progn
          (funcall view-fn)
          (let ((first-buf (map-nested-elt state '(:tool-calls "tc-1" :diff-buffer))))
            (should (buffer-live-p first-buf))
            (funcall view-fn)
            (should (eq first-buf (map-nested-elt state '(:tool-calls "tc-1" :diff-buffer))))))
      (when-let* ((diff-buf (map-nested-elt state '(:tool-calls "tc-1" :diff-buffer))))
        (when (buffer-live-p diff-buf)
          (agent-shell-diff-kill-buffer diff-buf)))
      (when (buffer-live-p shell-buf)
        (kill-buffer shell-buf)))))

(ert-deftest agent-shell-diff-killed-on-permission-response-test ()
  "Test that `agent-shell--send-permission-response' kills a tracked diff buffer."
  (let* ((diff-buf (agent-shell-diff
                    :diffs (list (list (cons :old "hello\n")
                                       (cons :new "world\n")
                                       (cons :file "test.el")))))
         (shell-buf (generate-new-buffer " *test-shell*"))
         (tool-data (list (cons :status "pending")
                          (cons :permission-request-id "req-1")
                          (cons :diff-buffer diff-buf)))
         (state (list (cons :buffer shell-buf)
                      (cons :tool-calls
                            (list (cons "tc-1" tool-data))))))
    (unwind-protect
        (progn
          (should (buffer-live-p diff-buf))
          (cl-letf (((symbol-function 'acp-send-response) #'ignore)
                    ((symbol-function 'acp-make-session-request-permission-response) #'ignore)
                    ((symbol-function 'agent-shell--delete-fragment) #'ignore)
                    ((symbol-function 'agent-shell--emit-event) #'ignore)
                    ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
                    ((symbol-function 'agent-shell-jump-to-latest-permission-button-row) #'ignore)
                    ((symbol-function 'agent-shell-viewport--buffer) (lambda (&rest _) nil)))
            (with-current-buffer shell-buf
              (agent-shell--send-permission-response
               :client nil
               :request-id "req-1"
               :option-id "opt-1"
               :state state
               :tool-call-id "tc-1")))
          (should-not (buffer-live-p diff-buf)))
      (when (buffer-live-p diff-buf)
        (kill-buffer diff-buf))
      (when (buffer-live-p shell-buf)
        (kill-buffer shell-buf)))))

(ert-deftest agent-shell-diff-killed-on-shell-clean-up-test ()
  "Test that `agent-shell--clean-up' kills tracked diff buffers."
  (let* ((diff-buf (agent-shell-diff
                    :diffs (list (list (cons :old "hello\n")
                                       (cons :new "world\n")
                                       (cons :file "test.el")))))
         (shell-buf (generate-new-buffer " *test-shell*"))
         (tool-data (list (cons :status "pending")
                          (cons :diff-buffer diff-buf)))
         (state (list (cons :buffer shell-buf)
                      (cons :tool-calls
                            (list (cons "tc-1" tool-data)))
                      (cons :idle-timer nil))))
    (unwind-protect
        (progn
          (should (buffer-live-p diff-buf))
          (with-current-buffer shell-buf
            (setq major-mode 'agent-shell-mode)
            (setq-local agent-shell--state state)
            (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t))
          (cl-letf (((symbol-function 'agent-shell--shutdown) #'ignore)
                    ((symbol-function 'agent-shell-viewport--buffer) (lambda (&rest _) nil)))
            (kill-buffer shell-buf))
          (should-not (buffer-live-p diff-buf)))
      (when (buffer-live-p diff-buf)
        (kill-buffer diff-buf))
      (when (buffer-live-p shell-buf)
        (kill-buffer shell-buf)))))

(provide 'agent-shell-diff-tests)
;;; agent-shell-diff-tests.el ends here
