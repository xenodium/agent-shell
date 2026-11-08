;;; agent-shell-block-motion-tests.el --- Tests for moving by blocks -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-block-motion)

;;; Code:

(defun agent-shell-block-motion-tests--buffer (source)
  "Return a buffer holding a finished turn, ready for block motion.

The turn is a submitted prompt, an expanded tool call, a message whose
SOURCE text is marked as a rendered source block, a collapsed group
whose child is hidden, and the live prompt.  Caller must kill it."
  (let ((buffer (generate-new-buffer " *agent-shell-block-motion*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (agent-shell-ui-mode 1)
        (insert (propertize "Claude> " 'font-lock-face 'comint-highlight-prompt)
                (propertize "fix it" 'font-lock-face 'comint-highlight-input)
                "\n")
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id "1" :block-id "tool"
          :label-left "Tool" :body "tool output")
         :expanded t)
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id "1" :block-id "msg"
          :body "Intro\nlet x;\nOutro")
         :expanded t)
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id "1" :block-id "child"
          :label-left "Child" :body "child body"
          :group-id "grp" :group-label "Group" :group-expanded nil))
        (goto-char (point-min))
        (search-forward source)
        (put-text-property (match-beginning 0) (match-end 0)
                           'agent-shell-markdown-source-block-body t)
        (goto-char (point-max))
        (insert (propertize "Claude> " 'font-lock-face 'comint-highlight-prompt)))
      (setq-local beginning-of-defun-function #'agent-shell--beginning-of-block)
      (setq-local end-of-defun-function #'agent-shell--end-of-block))
    buffer))

(defun agent-shell-block-motion-tests--block-start (qualified-id)
  "Return where fragment QUALIFIED-ID starts in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (prop-match-beginning
     (text-property-search-forward
      'agent-shell-ui-state nil
      (lambda (_ state)
        (equal (map-elt state :qualified-id) qualified-id))
      t))))

(defun agent-shell-block-motion-tests--text-start (text)
  "Return where TEXT first starts in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (match-beginning 0)))

(ert-deftest agent-shell-beginning-of-defun-walks-back-by-block-test ()
  "From the live prompt, each block is visited once, hidden ones never."
  (let ((buffer (agent-shell-block-motion-tests--buffer "let x;\n")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (let (visited)
            (while (beginning-of-defun)
              (push (point) visited))
            (should (equal (nreverse visited)
                           (list (agent-shell-block-motion-tests--block-start "1-grp")
                                 (agent-shell-block-motion-tests--block-start "1-msg")
                                 (agent-shell-block-motion-tests--block-start "1-tool")
                                 (point-min))))))
      (kill-buffer buffer))))

(ert-deftest agent-shell-beginning-of-defun-goes-to-start-of-block-at-point-test ()
  "Inside a block, the first move is to that block's own start."
  (let ((buffer (agent-shell-block-motion-tests--buffer "let x;\n")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (agent-shell-block-motion-tests--text-start "Outro"))
          (end-of-line)
          (should (beginning-of-defun))
          (should (= (point) (agent-shell-block-motion-tests--block-start "1-msg")))
          (goto-char (agent-shell-block-motion-tests--text-start "output"))
          (should (beginning-of-defun))
          (should (= (point) (agent-shell-block-motion-tests--block-start "1-tool"))))
      (kill-buffer buffer))))

(ert-deftest agent-shell-beginning-of-defun-leaves-source-block-for-message-test ()
  "In a source block, the move is to its start, then to the message's."
  (let ((buffer (agent-shell-block-motion-tests--buffer "let x;\n")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (agent-shell-block-motion-tests--text-start "x;"))
          (should (beginning-of-defun))
          (should (= (point) (agent-shell-block-motion-tests--text-start "let x;")))
          (should (beginning-of-defun))
          (should (= (point) (agent-shell-block-motion-tests--block-start "1-msg"))))
      (kill-buffer buffer))))

(ert-deftest agent-shell-beginning-of-defun-from-below-skips-closing-source-block-test ()
  "A source block ending its message is passed over from below."
  (let ((buffer (agent-shell-block-motion-tests--buffer "Outro")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (agent-shell-block-motion-tests--block-start "1-grp"))
          (should (beginning-of-defun))
          (should (= (point) (agent-shell-block-motion-tests--block-start "1-msg"))))
      (kill-buffer buffer))))

(ert-deftest agent-shell-beginning-of-defun-walks-forward-by-block-test ()
  "With a negative argument, blocks are visited downwards.
A source block counts only while inside the message holding it."
  (let ((buffer (agent-shell-block-motion-tests--buffer "let x;\n")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (let (visited)
            (while (beginning-of-defun -1)
              (push (point) visited))
            (should (equal (nreverse visited)
                           (list (agent-shell-block-motion-tests--block-start "1-tool")
                                 (agent-shell-block-motion-tests--block-start "1-msg")
                                 (agent-shell-block-motion-tests--text-start "let x;")
                                 (agent-shell-block-motion-tests--block-start "1-grp"))))))
      (kill-buffer buffer))))

(ert-deftest agent-shell-end-of-defun-moves-past-block-ends-test ()
  "Each move lands past the end of a block and short of the next one."
  (let ((buffer (agent-shell-block-motion-tests--buffer "let x;\n")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((message (agent-shell-block-motion-tests--block-start "1-msg"))
                (group (agent-shell-block-motion-tests--block-start "1-grp")))
            (goto-char (agent-shell-block-motion-tests--text-start "output"))
            (end-of-defun)
            (should (< (agent-shell-block-motion-tests--text-start "output") (point) message))
            (should-not (get-text-property (point) 'agent-shell-ui-state))
            (end-of-defun)
            (should (< (agent-shell-block-motion-tests--text-start "Outro") (point) group))
            (should-not (get-text-property (point) 'agent-shell-ui-state))))
      (kill-buffer buffer))))

(provide 'agent-shell-block-motion-tests)
;;; agent-shell-block-motion-tests.el ends here
