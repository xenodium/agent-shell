;;; agent-shell-completion-tests.el --- Tests for agent-shell completion -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comint)
(require 'ert)
(require 'map)
(require 'agent-shell)
(require 'agent-shell-completion)

;;; Code:

(ert-deftest agent-shell--completion-bounds-ignores-path-separators-test ()
  "Test `/` in file paths does not trigger command completion."
  (let ((command-chars "[:alnum:]_-")
        (path-chars "[:alnum:]/_.-"))
    (with-temp-buffer
      (insert "@path/abc")
      (goto-char (point-max))
      (should-not (agent-shell--completion-bounds command-chars ?/))
      (let ((bounds (agent-shell--completion-bounds path-chars ?@)))
        (should bounds)
        (should (equal (map-elt bounds :start) 2))
        (should (equal (map-elt bounds :end) 10)))))

  (with-temp-buffer
    (insert " /help")
    (goto-char (point-max))
    (let ((bounds (agent-shell--completion-bounds "[:alnum:]_-" ?/)))
      (should bounds)
      (should (equal (map-elt bounds :start) 3))
      (should (equal (map-elt bounds :end) 7)))))

(ert-deftest agent-shell--capf-exit-with-file-mention-test ()
  "Completing a path holding whitespace leaves a mention the parser reads whole.
The buffer already holds @ and the inserted candidate by the time the
exit function runs, so these start from that state."
  (with-temp-buffer
    (insert "@My Design.png")
    (goto-char (point-max))
    (agent-shell--capf-exit-with-file-mention "My Design.png" 'finished)
    (should (equal (buffer-string) "@\"My Design.png\" "))
    (should (equal (map-elt (seq-first (agent-shell--parse-file-mentions (buffer-string))) :path)
                   "My Design.png")))

  ;; No whitespace, no quotes.
  (with-temp-buffer
    (insert "@src/main.el")
    (goto-char (point-max))
    (agent-shell--capf-exit-with-file-mention "src/main.el" 'finished)
    (should (equal (buffer-string) "@src/main.el ")))

  ;; Text already in the prompt is left alone.
  (with-temp-buffer
    (insert "look at @My Design.png")
    (goto-char (point-max))
    (agent-shell--capf-exit-with-file-mention "My Design.png" 'finished)
    (should (equal (buffer-string) "look at @\"My Design.png\" "))))

(defun agent-shell-completion-tests--make-shell ()
  "Return a buffer offering /help and /compact as available commands."
  (let ((shell (generate-new-buffer " *agent-shell-completion-test*")))
    (with-current-buffer shell
      (setq-local agent-shell--state
                  '((:available-commands . (((name . "help")
                                             (description . "Show help"))
                                            ((name . "compact")
                                             (description . "Compact history")))))))
    shell))

(ert-deftest agent-shell-completion-command-at-input-start-test ()
  "Commands complete when / is the first character of the input."
  (let ((shell (agent-shell-completion-tests--make-shell)))
    (unwind-protect
        (with-temp-buffer
          (setq-local agent-shell-completion--shell-buffer shell)
          (insert "/he")
          (should (equal (nth 2 (agent-shell--command-completion-at-point))
                         '("help" "compact"))))
      (kill-buffer shell))))

(ert-deftest agent-shell-completion-company-prefix-length-test ()
  "A bare @ or / is enough for Company to pop, despite its minimum prefix."
  (let ((shell (agent-shell-completion-tests--make-shell)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--project-files)
                   (lambda () '("src/main.el"))))
          (with-temp-buffer
            (setq-local agent-shell-completion--shell-buffer shell)
            (insert "/")
            (should (eq (plist-get (nthcdr 3 (agent-shell--command-completion-at-point))
                                   :company-prefix-length)
                        t)))
          (with-temp-buffer
            (insert "@")
            (should (eq (plist-get (nthcdr 3 (agent-shell--file-completion-at-point))
                                   :company-prefix-length)
                        t))))
      (kill-buffer shell))))

(ert-deftest agent-shell-completion-command-mid-input-test ()
  "Agents only recognize a command as a message's very first character.
Anything ahead of the /, including whitespace and earlier lines of a
multi-line prompt, makes it plain text."
  (let ((shell (agent-shell-completion-tests--make-shell)))
    (unwind-protect
        (dolist (input '("  /he" "summarize /he" "summarize\n/he"))
          (with-temp-buffer
            (setq-local agent-shell-completion--shell-buffer shell)
            (insert input)
            (should-not (agent-shell--command-completion-at-point))))
      (kill-buffer shell))))

(ert-deftest agent-shell-completion-command-after-shell-prompt-test ()
  "Text above the prompt is not input, so / after the prompt still completes."
  (let ((shell (agent-shell-completion-tests--make-shell)))
    (unwind-protect
        (with-temp-buffer
          (comint-mode)
          (setq-local agent-shell-completion--shell-buffer shell)
          (insert "What time is it?\n\nIt is 5 o'clock.\n\nFake> ")
          (setq-local comint-last-prompt (cons (copy-marker (- (point) 6))
                                               (copy-marker (point))))
          (insert "/he")
          (should (equal (nth 2 (agent-shell--command-completion-at-point))
                         '("help" "compact")))
          (insert " now what /he")
          (should-not (agent-shell--command-completion-at-point)))
      (kill-buffer shell))))

(ert-deftest agent-shell-completion-command-below-stale-prompt-test ()
  "A prompt with agent output below it is stale, not an input area.
`comint-last-prompt' still points at it while output streams, so its end
is where output begins rather than where typing begins."
  (let ((shell (agent-shell-completion-tests--make-shell)))
    (unwind-protect
        (with-temp-buffer
          (comint-mode)
          (setq-local agent-shell-completion--shell-buffer shell)
          (insert "Fake> ")
          (setq-local comint-last-prompt (cons (copy-marker (- (point) 6))
                                               (copy-marker (point))))
          (save-excursion
            (insert (propertize "Thinking..." 'field 'output)))
          (insert "/he")
          (should-not (agent-shell--command-completion-at-point)))
      (kill-buffer shell))))

(ert-deftest agent-shell-completion-setup-queued-prompt-test ()
  "The queued-prompt hook enables completion for the event's shell.
Reached through `agent-shell-prompt-queue-setup-minibuffer-functions', so
the queue does not have to know completion exists."
  (let ((shell (generate-new-buffer " *agent-shell-completion-test*")))
    (unwind-protect
        (progn
          (with-current-buffer shell (agent-shell-completion-mode 1))
          (with-temp-buffer
            (agent-shell-completion--setup-queued-prompt
             `((:shell-buffer . ,shell)))
            (should (eq shell agent-shell-completion--shell-buffer))
            (should (memq #'agent-shell--file-completion-at-point
                          completion-at-point-functions))
            (should (memq #'agent-shell--trigger-completion-at-point
                          post-self-insert-hook))))
      (kill-buffer shell))))

(ert-deftest agent-shell-completion-setup-queued-prompt-without-mode-test ()
  "A shell without the mode still completes, but typing @ or / pops nothing."
  (let ((shell (generate-new-buffer " *agent-shell-completion-test*")))
    (unwind-protect
        (with-temp-buffer
          (agent-shell-completion--setup-queued-prompt
           `((:shell-buffer . ,shell)))
          (should (memq #'agent-shell--file-completion-at-point
                        completion-at-point-functions))
          (should-not (memq #'agent-shell--trigger-completion-at-point
                            post-self-insert-hook)))
      (kill-buffer shell))))

(ert-deftest agent-shell-completion-mode-only-pops-test ()
  "The mode toggles popping, while the CAPFs stay put."
  (with-temp-buffer
    (agent-shell-completion--setup)
    (agent-shell-completion-mode 1)
    (should (memq #'agent-shell--trigger-completion-at-point
                  post-self-insert-hook))
    (agent-shell-completion-mode -1)
    (should-not (memq #'agent-shell--trigger-completion-at-point
                      post-self-insert-hook))
    (should (memq #'agent-shell--file-completion-at-point
                  completion-at-point-functions))
    (should (memq #'agent-shell--command-completion-at-point
                  completion-at-point-functions))))

(ert-deftest agent-shell-completion-trigger-per-action-test ()
  "@ and / each pop only when their own option allows it.
Large projects can make @ slow while / stays cheap, so one is often
wanted without the other."
  (let ((shell (agent-shell-completion-tests--make-shell)))
    (unwind-protect
        (dolist (case '(((:file . nil) (:command . t)   (:popped . (?/)))
                        ((:file . t)   (:command . nil) (:popped . (?@)))))
          (let ((agent-shell-file-completion-enabled (map-elt case :file))
                (agent-shell-command-completion-enabled (map-elt case :command))
                (popped nil))
            (cl-letf (((symbol-function 'completion-at-point)
                       (lambda () (push (char-before) popped))))
              (dolist (input '("/" "@"))
                (with-temp-buffer
                  (setq-local agent-shell-completion--shell-buffer shell)
                  (insert input)
                  (agent-shell--trigger-completion-at-point))))
            (should (equal popped (map-elt case :popped)))))
      (kill-buffer shell))))

(ert-deftest agent-shell-completion-setup-mode-test ()
  "Setup enables the mode unless both popping options are off."
  (dolist (case '(((:file . t)   (:command . t)   (:mode . t))
                  ((:file . nil) (:command . t)   (:mode . t))
                  ((:file . t)   (:command . nil) (:mode . t))
                  ((:file . nil) (:command . nil) (:mode . nil))))
    (with-temp-buffer
      (let ((agent-shell-file-completion-enabled (map-elt case :file))
            (agent-shell-command-completion-enabled (map-elt case :command)))
        (agent-shell-completion--setup)
        (should (eq (and agent-shell-completion-mode t) (map-elt case :mode)))
        (should (memq #'agent-shell--file-completion-at-point
                      completion-at-point-functions))))))

(ert-deftest agent-shell--capf-exit-unfinished-test ()
  "Statuses other than `finished' leave the buffer alone.
UIs like Corfu and Company report `sole' or `exact' while the user may
still be typing."
  (dolist (status '(sole exact))
    (with-temp-buffer
      (insert "@My Design.png")
      (agent-shell--capf-exit-with-file-mention "My Design.png" status)
      (should (equal (buffer-string) "@My Design.png")))
    (with-temp-buffer
      (insert "/help")
      (agent-shell--capf-exit-with-space "help" status)
      (should (equal (buffer-string) "/help")))))

(provide 'agent-shell-completion-tests)
;;; agent-shell-completion-tests.el ends here
