;;; agent-shell-completion-tests.el --- Tests for agent-shell completion -*- lexical-binding: t; -*-

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
                          completion-at-point-functions))))
      (kill-buffer shell))))

(ert-deftest agent-shell-completion-setup-queued-prompt-without-mode-test ()
  "A shell without completion enabled leaves the minibuffer alone."
  (let ((shell (generate-new-buffer " *agent-shell-completion-test*")))
    (unwind-protect
        (with-temp-buffer
          (agent-shell-completion--setup-queued-prompt
           `((:shell-buffer . ,shell)))
          (should-not (memq #'agent-shell--file-completion-at-point
                            completion-at-point-functions)))
      (kill-buffer shell))))

(provide 'agent-shell-completion-tests)
;;; agent-shell-completion-tests.el ends here
