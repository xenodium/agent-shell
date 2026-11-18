((nil . ((eval . (defun agent-shell-run-all-tests (&optional in-process)
                   "Run all `agent-shell' tests in a fresh batch Emacs, as CI does.

Run `make test' with the current Emacs binary and the dependency
directories it resolves in `load-path', showing the results in a
`compilation-mode' buffer.

With prefix argument IN-PROCESS, reload the test files and run them in
the current Emacs instead, against whichever `agent-shell' is already
loaded."
                   (interactive "P")
                   (require 'seq)
                   (let ((root (project-root (project-current t))))
                     (if (or in-process noninteractive)
                         (progn
                           (require 'ert)
                           (mapatoms (lambda (sym)
                                       (when (and (ert-test-boundp sym)
                                                  (string-prefix-p "agent-shell" (symbol-name sym)))
                                         (ert-delete-test sym))))
                           (seq-do #'load
                                   (cons (expand-file-name "tests/agent-shell-fakes.el" root)
                                         (directory-files (expand-file-name "tests/" root)
                                                          t "-tests\\.el\\'")))
                           (if noninteractive
                               (ert-run-tests-batch-and-exit "^agent-shell")
                             (ert "^agent-shell")))
                       (let ((default-directory root))
                         (compile
                          (mapconcat
                           #'shell-quote-argument
                           (list "make" "test"
                                 (concat "EMACS="
                                         (expand-file-name invocation-name invocation-directory))
                                 (concat "DEPS="
                                         (string-join
                                          (seq-uniq
                                           (seq-map (lambda (library)
                                                      (directory-file-name
                                                       (file-name-directory
                                                        (or (locate-library library)
                                                            (user-error "Can't find %s in `load-path'"
                                                                        library)))))
                                                    '("acp" "shell-maker" "markdown-overlays")))
                                          " ")))
                           " "))))))))))
