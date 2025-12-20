DEPS = shell-maker acp package-lint

INIT_PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(${DEPS})) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
	(package-refresh-contents)) \
      (package-install pkg))) \
  (unless package-archive-contents (package-refresh-contents)) \
  )"

.PHONY: compile
compile:
	emacs -Q --eval ${INIT_PACKAGES} -batch -L . -L tests \
          --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile agent-shell*.el; \
	  (ret=$$? ; rm *.elc && exit $$ret)

.PHONY: package-lint
package-lint:
	emacs -Q --eval ${INIT_PACKAGES} -batch -L . -f package-lint-batch-and-exit agent-shell.el
