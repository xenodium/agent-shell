EMACS ?= emacs
DEPS ?= ../acp ../shell-maker ../compat
TEST_FILES = tests/agent-shell-fakes.el $(sort $(wildcard tests/*-tests.el))

.PHONY: test

test:
	$(EMACS) --batch -Q -L . $(addprefix -L ,$(DEPS)) \
	  --eval '(setq ert-load-file-name "test-results")' \
	  $(addprefix --load ,$(TEST_FILES)) \
	  -f ert-run-tests-batch-and-exit
