emacs ?= emacs

LOAD = -l ert -l tests/emacs-eclim-linter-init.el

all: test

test:
	$(emacs) -batch $(LOAD) -l tests/run-tests.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -batch $(LOAD) \
	--eval "(progn (add-to-list 'load-path default-directory) (mapc #'byte-compile-file '(\"eclim.el\")))"

clean:
	rm -f *.elc

.PHONY: all test compile clean
