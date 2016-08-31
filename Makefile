# makefie for emacs-eclim

EL_FILES := $(sort $(wildcard *.el))
ELC_FILES := $(EL_FILES:.el=.elc)

EMACS := emacs
LOAD_PATH := -L .
EMACS_OPTS := -batch $(LOAD_PATH)
TEST_LOAD_FILES = -l tests/emacs-eclim-linter-init.el

VPATH := .

all: test

test:
	$(EMACS) $(EMACS_OPTS) -l ert $(TEST_LOAD_FILES) -f ert-run-tests-batch-and-exit

lint:
	$(EMACS) $(EMACS_OPTS) $(TEST_LOAD_FILES) -f elisp-lint-files-batch *.el

compile: $(ELC_FILES)

%.elc: %.el
	@$(EMACS) $(EMACS_OPTS) --eval "(progn (package-initialize) (package-refresh-contents) (add-to-list 'load-path default-directory) (byte-recompile-file \"$^\" t 0))"

clean:
	rm -f *.elc tests/*.elc

.PHONY: all test compile clean
