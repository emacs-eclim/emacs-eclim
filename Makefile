# makefie for emacs-eclim

EL_FILES := $(sort $(wildcard *.el))
ELC_FILES := $(EL_FILES:.el=.elc)

EMACS := emacs
EMACS_OPTS := -batch $(LOAD_PATH)
LOAD_PATH := -L .
TEST_LOAD_FILES = -l tests/emacs-eclim-linter-init.el

all: test

test:
	$(EMACS) $(EMACS_OPTS) -l ert $(TEST_LOAD_FILES) -f ert-run-tests-batch-and-exit

lint:
	$(EMACS) $(EMACS_OPTS) $(TEST_LOAD_FILES) -f elisp-lint-files-batch *.el

compile: $(ELC_FILES)

%.elc: %.el
	@$(EMACS) $(EMACS_OPTS) --eval "(progn (package-initialize) (package-refresh-contents) (add-to-list 'load-path default-directory))"  -f batch-byte-compile $^

clean:
	rm -f *.elc

.PHONY: all test compile clean
