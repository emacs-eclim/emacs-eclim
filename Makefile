# makefie for emacs-eclim

EL_FILES := $(sort $(wildcard *.el))
ELC_FILES := $(EL_FILES:.el=.elc)

EMACS := emacs
CASK := cask
LOAD_PATH := -L .
EMACS_OPTS :=
EMACS_BATCH := $(EMACS) -Q -batch -L . $(EMACS_OPTS)
TEST_LOAD_FILES = -l test/test-helper.el

# Program availability
ifdef CASK
RUN_EMACS = $(CASK) exec $(EMACS_BATCH)
HAVE_CASK := $(shell sh -c "command -v $(CASK)")
ifndef HAVE_CASK
$(warning "$(CASK) is not available.  Please run make help")
endif
endif

VPATH := .

all: test

init:
	$(CASK) install
	$(CASK) update

test:
	$(RUN_EMACS) $(TEST_LOAD_FILES) -f eclim-run-tests

specs:
	$(CASK) exec buttercup -L . -L ./test/specs

lint: $(EL_FILES)
	$(RUN_EMACS) $(TEST_LOAD_FILES) -f eclim-lint-files $(EL_FILES)

compile: $(EL_FILES)
	$(RUN_EMACS) -l maint/eclim-compile.el -f eclim/batch-byte-compile $(EL_FILES)

clean:
	rm -f *.elc test/*.elc

help:
	@echo 'Run `make init` first to install and update all local dependencies.'
	@echo ''
	@echo 'Available targets:'
	@echo '  init:       Initialise the project.  RUN FIRST!'
	@echo '  lint:       Check all Emacs Lisp sources'
	@echo '  compile:    Byte-compile Emacs Lisp sources'
	@echo '  test:       Run all ERT unit tests'
	@echo '  specs:      Run all buttercup tests'
	@echo '  clean:      Clean compiled files'
	@echo ''
	@echo 'Available make variables:'
	@echo '  EMCS_OPTS:  Additional options to pass to `emacs`'
	@echo '  EMACS:      The path or name of the Emacs to use for tests and compilation'
	@echo ''
	@echo 'Available programs:'
	@echo '  $(CASK): $(if $(HAVE_CASK),yes,no)'
	@echo ''
	@echo 'You need $(CASK) to develop this package.'
	@echo 'See http://cask.readthedocs.io/ for more information.'

.PHONY: all init test specs lint compile clean help
