.PHONY: all test

all: lisp

TOP := .
LOAD_PATH = -L $(TOP)
EMACS ?= emacs
BATCH = $(EMACS) -Q --batch $(LOAD_PATH)

ELS = x509-mode.el
ELCS = $(ELS:.el=.elc)

lisp: $(ELCS)

%.elc: %.el
	$(BATCH) --eval "(when (file-exists-p \"$@\") (delete-file \"$@\"))" \
	-f batch-byte-compile $<

test:
	openssl version
	$(BATCH) --eval "\
	(progn \
	  (message \"%s\" (emacs-version)) \
	  (load-file \"$(TOP)/x509-mode-tests.el\") \
	  (ert-run-tests-batch-and-exit))"
