.PHONY: all test

all: lisp

TOP := .
LOAD_PATH += -L $(TOP)
EMACS ?= emacs
BATCH = $(EMACS) -Q --batch $(LOAD_PATH)
REQUIRES = compat
PACKAGES="(progn \
  (require 'package) \
  (package-initialize) \
  (dolist (pkg '(${REQUIRES})) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
        (package-refresh-contents)) \
      (package-install pkg))))"

# Future: Add package-lint to REQUIRES and melpa to package-archives.
#         Create a target with runs -f package-lint-batch-and-exit x509-mode.el
# REQUIRES += package-lint
# (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \

ELS = x509-mode.el
ELCS = $(ELS:.el=.elc)

lisp: $(ELCS)

%.elc: %.el
	$(BATCH) --eval $(PACKAGES) --eval \
          "(progn \
             (when (file-exists-p \"$@\") (delete-file \"$@\")) \
             (setq byte-compile-error-on-warn t))" \
	-f batch-byte-compile $<

test:
	openssl version
	$(BATCH) --eval $(PACKAGES) --eval "\
	(progn \
	  (message \"%s\" (emacs-version)) \
	  (load-file \"$(TOP)/x509-mode.el\") \
	  (load-file \"$(TOP)/x509-mode-tests.el\") \
	  (ert-run-tests-batch-and-exit))"
