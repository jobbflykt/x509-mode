.PHONY: all test

all: lisp

TOP := .
LOAD_PATH += -L $(TOP)
EMACS ?= emacs
BATCH = $(EMACS) -Q --batch $(LOAD_PATH)
REQUIRES = package-lint compat
PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (push '(\"gnu-elpa\" . \"http://mirrors.rockylinux.org/melpa/gnu-elpa/\") \
        package-archives) \
  (package-initialize) \
  (dolist (pkg '(${REQUIRES})) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
        (package-refresh-contents)) \
      (package-install pkg))) \
  )"

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
