.PHONY: all test lint package-lint checkdoc relint

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

# Add package-lint and relint for linting
LINT_REQUIRES = package-lint relint

LINT_PACKAGES = "(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(${REQUIRES} ${LINT_REQUIRES})) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
        (package-refresh-contents)) \
      (package-install pkg))))"

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

lint: package-lint checkdoc relint

package-lint:
	$(BATCH) --eval $(LINT_PACKAGES) -f package-lint-batch-and-exit \
	  x509-mode.el

checkdoc:
	$(BATCH) --eval $(PACKAGES) --eval "\
	(progn \
	  (require 'checkdoc) \
	  (find-file \"x509-mode.el\") \
	  (if (checkdoc-current-buffer t) \
	      (progn \
	        (with-current-buffer checkdoc-diagnostic-buffer \
	          (princ (buffer-string))) \
	        (kill-emacs 1)) \
	    (message \"checkdoc passed\") \
	    (kill-emacs 0)))"

relint:
	$(BATCH) --eval $(LINT_PACKAGES) --eval "\
	(progn \
	  (require 'relint) \
	  (relint-batch \"x509-mode.el\"))"
