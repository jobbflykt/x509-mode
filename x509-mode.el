;;; x509-mode.el --- View certificates, CRLs and keys using OpenSSL  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2017 Fredrik Axelsson <f.axelsson@gmail.com>

;; Author: Fredrik Axelsson <f.axelsson@gmail.com>
;; Homepage: https://github.com/jobbflykt/x509-mode
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; MIT License
;;
;; Copyright (c) 2017 Fredrik Axelsson <f.axelsson@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.


;;; Commentary:

;; Major mode for viewing certificates, CRLs, and other PKI-related files.
;;
;; Uses OpenSSL for viewing PEM and DER encoded PKI entities.
;;
;; Prerequisites: OpenSSL. Customize the variable `x509-openssl-cmd' to name
;; the openssl binary. Defaults are "openssl" on Linux (assuming it's on PATH)
;; and "C:/Program Files/Git/mingw64/bin/openssl.exe" on Windows (assuming Git
;; for Windows is installed in its default location).
;;
;; Usage: Open a file containing a certificate, either PEM or DER encode.  Now
;; use M-x `x509-viewcert' to create a new buffer that displays the decoded
;; certificate.  Use `x509-viewcrl', `x509-viewasn1',`x509-viewkey',
;; `x509-viewpublickey', `x509-viewdh', `x509-viewreq', `x509-viewpkcs7' in a
;; similar manner.
;;
;; M-x `x509-dwim' tries to guess what view-function to call.  It falls back to
;; `x509-viewasn1' if it fails.
;;
;; If point is at the beginning of, or in, a PEM region, all view functions,
;; including `x509-dwim', tries extra hard to use that region as input. This
;; often works even when there is other data ahead and after region and if the
;; region is indented or the lines are quoted.
;;
;; Use C-u prefix with any command for editing the command arguments.
;;
;; When in a x509 buffer, use keys `e' and `t' to edit current command or
;; toggle between x509-asn1-mode and x509-mode respectively.

;;; Code:

(require 'cl-lib)

(defgroup x509 nil
  "View certificates, CRLs, keys and other related files using OpenSSL."
  :group 'extensions
  :group 'convenience
  :link '(emacs-library-link :tag "Lisp File" "x509-mode.el"))

(defcustom x509-openssl-cmd (if (eq system-type 'windows-nt)
                                "C:/Program Files/Git/mingw64/bin/openssl.exe"
                              "openssl")
  "Path to OpenSSL binary.

Example:
\"/usr/bin/openssl\" or just \"openssl\" on Linux
\"C:/Program Files/Git/mingw64/bin/openssl\" on Windows."
  :type 'string
  :group 'x509)

(defcustom x509-x509-default-arg
  "x509 -text -noout -nameopt utf8 -nameopt multiline"
  "Default arguments for \"openssl x509\" command."
  :type 'string
  :group 'x509)

(defcustom x509-req-default-arg
  "req -text -noout -nameopt utf8 -nameopt multiline"
  "Default arguments for \"openssl req\" command."
  :type 'string
  :group 'x509)

(defcustom x509-crl-default-arg
  "crl -text -noout -nameopt utf8 -nameopt multiline"
  "Default arguments for \"openssl crl\" command."
  :type 'string
  :group 'x509)

(defcustom x509-pkcs7-default-arg
  "pkcs7 -noout -text -print_certs"
  "Default arguments for \"openssl pkcs7\" command."
  :type 'string
  :group 'x509)

(defcustom x509-dhparam-default-arg
  "dhparam -text -noout"
  "Default arguments for \"openssl dhparam\" command."
  :type 'string
  :group 'x509)

(defcustom x509-pkey-default-arg
  "pkey -text -noout"
  "Default arguments for \"openssl pkey\" command."
  :type 'string
  :group 'x509)

(defcustom x509-pkey-pubin-default-arg
  "pkey -text -noout -pubin"
  "Default arguments for \"openssl pkey -pubin\" command."
  :type 'string
  :group 'x509)

(defcustom x509-asn1parse-default-arg
  "asn1parse"
  "Default arguments for \"openssl asn1parse\" command."
  :type 'string
  :group 'x509)

(defgroup x509-faces nil
  "Faces used by x509."
  :group 'x509
  :group 'faces)

(defface x509-keyword-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for keywords."
  :group 'x509-faces)

(defface x509-constant-face
  '((t (:inherit font-lock-constant-face)))
  "Face for constants."
  :group 'x509-faces)

(defface x509-short-name-face
  '((t (:bold t)))
  "Face for short names, e.g, CN and OU."
  :group 'x509-faces)

(defface x509-string-face
  '((t (:inherit font-lock-string-face)))
  "Face for strings."
  :group 'x509-faces)

(defface x509-hex-string-face
  '((t (:inherit font-lock-comment-face)))
  "Face for colon-separated hex values."
  :group 'x509-faces)

(defface x509-oid-link-face
  '((t (:inherit (link font-lock-constant-face))))
  "Face for OID buttons."
  :group 'x509-faces)

(defface x509-oid-face
  '((t (:inherit font-lock-constant-face)))
  "Face for symbolic, known, OIDs."
  :group 'x509-faces)

(defface x509-asn1-sequence-face
  '((t (:inherit font-lock-regexp-grouping-backslash)))
  "Face for ASN.1 sequences."
  :group 'x509-faces)

(defface x509-warning-face
  '((t (:inherit font-lock-warning-face :inverse-video t)))
  "Face for bad values."
  :group 'x509-faces)

(defface x509-near-warning-face
  '((t (:inherit font-lock-function-name-face :inverse-video nil)))
  "Face for near expire date/time values."
  :group 'x509-faces)

(defface x509-browse-url-face
  '((t (:inherit link)))
  "Face for clickable URL links.")

(defun x509--match-date (cmp bound)
  "Return non-nil if it can find a date that CMP to current time.
Intended to search for dates in form \"Jun 11 00:00:01 2014 GMT\"
and compare them to the current time. Return non-nil, move point,
and set ‘match-data’ appropriately if it succeeds; like
‘re-search-forward’ would.  The argument BOUND is a buffer
position that bounds the search."
  (let ((mdata (match-data))
        (p (point)))
    (if (re-search-forward
         "[A-Z][a-z][a-z] +[0-9]+ ..:..:.. [0-9]\\{4\\} GMT" bound t)
        (if (condition-case nil
                ;; Compare date to current time.
                ;; date-to-time might fail on bogus time values or if time
                ;; is to far in the past/future
                (funcall cmp (date-to-time (match-string-no-properties 0))
                         (current-time))
              (error nil))
            ;; If compare true, return t.
            t
          ;; else restore match-data and point and return nil.
          (goto-char p)
          (set-match-data mdata)
          nil)
      ;; Not matching date at all -> return nil
      nil)))

(defun x509--match-date-in-past (bound)
  "Return non-nil if it can find a date that is the past.
Intended to search for dates in form \"Jun 11 00:00:01 2014 GMT\"
and compare them to the current time. Return non-nil, move point,
and set ‘match-data’ appropriately if it succeeds; like
‘re-search-forward’ would.  The optional argument BOUND is a
buffer position that bounds the search."
  (x509--match-date (lambda (d1 d2) (time-less-p d1 d2)) bound))

(defun x509--match-date-in-future (bound)
  "Return non-nil if it can find a date that is the future.
Intended to search for dates in form \"Jun 11 00:00:01 2014 GMT\"
and compare them to the current time. Return non-nil, move point,
and set ‘match-data’ appropriately if it succeeds; like
‘re-search-forward’ would.  The optional argument BOUND is a
buffer position that bounds the search."
  (x509--match-date (lambda (d1 d2) (not (time-less-p d1 d2))) bound))

(defcustom x509-warn-near-expire-days 30
  "Warn certificate expiration if time is near.

Set to `nil' to inhibit warning."
  :type 'integer
  :group 'x509)

(defcustom x509-query-oid-url-format "https://oid-rep.orange-labs.fr/get/%s"
  "Format string for constructing URL for querying OIDs

Used with `(format x509-query-oid-url-format oid)'"
  :type 'string
  :group 'x509)

(defun x509--match-date-near-now (bound)
  "Return non-nil it can find a date that is \"near\" in the future.

\"Near\" is defined by `x509-warn-near-expire-days'.
Intended to search for dates in form \"Jun 11 00:00:01 2014 GMT\"
and compare them to the current time. Return non-nil, move point,
and set ‘match-data’ appropriately if it succeeds; like
‘re-search-forward’ would.  The optional argument BOUND is a
buffer position that bounds the search."
  (x509--match-date
   (lambda (time now)
     ;; If we should highlight near expire times
     ;; and time is not in the future
     ;; and time is within decoded-time-delta from now
     (and x509-warn-near-expire-days
          (time-less-p now time)
          (time-less-p time
                       (time-add now
                                 (* x509-warn-near-expire-days 24 60 60)))))
   bound))

(defun x509--mark-browse-url-links(regex face compose-url-fn)
  "Make URLs clickable by making them buttons.

REGEX is used to find and delimit button.
FACE is the face to apply to the button.
COMPOSE-URL-FN is a function that takes a string and returns an URL.
For simple cases, COMPOSE-URL-FN returns its argument unchanged."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (search-forward-regexp regex nil t)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (url (funcall compose-url-fn (match-string-no-properties 0)))
               (help-echo (format "Click to browse-url %s" url)))
          ;; The url is stored in the face property
          (make-button
           start end
           'face face
           'follow-link t
           'url url
           'help-echo help-echo
           'action (lambda (button)
                     (browse-url (button-get button 'url)))))))))

(defun x509--mark-browse-http-links()
  "Make http URLs clickable by making them buttons."
  (x509--mark-browse-url-links "\\(file\\|https?\\)://[-_.:/A-Za-z0-9]+"
                               'x509-browse-url-face
                               (lambda (url) url)))

(defun x509--mark-browse-oid()
  "Make OIDs clickable by making them buttons."
  (x509--mark-browse-url-links "\\(?:[0-9]+\\.\\)\\{3,\\}[0-9]+"
                               'x509-oid-link-face
                               (lambda (oid)
                                 ;; Require OID at least 4 nodes deep to avoid
                                 ;; false positives.
                                 (format x509-query-oid-url-format oid))))

(eval-when-compile
  (defun x509--load-data-file (filename)
    "Split FILENAME linewise into a list.
Skip blank lines and comment lines.  Return list."
    (with-temp-buffer
      (insert-file-contents
       (if (null load-file-name) filename
         (expand-file-name filename (file-name-directory load-file-name))))
      (cl-remove-if (lambda (s) (string-match-p "^ *\\(?:#\\|$\\)" s))
                    (split-string (buffer-string) "\n")))))

(eval-when-compile
  (defconst x509--keywords
    (regexp-opt
     (x509--load-data-file "keywords.txt"))))

(eval-when-compile
  (defconst x509--constants
    (regexp-opt (x509--load-data-file "constants.txt") 'words)))

;; Keyword: constant
;; E.g. "Signature Algorithm: sha1WithRSAEncryption"
(eval-when-compile
  (defconst x509--keyword-w-constant
    (concat (regexp-opt
             (x509--load-data-file "keyword+constant.txt") t)
            ;; Followed by ": constant"
            ": *\\(.*\\)")))

;; Multiline Issuer and Subject, "-nameopt multiline"
;; E.g. "commonName                = GlobalSign Root CA"
(eval-when-compile
  (defconst x509--multiline-name
    (concat (regexp-opt
             (x509--load-data-file "long-name.txt") t)
            " *= \\(.*\\)")))

(defconst x509-font-lock-keywords
  (eval-when-compile
    (list
     ;; Subject and Issuer, long names
     `(,x509--multiline-name
       (1 'x509-keyword-face)
       (2 'x509-string-face))

     `(,x509--keywords . 'x509-keyword-face)

     `(,x509--constants . 'x509-constant-face)

     ;; Validity on a line alone (preceding "Not Before:")
     '("^ +Validity ?$" . 'x509-keyword-face)

     ;; Subject and Issuer, short names
     ;; something=string until ',' or '/' or EOL
     ;; E.g. CN=apa,OU=Räv
     '("\\(\\<\\w+=\\)\\(.*?\\)\\(?:[,/]\\|$\\)"
       (1 'x509-short-name-face)
       (2 'x509-string-face))

     ;; something = string until ',' or EOL
     ;; E.g. CN = ACCVRAIZ1, OU = PKIACCV, O = ACCV, C = ES
     '("\\(\\<\\w+\\) = \\(.*?\\)\\(?:[,/]\\|$\\)"
       (1 'x509-short-name-face)
       (2 'x509-string-face))

     ;; URI: and CPS: . Highlight keyword. URL is handled by
     ;; `x509--mark-browse-url-links'
     '("\\<\\(URI:\\|CPS: \\)"
       (1 'x509-keyword-face))

     ;; DNS:string email:string othername:string
     ;; until ',' or EOL
     '("\\<\\(DNS:\\|email:\\|othername:\\)\\(.*?\\)\\(?:,\\|$\\)"
       (1 'x509-keyword-face)
       (2 'x509-string-face))

     ;; Not Before: Jun 11 00:00:01 2014 GMT
     ;; Date is "MATCH-ANCHORED", see help for variable font-lock-keywords
     '("\\(Not Before\\): " (1 'x509-keyword-face)
       (x509--match-date-in-future nil nil (0 'x509-warning-face)))
     '("\\(Not After\\) : " (1 'x509-keyword-face)
       (x509--match-date-in-past nil nil (0 'x509-warning-face))
       (x509--match-date-near-now nil nil (0 'x509-near-warning-face)))
     ;; For CRL's when Next Update is in the past
     '("\\(Next Update\\): " (1 'x509-keyword-face)
       (x509--match-date-in-past nil nil (0 'x509-warning-face)))

     ;; Policy: OID
     ;; Has precedence over Keyword: constant below
     '("\\(Policy\\): \\([0-9]+\\.[0-9]+\\(:?\\.[0-9]+\\)*\\)"
       (1 'x509-keyword-face)
       (2 'x509-oid-face))

     ;; E.g. Public Key Algorithm: rsaEncryption
     `(,x509--keyword-w-constant
       (1 'x509-keyword-face)
       (2 'x509-constant-face))

     ;; CA:TRUE, CA:FALSE
     ;; CA used to be keyword+argument but CA: can be part of hex-string
     '("\\(CA\\):\\(TRUE\\|FALSE\\)"
       (1 'x509-keyword-face)
       (2 'x509-constant-face))

     ;; Hex dumps At least two two-digit hex-numbers separated by `:'
     ;; Can end in `:' for example in "Modulus"
     ;; fa:09(:....)
     '("[0-9a-fA-F][0-9a-fA-F]\\(?::[0-9a-fA-F][0-9a-fA-F]\\)+:?$" .
       'x509-hex-string-face)))
  "OpenSSL x509 highlighting.")

(defun x509-mode--kill-buffer()
  "Kill current buffer."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer))

;;;###autoload
(define-derived-mode x509-mode fundamental-mode "x509"
  "Major mode for displaying OpenSSL output.

\\{x509-mode-map}"
  (set (make-local-variable 'font-lock-defaults) '(x509-font-lock-keywords t))
  (define-key x509-mode-map "q" 'x509-mode--kill-buffer)
  (define-key x509-mode-map "t" 'x509--toggle-mode)
  (define-key x509-mode-map "e" 'x509--edit-params)
  (x509--mark-browse-http-links)
  (x509--mark-browse-oid))

(defun x509--buffer-encoding(buffer)
  "Heuristic for identifying PEM or DER encoding in BUFFER.
Return string \"PEM\" or \"DER\"."
  (with-current-buffer buffer
    (goto-char (point-min))
    (save-match-data
      (if (search-forward "-----BEGIN" nil t)
          "PEM"
        "DER"))))

(defun x509--pem-region()
  "Determine if point is in region delimited by \"-----BEGIN\" \"-----END\".
Return (begin . end) or nil"
  (save-excursion
    (save-match-data
      (let ((here (point)))
        (if (or (looking-at "-----BEGIN \\(.*?\\)-----")
                (re-search-backward "-----BEGIN \\(.*?\\)-----" nil t))
            (let ((begin (match-beginning 0))
                  (type (match-string-no-properties 1)))
              (if (and (search-forward (concat "-----END " type "-----") nil t)
                       ;; Ensure point is between begin and end.
                       (< here (match-end 0)))
                  (cons begin (match-end 0)))))))))

(defun x509--pem-region-type ()
  "Return type of pem region or nil if not matched.
Ex \"CERTIFICATE\" or \"DH PARAMETERS\""
  (let ((region (x509--pem-region)))
    (if region
        (save-excursion
          (save-match-data
            (goto-char (car region))
            (if (re-search-forward "-----BEGIN \\(.*?\\)-----" (cdr region) t)
                (match-string-no-properties 1)))))))

(defun x509--generate-input-buffer ()
  "Return a buffer containing data to be processed by OpenSSL.

Determine what portion of the current buffer is interesting to pass to
OpenSSL.

If point is in region delimited by \"-----BEGIN\" \"-----END\"
then that region is selected. The region is trimmed so that
leading and trailing non base-64 characters on each line are
removed. The idea is to be able to view, for example, a
certificate that is embedded as string in code.

If point is not in a PEM region, the whole buffer is used."
  (let* ((region (x509--pem-region))
         (begin (if region (car region) (point-min)))
         (end (if region (cdr region) (point-max)))
         (data (if region (buffer-substring-no-properties begin end)))
         (src-buffer (current-buffer))
         (new-buf (generate-new-buffer (generate-new-buffer-name
                                        (format " *in-x-%s*" (buffer-name))))))
    (with-current-buffer new-buf
      (set-buffer-file-coding-system 'no-conversion)
      (if region
          (insert data)
        (insert-buffer src-buffer))
      ;; If in PEM region, try to strip non base-64 characters
      (when region
        (goto-char (point-min))
        (while (re-search-forward
                "^[^-A-Za-z0-9+=/]+\\|[^-A-Za-z0-9+=/]+$" nil t)
          (replace-match "" nil nil))
        ;; Strip \n from eol. Can be seen in C code.
        (goto-char (point-min))
        (while (re-search-forward "\\\\n$" nil t)
          (replace-match "" nil nil)))
      new-buf)))

(defvar-local x509--shadow-buffer nil
  "Input buffer used for OpenSSL command.

Used when a view buffer wants to change command parameters and
re-process the input buffer or to change the command altogether.")
;; Make buffer local variable persist during major mode change.
(put 'x509--shadow-buffer 'permanent-local t)

(defvar-local x509--x509-mode-shadow-arguments nil
  "Current OpenSSL command arguments used in x509-mode.")
;; Make buffer local variable persist during major mode change.
(put 'x509--x509-mode-shadow-arguments 'permanent-local t)

(defvar-local x509--x509-asn1-mode-shadow-arguments nil
  "Current OpenSSL command argument used in x509-asn1-mode.")
;; Make buffer local variable persist during major mode change.
(put 'x509--x509-asn1-mode-shadow-arguments 'permanent-local t)

(defvar-local x509--x509-asn1-mode-offset-stack nil
  "Stack of (command start header-len pos) for strparse/offset x509-asn1-mode.
POS is the buffer position when going down. Used to restore pos
when going back up.")
;; Make buffer local variable persist during major mode change.
(put 'x509--x509-asn1-mode-offset-stack 'permanent-local t)

(defun x509--kill-shadow-buffer ()
  "Kill buffer hook function.
Run when killing a view buffer for cleaning up associated input buffer."
  (when (bound-and-true-p x509--shadow-buffer)
    (kill-buffer x509--shadow-buffer)))

(defun x509--process-buffer(input-buf openssl-arguments &optional output-buf)
  "Create new buffer named \"*x-[buffer-name]*\".

Pass content INPUT-BUF to openssl with
OPENSSL-ARGUMENTS. E.g. x509 -text.  If OUTPUT-BUF is non-'nil',
out to that buffer instead of generating a new one.

Return output buffer."
  (interactive)
  (let* ((buf (or output-buf
                  (generate-new-buffer (generate-new-buffer-name
                                        (format "*x-%s*" (buffer-name))))))
         ;; Operate on whole buffer. Output to buf
         (args (append
                (list nil nil x509-openssl-cmd nil buf nil)
                openssl-arguments)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; Remember input-buffer and arguments
      (setq x509--shadow-buffer input-buf)
      (add-hook 'kill-buffer-hook 'x509--kill-shadow-buffer nil t)
      (with-current-buffer input-buf
        (apply 'call-process-region args))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t))
    buf))

(defun x509--read-arguments (prompt default history)
  "Prompt, using PROMPT, for arguments if \\[universal-argument] prefix.

Provide DEFAULT argument and HISTORY.
Return argument string."
  (let ((history-delete-duplicates t))
    (if (equal current-prefix-arg '(4))
        (read-from-minibuffer prompt default nil nil history)
      (progn
        (add-to-history history default)
        default))))

(defun x509--add-inform-spec(arguments encoding)
  "Add or modify \"-inform ENCODING\" in ARGUMENTS."
  (if (string-match "-inform \\(PEM\\|DER\\)" arguments)
      (replace-match encoding nil nil arguments 1)
    (format "%s -inform %s" arguments encoding)))

(defun x509--generic-view (default history mode &optional input-buf output-buf)
  "Prepare an input buffer for data to be processed.
Optionally get modified command arguments from user.
Process data from input buffer using command arguments.

DEFAULT is the initial command line arguments to OpenSSL.
HISTORY is the command history symbol used with
`read-from-minibuffer'.  MODE is the major mode that will be
applied to the result buffer.  If INPUT-BUF is non-'nil', use
existing input buffer instead of creating one.  If OUTPUT-BUF is
non-'nil', use that instead of creating a new one.

Switch to resulting buffer and return it."
  (let* ((in-buf (or input-buf (x509--generate-input-buffer)))
         (encoding (x509--buffer-encoding in-buf))
         (initial (x509--add-inform-spec default encoding))
         (args (x509--read-arguments "arguments: " initial history))
         (result-buffer (x509--process-buffer
                         in-buf
                         (split-string-and-unquote args) output-buf)))
    (switch-to-buffer result-buffer)
    ;; Remember what arguments where used.
    (if (eq mode 'x509-mode)
        (setq x509--x509-mode-shadow-arguments args)
      (setq x509--x509-asn1-mode-shadow-arguments args))
    (funcall mode)
    result-buffer))

(defun x509--get-x509-toggle-mode-args ()
  "Ask user for command and return default arguments for that command."
  (let* ((collection '(("cert" . x509-x509-default-arg)
                       ("req" . x509-req-default-arg)
                       ("crl" . x509-crl-default-arg)
                       ("pkcs7" . x509-pkcs7-default-arg)
                       ("dhparam" . x509-dhparam-default-arg)
                       ("key" . x509-pkey-default-arg)
                       ("publickey" . x509-pkey-pubin-default-arg)
                       ("asn1parse" . x509-asn1parse-default-arg)))
         (choice (completing-read
                  "Parse as: "          ; PROMPT
                  collection            ; COLLECTION
                  nil                   ; PREDICATE
                  t                     ; REQUIRE-MATCH
                  )))
    (symbol-value (cdr (assoc choice collection)))))

(defun x509--get-x509-history (args)
  "Return history variable that matches command ARGS."
  (pcase (car (split-string-and-unquote args))
    ("x509" 'x509--viewcert-history)
    ("req" 'x509--viewreq-history)
    ("crl" 'x509--viewcrl-history)
    ("pkcs7" 'x509--viewpkcs7-history)
    ("dhparam" 'x509--viewdh-history)
    ("pkey"
     (if (string-match-p "-pubin" args)
         'x509--viewpublickey-history
       'x509--viewkey-history))
    ("asn1parse" 'x509--viewasn1-history)
    (_ nil)))

(defun x509--toggle-mode(&optional edit)
  "Toggle between asn1-mode and x509-mode.

If EDIT is non-'nil', edit current command arguments and redisplay."
  (interactive)
  (if edit
      (setq current-prefix-arg '(4)))
  (if (or (and edit (derived-mode-p 'x509-asn1-mode))   ; Edit asn1 mode
          (and (not edit) (derived-mode-p 'x509-mode))) ; Toggle to asn1
      (let ((default-args (or x509--x509-asn1-mode-shadow-arguments
                              x509-asn1parse-default-arg)))
        (x509--generic-view default-args 'x509--viewasn1-history
                            'x509-asn1-mode
                            x509--shadow-buffer (current-buffer)))
    (let* ((default-args (or x509--x509-mode-shadow-arguments
                             (x509--get-x509-toggle-mode-args)))
           (history (x509--get-x509-history default-args)))
      (x509--generic-view default-args history 'x509-mode
                          x509--shadow-buffer (current-buffer)))))

(defun x509--edit-params()
  "Edit command parameters in current buffer."
  (interactive)
  (x509--toggle-mode t))

;; ---------------------------------------------------------------------------
(defvar x509--viewcert-history nil "History list for `x509-viewcert'.")
;;;###autoload
(defun x509-viewcert ()
  "Parse current buffer as a certificate file.

With \\[universal-argument] prefix, you can edit the command arguments."
  (interactive)
  (x509--generic-view x509-x509-default-arg 'x509--viewcert-history
                      'x509-mode))

;; ---------------------------------------------------------------------------
(defvar x509--viewreq-history nil "History list for `x509-viewreq'.")
;;;###autoload
(defun x509-viewreq ()
  "Parse current buffer as a certificate request file.

With \\[universal-argument] prefix, you can edit the command arguments."
  (interactive)
  (x509--generic-view x509-req-default-arg 'x509--viewreq-history 'x509-mode))

;; ---------------------------------------------------------------------------
(defvar x509--viewcrl-history nil "History list for `x509-viewcrl'.")
;;;###autoload
(defun x509-viewcrl ()
  "Parse current buffer as a CRL file.

With \\[universal-argument] prefix, you can edit the command arguments."
  (interactive)
  (x509--generic-view x509-crl-default-arg 'x509--viewcrl-history 'x509-mode))

;; ---------------------------------------------------------------------------
(defvar x509--viewpkcs7-history nil "History list for `x509-viewpkcs7'.")
;;;###autoload
(defun x509-viewpkcs7 ()
  "Parse current buffer as a PKCS#7 file.

Output only certificates and CRLs by default.  Add the \"-print\"
switch to output details.

With \\[universal-argument] prefix, you can edit the command arguments."
  (interactive)
  (x509--generic-view x509-pkcs7-default-arg 'x509--viewpkcs7-history
                      'x509-mode))


;; ---------------------------------------------------------------------------
(defvar x509--viewdh-history nil "History list for `x509-viewdh'.")
;;;###autoload
(defun x509-viewdh ()
  "Parse current buffer as a DH-parameter file.

With \\[universal-argument] prefix, you can edit the command arguments."
  (interactive)
  (x509--generic-view x509-dhparam-default-arg 'x509--viewdh-history
                      'x509-mode))

;; ---------------------------------------------------------------------------
(defvar x509--viewkey-history nil "History list for `x509-viewkey'.")
;;;###autoload
(defun x509-viewkey ()
  "Display x509 private key using the OpenSSL pkey command.

With \\[universal-argument] prefix, you can edit the command arguments."
  (interactive)
  (x509--generic-view x509-pkey-default-arg 'x509--viewkey-history 'x509-mode))

;; ---------------------------------------------------------------------------
(defvar x509--viewpublickey-history nil
  "History list for `x509-publicviewkey'.")
;;;###autoload
(defun x509-viewpublickey ()
  "Display x509 public key using the OpenSSL pkey command.

With \\[universal-argument] prefix, you can edit the command arguments."
  (interactive)
  (x509--generic-view x509-pkey-pubin-default-arg
                      'x509--viewpublickey-history 'x509-mode))

;; ---------------------------------------------------------------------------
(defvar x509--viewlegacykey-history nil
  "History list for `x509-viewlegacykey'.")
;; Special. older openssl pkey cannot read from stdin so we need to use
;; buffer's file.
;;;###autoload
(defun x509-viewlegacykey (&optional args)
  "Display x509 private key using the OpenSSL pkey command.

This function works with older OpenSSL that could not read key from
stdin.  Instead, the buffer file is used with -in.

ARGS are arguments to the openssl command.

With \\[universal-argument] prefix, you can edit the command arguments.
For example to enter pass-phrase, add -passin pass:PASSPHRASE."
  (interactive (list (x509--read-arguments
                      "pkey args: "
                      (format "%s -inform %s -in \"%s\""
                              x509-pkey-default-arg
                              (x509--buffer-encoding (current-buffer))
                              (buffer-file-name))
                      'x509--viewlegacykey-history)))
  (let* ((buf (generate-new-buffer (generate-new-buffer-name
                                    (format "*x-%s*" (buffer-name))))))
    (setq args (append
                (list x509-openssl-cmd nil buf nil)
                (split-string-and-unquote args)))
    (apply 'call-process args)
    (switch-to-buffer buf)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (x509-mode)))

(defun x509--dwim-tester(openssl-commamd-args)
  "Test running OPENSSL-COMMAMD-ARGS in current buffer.
Return t if return status is 0, otherwise nil. Use to determine
if the buffer contains data of certain type."
  (let* ((in-buf (x509--generate-input-buffer))
         (encoding (x509--buffer-encoding in-buf))
         (args (x509--add-inform-spec openssl-commamd-args encoding))
         ;; Arguments to `call-process-region'. Just run the command and
         ;; discard the output.
         (proc-args (append
                     (list nil nil x509-openssl-cmd nil nil nil)
                     (split-string-and-unquote args))))
    (prog1
        (= 0 (with-current-buffer in-buf
               (apply 'call-process-region proc-args)))
      (kill-buffer in-buf))))

;; ---------------------------------------------------------------------------
;;;###autoload
(defun x509-dwim ()
  "Guess the type of object and call the corresponding view-function.

Look at -----BEGIN header for known object types.  Then test
different openssl commands until one succeeds.  Call
`x509-viewasn1' as a last resort."
  (interactive)
  (pcase (x509--pem-region-type)
    ((or "CERTIFICATE" "TRUSTED CERTIFICATE")
     (call-interactively 'x509-viewcert))
    ("CERTIFICATE REQUEST"
     (call-interactively 'x509-viewreq))
    ("DH PARAMETERS"
     (call-interactively 'x509-viewdh))
    ("PKCS7"
     (call-interactively 'x509-viewpkcs7))
    ((or "ENCRYPTED PRIVATE KEY" "PRIVATE KEY" "RSA PRIVATE KEY")
     (call-interactively 'x509-viewkey))
    ("PUBLIC KEY"
     (call-interactively 'x509-viewpublickey))
    ("X509 CRL"
     (call-interactively 'x509-viewcrl))
    (_
     (cond
      ((x509--dwim-tester x509-x509-default-arg)
       (call-interactively 'x509-viewcert))
      ((x509--dwim-tester x509-crl-default-arg)
       (call-interactively 'x509-viewcrl))
      ((x509--dwim-tester x509-pkey-default-arg)
       (call-interactively 'x509-viewkey))
      ((x509--dwim-tester x509-pkey-pubin-default-arg)
       (call-interactively 'x509-viewpublickey))
      ((x509--dwim-tester x509-req-default-arg)
       (call-interactively 'x509-viewreq))
      ((x509--dwim-tester x509-dhparam-default-arg)
       (call-interactively 'x509-viewdh))
      ((x509--dwim-tester x509-pkcs7-default-arg)
       (call-interactively 'x509-viewpkcs7))
      (t
       (call-interactively 'x509-viewasn1))))))

;; ----------------------------------------------------------------------------
;; asn1-mode

(defun x509--asn1-get-offset()
  "Return offset at current ASN.1 line.

Ex ^ 63:d=1  hl=2 l=  34
-> 63"
  (save-excursion
    (move-beginning-of-line 1)
    (if (re-search-forward "^ *\\([0-9]+\\):d=[0-9]+ *hl=\\([0-9]+\\)" nil t)
        (string-to-number (match-string-no-properties 1))
      0)))

(defun x509--asn1-get-total-length()
  "Return header length + data length current ASN.1 line.

Ex ^ 63:d=1  hl=2 l=  34
-> 2 + 34 = 36"
  (save-excursion
    (move-beginning-of-line 1)
    (if (re-search-forward "^ *\\([0-9]+\\):d=[0-9]+ *hl=\\([0-9]+\\) *l= *\\([0-9]+\\)" nil t)
        (+ (string-to-number (match-string-no-properties 2))
           (string-to-number (match-string-no-properties 3)))
      0)))

(defun x509--asn1-get-header-len()
  "Return header length at current ASN.1 line.

Ex ^ 63:d=1  hl=2 l=  34
-> 2

If current line is a BITSTRING, we add 1 to the header length to
account for the unused-bits byte."
  (save-excursion
    (move-beginning-of-line 1)
    (if (re-search-forward "^ *\\([0-9]+\\):d=[0-9]+ *hl=\\([0-9]+\\)" nil t)
        (let ((hl (string-to-number (match-string-no-properties 2)))
              (add-one (if (re-search-forward "BIT STRING"
                                              (line-end-position) t)
                           1
                         0)))
          (+ hl add-one))
      0)))

(defun x509--asn1-update-command-line-start-arg (arguments command start)
  "Add, modify or remove -offset N or -strparse N argument in ARGUMENTS.
Return updated argument string."
  (if (= start 0)
      ;; Remove -offset|-strparse argument of zero
      (if (string-match " *\\(?:-offset\\|-strparse\\) [0-9]+" arguments)
          (replace-match "" nil nil arguments 0)
        arguments)
    ;; Replace existing argument
    (if (string-match "\\(?:-offset\\|-strparse\\) [0-9]+" arguments)
        (replace-match (concat command " " (number-to-string start))
                       nil nil arguments 0)
      ;; Add new
      (format "%s %s %s" arguments command start))))

(defvar x509--asn1-mode-name "asn1"
  "Major mode name displayed in mode line.")

(defun x509--asn1-update-mode-line ()
  "Update command line mode name."
  (let* ((top (car x509--x509-asn1-mode-offset-stack))
         (command (if top
                      (if (string= (nth 0 top) "-strparse")
                          "s"
                        "o")))
         (offset (if top (nth 1 top)))
         new-mode-name)
    (if offset
        (setq new-mode-name (format "%s[%s%s]"
                                    x509--asn1-mode-name command offset))
      (setq new-mode-name x509--asn1-mode-name))
    (when (not (string= mode-name new-mode-name))
      (setq mode-name new-mode-name)
      (force-mode-line-update))))

(defun x509--asn1-offset-strparse(command)
  "Add -offset N or -strparse N to command line and redisplay.
COMMAND must be either \"-offset\" or \"-strparse\".
When \"-offset\", N i set to current offset + offset on line + header length.
When \"-strparse\", N i set to current offset + offset on line.

Mileage may vary if mixing calls to strparse and offset. We try
to get it right but it can get confusing.
"
  (let* ((line-offset (x509--asn1-get-offset))
         (header-len (x509--asn1-get-header-len))
         (strparsep (string= command "-strparse"))
         (add-header (if strparsep 0 header-len))
         (top (car x509--x509-asn1-mode-offset-stack))
         (current-offset (if top
                             (+ (nth 1 top) (if strparsep (nth 2 top) 0))
                           0))
         (new-offset (+ current-offset line-offset add-header))
         (new-args (x509--asn1-update-command-line-start-arg
                    (or x509--x509-asn1-mode-shadow-arguments
                        x509-asn1parse-default-arg)
                    command
                    new-offset)))
    (if (> new-offset 0)
        (push (list command new-offset header-len (point))
              x509--x509-asn1-mode-offset-stack))
    (x509--generic-view new-args 'x509--viewasn1-history
                        'x509-asn1-mode
                        x509--shadow-buffer (current-buffer))
    (x509--asn1-update-mode-line)))

(defun x509--asn1-offset-down()
  "Add -offset N argument to current asn1 command line and redisplay.
Offset is calculated from offset on current line."
  (interactive)
  (x509--asn1-offset-strparse "-offset"))

(defun x509--asn1-strparse()
  "Add -strparse N argument to current asn1 command line and redisplay.
Offset is calculated from offset on current line."
  (interactive)
  (x509--asn1-offset-strparse "-strparse"))

(defun x509--asn1-offset-up()
  "Pop offset and redisplay."
  (interactive)
  (when (and (boundp 'x509--x509-asn1-mode-offset-stack)
             x509--x509-asn1-mode-offset-stack)
    (let* ((current (pop x509--x509-asn1-mode-offset-stack))
           (point (nth 3 current))
           (up (car x509--x509-asn1-mode-offset-stack))
           (command (if up (nth 0 up) "none"))
           (offset (if up (nth 1 up) 0))
           (new-args (x509--asn1-update-command-line-start-arg
                      (or x509--x509-asn1-mode-shadow-arguments
                          x509-asn1parse-default-arg)
                      command
                      offset)))
      (x509--generic-view new-args 'x509--viewasn1-history
                          'x509-asn1-mode
                          x509--shadow-buffer (current-buffer))
      (goto-char point)
      (x509--asn1-update-mode-line))))

(defvar-local x509-asn1--last-point nil
  "Used to detect when the point has moved.")

(defvar-local x509-asn1--hexl-buffer nil
  "Hex buffer matching asn1 input buffer")

(defvar x509-asn1-overlays nil
  "List of overlays to use.")

(defun x509-asn1--remove-overlays ()
  "Clean up overlays, assuming they are no longer needed."
  (mapc #'delete-overlay x509-asn1-overlays)
  (setq x509-asn1-overlays nil))

(defun x509-asn1--setup-overlay (start end buf)
  "Setup overlay with START and END in BUF."
  (let ((o (make-overlay start end buf)))
    (overlay-put o 'face 'region)
    o))

(defun x509-asn1--hexl-offset-start (offset)
  "Return buffer point where byte at OFFSET starts."
  (let* ((sixteens (/ offset 16))
         (addresses (* 10 (+ 1 sixteens)))
         (trailers  (* 18 sixteens))
         (spaces (/ offset 2))
         (bytes (* offset 2)))
    ;; Point is 1 based.
    (+ 1 addresses trailers spaces bytes)))

(defun x509-asn1--hexl-offset-end (offset)
  "Return buffer point where OFFSET ends."
  (let* ((sixteens (/ offset 16))
         (even-sixteen (and (> offset 0)
                            (= 0 (mod offset 16))))
         (count-addresses (if even-sixteen
                              sixteens
                            (1+ sixteens)))
         (count-trailers (if even-sixteen
                             (max 0 (1- sixteens))
                           sixteens))
         (addresses (* 10 count-addresses))
         (trailers  (* 18 count-trailers))
         (spaces (/ offset 2))
         (bytes (* offset 2)))
    (if (= 0 offset)
        ;; Special. Don't risk end ahead of start.
        (x509-asn1--hexl-offset-start offset)
      ;; Point is 1 based.
      (+ 1 addresses trailers spaces bytes))))

(defun x509-asn1--update-overlays()
  (let* ((first (x509--asn1-get-offset))
         (length (x509--asn1-get-total-length))
         (last (+ first length))
         (hexl-start (x509-asn1--hexl-offset-start first))
         (hexl-end (x509-asn1--hexl-offset-end last)))
    (with-current-buffer x509-asn1--hexl-buffer
      (x509-asn1--remove-overlays)
      (if (eq ?  (char-after (1- hexl-end)))
          (setq hexl-end (- hexl-end 1)))
      (push (x509-asn1--setup-overlay hexl-start hexl-end (current-buffer))
            x509-asn1-overlays))))

(defun x509-asn1--post-command-hook()
  (unless (eq (point) x509-asn1--last-point)
    (setq x509-asn1--last-point (point))
    (x509-asn1--update-overlays)))

(defun x509-asn1-hexl()
  "Display hex buffer matching current input puffer"
  (interactive)
  (let ((src-buffer x509--shadow-buffer)
        (hexl-buffer (get-buffer-create "xhel")))
    (with-current-buffer hexl-buffer
      (set-buffer-file-coding-system 'no-conversion)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert-buffer src-buffer)
      (let ((buffer-undo-list t))
        (hexlify-buffer))
      (read-only-mode)
      (display-buffer (current-buffer) '(nil (inhibit-same-window . t))))
    (add-hook 'post-command-hook #'x509-asn1--post-command-hook nil t)
    ;;(run-at-time 0 nil #'x509-asn1--update-overlays)
    (setq x509-asn1--hexl-buffer hexl-buffer)
    ))

(eval-when-compile
  (defconst x509--asn1-primitives-keywords
    (regexp-opt '("prim" "EOC" "BOOLEAN" "INTEGER" "BIT_STRING" "BIT STRING"
                  "OCTET_STRING" "OCTET STRING" "NULL" "OID"
                  "UTCTIME" "GENERALIZEDTIME" "ENUMERATED"))))

(eval-when-compile
  (defconst x509--asn1-cons-keywords
    (regexp-opt '("SEQUENCE" "SET"))))

(eval-when-compile
  (defconst x509--asn1-strings
    (concat (regexp-opt
             ;; Keyword:
             '("UTF8STRING" "PRINTABLESTRING" "IA5STRING")
             t )   ; t = enclose in \\( \\) for easy subexpr reference
            ;; Followed by a string
            " *:\\(.*?\\)\\(?: *:\\|$\\)")))

(eval-when-compile
  (defconst x509--asn1-oid
    (concat (regexp-opt
             ;; Keyword:
             '("OID" "OBJECT")
             t )   ; t = enclose in \\( \\) for easy subexpr reference
            ;; Followed by an OID (derdigger) or object name (asn1parse)
            " *:\\(.*?\\)\\(?: *:\\|$\\)")))

(defconst x509-asn1-font-lock-keywords
  (eval-when-compile
    (list
     ;; Undetermined length, i.e. when the length byte is 0x80 indicating
     ;; zero following length bytes.
     '("l=\\(inf\\) "
       (1 'x509-constant-face))
     ;; BOOLEAN, INTEGER and such
     `(,x509--asn1-primitives-keywords . 'x509-keyword-face)
     ;; SET, SEQUENCE
     `(,x509--asn1-cons-keywords . 'x509-asn1-sequence-face)
     ;; cons: as in constructed. Same font as SET and SEQUENCE
     '("\\(cons\\):" (1 'x509-asn1-sequence-face))
     ;; Like SET and SEQUENCE
     '("\\(cont\\|appl\\|priv\\) \\[\\(.*?\\)\\]"
       (1 'x509-asn1-sequence-face)
       (2 'x509-string-face))
     ;; Parsing error messages
     '("error:.*\\|Error in encoding" . 'x509-warning-face)
     ;; String type + string value
     `(,x509--asn1-strings
       (1 'x509-keyword-face)
       (2 'x509-string-face))
     ;; "OID" followed by oid
     `(,x509--asn1-oid
       (1 'x509-keyword-face)
       (2 'x509-oid-face))))
  "openssl asn1parse highlighting")

;;;###autoload
(define-derived-mode x509-asn1-mode fundamental-mode x509--asn1-mode-name
  "Major mode for displaying openssl asn1parse output.

\\{x509-asn1-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(x509-asn1-font-lock-keywords t))
  (define-key x509-asn1-mode-map "q" 'x509-mode--kill-buffer)
  (define-key x509-asn1-mode-map "t" 'x509--toggle-mode)
  (define-key x509-asn1-mode-map "e" 'x509--edit-params)
  (define-key x509-asn1-mode-map "d" 'x509--asn1-offset-down)
  (define-key x509-asn1-mode-map "s" 'x509--asn1-strparse)
  (define-key x509-asn1-mode-map "u" 'x509--asn1-offset-up)
  (define-key x509-asn1-mode-map "x" 'x509-asn1-hexl)
  (x509--mark-browse-http-links)
  (x509--mark-browse-oid))

(defvar x509--viewasn1-history nil "History list for `x509-viewasn1'.")

;;;###autoload
(defun x509-viewasn1 ()
  "Parse current buffer as ASN.1.

With \\[universal-argument] prefix, you can edit the command arguments."
  (interactive)
  (x509--generic-view x509-asn1parse-default-arg 'x509--viewasn1-history
                      'x509-asn1-mode))

(provide 'x509-asn1-mode)
(provide 'x509-mode)

;;; x509-mode.el ends here
