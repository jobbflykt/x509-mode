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

;; Uses OpenSSL for viewing PEM and DER encoded PKI entities.

;; Usage:
;; Open a file containing a certificate, either PEM or DER encode.  Now
;; use M-x `x509-viewcert' to create a new buffer that displays the decoded
;; certificate.
;; Use `x509-viewcrl', `x509-viewasn1',`x509-viewkey', `x509-viewdh',
;; `x509-viewreq', `x509-viewpkcs7' in a similar manner.
;;
;; When point is at or in a PEM encoded region, M-x `x509-dwim' tries to guess
;; what view-function to call.  It falls back to `x509-viewasn1' if it fails.
;;
;; Use C-u prefix with any command for editing the command arguments.
;;
;; When in a x509 buffer, use keys `e' and `t' to edit current command or
;; toggle between x509-asn1-mode and x509-mode respectively.

;;; Code:

(require 'cl-lib)
(require 'time-date)

(defgroup x509 nil
  "View certificates, CRLs, keys and other related files using OpenSSL."
  :group 'extensions
  :group 'convenience
  :link '(emacs-library-link :tag "Lisp File" "x509-mode.el"))

(defcustom x509-openssl-cmd "openssl"
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

(defface x509-oid-face
  '((t (:inherit font-lock-constant-face)))
  "Face for unknown OIDs."
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

(defun x509--match-date-near-now (bound)
  (x509--match-date
   (lambda (time now)
     ;; We should highlight near expire times
     ;; and time is not in the future
     ;; and time is within decoded-time-delta from now
     (and x509-warn-near-expire-days
          (time-less-p now time)
          (let* ((inhibit-message t)
                 (message-log-max nil)

                 (delta (make-decoded-time
                         :day x509-warn-near-expire-days))
                 (decoded-now (decode-time now))
                 decoded-now-plus-delta
                 encoded-now-plus-delta)
            ;; FIXME The message "obsolete timestamp with cdr" appears when
            ;; decoded-time-add is called. Has something to do with
            ;; WARN_OBSOLETE_TIMESTAMPS in timefns.c but I don't understand
            ;; what the correct thing to do is.  For now, inhibit message while
            ;; doing time calculations
            (let ((inhibit-message t)
                  (message-log-max nil))
              (setq decoded-now-plus-delta (decoded-time-add decoded-now delta)))
            ;; FIXME In emacs 25.1, encode-time needs 6+ arguments while
            ;; later emacs uses just one, the time. If support for emacs 25.1 is
            ;; dropped, there is no need for `funcall' here.
            (setq encoded-now-plus-delta
                  (apply 'encode-time decoded-now-plus-delta))
            (time-less-p time encoded-now-plus-delta))))
   bound))

(defun x509--mark-browse-url-links()
  "Make http URLs clickable by making them buttons."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (search-forward-regexp
              "\\(file\\|https?\\)://[-_.:/A-Za-z0-9]+" nil t)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (url (match-string-no-properties 0))
               (help-echo (format "Click to browse-url %s" url)))
          ;; The url is stored in the face property
          (make-button
           start end
           'face 'x509-browse-url-face
           'follow-link t
           'x509-browse-url-face url
           'help-echo help-echo
           'action (lambda (button)
                     (browse-url
                      (button-get button 'x509-browse-url-face)))))))))

(defun x509--load-data-file (filename)
  "Split FILENAME linewise into a list.
Skip blank lines and comment lines.  Return list."
  (with-temp-buffer
    (insert-file-contents
     (if (null load-file-name) filename
       (expand-file-name filename (file-name-directory load-file-name))))
    (cl-remove-if (lambda (s) (string-match-p "^ *\\(?:#\\|$\\)" s))
                  (split-string (buffer-string) "\n"))))

(defconst x509--keywords
  (regexp-opt
   (x509--load-data-file "keywords.txt")))

(defconst x509--constants
  (regexp-opt (x509--load-data-file "constants.txt") 'words))

;; Keyword: constant
;; E.g. "Signature Algorithm: sha1WithRSAEncryption"
(defconst x509--keyword-w-constant
  (concat (regexp-opt
           (x509--load-data-file "keyword+constant.txt") t)
          ;; Followed by ": constant"
          ": *\\(.*\\)"))

;; Multiline Issuer and Subject, "-nameopt multiline"
;; E.g. "commonName                = GlobalSign Root CA"
(defconst x509--multiline-name
  (concat (regexp-opt
           (x509--load-data-file "long-name.txt") t)
          " *= \\(.*\\)"))

(defconst x509-font-lock-keywords
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
     'x509-hex-string-face))
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
  (set (make-local-variable 'font-lock-defaults) '(x509-font-lock-keywords))
  (define-key x509-mode-map "q" 'x509-mode--kill-buffer)
  (define-key x509-mode-map "t" 'x509--toggle-mode)
  (define-key x509-mode-map "e" 'x509--edit-params)
  (x509--mark-browse-url-links))

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
         (data (buffer-substring-no-properties begin end))
         (new-buf (generate-new-buffer (generate-new-buffer-name
                                        (format " *in-x-%s*" (buffer-name))))))
    (with-current-buffer new-buf
      (insert data)
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
      (setq x509--shadow-buffer input-buf)
      (add-hook 'kill-buffer-hook 'x509--kill-shadow-buffer nil t)
      (with-current-buffer input-buf
        (apply 'call-process-region args))
      ;; remember input-buffer and arguments
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
non-'nil', use that instead of creating a new one."
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
    (funcall mode)))

(defun x509--get-x509-toggle-mode-args ()
  "Ask user for command and return default arguments for that command."
  (let* ((collection '(("cert" . x509-x509-default-arg)
                       ("req" . x509-req-default-arg)
                       ("crl" . x509-crl-default-arg)
                       ("pkcs7" . x509-pkcs7-default-arg)
                       ("dhparam" . x509-dhparam-default-arg)
                       ("key" . x509-pkey-default-arg)
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
    ("pkey" 'x509--viewkey-history)
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

;; ---------------------------------------------------------------------------
;;;###autoload
(defun x509-dwim ()
  "Guess the type of object and call the corresponding view-function.

Look at -----BEGIN header for known object types.  If unknown
type, call `x509-viewasn1'."
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
    ("X509 CRL"
     (call-interactively 'x509-viewcrl))
    (_
     (call-interactively 'x509-viewasn1))))

;; ----------------------------------------------------------------------------
;; asn1-mode

(defconst x509--asn1-primitives-keywords
  (regexp-opt '("prim" "EOC" "BOOLEAN" "INTEGER" "BIT_STRING" "BIT STRING"
                "OCTET_STRING" "OCTET STRING" "NULL" "OID"
                "UTCTIME" "GENERALIZEDTIME" "ENUMERATED")))

(defconst x509--asn1-cons-keywords
  (regexp-opt '("SEQUENCE" "SET")))

(defconst x509--asn1-strings
  (concat (regexp-opt
           ;; Keyword:
           '("UTF8STRING" "PRINTABLESTRING" "IA5STRING")
           t )   ; t = enclose in \\( \\) for easy subexpr reference
          ;; Followed by a string
          " *:\\(.*?\\)\\(?: *:\\|$\\)"))

(defconst x509--asn1-oid
  (concat (regexp-opt
           ;; Keyword:
           '("OID" "OBJECT")
           t )   ; t = enclose in \\( \\) for easy subexpr reference
          ;; Followed by an OID (derdigger) or object name (asn1parse)
          " *:\\(.*?\\)\\(?: *:\\|$\\)"))

(defconst x509-asn1-font-lock-keywords
  (list
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
     (2 'x509-oid-face))
   "openssl asn1parse highlighting"))

;;;###autoload
(define-derived-mode x509-asn1-mode fundamental-mode "asn1"
  "Major mode for displaying openssl asn1parse output.

\\{x509-asn1-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(x509-asn1-font-lock-keywords))
  (define-key x509-asn1-mode-map "q" 'x509-mode--kill-buffer)
  (define-key x509-asn1-mode-map "t" 'x509--toggle-mode)
  (define-key x509-asn1-mode-map "e" 'x509--edit-params))

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
