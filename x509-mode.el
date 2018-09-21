;;; x509-mode.el --- View certificates, CRLs and keys using OpenSSL. -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Fredrik Axelsson <f.axelsson@gmai.com>

;; Author: Fredrik Axelsson <f.axelsson@gmai.com>
;;  Package-Requires: ((emacs "24.1") (cl-lib "0.5"))

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

;; Major for viewing certificates, CRLs, keys and DH-parameters.

;; Uses OpenSSL for viewing PEM and DER encoded PKI entities.

;; Usage:
;; Open a file containing a certificate, either PEM or DER encode.  Now
;; use M-x `x509-viewcert' to create a new buffer that displays the decoded
;; certificate.
;; Use M-x `x509-viewcrl', M-X `x509-viewasn1', M-x `x509-viewkey' and M-x
;; `x509-viewdh' in a similar manner.

;;; Code:

(defgroup x509 nil
  "View certificates, CRLs, keys and DH-parameters using OpenSSL"
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

(defgroup x509-faces nil
  "Faces used by x509."
  :group 'x509
  :group 'faces)

(defface x509-hex-string-face
  '((t (:inherit font-lock-comment-face :italic t)))
  "Face for colon-separated hex values."
  :group 'x509-faces)

(defface x509-oid-face
  '((t (:inherit font-lock-constant-face :bold t)))
  "Face for unknown OIDs."
  :group 'x509-faces)

(defface x509-bad-date-face
  '((t (:inherit default :background "red")))
  "Face for past and future dates."
  :group 'x509-faces)

(defface x509-browse-url-face
  '((((class color) (background light)) :inherit link)
    (((class color) (background  dark)) :inherit link))
  "Face for storing url used when clicking link.")

(defun x509--match-date (cmp bound)
  "Return true if it can find a date that CMP to current time.
Indented to search for dates in form \"Jun 11 00:00:01 2014 GMT\"
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
                (funcall cmp (date-to-time (match-string 0))
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
  "Return true if it can find a date that is the past.
Intended to search for dates in form \"Jun 11 00:00:01 2014 GMT\"
and compare them to the current time. Return non-nil, move point,
and set ‘match-data’ appropriately if it succeeds; like
‘re-search-forward’ would.  The optional argument BOUND is a
buffer position that bounds the search."
  (x509--match-date (lambda (d1 d2) (time-less-p d1 d2)) bound))

(defun x509--match-date-in-future (bound)
  "Return true if it can find a date that is the future.
Intended to search for dates in form \"Jun 11 00:00:01 2014 GMT\"
and compare them to the current time. Return non-nil, move point,
and set ‘match-data’ appropriately if it succeeds; like
‘re-search-forward’ would.  The optional argument BOUND is a
buffer position that bounds the search."
  (x509--match-date (lambda (d1 d2) (not (time-less-p d1 d2))) bound))

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

(require 'cl-lib)

(defun x509--load-data-file (filename)
  "Split FILENAME linewise into a list.
Skip blank lines and comment lines. Return list."
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

(defconst x509-font-lock-keywords
  (list
   `(,x509--keywords . 'font-lock-builtin-face)

   `(,x509--constants . 'font-lock-constant-face)

   ;; Validity on a line alone (preceding "Not Before:")
   '("^ +Validity ?$" . 'font-lock-builtin-face)

   ;; something=string until ',' or '/' or EOL
   ;; E.g. CN=apa,OU=Räv
   '("\\(\\<\\w+=\\)\\(.*?\\)\\(?:[,/]\\|$\\)"
     (1 'bold)
     (2 'font-lock-string-face))

   ;; something = string until ',' or EOL
   ;; E.g. CN = ACCVRAIZ1, OU = PKIACCV, O = ACCV, C = ES
   '("\\(\\<\\w+\\) = \\(.*?\\)\\(?:[,/]\\|$\\)"
     (1 'bold)
     (2 'font-lock-string-face))

   ;; URI: and CPS: . Highlight keyword. URL is handled by
   ;; `x509--mark-browse-url-links'
   '("\\<\\(URI:\\|CPS: \\)"
     (1 'font-lock-builtin-face))

   ;; DNS:string email:string
   '("\\<\\(DNS:\\|email:\\)\\(.*\\)"
     (1 'font-lock-builtin-face)
     (2 'font-lock-string-face))

   ;; Not Before: Jun 11 00:00:01 2014 GMT
   ;; Date is "MATCH-ANCHORED", see help for variable font-lock-keywords
   '("\\(Not Before\\): " (1 'font-lock-builtin-face)
     (x509--match-date-in-future nil nil (0 'x509-bad-date-face)))
   '("\\(Not After\\) : " (1 'font-lock-builtin-face)
     (x509--match-date-in-past nil nil (0 'x509-bad-date-face)))
   ;; For CRL's when Next Update is in the past
   '("\\(Next Update\\): " (1 'font-lock-builtin-face)
     (x509--match-date-in-past nil nil (0 'x509-bad-date-face)))

   ;; Policy: OID
   ;; Has precedence over Keyword: constant below
   '("\\(Policy\\): \\([0-9]+\\.[0-9]+\\(:?\\.[0-9]+\\)*\\)"
     (1 'font-lock-builtin-face)
     (2 'x509-oid-face))

   ;; E.g. Public Key Algorithm: rsaEncryption
   `(,x509--keyword-w-constant
     (1 'font-lock-builtin-face)
     (2 'font-lock-constant-face))

   ;; CA:TRUE, CA:FALSE
   ;; CA used to be keyword+argument but CA: can be part of hex-string
   '("\\(CA\\):\\(TRUE\\|FALSE\\)"
     (1 'font-lock-builtin-face)
     (2 'font-lock-constant-face))

   ;; Hex dumps At least two two-digit hex-numbers separated by `:'
   ;; Can end in `:' for example in "Modulus"
   ;; fa:09(:....)
   '("[0-9a-fA-F][0-9a-fA-F]\\(?::[0-9a-fA-F][0-9a-fA-F]\\)+:?$" .
     'x509-hex-string-face)
   )
  "OpenSSL x509 highlighting.")

(defun x509-mode--kill-buffer()
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer))

;;;###autoload
(define-derived-mode x509-mode fundamental-mode "x509"
  "Major mode for displaying openssl x509 output.

\\{x509-mode-map}"
  (set (make-local-variable 'font-lock-defaults) '(x509-font-lock-keywords))
  (define-key x509-mode-map "q" 'x509-mode--kill-buffer)
  (x509--mark-browse-url-links))

(defun x509--buffer-encoding()
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (if (search-forward "-----BEGIN" nil t)
          "PEM"
        "DER"))))

(defun x509--process-buffer(openssl-arguments)
  "Create new buffer named \"*x-[buffer-name]*\" and pass content of
current buffer to openssl with OPENSSL-ARGUMENTS. E.g. x509 -text"
  (interactive)
  (let* ((buf (generate-new-buffer (generate-new-buffer-name
                                    (format "*x-%s*" (buffer-name)))))
         (args (append
                (list (point-min) (point-max) x509-openssl-cmd nil buf nil)
                openssl-arguments)))
    (apply 'call-process-region args)
    (switch-to-buffer buf)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))

(defun x509--read-arguments (prompt default history)
  "Prompt, using PROMPT, for arguments if \\[universal-argument] prefix.

Provide DEFAULT arguement and HISTORY.
Return list with single argument string. "
  (if (equal current-prefix-arg '(4))
      (list (read-from-minibuffer prompt default nil nil history))
    (list default)))

(defvar x509--viewcert-history nil "History list for x509-viewcert.")

;;;###autoload
(defun x509-viewcert (&optional args)
  "Parse current buffer as a certificate file.
Display result in another buffer.

With \\[universal-argument] prefix, you can edit the command arguements."
  (interactive (x509--read-arguments
                "x509 args: "
                (format "x509 -nameopt utf8 -text -noout -inform %s"
                        (x509--buffer-encoding))
                'x509--viewcert-history))
  (x509--process-buffer (split-string-and-unquote args))
  (x509-mode))

(defvar x509--viewcrl-history nil "History list for x509-viewcrl.")

;;;###autoload
(defun x509-viewcrl (&optional args)
  "Parse current buffer as a CRL file. Display result in another buffer.

With \\[universal-argument] prefix, you can edit the command arguements."
  (interactive (x509--read-arguments "crl args: "
                                     (format "crl -text -noout -inform %s"
                                             (x509--buffer-encoding))
                                     'x509--viewcrl-history))
  (x509--process-buffer (split-string-and-unquote args))
  (x509-mode))

(defvar x509--viewdh-history nil "History list for x509-viewdh.")

;;;###autoload
(defun x509-viewdh (&optional args)
  "Parse current buffer as a DH-parameter file.
Display result in another buffer.

With \\[universal-argument] prefix, you can edit the command arguements."
  (interactive (x509--read-arguments "dhparam args: "
                                     (format "dhparam -text -noout -inform %s"
                                             (x509--buffer-encoding))
                                     'x509--viewdh-history))
  (x509--process-buffer (split-string-and-unquote args))
  (x509-mode))

(defvar x509--viewkey-history nil "History list for x509-viewkey.")

;; Special. older openssl pkey cannot read from stdin so we need to use
;; buffer's file.
;; FIXME: Create a temporary file with buffer content and use that as input to
;; pkey.
;;;###autoload
(defun x509-viewkey (&optional args)
  "Display x509 private key using the OpenSSL pkey command.

With \\[universal-argument] prefix, you can edit the command arguements.
For example to enter pass-phrase, add -passin pass:PASSPHRASE."
  (interactive (x509--read-arguments
                "pkey args: "
                (format "pkey -text -noout -inform %s -in \"%s\""
                        (x509--buffer-encoding) (buffer-file-name))
                'x509--viewkey-history))
  (x509--process-buffer (split-string-and-unquote args))
  (x509-mode))

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
   `(,x509--asn1-primitives-keywords . 'font-lock-builtin-face)
   ;; SET, SEQUENCE
   `(,x509--asn1-cons-keywords . 'font-lock-regexp-grouping-backslash)
   ;; cons: as in constructed. Same font as SET and SEQUENCE
   '("\\(cons\\):" (1 'font-lock-regexp-grouping-backslash))
   ;; Like SET and SEQUENCE
   '("\\(cont\\|\\appl\\|priv\\) \\[\\(.*?\\)\\]"
     (1 'font-lock-keyword-face)
     (2 'font-lock-regexp-grouping-backslash))
   ;; Parsing error messages
   '("error:.*\\|Error in encoding" . 'font-lock-warning-face)
   ;; String type + string value
   `(,x509--asn1-strings
     (1 'font-lock-builtin-face)
     (2 'font-lock-constant-face))
   ;; "OID" followed by oid
   `(,x509--asn1-oid
     (1 'font-lock-builtin-face)
     (2 'font-lock-function-name-face))
   "openssl asn1parse highligting"))

;;;###autoload
(define-derived-mode x509-asn1-mode fundamental-mode "asn1"
  "Major mode for displaying openssl asn1parse output.

\\{x509-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(x509-asn1-font-lock-keywords))
  (define-key x509-asn1-mode-map "q" 'x509-mode--kill-buffer))

(defvar x509--viewasn1-history nil "History list for x509-viewasn1.")

;;;###autoload
(defun x509-viewasn1 (&optional args)
  "Parse current buffer as ASN.1. Display result in another buffer.

With \\[universal-argument] prefix, you can edit the command arguements."
  (interactive (x509--read-arguments "asn1parse args: "
                                     (format "asn1parse -inform %s"
                                             (x509--buffer-encoding))
                                     'x509--viewasn1-history))
  (x509--process-buffer (split-string-and-unquote args))
  (x509-asn1-mode))

(provide 'x509-asn1-mode)
(provide 'x509-mode)

;;; x509-mode.el ends here
