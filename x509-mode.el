;;; x509-mode.el --- Decode certificates, CRLs and keys using OpenSSL -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Fredrik Axelsson <f.axelsson@gmai.com>

;; Author: Fredrik Axelsson <f.axelsson@gmai.com>
;; Version: 0.1
;; Package-Version:
;; Package-Requires: ((emacs "24.1"))

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode for viewing certificates, CRLs, keys and DH-parameters.

;; Uses OpenSSL for viewing PEM and DER encoded PKI entities.

;; While in a buffer displaying a certificate, use M-x x509-viewcert to create
;; a new buffer that displays the decoded certificate.  Use M-x x509-viewcrl,
;; M-X x509-viewasn1, M-x x509-viewkey M-x x509-viewdh in a similar manner.

;;; Code:

(defgroup x509-mode nil
  "View certificates, CRLs, keys and DH-parameters using OpenSSL"
  :group 'extensions
  :group 'convenience
  :link '(emacs-library-link :tag "Lisp File" "x509-mode.el"))

(defcustom x509-openssl-cmd "openssl"
  "Path to OpenSSL binary.
For example \"/usr/bin/openssl\" or \"C:/Program Files/Git/mingw64/bin/openssl\""
  :type 'string
  :group 'x509-mode)

(defgroup x509-mode-faces nil
  "Faces used by x509."
  :group 'x509-mode
  :group 'faces)

(defface x509-hex-string-face
  '((t (:inherit font-lock-comment-face :italic t)))
  "Face for colon-separated hex values."
  :group 'x509-mode-faces)

(defface x509-oid-face
  '((t (:inherit font-lock-constant-face :bold t)))
  "Face for unknown OIDs."
  :group 'x509-mode-faces)

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

(defconst x509--keywords
  (regexp-opt
   '("Authority Information Access:" "CA Issuers" "CA:" "CPS:"
     "CRL entry extensions" "CRL extensions"
     "Certificate Revocation List (CRL)" "Certificate:" "Data:" "DirName:"
     "Explicit Text:" "Exponent:" "Full Name:" "Issuer:" "Last Update"
     "Modulus:" "Next Update" "OCSP" "Public-Key:" "Revocation Date"
     "No Revoked Certificates" "Revoked Certificates"
     "Salt Length:" "Serial Number:"
     "Subject Public Key Info:" "Subject:" "Trailer Field:" "Trusted Uses"
     "User Notice:" "Version:" "X509v3 Authority Key Identifier:"
     "X509v3 CRL Distribution Points:" "X509v3 CRL Number:"
     "X509v3 CRL Reason Code:" "X509v3 Certificate Policies:"
     "X509v3 Extended Key Usage:" "X509v3 Subject Alternative Name:"
     "X509v3 Subject Key Identifier:" "X509v3 extensions:" "keyid:"
     "pathlen:" "pub:" "serial:"
     ;; openssl pkey
     "Private-Key:" "modulus:" "publicExponent:" "privateExponent:"
     "prime1:" "prime2:" "exponent1:" "exponent2:" "coefficient:"
     ;; openssl dhparam
     "PKCS#3 DH Parameters:" "prime:" "generator:")))

(defconst x509--constants
  (regexp-opt
   '("TRUE" "FALSE"
     ;; openssl-1.0.2/crypto/objects/objects.h
     "Code Signing" "E-mail Protection" "Invalidity Date"
     "Microsoft Commercial Code Signing" "Microsoft Encrypted File System"
     "Microsoft Individual Code Signing" "Microsoft Server Gated Crypto"
     "Microsoft Smartcardlogin" "Microsoft Trust List Signing"
     "Netscape Server Gated Crypto" "Strong Extranet ID"
     "TLS Web Client Authentication" "TLS Web Server Authentication"
     "Time Stamping" "X509v3 Delta CRL Indicator"
     ;; /openssl-1.0.2/crypto/x509v3/v3_bitst.c
     "Digital Signature" "Non Repudiation" "Key Encipherment"
     "Data Encipherment" "Key Agreement" "Certificate Sign" "CRL Sign"
     "Encipher Only" "Decipher Only"
     ;; CRL X509v3 reason code:
     "Superseded" "Affiliation Changed" "Cessation Of Operation")
   'words))

;; Keyword: constant
;; E.g. "Signature Algorithm: sha1WithRSAEncryption"
(defconst x509--keyword-w-constant
      (concat (regexp-opt
               ;; Keyword:
               '("ASN1 OID" "Alias" "Hash Algorithm" "Mask Algorithm" "Policy"
                 "Public Key Algorithm" "Signature Algorithm"
                 "X509v3 Basic Constraints" "X509v3 Key Usage")
               t )  ; t = enclose in \\( \\) for easy subexpr reference
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
   '("\\(\\<\\w+=\\)\\(.*?\\)\\(:?[,/]\\|$\\)"
     (1 'bold)
     (2 'font-lock-string-face))

   ;; something = string until ',' or EOL
   ;; E.g. CN = ACCVRAIZ1, OU = PKIACCV, O = ACCV, C = ES
   '("\\(\\<\\w+\\) = \\(.*?\\)\\(:?[,/]\\|$\\)"
     (1 'bold)
     (2 'font-lock-string-face))


   ;; URI:string, email:string CPS: string
   '("\\<\\(URI:\\|email:\\|CPS: \\)\\(.*\\)"
     (1 'font-lock-builtin-face)
     (2 'link))

   ;; Not Before: Jun 11 00:00:01 2014 GMT
   ;; Date is "MATCH-ANCHORED", see help for variable font-lock-keywords
   '("\\(Not Before\\): " (1 'font-lock-builtin-face)
     (x509--match-date-in-future nil nil (0 'font-lock-warning-face)))
   '("\\(Not After\\) : " (1 'font-lock-builtin-face)
     (x509--match-date-in-past nil nil (0 'font-lock-warning-face)))

   ;; Policy: OID
   ;; Has precedence over Keyword: constant below
   '("\\(Policy\\): \\([0-9]+\\.[0-9]+\\(:?\\.[0-9]+\\)*\\)"
     (1 'font-lock-builtin-face)
     (2 'x509-oid-face))

   ;; E.g. Public Key Algorithm: rsaEncryption
   `(,x509--keyword-w-constant
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
  (define-key x509-mode-map "q" 'x509-mode--kill-buffer))

(defun x509--buffer-encoding()
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (if (search-forward "-----BEGIN" nil t)
          "PEM"
        "DER"))))

(defun x509--process-buffer(&rest openssl-arguments)
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

;;;###autoload
(defun x509-viewcert ()
  "Parse current buffer as a certificate file and display result in another buffer."
  (interactive)
  (x509--process-buffer "x509" "-text" "-noout"
                        "-inform" (x509--buffer-encoding))
  (x509-mode))

;;;###autoload
(defun x509-viewcrl ()
  "Parse current buffer as a CRL file and display result in another buffer."
  (interactive)
  (x509--process-buffer "crl" "-text" "-noout"
                        "-inform" (x509--buffer-encoding))
  (x509-mode))

;;;###autoload
(defun x509-viewdh ()
  "Parse current buffer as a DH-parameter file and display result in another buffer."
  (interactive)
  (x509--process-buffer "dhparam" "-text" "-noout"
                        "-inform" (x509--buffer-encoding))
  (x509-mode))

;; Special. pkey cannot read from stdin so we need to use buffer's file.
;; FIXME: Create a temporary file with buffer content and use that as input to
;; pkey.
;;;###autoload
(defun x509-viewkey (&optional passphrase)
  "Display x509 private key using the OpenSSL pkey command.
With C-u prefix, you can set the pass-phrase as -passin pass:PASSPHRASE."
  (interactive
   (if (equal current-prefix-arg '(4)) ; C-u
       (list (read-from-minibuffer "Passphrase: "))))
  (let* ((buf (generate-new-buffer (generate-new-buffer-name
                                    (format "*x-%s*" (buffer-name)))))
         (args (append (list (point-min) (point-max)
                             x509-openssl-cmd
                             nil buf nil
                             "pkey" "-text" "-noout" "-inform"
                             (x509--buffer-encoding))
                       (if (not (null passphrase))
                           (list "-passin" (format "pass:%s" passphrase)))
                       (list (buffer-file-name)))))
    (apply 'call-process-region args)
    (switch-to-buffer buf)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (x509-mode)))

;; ----------------------------------------------------------------------------
;; asn1-mode

(defconst x509--asn1-primitives-keywords
  (regexp-opt '("prim" "EOC" "BOOLEAN" "INTEGER" "BIT_STRING" "BIT STRING"
                "OCTET_STRING" "OCTET STRING" "NULL" "OID"
                "UTCTIME" "GENERALIZEDTIME")))

(defconst x509--asn1-cons-keywords
  (regexp-opt '("cons" "SEQUENCE" "SET")))

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
   ;; SET, SEQUENCE and such
   `(,x509--asn1-cons-keywords . 'font-lock-regexp-grouping-backslash)
   ;; Like SET and SEQUENCE
   '("cont \\[.*?\\]" . 'font-lock-regexp-grouping-backslash)
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

;;;###autoload
(defun x509-viewasn1 ()
  "Decode as ASN.1 and display result in another buffer."
  (interactive)
  (x509--process-buffer "asn1parse" "-inform" (x509--buffer-encoding))
  (x509-asn1-mode))

(provide 'x509-asn1-mode)
(provide 'x509-mode)

;;; x509-mode.el ends here
