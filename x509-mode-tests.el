;;; x509-mode-tests.el --- Tests for x509-mode  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2017-2023 Fredrik Axelsson <f.axelsson@gmail.com>

;; This file is not part of GNU Emacs.

;; MIT License
;;
;; Copyright (C) 2017-2023 Fredrik Axelsson <f.axelsson@gmail.com>
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

;;; Code:

(require 'ert)

(require 'x509-mode)

(defvar x509--test-history nil)

(defun x509--test-make-gmt-time (&optional offset-seconds)
  "Return a time string.

Ex: \"Wed Aug 17 08:48:06 2022 GMT\"
Offset current time with OFFSET-SECONDS is not nil."
  (let ((offset (or offset-seconds 0)))
    (format-time-string "%b %e %H:%M:%S %Y GMT"
                        (time-add (current-time) (seconds-to-time offset))
                        "GMT")))


(ert-deftest x509--date-in-the-past ()
  "Find date in buffer that is in the past."
  (with-temp-buffer
    (let ((old-date (x509--test-make-gmt-time -60)))
      (insert old-date)
      (goto-char (point-min))
      (should (x509--match-date-in-past nil))
      (should (string= (match-string-no-properties 0) old-date)))))

(ert-deftest x509--date-in-the-future ()
  "Find date in buffer that is in the future."
  (with-temp-buffer
    (let ((future-date (x509--test-make-gmt-time 60)))
      (insert future-date)
      (goto-char (point-min))
      (should (x509--match-date-in-future (point-max)))
      (should (string= (match-string-no-properties 0) future-date)))))

(ert-deftest x509--date-near-now ()
  "Find date in buffer that is near now.

Examine a date that is in the future within
`x509-warn-near-expire-days' of now."
  (with-temp-buffer
    (let* ((days-ahead (- x509-warn-near-expire-days 1))
           (seconds-ahead (* days-ahead 24 60 60))
           (near-future-date (x509--test-make-gmt-time seconds-ahead)))
      (insert near-future-date)
      (goto-char (point-min))
      (should (x509--match-date-near-now (point-max)))
      (should (string= (match-string-no-properties 0) near-future-date)))))

(ert-deftest x509--date-near-now-in-future ()
  "Ensure that a date in the near future is not matched."
  (with-temp-buffer
    (let* ((days-before 1)
           (seconds-before (* days-before 24 60 60))
           (near-past-date (x509--test-make-gmt-time (- seconds-before))))
      (insert near-past-date)
      (goto-char (point-min))
      (should-not (x509--match-date-near-now (point-max))))))

(ert-deftest x509--date-near-now-not-active ()
  "Ensure that dates in the near future is not matched.
When `x509-warn-near-expire-days' is nil."
  (with-temp-buffer
    (let* ((x509-warn-near-expire-days nil)
           (days-ahead 1)
           (seconds-ahead (* days-ahead 24 60 60))
           (near-future-date (x509--test-make-gmt-time seconds-ahead)))
      (insert near-future-date)
      (goto-char (point-min))
      (should-not (x509--match-date-near-now (point-max))))))

(ert-deftest x509--mark-browse-http-links ()
  "Verify that an URL is buttonized and has the expected properties."
  (with-temp-buffer
    (let ((https "https://xyz.com/abc.html")
          https-button
          (http "http://123/456")
          http-button
          (file "file://apa/banan")
          file-button)
      (insert "xxx" https " yyy " http " " file)
      (x509--mark-browse-http-links)
      (should (setq https-button (next-button (point-min))))
      (when https-button
        (should (string= https (button-get https-button 'url)))
        (should
         (string=
          (format "Click to browse-url %s" https)
          (button-get https-button 'help-echo)))
        (should (setq http-button (next-button (button-end https-button))))
        (when http-button
          (should (string= http (button-get http-button 'url)))
          (should
           (string=
            (format "Click to browse-url %s" http)
            (button-get http-button 'help-echo)))
          (should (setq file-button (next-button (button-end http-button))))
          (when file-button
            (should (string= file (button-get file-button 'url)))
            (should
             (string=
              (format "Click to browse-url %s" file)
              (button-get file-button 'help-echo)))))))))

(ert-deftest x509--mark-browse-oid ()
  "Verify that an OID is buttonized and has the expected properties."
  (with-temp-buffer
    (let* ((oid "1.2.898.22")
           (expected-url (format x509-query-oid-url-format oid))
           (too-short "1.2.3")
           button)
      (insert "xxx " too-short "  " oid " yyy")
      (x509--mark-browse-oid)
      (should (setq button (next-button (point-min))))
      (when button
        (should (string= expected-url (button-get button 'url)))
        (should
         (string=
          (format "Click to browse-url %s" expected-url)
          (button-get button 'help-echo)))))))

(ert-deftest x509--load-data-file ()
  "Turn file into list of strings."
  ;; If x509-mode was loaded from compiled, `x509--load-data-file' is not
  ;; defined. It's only used to define constants when compiling. If loaded from
  ;; source, then we can test it.
  (skip-unless (fboundp 'x509--load-data-file))
  (let* ((text "# comment 1\n  \ndata 1\n\n # comment 2\n2\n")
         (tmp-file (make-temp-file "data"))
         data)
    (with-temp-file tmp-file
      (insert text))
    (setq data (x509--load-data-file tmp-file))
    (delete-file tmp-file)
    (should (equal data '("data 1" "2")))))

(ert-deftest x509--buffer-encoding ()
  "Identify PEM encoding."
  (with-temp-buffer
    (insert "not pem ")
    (should (equal "DER" (x509--buffer-encoding (current-buffer)))))
  (with-temp-buffer
    (insert "-----BEGIN ")
    (should (equal "PEM" (x509--buffer-encoding (current-buffer))))))

(ert-deftest x509--pem-region ()
  "Find region delimited by BEGIN/END."
  (with-temp-buffer
    ;;                      22                            53
    ;;                      v                             v
    (insert
     "-----BEGIN PKCS7----- -----END XX-----END PKCS7----- -----END PKCS7-----")
    (goto-char (point-min))
    (let ((region (x509--pem-region)))
      (should region)
      (should (equal 1 (car region)))
      (should (equal 53 (cdr region))))
    (goto-char 22)
    (let ((region (x509--pem-region)))
      (should region)
      (should (equal 1 (car region)))
      (should (equal 53 (cdr region))))))

(ert-deftest x509--pem-region-negative ()
  "Behave when there is no region."
  (with-temp-buffer
    (insert "-----BEGIN PKCS7----- -----END XZY-----")
    (goto-char (point-min))
    (should-not (x509--pem-region)))
  (with-temp-buffer
    (insert "-----END PKCS7-----")
    (goto-char (point-min))
    (should-not (x509--pem-region))))

(ert-deftest x509--pem-region-known-types ()
  (dolist (type x509--known-pki-types)
    (with-temp-buffer
      (insert (format "-----BEGIN %s----- -----END %s-----" type type))
      (goto-char (point-min))
      (should (x509--pem-region)))))

(ert-deftest x509--pem-region-type ()
  "Get type of region, e.g. \"CERTIFICATE\"."
  (with-temp-buffer
    (insert "-----BEGIN PKCS7----- -----END PKCS7-----")
    (goto-char (point-min))
    (should (equal "PKCS7" (x509--pem-region-type))))
  (with-temp-buffer
    (insert "-----END TYPE-----")
    (goto-char (point-min))
    (should-not (x509--pem-region-type))))

(ert-deftest x509--pem-region-next/prev ()
  (with-temp-buffer
    (insert
     ;; This is the initial valid region
     "-----BEGIN PKCS7-----\n-----END PKCS7-----\n"
     ;; This region with unknown type should be ignored.
     "-----BEGIN xxxxx-----\n-----END xxxxx-----\n"
     ;; This is the next valid region
     "-----BEGIN PKCS7-----\n-----END PKCS7-----\n")
    ;;^ point after next
    (goto-char (point-min))
    (let ((first-region (x509--pem-region))
          (expected-next-point 85))
      (should first-region)
      (should (= 1 (car first-region)))
      (let ((next-region (x509--pem-region-next/prev (current-buffer) 'next)))
        (should next-region)
        (should (= expected-next-point (car next-region)))
        (should (= expected-next-point (point)))
        ;; Next again should leave point unchanged
        (should-not (x509--pem-region-next/prev (current-buffer) 'next))
        (should (= expected-next-point (point))))
      ;; Prev 1
      (let ((prev-region (x509--pem-region-next/prev (current-buffer) 'prev)))
        (should prev-region)
        (should (= 1 (car prev-region)))
        (should (= 1 (point)))
        ;; Prev again should leave point unchanged
        (should-not (x509--pem-region-next/prev (current-buffer) 'prev))
        (should (= 1 (point)))))))

(ert-deftest x509--generate-input-buffer ()
  "Create buffer with valid data."
  ;; PEM region                llllllllllllllllllll20
  (with-temp-buffer
    (let ((q-pem-data
           (concat
            "const char* pem = \"-----BEGIN PKCS7-----\"\n"
            "                  \"data\"\n"
            "                  \"-----END PKCS7-----\";\n"))
          (clean-pem-data "-----BEGIN PKCS7-----\ndata\n-----END PKCS7-----"))
      (insert q-pem-data)
      (goto-char 20)
      (let ((buf (x509--generate-input-buffer)))
        (unwind-protect
            (progn
              (should buf)
              (with-current-buffer buf
                (should (equal clean-pem-data (buffer-string)))))
          (kill-buffer buf)))))
  ;; Not PEM data. Use whole buffer.
  (with-temp-buffer
    (let ((q-pem-data "somedata"))
      (insert q-pem-data)
      (goto-char 0)
      (let ((buf (x509--generate-input-buffer)))
        (unwind-protect
            (progn
              (should buf)
              (with-current-buffer buf
                (should (equal q-pem-data (buffer-string)))))
          (kill-buffer buf))))))

(defvar x509--test-pem-crl
  (concat
   "-----BEGIN X509 CRL-----\n"
   "MIIBqzCBlAIBATANBgkqhkiG9w0BAQsFADA8MRowGAYDVQQDDBFDQSBUZXN0VG9v\n"
   "bCBDQSAwMTEeMBwGA1UECgwVQ0EgVGVzdFRvb2wgQXV0aG9yaXR5Fw0yMjA2MTEw\n"
   "NjA5NTZaGA8yMDcyMDUyOTA2MDk1NlowIjAgAgECFw0yMjA2MTEwNjA5MzlaMAww\n"
   "CgYDVR0VBAMKAQAwDQYJKoZIhvcNAQELBQADggEBAAeCzW8Qjb9T0Y7GUygw+J28\n"
   "HH6LLprpUsxf9YuHXtrq6IcsM0pxtZIw0nRb/X56u8vjOXnjlDPEDEIwaNnhI+VR\n"
   "uzE2Caq4Xt7lilCtviZPyJXtceZ5SOh0pkZYrccSWln2+JuBDh+f3O2dOjxl5yZl\n"
   "XYcjYinabHXoRdhMG5pTC0X0TMCnl+Q1EsrFDNNwmDaXszR5MU7I/X3mgC3ulp8e\n"
   "j/bRF+3Y36I5ELBjTTj7A8Kd/WvafJzoe6fHUbo5uytY4ztXzmkiZXIdcOKsCvsd\n"
   "VBa4KOgROKhpFOIHhlXNV97Pl++QslL0gho9Rc0+NbKNlXyz1EyaDXMR5cXc/Ak=\n"
   "-----END X509 CRL-----")
  "Sample PEM encoded CRL for testing.")

(ert-deftest x509--process-buffer ()
  "Test OpenSSL process buffer."
  (let (shadow-buffer)
    (with-temp-buffer
      (insert x509--test-pem-crl)
      (goto-char (point-min))
      (let ((result-buffer
             (x509--process-buffer
              (current-buffer) '("crl" "-text" "-noout"))))
        (should result-buffer)
        (with-current-buffer result-buffer
          (should
           (re-search-forward "Certificate Revocation List (CRL):" nil t))
          (should (string-match-p "^\\*x-.*\\*" (buffer-name)))
          (should (boundp 'x509--shadow-buffer))
          (should (bufferp x509--shadow-buffer))
          (should (buffer-live-p x509--shadow-buffer))
          (setq shadow-buffer x509--shadow-buffer)
          (kill-buffer result-buffer)
          ;; Shadow buffer should be killed in hook
          (should-not (buffer-live-p shadow-buffer)))))))

(ert-deftest x509--read-arguments ()
  "Ensure that argument is added to history."
  (let ((args "my args"))
    (setq x509--test-history nil)
    (should
     (equal args (x509--read-arguments "PROMPT" args 'x509--test-history)))
    (should (equal x509--test-history (list args)))))

(ert-deftest x509--generic-view ()
  "Test creating a view buffer."
  (let ((expanded-args (concat x509-crl-default-arg " -inform PEM")))
    (setq x509--test-history nil)
    (with-temp-buffer
      (insert x509--test-pem-crl)
      (let ((result-buffer
             (x509--generic-view
              x509-crl-default-arg 'x509--test-history 'x509-mode)))
        (unwind-protect
            (with-current-buffer result-buffer
              (progn
                (should (derived-mode-p 'x509-mode))
                ;; Buffer encoding is added to the arguments which is added to the
                ;; x509--test-history.
                (should (equal x509--test-history (list expanded-args)))
                (should (boundp 'x509--x509-mode-shadow-arguments))
                (should
                 (equal x509--x509-mode-shadow-arguments expanded-args))))
          (kill-buffer result-buffer))))))

(ert-deftest x509--generic-view-asn1 ()
  "Test creating a view buffer for `x509-asn1-mode'."
  (let ((expanded-args (concat x509-asn1parse-default-arg " -inform PEM")))
    (setq x509--test-history nil)
    (with-temp-buffer
      (insert x509--test-pem-crl)
      (let ((result-buffer
             (x509--generic-view
              x509-asn1parse-default-arg 'x509--test-history 'x509-asn1-mode)))
        (unwind-protect
            (with-current-buffer result-buffer
              (should (derived-mode-p 'x509-asn1-mode))
              ;; Buffer encoding is added to the arguments which is added to the
              ;; x509--test-history.
              (should (equal x509--test-history (list expanded-args)))
              (should (boundp 'x509--x509-asn1-mode-shadow-arguments))
              (should
               (equal x509--x509-asn1-mode-shadow-arguments expanded-args)))
          (kill-buffer result-buffer))))))

(ert-deftest x509--get-x509-history ()
  "Verify that all commands return expected history variables."
  (should (equal 'x509--viewcert-history (x509--get-x509-history "x509")))
  (should (equal 'x509--viewreq-history (x509--get-x509-history "req")))
  (should (equal 'x509--viewcrl-history (x509--get-x509-history "crl")))
  (should (equal 'x509--viewpkcs7-history (x509--get-x509-history "pkcs7")))
  (should (equal 'x509--viewdh-history (x509--get-x509-history "dhparam")))
  (should (equal 'x509--viewec-history (x509--get-x509-history "ecparam")))
  (should (equal 'x509--viewkey-history (x509--get-x509-history "pkey")))
  (should
   (equal 'x509--viewpublickey-history (x509--get-x509-history "pkey -pubin")))
  (should (equal 'x509--viewasn1-history (x509--get-x509-history "asn1parse")))
  (should (equal nil (x509--get-x509-history "UNKNOWN"))))


(defun find-testfile (file-name)
  "Find FILE-NAME in testfiles."
  (expand-file-name file-name "testfiles"))

(defun view-test-helper
    (test-files
     view-command expected-mode expected-strings &optional anti-strings)
  "Run VIEW-COMMAND on all TEST-FILES.
Check that buffer has EXPECTED-MODE and contains EXPECTED-STRINGS.
Check that buffer does _not_ contain ANTI-STRINGS.
Repeat with `x509-dwim' which should produce the same result."
  (let ((regexes
         (if (listp expected-strings)
             expected-strings
           (list expected-strings)))
        (anti-regexes
         (if (listp anti-strings)
             anti-strings
           (list anti-strings)))
        (files
         (if (listp test-files)
             test-files
           (list test-files))))
    (dolist (test-file files)
      (dolist (view-func (list view-command 'x509-dwim))
        (with-temp-buffer
          (insert-file-contents-literally (find-testfile test-file))
          (let ((view-buffer (funcall view-func)))
            (unwind-protect
                (with-current-buffer view-buffer
                  (let ((content
                         (buffer-substring-no-properties
                          (point-min) (point-max))))
                    (should (derived-mode-p expected-mode))
                    (dolist (regex regexes)
                      (should (string-match-p regex content)))
                    (dolist (regex anti-regexes)
                      (should-not (string-match-p regex content)))))
              (kill-buffer view-buffer))))))))

(ert-deftest x509-viewcert ()
  "View cert."
  (view-test-helper
   '("CA/pki/crt/jobbflykt.crt" "CA/pki/crt/jobbflykt.cer")
   'x509-viewcert
   'x509-mode
   "Certificate:"
   "Warning"))

(ert-deftest x509-viewreq ()
  "View cert request."
  (view-test-helper
   '("CA/ca/request/jobbflykt.pem"
     "CA/ca/request/jobbflykt_req.der")
   'x509-viewreq 'x509-mode "Certificate Request:" "Warning"))

(ert-deftest x509-viewcrl ()
  "View CRL."
  (view-test-helper
   '("CA/pki/crl/ca_testtool_ca_01.crl" "CA/pki/crl/ca_testtool_ca_01.crl.pem")
   'x509-viewcrl
   'x509-mode
   "Certificate Revocation List (CRL):"))

(ert-deftest x509-viewpkcs7 ()
  "View p7."
  (view-test-helper
   '("CA/pki/pkcs7/jobbflykt.p7b" "CA/pki/pkcs7/jobbflykt.p7b.der")
   'x509-viewpkcs7
   'x509-mode
   '("Certificate:" "Certificate Revocation List (CRL):")))

(ert-deftest x509-viewdh ()
  "View Diffie-Hellman parameters."
  (view-test-helper
   '("dhparams-4096.pem"
     ;; Special: there is no telling if this is a public key
     ;; with modulus and exponent or if it is diffie-hellman
     ;; parameters P and G. So when "guessing", both options are
     ;; valid and x509-dwim can get you one or the other.
     ;;"dhparams-4096.der"
     )
   'x509-viewdh 'x509-mode "DH Parameters: (4096 bit)"))

(ert-deftest x509-viewec ()
  "View elliptic curve parameters."
  (view-test-helper
   '("ec-params.pem") 'x509-viewec 'x509-mode "EC-Parameters: (512 bit)"))

(ert-deftest x509-viewkey ()
  "View plaintext private key."
  (view-test-helper
   '("CA/pki/key/jobbflykt.key" "CA/pki/key/jobbflykt.key.der")
   'x509-viewkey
   'x509-mode
   "publicExponent: 65537 (0x10001)"))

(ert-deftest x509-viewpublickey ()
  "View public key."
  (view-test-helper
   '("CA/pki/key/jobbflykt-public.key"
     ;; See comment about public keys and diffie-hellman
     ;; parameter mixup in `x509-viewdh'
     ;;"CA/pki/key/jobbflykt-public.key.der"
     )
   'x509-viewpublickey 'x509-mode "Public-Key: (2048 bit)"))

(ert-deftest x509-viewasn1 ()
  "View ASN.1."
  (view-test-helper
   "not-pki.der"
   'x509-viewasn1
   'x509-asn1-mode
   "UTF8STRING        :Hello x509-mode"))

(ert-deftest x509-hexl ()
  "Open hexl buffer from `x509-asn1-mode'."
  (with-temp-buffer
    (insert-file-contents-literally (find-testfile "inf.der"))
    (let ((view-buffer (x509-viewasn1)))
      (unwind-protect
          (with-current-buffer view-buffer
            (x509-asn1-toggle-hexl)
            (should (boundp 'x509-asn1--hexl-buffer))
            (should (bufferp x509-asn1--hexl-buffer))
            (with-current-buffer x509-asn1--hexl-buffer
              (let ((hexl-str
                     (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-match-p "00000000: 3080" hexl-str))))
            (let ((hexl-buffer x509-asn1--hexl-buffer))
              (kill-buffer view-buffer)
              ;; hexl buffer should be killed when view buffer is.
              (should-not (buffer-live-p hexl-buffer))))
        (kill-buffer view-buffer)))))

(ert-deftest x509-hexl-from-pem-and-der ()
  "Open hexl buffer from both PEM and DER version of cert.
Hexl buffer content should be identical."
  (let (der-hexl-content
        pem-hexl-content)
    (with-temp-buffer
      (insert-file-contents-literally (find-testfile "oid_guy.cer"))
      (let ((der-ans1-buff (x509-viewasn1)))
        (unwind-protect
            (with-current-buffer der-ans1-buff
              (x509-asn1-toggle-hexl)
              (with-current-buffer x509-asn1--hexl-buffer
                (setq der-hexl-content
                      (buffer-substring-no-properties
                       (point-min) (point-max)))))
          (kill-buffer der-ans1-buff))))
    (with-temp-buffer
      (insert-file-contents-literally (find-testfile "oid_guy.crt"))
      (let ((pem-ans1-buff (x509-viewasn1)))
        (unwind-protect
            (with-current-buffer pem-ans1-buff
              (x509-asn1-toggle-hexl)
              (with-current-buffer x509-asn1--hexl-buffer
                (setq pem-hexl-content
                      (buffer-substring-no-properties
                       (point-min) (point-max)))))
          (kill-buffer pem-ans1-buff))))
    (should (string= der-hexl-content pem-hexl-content))))

(defun check-face-helper (regex expected-face &optional match)
  "Check that face at `match-beginning' MATCH matches EXPECTED-FACE.
Search for REGEX.  If MATCH is nil, look at beginning of whole regexp."
  (goto-char (point-min))
  (should (re-search-forward regex nil t))
  (let ((point
         (match-beginning
          (if match
              match
            0))))
    (should (eq (get-char-property point 'face) expected-face))))

(ert-deftest x509-viewasn1-faces ()
  "Check a few font lock faces in asn1mode buffer."
  (with-temp-buffer
    (insert-file-contents-literally (find-testfile "inf.der"))
    (let* ((x509-asn1-mode-hook nil)
           (result-buffer (x509-viewasn1)))
      (unwind-protect
          (with-current-buffer result-buffer
            (goto-char (point-min))
            (if (fboundp 'font-lock-ensure)
                (font-lock-ensure)
              (with-no-warnings (font-lock-fontify-buffer)))
            (check-face-helper "=\\(inf\\)" 'x509-constant-face 1)
            (check-face-helper "cons" 'x509-asn1-sequence-face)
            (check-face-helper "SEQUENCE" 'x509-asn1-sequence-face)
            (check-face-helper "prim" 'x509-keyword-face)
            (check-face-helper "INTEGER" 'x509-keyword-face)
            (check-face-helper "EOC" 'x509-keyword-face))
        (kill-buffer result-buffer)))))

(ert-deftest x509--asn1-update-command-line-start-arg ()
  "Test add, update and remove -offset N argument."
  ;; Zero offset. Don't add.
  (should
   (string= (x509--asn1-update-command-line-start-arg "x" "-offset" 0) "x"))
  ;; Zero offset. Remove
  (should
   (string=
    (x509--asn1-update-command-line-start-arg
     "x -offset 10 z" "-offset" 0)
    "x z"))
  ;; Add offset
  (should
   (string=
    (x509--asn1-update-command-line-start-arg
     "x" "-strparse" 17)
    "x -strparse 17"))
  ;; Update offset
  (should
   (string=
    (x509--asn1-update-command-line-start-arg
     "x -offset 989 y" "-strparse" 17)
    "x -strparse 17 y")))

(ert-deftest x509--asn1-down-up ()
  "Verify that going down and up in nested ASN.1 structures works.
nested.der should contain:
    0:d=0  hl=2 l=   5 cons: SEQUENCE
    2:d=1  hl=2 l=   3 cons: SEQUENCE
    4:d=2  hl=2 l=   1 prim: INTEGER           :-06"
  (with-temp-buffer
    (insert-file-contents-literally (find-testfile "nested.der"))
    (let ((result-buffer (x509-viewasn1)))
      (unwind-protect
          (with-current-buffer result-buffer
            (x509-asn1-offset-down)
            (should
             (equal x509--x509-asn1-mode-offset-stack '(("-offset" 2 2 1))))
            ;; Move point and verify it's restored when going up later
            (forward-char 4)
            (x509-asn1-offset-down)
            (should
             (equal
              x509--x509-asn1-mode-offset-stack
              '(("-offset" 4 2 5) ("-offset" 2 2 1))))
            (should
             (looking-at
              "    0:d=0  hl=2 l=   1 prim: INTEGER           :-06"))
            (x509-asn1-offset-up)
            (should
             (equal x509--x509-asn1-mode-offset-stack '(("-offset" 2 2 1))))
            (should (equal (point) 5))
            (x509-asn1-offset-up)
            (should (null x509--x509-asn1-mode-offset-stack))
            ;; Going up from top does nothing
            (x509-asn1-offset-up)
            (should (looking-at "    0:d=0  hl=2 l=   5 cons: SEQUENCE")))
        (kill-buffer result-buffer)))))

(ert-deftest x509-asn1-strparse ()
  "Verify that going down and up in nested ASN.1 structures works.
nested_bitstrings.bin should contain:
SEQUENCE             30 0C
                     2
  BITSTRING          03 0A 00
                     5
    BITSTRING        03 07 00
                     8
      BITSTRING      03 04 00
                     11
        INTEGER FA   02 01 FA"
  (with-temp-buffer
    (insert-file-contents-literally (find-testfile "nested_bitstrings.bin"))
    (let ((result-buffer (x509-viewasn1)))
      (unwind-protect
          (with-current-buffer result-buffer
            (forward-line 1)
            (x509-asn1-strparse)
            (should
             (equal x509--x509-asn1-mode-offset-stack '(("-strparse" 2 3 49))))
            ;; Move point and verify it's restored when going up later
            (forward-char 4)
            (x509-asn1-strparse)
            (should
             (equal
              x509--x509-asn1-mode-offset-stack
              '(("-strparse" 5 3 5) ("-strparse" 2 3 49))))
            (should
             (looking-at "    0:d=0  hl=2 l=   4 prim: BIT STRING        "))
            (x509-asn1-offset-up)
            (should
             (equal x509--x509-asn1-mode-offset-stack '(("-strparse" 2 3 49))))
            (should (equal (point) 5))
            (x509-asn1-offset-up)
            (should (null x509--x509-asn1-mode-offset-stack))
            ;; Going up from top does nothing
            (x509-asn1-offset-up)
            (should (looking-at "    2:d=1  hl=2 l=  10 prim: BIT STRING")))
        (kill-buffer result-buffer)))))

(defun check-content-helper (buffer expected-string)
  "Check that BUFFER contain the regex EXPECTED-STRING."
  (with-current-buffer buffer
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p expected-string content)))))

(ert-deftest x509-dwim-next/prev ()
  "Go forward and backward."
  (with-temp-buffer
    (insert-file-contents-literally (find-testfile "multi.pem"))
    (goto-char (point-min))
    (let ((view-buffer (x509-dwim)))
      (should view-buffer)
      (check-content-helper view-buffer "Certificate:")
      (with-current-buffer view-buffer
        (setq view-buffer (x509-dwim-next)))
      (should view-buffer)
      (check-content-helper view-buffer "Certificate Request:")
      (with-current-buffer view-buffer
        (setq view-buffer (x509-dwim-next)))
      (should view-buffer)
      (check-content-helper view-buffer "DH Parameters:")
      (with-current-buffer view-buffer
        (setq view-buffer (x509-dwim-next)))
      (should view-buffer)
      (check-content-helper view-buffer "Public-Key:")
      (with-current-buffer view-buffer
        (setq view-buffer (x509-dwim-next)))
      (should view-buffer)
      (check-content-helper view-buffer "EC-Parameters: (512 bit)")
      ;; At end, next should fail
      (with-current-buffer view-buffer
        (should-not (x509-dwim-next)))
      (check-content-helper view-buffer "EC-Parameters: (512 bit)")
      ;; Go backward
      (with-current-buffer view-buffer
        (setq view-buffer (x509-dwim-prev)))
      (should view-buffer)
      (check-content-helper view-buffer "Public-Key:")
      (with-current-buffer view-buffer
        (setq view-buffer (x509-dwim-prev)))
      (should view-buffer)
      (check-content-helper view-buffer "DH Parameters:")
      (with-current-buffer view-buffer
        (setq view-buffer (x509-dwim-prev)))
      (should view-buffer)
      (check-content-helper view-buffer "Certificate Request:")
      (with-current-buffer view-buffer
        (setq view-buffer (x509-dwim-prev)))
      (should view-buffer)
      (check-content-helper view-buffer "Certificate:")
      ;; At beginning, prev should fail
      (with-current-buffer view-buffer
        (should-not (x509-dwim-prev)))
      (check-content-helper view-buffer "Certificate:")
      ;; Kill it
      (with-current-buffer view-buffer
        (x509-mode-kill-buffer)))))

(ert-deftest x509-dwim-next/prev-asn1 ()
  "Go forward and backward in `x509-asn1-mode'."
  (with-temp-buffer
    (insert-file-contents-literally (find-testfile "multi.pem"))
    (goto-char (point-min))
    (let ((view-buffer (x509-viewasn1)))
      (should view-buffer)
      (check-content-helper view-buffer ":033AF1E6A711A9A0BB2864B11D09FAE5")
      ;; Go next and verify we are still in asn1 mode looking at the next
      ;; section.
      (with-current-buffer view-buffer
        (setq view-buffer (x509-dwim-next)))
      (should view-buffer)
      (check-content-helper view-buffer ":Extension Request")
      ;; Go back again
      (with-current-buffer view-buffer
        (setq view-buffer (x509-dwim-prev)))
      (should view-buffer)
      (check-content-helper view-buffer ":033AF1E6A711A9A0BB2864B11D09FAE5")
      ;; Kill it
      (with-current-buffer view-buffer
        (x509-mode-kill-buffer)))))

(ert-deftest x509-swoop ()
  "Multiple dwim in all regions in buffer."
  (let ((x509-swoop-separator "7iyefiaeo7bf")))
  (with-temp-buffer
    (insert-file-contents-literally (find-testfile "multi.pem"))
    (let ((swoop-buffer (x509-swoop)))
      (should swoop-buffer)
      (with-current-buffer swoop-buffer
        (check-content-helper swoop-buffer "Certificate:")
        (check-content-helper swoop-buffer "Certificate Request:")
        (check-content-helper swoop-buffer "DH Parameters:")
        (check-content-helper swoop-buffer "Public-Key:")
        (check-content-helper swoop-buffer "EC-Parameters: (512 bit)")
        (check-content-helper swoop-buffer x509-swoop-separator)
        (x509-mode-kill-buffer))))
  (with-temp-buffer
    (insert "nothing here")
    (should-not (x509-swoop))))

(provide 'x509-mode-tests)
;;; x509-mode-tests.el ends here
