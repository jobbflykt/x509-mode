;;; x509-mode-tests.el --- Tests for x509-mode -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2022 Fredrik Axelsson <f.axelsson@gmail.com>

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

;;; Code:

(require 'ert)

(require 'x509-mode)

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
    (let ((old-date (x509--test-make-gmt-time -60))
          (future-date (x509--test-make-gmt-time 60)))
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

(ert-deftest x509--load-data-file()
  "Turn file into list of strings."
  (let* ((text (concat "# comment 1\n"
                       "  \n"
                       "data 1\n"
                       "\n"
                       " # comment 2\n"
                       "2\n"))
         (tmp-file (make-temp-file "data"))
         data)
    (with-temp-file tmp-file
      (insert text))
    (setq data (x509--load-data-file tmp-file))
    (delete-file tmp-file)
    (should (equal data '("data 1" "2")))))

(ert-deftest x509--buffer-encoding()
  "Identify PEM encoding."
  (with-temp-buffer
    (insert "not pem ")
    (should (equal "DER" (x509--buffer-encoding (current-buffer)))))
  (with-temp-buffer
    (insert "-----BEGIN ")
    (should (equal "PEM" (x509--buffer-encoding (current-buffer))))))

(ert-deftest x509--pem-region()
  "Find region delimited by BEGIN/END."
  (with-temp-buffer
    ;;       1                                                    54
    ;;       v                                                    v
    (insert "-----BEGIN TYPE----- -----END BOGUS-----END TYPE----- -----END TYPE-----")
    (goto-char (point-min))
    (let ((region (x509--pem-region)))
      (should region)
      (should (equal 1 (car region)))
      (should (equal 54 (cdr region))))
    (goto-char 22)
    (let ((region (x509--pem-region)))
      (should region)
      (should (equal 1 (car region)))
      (should (equal 54 (cdr region))))))

(ert-deftest x509--pem-region-negative()
  "Behave when there is no region."
  (with-temp-buffer
    (insert "-----BEGIN TYPE----- -----END BOGUS-----")
    (goto-char (point-min))
    (should-not (x509--pem-region)))
  (with-temp-buffer
    (insert "-----END TYPE-----")
    (goto-char (point-min))
    (should-not (x509--pem-region))))

(ert-deftest x509--pem-region-type()
  "Get type of region, e.g. \"CERTIFICATE\"."
  (with-temp-buffer
    (insert "-----BEGIN my type----- -----END my type-----")
    (goto-char (point-min))
    (should (equal "my type" (x509--pem-region-type))))
  (with-temp-buffer
    (insert "-----END TYPE-----")
    (goto-char (point-min))
    (should-not (x509--pem-region-type))))

(ert-deftest x509--generate-input-buffer ()
  "Create buffer with valid data."
  ;; PEM region                llllllllllllllllllll20
  (with-temp-buffer
    (let ((q-pem-data (concat "const char* pem = \"-----BEGIN XX-----\"\n"
                              "                  \"data\"\n"
                              "                  \"-----END XX-----\";\n"))
          (clean-pem-data "-----BEGIN XX-----\ndata\n-----END XX-----"))
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
      (let ((result-buffer (x509--process-buffer
                            (current-buffer) '("crl" "-text" "-noout"))))
        (should result-buffer)
        (with-current-buffer result-buffer
          (should (re-search-forward "Certificate Revocation List (CRL):" nil t))
          (should (string-match-p "^\\*x-.*\\*" (buffer-name)))
          (should (boundp 'x509--shadow-buffer))
          (should (bufferp x509--shadow-buffer))
          (should (buffer-live-p x509--shadow-buffer))
          (setq shadow-buffer x509--shadow-buffer)
          (kill-buffer)
          ;; Shadow buffer should be killed in hook
          (should-not (buffer-live-p shadow-buffer)))))))

(ert-deftest x509--read-arguments ()
  "Ensure that argument is added to history."
  (let (history
        (args "my args"))
    (should (equal args (x509--read-arguments "PROMPT" args 'history)))
    (should (equal history (list args)))))

(ert-deftest x509--generic-view ()
  "Test creating a view buffer."
  (let (history
        (expanded-args (concat x509-crl-default-arg " -inform PEM")))
    (with-temp-buffer
      (insert x509--test-pem-crl)
      (x509--generic-view x509-crl-default-arg 'history 'x509-mode)
      (should (derived-mode-p 'x509-mode))
      ;; Buffer encoding is added to the arguments which is added to the
      ;; history.
      (should (equal history (list expanded-args)))
      (should (boundp 'x509--x509-mode-shadow-arguments))
      (should (equal x509--x509-mode-shadow-arguments expanded-args))
      (kill-buffer))))

(ert-deftest x509--generic-view-asn1 ()
  "Test creating a view buffer for x509-asn1-mode."
  (let (history
        (expanded-args (concat x509-asn1parse-default-arg " -inform PEM")))
    (with-temp-buffer
      (insert x509--test-pem-crl)
      (x509--generic-view x509-asn1parse-default-arg 'history 'x509-asn1-mode)
      (should (derived-mode-p 'x509-asn1-mode))
      ;; Buffer encoding is added to the arguments which is added to the
      ;; history.
      (should (equal history (list expanded-args)))
      (should (boundp 'x509--x509-asn1-mode-shadow-arguments))
      (should (equal x509--x509-asn1-mode-shadow-arguments expanded-args))
      (kill-buffer))))

(ert-deftest x509--get-x509-history ()
  "Verify that all commands return expected history variables."
  (should (equal 'x509--viewcert-history (x509--get-x509-history "x509")))
  (should (equal 'x509--viewreq-history (x509--get-x509-history "req")))
  (should (equal 'x509--viewcrl-history (x509--get-x509-history "crl")))
  (should (equal 'x509--viewpkcs7-history (x509--get-x509-history "pkcs7")))
  (should (equal 'x509--viewdh-history (x509--get-x509-history "dhparam")))
  (should (equal 'x509--viewkey-history (x509--get-x509-history "pkey")))
  (should (equal 'x509--viewasn1-history (x509--get-x509-history "asn1parse")))
  (should (equal nil (x509--get-x509-history "UNKNOWN"))))


(defun find-testfile(file-name)
  "Find FILE-NAME in testfiles."
  (expand-file-name file-name "testfiles"))

(ert-deftest x509-viewcert-pem ()
  "View PEM coded cert."
  (with-temp-buffer
    (insert-file-contents-literally (find-testfile "CA/pki/crt/jobbflykt.crt"))
    (x509-viewcert)
    (should (derived-mode-p 'x509-mode))
    (should (string-match-p "Certificate:" (buffer-string)))
    (kill-buffer)))

(ert-deftest x509-viewcert-der ()
  "View DER coded cert."
  (with-temp-buffer
    (insert-file-contents-literally (find-testfile "CA/pki/crt/jobbflykt.cer"))
    (x509-viewcert)
    (should (derived-mode-p 'x509-mode))
    (should (string-match-p "Certificate:" (buffer-string)))
    (kill-buffer)))

(ert-deftest x509-viewreq ()
  "View cert request."
  (with-temp-buffer
    (insert-file-contents-literally
     (find-testfile "CA/ca/request/jobbflykt.pem"))
    (x509-viewreq)
    (should (derived-mode-p 'x509-mode))
    (should (string-match-p "Certificate Request:" (buffer-string)))
    (kill-buffer)))

(ert-deftest x509-viewcrl-der ()
  "View DER-coded CRL."
  (with-temp-buffer
    (insert-file-contents-literally
     (find-testfile "CA/pki/crl/ca_testtool_ca_01.crl"))
    (x509-viewcrl)
    (should (derived-mode-p 'x509-mode))
    (should (string-match-p "Certificate Revocation List (CRL):"
                            (buffer-string)))
    (kill-buffer)))

(ert-deftest x509-viewcrl-pem ()
  "View PEM-coded CRL."
  (with-temp-buffer
    (insert-file-contents-literally
     (find-testfile "CA/pki/crl/ca_testtool_ca_01_crl.pem"))
    (x509-viewcrl)
    (should (derived-mode-p 'x509-mode))
    (should (string-match-p "Certificate Revocation List (CRL):"
                            (buffer-string)))
    (kill-buffer)))

(ert-deftest x509-viewpkcs7 ()
  "View pkcs7."
  (with-temp-buffer
    (insert-file-contents-literally
     (find-testfile "CA/pki/pkcs7/jobbflykt.p7b"))
    (x509-viewpkcs7)
    (should (derived-mode-p 'x509-mode))
    (should (string-match-p "Certificate:"
                            (buffer-string)))
    (should (string-match-p "Certificate Revocation List (CRL):"
                            (buffer-string)))
    (kill-buffer)))

(ert-deftest x509-viewdh ()
  "View Diffie-Hellman parameters."
  (with-temp-buffer
    (insert-file-contents-literally (find-testfile "dhparams-4096.pem"))
    (x509-viewdh)
    (should (derived-mode-p 'x509-mode))
    (should (string-match-p "DH Parameters: (4096 bit)" (buffer-string)))
    (kill-buffer)))

(ert-deftest x509-viewkey ()
  "View plaintext private key."
  (with-temp-buffer
    (insert-file-contents-literally (find-testfile "CA/pki/key/jobbflykt.key"))
    (x509-viewkey)
    (should (derived-mode-p 'x509-mode))
    (should (string-match-p "publicExponent: 65537 (0x10001)" (buffer-string)))
    (kill-buffer)))

(ert-deftest x509-viewasn1 ()
  "View plaintext private key as ASN.1."
  (with-temp-buffer
    (insert-file-contents-literally (find-testfile "CA/pki/crt/jobbflykt.crt"))
    (x509-viewasn1)
    (should (derived-mode-p 'x509-asn1-mode))
    (should (string-match-p ":X509v3 Subject Key Identifier" (buffer-string)))
    (kill-buffer)))

(provide 'x509-mode-tests)
;;; x509-mode-tests.el ends here
