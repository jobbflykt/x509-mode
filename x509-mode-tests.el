(require 'ert)

(require 'x509-mode)

(defun x509--test-make-gmt-time (&optional offset-seconds)
  "Return a time string.
Ex: \"Wed Aug 17 08:48:06 2022 GMT\""
  (let ((offset (or offset-seconds 0)))
    (format-time-string "%b %e %H:%M:%S %Y GMT"
                        (time-add (current-time) offset)
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
  "Identify PEM encoding"
  (with-temp-buffer
    (insert "not pem ")
    (should (equal "DER" (x509--buffer-encoding (current-buffer)))))
  (with-temp-buffer
    (insert "-----BEGIN ")
    (should (equal "PEM" (x509--buffer-encoding (current-buffer))))))

(ert-deftest x509--pem-region()
  "Find region delimited by BEGIN/END"
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
  "Behave when there is no region"
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
  "Create buffer with valid data"
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

(ert-deftest x509--process-buffer ()
  "Let OpenSSL process buffer."
  (let ((text
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
           "-----END X509 CRL-----"))
        shadow-buffer)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (x509--process-buffer (current-buffer) '("crl" "-text" "-noout"))
      (should (re-search-forward "Certificate Revocation List (CRL):" nil t))
      (should (string-match-p "^\\*x-.*\\*" (buffer-name)))
      (should (boundp 'x509--shadow-buffer))
      (should (bufferp x509--shadow-buffer))
      (should (buffer-live-p x509--shadow-buffer))
      (setq shadow-buffer x509--shadow-buffer)
      (kill-buffer)
      ;; Shadow buffer should be killed in hook
      (should-not (buffer-live-p shadow-buffer)))))
