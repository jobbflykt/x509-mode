# x509-mode

Minor mode for viewing certificates, CRLs, keys and DH-parameters.

Uses OpenSSL for viewing PEM and DER encoded PKI entities.

While in a buffer displaying a certificate, use `M-x x509-viewcert` to create a
new buffer that displays the decoded certificate.  Use `M-x x509-viewcrl`, `M-X
x509-viewasn1`, `M-x x509-viewkey` and `M-x x509-viewdh` in a similar manner.

Customize `x509-openssl-cmd` to the name of the OpenSSL binary. Typically `/usr/bin/openssl` on Linux. Git's `C:/Program Files/Git/mingw64/bin/openssl` can be used on Windows.

## Installation

Install the `x509-mode` package from MELPA.

## Screenshots

**X509 certificate**

![Certificate](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-cert.png)

**Certificate revocation list**

![CRL](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-crl.png)

**ASN.1**

![ASN.1](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-asn1.png)
