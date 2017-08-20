# x509-mode

Minor mode for viewing certificates, CRLs, keys and DH-parameters.

Uses OpenSSL for viewing PEM and DER encoded PKI entities.

While in a buffer displaying a certificate, use `M-x x509-viewcert` to create a
new buffer that displays the decoded certificate.  Use `M-x x509-viewcrl`, `M-X
x509-viewasn1`, `M-x x509-viewkey` and `M-x x509-viewdh` in a similar manner.

![Certificate](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-cert.png)

![CRL](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-crl.png)

![ASN.1](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-asn1.png)

## Installation

Install the `x509-mode` package from MELPA.
