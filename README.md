# x509-mode [![MELPA](https://melpa.org/packages/x509-mode-badge.svg)](https://melpa.org/#/x509-mode)

Major mode for viewing certificates, CRLs, keys, DH-parameters and ASN.1 using OpenSSL.

## Usage

Open a certificate, either PEM or DER encoded, in a buffer.

    M-x x509-viewcert

A new buffer should display the parsed certificate.

To view certificates, CRLs, private keys Diffie-Hellman parameters and parsed ASN.1 respectively:

    M-x x509-viewcert
    M-x x509-viewcrl
    M-X x509-viewasn1
    M-x x509-viewkey
    M-x x509-viewdh
    M-x x509-viewreq

The command line for all command can be edited with C-u prefix. Example:

    C-u M-x x509-viewcert

## Installation

Install x509-mode through elpa. It's available on [melpa](https://melpa.org)

    M-x package-install x509-mode

I use the convenient [use-package](https://melpa.org/#/use-package)

    (use-package x509-mode
      :ensure t)

## OpenSSL

x509-mode requires OpenSSL. The variable `x509-openssl-cmd` must name the openssl binary.

    M-x customize-variable x509-openssl-cmd

### Linux

    dnf install openssl

    (setq x509-openssl-cmd "openssl")

### Windows

x509-mode works with, for example, OpenSSL that comes with Git for Windows.

    (setq x509-openssl-cmd "C:/Program Files/Git/mingw64/bin/openssl.exe")

## Screenshots

**X509 certificate**

![Certificate](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-cert.png)

**Certificate revocation list**

![CRL](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-crl.png)

**Private Key**

![CRL](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-pkey.png)

**DH-params**

![CRL](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-dhparams.png)


**ASN.1**

![ASN.1](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-asn1.png)
