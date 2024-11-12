# x509-mode [![MELPA](https://melpa.org/packages/x509-mode-badge.svg)](https://melpa.org/#/x509-mode)

Major mode for viewing certificates, CRLs, keys, DH-parameters, EC-Parameters and ASN.1 using [OpenSSL](https://github.com/openssl/openssl).

## Usage

Open a certificate, either PEM or DER encoded, in a buffer.

    M-x x509-dwim

A new buffer should display the parsed certificate.

To view certificates, CRLs, private and public keys Diffie-Hellman parameters, certificate requests,pkcs7 files and parsed ASN.1 respectively:

    M-x x509-viewcert
    M-x x509-viewcrl
    M-x x509-viewkey
    M-x x509-viewpublickey
    M-x x509-viewdh
    M-x x509-viewec
    M-x x509-viewreq
    M-x x509-viewpkcs7
    M-x x509-viewasn1

If point is in, or at the beginning of, a PEM region, `M-x x509-dwim` tries to determine what kind of object it is and call the right x509-view-function on that region. If not in PEM region or if type is unknown, `x509-dwim` tries different commands until one works. If data is still unknown, `x509-dwim` falls back to `x509-viewasn1`.

    M-x x509-dwim

The command line for all command can be edited with C-u prefix. Example:

    C-u M-x x509-viewcert

Commands operate on PEM data around point by default. If point is in -----BEGIN/-----END, that region is sent to OpenSSL. Otherwise the whole buffer is considered. If no PEM region is found, either around point or in buffer, then the buffer is assumed to be DER encoded.

### Key bindings

**e** edit the current command.

**t** toggle between x509-mode and x509-asn1-mode

**n** go to next BEGIN/END region in source buffer and call `x509-dwim`

**p** go to prev BEGIN/END region in source buffer and call `x509-dwim`

**q** quit and kill view buffer.

In x509-asn1-mode, additional keys can be used to parse nested ASN.1 structures. Place cursor on a line in the asn.1 buffer and press:

**d** to re-parse at that -offset, going *down*.

**u** to go *up*. Can be used repeatedly.

Similarly, but with slightly different semantics, is

**s** which calls asn1parse with the flag "-strparse N".

Where *N* is the offset at the current line. -strparse is used to parse BIT STRINGs and OCTET STRINGs as. The offset *N** is calculated differently for -strparse and -offset.

Also in x509-asn1-mode,

**x** toggles viewing the underlying binary data in a hexl buffer.

Movement in the x509-asn1-mode buffer is reflected in the hexl-buffer (inspired by rmsbolt-mode).

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

## Screen shots

**X509 certificate**

![Certificate](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-cert.png)

**Certificate revocation list**

![CRL](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-crl.png)

**Private Key**

![CRL](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-pkey.png)

**DH-params**

![CRL](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-dhparams.png)

**EC-params**

![CRL](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-ecparams.png)

**ASN.1**

![ASN.1](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-asn1.png)
