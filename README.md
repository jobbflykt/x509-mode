# x509-mode for Emacs

[![CI](https://github.com/jobbflykt/x509-mode/actions/workflows/test.yml/badge.svg)](https://github.com/jobbflykt/x509-mode/actions/workflows/test.yml)

Emacs major mode for viewing certificates, CRLs, keys, DH-parameters,
EC-Parameters and ASN.1 using [OpenSSL](https://github.com/openssl/openssl).

## Usage

Open a certificate, either PEM or DER encoded, in a buffer.

    M-x x509-dwim

A new buffer displays the parsed certificate.

To view certificates, CRLs, private and public keys Diffie-Hellman parameters,
elliptic curve parameters, certificate requests, pkcs7 files and parsed ASN.1
respectively:

    M-x x509-viewcert
    M-x x509-viewcrl
    M-x x509-viewkey
    M-x x509-viewpublickey
    M-x x509-viewdh
    M-x x509-viewec
    M-x x509-viewreq
    M-x x509-viewpkcs7
    M-x x509-viewasn1

If point is in, or at the beginning of, a PEM region, `M-x x509-dwim` tries to
determine what kind of object it is and call the right x509-view-function on
that region. If not in PEM region or if type is unknown, `x509-dwim` tries
different commands until one works. If data is still unknown, `x509-dwim` falls
back to `x509-viewasn1`.

    M-x x509-dwim

The command line for all of the above commands can be edited with C-u
prefix. Example:

    C-u M-x x509-viewcert

Commands operate on PEM data around point by default. If point is in
-----BEGIN/-----END, that region is sent to OpenSSL. Otherwise the whole buffer
is considered. If no PEM region is found, either around point or in buffer,
then the buffer is assumed to be DER encoded.

There is special command, `x509-swoop`, that has different semantics than those
above. It searches the whole buffer for _all_ recognized PEM regions and parses
them one by one. The output of all regions are sent to the same buffer. The
result buffer does not have the capabilities of a normal `x509-buffer`. There
is no way to edit command line argument, toggle to `x509-asn1-mode` or move to
next/previous region. `x509-swoop` intended to be used on PEM-coded certificate
chain files but work in files with various PKI resources.

    M-x x509-swoop

### Key bindings

**e** edit the current command.

**t** toggle between `x509-mode` and `x509-asn1-mode`

**n** go to next BEGIN/END region in source buffer and call `x509-dwim`

**p** go to previous BEGIN/END region in source buffer and call `x509-dwim`

**q** quit and kill view buffer.

In x509-asn1-mode, additional keys can be used to parse nested ASN.1
structures. Place cursor on a line in the asn.1 buffer and press:

**d** to re-parse at that -offset, going *down*.

**u** to go *up*. Can be used repeatedly.

Similarly, but with slightly different semantics and often more useful, is

**s** which calls asn1parse with the flag "-strparse N".

Where *N* is the offset at the current line. -strparse is used to parse BIT
STRINGs and OCTET STRINGs as DER encoded asn1. The offset *N* is calculated
differently for -strparse and -offset.

Also in x509-asn1-mode,

**x** toggles viewing the underlying binary data in a hexl buffer.

Movement in the x509-asn1-mode buffer is reflected in the hexl-buffer (inspired
by rmsbolt-mode).

## Installation

It's available on
[![MELPA](https://melpa.org/packages/x509-mode-badge.svg)](https://melpa.org/#/x509-mode)

    M-x package-install x509-mode

Or use the convenient [use-package](https://melpa.org/#/use-package)

    (use-package x509-mode
      :ensure t)

## OpenSSL

x509-mode requires OpenSSL. The variable `x509-openssl-cmd` must name the
openssl binary.

    M-x customize-variable x509-openssl-cmd

### Linux

    dnf install openssl

    (setq x509-openssl-cmd "openssl")

### Windows

x509-mode works with, for example, OpenSSL that comes with Git for Windows.

    (setq x509-openssl-cmd "C:/Program Files/Git/mingw64/bin/openssl.exe")

## Configuration

Default parameters to openssl commands and some other variables can be
customized.

    M-x customize-group x509

Faces mostly inherit from built-in faces. They can be customized.

    M-x customize-group x509-faces

## Screen shots

**X509 certificate**

![Certificate](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-cert.png "Certificate")

**Certificate revocation list**

![CRL](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-crl.png "Certificate revocation list")

**Private Key**

![Private key](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-pkey.png "Private key")

**DH-params**

![DH-params](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-dhparams.png "Diffie-Hellman parameters")

**EC-params**

![EC-Params](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-ecparams.png "Elliptic curve parameters")

**ASN.1**

![ASN.1](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-asn1.png "ASN.1")

**Hexl follow mode**

![hexl](https://github.com/jobbflykt/x509-mode/raw/master/screenshots/screenshot-hexl.png "Hexl mode")
