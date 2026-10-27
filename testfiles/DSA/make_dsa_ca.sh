#!/bin/bash
set -o nounset
set -o errexit
set -o pipefail
IFS=$'\n\t'

set -x
set -e

# CA
openssl dsaparam -out ca_dsa_params.pem 2048
openssl gendsa -out ca_key.pem ca_dsa_params.pem
MSYS2_ARG_CONV_EXCL='*' openssl req \
  -new \
  -x509 \
  -days 3650 \
  -key ca_key.pem \
  -out ca_cert.pem \
  -subj '/CN=My DSA TestCA'

# Client
openssl dsaparam -out client_dsa_params.pem 2048
openssl gendsa -out client_key.pem client_dsa_params.pem
MSYS2_ARG_CONV_EXCL='*' openssl req \
  -new \
  -key client_key.pem \
  -out client.csr \
  -subj "/CN=client1"

# Sign
openssl x509 \
  -req \
  -in client.csr \
  -CA ca_cert.pem \
  -CAkey ca_key.pem \
  -CAcreateserial \
  -out client_cert.pem \
  -days 3650 \
  -sha256

# Cert and key
cat client_cert.pem client_key.pem > cert_and_key.pem

# PKCS12
openssl pkcs12 \
  -legacy \
  -export \
  -out client.p12 \
  -inkey client_key.pem \
  -in client_cert.pem \
  -certfile ca_cert.pem \
  -passout pass:11111111
