#!/bin/bash
set -o nounset
set -o errexit
set -o pipefail
IFS=$'\n\t'

export MSYS2_ARG_CONV_EXCL='*'

# CA
openssl ecparam -name prime256v1 -genkey -noout -out ca_key.pem
openssl req \
  -new \
  -x509 \
  -days 3650 \
  -key ca_key.pem \
  -out ca_cert.pem \
  -subj "/CN=My EC Test CA" \
  -sha256

# Client
openssl ecparam -name prime256v1 -genkey -noout -out client_key.pem
openssl req \
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
  -export \
  -out client.p12 \
  -inkey client_key.pem \
  -in client_cert.pem \
  -certfile ca_cert.pem \
  -passout pass:11111111
