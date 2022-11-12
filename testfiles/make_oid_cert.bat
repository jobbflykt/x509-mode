ECHO OFF
ECHO %PATH% | findstr catesttool >nul  || ^
  set PATH=%PATH%;C:\Program Files\link22\catesttool

ECHO ON
catesttool new CA2 || exit /b 1
catesttool make-ca-cert CA2 --cn "CA 02"  || exit /b 1
catesttool make-cert CA2 --cn "Oid Guy" ^
  --policy 2.16.840.1.101.3.2.1.3.13,2.16.840.1.101.3.2.1.3.47 ^
  --ext-key-usage  1.3.6.1.4.1.311.20.2.2,2.16.840.1.101.3.2.1.3.47,clientAuth,codeSigning,emailProtection ^
  || exit /b 1
