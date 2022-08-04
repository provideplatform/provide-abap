INTERFACE zif_proubc_vault
  PUBLIC .

  "vault key types
  CONSTANTS: c_vaultkey_sym  TYPE string VALUE 'symmetric',
             c_vaultkey_asym TYPE string VALUE 'assymetric'.

  "vautl key specs
  "AES-256-GCM
  CONSTANTS: c_vaultkey_spec_chacha20 TYPE string VALUE 'ChaCha20'.
  "Baby Jubjub
  "C25519
  "Ed25519
  "Ed25519 NKey
  "RSA
  "secp256k1
  "hierarchical deterministic

ENDINTERFACE.
