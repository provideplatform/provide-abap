INTERFACE zif_prvd_vault
  PUBLIC .

  "vault key types
  CONSTANTS: c_vaultkey_sym  TYPE string VALUE 'symmetric',
             c_vaultkey_asym TYPE string VALUE 'assymetric'.

  "vault key specs - symmetric
  CONSTANTS: c_vaultkey_spec_aes256 TYPE string VALUE 'AES-256-GCM'.
  CONSTANTS: c_vaultkey_spec_chacha20 TYPE string VALUE 'ChaCha20'.
  "RSA
  CONSTANTS: c_vaultkey_spec_rsa TYPE string VALUE 'RSA'.

  "vault key specs - assymetric
  CONSTANTS: c_vaultkey_spec_bjj TYPE string VALUE 'babyJubJub'.
  CONSTANTS: c_vaultkey_spec_c25519 TYPE string VALUE 'C25519'.
  CONSTANTS: c_vaultkey_spec_ed25519 TYPE string VALUE 'Ed25519'.
  CONSTANTS: c_vaultkey_spec_nats_ed25519 TYPE string VALUE 'Ed25519-nkey'.
  CONSTANTS: c_vaultkey_spec_secp256k1 TYPE string VALUE 'secp256k1'.
  CONSTANTS: c_vaultkey_spec_bip39 TYPE string VALUE 'BIP39'.


  TYPES: BEGIN OF ty_vault_query,
           id          TYPE zcasesensitive_str,
           created_at  TYPE string,
           name        TYPE string,
           description TYPE string,
         END OF ty_vault_query.

  TYPES: tty_vault_query TYPE TABLE OF ty_vault_query WITH KEY id.

  TYPES: BEGIN OF ty_vault_create,
           name        TYPE string,
           description TYPE string,
         END OF ty_vault_create.

  TYPES: BEGIN OF ty_vault_keys,
           id          TYPE string,
           created_at  TYPE string,
           vault_id    TYPE string,
           type        TYPE string,
           usage       TYPE string,
           spec        TYPE string,
           name        TYPE string,
           description TYPE string,
           address     TYPE string,
           public_key  TYPE string,
           fingerprint TYPE string,
         END OF ty_vault_keys.

  TYPES: ty_vault_keys_list TYPE TABLE OF ty_vault_keys WITH KEY id.



  "! POST - "Create a key: C25519"
  "! Operation id: Createakey:C25519
  "! Parameter: authorization, required, header
  "! Parameter: Content-Type, required, header
  "! Parameter: vault_id, required, path
  "! Response: 200
  "! Body schema: string
  METHODS create_key
    IMPORTING
              authorization       TYPE string
              content_type        TYPE string
              vault_id            TYPE string
              body                TYPE string
    EXPORTING
              ev_apiresponsestr   TYPE string
              ev_apiresponse      TYPE REF TO data
              ev_httpresponsecode TYPE i
    RAISING   cx_static_check.

  "! GET - "List keys"
  "! Operation id: Listkeys
  "! Parameter: authorization, required, header
  "! Parameter: Content-Type, required, header
  "! Parameter: vault_id, required, path
  "! Response: 200
  "! Body schema: string
  METHODS list_keys
    IMPORTING
              iv_vault_id         TYPE string
    EXPORTING
              ev_apiresponsestr   TYPE string
              ev_apiresponse      TYPE REF TO data
              ev_httpresponsecode TYPE i
    RAISING   cx_static_check.

  "! POST - "Derive a key: ChaCha20"
  "! Operation id: Deriveakey:ChaCha20
  "! Parameter: authorization, required, header
  "! Parameter: Content-Type, required, header
  "! Parameter: vault_id, required, path
  "! Response: 200
  "! Body schema: string
  METHODS derive_key
    IMPORTING
              authorization       TYPE string
              content_type        TYPE string
              vault_id            TYPE string
              body                TYPE string
    EXPORTING
              ev_apiresponsestr   TYPE string
              ev_apiresponse      TYPE REF TO data
              ev_httpresponsecode TYPE i
    RAISING   cx_static_check.

  "! DELETE - "Delete a key"
  "! Operation id: Deleteakey
  "! Parameter: authorization, required, header
  "! Parameter: Content-Type, required, header
  "! Parameter: vault_id, required, path
  "! Parameter: key_id, required, path
  "! Response: 200
  METHODS delete_key
    IMPORTING
              authorization       TYPE string
              content_type        TYPE string
              vault_id            TYPE string
              key_id              TYPE string
    EXPORTING
              ev_apiresponsestr   TYPE string
              ev_apiresponse      TYPE REF TO data
              ev_httpresponsecode TYPE i
    RAISING   cx_static_check.

  "! GET - "List secrets"
  "! Operation id: Listsecrets
  "! Parameter: authorization, required, header
  "! Parameter: Content-Type, required, header
  "! Parameter: vault_id, required, path
  "! Response: 200
  METHODS list_secrets
    IMPORTING
              authorization       TYPE string
              content_type        TYPE string
              vault_id            TYPE string
    EXPORTING
              ev_apiresponsestr   TYPE string
              ev_apiresponse      TYPE REF TO data
              ev_httpresponsecode TYPE i
    RAISING   cx_static_check.

  "! POST - "Retreive secret"
  "! Operation id: Retreivesecret
  "! Parameter: authorization, required, header
  "! Parameter: Content-Type, required, header
  "! Parameter: vault_id, required, path
  "! Parameter: secret_id, required, path
  "! Response: 200
  "! Body schema: string
  METHODS retreive_secret
    IMPORTING
              authorization       TYPE string
              content_type        TYPE string
              vault_id            TYPE string
              secret_id           TYPE string
              body                TYPE string
    EXPORTING
              ev_apiresponsestr   TYPE string
              ev_apiresponse      TYPE REF TO data
              ev_httpresponsecode TYPE i
    RAISING   cx_static_check.

  "! DELETE - "Delete secret"
  "! Operation id: Deletesecret
  "! Parameter: authorization, required, header
  "! Parameter: Content-Type, required, header
  "! Parameter: vault_id, required, path
  "! Parameter: secret_id, required, path
  "! Response: 200
  "! Body schema: string
  METHODS delete_secret
    IMPORTING
              authorization       TYPE string
              content_type        TYPE string
              vault_id            TYPE string
              secret_id           TYPE string
              body                TYPE string
    EXPORTING
              ev_apiresponsestr   TYPE string
              ev_apiresponse      TYPE REF TO data
              ev_httpresponsecode TYPE i
    RAISING   cx_static_check.

  "! POST - "Create Vault"
  "! Operation id: CreateVault
  "! Parameter: Content-Type, required, header
  "! Parameter: content-type, required, header
  "! Parameter: Authorization, required, header
  "! Response: 200
  "! Body schema: string
  METHODS create_vault
    IMPORTING
              content_type        TYPE string
              authorization       TYPE string
              body                TYPE string
    EXPORTING
              ev_apiresponsestr   TYPE string
              ev_apiresponse      TYPE REF TO data
              ev_httpresponsecode TYPE i
    RAISING   cx_static_check.

  "! GET - "List Vaults"
  "! Operation id: ListVaults
  "! Parameter: Authorization, required, header
  "! Response: 200
  "! Body schema: string
  METHODS list_vaults
    EXPORTING ev_apiresponsestr   TYPE string
              ev_apiresponse      TYPE REF TO data
              ev_httpresponsecode TYPE i
    RAISING   cx_static_check.

  "! POST - "Create Seal/Unseal key"
  "! Operation id: CreateSeal/Unsealkey
  "! Parameter: authorization, required, header
  "! Parameter: Content-Type, required, header
  "! Response: 200
  METHODS createseal_unsealkey
    IMPORTING
              authorization       TYPE string
              content_type        TYPE string
    EXPORTING
              ev_apiresponsestr   TYPE string
              ev_apiresponse      TYPE REF TO data
              ev_httpresponsecode TYPE i
    RAISING   cx_static_check.

  "! POST - "Unseal vault"
  "! Operation id: Unsealvault
  "! Parameter: authorization, required, header
  "! Parameter: Content-Type, required, header
  "! Response: 200
  "! Body schema: string
  METHODS unseal_vault
    IMPORTING
              authorization       TYPE string
              content_type        TYPE string
              body                TYPE string
    EXPORTING
              ev_apiresponsestr   TYPE string
              ev_apiresponse      TYPE REF TO data
              ev_httpresponsecode TYPE i
    RAISING   cx_static_check.

ENDINTERFACE.
