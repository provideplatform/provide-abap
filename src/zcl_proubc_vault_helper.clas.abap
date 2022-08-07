CLASS zcl_proubc_vault_helper DEFINITION
  PUBLIC
  INHERITING FROM zcl_proubc_api_helper
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor,
      create_key,
      derive_key,
      list_keys,
      delete_keys,
      encrypt,
      decrypt,
      sign,
      verify,
      setup_vault_msgs.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_proubc_vault_helper IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.
  METHOD create_key.
  ENDMETHOD.
  METHOD derive_key.
  ENDMETHOD.
  METHOD list_keys.
  ENDMETHOD.
  METHOD delete_keys.
  ENDMETHOD.
  METHOD encrypt.
  ENDMETHOD.
  METHOD decrypt.
  ENDMETHOD.
  METHOD sign.
  ENDMETHOD.
  METHOD verify.
  ENDMETHOD.
  METHOD setup_vault_msgs.
  ENDMETHOD.
ENDCLASS.
