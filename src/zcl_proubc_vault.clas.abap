CLASS zcl_proubc_vault DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_proubc_vault.
    METHODS constructor IMPORTING ii_client TYPE REF TO if_http_client.
  PROTECTED SECTION.
    DATA mi_client TYPE REF TO if_http_client.
    DATA mo_json TYPE REF TO zcl_oapi_json.
    METHODS send_receive RETURNING VALUE(rv_code) TYPE i.
  private section.
    methods sap_auth_check.
ENDCLASS.



CLASS zcl_proubc_vault IMPLEMENTATION.
METHOD constructor.
    mi_client = ii_client.
  ENDMETHOD.

  METHOD send_receive.
    mi_client->send( ).
    mi_client->receive( ).
    mi_client->response->get_status( IMPORTING code = rv_code ).
  ENDMETHOD.

  METHOD zif_proubc_vault~create_key.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults/{vault_id}/keys'.
    lv_temp = vault_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{vault_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'authorization' value = authorization ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_vault~list_keys.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults/{vault_id}/keys'.
    lv_temp = vault_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{vault_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'authorization' value = authorization ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_vault~derive_key.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults/{vault_id}/derive'.
    lv_temp = vault_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{vault_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'authorization' value = authorization ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_vault~delete_key.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults/{vault_id}/keys/{key_id}'.
    lv_temp = vault_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{vault_id}' IN lv_uri WITH lv_temp.
    lv_temp = key_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{key_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'authorization' value = authorization ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_vault~list_secrets.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults/{vault_id}/secrets'.
    lv_temp = vault_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{vault_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'authorization' value = authorization ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_vault~retreive_secret.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults/{vault_id}/secrets/{secret_id}'.
    lv_temp = vault_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{vault_id}' IN lv_uri WITH lv_temp.
    lv_temp = secret_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{secret_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'authorization' value = authorization ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_vault~delete_secret.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults/{vault_id}/secrets/{secret_id}'.
    lv_temp = vault_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{vault_id}' IN lv_uri WITH lv_temp.
    lv_temp = secret_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{secret_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'authorization' value = authorization ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_vault~create_vault.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    mi_client->request->set_header_field( name = 'Authorization' value = authorization ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_vault~list_vaults.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'Authorization' value = authorization ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_vault~createseal_unsealkey.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/unsealerkey'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'authorization' value = authorization ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_vault~unseal_vault.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/unseal'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'authorization' value = authorization ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  method sap_auth_check.
  ENDMETHOD.
ENDCLASS.
