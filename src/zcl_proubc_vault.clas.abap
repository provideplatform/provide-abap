CLASS zcl_proubc_vault DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_proubc_vault.
    METHODS constructor IMPORTING !ii_client   TYPE REF TO if_http_client
                                  !iv_tenant   TYPE zPRVDTENANTID
                                  !iv_bpitoken TYPE zPRVDREFRESHTOKEN.
  PROTECTED SECTION.
    DATA mi_client TYPE REF TO if_http_client.
    DATA lv_vault_url TYPE string VALUE 'https://vault.provide.services'.
    DATA mo_json TYPE REF TO zcl_oapi_json.
    DATA lv_bpitoken TYPE zprvdrefreshtoken.
    data lv_tenantid type zcasesensitive_str.
    METHODS send_receive RETURNING VALUE(rv_code) TYPE i.
  PRIVATE SECTION.
    METHODS sap_auth_check.
    METHODS get_bpi_token
      RAISING cx_static_check.
ENDCLASS.



CLASS ZCL_PROUBC_VAULT IMPLEMENTATION.


  METHOD constructor.
    mi_client = ii_client.
    lv_bpitoken = iv_bpitoken.
    lv_tenantid = iv_tenant.
  ENDMETHOD.


  METHOD get_bpi_token.
    DATA lv_bearertoken TYPE string.
    CONCATENATE 'Bearer' lv_bpitoken INTO lv_bearertoken SEPARATED BY space.
    mi_client->request->set_header_field(
      EXPORTING
        name  = 'Authorization'    " Name of the header field
        value = lv_bearertoken    " HTTP header field value
    ).
  ENDMETHOD.


  METHOD sap_auth_check.
    "TODO create authorization field in su20. need to check default character length for tenant id. SAP auth check limits to 40 chars.
  ENDMETHOD.


  METHOD send_receive.
    mi_client->send( ).
    mi_client->receive( ).
    mi_client->response->get_status( IMPORTING code = rv_code ).
  ENDMETHOD.


  METHOD zif_proubc_vault~createseal_unsealkey.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/unsealerkey'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    me->get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    lv_code = send_receive( ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
    ENDCASE.
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
    me->get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    "WRITE / lv_code. ~replace with logging call
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
    me->get_bpi_token( ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    "WRITE / lv_code. ~replace with logging call
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
    me->get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    lv_code = send_receive( ).
    "WRITE / lv_code. ~replace with logging call
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
    me->get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    "WRITE / lv_code. ~replace with logging call
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
    me->get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    "WRITE / lv_code. ~replace with logging call
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
    me->get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    "WRITE / lv_code. ~replace with logging call
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
    me->get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    lv_code = send_receive( ).
    "WRITE / lv_code. ~replace with logging call
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
    me->get_bpi_token( ).
    "mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    "WRITE / lv_code. ~replace with logging call
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
    me->get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    "WRITE / lv_code. ~replace with logging call
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
    me->get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type' value = content_type ).
    mi_client->request->set_cdata( body ).

    lv_code = send_receive( ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
