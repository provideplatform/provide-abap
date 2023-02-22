CLASS zcl_prvd_vault DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_prvd_vault.
    "! Constructor method for instantiating the PRVD Vault API proxy
    METHODS constructor IMPORTING !ii_client   TYPE REF TO if_http_client
                                  !iv_tenant   TYPE zprvdtenantid
                                  !iv_bpitoken TYPE zprvdrefreshtoken.
  PROTECTED SECTION.
    DATA mi_client TYPE REF TO if_http_client.
    DATA mv_vault_url TYPE string VALUE 'https://vault.provide.services'.
    DATA mv_bpitoken TYPE zprvdrefreshtoken.
    DATA mv_tenantid TYPE zcasesensitive_str.
    METHODS send_receive RETURNING VALUE(rv_code) TYPE i.
  PRIVATE SECTION.
    METHODS sap_auth_check.
    METHODS get_bpi_token
      RAISING cx_static_check.
ENDCLASS.



CLASS zcl_prvd_vault IMPLEMENTATION.


  METHOD constructor.
    mi_client = ii_client.
    mv_bpitoken = iv_bpitoken.
    mv_tenantid = iv_tenant.
  ENDMETHOD.


  METHOD get_bpi_token.
    DATA lv_bearertoken TYPE string.
    CONCATENATE 'Bearer' mv_bpitoken INTO lv_bearertoken SEPARATED BY space.
    mi_client->request->set_header_field( name  = 'Authorization'
                                          value = lv_bearertoken ).
  ENDMETHOD.


  METHOD sap_auth_check.
    "TODO create authorization field in su20. need to check default character length for tenant id. SAP auth check limits to 40 chars.
  ENDMETHOD.


  METHOD send_receive.
    mi_client->send( ).
    mi_client->receive( ).
    mi_client->response->get_status( IMPORTING code = rv_code ).
  ENDMETHOD.


  METHOD zif_prvd_vault~createseal_unsealkey.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/unsealerkey'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type'
                                         value = content_type ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_vault~create_key.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults/{vault_id}/keys'.
    lv_temp = vault_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{vault_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type'
                                         value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_vault~create_vault.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    mi_client->request->set_header_field( name = 'Content-Type'
                                         value = content_type ).
    mi_client->request->set_header_field( name = 'content-type'
                                         value = content_type ).
    get_bpi_token( ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_vault~delete_key.
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
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type'
                                         value = content_type ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_vault~delete_secret.
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
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type'
                                         value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_vault~derive_key.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults/{vault_id}/derive'.
    lv_temp = vault_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{vault_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type'
                                         value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_vault~list_keys.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/vaults/{vault_id}/keys'.
    lv_temp = iv_vault_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{vault_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_vault~list_secrets.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults/{vault_id}/secrets'.
    lv_temp = vault_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{vault_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type'
                                         value = content_type ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_vault~list_vaults.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/vaults'.
    DATA lv_responsestr type string.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bpi_token( ).
    "mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    lv_responsestr = mi_client->response->get_cdata( ).
    ev_apiresponsestr = lv_responsestr.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = lv_responsestr
      CHANGING
        data             = ev_apiresponse
    ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_vault~retreive_secret.
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
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type'
                                         value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_vault~unseal_vault.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE 'https://vault.provide.services/api/v1/unseal'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bpi_token( ).
    mi_client->request->set_header_field( name = 'Content-Type'
                                         value = content_type ).
    mi_client->request->set_cdata( body ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
