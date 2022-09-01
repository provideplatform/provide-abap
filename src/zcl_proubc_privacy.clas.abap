CLASS zcl_proubc_privacy DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_proubc_privacy.
    METHODS constructor IMPORTING !ii_client   TYPE REF TO if_http_client
                                  !iv_tenant   TYPE zPRVDTENANTID
                                  !iv_bpitoken TYPE zPRVDREFRESHTOKEN..
  PROTECTED SECTION.
    DATA mi_client TYPE REF TO if_http_client.
    DATA lv_bpitoken TYPE zprvdrefreshtoken.
    DATA lv_tenantid TYPE zcasesensitive_str.
    METHODS send_receive RETURNING VALUE(rv_code) TYPE i.
ENDCLASS.

CLASS zcl_proubc_privacy IMPLEMENTATION.
  METHOD constructor.
    mi_client = ii_client.
    lv_bpitoken = iv_bpitoken.
    lv_tenantid = iv_tenant.
  ENDMETHOD.

  METHOD send_receive.
    mi_client->send( ).
    mi_client->receive( ).
    mi_client->response->get_status( IMPORTING code = rv_code ).
  ENDMETHOD.

  METHOD zif_proubc_privacy~listcircuits.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/circuits'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_privacy~createcircuit.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/circuits'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
* todo, set body, #/components/schemas/CreatecircuitRequest
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_privacy~verify.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/circuits/{circuit_id}/verify'.
    lv_temp = circuit_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{circuit_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
* todo, set body, #/components/schemas/VerifyRequest
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

