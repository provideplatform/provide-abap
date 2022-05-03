CLASS Z100085_zcl_proubc_ident DEFINITION PUBLIC GLOBAL FRIENDS Z100085_zcl_proubc_api_helper.
* Generated by abap-openapi-client
* Ident, 1.0
  PUBLIC SECTION.
    INTERFACES Z100085_zif_proubc_ident.
    METHODS constructor IMPORTING ii_client       TYPE REF TO if_http_client
                                  iv_tenant       TYPE string
                                  iv_refreshtoken TYPE string.
  PROTECTED SECTION.
    DATA mi_client TYPE REF TO if_http_client.
    DATA mo_json TYPE REF TO Z100085_zcl_oapi_json.
    DATA authtoken TYPE string.
    DATA refreshtoken TYPE string.
    DATA tenant TYPE string.
    METHODS send_receive RETURNING VALUE(rv_code) TYPE i.
    METHODS parse_createapplicationrequest
      IMPORTING iv_prefix                       TYPE string
      RETURNING VALUE(createapplicationrequest) TYPE Z100085_zif_proubc_ident=>createapplicationrequest
      RAISING   cx_static_check.
    METHODS parse_associateusertoapplicati
      IMPORTING iv_prefix                             TYPE string
      RETURNING VALUE(associateusertoapplicationrequ) TYPE Z100085_zif_proubc_ident=>associateusertoapplicationrequ
      RAISING   cx_static_check.
    METHODS parse_updateapplicationrequest
      IMPORTING iv_prefix                       TYPE string
      RETURNING VALUE(updateapplicationrequest) TYPE Z100085_zif_proubc_ident=>updateapplicationrequest
      RAISING   cx_static_check.
    METHODS parse_createorganizationreques
      IMPORTING iv_prefix                        TYPE string
      RETURNING VALUE(createorganizationrequest) TYPE Z100085_zif_proubc_ident=>createorganizationrequest
      RAISING   cx_static_check.
    METHODS parse_updateorganizationdetail
      IMPORTING iv_prefix                             TYPE string
      RETURNING VALUE(updateorganizationdetailsreque) TYPE Z100085_zif_proubc_ident=>updateorganizationdetailsreque
      RAISING   cx_static_check.
    METHODS parse_authorizelong_termtokenr
      IMPORTING iv_prefix                             TYPE string
      RETURNING VALUE(authorizelong_termtokenrequest) TYPE Z100085_zif_proubc_ident=>authorizelong_termtokenrequest
      RAISING   cx_static_check.
    METHODS parse_authenticationrequest
      IMPORTING iv_prefix                    TYPE string
      RETURNING VALUE(authenticationrequest) TYPE Z100085_zif_proubc_ident=>authenticationrequest
      RAISING   cx_static_check.
    METHODS parse_createuserrequest
      IMPORTING iv_prefix                TYPE string
      RETURNING VALUE(createuserrequest) TYPE Z100085_zif_proubc_ident=>createuserrequest
      RAISING   cx_static_check.
    METHODS parse_updateuserrequest
      IMPORTING iv_prefix                TYPE string
      RETURNING VALUE(updateuserrequest) TYPE Z100085_zif_proubc_ident=>updateuserrequest
      RAISING   cx_static_check.
    METHODS set_refresh_bearer_token IMPORTING iv_tokenstring TYPE string.
    METHODS get_refresh_bearer_token
      RAISING cx_static_check.
    METHODS set_auth_token IMPORTING iv_tokenstring TYPE string.
    METHODS get_authtoken RETURNING VALUE(rv_authtoken) TYPE string.
ENDCLASS.

CLASS Z100085_zcl_proubc_ident IMPLEMENTATION.
  METHOD constructor.
    mi_client = ii_client.
    refreshtoken = iv_refreshtoken.
    tenant = iv_tenant.
  ENDMETHOD.

  METHOD send_receive.
    mi_client->send( ).
    mi_client->receive( ).
    mi_client->response->get_status( IMPORTING code = rv_code ).
  ENDMETHOD.

  METHOD parse_createapplicationrequest.
    createapplicationrequest-name = mo_json->value_string( iv_prefix && '/name' ).
  ENDMETHOD.

  METHOD parse_associateusertoapplicati.
    associateusertoapplicationrequ-user_id = mo_json->value_string( iv_prefix && '/user_id' ).
  ENDMETHOD.

  METHOD parse_updateapplicationrequest.
    updateapplicationrequest-name = mo_json->value_string( iv_prefix && '/name' ).
    updateapplicationrequest-description = mo_json->value_string( iv_prefix && '/description' ).
    updateapplicationrequest-type = mo_json->value_string( iv_prefix && '/type' ).
    updateapplicationrequest-hidden = mo_json->value_boolean( iv_prefix && '/hidden' ).
  ENDMETHOD.

  METHOD parse_createorganizationreques.
    createorganizationrequest-name = mo_json->value_string( iv_prefix && '/name' ).
    createorganizationrequest-description = mo_json->value_string( iv_prefix && '/description' ).
  ENDMETHOD.

  METHOD parse_updateorganizationdetail.
    updateorganizationdetailsreque-name = mo_json->value_string( iv_prefix && '/name' ).
    updateorganizationdetailsreque-description = mo_json->value_string( iv_prefix && '/description' ).
  ENDMETHOD.

  METHOD parse_authorizelong_termtokenr.
    authorizelong_termtokenrequest-scope = mo_json->value_string( iv_prefix && '/scope' ).
    authorizelong_termtokenrequest-organization_id = mo_json->value_string( iv_prefix && '/organization_id' ).
  ENDMETHOD.

  METHOD parse_authenticationrequest.
    authenticationrequest-email = mo_json->value_string( iv_prefix && '/email' ).
    authenticationrequest-password = mo_json->value_string( iv_prefix && '/password' ).
  ENDMETHOD.

  METHOD parse_createuserrequest.
    createuserrequest-email = mo_json->value_string( iv_prefix && '/email' ).
    createuserrequest-first_name = mo_json->value_string( iv_prefix && '/first_name' ).
    createuserrequest-last_name = mo_json->value_string( iv_prefix && '/last_name' ).
  ENDMETHOD.

  METHOD parse_updateuserrequest.
    updateuserrequest-email = mo_json->value_string( iv_prefix && '/email' ).
    updateuserrequest-first_name = mo_json->value_string( iv_prefix && '/first_name' ).
    updateuserrequest-last_name = mo_json->value_string( iv_prefix && '/last_name' ).
    updateuserrequest-password = mo_json->value_string( iv_prefix && '/password' ).
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~createapplication.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/applications'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'name' value = name ).
* todo, set body, #/components/schemas/CreateapplicationRequest
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~listapplications.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/applications'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~associateusertoapplication.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/applications/{application_id}/users'.
    lv_temp = application_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{application_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
* todo, set body, #/components/schemas/AssociateusertoapplicationRequest
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~listapplicationusers.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/applications/{application_id}/users'.
    lv_temp = application_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{application_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~getapplicationdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/applications/{application_id}'.
    lv_temp = application_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{application_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~updateapplication.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/applications/{application_id}'.
    lv_temp = application_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{application_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'name' value = name ).
* todo, set body, #/components/schemas/UpdateapplicationRequest
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~deleteapplication.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/applications/{application_id}'.
    lv_temp = application_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{application_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~listorganizations.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/organizations'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~createorganization.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/organizations'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'name' value = name ).
* todo, set body, #/components/schemas/CreateorganizationRequest
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~getorganizationdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/organizations/{organization_id}'.
    lv_temp = organization_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{organization_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    mi_client->request->set_header_field( name = 'name' value = name ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~updateorganizationdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/organizations/{organization_id}'.
    lv_temp = organization_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{organization_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'name' value = name ).
* todo, set body, #/components/schemas/UpdateorganizationdetailsRequest
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~listtokens.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/tokens'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    mi_client->request->set_header_field( name = 'name' value = name ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~authorizelong_termtoken.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/tokens'.
    DATA: lv_longtermrequestdata  TYPE REF TO data,
          lv_requeststr type string.
    DATA lv_authresponsestr TYPE string.

    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).

    z100085_zcl_proubc_api_helper=>copy_data_to_ref( EXPORTING is_data = body
                      CHANGING cr_data = lv_longtermrequestdata  ).

    lv_requeststr = /ui2/cl_json=>serialize( exporting data = lv_longtermrequestdata
                                       pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata(
      EXPORTING
        data   =  lv_requeststr
*        offset = 0
*        length = -1
    ).

*    DATA: lv_wtfman                     TYPE REF TO data,
*          lv_wtfstr TYPE string.
*    mi_client->request->get_data(
**      EXPORTING
**        offset             = 0
**        length             = -1
**        virus_scan_profile = '/SIHTTP/HTTP_UPLOAD'
**        vscan_scan_always  = if_http_entity=>co_content_check_profile
*      RECEIVING
*        data               =  lv_wtfstr
*    ).

    me->get_refresh_bearer_token( ).

    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200 OR 201 OR 202.
         lv_authresponsestr = mi_client->response->get_cdata( ).
        /ui2/cl_json=>deserialize( EXPORTING json = lv_authresponsestr CHANGING data =  apiresponse ).
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~updateuser.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/tokens'.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'name' value = name ).
* todo, set body, #/components/schemas/UpdateuserRequest
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~revoketoken.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/tokens/f2202ba1-e2af-4505-9b1a-53e1ce8de904'.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~authentication.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/authenticate'.
    DATA lv_basicauthdata TYPE REF TO data.
    DATA lv_authresponsestr TYPE string.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
* todo, set body, #/components/schemas/AuthenticationRequest
    z100085_zcl_proubc_api_helper=>copy_data_to_ref( EXPORTING is_data = body
                      CHANGING cr_data = lv_basicauthdata  ).

    "/ui2/cl_json=>serialize( ls_basicauthdata ).

    mi_client->request->set_cdata(
      EXPORTING
        data   =  /ui2/cl_json=>serialize( EXPORTING data = lv_basicauthdata
                                                     pretty_name      = /ui2/cl_json=>pretty_mode-low_case )
*        offset = 0
*        length = -1
    ).

    lv_code = send_receive( ).
    "WRITE / lv_code.
    CASE lv_code.
      WHEN 201.
        lv_authresponsestr = mi_client->response->get_cdata( ).
        /ui2/cl_json=>deserialize( EXPORTING json = lv_authresponsestr CHANGING data =  apiresponse ).
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~listuserscopy.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/users'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~createuser.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/users'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
* todo, set body, #/components/schemas/CreateuserRequest
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~getuserdetail.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/users/{user_id}'.
    lv_temp = user_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{user_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    mi_client->request->set_header_field( name = 'name' value = name ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD Z100085_zif_proubc_ident~deleteuser.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/tokens/{user_id}'.
    lv_temp = user_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{user_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD set_refresh_bearer_token.
    "todo add method implementation to retrive auth token from refresh token
    refreshtoken = iv_tokenstring.
  ENDMETHOD.

  METHOD get_refresh_bearer_token.
    DATA lv_bearertoken TYPE string.
    "todo check auth token is valid (not empty or expired)
    CONCATENATE 'bearer' refreshtoken INTO lv_bearertoken SEPARATED BY space.
    mi_client->request->set_header_field(
      EXPORTING
        name  = 'authorization'    " Name of the header field
        value = lv_bearertoken    " HTTP header field value
    ).
  ENDMETHOD.

  METHOD get_authtoken.
    DATA: lv_authtoken TYPE string.
    "get and check org/tenant entered
    "" SAP user id -> tenant authorization
    "retrieve the cached authtoken
    "todo check the expiry of the authtoken. If expired get a new one from refresh token.
    lv_authtoken = authtoken.
    rv_authtoken = lv_authtoken.
  ENDMETHOD.

  METHOD set_auth_token.
    authtoken = iv_tokenstring.
  ENDMETHOD.

ENDCLASS.

