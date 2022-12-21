CLASS zcl_proubc_ident DEFINITION
  PUBLIC
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_proubc_api_helper .

* Generated by abap-openapi-client
* Ident, 1.0
  PUBLIC SECTION.

    INTERFACES zif_proubc_ident .
    "! Initializes the Ident API proxy
    METHODS constructor
      IMPORTING
        !ii_client       TYPE REF TO if_http_client
        !iv_tenant       TYPE zprvdtenantid
        !iv_refreshtoken TYPE zprvdrefreshtoken .
  PROTECTED SECTION.
    DATA mi_client TYPE REF TO if_http_client.
    DATA mv_authtoken TYPE zprvdrefreshtoken.
    DATA mv_refreshtoken TYPE zprvdrefreshtoken.
    DATA mv_tenant TYPE zprvdtenantid.
    METHODS send_receive RETURNING VALUE(rv_code) TYPE i.
    METHODS set_refresh_bearer_token IMPORTING iv_tokenstring TYPE string.
    METHODS get_refresh_bearer_token
      RAISING cx_static_check.
    METHODS set_auth_token IMPORTING iv_tokenstring TYPE string.
    METHODS get_mv_authtoken RETURNING VALUE(rv_mv_authtoken) TYPE string.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_proubc_ident IMPLEMENTATION.


  METHOD constructor.
    mi_client = ii_client.
    mv_refreshtoken = iv_refreshtoken.
    mv_tenant = iv_tenant.
  ENDMETHOD.


  METHOD get_mv_authtoken.
    DATA: lv_mv_authtoken TYPE string.
    "get and check org/mv_tenant entered
    "" SAP user id -> mv_tenant authorization
    "retrieve the cached mv_authtoken
    "todo check the expiry of the mv_authtoken. If expired get a new one from refresh token.
    lv_mv_authtoken = mv_authtoken.
    rv_mv_authtoken = lv_mv_authtoken.
  ENDMETHOD.


  METHOD get_refresh_bearer_token.
    DATA lv_bearertoken TYPE string.
    "todo check auth token is valid (not empty or expired)

    DATA(lv_refreshtoken_length) = strlen( mv_refreshtoken ).

    CONCATENATE 'bearer' mv_refreshtoken INTO lv_bearertoken SEPARATED BY space.

    mi_client->request->set_header_field(
        name  = 'authorization'
        value = lv_bearertoken ).
  ENDMETHOD.

  METHOD send_receive.
    mi_client->send( ).
    mi_client->receive( ).
    mi_client->response->get_status( IMPORTING code = rv_code ).
  ENDMETHOD.

  METHOD set_auth_token.
    mv_authtoken = iv_tokenstring.
  ENDMETHOD.

  METHOD set_refresh_bearer_token.
    "todo add method implementation to retrive auth token from refresh token
    mv_refreshtoken = iv_tokenstring.
  ENDMETHOD.

  METHOD zif_proubc_ident~associateusertoapplication.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/applications/{application_id}/users'.
    lv_temp = application_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{application_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/AssociateusertoapplicationRequest
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr 
                                CHANGING data = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~authentication.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/authenticate'.
    DATA lv_basicauthdata TYPE REF TO data.
    DATA lv_authresponsestr TYPE string.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/AuthenticationRequest
    zcl_proubc_api_helper=>copy_data_to_ref( EXPORTING is_data = body
                                              CHANGING cr_data = lv_basicauthdata ).

    mi_client->request->set_cdata( data   =  /ui2/cl_json=>serialize( data        = lv_basicauthdata
                                                                      pretty_name = /ui2/cl_json=>pretty_mode-low_case ) ).

    lv_code = send_receive( ).
    ""TODO add logging call
    CASE lv_code.
      WHEN 201.
        lv_authresponsestr = mi_client->response->get_cdata( ).
        /ui2/cl_json=>deserialize( EXPORTING json = lv_authresponsestr 
                                    CHANGING data = apiresponse ).
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~authorizelong_termtoken.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/tokens'.
    DATA: lv_longtermrequestdata TYPE REF TO data,
          lv_requeststr          TYPE string.
    DATA lv_authresponsestr TYPE string.
    DATA: lt_headerfields TYPE tihttpnvp.

    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri' 
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type' 
                                          value = 'application/json' ).
    me->get_refresh_bearer_token( ).
    mi_client->request->get_header_fields(
      CHANGING
        fields = lt_headerfields ).

    zcl_proubc_api_helper=>copy_data_to_ref( EXPORTING is_data = body
                                             CHANGING cr_data  = lv_longtermrequestdata ).

    lv_requeststr = /ui2/cl_json=>serialize( data  = lv_longtermrequestdata
                                       pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata( lv_requeststr ).

    lv_code = send_receive( ).
    status = lv_code.

    CASE lv_code.
      WHEN 200 OR 201 OR 202.
        DATA: lv_parsedresponse TYPE zif_proubc_ident=>authorizelongtermtokenresponse.
        lv_authresponsestr = mi_client->response->get_cdata( ).
        /ui2/cl_json=>deserialize( EXPORTING json = lv_authresponsestr CHANGING data =  apiresponse ).
      WHEN 401.
      "refresh token incorrect
      WHEN 407. 
      "check the certs in strust
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~createapplication.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/applications'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'name'
                                          value = name ).
* todo, set body, #/components/schemas/CreateapplicationRequest
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr 
                               CHANGING  data = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~createorganization.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/organizations'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'name'
                                          value = name ).
* todo, set body, #/components/schemas/CreateorganizationRequest
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr 
                                CHANGING data = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~createuser.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/users'.
    DATA lv_requestdata TYPE REF TO data.
    DATA lv_requeststr TYPE string.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    zcl_proubc_api_helper=>copy_data_to_ref( EXPORTING is_data = body
                                              CHANGING cr_data = lv_REQUESTDATA ).
    lv_requeststr = /ui2/cl_json=>serialize( data = lv_requestdata
                                             pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata( data = lv_requeststr ).
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr
                               CHANGING data  = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~deleteapplication.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/applications/{application_id}'.
    lv_temp = application_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{application_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name = '~request_uri' 
                                         value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type'
                                         value = content_type ).
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr 
                                CHANGING data = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~deleteuser.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/tokens/{user_id}'.
    lv_temp = user_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{user_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name = '~request_uri' 
                                         value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' 
                                         value = content_type ).
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr 
                                CHANGING data = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~getapplicationdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/applications/{application_id}'.
    lv_temp = application_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{application_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type'
                                          value = content_type ).
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr
                               CHANGING data  = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
       "Success
       WHEN OTHERS.
       "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~getorganizationdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/organizations/{organization_id}'.
    lv_temp = organization_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{organization_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type'
                                          value = content_type ).
    mi_client->request->set_header_field( name  = 'name'
                                          value = name ).
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr
                               CHANGING data  = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~getuserdetail.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/users/{user_id}'.
    lv_temp = user_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{user_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type'
                                          value = content_type ).
    mi_client->request->set_header_field( name  = 'name'
                                          value = name ).
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr
                               CHANGING data  = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~listapplications.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/applications'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr
                               CHANGING data  = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~listapplicationusers.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/applications/{application_id}/users'.
    lv_temp = application_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{application_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' 
                                          value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' 
                                          value = content_type ).
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr
                               CHANGING data  = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~listorganizations.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/organizations'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type'
                                          value = content_type ).
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr 
                               CHANGING data  = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~listtokens.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/tokens'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' 
                                         value = content_type ).
    mi_client->request->set_header_field( name = 'name'
                                         value = name ).
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr
                                CHANGING data = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_ident~listuserscopy.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/users'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' 
                                         value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' 
                                         value = content_type ).
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr 
                                CHANGING data = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~refresh_access_token.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/tokens'.
    DATA: lv_longtermrequestdata TYPE REF TO data,
          lv_requeststr          TYPE string.
    DATA lv_authresponsestr TYPE string.
    DATA: lt_headerfields TYPE tihttpnvp.

    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri' 
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type' 
                                          value = 'application/json' ).
    get_refresh_bearer_token( ).
    
    mi_client->request->get_header_fields( CHANGING fields = lt_headerfields ).

    zcl_proubc_api_helper=>copy_data_to_ref( EXPORTING is_data = body
                                              CHANGING cr_data = lv_longtermrequestdata ).

    lv_requeststr = /ui2/cl_json=>serialize( EXPORTING data = lv_longtermrequestdata
                                                pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata( data = lv_requeststr ).

    lv_code = send_receive( ).
    status = lv_code.

    CASE lv_code.
      WHEN 200 OR 201 OR 202.
        DATA: lv_parsedresponse TYPE zif_proubc_ident=>authorizelongtermtokenresponse.
        lv_authresponsestr = mi_client->response->get_cdata( ).
        /ui2/cl_json=>deserialize( EXPORTING json = lv_authresponsestr
                                    CHANGING data = apiresponse ).
      WHEN 401. 
      "refresh token incorrect
      WHEN 407. 
      "check the certs in strust
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~revoketoken.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/tokens/f2202ba1-e2af-4505-9b1a-53e1ce8de904'.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type' 
                                          value = content_type ).
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr
                               CHANGING data  = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~updateapplication.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/applications/{application_id}'.
    lv_temp = application_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{application_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'name'
                                          value = name ).
* todo, set body, #/components/schemas/UpdateapplicationRequest
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr 
                               CHANGING data  = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~updateorganizationdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/organizations/{organization_id}'.
    lv_temp = organization_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{organization_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'name'
                                          value = name ).
    lv_code           = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr
                               CHANGING data  = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.


  METHOD zif_proubc_ident~updateuser.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/tokens'.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name  = '~request_uri' 
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'name' 
                                          value = name ).
* todo, set body, #/components/schemas/UpdateuserRequest
    lv_code = send_receive( ).
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ev_apiresponsestr 
                               CHANGING data  = ev_apiresponse ).
    ev_httpresponsecode = lv_code.
    "TODO add logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
