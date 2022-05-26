CLASS z100085_zcl_proubc_api_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      map_data_to_tenant IMPORTING iv_data    TYPE REF TO data
                         EXPORTING et_tenants TYPE z100085_prvdorgs,
      build_refresh_token IMPORTING iv_refreshtoken1 TYPE string
                                    iv_refreshtoken2 TYPE string
                          EXPORTING ev_tokenlength   TYPE int4
                                    ev_refreshtoken  TYPE z100085_prvdrefreshtoken,
      copy_data_to_ref
        IMPORTING
          !is_data TYPE any
        CHANGING
          !cr_data TYPE REF TO data.
    METHODS:
      call_ident_api IMPORTING iv_tenant TYPE z100085_prvdtenantid EXPORTING ev_authtoken TYPE REF TO data
                                                                             ev_bpiendpoint TYPE string,
      call_baseline_api,
      authenticate_ident_api_basic IMPORTING iv_userid   TYPE string
                                             iv_password TYPE z100085_casesensitive_str
                                   EXPORTING authtoken   TYPE REF TO data,
      baseline_health_check IMPORTING iv_tenant TYPE z100085_prvdtenantid EXPORTING ev_isreachable TYPE boolean,
      setup_protocol_msg,
      send_protocol_msg IMPORTING body TYPE z100085_zif_proubc_baseline=>protocolmessage_req EXPORTING statuscode TYPE i.
  PROTECTED SECTION.
    DATA: lv_defaulttenant        TYPE z100085_prvdorgs-organization_id VALUE 'e41dea7b-3510-4ffa-8ff4-53f3b158c8b4',
          lv_defaultidenttoken    TYPE REF TO data,
          lv_defaultbaselinetoken TYPE REF TO data,
          lo_ident_client         TYPE REF TO Z100085_zif_proubc_ident,
          lo_baseline_client      TYPE REF TO z100085_zif_proubc_baseline.
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_api_helper IMPLEMENTATION.
  METHOD map_data_to_tenant.
    DATA: ls_tenant TYPE z100085_prvdorgs.
    FIELD-SYMBOLS: <ls_data> TYPE REF TO data.
    "Todo this is dumping the ABAP code, need it fixed to map the deserialized data into the ABAP structure for populating db
*    "might all be garbage. try something else.
*    ASSIGN iv_data->* TO <ls_data>.
*    DATA(lo_structdescr) = CAST cl_abap_structdescr(  cl_abap_structdescr=>describe_by_data( p_data = <ls_data> ) ).
*    DATA(components)     = lo_structdescr->get_components( ).
*    LOOP AT components ASSIGNING FIELD-SYMBOL(<fs_component>).
*    ENDLOOP.

    "loop at iv_data assigning field-symbol(<fs_data>).
    "endloop.
    "assign iv_data->* to et_tenants.
    "assign component '' of structure iv_data to ls_tenant-prvdorgid.
  ENDMETHOD.
  METHOD build_refresh_token.
    "strlen( iv_refreshtoken1 )
    CONCATENATE iv_refreshtoken1 iv_refreshtoken2 INTO ev_refreshtoken.
  ENDMETHOD.
  METHOD copy_data_to_ref.
    FIELD-SYMBOLS:
                 <ls_data> TYPE any.

    CREATE DATA cr_data LIKE is_data.
    ASSIGN cr_data->* TO <ls_data>.
    <ls_data> = is_data.

  ENDMETHOD.
  METHOD call_ident_api.
    DATA:
      lo_http_client     TYPE REF TO if_http_client,
      lo_ident_api       TYPE REF TO z100085_zif_proubc_ident,
      ls_prvdtenant      TYPE z100085_prvdorgs,
      lv_refreshtokenstr TYPE z100085_prvdrefreshtoken,
      lv_identurl        TYPE string,
      lv_apiresponse     TYPE REF TO data.

    "z100085_zcl_proubc_prvdtenants=>get_prvdtenant( EXPORTING iv_prvdtenant = iv_tenant
    "                                               IMPORTING ev_prvdtenant = ls_prvdtenant
    "                                                 ).

    SELECT SINGLE * FROM z100085_prvdorgs INTO ls_prvdtenant WHERE organization_id = iv_tenant.

    CONCATENATE ls_prvdtenant-refresh_token ls_prvdtenant-refresh_tokenext INTO lv_refreshtokenstr.
    lv_identurl = ls_prvdtenant-ident_endpoint.

    ev_bpiendpoint = ls_prvdtenant-bpi_endpoint.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = lv_identurl
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      " error handling
    ENDIF.



    lo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.
    lo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client value = '100' ).

    lo_ident_api = NEW Z100085_zcl_proubc_ident( ii_client = lo_http_client iv_tenant = iv_tenant iv_refreshtoken = lv_refreshtokenstr  ).
    lo_ident_client = lo_ident_api.

    DATA: authtokenreqbody TYPE z100085_zif_proubc_ident=>authorizelong_termtokenrequest.

    authtokenreqbody-organization_id = iv_tenant.
    authtokenreqbody-scope = 'offline_access'.

    "lo_ident_api->

    lo_ident_api->authorizelong_termtoken( EXPORTING body = authtokenreqbody IMPORTING apiresponse = lv_apiresponse  ).
*    CATCH cx_static_check.

    ev_authtoken = lv_apiresponse.


    "TODO store the auth token securely
  ENDMETHOD.
  METHOD call_baseline_api.
    DATA:
      lo_http_client  TYPE REF TO if_http_client,
      lo_baseline_api TYPE REF TO z100085_zif_proubc_baseline.
    "TODO check validity of current auth token
    "or call me-call_ident_api to get new auth token

    cl_http_client=>create_by_url(
    EXPORTING
      url                = 'https://baseline.provide.services'
    IMPORTING
      client             = lo_http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4 ).
    IF sy-subrc <> 0.
      " error handling
    ENDIF.

    lo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.
    lo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client value = '100' ).

    lo_baseline_api = NEW z100085_zcl_proubc_baseline( ii_client = lo_http_client ).
    "TODO - do something useful with the Baseline API
    "lo_baseline_api->authentication( body =  ).
*    CATCH cx_static_check.
  ENDMETHOD.

  METHOD authenticate_ident_api_basic.
    DATA: ls_basicauthpayload TYPE z100085_zif_proubc_ident=>authenticationrequest,
          lo_http_client      TYPE REF TO if_http_client,
          lo_ident_api        TYPE REF TO Z100085_zif_proubc_ident.


    cl_http_client=>create_by_url(
      EXPORTING
        url                = 'https://ident.provide.services'
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      " error handling
    ENDIF.

    lo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.
    lo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client value = '100' ).


    lo_ident_api = NEW Z100085_zcl_proubc_ident( ii_client = lo_http_client iv_tenant = 'e41dea7b-3510-4ffa-8ff4-53f3b158c8b4' iv_refreshtoken = ''  ).
    lo_ident_api->authentication( EXPORTING body = ls_basicauthpayload
                                  IMPORTING apiresponse = authtoken  ).

  ENDMETHOD.
  METHOD baseline_health_check.
    DATA:
      lo_http_client  TYPE REF TO if_http_client,
      lo_baseline_api TYPE REF TO z100085_zif_proubc_baseline,
      lv_authreq      TYPE z100085_zif_proubc_baseline=>authenticationrequest,
      lv_tenant_jwt   TYPE REF TO data,
      lv_tenant       TYPE z100085_prvdtenantid,
      lv_baseline_jwt TYPE REF TO data,
      lv_code         TYPE i,
      lv_bpiendpoint  TYPE string.

    "TODO improve the error handling for invalid tenant ids
    "get the current auth token
    "for testing purposes 'e41dea7b-3510-4ffa-8ff4-53f3b158c8b4'
    lv_tenant = iv_tenant.
    me->call_ident_api( EXPORTING iv_tenant = lv_tenant
                        IMPORTING ev_authtoken = lv_tenant_jwt
                                  ev_bpiendpoint = lv_bpiendpoint  ).

    FIELD-SYMBOLS: <fs_authreq>  TYPE any,
                   <fs_authreq2> TYPE string.

    IF lv_tenant_jwt IS NOT INITIAL.
      TRY.
          ASSIGN lv_tenant_jwt->* TO FIELD-SYMBOL(<ls_data>). "dereference into field symbol
          ASSIGN COMPONENT 'ACCESS_TOKEN' OF STRUCTURE <ls_data> TO <fs_authreq>.
          ASSIGN <fs_authreq>->* TO <fs_authreq2>.
          lv_authreq = <fs_authreq2>.
          "assign <fs_authreq>->* to lv_authenticationrequest.
          "lv_authenticationrequest = <fs_authreq>.

          "TODO check validity of current auth token
          "or call me-call_ident_api to get new auth token

          cl_http_client=>create_by_url(
          EXPORTING
            url                = lv_bpiendpoint
          IMPORTING
            client             = lo_http_client
          EXCEPTIONS
            argument_not_found = 1
            plugin_not_active  = 2
            internal_error     = 3
            OTHERS             = 4 ).
          IF sy-subrc <> 0.
            " error handling
          ENDIF.

          lo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.
          lo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client value = '100' ).

          lo_baseline_api = NEW z100085_zcl_proubc_baseline( ii_client = lo_http_client ).

          "lv_authenticationrequest-

          lv_baseline_jwt = lo_baseline_api->bearerauthentication( EXPORTING body = lv_authreq iv_tenantid = lv_tenant IMPORTING code = lv_code  ).
*    CATCH cx_static_check.

          IF lv_code = 201.
            ev_isreachable = 'X'.
          ELSE.
            ev_isreachable = '-'.
          ENDIF.
        CATCH cx_root.
          ev_isreachable = '-'.
      ENDTRY.
    ELSE.
      ev_isreachable = '-'.
    ENDIF.


  ENDMETHOD.


  METHOD setup_protocol_msg.
    "resolve tenant
    DATA:
      lo_http_client  TYPE REF TO if_http_client,
      lo_baseline_api TYPE REF TO z100085_zif_proubc_baseline,
      lv_authreq      TYPE z100085_zif_proubc_baseline=>authenticationrequest,
      lv_tenant_jwt   TYPE REF TO data,
      lv_tenant       TYPE z100085_prvdtenantid,
      lv_baseline_jwt TYPE REF TO data,
      lv_code         TYPE i,
      lv_bpiendpoint  TYPE string.

    FIELD-SYMBOLS: <fs_authreq>  TYPE any,
                   <fs_authreq2> TYPE string.


    me->call_ident_api( EXPORTING iv_tenant = lv_defaulttenant
                      IMPORTING ev_authtoken = lv_tenant_jwt
                                ev_bpiendpoint = lv_bpiendpoint  ).
    lv_defaultidenttoken = lv_tenant_jwt.

    IF lv_tenant_jwt IS NOT INITIAL.
      TRY.
          ASSIGN lv_tenant_jwt->* TO FIELD-SYMBOL(<ls_data>). "dereference into field symbol
          ASSIGN COMPONENT 'ACCESS_TOKEN' OF STRUCTURE <ls_data> TO <fs_authreq>.
          ASSIGN <fs_authreq>->* TO <fs_authreq2>.
          lv_authreq = <fs_authreq2>.
          "assign <fs_authreq>->* to lv_authenticationrequest.
          "lv_authenticationrequest = <fs_authreq>.

          "TODO check validity of current auth token
          "or call me-call_ident_api to get new auth token

          cl_http_client=>create_by_url(
          EXPORTING
            url                = lv_bpiendpoint
          IMPORTING
            client             = lo_http_client
          EXCEPTIONS
            argument_not_found = 1
            plugin_not_active  = 2
            internal_error     = 3
            OTHERS             = 4 ).
          IF sy-subrc <> 0.
            " error handling
          ENDIF.

          lo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.
          lo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client value = '100' ).

          lo_baseline_api = NEW z100085_zcl_proubc_baseline( ii_client = lo_http_client ).
          lo_baseline_client = lo_baseline_api.

          lv_baseline_jwt = lo_baseline_api->bearerauthentication( EXPORTING body = lv_authreq iv_tenantid = lv_tenant IMPORTING code = lv_code  ).
          lv_defaultbaselinetoken = lv_baseline_jwt.
        CATCH cx_root.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD send_protocol_msg.

    TRY.
        lo_baseline_client->send_protocol_msg( EXPORTING body = body IMPORTING statuscode = statuscode ).
      CATCH cx_static_check.
        "wat do
    ENDTRY.
*    CATCH cx_static_check.

  ENDMETHOD.
ENDCLASS.
