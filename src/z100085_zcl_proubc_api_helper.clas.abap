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
      call_ident_api, " IMPORTING iv_tenant TYPE z100085_prvdtenantid,
      call_baseline_api,
      authenticate_ident_api_basic IMPORTING iv_userid   TYPE string
                                             iv_password TYPE Z100085_CASESENSITIVE_STR
                                   EXPORTING authtoken   TYPE REF TO data.
  PROTECTED SECTION.
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
          lo_http_client TYPE REF TO if_http_client,
          lo_ident_api type ref to z100085_zif_proubc_ident,
          ls_prvdtenant type z100085_prvdorgs,
          lv_refreshtokenstr type string.

    z100085_zcl_proubc_prvdtenants=>get_prvdtenant( exporting iv_prvdtenant = 'e41dea7b-3510-4ffa-8ff4-53f3b158c8b4'
                                                    importing ev_prvdtenant = ls_prvdtenant
                                                      ).

    CONCATENATE ls_prvdtenant-refresh_token ls_prvdtenant-refresh_tokenext into lv_refreshtokenstr.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = 'https://ident.provide.services' "TODO use the ident URL here
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

    lo_ident_api = NEW Z100085_zcl_proubc_ident( ii_client = lo_http_client iv_tenant = 'e41dea7b-3510-4ffa-8ff4-53f3b158c8b4' iv_refreshtoken = lv_refreshtokenstr  ).

    data: authtokenreqbody type z100085_zif_proubc_ident=>authorizelong_termtokenrequest.

    authtokenreqbody-organization_id = 'e41dea7b-3510-4ffa-8ff4-53f3b158c8b4'.
    authtokenreqbody-scope = 'offline_access'.

    "lo_ident_api->

    lo_ident_api->authorizelong_termtoken( body = authtokenreqbody  ).
*    CATCH cx_static_check.


    "lo_ident_api->authentication( exporting body = ls_basicauthpayload
    "                              importing apiresponse = authtoken  ).


    "TODO store the auth token securely
  ENDMETHOD.
  METHOD call_baseline_api.
    DATA:
        lo_http_client TYPE REF TO if_http_client,
        lo_baseline_api type ref to z100085_zif_proubc_baseline.
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

    lo_baseline_api = new z100085_zcl_proubc_baseline( ii_client = lo_http_client ).
    "TODO - do something useful with the Baseline API
    "lo_baseline_api->authentication( body =  ).
*    CATCH cx_static_check.
  ENDMETHOD.

  METHOD authenticate_ident_api_basic.
    DATA: ls_basicauthpayload TYPE z100085_zif_proubc_ident=>authenticationrequest,
          lo_http_client TYPE REF TO if_http_client,
          lo_ident_api TYPE REF TO Z100085_zif_proubc_ident.


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
    lo_ident_api->authentication( exporting body = ls_basicauthpayload
                                  importing apiresponse = authtoken  ).

  ENDMETHOD.
ENDCLASS.
