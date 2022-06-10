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
      constructor,
      call_ident_api IMPORTING iv_tenant TYPE z100085_prvdtenantid EXPORTING ev_authtoken TYPE REF TO data
                                                                             status TYPE i
                                                                             ev_bpiendpoint TYPE string,
      call_baseline_api,
      authenticate_ident_api_basic IMPORTING iv_userid   TYPE string
                                             iv_password TYPE z100085_casesensitive_str
                                   EXPORTING authtoken   TYPE REF TO data,
      baseline_health_check IMPORTING iv_tenant TYPE z100085_prvdtenantid EXPORTING ev_isreachable TYPE boolean,
      setup_protocol_msg EXPORTING setup_success TYPE boolean,
      send_protocol_msg IMPORTING body TYPE z100085_zif_proubc_baseline=>protocolmessage_req
                        EXPORTING statuscode TYPE i
                                  apiresponsestr TYPE string
                                  apiresponse TYPE REF TO data,
      get_default_tenant RETURNING VALUE(ev_defaulttenant) TYPE z100085_prvdorgs-organization_id,
      get_default_tenant_bpiendpoint RETURNING VALUE(ev_bpiendpoint) TYPE z100085_prvdorgs-bpi_endpoint,
      build_dummy_idoc_protocol_msg RETURNING VALUE(es_dummy_idoc_msg) TYPE z100085_zif_proubc_baseline=>protocolmessage_req.
  PROTECTED SECTION.
    DATA: lv_defaulttenant        TYPE z100085_prvdorgs-organization_id VALUE 'e41dea7b-3510-4ffa-8ff4-53f3b158c8b4',
          lv_defaultidenttoken    TYPE REF TO data,
          lv_defaultbaselinetoken TYPE REF TO data,
          lv_default_bpiendpoint  TYPE string,
          lo_ident_client         TYPE REF TO Z100085_zif_proubc_ident,
          lo_baseline_client      TYPE REF TO z100085_zif_proubc_baseline.
    METHODS: set_default_tenant IMPORTING iv_defaulttenant TYPE z100085_prvdorgs-organization_id OPTIONAL.
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_api_helper IMPLEMENTATION.
  METHOD constructor.
    "todo more robust selection criteria - based on sap user authorization/mapping to tenant
    SELECT organization_id,
           bpi_endpoint
        FROM z100085_prvdorgs
        INTO TABLE @DATA(lt_defaultorg)
        UP TO 1 ROWS
        ORDER BY created_at DESCENDING.
    IF sy-subrc = 0.
      READ TABLE lt_defaultorg INDEX 1 INTO DATA(wa_defaulttenant).
      IF sy-subrc = 0.
        lv_defaulttenant = wa_defaulttenant-organization_id.
        lv_default_bpiendpoint = wa_defaulttenant-bpi_endpoint.
      ENDIF.
    ENDIF.

  ENDMETHOD.
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
      lv_apiresponse     TYPE REF TO data,
      lv_tenant          TYPE z100085_prvdorgs-organization_id.

    "z100085_zcl_proubc_prvdtenants=>get_prvdtenant( EXPORTING iv_prvdtenant = iv_tenant
    "                                               IMPORTING ev_prvdtenant = ls_prvdtenant
    "                                                 ).

    IF iv_tenant IS NOT INITIAL.
      lv_tenant = iv_tenant.
    ELSE.
      lv_tenant = lv_defaulttenant.
    ENDIF.

    SELECT SINGLE * FROM z100085_prvdorgs INTO ls_prvdtenant WHERE organization_id = lv_tenant.

    CHECK sy-subrc = 0. "todo send error message, org not found

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

    lo_ident_api = NEW Z100085_zcl_proubc_ident( ii_client = lo_http_client iv_tenant = lv_tenant iv_refreshtoken = lv_refreshtokenstr  ).
    lo_ident_client = lo_ident_api.

    DATA: authtokenreqbody TYPE z100085_zif_proubc_ident=>refresh_accesstoken_request.

    authtokenreqbody-organization_id = lv_tenant.
    "authtokenreqbody-scope = ''.
    authtokenreqbody-grant_type = 'refresh_token'.

    "lo_ident_api->

    "lo_ident_api->authorizelong_termtoken( EXPORTING body = authtokenreqbody IMPORTING status = status apiresponse = lv_apiresponse  ).
    lo_ident_api->refresh_access_token(
      EXPORTING
        body        = authtokenreqbody
      IMPORTING
        status      = status
        apiresponse = lv_apiresponse
    ).
*    CATCH cx_static_check.
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

          "lo_baseline_api->
          "lv_baseline_jwt = lo_baseline_api->bearerauthentication( EXPORTING body = lv_authreq iv_tenantid = lv_tenant IMPORTING code = lv_code  ).
*    CATCH cx_static_check.

* why is this raising exceptions?
          lo_baseline_api->status(
            IMPORTING
              statuscode = lv_code
          ).

          IF lv_code = 200 or lv_code = 204.
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
      lv_ident_code   TYPE i,
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
          IF lv_baseline_jwt IS NOT INITIAL.
            setup_success = 'X'.
          ELSE.
            setup_success = '-'.
          ENDIF.
          lv_defaultbaselinetoken = lv_baseline_jwt.
        CATCH cx_root.
      ENDTRY.
    ELSE.
      setup_success = '-'. " failed
    ENDIF.
  ENDMETHOD.
  METHOD send_protocol_msg.

    TRY.
        lo_baseline_client->send_protocol_msg( EXPORTING body = body
                                               IMPORTING statuscode = statuscode
                                                         apiresponsestr = apiresponsestr
                                                         apiresponse = apiresponse ).
      CATCH cx_static_check.
        "wat do
    ENDTRY.
*    CATCH cx_static_check.

  ENDMETHOD.

  METHOD get_default_tenant.
    "TODO add authorization check and mapping to sap user id
    ev_defaulttenant = lv_defaulttenant.
  ENDMETHOD.
  METHOD get_default_tenant_bpiendpoint.
    "TODO add authorization check and mapping to sap user id
    ev_bpiendpoint = lv_default_bpiendpoint.
  ENDMETHOD.
  METHOD set_default_tenant.
    "TODO add authorization check and mapping to sap user id
    lv_defaulttenant = iv_defaulttenant.
  ENDMETHOD.

  METHOD build_dummy_idoc_protocol_msg.
    DATA ls_dummy_idoc_protocol_msg TYPE z100085_zif_proubc_baseline=>protocolmessage_req.
    SELECT docnum,
       idoctp,
       status,
       credat,
       cretim,
       upddat,
       updtim
       FROM edidc
       UP TO 1 ROWS
       "inner join EDID4 as b on a~docnum = b~docnum
       INTO TABLE @DATA(lt_selected_idocs)
       WHERE direct = '1'
       AND status = '03'
       AND mestyp = 'ORDERS'
       AND idoctp = 'ORDERS05'.

    CHECK sy-subrc = 0.

    READ TABLE lt_selected_idocs INTO DATA(wa_selected_idoc) INDEX 1.

    DATA:
      lv_idocnum      TYPE edidc-docnum,
      lv_newidocnum   TYPE   edidc-docnum,
      lt_edids        TYPE TABLE OF edids,
      lt_edidd        TYPE TABLE OF edidd,
      wa_idoc_control TYPE edidc,
      lv_status       TYPE i.

    CLEAR: lt_edids, lt_edidd, lv_idocnum.


    CALL FUNCTION 'IDOC_READ_COMPLETELY'
      EXPORTING
        document_number = wa_selected_idoc-docnum
      IMPORTING
        idoc_control    = wa_idoc_control
      TABLES
        int_edids       = lt_edids
        int_edidd       = lt_edidd
      EXCEPTIONS
        OTHERS          = 1.

    ls_dummy_idoc_protocol_msg-payload_mimetype = 'json'.
    ls_dummy_idoc_protocol_msg-type = 'ORDERS05'.

    z100085_zcl_proubc_idochlpr=>get_DUMMY_objid( EXPORTING iv_schema = 'ORDERS05'
                   IMPORTING ev_objid = ls_dummy_idoc_protocol_msg-id
                             ev_newidocnum = lv_newidocnum
                    CHANGING ct_edidd = lt_edidd ).

    DATA: lv_idocjson TYPE string.
    lv_idocjson = /ui2/cl_json=>serialize(
       EXPORTING
         data             = lt_edidd
*            compress         =
*            name             =
*            pretty_name      =
*            type_descr       =
*            assoc_arrays     =
*            ts_as_iso8601    =
*            expand_includes  =
*            assoc_arrays_opt =
*            numc_as_string   =
*            name_mappings    =
*            conversion_exits =
*          RECEIVING
*            r_json           =
     ).
    ls_dummy_idoc_protocol_msg-payload = lv_idocjson.

    es_dummy_idoc_msg = ls_dummy_idoc_protocol_msg.
  ENDMETHOD.

ENDCLASS.
