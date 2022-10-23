CLASS zcl_proubc_api_helper DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS build_refresh_token
      IMPORTING
        !iv_refreshtoken1 TYPE string
        !iv_refreshtoken2 TYPE string
      EXPORTING
        !ev_tokenlength   TYPE int4
        !ev_refreshtoken  TYPE zPRVDREFRESHTOKEN .
    CLASS-METHODS copy_data_to_ref
      IMPORTING
        !is_data TYPE any
      CHANGING
        !cr_data TYPE REF TO data .
    CLASS-METHODS prvd_tenant_sap_authcheck
      IMPORTING
        !iv_tenant TYPE zPRVDTENANTID.
    METHODS constructor
      IMPORTING
        !iv_tenant TYPE zPRVDTENANTID OPTIONAL
        !iv_subject_acct_id TYPE zprvdtenantid OPTIONAL
        !iv_workgroup_id type zprvdtenantid OPTIONAL.
    METHODS call_ident_api
      IMPORTING
        !iv_tenant      TYPE zPRVDTENANTID
        !iv_subjacct    TYPE zprvdtenantid
        !iv_wrkgrpid    type zprvdtenantid OPTIONAL
      EXPORTING
        !ev_authtoken   TYPE REF TO data
        !status         TYPE i
        !ev_bpiendpoint TYPE string .
    METHODS baseline_health_check
      IMPORTING
        !iv_tenant      TYPE zPRVDTENANTID optional
        iv_subjacct    TYPE zprvdtenantid OPTIONAl
      EXPORTING
        !ev_isreachable TYPE boolean .
    METHODS setup_protocol_msg
      EXPORTING
        !setup_success TYPE boolean .
    METHODS send_protocol_msg
      IMPORTING
        !body           TYPE zif_proubc_baseline=>protocolmessage_req
      EXPORTING
        !statuscode     TYPE i
        !apiresponsestr TYPE string
        !apiresponse    TYPE REF TO data .
    METHODS send_bpiobjects_msg
      IMPORTING
        !body           TYPE zif_proubc_baseline=>bpiobjects_req
      EXPORTING
        !statuscode     TYPE i
        !apiresponsestr TYPE string
        !apiresponse    TYPE REF TO data .
    METHODS create_businessobjects_msg
      IMPORTING
        !body           TYPE zif_proubc_baseline=>businessobject
      EXPORTING
        !statuscode     TYPE i
        !apiresponsestr TYPE string
        !apiresponse    TYPE REF TO data .
    METHODS get_default_tenant
      RETURNING
        VALUE(ev_defaulttenant) TYPE ZprvdtenantID .
    METHODS get_default_tenant_bpiendpoint
      RETURNING
        VALUE(ev_bpiendpoint) TYPE Zprvdtenants-bpi_endpoint .
    METHODS build_dummy_idoc_protocol_msg
      RETURNING
        VALUE(es_dummy_idoc_msg) TYPE zif_proubc_baseline=>protocolmessage_req .
    METHODS list_bpi_accounts .
    methods get_nchain_helper EXPORTING eo_prvd_nchain_helper type REF TO zcl_proubc_nchain_helper.

  PROTECTED SECTION.
    DATA: lv_defaulttenant        TYPE zprvdtenants-organization_id,
          lv_defaultsubjectacct   TYPE zprvdtenantid,
          lv_selected_workgroupid type zprvdtenantid,
          lv_defaultidenttoken    TYPE REF TO data,
          lv_defaultbaselinetoken TYPE REF TO data,
          lv_bpitoken             TYPE zprvdrefreshtoken,
          lv_default_bpiendpoint  TYPE string,
          lo_ident_client         TYPE REF TO zif_proubc_ident,
          lo_baseline_client      TYPE REF TO zif_proubc_baseline.
    METHODS: set_default_tenant IMPORTING iv_defaulttenant TYPE zprvdtenants-organization_id OPTIONAL.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_proubc_api_helper IMPLEMENTATION.


  METHOD baseline_health_check.
    DATA:
      lo_http_client  TYPE REF TO if_http_client,
      lo_baseline_api TYPE REF TO zif_proubc_baseline,
      lv_authreq      TYPE zprvdrefreshtoken,
      lv_tenant_jwt   TYPE REF TO data,
      lv_tenant       TYPE zprvdtenantid,
      lv_subjacct     type zprvdtenants-subject_account_id,
      lv_baseline_jwt TYPE REF TO data,
      lv_code         TYPE i,
      lv_bpiendpoint  TYPE string.

    "TODO improve the error handling for invalid tenant ids
    "get the current auth token
    lv_tenant = iv_tenant.
    IF lv_tenant IS INITIAL.
      lv_tenant = lv_defaulttenant.
    ENDIF.
    lv_subjacct = iv_subjacct.
    IF lv_subjacct IS INITIAL.
      lv_subjacct = lv_defaultsubjectacct.
    ENDIF.
    me->call_ident_api( EXPORTING iv_tenant = lv_tenant
                                  iv_subjacct = lv_defaultsubjectacct
                        IMPORTING ev_authtoken = lv_tenant_jwt
                                  ev_bpiendpoint = lv_bpiendpoint  ).

    FIELD-SYMBOLS: <fs_authreq>  TYPE any,
                   <fs_authreq2> TYPE string.

    IF lv_tenant_jwt IS NOT INITIAL.
      TRY.
          ASSIGN lv_tenant_jwt->* TO FIELD-SYMBOL(<ls_data>).
          ASSIGN COMPONENT 'ACCESS_TOKEN' OF STRUCTURE <ls_data> TO <fs_authreq>.
          ASSIGN <fs_authreq>->* TO <fs_authreq2>.
          lv_authreq = <fs_authreq2>.

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

          lo_baseline_api = NEW zcl_proubc_baseline( ii_client = lo_http_client iv_bpitenant_url = lv_bpiendpoint iv_bpitoken = lv_authreq ).

* raises exception when docker instance of configured bpi instance is down
          TRY.
              lo_baseline_api->status(
                IMPORTING
                  statuscode = lv_code
              ).
            CATCH cx_static_check.
              ev_isreachable = '-'.
            CATCH cx_root.
              ev_isreachable = '-'.
          ENDTRY.

          IF lv_code = 200 OR lv_code = 204.
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


  METHOD build_dummy_idoc_protocol_msg.
    DATA ls_dummy_idoc_protocol_msg TYPE zif_proubc_baseline=>protocolmessage_req.
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

    zcl_proubc_idochlpr=>get_DUMMY_objid( EXPORTING iv_schema = 'ORDERS05'
                   IMPORTING ev_objid = ls_dummy_idoc_protocol_msg-id
                             ev_newidocnum = lv_newidocnum
                    CHANGING ct_edidd = lt_edidd ).

    DATA: lv_idocjson TYPE string.
    lv_idocjson = /ui2/cl_json=>serialize(
       EXPORTING
         data             = lt_edidd
     ).
    ls_dummy_idoc_protocol_msg-payload = lv_idocjson.

    es_dummy_idoc_msg = ls_dummy_idoc_protocol_msg.
  ENDMETHOD.


  METHOD build_refresh_token.
    CONCATENATE iv_refreshtoken1 iv_refreshtoken2 INTO ev_refreshtoken.
  ENDMETHOD.


  METHOD call_ident_api.
    DATA:
      lo_http_client     TYPE REF TO if_http_client,
      lo_ident_api       TYPE REF TO zif_proubc_ident,
      ls_prvdtenant      TYPE zprvdtenants,
      lv_refreshtokenstr TYPE zprvdrefreshtoken,
      lv_identurl        TYPE string,
      lv_apiresponse     TYPE REF TO data,
      lv_tenant          TYPE zprvdtenantid,
       lv_subjacct       TYPE zprvdtenants-subject_account_id.

    IF iv_tenant IS NOT INITIAL.
      lv_tenant = iv_tenant.
    ELSE.
      lv_tenant = lv_defaulttenant.
    ENDIF.

        IF iv_subjacct IS NOT INITIAL.
      lv_subjacct = iv_subjacct.
    ELSE.
      lv_subjacct = lv_defaultsubjectacct.
    ENDIF.

    "todo add subject account id
    SELECT SINGLE * FROM zprvdtenants INTO ls_prvdtenant WHERE organization_id = lv_tenant
    AND subject_account_id = lv_subjacct.

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

    lo_ident_api = NEW zcl_proubc_ident( ii_client = lo_http_client iv_tenant = lv_tenant iv_refreshtoken = lv_refreshtokenstr  ).
    lo_ident_client = lo_ident_api.

    DATA: authtokenreqbody TYPE zif_proubc_ident=>refresh_accesstoken_request.

    authtokenreqbody-organization_id = lv_tenant.
    authtokenreqbody-grant_type = 'refresh_token'.
    lo_ident_api->refresh_access_token(
      EXPORTING
        body        = authtokenreqbody
      IMPORTING
        status      = status
        apiresponse = lv_apiresponse
    ).
    ev_authtoken = lv_apiresponse.
  ENDMETHOD.


  METHOD constructor.
    "todo more robust selection criteria - based on sap user authorization/mapping to tenant

    "option 1 create ZPRVDTENANT parameter id - and auth check it whenever used

    lv_selected_workgroupid = iv_workgroup_id.

    "try using the tenant id provided by user
    IF iv_tenant IS NOT INITIAL AND iv_subject_acct_id IS NOT INITIAL.
      SELECT organization_id,
             subject_account_id,
             bpi_endpoint
          FROM zprvdtenants
          INTO TABLE @DATA(lt_defaultorg)
          WHERE organization_id = @iv_tenant
          AND   subject_account_id = @iv_subject_acct_id.
      IF sy-subrc = 0.
        READ TABLE lt_defaultorg INDEX 1 INTO DATA(wa_defaulttenant).
        IF sy-subrc = 0.
          lv_defaulttenant = wa_defaulttenant-organization_id.
          lv_defaultsubjectacct = wa_defaulttenant-subject_account_id.
          lv_default_bpiendpoint = wa_defaulttenant-bpi_endpoint.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT organization_id,
           subject_account_id,
           bpi_endpoint
        FROM zprvdtenants
        INTO TABLE @lt_defaultorg
        UP TO 1 ROWS
        ORDER BY created_at DESCENDING.
    IF sy-subrc = 0.
      READ TABLE lt_defaultorg INDEX 1 INTO wa_defaulttenant.
      IF sy-subrc = 0.
        lv_defaulttenant = wa_defaulttenant-organization_id.
        lv_defaultsubjectacct  = wa_defaulttenant-subject_account_id.
        lv_default_bpiendpoint = wa_defaulttenant-bpi_endpoint.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD copy_data_to_ref.
    FIELD-SYMBOLS:
                 <ls_data> TYPE any.

    CREATE DATA cr_data LIKE is_data.
    ASSIGN cr_data->* TO <ls_data>.
    <ls_data> = is_data.

  ENDMETHOD.


  METHOD create_businessobjects_msg.

    TRY.
        lo_baseline_client->createbaselinebusinessobject( EXPORTING body = body
                                               IMPORTING statuscode = statuscode
                                                         apiresponsestr = apiresponsestr
                                                         apiresponse = apiresponse  ).
      CATCH cx_static_check.
    ENDTRY.
  ENDMETHOD.


  METHOD get_default_tenant.
    "TODO add authorization check and mapping to sap user id
    ev_defaulttenant = lv_defaulttenant.
  ENDMETHOD.


  METHOD get_default_tenant_bpiendpoint.
    "TODO add authorization check and mapping to sap user id
    ev_bpiendpoint = lv_default_bpiendpoint.
  ENDMETHOD.


  METHOD list_bpi_accounts.
    TRY.
        lo_baseline_client->listaccounts(
        ).
      CATCH cx_static_check.
    ENDTRY.
  ENDMETHOD.


  METHOD send_bpiobjects_msg.
    TRY.
        lo_baseline_client->send_bpiobjects_msg( EXPORTING body = body
                                               IMPORTING statuscode = statuscode
                                                         apiresponsestr = apiresponsestr
                                                         apiresponse = apiresponse ).
      CATCH cx_static_check.
    ENDTRY.
  ENDMETHOD.


  METHOD send_protocol_msg.
    DATA: ls_finalized_protocol_msg TYPE zif_proubc_baseline=>protocolmessage_req.

          ls_finalized_protocol_msg = body.
          ls_finalized_protocol_msg-subject_account_id = lv_defaultsubjectacct.
    TRY.
        lo_baseline_client->send_protocol_msg( EXPORTING IV_body = ls_finalized_protocol_msg
                                                         IV_bpitoken = lv_bpitoken
                                               IMPORTING statuscode = statuscode
                                                         apiresponsestr = apiresponsestr
                                                         apiresponse = apiresponse ).
      CATCH cx_static_check.
    ENDTRY.
  ENDMETHOD.


  METHOD setup_protocol_msg.
    "resolve tenant
    DATA:
      lo_http_client  TYPE REF TO if_http_client,
      lo_baseline_api TYPE REF TO zif_proubc_baseline,
      lv_authreq      TYPE zprvdrefreshtoken,
      lv_tenant_jwt   TYPE REF TO data,
      lv_tenant       TYPE zprvdtenantid,
      lv_baseline_jwt TYPE REF TO data,
      lv_ident_code   TYPE i,
      lv_code         TYPE i,
      lv_bpiendpoint  TYPE string.

    FIELD-SYMBOLS: <fs_authreq>  TYPE any,
                   <fs_authreq2> TYPE string.



    me->call_ident_api( EXPORTING iv_tenant = lv_defaulttenant
                                  iv_subjacct = lv_defaultsubjectacct
                      IMPORTING ev_authtoken = lv_tenant_jwt
                                ev_bpiendpoint = lv_bpiendpoint  ).
    lv_defaultidenttoken = lv_tenant_jwt.

    IF lv_tenant_jwt IS NOT INITIAL.
      TRY.
          ASSIGN lv_tenant_jwt->* TO FIELD-SYMBOL(<ls_data>). "dereference into field symbol
          ASSIGN COMPONENT 'ACCESS_TOKEN' OF STRUCTURE <ls_data> TO <fs_authreq>.
          ASSIGN <fs_authreq>->* TO <fs_authreq2>.
          lv_bpitoken  = <fs_authreq2>.

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

          lo_baseline_client = NEW zcl_proubc_baseline( ii_client = lo_http_client iv_bpitenant_url = lv_bpiendpoint iv_bpitoken =  lv_bpitoken  ).
          IF sy-subrc = 0.
            setup_success = 'X'.
          ELSE.
            setup_success = '-'.
          ENDIF.
        CATCH cx_root.
      ENDTRY.
    ELSE.
      setup_success = '-'. " failed
    ENDIF.
  ENDMETHOD.


  METHOD set_default_tenant.
    "TODO add authorization check and mapping to sap user id
    lv_defaulttenant = iv_defaulttenant.
  ENDMETHOD.

  METHOD prvd_tenant_sap_authcheck.
    IF iv_tenant IS NOT INITIAL.
      DATA lo_digest TYPE REF TO cl_abap_message_digest.
      DATA lv_hash_string TYPE zcasesensitivesha1.
      DATA lv_hash_base64 TYPE string.

* create a message digest object with a given hash algo
      lo_digest = cl_abap_message_digest=>get_instance( 'sha1' ).
      lo_digest->update( if_data = cl_abap_message_digest=>string_to_xstring( |{ iv_tenant }| ) ).
      lo_digest->digest( ).
      lv_hash_string = lo_digest->to_string( ).

      AUTHORITY-CHECK OBJECT 'zprvdtenan'
        ID 'ACTVT' FIELD '16'
        ID 'ZPRVDSHA1' FIELD lv_hash_string.
      IF sy-subrc = 0.
      ENDIF.

    ELSE.
    ENDIF.
  ENDMETHOD.

  method get_nchain_helper.
    data: lo_prvd_nchain_helper type REF TO zcl_proubc_nchain_helper.
          lo_prvd_nchain_helper ?= me.
          eo_prvd_nchain_helper = lo_prvd_nchain_helper.
  ENDMETHOD.
ENDCLASS.
