CLASS zcl_prvd_api_helper DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! Constructs the refresh token from 2 parts into one and provides the length
    CLASS-METHODS build_refresh_token
      IMPORTING
        !iv_refreshtoken1 TYPE string
        !iv_refreshtoken2 TYPE string
      EXPORTING
        !ev_tokenlength   TYPE int4
        !ev_refreshtoken  TYPE zprvdrefreshtoken .
    "! Utillity method to copy from type any to type ref to data
    CLASS-METHODS copy_data_to_ref
      IMPORTING
        !is_data TYPE any
      CHANGING
        !cr_data TYPE REF TO data .
    "! Generic method to perform SAP authority checks for PRVD stack API activities
    CLASS-METHODS prvd_tenant_sap_authcheck
      IMPORTING
        !iv_tenant TYPE zprvdtenantid.
    "! Derives the PRVD subject account id from PRVD Org and PRVD Workgroup ID
    "! @parameter iv_organization | PRVD Org
    "! @parameter iv_workgroup_Id | PRVD workgroup ID
    "! @parameter ev_subject_account_id | PRVD Subject Account ID - derived from given info
    CLASS-METHODS get_subject_account_id
      IMPORTING
        !iv_organization       TYPE zprvdtenantid
        !iv_workgroup_id       TYPE zprvdtenantid
      EXPORTING
        !ev_subject_account_id TYPE zprvdtenantid.
    "! Method to create an instance of the API Helper object
    "! @parameter iv_tenant | PRVD Org ID
    "! @parameter iv_subject_acct_id  | PRVD Subject Account ID
    "! @parameter iv_workgroup_id | PRVD Workgroup ID
    METHODS constructor
      IMPORTING
        !iv_tenant          TYPE zprvdtenantid OPTIONAL
        !iv_subject_acct_id TYPE zprvdtenantid OPTIONAL
        !iv_workgroup_id    TYPE zprvdtenantid OPTIONAL.
    "! Method to retrieve access token from PRVD Ident API using user's PRVD subject account
    "! @parameter iv_tenant | PRVD Org ID
    "! @parameter iv_subjacct | PRVD Subject Account ID
    "! @parameter iv_wrkgrpid | PRVD Workgroup ID
    "! @parameter ev_authtoken | Access token structure
    "! @parameter status | HTTP status code
    "! @parameter ev_bpiendpoint | BPI Endpoint host
    METHODS call_ident_api
      IMPORTING
        !iv_tenant      TYPE zprvdtenantid OPTIONAL
        !iv_subjacct    TYPE zprvdtenantid OPTIONAL
        !iv_wrkgrpid    TYPE zprvdtenantid OPTIONAL
      EXPORTING
        !ev_authtoken   TYPE REF TO data
        !status         TYPE i
        !ev_bpiendpoint TYPE string .
    "! Method to check if configured PRVD BPI endpoint is currently reachable
    METHODS baseline_health_check
      IMPORTING
        !iv_tenant      TYPE zprvdtenantid OPTIONAL
        iv_subjacct     TYPE zprvdtenantid OPTIONAL
      EXPORTING
        !ev_isreachable TYPE boolean .
    "! Ensures the API helper class is properly initialized to send PRVD Baseline protocol messages
    METHODS setup_protocol_msg
      EXPORTING
        !setup_success TYPE boolean .
    "! Emits PRVD Baseline protocol message
    METHODS send_protocol_msg
      IMPORTING
        !is_body           TYPE zif_prvd_baseline=>protocolmessage_req
      EXPORTING
        !ev_statuscode     TYPE i
        !ev_apiresponsestr TYPE string
        !ev_apiresponse    TYPE REF TO data .
    "! Method that attempts to resolve a PRVD tenant to the user based upon available data
    METHODS get_default_tenant
      RETURNING
        VALUE(ev_defaulttenant) TYPE zprvdtenantid .
    "! Method to return the PRVD Baseline BPI endpoint to be used
    METHODS get_default_tenant_bpiendpoint
      RETURNING
        VALUE(ev_bpiendpoint) TYPE zprvdtenants-bpi_endpoint .
    METHODS build_dummy_idoc_protocol_msg
      RETURNING
        VALUE(es_dummy_idoc_msg) TYPE zif_prvd_baseline=>protocolmessage_req .
    "! Lists the BPI accounts available to the user
    METHODS list_bpi_accounts .
    "! Gets the current subject account
    METHODS get_subject_account RETURNING VALUE(rv_subject_account) TYPE zprvdtenantid.
    "! Gets the current workgroup
    METHODS get_workgroup RETURNING VALUE(rv_workgroup) TYPE zprvdtenantid.
    "! Method to return PRVD Nchain helper class
    METHODS get_nchain_helper EXPORTING eo_prvd_nchain_helper TYPE REF TO zcl_prvd_nchain_helper.
    "! Method to return the access token
    METHODS get_access_token RETURNING VALUE(rv_access_token) type zprvdrefreshtoken.
  PROTECTED SECTION.
    DATA: mv_defaulttenant        TYPE zprvdtenants-organization_id,
          mv_defaultsubjectacct   TYPE zprvdtenantid,
          mv_selected_workgroupid TYPE zprvdtenantid,
          mv_defaultidenttoken    TYPE REF TO data,
          mv_defaultbaselinetoken TYPE REF TO data,
          mv_bpitoken             TYPE zprvdrefreshtoken,
          mv_default_bpiendpoint  TYPE string,
          mo_ident_client         TYPE REF TO zif_prvd_ident,
          mo_baseline_client      TYPE REF TO zif_prvd_baseline.
    METHODS set_default_tenant IMPORTING iv_defaulttenant TYPE zprvdtenants-organization_id OPTIONAL.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_prvd_api_helper IMPLEMENTATION.


  METHOD baseline_health_check.
    DATA:
      lo_http_client  TYPE REF TO if_http_client,
      lo_baseline_api TYPE REF TO zif_prvd_baseline,
      lv_authreq      TYPE zprvdrefreshtoken,
      lv_tenant_jwt   TYPE REF TO data,
      lv_tenant       TYPE zprvdtenantid,
      lv_subjacct     TYPE zprvdtenants-subject_account_id,
      lv_baseline_jwt TYPE REF TO data,
      lv_code         TYPE i,
      lv_bpiendpoint  TYPE string.

    FIELD-SYMBOLS: <fs_authreq>  TYPE any,
                   <fs_authreq2> TYPE string.

    "TODO improve the error handling for invalid tenant ids
    "get the current auth token
    lv_tenant = iv_tenant.
    IF lv_tenant IS INITIAL.
      lv_tenant = mv_defaulttenant.
    ENDIF.
    lv_subjacct = iv_subjacct.
    IF lv_subjacct IS INITIAL.
      lv_subjacct = mv_defaultsubjectacct.
    ENDIF.
    call_ident_api( EXPORTING iv_tenant      = lv_tenant
                              iv_subjacct    = mv_defaultsubjectacct
                    IMPORTING ev_authtoken   = lv_tenant_jwt
                              ev_bpiendpoint = lv_bpiendpoint ).



    IF lv_tenant_jwt IS NOT INITIAL.
      TRY.
          ASSIGN lv_tenant_jwt->* TO FIELD-SYMBOL(<ls_data>).
          IF sy-subrc <> 0.
          ENDIF.
          ASSIGN COMPONENT 'ACCESS_TOKEN' OF STRUCTURE <ls_data> TO <fs_authreq>.
          IF sy-subrc <> 0.
          ENDIF.
          ASSIGN <fs_authreq>->* TO <fs_authreq2>.
          IF sy-subrc <> 0.
          ENDIF.
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
          lo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client
                                                     value = '100' ).

          lo_baseline_api = NEW zcl_prvd_baseline( ii_client        = lo_http_client
                                                     iv_bpitenant_url = lv_bpiendpoint
                                                     iv_bpitoken      = lv_authreq ).

* raises exception when docker instance of configured bpi instance is down
          TRY.
              lo_baseline_api->status(
                IMPORTING
                  statuscode = lv_code ).
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
    DATA ls_dummy_idoc_protocol_msg TYPE zif_prvd_baseline=>protocolmessage_req.
    SELECT docnum,
           idoctp,
           status,
           credat,
           cretim,
           upddat,
       updtim
       FROM edidc
       UP TO 1 ROWS
       INTO TABLE @DATA(lt_selected_idocs)
       WHERE direct = '1'
       AND status = '03'
       AND mestyp = 'ORDERS'
       AND idoctp = 'ORDERS05'
              ORDER BY PRIMARY KEY.

    IF sy-subrc <> 0.
      "raise message no idocs available
      RETURN.
    ENDIF.

    READ TABLE lt_selected_idocs INTO DATA(wa_selected_idoc) INDEX 1.
    IF sy-subrc <> 0.
      "no idocs available
    ENDIF.

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
    IF sy-subrc <> 0.
      "Message error reading idoc
    ENDIF.

    ls_dummy_idoc_protocol_msg-payload_mimetype = 'json'.
    ls_dummy_idoc_protocol_msg-type = 'ORDERS05'.

    zcl_prvd_idochlpr=>get_dummy_objid( EXPORTING iv_schema = 'ORDERS05'
                   IMPORTING ev_objid = ls_dummy_idoc_protocol_msg-id
                             ev_newidocnum = lv_newidocnum
                    CHANGING ct_edidd = lt_edidd ).

    DATA lv_idocjson TYPE string.


    es_dummy_idoc_msg = ls_dummy_idoc_protocol_msg.
  ENDMETHOD.


  METHOD build_refresh_token.
    CONCATENATE iv_refreshtoken1 iv_refreshtoken2 INTO ev_refreshtoken.
  ENDMETHOD.


  METHOD call_ident_api.
    DATA:
      lo_http_client      TYPE REF TO if_http_client,
      lo_ident_api        TYPE REF TO zif_prvd_ident,
      ls_prvdtenant       TYPE zprvdtenants,
      lv_refreshtokenstr  TYPE zprvdrefreshtoken,
      lv_identurl         TYPE string,
      lv_apiresponse      TYPE REF TO data,
      lv_tenant           TYPE zprvdtenantid,
      lv_subjacct         TYPE zprvdtenants-subject_account_id,
      lv_authtokenreqbody TYPE zif_prvd_ident=>refresh_accesstoken_request.

    IF iv_tenant IS NOT INITIAL.
      lv_tenant = iv_tenant.
    ELSE.
      lv_tenant = mv_defaulttenant.
    ENDIF.

    IF iv_subjacct IS NOT INITIAL.
      lv_subjacct = iv_subjacct.
    ELSE.
      lv_subjacct = mv_defaultsubjectacct.
    ENDIF.

    "todo add subject account id
    SELECT SINGLE * FROM zprvdtenants INTO ls_prvdtenant WHERE organization_id = lv_tenant
      AND subject_account_id = lv_subjacct.

    "todo send error message, org not found
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

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
      RETURN.
    ENDIF.



    lo_http_client->propertytype_accept_cookie       = if_http_client=>co_enabled.
    lo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client
                                               value = '100' ).

    lo_ident_api = NEW zcl_prvd_ident( ii_client       = lo_http_client
                                         iv_tenant       = lv_tenant
                                         iv_refreshtoken = lv_refreshtokenstr ).
    mo_ident_client = lo_ident_api.



    lv_authtokenreqbody-organization_id = lv_tenant.
    lv_authtokenreqbody-grant_type = 'refresh_token'.
    lo_ident_api->refresh_access_token(
      EXPORTING
        body        = lv_authtokenreqbody
      IMPORTING
        status      = status
        apiresponse = lv_apiresponse   ).
    ev_authtoken = lv_apiresponse.
  ENDMETHOD.


  METHOD constructor.
    "todo more robust selection criteria - based on sap user authorization/mapping to tenant

    "option 1 create ZPRVDTENANT parameter id - and auth check it whenever used

    mv_selected_workgroupid = iv_workgroup_id.

    "try using the tenant id provided by user
    IF iv_tenant IS NOT INITIAL AND iv_subject_acct_id IS NOT INITIAL.
      SELECT organization_id,
             subject_account_id,
             workgroup_id,
             bpi_endpoint
          FROM zprvdtenants
          INTO TABLE @DATA(lt_defaultorg)
          WHERE organization_id = @iv_tenant
          AND subject_account_id = @iv_subject_acct_id
          ORDER BY PRIMARY KEY.
      IF sy-subrc = 0.
        READ TABLE lt_defaultorg INDEX 1 INTO DATA(wa_defaulttenant).
        IF sy-subrc = 0.
          mv_defaulttenant = wa_defaulttenant-organization_id.
          mv_defaultsubjectacct = wa_defaulttenant-subject_account_id.
          mv_selected_workgroupid = wa_defaulttenant-workgroup_id.
          mv_default_bpiendpoint = wa_defaulttenant-bpi_endpoint.
          RETURN.
        ELSE.
          "Message no default org determined
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT organization_id,
           subject_account_id,
           workgroup_id,
           bpi_endpoint
        FROM zprvdtenants
        INTO TABLE @lt_defaultorg
        UP TO 1 ROWS
        ORDER BY created_at DESCENDING.
    IF sy-subrc = 0.
      READ TABLE lt_defaultorg INDEX 1 INTO wa_defaulttenant.
      IF sy-subrc = 0.
        mv_defaulttenant = wa_defaulttenant-organization_id.
        mv_defaultsubjectacct  = wa_defaulttenant-subject_account_id.
        mv_selected_workgroupid = wa_defaulttenant-workgroup_id.
        mv_default_bpiendpoint = wa_defaulttenant-bpi_endpoint.
      ENDIF.
    ELSE.
      MESSAGE e010(zclproubcmsg) WITH iv_tenant iv_subject_acct_id mv_selected_workgroupid.
    ENDIF.

    "core authority check - is user allowed to use this subject account
    AUTHORITY-CHECK OBJECT 'ZPRVDTENAN' ID 'ACTVT' FIELD '16'
      ID 'ZPRVDSHA1' FIELD mv_defaultsubjectacct.
    IF sy-subrc <> 0.
      MESSAGE e012(zclproubcmsg) WITH sy-uname mv_defaultsubjectacct mv_defaulttenant mv_selected_workgroupid.
    ENDIF.

  ENDMETHOD.


  METHOD copy_data_to_ref.
    FIELD-SYMBOLS
                 <ls_data> TYPE any.

    CREATE DATA cr_data LIKE is_data.
    ASSIGN cr_data->* TO <ls_data>.
    IF sy-subrc = 0.
      <ls_data> = is_data.
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD get_default_tenant.
    "TODO add authorization check and mapping to sap user id
    ev_defaulttenant = mv_defaulttenant.
  ENDMETHOD.


  METHOD get_default_tenant_bpiendpoint.
    "TODO add authorization check and mapping to sap user id
    ev_bpiendpoint = mv_default_bpiendpoint.
  ENDMETHOD.


  METHOD list_bpi_accounts.
    TRY.
        mo_baseline_client->listaccounts( ).
      CATCH cx_static_check.
    ENDTRY.
  ENDMETHOD.


  METHOD send_protocol_msg.
    DATA ls_finalized_protocol_msg TYPE zif_prvd_baseline=>protocolmessage_req.

    ls_finalized_protocol_msg                    = is_body.
    ls_finalized_protocol_msg-subject_account_id = mv_defaultsubjectacct.
    ls_finalized_protocol_msg-workgroup_id       = mv_selected_workgroupid.
    TRY.
        mo_baseline_client->send_protocol_msg( EXPORTING iv_body        = ls_finalized_protocol_msg
                                                         iv_bpitoken    = mv_bpitoken
                                               IMPORTING ev_statuscode     = ev_statuscode
                                                         ev_apiresponsestr = ev_apiresponsestr
                                                         ev_apiresponse    = ev_apiresponse ).
      CATCH cx_static_check.
    ENDTRY.
  ENDMETHOD.


  METHOD setup_protocol_msg.
    "resolve tenant
    DATA:
      lo_http_client  TYPE REF TO if_http_client,
      lo_baseline_api TYPE REF TO zif_prvd_baseline,
      lv_authreq      TYPE zprvdrefreshtoken,
      lv_tenant_jwt   TYPE REF TO data,
      lv_tenant       TYPE zprvdtenantid,
      lv_baseline_jwt TYPE REF TO data,
      lv_ident_code   TYPE i,
      lv_code         TYPE i,
      lv_bpiendpoint  TYPE string.

    FIELD-SYMBOLS: <fs_authreq>  TYPE any,
                   <fs_authreq2> TYPE string.

    call_ident_api( EXPORTING iv_tenant      = mv_defaulttenant
                              iv_subjacct    = mv_defaultsubjectacct
                    IMPORTING ev_authtoken   = lv_tenant_jwt
                              ev_bpiendpoint = lv_bpiendpoint ).
    mv_defaultidenttoken = lv_tenant_jwt.

    IF lv_tenant_jwt IS NOT INITIAL.
      TRY.
          ASSIGN lv_tenant_jwt->* TO FIELD-SYMBOL(<ls_data>).
          IF sy-subrc <> 0.
          ENDIF.
          ASSIGN COMPONENT 'ACCESS_TOKEN' OF STRUCTURE <ls_data> TO <fs_authreq>.
          IF sy-subrc <> 0.
          ENDIF.
          ASSIGN <fs_authreq>->* TO <fs_authreq2>.
          IF sy-subrc <> 0.
          ENDIF.
          mv_bpitoken  = <fs_authreq2>.

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
          lo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client
                                                     value = '100' ).

          mo_baseline_client = NEW zcl_prvd_baseline( ii_client        = lo_http_client
                                                        iv_bpitenant_url = lv_bpiendpoint
                                                        iv_bpitoken      = mv_bpitoken ).
          IF sy-subrc = 0.
            setup_success = 'X'.
          ELSE.
            "failed setup
            setup_success = '-'.
          ENDIF.
        CATCH cx_root.
      ENDTRY.
    ELSE.
      "failed setup
      setup_success = '-'.
    ENDIF.
  ENDMETHOD.


  METHOD set_default_tenant.
    "TODO add authorization check and mapping to sap user id
    mv_defaulttenant = iv_defaulttenant.
  ENDMETHOD.


  METHOD prvd_tenant_sap_authcheck.
    DATA lo_digest TYPE REF TO cl_abap_message_digest.
    DATA lv_hash_string TYPE zcasesensitivesha1.
    DATA lv_hash_base64 TYPE string.
    IF iv_tenant IS NOT INITIAL.
* create a message digest object with a given hash algo
      lo_digest = cl_abap_message_digest=>get_instance( 'sha1' ).
      lo_digest->update( if_data = cl_abap_message_digest=>string_to_xstring( |{ iv_tenant }| ) ).
      lo_digest->digest( ).
      lv_hash_string = lo_digest->to_string( ).

      AUTHORITY-CHECK OBJECT 'zprvdtenan'
        ID 'ACTVT' FIELD '16'
        ID 'ZPRVDSHA1' FIELD lv_hash_string.
      IF sy-subrc <> 0.
      ENDIF.
    ELSE.
      "message no subject account determined
    ENDIF.
  ENDMETHOD.


  METHOD get_nchain_helper.
    DATA lo_prvd_nchain_helper TYPE REF TO zcl_prvd_nchain_helper.
    lo_prvd_nchain_helper = NEW zcl_prvd_nchain_helper( io_prvd_api_helper = me ).
    eo_prvd_nchain_helper = lo_prvd_nchain_helper.
  ENDMETHOD.


  METHOD get_subject_account_id.
    DATA: lv_unhashed_subject_account_id TYPE string,
          lv_hashed_subject_account_id   TYPE string,
          lo_digest                      TYPE REF TO cl_abap_message_digest.
    CONCATENATE iv_organization iv_workgroup_id INTO lv_unhashed_subject_account_id SEPARATED BY '.'.

    lo_digest = cl_abap_message_digest=>get_instance( 'sha256' ).
    lo_digest->update( if_data = cl_abap_message_digest=>string_to_xstring( |{ lv_unhashed_subject_account_id }| ) ).
    lo_digest->digest( ).
    lv_hashed_subject_account_id = lo_digest->to_string( ).

    ev_subject_account_id = lv_hashed_subject_account_id.
  ENDMETHOD.

  METHOD get_subject_account.
    rv_subject_account = mv_defaultsubjectacct.
  ENDMETHOD.
  METHOD get_workgroup.
    rv_workgroup = mv_selected_workgroupid.
  ENDMETHOD.

  method get_access_token.
    rv_access_token = mv_bpitoken.
  ENDMETHOD.
ENDCLASS.
