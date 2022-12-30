CLASS zcl_prvd_tenants_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! Returns specific PRVD Org table entry per key criteria
    "! TODO add params for workgroup ID, rename to PRVd Org
    CLASS-METHODS get_prvdtenant
      IMPORTING
        !iv_prvdtenant TYPE zprvdtenantid OPTIONAL
        !iv_subjacctid TYPE zprvdtenantid
      EXPORTING
        !ev_prvdtenant TYPE zif_prvd_tenants=>ty_tenant_wo_token .
    "! Returns list of PRVD Org table entries per criteria
    CLASS-METHODS get_allprvdtenant
      EXPORTING
        !et_prvdtenant TYPE zif_prvd_tenants=>tty_tenant_wo_token .
    "! Creates a new entry to the PRVD Tenant table
    CLASS-METHODS create_prvdtenant
      IMPORTING
        !it_prvdtenant TYPE zttprvdtenant
      EXPORTING
        !et_prvdtenant TYPE zttprvdtenant .
    "! Updates the selected PRVD tenant table entry with changed data
    CLASS-METHODS update_prvdtenant
      IMPORTING
        !it_prvdtenant TYPE zttprvdtenant
      EXPORTING
        !et_prvdtenant TYPE zttprvdtenant .
    "! Deletes the selected PRVD tenant table entry
    CLASS-METHODS delete_prvdtenant
      EXPORTING
        !ev_prvdtenantid TYPE zprvdtenantid
        !ev_subj_acct_id TYPE zprvdtenantid.
    METHODS get_refreshtoken
      EXPORTING
        !ev_prvdtenantid TYPE zprvdtenantid .
    METHODS get_authtoken
      EXPORTING
        !ev_prvdtenantid TYPE zprvdtenantid .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_prvd_tenants_helper IMPLEMENTATION.


  METHOD create_prvdtenant.
    CONSTANTS: lc_default_identurl TYPE string VALUE 'https://ident.provide.services',
               lc_default_bpiurl TYPE string VALUE 'https://baseline.provide.services'.
    DATA: ls_prvdtenant         TYPE zprvdtenants,
          lt_prvdtenant         TYPE TABLE OF zprvdtenants,
          lt_existingprvdtenant TYPE TABLE OF zprvdtenants,
          lv_timestamp          TYPE timestampl.
    "TODO add SAP auth check

    CHECK it_prvdtenant IS NOT INITIAL.

    "duplicate check

    "TODO improve data validation process, add subject account id
    "throw out error message if the tenant already exists
*    SELECT * FROM zprvdtenants INTO TABLE lt_existingprvdtenant
*        FOR ALL ENTRIES IN it_prvdtenant WHERE tenant_id = it_prvdtenant-tenant_id.
*    IF sy-dbcnt > 0.
*      MESSAGE e004(zclproubcmsg) WITH 'orgid'.
*    ENDIF.

    GET TIME STAMP FIELD lv_timestamp.

    LOOP AT it_prvdtenant ASSIGNING FIELD-SYMBOL(<fs_prvdtenant>).
      CLEAR: ls_prvdtenant.
      ls_prvdtenant-mandt = sy-mandt.
      ls_prvdtenant-organization_id = <fs_prvdtenant>-organization_id.
      ls_prvdtenant-workgroup_id = <fs_prvdtenant>-workgroup_id.
      ls_prvdtenant-subject_account_id = <fs_prvdtenant>-subject_account_id.
      IF <fs_prvdtenant>-bpi_endpoint IS NOT INITIAL.
        ls_prvdtenant-bpi_endpoint = <fs_prvdtenant>-bpi_endpoint.
      ELSE.
        ls_prvdtenant-bpi_endpoint = lc_default_bpiurl.
      ENDIF.
      IF <fs_prvdtenant>-ident_endpoint IS NOT INITIAL.
        ls_prvdtenant-ident_endpoint = <fs_prvdtenant>-ident_endpoint.
      ELSE.
        ls_prvdtenant-ident_endpoint = lc_default_identurl.
      ENDIF.
      DATA(lv_tokenlength) = strlen( <fs_prvdtenant>-refresh_token ).
      IF lv_tokenlength LE 1024 .
        ls_prvdtenant-refresh_token = <fs_prvdtenant>-refresh_token(lv_tokenlength).
      ELSE.
        ls_prvdtenant-refresh_token = <fs_prvdtenant>-refresh_token(1024).
        ls_prvdtenant-refresh_tokenext = <fs_prvdtenant>-refresh_token+1024(1024).
      ENDIF.
      ls_prvdtenant-createdby = sy-uname.
      ls_prvdtenant-created_at = lv_timestamp.
      APPEND ls_prvdtenant TO lt_prvdtenant.
    ENDLOOP.

    MODIFY zprvdtenants FROM TABLE lt_prvdtenant.
    IF sy-subrc <> 0.
      "TODO add raise exception here
    ELSE.
      MOVE-CORRESPONDING lt_prvdtenant TO et_prvdtenant.
    ENDIF.

  ENDMETHOD.

  METHOD delete_prvdtenant.
    "TODO add SAP auth

    IF ev_prvdtenantid IS NOT INITIAL.
      DELETE FROM zprvdtenants WHERE organization_id = ev_prvdtenantid
                                 AND subject_account_id = ev_subj_acct_id.
      IF sy-subrc = 0.
        "todo Add some logging for this
      ELSE.
      "delete failed. why?
        "TODO raise exception here
      ENDIF.
    ELSE.
      "TODO raise exception msg here
    ENDIF.


  ENDMETHOD.


  METHOD get_allprvdtenant.
    DATA: lt_prvdtenant TYPE TABLE OF zprvdtenants,
          ls_prvdtenant TYPE zif_prvd_tenants=>ty_tenant_wo_token,
          lo_api_helper TYPE REF TO zcl_prvd_api_helper.

    lo_api_helper = NEW zcl_prvd_api_helper( ).
    SELECT * FROM zprvdtenants INTO TABLE lt_prvdtenant.
    IF sy-subrc = 0.
    ELSEIF sy-subrc EQ 4.
    "# can't find it. thats ok
    ELSEIF sy-subrc EQ 8.
    "problem with the db
    ELSE.
    "general error

    ENDIF.

    "TODO only display entries user is authorized to view

    LOOP AT lt_prvdtenant ASSIGNING FIELD-SYMBOL(<fs_prvdtenant>).
      CLEAR ls_prvdtenant.
      ls_prvdtenant-mandt = <fs_prvdtenant>-mandt.
      ls_prvdtenant-organization_id = <fs_prvdtenant>-organization_id.
      ls_prvdtenant-subject_account_id = <fs_prvdtenant>-subject_account_id.
      ls_prvdtenant-ident_endpoint = <fs_prvdtenant>-ident_endpoint.
      ls_prvdtenant-bpi_endpoint = <fs_prvdtenant>-bpi_endpoint.
      ls_prvdtenant-created_by = <fs_prvdtenant>-createdby.
      ls_prvdtenant-created_at = <fs_prvdtenant>-created_at.
      ls_prvdtenant-changed_by = <fs_prvdtenant>-changedby.
      ls_prvdtenant-changed_at = <fs_prvdtenant>-changed_at.
      "ls_prvdtenant-reachable = abap_false. "TODO add call to BPI endpoint
      APPEND ls_prvdtenant TO et_prvdtenant.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_authtoken.
    "use the refresh token to get a new authtoken to use in other prvd Baseline APIs
  ENDMETHOD.


  METHOD get_prvdtenant.
    DATA: ls_prvdtenant TYPE zprvdtenants,
          lo_api_helper TYPE REF TO zcl_prvd_api_helper.

    lo_api_helper = NEW zcl_prvd_api_helper( ).
    IF iv_prvdtenant IS NOT INITIAL.
      SELECT SINGLE * FROM zprvdtenants INTO ls_prvdtenant WHERE organization_id = iv_prvdtenant
                                                           AND subject_account_id = iv_subjacctid.
      IF sy-subrc <> 0.
        "message no PRVD tenant found
      ENDIF.
    ELSE.
      SELECT SINGLE * FROM zprvdtenants INTO ls_prvdtenant WHERE subject_account_id = iv_subjacctid.
      IF sy-subrc <> 0.
        "message no PRVD tenant found
      ENDIF.
    ENDIF.
    IF sy-subrc = 0.
      ev_prvdtenant-bpi_endpoint = ls_prvdtenant-bpi_endpoint.
      ev_prvdtenant-changed_at = ls_prvdtenant-changed_at.
      ev_prvdtenant-changed_by = ls_prvdtenant-changedby.
      ev_prvdtenant-created_at = ls_prvdtenant-created_at.
      ev_prvdtenant-created_by = ls_prvdtenant-createdby.
      ev_prvdtenant-ident_endpoint = ls_prvdtenant-ident_endpoint.
      ev_prvdtenant-mandt = ls_prvdtenant-mandt.
      ev_prvdtenant-organization_id = ls_prvdtenant-organization_id.
      ev_prvdtenant-subject_account_id = ls_prvdtenant-subject_account_id.
      lo_api_helper->baseline_health_check( EXPORTING iv_tenant    = ls_prvdtenant-subject_account_id
                                          IMPORTING ev_isreachable = ev_prvdtenant-reachable ).
      "ev_prvdtenant-reachable = abap_false. "TODO call the bpi health check
    ELSEIF sy-subrc EQ 4.
    "can't find it. thats ok
    ELSEIF sy-subrc EQ 8.
    "problem with the db
    ELSE.
    "general error wtf
    ENDIF.
  ENDMETHOD.


  METHOD get_refreshtoken.
    DATA: lo_http_client TYPE REF TO if_http_client,
          lo_ident_api   TYPE REF TO zcl_prvd_ident,
          lv_identapiurl TYPE string.

    "todo what authentication is needed here?
    cl_http_client=>create_by_url(
    EXPORTING
       url   = lv_identapiurl
    IMPORTING
      client = lo_http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
    ).

    IF sy-subrc NE 0.
      "todo add exception handling here
*    RAISE EXCEPTION TYPE /ui5/cx_ui5_upload_to_mime
*      EXPORTING
*        textid = /ui5/cx_ui5_upload_to_mime=>no_zip_file_found.
    ENDIF.

    TRY.
        "lo_ident_api = NEW zcl_proubc_ident( ii_client = lo_http_client  ).

      CATCH cx_root.
        "todo implement better exception handling
    ENDTRY.

  ENDMETHOD.


  METHOD update_prvdtenant.
    DATA: ls_prvdtenant    TYPE zprvdtenants,
          lt_prvdtenant    TYPE TABLE OF zprvdtenants,
          lv_timestamp     TYPE timestampl,
          lt_targettenants TYPE TABLE OF zprvdtenants.

    "TODO add SAP auth

    CHECK it_prvdtenant IS NOT INITIAL.

    DESCRIBE TABLE it_prvdtenant LINES DATA(lv_targetcount).

    IF it_prvdtenant IS INITIAL OR lv_targetcount = 0.
      "raise error for empty payload
    ENDIF.

    GET TIME STAMP FIELD lv_timestamp.

    SELECT * FROM zprvdtenants INTO TABLE lt_targettenants
        FOR ALL ENTRIES IN it_prvdtenant WHERE organization_id = it_prvdtenant-organization_id.

    LOOP AT it_prvdtenant ASSIGNING FIELD-SYMBOL(<fs_prvdtenant>).
      CLEAR: ls_prvdtenant.
      ls_prvdtenant-mandt = sy-mandt.
      ls_prvdtenant-organization_id = <fs_prvdtenant>-organization_id.
      ls_prvdtenant-subject_account_id = <fs_prvdtenant>-subject_account_id.
      ls_prvdtenant-workgroup_id = <fs_prvdtenant>-workgroup_id.
      ls_prvdtenant-bpi_endpoint = <fs_prvdtenant>-bpi_endpoint.
      ls_prvdtenant-ident_endpoint = <fs_prvdtenant>-ident_endpoint.
      DATA(lv_tokenlength) = strlen( <fs_prvdtenant>-refresh_token ).
      IF lv_tokenlength LE 1024 .
        ls_prvdtenant-refresh_token = <fs_prvdtenant>-refresh_token(lv_tokenlength).
      ELSE.
        ls_prvdtenant-refresh_token = <fs_prvdtenant>-refresh_token(1024).
        ls_prvdtenant-refresh_tokenext = <fs_prvdtenant>-refresh_token+1024(1024).
      ENDIF.
      READ TABLE lt_targettenants ASSIGNING FIELD-SYMBOL(<fs_olddata>) WITH KEY organization_id = <fs_prvdtenant>-organization_id.
      IF sy-subrc = 0.
        ls_prvdtenant-createdby = <fs_olddata>-createdby.
        ls_prvdtenant-created_at = <fs_olddata>-created_at.
      ENDIF.
      ls_prvdtenant-changedby = sy-uname.
      ls_prvdtenant-changed_at = lv_timestamp.
      APPEND ls_prvdtenant TO lt_prvdtenant.
    ENDLOOP.

    MODIFY zprvdtenants FROM TABLE lt_prvdtenant.
    IF sy-subrc <> 0.
      "TODO add raise exception here
    ELSE.
      MOVE-CORRESPONDING lt_prvdtenant TO et_prvdtenant.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
