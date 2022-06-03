CLASS z100085_zcl_proubc_prvdtenants DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_prvdtenant
      IMPORTING
        !iv_prvdtenant TYPE z100085_prvdtenantid
      EXPORTING
        !ev_prvdtenant TYPE z100085_zif_proubc_tenants=>ty_tenant_wo_token.
    CLASS-METHODS get_allprvdtenant
      EXPORTING
        !et_prvdorg TYPE  z100085_zif_proubc_tenants=>tty_tenant_wo_token..
    CLASS-METHODS create_prvdtenant
      IMPORTING
        !it_prvdorg TYPE z100085_ztt_prvdorg
      EXPORTING
        !et_prvdorg TYPE z100085_ztt_prvdorg.
    CLASS-METHODS update_prvdtenant
      IMPORTING
        !it_prvdorg TYPE z100085_ztt_prvdorg
      EXPORTING
        !et_prvdorg TYPE z100085_ztt_prvdorg .
    CLASS-METHODS delete_prvdtenant
      EXPORTING
        !ev_prvdorgid TYPE z100085_zs_prvdorg-organization_id .
    METHODS get_refreshtoken
      EXPORTING
        !ev_prvdorgid TYPE z100085_zs_prvdorg-organization_id .
    METHODS get_authtoken
      EXPORTING
        !ev_prvdorgid TYPE z100085_zs_prvdorg-organization_id .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_prvdtenants IMPLEMENTATION.


  METHOD create_prvdtenant.
    DATA: ls_prvdorg         TYPE z100085_prvdorgs,
          lt_prvdorg         TYPE TABLE OF z100085_prvdorgs,
          lt_existingprvdorg TYPE TABLE OF z100085_prvdorgs,
          lv_timestamp       TYPE timestampl.
    "TODO add SAP auth check

    CHECK it_prvdorg IS NOT INITIAL.

    "duplicate check

    "throw out error message if the tenant already exists
    SELECT * FROM z100085_prvdorgs INTO TABLE lt_existingprvdorg
        FOR ALL ENTRIES IN it_prvdorg WHERE organization_id = it_prvdorg-organization_id.
    IF sy-dbcnt > 0.
      MESSAGE e004(z100085_zclproubcmsg) WITH 'orgid'.
    ENDIF.

    GET TIME STAMP FIELD lv_timestamp.

    LOOP AT it_prvdorg ASSIGNING FIELD-SYMBOL(<fs_prvdorg>).
      CLEAR: ls_prvdorg.
      ls_prvdorg-mandt = sy-mandt.
      ls_prvdorg-organization_id = <fs_prvdorg>-organization_id.
      ls_prvdorg-bpi_endpoint = <fs_prvdorg>-bpi_endpoint.
      ls_prvdorg-ident_endpoint = <fs_prvdorg>-ident_endpoint.
      DATA(lv_tokenlength) = strlen( <fs_prvdorg>-refresh_token ).
      IF lv_tokenlength LE 1024 .
        ls_prvdorg-refresh_token = <fs_prvdorg>-refresh_token(lv_tokenlength).
      ELSE.
        DATA(lv_remaininglength) = 2048 - lv_tokenlength.
        ls_prvdorg-refresh_token = <fs_prvdorg>-refresh_token(1024).
        ls_prvdorg-refresh_tokenext = <fs_prvdorg>-refresh_token+1024(lv_remaininglength).
      ENDIF.



      ls_prvdorg-createdby = sy-uname.
      ls_prvdorg-created_at = lv_timestamp.
      APPEND ls_prvdorg TO lt_prvdorg.
    ENDLOOP.

    MODIFY z100085_prvdorgs FROM TABLE lt_prvdorg.
    IF sy-subrc <> 0.
      "TODO add raise exception here
    ELSE.
      MOVE-CORRESPONDING lt_prvdorg TO et_prvdorg.
    ENDIF.


  ENDMETHOD.


  METHOD delete_prvdtenant.
    "TODO add SAP auth

    IF ev_prvdorgid IS NOT INITIAL.
      DELETE FROM z100085_prvdorgs WHERE organization_id = ev_prvdorgid.
      IF sy-subrc = 0.
        "todo Add some logging for this
      ELSE. "delete failed. why?
        "TODO raise exception here
      ENDIF.
    ELSE.
      "TODO raise exception msg here
    ENDIF.


  ENDMETHOD.


  METHOD get_allprvdtenant.
    DATA: lt_prvdorg    TYPE TABLE OF z100085_prvdorgs,
          ls_prvdorg    TYPE z100085_zif_proubc_tenants=>ty_tenant_wo_token,
          lo_api_helper TYPE REF TO z100085_zcl_proubc_api_helper.

    lo_api_helper = NEW z100085_zcl_proubc_api_helper( ).
    SELECT * FROM z100085_prvdorgs INTO TABLE lt_prvdorg.
    IF sy-subrc = 0.
    ELSEIF sy-subrc EQ 4. "can't find it. thats ok
    ELSEIF sy-subrc EQ 8. "problem with the db
    ELSE. "general error

    ENDIF.

    "TODO only display entries user is authorized to view

    LOOP AT lt_prvdorg ASSIGNING FIELD-SYMBOL(<fs_prvdorg>).
      CLEAR ls_prvdorg.
      ls_prvdorg-mandt = <fs_prvdorg>-mandt.
      ls_prvdorg-organization_id = <fs_prvdorg>-organization_id.
      ls_prvdorg-ident_endpoint = <fs_prvdorg>-ident_endpoint.
      ls_prvdorg-bpi_endpoint = <fs_prvdorg>-bpi_endpoint.
      ls_prvdorg-created_by = <fs_prvdorg>-createdby.
      ls_prvdorg-created_at = <fs_prvdorg>-created_at.
      ls_prvdorg-changed_by = <fs_prvdorg>-changedby.
      ls_prvdorg-changed_at = <fs_prvdorg>-changed_at.
      "ls_prvdorg-reachable = abap_false. "TODO add call to BPI endpoint
      APPEND ls_prvdorg TO et_prvdorg.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_prvdtenant.
    DATA: ls_prvdorg    TYPE z100085_prvdorgs,
          lo_api_helper TYPE REF TO z100085_zcl_proubc_api_helper.

    lo_api_helper = NEW z100085_zcl_proubc_api_helper( ).
    SELECT SINGLE * FROM z100085_prvdorgs INTO ls_prvdorg WHERE organization_id = iv_prvdtenant.
    IF sy-subrc = 0.
      ev_prvdtenant-bpi_endpoint = ls_prvdorg-bpi_endpoint.
      ev_prvdtenant-changed_at = ls_prvdorg-changed_at.
      ev_prvdtenant-changed_by = ls_prvdorg-changedby.
      ev_prvdtenant-created_at = ls_prvdorg-created_at.
      ev_prvdtenant-created_by = ls_prvdorg-createdby.
      ev_prvdtenant-ident_endpoint = ls_prvdorg-ident_endpoint.
      ev_prvdtenant-mandt = ls_prvdorg-mandt.
      ev_prvdtenant-organization_id = ls_prvdorg-organization_id.
      lo_api_helper->baseline_health_check(
        EXPORTING
          iv_tenant      =  ls_prvdorg-organization_id
        IMPORTING
          ev_isreachable = ev_prvdtenant-reachable
      ).
      "ev_prvdtenant-reachable = abap_false. "TODO call the bpi health check
    ELSEIF sy-subrc EQ 4. "can't find it. thats ok
    ELSEIF sy-subrc EQ 8. "problem with the db
    ELSE. "general error wtf
    ENDIF.
  ENDMETHOD.


  METHOD get_refreshtoken.
    DATA: lo_http_client TYPE REF TO if_http_client,
          lo_ident_api   TYPE REF TO z100085_zcl_proubc_ident,
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
        "lo_ident_api = NEW z100085_zcl_proubc_ident( ii_client = lo_http_client  ).

      CATCH cx_root.
        "todo implement better exception handling
    ENDTRY.

  ENDMETHOD.


  METHOD update_prvdtenant.
    DATA: ls_prvdorg       TYPE z100085_prvdorgs,
          lt_prvdorg       TYPE TABLE OF z100085_prvdorgs,
          lv_timestamp     TYPE timestampl,
          lt_targettenants TYPE TABLE OF z100085_prvdorgs.

    "TODO add SAP auth

    CHECK it_prvdorg IS NOT INITIAL.

    DESCRIBE TABLE it_prvdorg LINES DATA(lv_targetcount).

    IF it_prvdorg IS INITIAL OR lv_targetcount = 0.
      "raise error for empty payload
    ENDIF.

    GET TIME STAMP FIELD lv_timestamp.

    SELECT * FROM z100085_prvdorgs INTO TABLE lt_targettenants
        FOR ALL ENTRIES IN it_prvdorg WHERE organization_id = it_prvdorg-organization_id.

    LOOP AT it_prvdorg ASSIGNING FIELD-SYMBOL(<fs_prvdorg>).
      CLEAR: ls_prvdorg.
      ls_prvdorg-mandt = sy-mandt.
      ls_prvdorg-organization_id = <fs_prvdorg>-organization_id.
      ls_prvdorg-bpi_endpoint = <fs_prvdorg>-bpi_endpoint.
      ls_prvdorg-ident_endpoint = <fs_prvdorg>-ident_endpoint.
      "ls_prvdorg-refresh_token = <fs_prvdorg>-refresh_token.
      "ls_prvdorg-refresh_tokenext = <fs_prvdorg>-refresh_tokenext.
      DATA(lv_tokenlength) = strlen( <fs_prvdorg>-refresh_token ).
      IF lv_tokenlength LE 1024 .
        ls_prvdorg-refresh_token = <fs_prvdorg>-refresh_token(lv_tokenlength).
      ELSE.
        DATA(lv_remaininglength) = 2048 - lv_tokenlength.
        ls_prvdorg-refresh_token = <fs_prvdorg>-refresh_token(1024).
        ls_prvdorg-refresh_tokenext = <fs_prvdorg>-refresh_token+1024(lv_remaininglength).
      ENDIF.
      READ TABLE lt_targettenants ASSIGNING FIELD-SYMBOL(<fs_olddata>) WITH KEY organization_id = <fs_prvdorg>-organization_id.
      IF sy-subrc = 0.
        ls_prvdorg-createdby = <fs_olddata>-createdby.
        ls_prvdorg-created_at = <fs_olddata>-created_at.
      ENDIF.
      ls_prvdorg-changedby = sy-uname.
      ls_prvdorg-changed_at = lv_timestamp.
      APPEND ls_prvdorg TO lt_prvdorg.
    ENDLOOP.

    MODIFY Z100085_prvdorgs FROM TABLE lt_prvdorg.
    IF sy-subrc <> 0.
      "TODO add raise exception here
    ELSE.
      MOVE-CORRESPONDING lt_prvdorg TO et_prvdorg.
    ENDIF.


  ENDMETHOD.

  METHOD get_authtoken.
    "use the refresh token to get a new authtoken to use in other prvd Baseline APIs
  ENDMETHOD.
ENDCLASS.
