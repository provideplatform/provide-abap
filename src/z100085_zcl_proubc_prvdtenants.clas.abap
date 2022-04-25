class Z100085_ZCL_PROUBC_PRVDTENANTS definition
  public
  final
  create public .

public section.

  class-methods GET_PRVDTENANT
    exporting
      !EV_PRVDORGID type Z100085_ZS_PRVDORG-ORGANIZATION_ID .
  class-methods GET_ALLPRVDTENANT
    exporting
      !ET_PRVDORG type Z100085_ZTT_PRVDORG .
  class-methods CREATE_PRVDTENANT
    importing
      !IS_PRVDORG type Z100085_ZS_PRVDORG .
  class-methods UPDATE_PRVDTENANT
    importing
      !IT_PRVDORG type Z100085_ZTT_PRVDORG
    exporting
      !ET_PRVDORG type Z100085_ZTT_PRVDORG .
  class-methods DELETE_PRVDTENANT
    exporting
      !EV_PRVDORGID type Z100085_ZS_PRVDORG-ORGANIZATION_ID .
  methods GET_REFRESHTOKEN
    exporting
      !EV_PRVDORGID type Z100085_ZS_PRVDORG-ORGANIZATION_ID .
  methods GET_AUTHTOKEN
    exporting
      !EV_PRVDORGID type Z100085_ZS_PRVDORG-ORGANIZATION_ID .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z100085_ZCL_PROUBC_PRVDTENANTS IMPLEMENTATION.


  METHOD CREATE_PRVDTENANT.
    DATA: ls_prvdorg   TYPE z100085_prvdorgs,
          lv_timestamp TYPE timestampl.
    "TODO add SAP atuh
    "duplicate check

    GET TIME STAMP FIELD lv_timestamp.

    ls_prvdorg-mandt = sy-mandt.
    ls_prvdorg-organization_id = is_prvdorg-organization_id.
    ls_prvdorg-bpi_endpoint = is_prvdorg-bpi_endpoint.
    ls_prvdorg-ident_endpoint = is_prvdorg-ident_endpoint.
    ls_prvdorg-refresh_token = is_prvdorg-refresh_token.
    ls_prvdorg-refresh_tokenext = is_prvdorg-refresh_tokenext.
    ls_prvdorg-createdby = sy-uname.
    ls_prvdorg-created_at = lv_timestamp.

    INSERT z100085_prvdorgs FROM ls_prvdorg.
    IF sy-subrc <> 0.
      "TODO add raise exception here
    ENDIF.


  ENDMETHOD.


  METHOD DELETE_PRVDTENANT.
    "TODO add SAP auth

    DELETE FROM z100085_prvdorgs WHERE organization_id = ev_prvdorgid.
    IF sy-subrc = 0.
      "todo Add some logging for this
    ELSE. "delete failed. why?
      "TODO raise exception here
    ENDIF.


  ENDMETHOD.


  METHOD GET_ALLPRVDTENANT.
    DATA: lt_prvdorg TYPE TABLE OF z100085_prvdorgs,
          ls_prvdorg TYPE z100085_zs_prvdorg.
    SELECT * FROM z100085_prvdorgs INTO TABLE lt_prvdorg.
    IF sy-subrc = 0.
    ELSEIF sy-subrc EQ 4. "can't find it. thats ok
    ELSEIF sy-subrc EQ 8. "problem with the db
    ELSE. "general error

    ENDIF.

    "TODO only display entries user is authorized to view

    LOOP AT lt_prvdorg ASSIGNING FIELD-SYMBOL(<fs_prvdorg>).
      CLEAR ls_prvdorg.
      ls_prvdorg-organization_id = <fs_prvdorg>-organization_id.
      ls_prvdorg-ident_endpoint = <fs_prvdorg>-ident_endpoint.
      ls_prvdorg-bpi_endpoint = <fs_prvdorg>-bpi_endpoint.
      ls_prvdorg-createdby = <fs_prvdorg>-createdby.
      ls_prvdorg-created_at = <fs_prvdorg>-created_at.
      ls_prvdorg-changedby = <fs_prvdorg>-changedby.
      ls_prvdorg-changed_at = <fs_prvdorg>-changed_at.
      ls_prvdorg-refresh_token = <fs_prvdorg>-refresh_token.
      ls_prvdorg-refresh_tokenext = <fs_prvdorg>-refresh_tokenext.
      APPEND ls_prvdorg TO et_prvdorg.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_PRVDTENANT.
    DATA: ls_prvdorg TYPE z100085_prvdorgs.
    SELECT SINGLE * FROM z100085_prvdorgs INTO ls_prvdorg WHERE organization_id = ev_prvdorgid.
    IF sy-subrc = 0.
    ELSEIF sy-subrc EQ 4. "can't find it. thats ok
    ELSEIF sy-subrc EQ 8. "problem with the db
    ELSE. "general error wtf
    ENDIF.
  ENDMETHOD.


  METHOD GET_REFRESHTOKEN.
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
        lo_ident_api = NEW z100085_zcl_proubc_ident( ii_client = lo_http_client  ).

      CATCH cx_root.
        "todo implement better exception handling
    ENDTRY.

  ENDMETHOD.


  METHOD UPDATE_PRVDTENANT.
    DATA: ls_prvdorg   TYPE z100085_prvdorgs,
          lt_prvdorg   TYPE TABLE OF z100085_prvdorgs,
          lv_timestamp TYPE timestampl.
    "TODO add SAP auth

    GET TIME STAMP FIELD lv_timestamp.

    LOOP AT it_prvdorg ASSIGNING FIELD-SYMBOL(<fs_prvdorg>).
      ls_prvdorg-mandt = sy-mandt.
      ls_prvdorg-organization_id = <fs_prvdorg>-organization_id.
      ls_prvdorg-bpi_endpoint = <fs_prvdorg>-bpi_endpoint.
      ls_prvdorg-ident_endpoint = <fs_prvdorg>-ident_endpoint.
      ls_prvdorg-refresh_token = <fs_prvdorg>-refresh_token.
      ls_prvdorg-refresh_tokenext = <fs_prvdorg>-refresh_tokenext.
      ls_prvdorg-changedby = sy-uname.
      ls_prvdorg-changed_at = lv_timestamp.
    ENDLOOP.

    UPDATE Z100085_prvdorgs FROM TABLE lt_prvdorg.
    IF sy-subrc <> 0.
      "TODO add raise exception here
    ENDIF.


  ENDMETHOD.

  method get_authtoken.
  "use the refresh token to get a new authtoken to use in other prvd Baseline APIs
  endmethod.
ENDCLASS.
