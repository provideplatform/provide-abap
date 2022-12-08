CLASS zcl_proubc_busobjhlpr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! Creates an entry to the BPI objects table
    CLASS-METHODS create_object
      IMPORTING
        !it_objects TYPE ztty_bpiobj
      EXPORTING
        !et_objects TYPE ztty_bpiobj .
    CLASS-METHODS get_object
      IMPORTING
        !iv_objectid TYPE zbpiobj-object_id OPTIONAL
      EXPORTING
        !et_objects  TYPE ztty_bpiobj .
    CLASS-METHODS update_object
      IMPORTING
        !it_objects TYPE ztty_bpiobj
      EXPORTING
        !et_objects TYPE ztty_bpiobj .
    CLASS-METHODS get_object_status
      IMPORTING
        !iv_objectid   TYPE zbpiobj-object_id
      EXPORTING
        !es_objectstat TYPE zif_proubc_object=>ty_update_status_res .
    CLASS-METHODS update_object_status
      IMPORTING
        !iv_objectid   TYPE zbpiobj-object_id
        !is_objectstat TYPE zif_proubc_object=>ty_update_status_req
      EXPORTING
        !es_objectstat TYPE zif_proubc_object=>ty_update_status_res .
    CLASS-METHODS validate_object_create
      IMPORTING
        !it_objects TYPE ztty_bpiobj
      EXPORTING
        !et_objects TYPE ztty_bpiobj .
    CLASS-METHODS validate_object_update
      IMPORTING
        !it_objects TYPE ztty_bpiobj
      EXPORTING
        !et_objects TYPE ztty_bpiobj .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_proubc_busobjhlpr IMPLEMENTATION.


  METHOD create_object.
    DATA: ls_bpiobj    TYPE zbpiobj,
          lt_bpiobj    TYPE TABLE OF zbpiobj,
          l_timestampl TYPE timestampl.
    FIELD-SYMBOLS: <fs_object> TYPE zbpiobj.
    "TODO add SAP authorization objects

    GET TIME STAMP FIELD l_timestampl.
    "ensure object does not already exist
    LOOP AT it_objects ASSIGNING <fs_object>.
      CLEAR: ls_bpiobj.
      ls_bpiobj-baseline_id = <fs_object>-baseline_id.
      ls_bpiobj-created_by = sy-uname.
      ls_bpiobj-created_at = l_timestampl.
      ls_bpiobj-mandt = sy-mandt.
      ls_bpiobj-proof = <fs_object>-proof.
      ls_bpiobj-schema_id = <fs_object>-schema_id.
      ls_bpiobj-schematype = <fs_object>-schematype.
      ls_bpiobj-status = <fs_object>-status.
      ls_bpiobj-object_id = <fs_object>-object_id.
      ls_bpiobj-subject_account_id = <fs_object>-subject_account_id.
      ls_bpiobj-workgroup_id = <fs_object>-workgroup_id.
      APPEND ls_bpiobj TO lt_bpiobj.
    ENDLOOP.
    MODIFY zbpiobj FROM TABLE lt_bpiobj.
    IF sy-subrc = 0.
      et_objects = lt_bpiobj.
    ENDIF.

  ENDMETHOD.


  METHOD get_object.
    DATA: lt_objects TYPE TABLE OF zbpiobj,
          ls_object  TYPE zbpiobj.

    "TODO add SAP authorization objects
    IF iv_objectid IS NOT INITIAL.
      SELECT SINGLE * FROM zbpiobj INTO ls_object WHERE object_id = iv_objectid.
      APPEND ls_object TO lt_objects.
    ELSE.
      "TODO add more and better default selection criteria
      SELECT * FROM zbpiobj INTO TABLE lt_objects UP TO 1000 ROWS.
    ENDIF.
    et_objects = lt_objects.
  ENDMETHOD.


  METHOD get_object_status.
    "TODO add SAP authorization objects
    DATA: ls_bpiobj TYPE zbpiobj.
    SELECT SINGLE * FROM zbpiobj INTO ls_bpiobj WHERE object_id = iv_objectid.
    IF sy-subrc = 0.
      es_objectstat-baseline_id = ls_bpiobj-baseline_id.
      es_objectstat-object_id = ls_bpiobj-object_id.
      es_objectstat-status = ls_bpiobj-status.
    ENDIF.
  ENDMETHOD.


  METHOD update_object.
    "TODO add SAP authorization objects
    "check object exists

    DATA: ls_bpiobj       TYPE zbpiobj,
          lt_bpiobj       TYPE TABLE OF zbpiobj,
          lt_targetbpiobj TYPE TABLE OF zbpiobj,
          l_timestampl    TYPE timestampl.

    "TODO add SAP authorization objects

    DESCRIBE TABLE it_objects LINES DATA(lv_targetcount).

    IF it_objects IS INITIAL OR lv_targetcount = 0.
      "raise error for empty payload
    ENDIF.

    SELECT * FROM zbpiobj INTO TABLE lt_targetbpiobj
        FOR ALL ENTRIES IN it_objects WHERE object_id = it_objects-object_id.
    IF SY-SUBRC <> 0.
      "Raise message no bpi object for update
    ENDIF.

    GET TIME STAMP FIELD l_timestampl.
    FIELD-SYMBOLS: <fs_object> TYPE zbpiobj.
    "ensure object exists

    CLEAR: ls_bpiobj.
    LOOP AT it_objects ASSIGNING FIELD-SYMBOL(<fs_object_upd>).
      READ TABLE lt_targetbpiobj ASSIGNING FIELD-SYMBOL(<fs_object_old>) WITH KEY object_id = <fs_object_upd>-object_id
                                                                                  baseline_id = <fs_object_upd>-baseline_id.
      IF sy-subrc = 0.
        "TODO refactor this into something nice
        IF <fs_object_upd>-baseline_id IS NOT INITIAL.
          ls_bpiobj-baseline_id = <fs_object_upd>-baseline_id.
        ELSE.
          ls_bpiobj-baseline_id = <fs_object_old>-baseline_id.
        ENDIF.
        IF <fs_object_upd>-proof IS NOT INITIAL.
          ls_bpiobj-proof = <fs_object_upd>-proof.
        ELSE.
          ls_bpiobj-proof = <fs_object_old>-proof.
        ENDIF.
        IF <fs_object_upd>-schema_id IS NOT INITIAL.
          ls_bpiobj-schema_id = <fs_object_upd>-schema_id.
        ELSE.
          ls_bpiobj-schema_id = <fs_object_old>-schema_id.
        ENDIF.
        IF <fs_object_upd>-schematype IS NOT INITIAL.
          ls_bpiobj-schematype = <fs_object_upd>-schematype.
        ELSE.
          ls_bpiobj-schematype = <fs_object_old>-schematype.
        ENDIF.
        IF <fs_object_upd>-status IS NOT INITIAL.
          ls_bpiobj-status = <fs_object_upd>-status.
        ELSE.
          ls_bpiobj-status = <fs_object_old>-status.
        ENDIF.
        IF <fs_object_upd>-subject_account_id IS NOT INITIAL.
          ls_bpiobj-subject_account_id = <fs_object_upd>-subject_account_id.
        ELSE.
          ls_bpiobj-subject_account_id = <fs_object_old>-subject_account_id.
        ENDIF.
        IF <fs_object_upd>-workgroup_id IS NOT INITIAL.
          ls_bpiobj-workgroup_id = <fs_object_upd>-workgroup_id.
        ELSE.
          ls_bpiobj-workgroup_id = <fs_object_old>-workgroup_id.
        ENDIF.

        ls_bpiobj-changed_by = sy-uname.
        ls_bpiobj-changed_at = l_timestampl.
        ls_bpiobj-mandt = sy-mandt.

        "this should never change!
        ls_bpiobj-object_id = <fs_object_upd>-object_id.
        APPEND ls_bpiobj TO lt_bpiobj.
      ENDIF.


    ENDLOOP.

    MODIFY zbpiobj FROM TABLE lt_bpiobj.
    IF sy-subrc = 0.
      et_objects = lt_bpiobj.
    else.
      "raise message error updating table
    ENDIF.


  ENDMETHOD.


  METHOD update_object_status.
    "TODO add SAP authorization objects
    "TODO archive the prior status
    DATA: ls_bpiobj    TYPE zbpiobj,
          lt_bpiobj    TYPE TABLE OF zbpiobj,
          l_timestampl TYPE timestampl.

    GET TIME STAMP FIELD l_timestampl.
    "get the existing record and add updates
    SELECT SINGLE * FROM zbpiobj INTO ls_bpiobj WHERE object_id = iv_objectid.
    IF sy-subrc = 0.
      ls_bpiobj-baseline_id = is_objectstat-baseline_id.
      ls_bpiobj-status = is_objectstat-status.
      ls_bpiobj-changed_by = sy-uname.
      ls_bpiobj-changed_at = l_timestampl.
      APPEND ls_bpiobj TO lt_bpiobj.
    ELSE.
      "Raise error no BPI object found
    ENDIF.
    IF lt_bpiobj IS NOT INITIAL.
      MODIFY zbpiobj FROM TABLE lt_bpiobj.
      IF sy-subrc = 0.
        es_objectstat-baseline_id = is_objectstat-baseline_id.
        es_objectstat-object_id = iv_objectid.
        es_objectstat-status    = is_objectstat-status.
      ENDIF.
    ELSE. "nothin to update
    ENDIF.
  ENDMETHOD.


  METHOD validate_object_create.
    DATA: lt_objects TYPE TABLE OF zbpiobj.
    lt_objects = it_objects.
    LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<fs_object>).
      "TODO add error logging
      "object id is required
      "baseline id is required
    ENDLOOP.
    et_objects = lt_objects.
  ENDMETHOD.


  METHOD validate_object_update.
    DATA: lt_objects TYPE TABLE OF zbpiobj.
    lt_objects = it_objects.
    LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<fs_object>).
      "TODO add error logging
      "object id is required
      "baseline id is required
    ENDLOOP.
    et_objects = lt_objects.
  ENDMETHOD.
ENDCLASS.
