CLASS z100085_zcl_proubc_busobjhlpr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS: create_object IMPORTING it_objects TYPE z100085_ztty_bpiobj
                                 EXPORTING et_objects TYPE z100085_ztty_bpiobj,
      get_object IMPORTING iv_objectid TYPE z100085_bpiobj-object_id OPTIONAL
                 EXPORTING et_objects  TYPE z100085_ztty_bpiobj,
      update_object IMPORTING it_objects TYPE z100085_ztty_bpiobj
                    EXPORTING et_objects TYPE z100085_ztty_bpiobj,
      get_object_status IMPORTING iv_objectid   TYPE z100085_bpiobj-object_id
                        EXPORTING es_objectstat TYPE z100085_zif_proubc_object=>ty_update_status_res,
      update_object_status IMPORTING iv_objectid   TYPE z100085_bpiobj-object_id
                                     is_objectstat TYPE z100085_zif_proubc_object=>ty_update_status_req
                           EXPORTING es_objectstat TYPE z100085_zif_proubc_object=>ty_update_status_res,
      validate_object_create IMPORTING it_objects TYPE z100085_ztty_bpiobj
                             EXPORTING et_objects TYPE z100085_ztty_bpiobj,
      validate_object_update IMPORTING it_objects TYPE z100085_ztty_bpiobj
                             EXPORTING et_objects TYPE z100085_ztty_bpiobj .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_busobjhlpr IMPLEMENTATION.
  METHOD create_object.
    DATA: ls_bpiobj    TYPE z100085_bpiobj,
          lt_bpiobj    TYPE TABLE OF z100085_bpiobj,
          l_timestampl TYPE timestampl.

    "TODO add SAP authorization objects

    GET TIME STAMP FIELD l_timestampl.
    FIELD-SYMBOLS: <fs_object> TYPE z100085_bpiobj.
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
      APPEND ls_bpiobj TO lt_bpiobj.
    ENDLOOP.
    MODIFY z100085_bpiobj FROM TABLE lt_bpiobj.
    IF sy-subrc = 0.
      et_objects = lt_bpiobj.
    ENDIF.

  ENDMETHOD.
  METHOD get_object.
    DATA: lt_objects TYPE TABLE OF z100085_bpiobj,
          ls_object  TYPE z100085_bpiobj.

    "TODO add SAP authorization objects
    IF iv_objectid IS NOT INITIAL.
      SELECT SINGLE * FROM z100085_bpiobj INTO ls_object WHERE object_id = iv_objectid.
      APPEND ls_object TO lt_objects.
    ELSE.
      "TODO add more and better default selection criteria
      SELECT * FROM z100085_bpiobj INTO TABLE lt_objects UP TO 1000 ROWS.
    ENDIF.
    et_objects = lt_objects.
  ENDMETHOD.
  METHOD update_object.
    "TODO add SAP authorization objects
    "check object exists

    DATA: ls_bpiobj       TYPE z100085_bpiobj,
          lt_bpiobj       TYPE TABLE OF z100085_bpiobj,
          lt_targetbpiobj TYPE TABLE OF z100085_bpiobj,
          l_timestampl    TYPE timestampl.

    "TODO add SAP authorization objects

    DESCRIBE TABLE it_objects LINES DATA(lv_targetcount).

    IF it_objects IS INITIAL OR lv_targetcount = 0.
      "raise error for empty payload
    ENDIF.

    SELECT * FROM z100085_bpiobj INTO TABLE lt_targetbpiobj
        FOR ALL ENTRIES IN it_objects WHERE object_id = it_objects-object_id.

    GET TIME STAMP FIELD l_timestampl.
    FIELD-SYMBOLS: <fs_object> TYPE z100085_bpiobj.
    "ensure object exists

    CLEAR: ls_bpiobj.
    LOOP AT it_objects ASSIGNING FIELD-SYMBOL(<fs_object_upd>).
      READ TABLE lt_targetbpiobj ASSIGNING FIELD-SYMBOL(<fs_object_old>) WITH KEY object_id = <fs_object_upd>-object_id.
      IF sy-subrc = 0.
      "TODO refactor this into something nice
        if <fs_object_upd>-baseline_id is not INITIAL.
        ls_bpiobj-baseline_id = <fs_object_upd>-baseline_id.
        else.
        ls_bpiobj-baseline_id = <fs_object_old>-baseline_id.
        endif.
        if <fs_object_upd>-proof is not initial.
        ls_bpiobj-proof = <fs_object_upd>-proof.
        else.
        ls_bpiobj-proof = <fs_object_old>-proof.
        endif.
        if <fs_object_upd>-schema_id is not INITIAL.
        ls_bpiobj-schema_id = <fs_object_upd>-schema_id.
        else.
        ls_bpiobj-schema_id = <fs_object_old>-schema_id.
        endif.
        if <fs_object_upd>-schematype is not INITIAL.
        ls_bpiobj-schematype = <fs_object_upd>-schematype.
        else.
        ls_bpiobj-schematype = <fs_object_old>-schematype.
        endif.
        if <fs_object_upd>-status is not INITIAL.
        ls_bpiobj-status = <fs_object_upd>-status.
        else.
        ls_bpiobj-status = <fs_object_old>-status.
        endif.

        ls_bpiobj-changed_by = sy-uname.
        ls_bpiobj-changed_at = l_timestampl.
        ls_bpiobj-mandt = sy-mandt.

        "this should never change!
        ls_bpiobj-object_id = <fs_object_upd>-object_id.
        APPEND ls_bpiobj TO lt_bpiobj.
      ENDIF.


    ENDLOOP.

    MODIFY z100085_bpiobj FROM TABLE lt_bpiobj.
    IF sy-subrc = 0.
      et_objects = lt_bpiobj.
    ENDIF.


  ENDMETHOD.
  METHOD get_object_status.
    "TODO add SAP authorization objects
    DATA: ls_bpiobj TYPE z100085_bpiobj.
    SELECT SINGLE * FROM z100085_bpiobj INTO ls_bpiobj WHERE object_id = iv_objectid.
    IF sy-subrc = 0.
      es_objectstat-baseline_id = ls_bpiobj-baseline_id.
      es_objectstat-object_id = ls_bpiobj-object_id.
      es_objectstat-status = ls_bpiobj-status.
    ENDIF.
  ENDMETHOD.
  METHOD update_object_status.
    "TODO add SAP authorization objects
    "TODO archive the prior status
    DATA: ls_bpiobj    TYPE z100085_bpiobj,
          lt_bpiobj    TYPE TABLE OF z100085_bpiobj,
          l_timestampl TYPE timestampl.

    GET TIME STAMP FIELD l_timestampl.
    "get the existing record and add updates
    SELECT SINGLE * FROM z100085_bpiobj INTO ls_bpiobj WHERE object_id = iv_objectid.
    IF sy-subrc = 0.
      ls_bpiobj-baseline_id = is_objectstat-baseline_id.
      ls_bpiobj-status = is_objectstat-status.
      ls_bpiobj-changed_by = sy-uname.
      ls_bpiobj-changed_at = l_timestampl.
      APPEND ls_bpiobj TO lt_bpiobj.
    ENDIF.
    IF lt_bpiobj IS NOT INITIAL.
      MODIFY z100085_bpiobj FROM TABLE lt_bpiobj.
      IF sy-subrc = 0.
        es_objectstat-baseline_id = is_objectstat-baseline_id.
        es_objectstat-object_id = iv_objectid.
        es_objectstat-status    = is_objectstat-status.
      ENDIF.
    ELSE. "nothin to update
    ENDIF.
  ENDMETHOD.
  METHOD validate_object_create.
    "object ID is required
  ENDMETHOD.
  METHOD validate_object_update.
    "object ID is required
    "baseline ID required for certain statuses
  ENDMETHOD.
ENDCLASS.
