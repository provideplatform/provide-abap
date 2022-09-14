CLASS z100085_zcl_proubc_busobjhlpr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS: create_object IMPORTING it_objects TYPE z100085_ztty_bpiobj
                                 EXPORTING et_objects TYPE z100085_ztty_bpiobj,
      create_mock_object IMPORTING it_objects TYPE z100085_ztty_bpiobj
                                   iv_payload type string
                         EXPORTING et_objects TYPE z100085_ztty_bpiobj,
      get_object IMPORTING iv_objectid TYPE z100085_bpiobj-object_id OPTIONAL
                 EXPORTING et_objects  TYPE z100085_ztty_bpiobj,
      update_object IMPORTING it_objects TYPE z100085_ztty_bpiobj
                    EXPORTING et_objects TYPE z100085_ztty_bpiobj,
      update_mock_object IMPORTING it_objects TYPE z100085_ztty_bpiobj
                                   iv_payload type string
                         EXPORTING et_objects TYPE z100085_ztty_bpiobj,
      get_object_status IMPORTING iv_objectid   TYPE z100085_bpiobj-object_id
                        EXPORTING es_objectstat TYPE z100085_zif_proubc_object=>ty_update_status_res,
      update_object_status IMPORTING iv_objectid   TYPE z100085_bpiobj-object_id
                                     is_objectstat TYPE z100085_zif_proubc_object=>ty_update_status_req
                           EXPORTING es_objectstat TYPE z100085_zif_proubc_object=>ty_update_status_res,
      validate_object_create IMPORTING it_objects TYPE z100085_ztty_bpiobj
                             EXPORTING et_objects TYPE z100085_ztty_bpiobj,
      validate_object_update IMPORTING it_objects TYPE z100085_ztty_bpiobj
                             EXPORTING et_objects TYPE z100085_ztty_bpiobj,
      create_mock_hash IMPORTING iv_object_id   TYPE char20
                                 iv_baseline_id TYPE z100085_baselineid
                                 iv_payload     TYPE string
                       EXPORTING ev_outputhash  TYPE char256
                                 ev_timestamp   TYPE timestampl,
      verify_mock_hash IMPORTING iv_inputhash    TYPE string
                                 iv_object_id    TYPE char20
                       EXPORTING ev_mockbaseline TYPE char1,
      generate_mock_input_hash IMPORTING iv_objectid TYPE char20
                               EXPORTING ev_mockhash TYPE string.
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
  METHOD create_mock_object.
    DATA: ls_bpiobj    TYPE z100085_bpiobj,
          lt_bpiobj    TYPE TABLE OF z100085_bpiobj,
          l_timestampl TYPE timestampl,
          lv_payload type string.

    "TODO add SAP authorization objects

    GET TIME STAMP FIELD l_timestampl.
    FIELD-SYMBOLS: <fs_object> TYPE z100085_bpiobj.

    "ensure object does not already exist
    LOOP AT it_objects ASSIGNING <fs_object>.

    z100085_zcl_proubc_busobjhlpr=>create_mock_hash(
      EXPORTING
        iv_object_id   = <fs_object>-object_id
        iv_baseline_id = <fs_object>-baseline_id
        iv_payload     = lv_payload
      IMPORTING
        ev_outputhash  = ls_bpiobj-proof
        ev_timestamp   = l_timestampl
    ).

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
      ls_bpiobj-mock_object = 'X'.
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
  METHOD update_mock_object.
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
    "create a new baseline id
  ENDMETHOD.
  METHOD validate_object_update.
    "object ID is required
  ENDMETHOD.
  METHOD create_mock_hash.

    "Use built-in cryptographic functions of ABAP to create mock zk proof

    DATA lo_digest TYPE REF TO cl_abap_message_digest.

    DATA lv_timestamp TYPE timestampl.
    DATA lv_int TYPE int4.
    DATA lv_text TYPE string.
    DATA lv_shared_secret TYPE string.

    DATA lv_hash_string_temp TYPE string.
    DATA lv_hash_string_final TYPE string.
    DATA lv_body TYPE string.


* prepare test data
    lv_shared_secret = 'we_shared_this_offline_and_decrypted_it_w_userkey_here'.
    GET TIME STAMP FIELD lv_timestamp.
    lv_int = iv_object_id.
    lv_text = iv_baseline_id.
    lv_body = iv_payload.

* create a message digest object with a given hash algo
    lo_digest = cl_abap_message_digest=>get_instance( 'sha1' ).

    lo_digest->update( if_data = cl_abap_message_digest=>string_to_xstring( |{ lv_timestamp }| ) ).
    lo_digest->update( if_data = cl_abap_message_digest=>string_to_xstring( |{ lv_int }| ) ).
    lo_digest->update( if_data = cl_abap_message_digest=>string_to_xstring( |{ lv_text }| ) ).

    lo_digest->digest( ).

    lv_hash_string_temp = lo_digest->to_string( ).

    lo_digest = cl_abap_message_digest=>get_instance( 'sha1' ).
    lo_digest->update( if_data = cl_abap_message_digest=>string_to_xstring( |{ lv_hash_string_temp }| ) ).
    lo_digest->update( if_data = cl_abap_message_digest=>string_to_xstring( |{ lv_shared_secret }| ) ).

    lo_digest->digest( ).
    lv_hash_string_final = lo_digest->to_string( ).

    ev_timestamp = lv_timestamp.
    ev_outputhash = lv_hash_string_final.

  ENDMETHOD.
  METHOD verify_mock_hash.
    DATA: lv_shared_secret TYPE string.
    "could be loaded via vault service API or sap user auth check
    lv_shared_secret = 'we_shared_this_offline_and_decrypted_it_w_userkey_here'.
  ENDMETHOD.
  METHOD generate_mock_input_hash.

  ENDMETHOD.
ENDCLASS.
