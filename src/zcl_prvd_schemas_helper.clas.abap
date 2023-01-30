CLASS zcl_prvd_schemas_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      "! Adds a schema traffic light entry
      add_schema_traflight IMPORTING !iv_schemaname        TYPE char100
                                     !iv_valid_from        TYPE timestampl OPTIONAL
                                     !iv_valid_to          TYPE timestampl OPTIONAL
                                     !iv_schema_tlight     TYPE zprvd_schema_trafficlight
                                     !iv_schema_type       TYPE zprvd_schema_types
                                     !iv_deletion_flag     TYPE char1 OPTIONAL
                           EXPORTING !es_created_traflight TYPE zprvdtraflight ,
      "! Deletes a schema traffic light entry
      delete_entries.
  PROTECTED SECTION.
    METHODS: create_schema_id IMPORTING !iv_schemaname TYPE char100
                                        !iv_valid_from TYPE timestampl OPTIONAL
                              EXPORTING !ev_schemaid   TYPE zcasesensitivesha1.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PRVD_SCHEMAS_HELPER IMPLEMENTATION.


  METHOD create_schema_id.
    DATA lo_digest TYPE REF TO cl_abap_message_digest.

    DATA lv_timestamp TYPE timestampl.
    DATA lv_int TYPE int4.
    DATA lv_text TYPE string.

    DATA lv_hash_string TYPE string.
    DATA lv_hash_base64 TYPE string.

* prepare test data
    IF iv_valid_from IS NOT INITIAL.
      lv_timestamp = iv_valid_from.
    ELSE.
      GET TIME STAMP FIELD lv_timestamp.
    ENDIF.

* create a message digest object with a given hash algo
    lo_digest = cl_abap_message_digest=>get_instance( 'sha256' ).
    lo_digest->update( if_data = cl_abap_message_digest=>string_to_xstring( |{ lv_timestamp }| ) ).
    lo_digest->update( if_data = cl_abap_message_digest=>string_to_xstring( |{ iv_schemaname }| ) ).
    lo_digest->digest( ).

    lv_hash_string = lo_digest->to_string( ).
    ev_schemaid = lv_hash_string.
  ENDMETHOD.


  METHOD add_schema_traflight.
    DATA: ls_schema_traflight    TYPE zprvdtraflight,
          lv_default_validfrom   TYPE timestampl,
          lv_temp_validto        TYPE timestampl,
          lv_default_validto     TYPE timestampl,
          lv_default_validtodate LIKE sy-datum,
          lv_default_validtotime LIKE sy-uzeit.

    IF iv_schemaname IS INITIAL.
      "raise schema name missing error
    ENDIF.

    ls_schema_traflight-schema_name = iv_schemaname.

    IF iv_valid_from IS NOT INITIAL.
      lv_default_validfrom = iv_valid_from.
    ELSE.
      GET TIME STAMP FIELD lv_default_validfrom.
    ENDIF.
    IF iv_valid_to IS NOT INITIAL.
      lv_default_validto = iv_valid_to.
    ELSE.
      GET TIME STAMP FIELD lv_temp_validto.
      CONVERT TIME STAMP lv_temp_validto TIME ZONE sy-timlo
      INTO DATE lv_default_validtodate TIME lv_default_validtotime.
      "trash this man...
      lv_default_validtodate = lv_default_validtodate + 7.
      CONVERT DATE lv_default_validtodate TIME lv_default_validtotime
          INTO TIME STAMP lv_default_validto  TIME ZONE sy-timlo.
    ENDIF.

    IF lv_default_validto < lv_default_validfrom.
    ENDIF.

    ls_schema_traflight-valid_from = lv_default_validfrom.
    ls_schema_traflight-valid_to = lv_default_validto.
    ls_schema_traflight-schema_tlight = iv_schema_tlight.
    ls_schema_traflight-schema_type = iv_schema_type.

    SELECT SINGLE * FROM zprvdtraflight INTO @DATA(ls_existing_tlight) WHERE
        schema_name = @ls_schema_traflight-schema_name
        AND schema_tlight = @ls_schema_traflight-schema_tlight
        AND schema_type = @ls_schema_traflight-schema_type
        AND valid_from GE @ls_schema_traflight-valid_from
        AND valid_to LE @ls_schema_traflight-valid_to.
    IF sy-subrc = 0.
      ls_schema_traflight-schema_id = ls_existing_tlight-schema_id.
      ls_schema_traflight-schema_name = ls_existing_tlight-schema_name.
      ls_schema_traflight-schema_type = ls_existing_tlight-schema_type.
      ls_schema_traflight-schema_tlight = ls_existing_tlight-schema_tlight.
      ls_schema_traflight-schema_desc = ls_existing_tlight-schema_desc.
      ls_schema_traflight-created_on = ls_existing_tlight-created_on.
      ls_schema_traflight-created_by = ls_existing_tlight-created_by.
      ls_schema_traflight-changed_by = sy-uname.
      GET TIME STAMP FIELD ls_schema_traflight-changed_on.
      ls_schema_traflight-deletion_flag = iv_deletion_flag.
    ELSEIF sy-subrc EQ 4.
      create_schema_id(
        EXPORTING
          iv_schemaname = iv_schemaname
          iv_valid_from = ls_schema_traflight-valid_from
        IMPORTING
          ev_schemaid   = ls_schema_traflight-schema_id ).
      ls_schema_traflight-created_by = sy-uname.
      GET TIME STAMP FIELD ls_schema_traflight-created_on.
      IF ls_schema_traflight-schema_id IS INITIAL.
        "message error creating schema id
      ENDIF.
    ELSE.
      "message error setting traffic light
    ENDIF.

    IF ls_schema_traflight IS NOT INITIAL.
      MODIFY zprvdtraflight FROM ls_schema_traflight.
      IF sy-subrc <> 0.
        "error updating table
      ENDIF.
    ELSE.
      "no data to update
    ENDIF.


  ENDMETHOD.


  METHOD delete_entries.
    DATA: lv_del_count TYPE i.
    SELECT * FROM zprvdtraflight INTO TABLE @DATA(lt_marked_to_delete) WHERE deletion_flag = 'X'.
    IF sy-subrc = 0.
      DESCRIBE TABLE lt_marked_to_delete LINES lv_del_count.
      DELETE zprvdtraflight FROM TABLE lt_marked_to_delete.
      IF sy-subrc = 0.
        WRITE: 'deleted entries:', 30 lv_del_count.
      ELSE.
        "error updating table
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
