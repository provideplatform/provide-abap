*&---------------------------------------------------------------------*
*& Report zproubc_manage_schema_tlights
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprvd_manage_schema_tlights.

DATA: lo_prvd_schemas_helper TYPE REF TO zcl_prvd_schemas_helper,
      ls_traflight           TYPE zprvdtraflight.

PARAMETERS: p_sname  TYPE char100,
            p_sdesc  type char255 DEFAULT 'description',
            p_tlight TYPE zprvd_schema_trafficlight,
            p_stype  TYPE zprvd_schema_types,
            p_vfrom  TYPE sy-datum DEFAULT sy-datum,
            p_vto    TYPE sy-datum,
            p_setdel TYPE char1 AS CHECKBOX,
            p_godel  TYPE char1 AS CHECKBOX.

START-OF-SELECTION.
  lo_prvd_schemas_helper = NEW zcl_prvd_schemas_helper( ).

  IF p_godel IS NOT INITIAL.
  ELSE.
    lo_prvd_schemas_helper->add_schema_traflight(
      EXPORTING
        iv_schemaname        = p_sname
*        iv_valid_from        =
*        iv_valid_to          =
        iv_schema_tlight     = p_tlight
        iv_schema_type       =  p_stype
        iv_deletion_flag     = p_setdel
      IMPORTING
        es_created_traflight = ls_traflight
    ).
    IF sy-subrc = 0.
      WRITE: 'Updated traffic light settings'.
      "todo - add a printout if you really need it.
    ELSE.
      WRITE: 'Error setting traffing light'.
    ENDIF.
  ENDIF.
