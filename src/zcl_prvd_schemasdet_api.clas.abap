CLASS zcl_prvd_schemasdet_api DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS if_rest_resource~get
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PRVD_SCHEMASDET_API IMPLEMENTATION.


  METHOD if_rest_resource~get.
    DATA: lv_selectedbasictype TYPE string,
          ls_basictypes        TYPE zif_prvd_idoc=>ty_basictype,
          ls_responsedata      TYPE zif_prvd_idoc=>ty_basictype_w_segments,
          lv_idoctype          TYPE ledid_idoctype.

    DATA(lt_uriattributes) = mo_request->get_uri_attributes( ).
    READ TABLE lt_uriattributes WITH KEY name = 'schemadetailsid' ASSIGNING FIELD-SYMBOL(<fs_basictype>).
    IF sy-subrc = 0.
      lv_selectedbasictype = <fs_basictype>-value.
      REPLACE ALL OCCURRENCES OF '%2F' IN lv_selectedbasictype WITH '/'.
      lv_idoctype = lv_selectedbasictype.
    ENDIF.

    "get the selected idoc Basic type
    SELECT SINGLE  a~idoctyp,
            b~descrp,
            a~presp,
            a~pwork,
            a~plast,
            a~closed,
            a~released,
            a~pretyp,
            a~generated,
            a~credate,
            a~cretime,
            a~ldate,
            a~ltime,
            a~applrel
    FROM edbas AS a
    LEFT OUTER JOIN edbast AS b ON a~idoctyp = b~idoctyp
                                AND b~langua = 'E'
    WHERE a~idoctyp = @lv_selectedbasictype
    INTO @lS_basictypes.
    IF sy-subrc <> 0.
    ENDIF.

    ls_responsedata-basictype = ls_basictypes.

    GET PARAMETER ID 'SEG' FIELD DATA(l_segtyp).
    GET PARAMETER ID 'IDC' FIELD DATA(l_idoctyp).
    GET PARAMETER ID 'CIM' FIELD DATA(l_cimtyp).
    GET PARAMETER ID 'EDIDEF_OBJTYP' FIELD DATA(l_type).
    GET PARAMETER ID 'EDD' FIELD DATA(l_object).
    GET PARAMETER ID 'EDI_SELDOCU' FIELD DATA(l_recsel).

    DATA: lv_idoc_type      TYPE ledid_idoc_type,
          lt_idoc_struct    TYPE ledid_t_idoc_struct,
          lt_segments       TYPE ledid_t_segment,
          lt_segment_struct TYPE ledid_t_segment_struct.

    "get the idoc segments
    CALL FUNCTION 'IDOC_TYPE_COMPLETE_READ'
      EXPORTING
        struct_type    = 'B'
        idoctype       = lv_idoctype
        release        = ''
        applrel        = ''
        version        = '3'
      IMPORTING
        idoc_type      = lv_idoc_type
      TABLES
        idoc_struct    = lt_idoc_struct
        segments       = lt_segments
        segment_struct = lt_segment_struct
      EXCEPTIONS
        OTHERS         = 1.

    DATA: lv_idoc_schema_json_tree TYPE REF TO data.
    zcl_proubc_idochlpr=>idoc_schema_to_json_tree(
      EXPORTING
        it_idoc_struct           = lt_idoc_struct
        it_segments              = lt_segments
        it_segment_struct        = lt_segment_struct
      IMPORTING
        ev_idoc_schema_json_tree = lv_idoc_schema_json_tree ).

    DATA: lv_responsejson TYPE string.
    lv_responsejson = /ui2/cl_json=>serialize( data = lv_idoc_schema_json_tree ).

    "create the json HTTP response
    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_responsejson ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.
ENDCLASS.
