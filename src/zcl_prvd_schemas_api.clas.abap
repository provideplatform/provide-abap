CLASS zcl_prvd_schemas_api DEFINITION
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



CLASS zcl_prvd_schemas_api IMPLEMENTATION.
  METHOD if_rest_resource~get.

    DATA(lt_uriattributes) = mo_request->get_uri_attributes( ).
    DATA: lv_schematype TYPE string,
          lt_basictypes TYPE zif_idocapi_typelist=>tt_basictype.

    READ TABLE lt_uriattributes WITH KEY name = 'schematype' ASSIGNING FIELD-SYMBOL(<fs_schematype>).
    IF sy-subrc = 0.
      lv_schematype = <fs_schematype>-value.
      TRANSLATE lv_schematype TO UPPER CASE.
    ENDIF.

    CASE lv_schematype.
      WHEN 'IDOC'.
        SELECT  a~idoctyp
        b~descrp
        a~presp
        a~pwork
        a~plast
        a~closed
        a~released
        a~pretyp
        a~generated
        a~credate
        a~cretime
        a~ldate
        a~ltime
        a~applrel
  FROM edbas AS a
  LEFT OUTER JOIN edbast AS b ON a~idoctyp = b~idoctyp
                              AND b~langua = 'E'
  INTO TABLE lt_basictypes.

        IF sy-subrc = 0.
          DATA(lo_entity) = mo_response->create_entity( ).
          lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
          lo_entity->set_string_data( /ui2/cl_json=>serialize( data        = lt_basictypes
                                                               pretty_name = /ui2/cl_json=>pretty_mode-low_case ) ).
          mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
        else.
          "message No idoc types found
        ENDIF.
      WHEN 'DDIC'.
      "Handle DDIC types
      WHEN OTHERS.
        mo_response->set_status( cl_rest_status_code=>gc_client_error_not_found ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
