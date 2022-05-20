CLASS z100085_zcl_proubc_objstatapi DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS if_rest_resource~get
        REDEFINITION .
    METHODS if_rest_resource~put
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_objstatapi IMPLEMENTATION.


  METHOD if_rest_resource~get.
    DATA(lt_uriattributes) = mo_request->get_uri_attributes( ).
    DATA(lo_entity) = mo_response->create_entity( ).
    DATA ls_status_response TYPE z100085_zif_proubc_object=>ty_update_status_res.
    DATA lv_statusdata TYPE REF TO data.
    READ TABLE lt_uriattributes WITH KEY name = 'ID' ASSIGNING FIELD-SYMBOL(<fs_object_get>).
    IF sy-subrc = 0.
      DATA lv_objectid TYPE z100085_bpiobj-object_id.
      lv_objectid = <fs_object_get>-value.
      z100085_zcl_proubc_busobjhlpr=>get_object_status(
        EXPORTING
          iv_objectid = lv_objectid
        IMPORTING
          es_objects  = ls_status_response
      ).
    ELSE.
    ENDIF.
    "/objects
    z100085_zcl_proubc_api_helper=>copy_data_to_ref(
            EXPORTING is_data = ls_status_response
            CHANGING cr_data = lv_statusdata
      ).

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( /ui2/cl_json=>serialize( EXPORTING data = lv_statusdata pretty_name = /ui2/cl_json=>pretty_mode-low_case ) ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).

    "/objects/{ID}/status





  ENDMETHOD.

  METHOD if_rest_resource~put.

    DATA(lo_entity) = mo_response->create_entity( ).

    DATA: ls_objectstat   TYPE z100085_zif_proubc_object=>ty_update_status_req,
          ls_resp_objstat TYPE z100085_zif_proubc_object=>ty_update_status_res.
    DATA(lv_request_body) = mo_request->get_entity( )->get_string_data( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ls_objectstat ).

    DATA(lt_uriattributes) = mo_request->get_uri_attributes( ).
    READ TABLE lt_uriattributes WITH KEY name = 'ID' ASSIGNING FIELD-SYMBOL(<fs_object_put>).
    IF sy-subrc = 0.
      DATA lv_objectid TYPE z100085_bpiobj-object_id.
      lv_objectid = <fs_object_put>-value.
      z100085_zcl_proubc_busobjhlpr=>update_object_status(
        EXPORTING
          iv_objectid = lv_objectid
          is_object   = ls_objectstat
        IMPORTING
          et_objects  = ls_resp_objstat
      ).
    ENDIF.

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( /ui2/cl_json=>serialize( EXPORTING data = ls_resp_objstat pretty_name = /ui2/cl_json=>pretty_mode-low_case ) ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
    "/objects/{ID}/status
  ENDMETHOD.
ENDCLASS.
