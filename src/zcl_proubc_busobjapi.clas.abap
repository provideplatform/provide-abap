CLASS zcl_proubc_busobjapi DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS if_rest_resource~get
        REDEFINITION .
    METHODS if_rest_resource~post
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PROUBC_BUSOBJAPI IMPLEMENTATION.


  METHOD if_rest_resource~get.
    "/objects/{ID}
    "todo handle other URI params
    DATA(lo_entity) = mo_response->create_entity( ).
    DATA lv_objectid TYPE zbpiobj-object_id.
    DATA lt_object TYPE  ztty_bpiobj.
    DATA ls_object TYPE zbpiobj.
    DATA lv_bpiobjdata TYPE REF TO data.

    zcl_proubc_busobjhlpr=>get_object(
      EXPORTING
        iv_objectid = lv_objectid
      IMPORTING
        et_objects  = lt_object
    ).
    zcl_proubc_api_helper=>copy_data_to_ref(
            EXPORTING is_data = lt_object
            CHANGING cr_data = lv_bpiobjdata
      ).


    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( /ui2/cl_json=>serialize( EXPORTING data = lv_bpiobjdata pretty_name = /ui2/cl_json=>pretty_mode-low_case ) ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.


  METHOD if_rest_resource~post.
    "POST /objects
    DATA(lo_entity) = mo_response->create_entity( ).
    DATA lv_bpiobjdata TYPE REF TO data.
    "create the business object
    DATA: lt_objects TYPE zif_proubc_object=>tty_create_object_req_objid.
    DATA(lv_request_body) = mo_request->get_entity( )->get_string_data( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = lt_objects ).

    DATA: ls_obj      TYPE zbpiobj,
          lt_obj      TYPE ztty_bpiobj,
          lt_resp_obj TYPE ztty_bpiobj.
    LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<fs_object>).
      CLEAR: ls_obj.
      ls_obj-baseline_id = <fs_object>-baseline_id.
      ls_obj-object_id = <fs_object>-object_id.
      ls_obj-schematype   = <fs_object>-schema_type.
      ls_obj-schema_id = <fs_object>-type.
      ls_obj-status = 'Created'.
      APPEND  ls_obj TO lt_obj.
    ENDLOOP.

    zcl_proubc_busobjhlpr=>create_object(
      EXPORTING
        it_objects = lt_obj
      IMPORTING
        et_objects = lt_resp_obj
    ).

    zcl_proubc_api_helper=>copy_data_to_ref(
           EXPORTING is_data = lt_resp_obj
           CHANGING cr_data = lv_bpiobjdata
   ).


    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( /ui2/cl_json=>serialize( EXPORTING data = lv_bpiobjdata pretty_name = /ui2/cl_json=>pretty_mode-low_case ) ).
    mo_response->set_status( cl_rest_status_code=>gc_success_created ).

  ENDMETHOD.
ENDCLASS.
