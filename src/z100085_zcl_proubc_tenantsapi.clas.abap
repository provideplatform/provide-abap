CLASS z100085_zcl_proubc_tenantsapi DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      if_rest_resource~get REDEFINITION,
      if_rest_resource~post REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_tenantsapi IMPLEMENTATION.
  METHOD if_rest_resource~get.
    DATA: li_api            TYPE REF TO if_mr_api,
          lv_data           TYPE string,
          lv_mime           TYPE string,
          lv_url            TYPE string,
          lv_prvdorgid      TYPE z100085_zs_prvdorg-organization_id,
          lt_prvdtenants    TYPE z100085_ztt_prvdorg,
          ls_prvdtenant     TYPE z100085_ZS_PRVDORG,
          lt_prvdtenants_in TYPE z100085_ztt_prvdorg,
          lt_prvdtenants_out type z100085_ztt_prvdorg,
          lv_tenantdata     TYPE REF TO data.

    z100085_zcl_proubc_prvdtenants=>get_allprvdtenant( IMPORTING et_prvdorg = lt_prvdtenants ).
    z100085_zcl_proubc_api_helper=>copy_data_to_ref(
          EXPORTING is_data = lt_prvdtenants
          CHANGING cr_data = lv_tenantdata
    ).

    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( /ui2/cl_json=>serialize( lv_tenantdata ) ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.
  METHOD if_rest_resource~post.
    DATA: lt_prvdtenants TYPE z100085_ztt_prvdorg,
          ls_prvdtenant  TYPE z100085_ZS_PRVDORG,
          lt_prvdtenants_out type z100085_ztt_prvdorg,
          lv_tenantdata type ref to data.

    DATA(lv_request_body) = mo_request->get_entity( )->get_string_data( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = lt_prvdtenants ).

    z100085_zcl_proubc_prvdtenants=>create_prvdtenant( EXPORTING it_prvdorg = lt_prvdtenants IMPORTING et_prvdorg = lt_prvdtenants_out ).
    z100085_zcl_proubc_api_helper=>copy_data_to_ref(
          EXPORTING is_data = lt_prvdtenants
          CHANGING cr_data = lv_tenantdata
    ).
    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( /ui2/cl_json=>serialize( lv_tenantdata ) ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.
ENDCLASS.
