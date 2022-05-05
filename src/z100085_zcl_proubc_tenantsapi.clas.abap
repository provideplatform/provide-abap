CLASS z100085_zcl_proubc_tenantsapi DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      if_rest_resource~get REDEFINITION,
      if_rest_resource~post REDEFINITION,
      if_rest_resource~put REDEFINITION,
      if_rest_resource~delete REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_tenantsapi IMPLEMENTATION.
  METHOD if_rest_resource~get.
    DATA: li_api             TYPE REF TO if_mr_api,
          lv_data            TYPE string,
          lv_mime            TYPE string,
          lv_url             TYPE string,
          lv_tenantid        TYPE z100085_zs_prvdorg-organization_id,
          lt_prvdtenants     TYPE z100085_ztt_prvdorg,
          ls_prvdtenant      TYPE z100085_prvdorgs,
          lt_prvdtenants_in  TYPE z100085_ztt_prvdorg,
          lt_prvdtenants_out TYPE z100085_ztt_prvdorg,
          "lo_entity          type ref to if_rest_response,
          lv_tenantdata      TYPE REF TO data.

    "TODO add SAP auths for reading the tenant(s)

    DATA(lt_uriattributes) = mo_request->get_uri_attributes( ).
    READ TABLE lt_uriattributes WITH KEY name = 'ID' ASSIGNING FIELD-SYMBOL(<fs_tenantid>).
    IF sy-subrc = 0.
      lv_tenantid = <fs_tenantid>-value.
      z100085_zcl_proubc_prvdtenants=>get_prvdtenant(
        EXPORTING
          iv_prvdtenant = lv_tenantid
        IMPORTING
          ev_prvdtenant = ls_prvdtenant
      ).

      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( /ui2/cl_json=>serialize( exporting data = ls_prvdtenant pretty_name = /ui2/cl_json=>pretty_mode-low_case ) ).
      mo_response->set_status( cl_rest_status_code=>gc_success_ok ).

    ELSE.


      z100085_zcl_proubc_prvdtenants=>get_allprvdtenant( IMPORTING et_prvdorg = lt_prvdtenants ).
      z100085_zcl_proubc_api_helper=>copy_data_to_ref(
            EXPORTING is_data = lt_prvdtenants
            CHANGING cr_data = lv_tenantdata
      ).

      lo_entity = mo_response->create_entity( ).
      lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( /ui2/cl_json=>serialize( exporting data = lv_tenantdata pretty_name = /ui2/cl_json=>pretty_mode-low_case ) ).
      mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
    ENDIF.
  ENDMETHOD.
  METHOD if_rest_resource~post.
    DATA: lt_prvdtenants     TYPE z100085_ztt_prvdorg,
          ls_prvdtenant      TYPE z100085_ZS_PRVDORG,
          lt_prvdtenants_out TYPE z100085_ztt_prvdorg,
          lv_tenantdata      TYPE REF TO data.

    DATA(lv_request_body) = mo_request->get_entity( )->get_string_data( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = lt_prvdtenants ).

    z100085_zcl_proubc_prvdtenants=>create_prvdtenant( EXPORTING it_prvdorg = lt_prvdtenants IMPORTING et_prvdorg = lt_prvdtenants_out ).
    z100085_zcl_proubc_api_helper=>copy_data_to_ref(
          EXPORTING is_data = lt_prvdtenants_out
          CHANGING cr_data = lv_tenantdata
    ).
    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( /ui2/cl_json=>serialize( exporting data = lv_tenantdata pretty_name = /ui2/cl_json=>pretty_mode-low_case ) ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

  METHOD if_rest_resource~put.
    DATA: lt_prvdtenants     TYPE z100085_ztt_prvdorg,
          ls_prvdtenant      TYPE z100085_ZS_PRVDORG,
          lt_prvdtenants_out TYPE z100085_ztt_prvdorg,
          lv_tenantdata      TYPE REF TO data.

    DATA(lv_request_body) = mo_request->get_entity( )->get_string_data( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = lt_prvdtenants ).

    z100085_zcl_proubc_prvdtenants=>update_prvdtenant( EXPORTING it_prvdorg = lt_prvdtenants IMPORTING et_prvdorg = lt_prvdtenants_out ).
    z100085_zcl_proubc_api_helper=>copy_data_to_ref(
          EXPORTING is_data = lt_prvdtenants_out
          CHANGING cr_data = lv_tenantdata
    ).
    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( /ui2/cl_json=>serialize( exporting data = lv_tenantdata pretty_name = /ui2/cl_json=>pretty_mode-low_case  ) ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

  METHOD if_rest_resource~delete.
    DATA: lv_tenantid TYPE z100085_zs_prvdorg-organization_id.
    DATA(lt_uriattributes) = mo_request->get_uri_attributes( ).
    READ TABLE lt_uriattributes WITH KEY name = 'ID' ASSIGNING FIELD-SYMBOL(<fs_tenantid>).
    IF sy-subrc = 0.
      lv_tenantid = <fs_tenantid>-value.
      z100085_zcl_proubc_prvdtenants=>delete_prvdtenant( IMPORTING ev_prvdorgid = lv_tenantid ).
    ENDIF.
    "TODO add a delete response if totally necessary....

  ENDMETHOD.
ENDCLASS.
