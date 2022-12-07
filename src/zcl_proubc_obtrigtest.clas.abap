CLASS zcl_proubc_obtrigtest DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS if_rest_resource~post
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PROUBC_OBTRIGTEST IMPLEMENTATION.


  METHOD if_rest_resource~post.
    DATA: lo_api_helper             TYPE REF TO zcl_proubc_api_helper,
          lv_status                 TYPE i,
          ls_protocol_msg_req       TYPE zif_proubc_baseline=>protocolmessage_req,
          ls_dummy_protocol_msg_req TYPE zif_proubc_baseline=>protocolmessage_req,
          ls_testresponse           TYPE zif_proubc_testobj=>ty_obtrigtest,
          lv_testresponse           TYPE string,
          lv_apiresponsestr         TYPE string,
          lv_apiresponse            TYPE REF TO data.

    lo_api_helper = NEW zcl_proubc_api_helper( ).

    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).

    DATA(lv_request_body) = mo_request->get_entity( )->get_string_data( ).
    /ui2/cl_json=>deserialize(
          EXPORTING
            json             = lv_request_body
      CHANGING
        data             = ls_protocol_msg_req ).

    IF ls_protocol_msg_req IS INITIAL.
      ls_dummy_protocol_msg_req = lo_api_helper->build_dummy_idoc_protocol_msg( ).
    ENDIF.

    DATA: lv_setup_success TYPE boolean.
    lo_api_helper->setup_protocol_msg( IMPORTING setup_success = lv_setup_success ).

    IF lv_setup_success = 'X'.
      lo_api_helper->send_protocol_msg( EXPORTING body = ls_protocol_msg_req IMPORTING statuscode = lv_status
                                                                                       apiresponsestr = lv_apiresponsestr
                                                                                        ).

      ls_testresponse-bpi_tenant_id = lo_api_helper->get_default_tenant( ).
      ls_testresponse-status = lv_status. "201, 400, etc
      ls_testresponse-bpi_response_payload = lv_apiresponsestr.
      ls_testresponse-bpi_endpoint = lo_api_helper->get_default_tenant_bpiendpoint( ).
      ls_testresponse-msg_payload = ls_protocol_msg_req.
    ELSE.
      ls_testresponse-bpi_tenant_id = lo_api_helper->get_default_tenant( ).
      ls_testresponse-status = 401. "201, 400, etc
      ls_testresponse-bpi_response_payload = 'ident call failed - check tenant if reachable'.
      ls_testresponse-bpi_endpoint = lo_api_helper->get_default_tenant_bpiendpoint( ).
      ls_testresponse-msg_payload = ls_protocol_msg_req.
    ENDIF.

    lv_testresponse = /ui2/cl_json=>serialize( data        = ls_testresponse
                                               pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_testresponse ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).

  ENDMETHOD.
ENDCLASS.
