CLASS z100085_zcl_proubc_obtrigtest DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  METHODS: if_rest_resource~post REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_obtrigtest IMPLEMENTATION.
    method if_rest_resource~post.
        data: lo_api_helper type ref to z100085_zcl_proubc_api_helper,
              lv_status type i,
              ls_protocol_msg_req  TYPE z100085_zif_proubc_baseline=>protocolmessage_req.

        /ui2/cl_json=>deserialize(
*          EXPORTING
*            json             =
*            jsonx            =
*            pretty_name      =
*            assoc_arrays     =
*            assoc_arrays_opt =
*            name_mappings    =
*            conversion_exits =
          CHANGING
            data             = ls_protocol_msg_req
        ).
        lo_api_helper = new Z100085_zcl_proubc_api_helper( ).
        lo_api_helper->setup_protocol_msg( ).

        lo_api_helper->send_protocol_msg( EXPORTING body = ls_protocol_msg_req IMPORTING statuscode = lv_status  ).
    endmethod.
ENDCLASS.
