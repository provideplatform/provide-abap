CLASS z100085_zcl_proubc_api_sicf DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
  interfaces: if_http_extension .
  PROTECTED SECTION.
  methods read_mime
    importing
        !II_SERVER type ref to IF_HTTP_SERVER
        !IV_URL type string .
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_api_sicf IMPLEMENTATION.
    method if_http_extension~handle_request.
        data: lv_path type string,
              lv_name type string,
              li_http type ref to if_http_extension.

        lv_path = server->request->get_header_field( '~path' ).

        " /proubc/organizations/{id}/proxy handler
        " /proubc/status health check
        " /proubc/business_objects/{id}/status
        " /proubc/business_object_models/?=recordType
        " /proubc/proxies
        " /proubc/auth
    endmethod.

    method read_mime.
        data: li_api type ref to if_mr_api,
              lv_data type xstring,
              lv_mime type string,
              lv_url type string.

        concatenate '/SAP/PUBLIC/PROUBC' iv_url into lv_url.

        li_api = cl_mime_repository_api=>if_mr_api~get_api( ).

        li_api->get(
            exporting
              i_url = lv_url
            importing
              e_content = lv_data
              e_mime_type = lv_mime
            exceptions
              not_found = 1
        ).

        if sy-subrc = 1.
            ii_server->response->set_cdata( '404' ).
            ii_server->response->set_status( code = 404 reason = '404' ).
            return.
        endif.

        ii_server->response->set_compression( ).
        ii_server->response->set_content_type( lv_mime ).
        ii_server->response->set_data( lv_data ).
    endmethod.
ENDCLASS.
