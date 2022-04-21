CLASS z100085_zcl_proubc_api_sicf DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
  interfaces: if_http_extension .
  constants: c_json type string value 'application/json'.
  PROTECTED SECTION.
  methods:
   get_controller,
   organizations
       importing
        !II_SERVER type ref to IF_HTTP_SERVER
        !IV_URL type string,
   status
       importing
        !II_SERVER type ref to IF_HTTP_SERVER
        !IV_URL type string,
   business_objects
       importing
        !II_SERVER type ref to IF_HTTP_SERVER
        !IV_URL type string,
   business_object_models
       importing
        !II_SERVER type ref to IF_HTTP_SERVER
        !IV_URL type string,
   proxies
       importing
        !II_SERVER type ref to IF_HTTP_SERVER
        !IV_URL type string,
   auth
       importing
        !II_SERVER type ref to IF_HTTP_SERVER
        !IV_URL type string,
   read_mime
    importing
        !II_SERVER type ref to IF_HTTP_SERVER
        !IV_URL type string .
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_api_sicf IMPLEMENTATION.
    method if_http_extension~handle_request.
        data: lv_path type string,
              lv_pathfull type string,
              lv_query_string type string,
              lv_name type string,
              li_http type ref to if_http_extension.


     " get URL info - will be used for finding matching URL patterns and specified controllers
     lv_pathfull = server->request->get_header_field( name = '~path_translated' ).
     lv_path = server->request->get_header_field( '~path' ).
     lv_query_string = cl_http_utility=>unescape_url( server->request->get_header_field( name = '~query_string' ) ).

     " get requested content type
     DATA(lv_req_content_type) = server->request->get_content_type( ).

     " if not specified, initial content type is set to JSON
     IF lv_req_content_type IS INITIAL.
        lv_req_content_type = Z100083_if_rest_ws_prov=>gc_content_type-json.
     ENDIF.

*
*        case lv_path.
*            when 'organizations'.
*            when 'status'.
*            when 'business_objects'.
*            when 'business_object_models'.
*            when 'proxies'.
*            when 'auth'.
*        endcase.

        "lousy code, but gotta go fast here
        "may need to write this into multiple handlers depending on complexity
        if lv_path cp 'organizations'.
            me->organizations( exporting ii_server = server
                                         iv_url = lv_path ).
        elseif lv_path cp 'status'.
            me->status( exporting ii_server = server
                                         iv_url = lv_path ).
        elseif lv_path cp 'business_objects'.
            me->business_objects( exporting ii_server = server
                                         iv_url = lv_path ).
        elseif lv_path cp 'business_object_models'.
            me->business_object_models( exporting ii_server = server
                                         iv_url = lv_path ).
        elseif lv_path cp 'proxies'.
            me->proxies( exporting ii_server = server
                                         iv_url = lv_path ).
        elseif lv_path cp 'auth'.
            me->auth( exporting ii_server = server
                                         iv_url = lv_path ).
        else. "default redirect to swagger doc / error?
*            "server->response->set_cdata( '404' ).
*            data: lv_errorpage type xstring value 'error finding path binding'.
*            server->response->set_data( lv_errorpage  ).
*            server->response->set_status( code = 404 reason = '404' ).

            me->auth( exporting ii_server = server
                                         iv_url = lv_path ).
        endif.

        " /proubc/organizations/{id}/proxy handler
        " /proubc/status health check
        " /proubc/business_objects/{id}/status
        " /proubc/business_object_models/?=recordType
        " /proubc/proxies
        " /proubc/auth
    endmethod.

    method get_controller.
    endmethod.

    method organizations.
        data: li_api type ref to if_mr_api,
              lv_data type xstring,
              lv_mime type string,
              lv_url type string.

        lv_data = 'organizations'.

        ii_server->response->set_compression( ).
        ii_server->response->set_content_type( c_json ).
        ii_server->response->set_data( lv_data ).
    endmethod.

    method status.
        data: li_api type ref to if_mr_api,
              lv_data type xstring,
              lv_mime type string,
              lv_url type string.

        lv_data = 'status'.

        ii_server->response->set_compression( ).
        ii_server->response->set_content_type( c_json ).
        ii_server->response->set_data( lv_data ).
    endmethod.

    method business_objects.
        data: li_api type ref to if_mr_api,
              lv_data type xstring,
              lv_mime type string,
              lv_url type string.

        lv_data = 'business objects'.

        ii_server->response->set_compression( ).
        ii_server->response->set_content_type( c_json ).
        ii_server->response->set_data( lv_data ).
    endmethod.

    method business_object_models.
        data: li_api type ref to if_mr_api,
              lv_data type xstring,
              lv_mime type string,
              lv_url type string.

        lv_data = 'business_object_models'.

        ii_server->response->set_compression( ).
        ii_server->response->set_content_type( c_json ).
        ii_server->response->set_data( lv_data ).
    endmethod.

    method proxies.
        data: li_api type ref to if_mr_api,
              lv_data type xstring,
              lv_mime type string,
              lv_url type string.

        lv_data = 'proxy'.

        ii_server->response->set_compression( ).
        ii_server->response->set_content_type( c_json ).
        ii_server->response->set_data( lv_data ).
    endmethod.

    method auth.
        data: li_api type ref to if_mr_api,
              lv_data type xstring,
              lv_mime type string,
              lv_url type string.

        lv_data = 'auth'.

        ii_server->response->set_compression( ).
        ii_server->response->set_content_type( c_json ).
        ii_server->response->set_data( lv_data ).
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
