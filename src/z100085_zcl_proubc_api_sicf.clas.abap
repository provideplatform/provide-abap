CLASS z100085_zcl_proubc_api_sicf DEFINITION
  PUBLIC
 INHERITING FROM cl_rest_http_handler
  CREATE PUBLIC .

  PUBLIC SECTION.
    "INTERFACES: if_http_extension .
    METHODS: if_rest_application~get_root_handler REDEFINITION,
      handle_request_old.
    CONSTANTS: c_json TYPE string VALUE 'application/json'.
  PROTECTED SECTION.
    METHODS:
      get_controller,
      tenants
        IMPORTING
          !ii_server      TYPE REF TO if_http_server
          !Iv_httpverb    TYPE string
          !IV_payload     TYPE string
          !iv_payloaddata TYPE REF TO data
          !it_tenants_in  TYPE z100085_Ztt_prvdorg
          !iv_url         TYPE string
        EXPORTING
          !ev_payload     TYPE string
          !ev_returncode  TYPE string,
      status
        IMPORTING
          !ii_server     TYPE REF TO if_http_server
          !Iv_httpverb   TYPE string
          !IV_payload    TYPE string
          !iv_url        TYPE string
        EXPORTING
          !ev_payload    TYPE string
          !ev_returncode TYPE string,
      business_objects
        IMPORTING
          !ii_server     TYPE REF TO if_http_server
          !Iv_httpverb   TYPE string
          !IV_payload    TYPE string
          !iv_url        TYPE string
        EXPORTING
          !ev_payload    TYPE string
          !ev_returncode TYPE string,
      business_object_models
        IMPORTING
          !ii_server     TYPE REF TO if_http_server
          !Iv_httpverb   TYPE string
          !IV_payload    TYPE string
          !iv_url        TYPE string
        EXPORTING
          !ev_payload    TYPE string
          !ev_returncode TYPE string,
      proxies
        IMPORTING
          !ii_server     TYPE REF TO if_http_server
          !Iv_httpverb   TYPE string
          !IV_payload    TYPE string
          !iv_url        TYPE string
        EXPORTING
          !ev_payload    TYPE string
          !ev_returncode TYPE string,
      auth
        IMPORTING
          !ii_server     TYPE REF TO if_http_server
          !Iv_httpverb   TYPE string
          !IV_payload    TYPE string
          !iv_url        TYPE string
        EXPORTING
          !ev_payload    TYPE string
          !ev_returncode TYPE string,
      copy_data_to_ref
        IMPORTING
          !is_data TYPE any
        CHANGING
          !cr_data TYPE REF TO data ,
      read_mime
        IMPORTING
          !ii_server TYPE REF TO if_http_server
          !iv_url    TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_api_sicf IMPLEMENTATION.


  METHOD get_controller.
  ENDMETHOD.


  METHOD tenants.
    DATA: li_api            TYPE REF TO if_mr_api,
          lv_data           TYPE string,
          lv_mime           TYPE string,
          lv_url            TYPE string,
          lv_prvdorgid      TYPE z100085_zs_prvdorg-organization_id,
          lt_prvdtenants    TYPE z100085_ztt_prvdorg,
          ls_prvdtenant     TYPE z100085_ZS_PRVDORG,
          lt_prvdtenants_in TYPE z100085_ztt_prvdorg,
          lv_tenantdata     TYPE REF TO data.

    TRY.
        CASE iv_httpverb.
          WHEN 'GET'.
            z100085_zcl_proubc_prvdtenants=>get_allprvdtenant( IMPORTING et_prvdorg = lt_prvdtenants ).
            copy_data_to_ref(
                EXPORTING is_data = lt_prvdtenants
                CHANGING cr_data = lv_tenantdata
            ).
            /ui2/cl_json=>serialize(
              EXPORTING
                data             = lv_tenantdata
*            pretty_name      =
              RECEIVING
                r_json           = lv_data
            ).
          WHEN 'PUT'.
            "z100085_zcl_proubc_prvdtenants=>update_prvdtenant( it_prvdorg = )
          WHEN 'POST'.
            "move-corresponding iv_payloaddata to ls_prvdtenant.

            "z100085_zcl_proubc_api_helper=>map_data_to_tenant( EXPORTING iv_data = iv_payloaddata ).
          "  z100085_zcl_proubc_prvdtenants=>create_prvdtenant( EXPORTING is_prvdorg = lt_prvdtenants ).
          WHEN 'DELETE'.
          WHEN OTHERS.
        ENDCASE.
      CATCH cx_root.
        "todo exception handling
    ENDTRY.

    ii_server->response->set_compression( ).
    ii_server->response->set_content_type( c_json ).
    "ii_server->response->set_data( lv_data ).
    ii_server->response->set_cdata( lv_data ).
  ENDMETHOD.


  METHOD status.
    DATA: li_api  TYPE REF TO if_mr_api,
          lv_data TYPE xstring,
          lv_mime TYPE string,
          lv_url  TYPE string.

    lv_data = 'status'.

    ii_server->response->set_compression( ).
    ii_server->response->set_content_type( c_json ).
    ii_server->response->set_data( lv_data ).
  ENDMETHOD.


  METHOD business_objects.
    DATA: li_api  TYPE REF TO if_mr_api,
          lv_data TYPE xstring,
          lv_mime TYPE string,
          lv_url  TYPE string.

    lv_data = 'business objects'.

    ii_server->response->set_compression( ).
    ii_server->response->set_content_type( c_json ).
    ii_server->response->set_data( lv_data ).
  ENDMETHOD.


  METHOD business_object_models.
    DATA: li_api  TYPE REF TO if_mr_api,
          lv_data TYPE xstring,
          lv_mime TYPE string,
          lv_url  TYPE string.

    lv_data = 'business_object_models'.

    ii_server->response->set_compression( ).
    ii_server->response->set_content_type( c_json ).
    ii_server->response->set_data( lv_data ).
  ENDMETHOD.


  METHOD proxies.
    DATA: li_api  TYPE REF TO if_mr_api,
          lv_data TYPE xstring,
          lv_mime TYPE string,
          lv_url  TYPE string.

    lv_data = 'proxy'.

    ii_server->response->set_compression( ).
    ii_server->response->set_content_type( c_json ).
    ii_server->response->set_data( lv_data ).
  ENDMETHOD.


  METHOD auth.
    DATA: li_api  TYPE REF TO if_mr_api,
          lv_data TYPE string, "xstring,
          lv_mime TYPE string,
          lv_url  TYPE string.

    lv_data = '{ method: auth}'.

    ii_server->response->set_compression( ).
    ii_server->response->set_content_type( c_json ).
    "ii_server->response->set_data( lv_data ).
    ii_server->response->set_cdata(
        EXPORTING
            data   = lv_data    " Character data
*        offset = 0    " Offset into character data
*        length = -1    " Length of character data
    ).
  ENDMETHOD.


  METHOD read_mime.
    DATA: li_api  TYPE REF TO if_mr_api,
          lv_data TYPE xstring,
          lv_mime TYPE string,
          lv_url  TYPE string.

    CONCATENATE '/SAP/PUBLIC/PROUBC' iv_url INTO lv_url.

    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).

    li_api->get(
        EXPORTING
          i_url = lv_url
        IMPORTING
          e_content = lv_data
          e_mime_type = lv_mime
        EXCEPTIONS
          not_found = 1
    ).

    IF sy-subrc = 1.
      ii_server->response->set_cdata( '404' ).
      ii_server->response->set_status( code = 404 reason = '404' ).
      RETURN.
    ENDIF.

    ii_server->response->set_compression( ).
    ii_server->response->set_content_type( lv_mime ).
    ii_server->response->set_data( lv_data ).
  ENDMETHOD.


  METHOD copy_data_to_ref. "copied from oData implementation. see DPC class
    FIELD-SYMBOLS:
                 <ls_data> TYPE any.

    CREATE DATA cr_data LIKE is_data.
    ASSIGN cr_data->* TO <ls_data>.
    <ls_data> = is_data.

  ENDMETHOD.


  METHOD handle_request_old.
    "    DATA: lv_path          TYPE string,
    "         lv_pathfull      TYPE string,
    "          lv_query_string  TYPE string,
    "          lv_httpverb      TYPE string,
    "          lv_payload       TYPE string,
    "          lv_name          TYPE string,
    "          li_http          TYPE REF TO if_http_extension,
    "          lt_fields        TYPE tihttpnvp,
    "          lv_returncode    TYPE string,
    "          lv_returnpayload TYPE string,
    "          lt_tenants_in type Z100085_ZTT_PRVDORG.
    "
    "
    "    " get URL info - will be used for finding matching URL patterns and specified controllers
    "    lv_pathfull = server->request->get_header_field( name = '~path_translated' ).
    "    lv_path = server->request->get_header_field( '~path' ).
    "    lv_httpverb = server->request->get_header_field( '~request_method' ).
    "    lv_query_string = cl_http_utility=>unescape_url( server->request->get_header_field( name = '~query_string' ) ).
    "
    "    server->request->get_header_fields(
    "      CHANGING
    "        fields = lt_fields    " Header fields
    "    ).
    "
    "    "get payload if put or post
    "    "maybe too generic - maybe should handle in specific routes rather than generically
    "*    IF lv_httpverb = 'PUT' OR lv_httpverb = 'POST'.
    "*        data lv_payloaddata type ref to data.
    "*        data(lv_maybepayload) = server->request->get_cdata( ).
    "*       /UI2/CL_JSON=>deserialize( exporting json = lv_maybepayload
    "*                                  changing data = lv_payloaddata ).
    "*    ENDIF.
    "
    "    " get requested content type
    "    DATA(lv_req_content_type) = server->request->get_content_type( ).
    "
    "    " if not specified, initial content type is set to JSON
    "    IF lv_req_content_type IS INITIAL.
    "      lv_req_content_type = c_json.
    "    ENDIF.
    "
    "*
    "*        case lv_path.
    "*            when 'tenants'.
    "*            when 'status'.
    "*            when 'business_objects'.
    "*            when 'business_object_models'.
    "*            when 'proxies'.
    "*            when 'auth'.
    "*        endcase.
    "
    "    "lousy code, but gotta go fast here
    "    "may need to write this into multiple handlers depending on complexity
    "    TRY.
    "        IF lv_path EQ '/sap/proubc/tenants'.
    "
    "          IF lv_httpverb = 'PUT' OR lv_httpverb = 'POST'.
    "            DATA lv_payloaddata TYPE REF TO data.
    "            DATA(lv_maybepayload) = server->request->get_cdata( ).
    "            /ui2/cl_json=>deserialize( EXPORTING json = lv_maybepayload
    "                                       CHANGING data = lv_payloaddata ).
    "          ENDIF.
    "
    "          me->tenants( EXPORTING ii_server = server
    "                                 iv_httpverb = lv_httpverb
    "                                 iv_payload = lv_payload
    "                                 iv_payloaddata = lv_payloaddata
    "                                 iv_url = lv_path
    "                       IMPORTING  ev_payload  = lv_returnpayload
    "                                  ev_returncode = lv_returncode ).
    "
    "        ELSEIF lv_path EQ '/sap/proubc/status'.
    "          me->status( EXPORTING ii_server = server
    "                                 iv_httpverb = lv_httpverb
    "                                 iv_payload = lv_payload
    "                                       iv_url = lv_path
    "                       IMPORTING  ev_payload  = lv_returnpayload
    "                                  ev_returncode = lv_returncode  ).
    "        ELSEIF lv_path+28 EQ '/sap/proubc/business_objects'.
    "          me->business_objects( EXPORTING ii_server = server
    "                                          iv_httpverb = lv_httpverb
    "                                          iv_payload = lv_payload
    "                                          iv_url = lv_path
    "                       IMPORTING  ev_payload  = lv_returnpayload
    "                                  ev_returncode = lv_returncode ).
    "        ELSEIF lv_path EQ '/sap/proubc/business_object_models'.
    "          me->business_object_models( EXPORTING ii_server = server
    "                                                iv_httpverb = lv_httpverb
    "                                                iv_payload = lv_payload
    "                                                iv_url = lv_path
    "                       IMPORTING  ev_payload  = lv_returnpayload
    "                                  ev_returncode = lv_returncode ).
    "          " We replaced this with tenants right?
    "*        elseif lv_path cp 'proxies'.
    "*            me->proxies( exporting ii_server = server
    "*                                         iv_httpverb = lv_httpverb
    "*                                         iv_payload = lv_payload
    "*                                         iv_url = lv_path
    "*                         importing  ev_payload  = lv_returnpayload
    "*                                    ev_returncode = lv_returncode ).
    "        ELSEIF lv_path CP '/sap/proubc/auth'.
    "          me->auth( EXPORTING ii_server = server
    "                              iv_httpverb = lv_httpverb
    "                              iv_payload = lv_payload
    "                                       iv_url = lv_path
    "                       IMPORTING  ev_payload  = lv_returnpayload
    "                                  ev_returncode = lv_returncode ).
    "        ELSE. "default redirect to swagger doc / error?
    "*            "server->response->set_cdata( '404' ).
    "*            data: lv_errorpage type xstring value 'error finding path binding'.
    "*            server->response->set_data( lv_errorpage  ).
    "*            server->response->set_status( code = 404 reason = '404' ).
    "
    "          me->auth( EXPORTING ii_server = server
    "                              iv_httpverb = lv_httpverb
    "                              iv_payload = lv_payload
    "                                       iv_url = lv_path
    "                       IMPORTING  ev_payload  = lv_returnpayload
    "                                  ev_returncode = lv_returncode ).
    "        ENDIF.
    "      CATCH cx_root.
    "        "todo add more exception handling
    "    ENDTRY.
    "
    "    " /proubc/tenants/{id}/proxy handler
    "    " /proubc/status health check
    "    " /proubc/business_objects/{id}/status
    "    " /proubc/business_object_models/?=recordType
    "    " /proubc/proxies
    "    " /proubc/auth
  ENDMETHOD.


  METHOD if_rest_application~get_root_handler.
     DATA(lo_router) = NEW cl_rest_router( ).
       lo_router->attach( iv_template = '/tenants'   iv_handler_class = 'Z100085_ZCL_PROUBC_TENANTSAPI' ).
       lo_router->attach( iv_template = '/tenants/{ID}' iv_handler_class = 'Z100085_ZCL_PROUBC_TENANTSAPI' ).
       lo_router->attach( iv_template = '/status' iv_handler_class = 'Z100085_ZCL_PROUBC_HEALTHAPI' ).

       "middleware tells SAP to create a new object (ex: sales order)
       lo_router->attach( iv_template = '/objects' iv_handler_class = 'Z100085_ZCL_PROUBC_BUSOBJAPI' ).
       "middleware tells SAP to update existing object by id. Are we querying by object ID or by baseline ID?
       lo_router->attach( iv_template = '/objects/{ID}' iv_handler_class = 'Z100085_ZCL_PROUBC_OBJIDAPI' ).
       " { table struct + type: ""} type ===
       "middleware tells SAP to update status in BPI table
       lo_router->attach( iv_template = '/objects/{ID}/status' iv_handler_class = 'Z100085_ZCL_PROUBC_OBJSTATAPI' ).

       " { status: "something", baseline_id: ""}

       lo_router->attach( iv_template = '/auth' iv_handler_class = 'Z100085_ZCL_PROUBC_AUTHAPI').
       lo_router->attach( iv_template = '/schemas'   iv_handler_class = 'Z100085_ZCL_IDOCAPI_BTYPEAPI' ).
       lo_router->attach( iv_template = '/schemas/{basictypeid}'   iv_handler_class = 'Z100085_ZCL_IDOCAPI_SEGMENTAPI' ).
     ro_root_handler = lo_router.
  ENDMETHOD.
ENDCLASS.
