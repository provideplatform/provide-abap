CLASS zcl_proubc_api_sicf DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_http_handler
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_json TYPE string VALUE 'application/json' ##NO_TEXT.

    "INTERFACES: if_http_extension .
    METHODS if_rest_application~get_root_handler
        REDEFINITION .
  PROTECTED SECTION.
    METHODS:
      read_mime
        IMPORTING
          !ii_server TYPE REF TO if_http_server
          !iv_url    TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PROUBC_API_SICF IMPLEMENTATION.


  METHOD if_rest_application~get_root_handler.
    DATA(lo_router) = NEW cl_rest_router( ).
    lo_router->attach( iv_template = '/tenants'   iv_handler_class = 'ZCL_PROUBC_TENANTSAPI' ).
    lo_router->attach( iv_template = '/tenants/{ID}' iv_handler_class = 'ZCL_PROUBC_TENANTSAPI' ).
    lo_router->attach( iv_template = '/tenants/{ID}/{SUBJACCTID}' iv_handler_class = 'ZCL_PROUBC_TENANTSAPI' ).
    lo_router->attach( iv_template = '/status' iv_handler_class = 'ZCL_PROUBC_HEALTHAPI' ).

    "middleware tells SAP to create a new object (ex: sales order)
    lo_router->attach( iv_template = '/objects' iv_handler_class = 'ZCL_PROUBC_BUSOBJAPI' ).
    "middleware tells SAP to update existing object by id. Are we querying by object ID or by baseline ID?
    lo_router->attach( iv_template = '/objects/{ID}' iv_handler_class = 'ZCL_PROUBC_OBJIDAPI' ).
    "middleware tells SAP to update status in BPI table
    lo_router->attach( iv_template = '/objects/{ID}/status' iv_handler_class = 'ZCL_PROUBC_OBJSTATAPI' ).

    "Endpoints used by Shuttle for workflow building
    lo_router->attach( iv_template = '/auth' iv_handler_class = 'ZCL_PROUBC_AUTHAPI').
    lo_router->attach( iv_template = '/schemas'   iv_handler_class = 'ZCL_IDOCAPI_BTYPEAPI' ).
    lo_router->attach( iv_template = '/schemas/{basictypeid}'   iv_handler_class = 'ZCL_IDOCAPI_SEGMENTAPI' ).

    "for test purposes only, builds a bunch of mock ORDERS05 idocs and sends them to be zk-proofed
    lo_router->attach( iv_template = '/test/trigger_outbound' iv_handler_class = 'ZCL_PROUBC_OBTRIGTEST' ).

    ro_root_handler = lo_router.
  ENDMETHOD.


  METHOD read_mime.
    "TODO need this later for Swagger docs
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
ENDCLASS.
