CLASS zcl_prvd_ipfs_simple DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_prvd_ipfs_simple.
    "! Class instance constructor for the simple IPFS API proxy
    METHODS: constructor IMPORTING ii_client   TYPE REF TO if_http_client
                                   iv_ipfs_url TYPE string.

  PROTECTED SECTION.
    DATA: mi_client   TYPE REF TO if_http_client,
          mv_ipfs_url TYPE string.
    METHODS: send_receive RETURNING VALUE(rv_code) TYPE i.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_prvd_ipfs_simple IMPLEMENTATION.

  METHOD constructor.
    mi_client = ii_client.
    mv_ipfs_url = iv_ipfs_url.
  ENDMETHOD.

  METHOD send_receive.
    mi_client->send( ).
    mi_client->receive( ).
    mi_client->response->get_status( IMPORTING code = rv_code ).
  ENDMETHOD.


  METHOD zif_prvd_ipfs_simple~add.
    DATA lv_code TYPE i.
    DATA lv_uri TYPE string VALUE '/api/v0/add'.
    DATA lv_uri_w_projid TYPE string.
    DATA lv_creds TYPE string.
    DATA lv_creds_base64 TYPE string.
    DATA lv_creds_header TYPE string.
    DATA(lv_binary_header) = mi_client->request->add_multipart( ).
    DATA lv_content_dispo TYPE string.
    DATA lv_content_dispo_alt TYPE string.
    DATA lv_content_base64 TYPE string.
    lv_content_dispo = 'form-data;name="{file}"; filename="{file}"; file="{file}"'.
    lv_content_dispo_alt = 'form-data;path="{content}"'.

    REPLACE ALL OCCURRENCES OF '{file}' IN lv_content_dispo WITH iv_filename.
    REPLACE ALL OCCURRENCES OF '{filetype}' IN lv_content_dispo WITH iv_filetype.

    mi_client->request->set_method( 'POST' ).

    mi_client->propertytype_logon_popup = mi_client->co_disabled.
    mi_client->authenticate( username = iv_ipfsprojid
                             password = iv_ipfsapikey ).

    CONCATENATE iv_ipfsprojid ':' iv_ipfsapikey INTO lv_creds.

    cl_http_utility=>if_http_utility~encode_base64( EXPORTING unencoded = lv_creds
                                                    RECEIVING encoded   = lv_creds_base64.

    CONCATENATE 'Basic' lv_creds_base64 INTO lv_creds_header SEPARATED BY space.
    mi_client->request->set_header_field( name  = 'Authorization'
                                          value = lv_creds_header ).

    mi_client->request->set_header_field( name = 'Content-Type'
                                         value = 'multipart/form-data' ).

    mi_client->request->set_header_field( name = 'Accept' 
                                         value = '*/*' ).
    mi_client->request->set_header_field( name = 'Accept-Encoding'
                                         value = 'gzip, deflate, br').
    mi_client->request->set_header_field( name = 'Connection'
                                         value = 'keep-alive').
    mi_client->request->if_http_entity~set_formfield_encoding(
        formfield_encoding = cl_http_request=>if_http_entity~co_encoding_raw ).

    DATA(lv_content_string) = cl_bcs_convert=>xstring_to_string( iv_xstr = iv_binarystring
                                                                 iv_cp   = 1100 ).

    lv_content_base64 = cl_http_utility=>if_http_utility~encode_base64( unencoded = iv_contentstring ).

    REPLACE ALL OCCURRENCES OF '{content}' IN lv_content_dispo WITH lv_content_string.
    REPLACE ALL OCCURRENCES OF '{content}' IN lv_content_dispo_alt WITH lv_content_base64.

    lv_binary_header->set_header_field( name  = 'content-disposition'
                                        value = lv_content_dispo_alt ).

    lv_binary_header->set_content_type( 'application/json' ).

    CONCATENATE lv_uri '?project_id=' iv_ipfsprojid INTO lv_uri_w_projid.
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    mi_client->request->set_form_field( name = 'project_id'
                                       value = iv_ipfsprojid ).

    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json            = ev_apiresponsestr
      CHANGING
        data            = ev_apiresponse ).

  ENDMETHOD.


ENDCLASS.
