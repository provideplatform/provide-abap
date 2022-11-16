CLASS zcl_prvd_ipfs_simple DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_prvd_ipfs_simple.
    METHODS: constructor IMPORTING ii_client   TYPE REF TO if_http_client
                                   iv_ipfs_url TYPE string.

  PROTECTED SECTION.
    DATA: mi_client   TYPE REF TO if_http_client,
          lv_ipfs_url TYPE string.
    METHODS: send_receive RETURNING VALUE(rv_code) TYPE i.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_prvd_ipfs_simple IMPLEMENTATION.

  METHOD constructor.
    mi_client = ii_client.
    lv_ipfs_url = iv_ipfs_url.
  ENDMETHOD.

  METHOD send_receive.
    mi_client->send( ).
    mi_client->receive( ).
    mi_client->response->get_status( IMPORTING code = rv_code ).
  ENDMETHOD.


  METHOD zif_prvd_ipfs_simple~add.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v0/add'.
    DATA lv_uri_w_projid TYPE string.
    DATA lv_creds TYPE string.
    DATA lv_creds_base64 TYPE string.
    DATA lv_creds_header TYPE string.
    DATA(lv_binary_header) = mi_client->request->add_multipart( ).
    DATA lv_content_dispo TYPE string.
    lv_content_dispo = 'form-data;name="{file}"; filename="{file}";'.

    REPLACE ALL OCCURRENCES OF '{file}' IN lv_content_dispo WITH iv_filename.
    REPLACE ALL OCCURRENCES OF '{filetype}' IN lv_content_dispo WITH iv_filetype.

    mi_client->propertytype_logon_popup = mi_client->co_disabled.
    CALL METHOD mi_client->authenticate
      EXPORTING
        username = iv_ipfsprojid
        password = iv_ipfsapikey.

    CONCATENATE iv_ipfsprojid ':' iv_ipfsapikey INTO lv_creds.

    CALL METHOD cl_http_utility=>if_http_utility~encode_base64
      EXPORTING
        unencoded = lv_creds
      RECEIVING
        encoded   = lv_creds_base64.

    CONCATENATE 'Basic' lv_creds_base64 INTO lv_creds_header SEPARATED BY space.
    mi_client->request->set_header_field(
      EXPORTING
        name  = 'Authorization'    " Name of the header field
        value = lv_creds_header    " HTTP header field value
    ).

    CALL METHOD lv_binary_header->set_header_field
      EXPORTING
        name  = 'content-disposition'
        value = lv_content_dispo.

    CALL METHOD lv_binary_header->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'multipart/form-data'.

    cl_bcs_convert=>xstring_to_string(
        EXPORTING
          iv_xstr   = iv_binarystring
          iv_cp     =  1100                " SAP character set identification
        RECEIVING
          rv_string = DATA(lv_content_string)
    ).


    CALL METHOD lv_binary_header->set_form_field
      EXPORTING
        name  = 'path'
        value = lv_content_string.

*    DATA(len) = xstrlen( iv_binarystring  ).
*
*    CALL METHOD lv_binary_header->set_data
*      EXPORTING
*        data   = iv_binarystring
*        offset = 0
*        length = len.

    mi_client->request->set_method( 'POST' ).
    CONCATENATE lv_uri '?project_id=' iv_ipfsprojid INTO lv_uri_w_projid.
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri_w_projid ).

    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse
    ).

  ENDMETHOD.


ENDCLASS.
