CLASS zcl_prvd_nchain DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_prvd_nchain.
    "! Constructor method to creates the PRVD Nchain API Proxy class
    METHODS constructor IMPORTING !ii_client   TYPE REF TO if_http_client
                                  !iv_tenant   TYPE zprvdtenantid
                                  !iv_bpitoken TYPE zprvdrefreshtoken.
  PROTECTED SECTION.
    DATA: mi_client   TYPE REF TO if_http_client,
          mv_bpitoken TYPE zprvdrefreshtoken,
          mv_tenantid TYPE zcasesensitive_str.
    METHODS send_receive RETURNING VALUE(rv_code) TYPE i.
  PRIVATE SECTION.
    METHODS sap_auth_check.
    METHODS get_bpi_token
      RAISING cx_static_check.
ENDCLASS.

CLASS zcl_prvd_nchain IMPLEMENTATION.
  METHOD constructor.
    mi_client = ii_client.
    mv_bpitoken = iv_bpitoken.
    mv_tenantid = iv_tenant.
  ENDMETHOD.

  METHOD send_receive.
    mi_client->send( ).
    mi_client->receive( ).
    mi_client->response->get_status( IMPORTING code = rv_code ).
  ENDMETHOD.

  METHOD get_bpi_token.
    DATA lv_bearertoken TYPE string.
    CONCATENATE 'Bearer' mv_bpitoken INTO lv_bearertoken SEPARATED BY space.
    mi_client->request->set_header_field(
        name  = 'Authorization'
        value = lv_bearertoken ).
  ENDMETHOD.


  METHOD sap_auth_check.
    "TODO create authorization field in su20. need to check default character length for tenant id. SAP auth check limits to 40 chars.
  ENDMETHOD.


  METHOD zif_prvd_nchain~listconnectors.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/connectors'.
    lv_temp = iv_public.
    CONDENSE lv_temp.
    mi_client->request->set_form_field( name  = 'public'
                                        value = lv_temp ).
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type'
                                          value = iv_content_type ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~createconnector.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/connectors'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
* todo, set body, #/components/schemas/CreateconnectorRequest
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
       "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~getconnectordetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/connectors/{connector_id}'.
    lv_temp = iv_connector_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{connector_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~deleteconnector.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/connectors/{connector_id}'.
    lv_temp = iv_connector_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{connector_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                          value = lv_uri ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~updatenetwork.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/connectors/{connector_id}'.
    lv_temp = iv_connector_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{connector_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~getloadbalancerdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/connectors/{connector_id}/load_balancers'.
    lv_temp = iv_connector_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{connector_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json            = ev_apiresponsestr
      CHANGING
        data            = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~listnetworks.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/networks/{network_id}'.
    lv_temp = iv_network_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{network_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~getnetworkdetail.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/networks/{connector_id}'.
    lv_temp = iv_connector_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{connector_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~getnetworkstatus.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/networks/{network_id}/status'.
    lv_temp = iv_network_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{network_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~createnetwork.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/networks'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).

* todo, set body, #/components/schemas/CreatenetworkRequest
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~createaccounts.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/accounts'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
* todo, set body, #/components/schemas/CreateaccountsRequest
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~listaccounts.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/accounts'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type'
                                          value = iv_content_type ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~getaccountdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/accounts/{account_id}'.
    lv_temp = iv_account_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{account_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type'
                                          value = iv_content_type ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~listhdwallets.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/wallets'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type'
                                          value = iv_content_type ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~createhdwallet.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/wallets'.
    DATA lv_requestdata TYPE REF TO data.
    DATA lv_requeststr TYPE string.

    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).

    zcl_prvd_api_helper=>copy_data_to_ref( EXPORTING is_data = is_walletrequest
                                             CHANGING cr_data  = lv_requestdata  ).

    lv_requeststr = /ui2/cl_json=>serialize( data        = lv_requestdata
                                             pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata( data = lv_requeststr ).

    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse
    ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~listhdwalletaccounts.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/wallets/{wallet_id}/accounts'.
    lv_temp = iv_wallet_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{wallet_id}' IN lv_uri WITH lv_temp.
    lv_temp = iv_page.
    CONDENSE lv_temp.
    mi_client->request->set_form_field( name  = 'page'
                                        value = lv_temp ).
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type'
                                          value = iv_content_type ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~listtransactions.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/transactions'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type'
                                          value = iv_content_type ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~create_broadcast_txn_ac.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/transactions'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~gettransactiondetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/transactions/{transaction_id}'.
    lv_temp = iv_transaction_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{transaction_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type'
                                          value = iv_content_type ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~listcontracts.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/contracts'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type'
                                          value = iv_content_type ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~deploycontract.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/contracts'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
* todo, set body, #/components/schemas/DeploycontractRequest
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~getcontractdetail.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/contracts/{contract_id}'.
    lv_temp = iv_contract_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{contract_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    mi_client->request->set_header_field( name  = 'content-type'
                                          value = iv_content_type ).
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~executecontract.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/contracts/{contract_id}/execute'.
    DATA lv_requeststr TYPE string.
    DATA lv_requestdata TYPE REF TO data.
    DATA: lv_response_xstring TYPE xstring.
    lv_temp = iv_contract_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{contract_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).

    zcl_prvd_api_helper=>copy_data_to_ref( EXPORTING is_data = is_execcontractreq
                                              CHANGING cr_data = lv_requestdata ).

    lv_requeststr = /ui2/cl_json=>serialize( data        = lv_requestdata
                                             pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata( data = lv_requeststr ).

    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    lv_response_xstring = mi_client->response->get_data( ).
    ev_apiresponsexstr = lv_response_xstring.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~executereadonlycontract.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/contracts/{contract_id}/execute'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
* todo, set body, #/components/schemas/ExecutereadonlycontractRequest
    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_nchain~createpricefeedcontract.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/contracts'.
    DATA lv_requeststr TYPE string.
    DATA lv_requestdata TYPE REF TO data.
    lv_temp = iv_smartcontractaddr.
    REPLACE ALL OCCURRENCES OF '{contract_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).

    zcl_prvd_api_helper=>copy_data_to_ref( EXPORTING is_data = is_pricefeedcontract
                                             CHANGING cr_data  = lv_requestdata  ).

    lv_requeststr = /ui2/cl_json=>serialize( data        = lv_requestdata
                                             pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata( data = lv_requeststr ).

    get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
      "Success
      WHEN OTHERS.
      "message error calling &1-method &2-lv_uri. HTTP response &3-lv_code
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

