CLASS zcl_proubc_nchain DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_proubc_nchain.
    METHODS constructor IMPORTING !ii_client   TYPE REF TO if_http_client
                                  !iv_tenant   TYPE zprvdtenantid
                                  !iv_bpitoken TYPE zprvdrefreshtoken.
  PROTECTED SECTION.
    DATA: mi_client   TYPE REF TO if_http_client,
          lv_bpitoken TYPE zprvdrefreshtoken,
          lv_tenantid TYPE zcasesensitive_str.
    METHODS send_receive RETURNING VALUE(rv_code) TYPE i.
  PRIVATE SECTION.
    METHODS sap_auth_check.
    METHODS get_bpi_token
      RAISING cx_static_check.
ENDCLASS.

CLASS zcl_proubc_nchain IMPLEMENTATION.
  METHOD constructor.
    mi_client = ii_client.
    lv_bpitoken = iv_bpitoken.
    lv_tenantid = iv_tenant.
  ENDMETHOD.

  METHOD send_receive.
    mi_client->send( ).
    mi_client->receive( ).
    mi_client->response->get_status( IMPORTING code = rv_code ).
  ENDMETHOD.

  METHOD get_bpi_token.
    DATA lv_bearertoken TYPE string.
    CONCATENATE 'Bearer' lv_bpitoken INTO lv_bearertoken SEPARATED BY space.
    mi_client->request->set_header_field(
      EXPORTING
        name  = 'Authorization'    " Name of the header field
        value = lv_bearertoken    " HTTP header field value
    ).
  ENDMETHOD.


  METHOD sap_auth_check.
    "TODO create authorization field in su20. need to check default character length for tenant id. SAP auth check limits to 40 chars.
  ENDMETHOD.


  METHOD zif_proubc_nchain~listconnectors.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/connectors'.
    lv_temp = public.
    CONDENSE lv_temp.
    mi_client->request->set_form_field( name = 'public' value = lv_temp ).
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~createconnector.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/connectors'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
* todo, set body, #/components/schemas/CreateconnectorRequest
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~getconnectordetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/connectors/{connector_id}'.
    lv_temp = connector_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{connector_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~deleteconnector.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/connectors/{connector_id}'.
    lv_temp = connector_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{connector_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~updatenetwork.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/connectors/{connector_id}'.
    lv_temp = connector_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{connector_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~getloadbalancerdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/connectors/{connector_id}/load_balancers'.
    lv_temp = connector_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{connector_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~listnetworks.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/networks/{network_id}'.
    lv_temp = network_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{network_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~getnetworkdetail.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/networks/{connector_id}'.
    lv_temp = connector_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{connector_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~getnetworkstatus.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/networks/{network_id}/status'.
    lv_temp = network_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{network_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~createnetwork.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/networks'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
* todo, set body, #/components/schemas/CreatenetworkRequest
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~createaccounts.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/accounts'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
* todo, set body, #/components/schemas/CreateaccountsRequest
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~listaccounts.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/accounts'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~getaccountdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/accounts/{account_id}'.
    lv_temp = account_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{account_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~listhdwallets.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/wallets'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~createhdwallet.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/wallets'.
    DATA lv_requestdata TYPE REF TO data.
    DATA lv_requeststr TYPE string.

    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).

    zcl_proubc_api_helper=>copy_data_to_ref( EXPORTING is_data = is_walletrequest
                  CHANGING cr_data = lv_requestdata  ).

    lv_requeststr = /ui2/cl_json=>serialize( EXPORTING data = lv_requestdata
                                       pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata(
      EXPORTING
        data   =  lv_requeststr
    ).

    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~listhdwalletaccounts.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/wallets/{wallet_id}/accounts'.
    lv_temp = wallet_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{wallet_id}' IN lv_uri WITH lv_temp.
    lv_temp = page.
    CONDENSE lv_temp.
    mi_client->request->set_form_field( name = 'page' value = lv_temp ).
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~listtransactions.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/transactions'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~create_broadcast_txn_ac.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/transactions'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~gettransactiondetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/transactions/{transaction_id}'.
    lv_temp = transaction_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{transaction_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~listcontracts.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/contracts'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~deploycontract.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/contracts'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
* todo, set body, #/components/schemas/DeploycontractRequest
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~getcontractdetail.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/contracts/{contract_id}'.
    lv_temp = contract_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{contract_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
    mi_client->request->set_header_field( name = 'content-type' value = content_type ).
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~executecontract.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/contracts/{contract_id}/execute'.
    DATA lv_requeststr TYPE string.
    DATA lv_requestdata TYPE REF TO data.
    lv_temp = iv_contract_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{contract_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).

    zcl_proubc_api_helper=>copy_data_to_ref( EXPORTING is_data = is_execcontractreq
                  CHANGING cr_data = lv_requestdata  ).

    lv_requeststr = /ui2/cl_json=>serialize( EXPORTING data = lv_requestdata
                                       pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata(
      EXPORTING
        data   =  lv_requeststr
    ).

    me->get_bpi_token( ).
    lv_code = send_receive( ).
    ev_httpresponsecode = lv_code.
    ev_apiresponsestr = mi_client->response->get_cdata( ).
    DATA: lv_response_xstring TYPE xstring.
    lv_response_xstring = mi_client->response->get_data(
*      EXPORTING
*        offset             = 0
*        length             = -1
*        virus_scan_profile = '/SIHTTP/HTTP_UPLOAD'
*        vscan_scan_always  = if_http_entity=>co_content_check_profile
*      RECEIVING
*        data               =
    ).
    ev_apiresponsexstr = lv_response_xstring.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = ev_apiresponsestr
      CHANGING
        data             = ev_apiresponse
    ).
    "WRITE / lv_code. ~replace with logging call
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~executereadonlycontract.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/contracts/{contract_id}/execute'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
* todo, set body, #/components/schemas/ExecutereadonlycontractRequest
    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~createpricefeedcontract.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/contracts'.
    DATA lv_requeststr TYPE string.
    DATA lv_requestdata TYPE REF TO data.
    lv_temp = iv_smartcontractaddr.
    REPLACE ALL OCCURRENCES OF '{contract_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).

    zcl_proubc_api_helper=>copy_data_to_ref( EXPORTING is_data = is_pricefeedcontract
                  CHANGING cr_data = lv_requestdata  ).

    lv_requeststr = /ui2/cl_json=>serialize( EXPORTING data = lv_requestdata
                                       pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata(
      EXPORTING
        data   =  lv_requeststr
    ).

    me->get_bpi_token( ).
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
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

