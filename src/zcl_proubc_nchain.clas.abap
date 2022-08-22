CLASS zcl_proubc_nchain DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_proubc_nchain.
    METHODS constructor IMPORTING ii_client TYPE REF TO if_http_client.
  PROTECTED SECTION.
    DATA mi_client TYPE REF TO if_http_client.
    METHODS send_receive RETURNING VALUE(rv_code) TYPE i.
ENDCLASS.

CLASS zcl_proubc_nchain IMPLEMENTATION.
  METHOD constructor.
    mi_client = ii_client.
  ENDMETHOD.

  METHOD send_receive.
    mi_client->send( ).
    mi_client->receive( ).
    mi_client->response->get_status( IMPORTING code = rv_code ).
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~createhdwallet.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/wallets'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
* todo, set body, #/components/schemas/CreateHDwalletRequest
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_proubc_nchain~executecontract.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/contracts/{contract_id}/execute'.
    lv_temp = contract_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{contract_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri' value = lv_uri ).
* todo, set body, #/components/schemas/ExecutecontractRequest
    lv_code = send_receive( ).
    WRITE / lv_code.
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
    lv_code = send_receive( ).
    WRITE / lv_code.
    CASE lv_code.
      WHEN 200.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

