CLASS zcl_prvd_nchain_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_prvd_api_helper zcl_prvd_vault_helper.

  PUBLIC SECTION.
    METHODS:
      "! Creates or retrieves an existing instance of the PRVD Nchain helper class
      constructor IMPORTING !io_prvd_api_helper    TYPE REF TO zcl_prvd_api_helper OPTIONAL
                            !io_prvd_vault_helper  TYPE REF TO zcl_prvd_vault_helper OPTIONAL
                            !iv_org_id             TYPE zprvdtenantid OPTIONAL
                            !iv_subject_account_id TYPE zprvdtenantid OPTIONAL
                            !iv_workgroup_id       TYPE zprvdtenantid OPTIONAL
                            !iv_bpitoken           TYPE zprvdrefreshtoken OPTIONAL ,
      "! Smart contract integration example with the Chainlink ETH/USD price feed on Polygon Mumbai testnet
      call_chainlink_pricefeed IMPORTING !iv_inputcurrency    TYPE string
                                         !iv_inputamount      TYPE  string
                                         !iv_outputcurrency   TYPE string
                               EXPORTING !es_contract_resp    TYPE zif_prvd_nchain=>ty_executecontract_resp
                                         !es_contract_summary TYPE zif_prvd_nchain=>ty_executecontract_summary
                                         !ev_outputamount     TYPE string,
      "! Generates the data structure needed to call a smart contract on an EVM network via PRVD Nchain
      smartcontract_factory IMPORTING !iv_smartcontractaddress TYPE zprvd_smartcontract_addr
                                      !iv_name                 TYPE string
                                      !iv_walletaddress        TYPE zcasesensitive_str
                                      !iv_nchain_networkid     TYPE zprvd_nchain_networkid
                                      !iv_contracttype         TYPE zcasesensitive_str OPTIONAL
                            EXPORTING !es_selectedcontract     TYPE zif_prvd_nchain=>ty_chainlinkpricefeed_req,
      "! Gets the current EVM wallet address being used (e.g 0x409148kldsjflakj...)
      get_wallet_address RETURNING VALUE(ev_wallet_address) TYPE zprvd_smartcontract_addr,
      "! Creates a generic account Nchain txn
      create_account_txn IMPORTING iv_network_id TYPE zprvd_nchain_networkid
                                   iv_to_addr    TYPE zprvd_smartcontract_addr
                                   iv_vaultid    TYPE zprvdvaultid
                                   iv_vaultkey   TYPE zprvdvaultid
                                   iv_value      TYPE i,
      "! Gets the PRVD Nchain API proxy
      get_nchain_client RETURNING VALUE(ro_nchain_client) TYPE REF TO zcl_prvd_nchain,
      approve_smart_contract IMPORTING iv_approval_data              TYPE string
                                       is_signature                  TYPE zif_prvd_vault=>ty_signature
                                       iv_signing_address            TYPE zprvd_smartcontract_addr
                                       iv_destination_smart_contract TYPE zprvd_smartcontract_addr,
      "! Retrieves list of available wallets
      get_wallets EXPORTING et_wallets TYPE zif_prvd_nchain=>ty_wallet_list,
      "! Retrieves lists of accounts available for signing
      get_accounts EXPORTING et_accounts TYPE zif_prvd_nchain=>ty_account_list,
      "! Executes a contract via PRVD Nchain
      execute_contract_by_wallet IMPORTING iv_contract_id               TYPE zcasesensitive_str
                                           iv_exec_contract_req         TYPE zif_prvd_nchain=>ty_executecontractrequest
                                 RETURNING VALUE(rs_exec_contract_resp) TYPE zif_prvd_nchain=>ty_executecontract_resp,
      execute_contract_by_account IMPORTING iv_contract_id               TYPE zcasesensitive_str
                                            iv_exec_contract_req         TYPE zif_prvd_nchain=>ty_executecontractreq_account
                                  RETURNING VALUE(rs_exec_contract_resp) TYPE zif_prvd_nchain=>ty_executecontract_resp,
      "! Sets up contract instance on Nchain
      create_contract IMPORTING iv_smartcontractaddr  TYPE zprvd_smartcontract_addr
                                is_contract           TYPE zif_prvd_nchain=>ty_create_contract_req
                      RETURNING VALUE(rv_contract_id) TYPE zcasesensitive_str,
      "! Retrieves the transaction details
      get_tx_details IMPORTING iv_ref_number        TYPE zcasesensitive_str
                     RETURNING VALUE(rs_tx_details) TYPE zif_prvd_nchain=>ty_basic_txn_details,
      get_contract_instance IMPORTING iv_network_id         TYPE zprvd_nchain_networkid
                                      iv_smartcontract_addr TYPE zprvd_smartcontract_addr
                            RETURNING VALUE(rv_contract_id) TYPE zcasesensitive_str,
      add_contract_to_nchain IMPORTING iv_network_id         TYPE zprvd_nchain_networkid
                                       iv_smartcontract_addr TYPE zprvd_smartcontract_addr
                                       iv_contract_name      TYPE zcasesensitive_str
                                       iv_contract_type      TYPE zcasesensitive_str
                                       iv_org_wallet_id      type zcasesensitive_str
                             RETURNING VALUE(rv_contract_id) TYPE zcasesensitive_str.
  PROTECTED SECTION.
    DATA: mv_tenant             TYPE zprvdtenantid,
          mv_org_id             TYPE zprvdtenantid,
          mv_subject_account_id TYPE zprvdtenantid,
          mv_workgroup_id       TYPE zprvdtenantid,
          mo_http_client        TYPE REF TO if_http_client,
          mo_nchain_api         TYPE REF TO zcl_prvd_nchain,
          mv_nchain_api_url     TYPE string,
          mo_prvd_api_helper    TYPE REF TO zcl_prvd_api_helper,
          mo_prvd_vault_helper  TYPE REF TO zcl_prvd_vault_helper,
          mv_prvd_token         TYPE zprvdrefreshtoken.
    METHODS: get_vault_helper RETURNING VALUE(ro_prvd_vault_helper) TYPE REF TO zcl_prvd_vault_helper.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_prvd_nchain_helper IMPLEMENTATION.


  METHOD constructor.
    DATA: lv_jwt    TYPE REF TO data,
          lv_status TYPE i.

    IF io_prvd_api_helper IS BOUND.
      mo_prvd_api_helper = io_prvd_api_helper.
    ELSE.
      mo_prvd_api_helper = NEW zcl_prvd_api_helper( iv_tenant          = iv_org_id
                                                      iv_subject_acct_id = iv_subject_account_id
                                                      iv_workgroup_id    = iv_workgroup_id ).
      mv_org_id = iv_org_id.
      mv_subject_account_id = iv_subject_account_id.
      mv_workgroup_id = mv_workgroup_id.
    ENDIF.

    IF !io_prvd_vault_helper  IS BOUND.
      mo_prvd_vault_helper = io_prvd_vault_helper.
    ELSE.
      mo_prvd_vault_helper = NEW zcl_prvd_vault_helper( io_prvd_api_helper = mo_prvd_api_helper ).
    ENDIF.

    mo_prvd_api_helper->call_ident_api(
        IMPORTING
          ev_authtoken   = lv_jwt
          status         = lv_status ).

    FIELD-SYMBOLS: <fs_authreq>  TYPE any,
                   <fs_authreq2> TYPE string.
    ASSIGN lv_jwt->* TO FIELD-SYMBOL(<ls_data>).
    IF sy-subrc <> 0.
    ENDIF.
    ASSIGN COMPONENT 'ACCESS_TOKEN' OF STRUCTURE <ls_data> TO <fs_authreq>.
    IF sy-subrc <> 0.
    ENDIF.
    ASSIGN <fs_authreq>->* TO <fs_authreq2>.
    IF sy-subrc <> 0.
    ENDIF.
    mv_prvd_token  = <fs_authreq2>.

    mv_nchain_api_url = 'https://nchain.provide.services'.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = mv_nchain_api_url
      IMPORTING
        client             = mo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      " error handling
    ENDIF.

    mo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.
    mo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client
                                               value = '100' ).

    mo_nchain_api = NEW zcl_prvd_nchain( ii_client   = mo_http_client
                                           iv_tenant   = mv_org_id
                                           iv_bpitoken = mv_prvd_token ).

  ENDMETHOD.


  METHOD call_chainlink_pricefeed.
    "create the wallet
    DATA: ls_pricefeedwallet            TYPE zif_prvd_nchain=>ty_createhdwalletrequest,
          lv_getwallet_str              TYPE string,
          lv_getwallet_data             TYPE REF TO data,
          lv_getwallet_responsecode     TYPE i,
          ls_wallet_created             TYPE zif_prvd_nchain=>ty_hdwalletcreate_resp,
          ls_selectedcontract           TYPE zif_prvd_nchain=>ty_chainlinkpricefeed_req,
          lv_createdcontract_str        TYPE string,
          lv_createdcontract_data       TYPE REF TO data,
          lv_createdcontract_responsecd TYPE i,
          ls_executecontract            TYPE zif_prvd_nchain=>ty_executecontractrequest,
          lv_executecontract_str        TYPE string,
          lv_executecontract_xstr       TYPE xstring,
          lv_executecontract_data       TYPE REF TO data,
          lv_executecontract_responsecd TYPE i,
          lv_network_contract_id        TYPE zprvd_smartcontract_addr,
          lv_prvd_stack_contract_id     TYPE zcasesensitive_str,
          ls_execute_contract_resp      TYPE zif_prvd_nchain=>ty_executecontract_resp,
          ls_execute_contract_summary   TYPE zif_prvd_nchain=>ty_executecontract_summary.

    ls_pricefeedwallet-purpose = 44.
    mo_nchain_api->zif_prvd_nchain~createhdwallet( EXPORTING is_walletrequest    = ls_pricefeedwallet
                                                     IMPORTING ev_apiresponsestr   = lv_getwallet_str
                                                               ev_apiresponse      = lv_getwallet_data
                                                               ev_httpresponsecode = lv_getwallet_responsecode ).
    CASE lv_getwallet_responsecode.
      WHEN 201.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_getwallet_str
                                    CHANGING data = ls_wallet_created ).
      WHEN OTHERS.
        "add error handling
    ENDCASE.
    "eth/usd pair -- see https://docs.chain.link/docs/consuming-data-feeds/
    "https://docs.chain.link/docs/data-feeds/price-feeds/addresses/?network=polygon#Mumbai%20Testnet
    smartcontract_factory( EXPORTING iv_smartcontractaddress = '0x0715A7794a1dc8e42615F059dD6e406A6594651A'
                                     iv_name                 = 'ETH/USD'
                                     iv_walletaddress        = ls_wallet_created-id
                                     iv_nchain_networkid     = '4251b6fd-c98d-4017-87a3-d691a77a52a7'
                                     iv_contracttype         = 'price-feed'
                           IMPORTING es_selectedcontract = ls_selectedcontract ).
    mo_nchain_api->zif_prvd_nchain~createpricefeedcontract(
      EXPORTING
        iv_smartcontractaddr = '0x0715A7794a1dc8e42615F059dD6e406A6594651A'
        is_pricefeedcontract = ls_selectedcontract
      IMPORTING
        ev_apiresponsestr    = lv_createdcontract_str
        ev_apiresponse       = lv_createdcontract_data
        ev_httpresponsecode  = lv_createdcontract_responsecd ).
    CASE lv_createdcontract_responsecd.
      WHEN 201.

        FIELD-SYMBOLS: <fs_prvd_stack_contractid>     TYPE any,
                       <fs_prvd_stack_contractid_str> TYPE string.

        IF lv_createdcontract_data IS NOT INITIAL.
          ASSIGN lv_createdcontract_data->* TO FIELD-SYMBOL(<ls_contractdata>).
          IF sy-subrc <> 0.
          ENDIF.
          ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_contractdata> TO <fs_prvd_stack_contractid>.
          IF sy-subrc <> 0.
          ENDIF.
          ASSIGN <fs_prvd_stack_contractid>->* TO <fs_prvd_stack_contractid_str>.
          IF sy-subrc <> 0.
          ENDIF.
          lv_prvd_stack_contract_id = <fs_prvd_stack_contractid_str>.
        ENDIF.
        ls_executecontract-method = 'latestRoundData'.
        ls_executecontract-value = 0.
        ls_executecontract-wallet_id = ls_wallet_created-id.
      WHEN 404.
        "contract not found - might not be deployed
      WHEN OTHERS.
    ENDCASE.
*
    mo_nchain_api->zif_prvd_nchain~executecontract_by_wallet(
      EXPORTING
        iv_contract_id      = lv_prvd_stack_contract_id
        is_execcontractreq  = ls_executecontract
      IMPORTING
        ev_apiresponsestr   = lv_executecontract_str
        ev_apiresponsexstr  = lv_executecontract_xstr
        ev_apiresponse      =  lv_executecontract_data
        ev_httpresponsecode =  lv_executecontract_responsecd ).
    CASE lv_executecontract_responsecd.
      WHEN 200.
        ls_execute_contract_summary-nchain_network_id = '4251b6fd-c98d-4017-87a3-d691a77a52a7'.
        ls_execute_contract_summary-prvd_stack_contractid = lv_prvd_stack_contract_id.
        ls_execute_contract_summary-smartcontract_addr = '0x0715A7794a1dc8e42615F059dD6e406A6594651A'.
        ls_execute_contract_summary-walletid = ls_wallet_created-id.
        "TODO - losing response values when deserializing. Round IDs surpass p8 type
        /ui2/cl_json=>deserialize( EXPORTING jsonx = lv_executecontract_xstr CHANGING data = ls_execute_contract_resp  ).
        ASSIGN lv_executecontract_data->* TO FIELD-SYMBOL(<ls_contractoutputs>).
        IF sy-subrc <> 0.
        ENDIF.
        ASSIGN COMPONENT 'RESPONSE' OF STRUCTURE <ls_contractoutputs> TO FIELD-SYMBOL(<fs_executecontract_resp>).
        IF sy-subrc <> 0.
        ENDIF.
        es_contract_resp = ls_execute_contract_resp.
        es_contract_summary = ls_execute_contract_summary.
        "PRVD Nchain response may look like this:
        "{
*    "confidence": null,
*    "ref": "e71f3955-77e7-4a39-8abd-ee129c9f28b1",
*    "response": [
*        18446744073709652396,
*        155343681484,
*        1666975920,
*        1666975920,
*        18446744073709652396
*    ]
*}
* data values in response align in sequest to the returns in latest round outputs
        "latestRoundData outputs - see https://mumbai.polygonscan.com/address/0x0715A7794a1dc8e42615F059dD6e406A6594651A#code
*       function latestRoundData()
*    public
*    view
*    virtual
*    override
*    returns (
*      uint80 roundId,
*      int256 answer, <-- this is your price, still needs some unit conversions for SAP
*      uint256 startedAt,
*      uint256 updatedAt,
*      uint80 answeredInRound
*    ) "log the other data - important reference data point for checking later if oracle behaved as expected
      WHEN OTHERS.
    ENDCASE.
*    CATCH cx_static_check.
  ENDMETHOD.


  METHOD smartcontract_factory.
    DATA: ls_contract TYPE zif_prvd_nchain=>ty_chainlinkpricefeed_req.
    ls_contract-address = iv_smartcontractaddress.
    ls_contract-name = iv_name.
    ls_contract-network_id = iv_nchain_networkid.
    ls_contract-params-wallet_id = iv_walletaddress.
    ls_contract-params-compiled_artifact-name = 'EACAggregatorProxy'.
    zcl_prvd_file_helper=>get_smartcontract_abi( EXPORTING iv_nchain_networkid      = iv_nchain_networkid
                                                             iv_smartcontract_address = iv_smartcontractaddress
                                                   IMPORTING ev_abi_data              = ls_contract-params-compiled_artifact-abi ).
    ls_contract-type = iv_contracttype.
    es_selectedcontract = ls_contract.

  ENDMETHOD.


  METHOD get_vault_helper.
    IF mo_prvd_vault_helper IS NOT BOUND.
      mo_prvd_vault_helper = NEW zcl_prvd_vault_helper(  ).
    ENDIF.
    ro_prvd_vault_helper = mo_prvd_vault_helper.
  ENDMETHOD.


  METHOD get_wallet_address.
    get_vault_helper( ).
    ev_wallet_address = ''.
  ENDMETHOD.


  METHOD get_nchain_client.
    ro_nchain_client = mo_nchain_api.
  ENDMETHOD.

  METHOD create_account_txn.
    DATA: ls_nchain_txn      TYPE zif_prvd_nchain=>ty_create_broadcast_txn_ac,
          lv_apiresponsestr  TYPE string,
          lv_apiresponsedata TYPE REF TO data,
          lv_apiresponsecd   TYPE i.


    ls_nchain_txn-network_id = iv_network_id.
    ls_nchain_txn-key_id = iv_vaultkey.
    ls_nchain_txn-user_id = iv_vaultid.
    ls_nchain_txn-to = iv_to_addr.
    ls_nchain_txn-value = iv_value.


    mo_nchain_api->zif_prvd_nchain~create_broadcast_txn_ac(
        EXPORTING is_nchain_txn       = ls_nchain_txn
        IMPORTING ev_apiresponsestr   = lv_apiresponsestr
                  ev_apiresponse      = lv_apiresponsedata
                  ev_httpresponsecode = lv_apiresponsecd ).
*          CATCH cx_static_check.
  ENDMETHOD.

  METHOD approve_smart_contract.
    DATA: ls_approve_smartcontract TYPE zif_prvd_nchain=>ty_contract_approval,
          lv_apiresponsestr        TYPE string,
          lv_apiresponsedata       TYPE REF TO data,
          lv_apiresponsecd         TYPE i.

    mo_nchain_api->zif_prvd_nchain~approve_smart_contract(
          EXPORTING is_contract_approval = ls_approve_smartcontract
          IMPORTING ev_apiresponsestr   = lv_apiresponsestr
                    ev_apiresponse      = lv_apiresponsedata
                    ev_httpresponsecode = lv_apiresponsecd  ).
    CASE lv_apiresponsecd.
      WHEN 201.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD get_wallets.
    DATA: lv_apiresponsestr  TYPE string,
          lv_apiresponsedata TYPE REF TO data,
          lv_apiresponsecd   TYPE i.

    mo_nchain_api->zif_prvd_nchain~listhdwallets(
      IMPORTING
        ev_apiresponsestr   = lv_apiresponsestr
        ev_apiresponse      = lv_apiresponsedata
        ev_httpresponsecode = lv_apiresponsecd ).
    CASE lv_apiresponsecd.
      WHEN 200.
        /ui2/cl_json=>deserialize(
    EXPORTING
      json             = lv_apiresponsestr
    CHANGING
      data             = et_wallets ).
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD get_accounts.
    DATA: lv_apiresponsestr  TYPE string,
          lv_apiresponsedata TYPE REF TO data,
          lv_apiresponsecd   TYPE i.

    mo_nchain_api->zif_prvd_nchain~listaccounts(
      IMPORTING
        ev_apiresponsestr   = lv_apiresponsestr
        ev_apiresponse      = lv_apiresponsedata
        ev_httpresponsecode = lv_apiresponsecd ).
    CASE lv_apiresponsecd.
      WHEN 200.
        /ui2/cl_json=>deserialize(
            EXPORTING
              json             = lv_apiresponsestr
            CHANGING
              data             = et_accounts ).
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD execute_contract_by_wallet.
    DATA: lv_apiresponsestr  TYPE string,
          lv_apiresponsedata TYPE REF TO data,
          lv_apiresponsecd   TYPE i.

    mo_nchain_api->zif_prvd_nchain~executecontract_by_wallet(
      EXPORTING iv_contract_id      = iv_contract_id
                is_execcontractreq = iv_exec_contract_req
      IMPORTING ev_apiresponsestr   = lv_apiresponsestr
                ev_apiresponse      = lv_apiresponsedata
                ev_httpresponsecode = lv_apiresponsecd ).
    CASE lv_apiresponsecd.
      WHEN 202.
        /ui2/cl_json=>deserialize(
            EXPORTING
              json             = lv_apiresponsestr
            CHANGING
              data             = rs_exec_contract_resp ).
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD execute_contract_by_account.
    DATA: lv_apiresponsestr  TYPE string,
          lv_apiresponsedata TYPE REF TO data,
          lv_apiresponsecd   TYPE i.

    mo_nchain_api->zif_prvd_nchain~executecontract_by_account(
      EXPORTING iv_contract_id      = iv_contract_id
                is_execcontractreq = iv_exec_contract_req
      IMPORTING ev_apiresponsestr   = lv_apiresponsestr
                ev_apiresponse      = lv_apiresponsedata
                ev_httpresponsecode = lv_apiresponsecd ).
    CASE lv_apiresponsecd.
      WHEN 202.
        /ui2/cl_json=>deserialize(
            EXPORTING
              json             = lv_apiresponsestr
            CHANGING
              data             = rs_exec_contract_resp ).
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD create_contract.
    DATA: lv_createdcontract_str        TYPE string,
          lv_createdcontract_data       TYPE REF TO data,
          lv_createdcontract_responsecd TYPE i.

    mo_nchain_api->zif_prvd_nchain~create_contract(
        EXPORTING
          iv_smartcontractaddr = iv_smartcontractaddr
          is_contract = is_contract
        IMPORTING
          ev_apiresponsestr    = lv_createdcontract_str
          ev_apiresponse       = lv_createdcontract_data
          ev_httpresponsecode  = lv_createdcontract_responsecd ).
    CASE lv_createdcontract_responsecd.
      WHEN 201.

        FIELD-SYMBOLS: <fs_prvd_stack_contractid>     TYPE any,
                       <fs_prvd_stack_contractid_str> TYPE string.

        IF lv_createdcontract_data IS NOT INITIAL.
          ASSIGN lv_createdcontract_data->* TO FIELD-SYMBOL(<ls_contractdata>).
          IF sy-subrc <> 0.
          ENDIF.
          ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_contractdata> TO <fs_prvd_stack_contractid>.
          IF sy-subrc <> 0.
          ENDIF.
          ASSIGN <fs_prvd_stack_contractid>->* TO <fs_prvd_stack_contractid_str>.
          IF sy-subrc <> 0.
          ENDIF.
          rv_contract_id = <fs_prvd_stack_contractid_str>.
        ENDIF.
      WHEN 404.
        "contract not found - might not be deployed
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD get_tx_details.
    DATA: lv_apiresponsestr  TYPE string,
          lv_apiresponsedata TYPE REF TO data,
          lv_apiresponsecd   TYPE i.
    mo_nchain_api->zif_prvd_nchain~gettransactiondetails(
      EXPORTING
        iv_transaction_id   = iv_ref_number
      IMPORTING
        ev_apiresponsestr   = lv_apiresponsestr
        ev_apiresponse      = lv_apiresponsedata
        ev_httpresponsecode = lv_apiresponsecd    ).
    CASE lv_apiresponsecd.
      WHEN 200.
        /ui2/cl_json=>deserialize(
          EXPORTING
            json             = lv_apiresponsestr
          CHANGING
            data             = rs_tx_details ).
      WHEN OTHERS.
    ENDCASE.
*    CATCH cx_static_check.
  ENDMETHOD.

  METHOD get_contract_instance.
    DATA: lv_apiresponsestr  TYPE string,
          lv_apiresponsedata TYPE REF TO data,
          lv_apiresponsecd   TYPE i,
          lt_contractslist   TYPE zif_prvd_nchain=>ty_contract_list,
          ls_contract        TYPE zif_prvd_nchain=>ty_deployed_contract.

    mo_nchain_api->zif_prvd_nchain~listcontracts(
      IMPORTING
        ev_apiresponsestr   = lv_apiresponsestr
        ev_apiresponse      = lv_apiresponsedata
        ev_httpresponsecode = lv_apiresponsecd ).

    CASE lv_apiresponsecd.
      WHEN 200.
        /ui2/cl_json=>deserialize(
         EXPORTING
           json             = lv_apiresponsestr
         CHANGING
           data             = lt_contractslist ).
        READ TABLE lt_contractslist WITH KEY network_id = iv_network_id  address = iv_smartcontract_addr
            INTO ls_contract.
        IF sy-subrc = 0.
          rv_contract_id = ls_contract-id.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

*    CATCH cx_static_check.
  ENDMETHOD.

  METHOD add_contract_to_nchain.
    DATA: lv_apiresponsestr  TYPE string,
          lv_apiresponsedata TYPE REF TO data,
          lv_apiresponsecd   TYPE i,
          ls_vault               TYPE zif_prvd_vault=>ty_vault_query,
          ls_wallet_key          TYPE zif_prvd_vault=>ty_vault_keys,
          ls_selectedcontract    TYPE zif_prvd_nchain=>ty_chainlinkpricefeed_req.


    smartcontract_factory( EXPORTING iv_smartcontractaddress = iv_smartcontract_addr
                           iv_name                 = iv_contract_name
                           iv_walletaddress        = iv_org_wallet_id
                           iv_nchain_networkid     = iv_network_id
                           iv_contracttype         = iv_contract_type
                 IMPORTING es_selectedcontract = ls_selectedcontract ).
    rv_contract_id = create_contract( EXPORTING iv_smartcontractaddr = iv_smartcontract_addr is_contract = ls_selectedcontract ).
  ENDMETHOD.
ENDCLASS.
