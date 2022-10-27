CLASS zcl_proubc_nchain_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_proubc_api_helper zcl_proubc_vault_helper.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING !io_prvd_api_helper    TYPE REF TO zcl_proubc_api_helper OPTIONAL
                            !io_prvd_vault_helper  TYPE REF TO zcl_proubc_vault_helper OPTIONAL
                            !iv_org_id             TYPE zprvdtenantid OPTIONAL
                            !iv_subject_account_id TYPE zprvdtenantid OPTIONAL
                            !iv_workgroup_id       TYPE zprvdtenantid OPTIONAL
                            !iv_bpitoken           TYPE zprvdrefreshtoken OPTIONAL ,
      call_chainlink_pricefeed IMPORTING !iv_inputcurrency  TYPE string
                                         !iv_inputamount    TYPE  string
                                         !iv_outputcurrency TYPE string
                               EXPORTING !ev_outputamount   TYPE string,
      smartcontract_factory IMPORTING !iv_smartcontractaddress TYPE zproubc_smartcontract_addr
                                      !iv_name                 TYPE string
                                      !iv_contract             TYPE zcasesensitive_str
                                      !iv_walletaddress        TYPE zcasesensitive_str
                                      !iv_nchain_networkid     TYPE zprvd_nchain_networkid
                                      !iv_contracttype         TYPE zcasesensitive_str OPTIONAL
                            EXPORTING !es_selectedcontract     TYPE zif_proubc_nchain=>ty_chainlinkpricefeed_req,
      get_wallet_address EXPORTING ev_wallet_address TYPE zproubc_smartcontract_addr.
  PROTECTED SECTION.
    DATA: lv_tenant             TYPE zprvdtenantid,
          lv_org_id             TYPE zprvdtenantid,
          lv_subject_account_id TYPE zprvdtenantid,
          lv_workgroup_id       TYPE zprvdtenantid,
          lo_http_client        TYPE REF TO if_http_client,
          lo_nchain_api         TYPE REF TO zcl_proubc_nchain,
          lv_nchain_api_url     TYPE string,
          lo_prvd_api_helper    TYPE REF TO zcl_proubc_api_helper,
          lo_prvd_vault_helper  TYPE REF TO zcl_proubc_vault_helper,
          lv_prvd_token         TYPE zprvdrefreshtoken.
    METHODS: get_vault_helper.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_proubc_nchain_helper IMPLEMENTATION.

  METHOD constructor.

    IF io_prvd_api_helper IS BOUND.
      lo_prvd_api_helper = io_prvd_api_helper.
    ELSE.
      lo_prvd_api_helper = NEW zcl_proubc_api_helper( iv_tenant = iv_org_id
                                                      iv_subject_acct_id = iv_subject_account_id
                                                      iv_workgroup_id = iv_workgroup_id ).
    ENDIF.

*    lv_subject_account_id
*    lv_workgroup_id

    IF !io_prvd_vault_helper  IS BOUND.
      lo_prvd_vault_helper = io_prvd_vault_helper.
    ELSE.
      lo_prvd_vault_helper = NEW zcl_proubc_vault_helper( io_api_helper = lo_prvd_api_helper ).
    ENDIF.

    DATA: lv_jwt    TYPE REF TO data,
          lv_status TYPE i.

    me->lo_prvd_api_helper->call_ident_api(
        IMPORTING
          ev_authtoken   = lv_jwt
          status         = lv_status
          "ev_bpiendpoint =
    ).

    FIELD-SYMBOLS: <fs_authreq>  TYPE any,
                   <fs_authreq2> TYPE string.
    ASSIGN lv_jwt->* TO FIELD-SYMBOL(<ls_data>). "dereference into field symbol
    ASSIGN COMPONENT 'ACCESS_TOKEN' OF STRUCTURE <ls_data> TO <fs_authreq>.
    ASSIGN <fs_authreq>->* TO <fs_authreq2>.
    lv_prvd_token  = <fs_authreq2>.

    lv_nchain_api_url = 'https://nchain.provide.services'.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = lv_nchain_api_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      " error handling
    ENDIF.

    lo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.
    lo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client value = '100' ).

    "to do fix params
    lo_nchain_api = NEW zcl_proubc_nchain( ii_client = lo_http_client iv_tenant = lv_org_id iv_bpitoken = lv_prvd_token  ).

  ENDMETHOD.

  METHOD call_chainlink_pricefeed.
    "create the wallet
    DATA: ls_pricefeedwallet            TYPE zif_proubc_nchain=>ty_createhdwalletrequest,
          lv_getwallet_str              TYPE string,
          lv_getwallet_data             TYPE REF TO data,
          lv_getwallet_responsecode     TYPE i,
          ls_wallet_created             TYPE zif_proubc_nchain=>ty_hdwalletcreate_resp,
          ls_selectedcontract           TYPE zif_proubc_nchain=>ty_chainlinkpricefeed_req,
          lv_createdcontract_str        TYPE string,
          lv_createdcontract_data       TYPE REF TO data,
          lv_createdcontract_responsecd TYPE i,
          ls_executecontract            TYPE zif_proubc_nchain=>ty_executecontractrequest,
          lv_executecontract_str        TYPE string,
          lv_executecontract_data       TYPE REF TO data,
          lv_executecontract_responsecd TYPE i.

    ls_pricefeedwallet-purpose = 44.
    me->lo_nchain_api->zif_proubc_nchain~createhdwallet( EXPORTING is_walletrequest = ls_pricefeedwallet
                                                         IMPORTING ev_apiresponsestr   = lv_getwallet_str
                                                                   ev_apiresponse       = lv_getwallet_data
                                                                   ev_httpresponsecode = lv_getwallet_responsecode ).
    CASE lv_getwallet_responsecode.
      WHEN 201.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_getwallet_str CHANGING data = ls_wallet_created ).
      WHEN OTHERS. "add error handling
    ENDCASE.
    "eth/usd pair -- see https://docs.chain.link/docs/consuming-data-feeds/
    "https://goerli.etherscan.io/address/{{iv_smartcontractaddress}}#code
    me->smartcontract_factory(  EXPORTING iv_smartcontractaddress = '0xD4a33860578De61DBAbDc8BFdb98FD742fA7028e'
                                          iv_name                 = 'ETH/USD'
                                          iv_contract             = '' "this is more complex
                                          iv_walletaddress        = ls_wallet_created-id  "from the wallet we created earlier
                                          iv_nchain_networkid     = '1b16996e-3595-4985-816c-043345d22f8c' "goerli testnet nchain id, check if this always same
                                          iv_contracttype         = 'price-feed'
                                IMPORTING es_selectedcontract = ls_selectedcontract ).
    me->lo_nchain_api->zif_proubc_nchain~createpricefeedcontract(
      EXPORTING
        is_pricefeedcontract = ls_selectedcontract
      IMPORTING
        ev_apiresponsestr    = lv_createdcontract_str
        ev_apiresponse       = lv_createdcontract_data
        ev_httpresponsecode  = lv_createdcontract_responsecd
    ).
    CASE lv_createdcontract_responsecd.
      WHEN 202.
      when 404. "contract not found - but why?
      WHEN OTHERS.
    ENDCASE.
*
    me->lo_nchain_api->zif_proubc_nchain~executecontract(
      EXPORTING
        iv_contract_id      = '0xD4a33860578De61DBAbDc8BFdb98FD742fA7028e'
        is_execcontractreq  = ls_executecontract
      IMPORTING
        ev_apiresponsestr   = lv_executecontract_str
        ev_apiresponse      =  lv_executecontract_data
        ev_httpresponsecode =  lv_executecontract_responsecd
    ).
    CASE lv_executecontract_responsecd.
      WHEN 202.
      WHEN OTHERS.
    ENDCASE.
*    CATCH cx_static_check.
  ENDMETHOD.

  METHOD smartcontract_factory.
    DATA: ls_contract TYPE zif_proubc_nchain=>ty_chainlinkpricefeed_req.
    ls_contract-address = iv_smartcontractaddress.
    ls_contract-name = iv_name.
    ls_contract-network_id = iv_nchain_networkid.
    ls_contract-params-wallet_id = iv_walletaddress.
    ls_contract-params-compiled_artifact-name = 'EACAggregatorProxy'.
    zcl_proubc_file_helper=>get_smartcontract_abi( EXPORTING iv_nchain_networkid = iv_nchain_networkid
                                         iv_smartcontract_address = iv_smartcontractaddress
                               IMPORTING ev_abi_data   = ls_contract-params-compiled_artifact-abi ).
    ls_contract-type = iv_contracttype.
    es_selectedcontract = ls_contract.

  ENDMETHOD.

  METHOD get_vault_helper.
    IF lo_prvd_vault_helper IS NOT BOUND.
      lo_prvd_vault_helper = NEW zcl_proubc_vault_helper(  ).
    ENDIF.
  ENDMETHOD.

  METHOD get_wallet_address.
    me->get_vault_helper( ).
    ev_wallet_address = ''.
  ENDMETHOD.

ENDCLASS.
