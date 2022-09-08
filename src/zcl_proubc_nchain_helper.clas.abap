CLASS zcl_proubc_nchain_helper DEFINITION
  PUBLIC
  INHERITING FROM zcl_proubc_api_helper
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING !iv_tenant      TYPE zPRVDTENANTID OPTIONAL,
      call_chainlink_pricefeed.
  PROTECTED SECTION.
    DATA: lv_tenant         TYPE zprvdtenantid,
          lo_http_client    TYPE REF TO if_http_client,
          lo_nchain_api     TYPE REF TO zcl_proubc_nchain,
          lv_nchain_api_url TYPE string.
    METHODS: smartcontract_factory IMPORTING !iv_smartcontractaddress TYPE zcasesensitive_str
                                             !iv_name                 TYPE string
                                             !iv_contract             TYPE zcasesensitive_str
                                             !iv_walletaddress        TYPE zcasesensitive_str
                                             !iv_abi_index            TYPE zcasesensitive_str
                                             !iv_nchain_networkid     TYPE zcasesensitive_str
                                             !iv_contracttype         TYPE zcasesensitive_str OPTIONAL
                                   EXPORTING !es_selectedContract     TYPE zif_proubc_nchain=>ty_chainlinkpricefeed_req, "
      get_smartcontract_abi IMPORTING !iv_abi_index TYPE zcasesensitive_str
                            EXPORTING !ev_abi_str   TYPE zcasesensitive_str .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_proubc_nchain_helper IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    lv_nchain_api_url = 'https://nchain.provide.services'.

    IF iv_tenant IS NOT INITIAL.
      lv_tenant = iv_tenant.
    ELSE.
      lv_tenant = lv_defaulttenant.
    ENDIF.

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

    lo_nchain_api = NEW zcl_proubc_nchain( ii_client = lo_http_client iv_tenant = lv_tenant iv_bpitoken = lv_bpitoken  ).

  ENDMETHOD.

  METHOD call_chainlink_pricefeed.
    "create the wallet
    DATA: ls_pricefeedwallet            TYPE zif_proubc_nchain=>ty_createhdwalletrequest,
          lv_getwallet_str              TYPE string,
          lv_getwallet_data             TYPE REF TO data,
          lv_getwallet_responsecode     TYPE i,
          ls_selectedcontract           TYPE zif_proubc_nchain=>ty_chainlinkpricefeed_req,
          lv_createdcontract_str        TYPE string,
          lv_createdcontract_data       TYPE REF TO data,
          lv_createdcontract_responsecd TYPE i.
    ls_pricefeedwallet-purpose = 44.
    me->lo_nchain_api->zif_proubc_nchain~createhdwallet( EXPORTING is_walletrequest = ls_pricefeedwallet
                                                         IMPORTING ev_apiresponsestr   = lv_getwallet_str
                                                                   ev_apiresponse       = lv_getwallet_data
                                                                   ev_httpresponsecode = lv_getwallet_responsecode ).
    CASE lv_getwallet_responsecode.
      WHEN 202.
      WHEN OTHERS. "add error handling
    ENDCASE.
    "eth/usd pair -- see https://docs.chain.link/docs/consuming-data-feeds/
    "https://goerli.etherscan.io/address/{{iv_smartcontractaddress}}#code
    me->smartcontract_factory(  EXPORTING iv_smartcontractaddress = '0xD4a33860578De61DBAbDc8BFdb98FD742fA7028e'
                                          iv_name                 = 'ETH/USD'
                                          iv_contract             = '' "this is more complex
                                          iv_walletaddress        = '' "from the wallet we created earlier
                                          iv_abi_index            = '' "might have been alread declared in iv_contract
                                          iv_nchain_networkid     = '1b16996e-3595-4985-816c-043345d22f8c' "goerli testnet nchain id, check if this always same
                                          iv_contracttype         = 'price-feed'
                                IMPORTING es_selectedContract = ls_selectedcontract ).
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
      WHEN OTHERS.
    ENDCASE.
*
    "me->lo_nchain_api->zif_proubc_nchain~executecontract( ).
  ENDMETHOD.

  METHOD smartcontract_factory.
    DATA: ls_contract TYPE zif_proubc_nchain=>ty_chainlinkpricefeed_req.
    ls_contract-address = iv_smartcontractaddress.
    ls_contract-name = iv_name.
    ls_contract-network_id = iv_nchain_networkid.
    ls_contract-params-wallet_id = iv_walletaddress.
    ls_contract-params-compiled_artifact-name = 'EACAggregatorProxy'.
    me->get_smartcontract_abi( EXPORTING iv_abi_index = iv_abi_index
                               IMPORTING ev_abi_str   = ls_contract-params-compiled_artifact-abi ).
    ls_contract-type = iv_contracttype.

  ENDMETHOD.

  METHOD get_smartcontract_abi.
  ENDMETHOD.

ENDCLASS.
