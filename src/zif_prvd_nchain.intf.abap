INTERFACE zif_prvd_nchain
  PUBLIC .


  TYPES:
* Component schema: CreateconnectorRequest, object
    BEGIN OF ty_createconnectorrequest,
      name       TYPE string,
      network_id TYPE string,
      type       TYPE string,
      config     TYPE config,
    END OF ty_createconnectorrequest .
  TYPES:
* Component schema: Credentials, object
    BEGIN OF ty_credentials,
      aws_access_key_id     TYPE string,
      aws_secret_access_key TYPE string,
    END OF ty_credentials .
  TYPES:
* Component schema: m00000, object
    BEGIN OF ty_m00000,
      tcp TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      udp TYPE STANDARD TABLE OF string WITH EMPTY KEY,
    END OF ty_m00000 .
  TYPES:
* Component schema: Ingress, object
    BEGIN OF ty_ingress,
      ingress TYPE ty_m00000,
    END OF ty_ingress .
  TYPES:
* Component schema: Security, object
    BEGIN OF ty_security,
      egress  TYPE string,
      ingress TYPE ty_ingress,
    END OF ty_security .
  TYPES:
* Component schema: Config, object
    BEGIN OF ty_config,
      region      TYPE string,
      target_id   TYPE string,
      provider_id TYPE string,
      role        TYPE string,
      container   TYPE string,
      credentials TYPE STANDARD TABLE OF ty_credentials WITH EMPTY KEY,
      image       TYPE string,
      api_port    TYPE i,
      security    TYPE STANDARD TABLE OF ty_security WITH EMPTY KEY,
    END OF ty_config .
  TYPES:
* Component schema: Chainspec, object
    BEGIN OF ty_subchainspec_alloc,
      dummy_workaround TYPE i,
    END OF ty_subchainspec_alloc .
  TYPES:
* Component schema: Config2, object
    BEGIN OF ty_config2,
      homesteadblock      TYPE i,
      eip150block         TYPE i,
      eip155block         TYPE i,
      eip158block         TYPE i,
      byzantiumblock      TYPE i,
      constantinopleblock TYPE i,
      petersburgblock     TYPE i,
    END OF ty_config2 .
  TYPES:
    BEGIN OF ty_chainspec,
      config     TYPE ty_config2,
      alloc      TYPE ty_subchainspec_alloc,
      coinbase   TYPE string,
      difficulty TYPE string,
      extradata  TYPE string,
      gaslimit   TYPE string,
      nonce      TYPE string,
      mixhash    TYPE string,
      parenthash TYPE string,
      timestamp  TYPE string,
    END OF ty_chainspec .
  TYPES:
* Component schema: Config1, object
    BEGIN OF ty_config1,
      native_currency TYPE string,
      platform        TYPE string,
      engine_id       TYPE string,
      chain           TYPE string,
      protocol_id     TYPE string,
      chainspec       TYPE ty_chainspec,
    END OF ty_config1 .
  TYPES:
* Component schema: CreatenetworkRequest, object
    BEGIN OF ty_createnetworkrequest,
      name      TYPE string,
      cloneable TYPE abap_bool,
      config    TYPE ty_config1,
    END OF ty_createnetworkrequest .
  TYPES:
* Component schema: CreateaccountsRequest, object
    BEGIN OF ty_createaccountsrequest,
      network_id TYPE string,
    END OF ty_createaccountsrequest .
  TYPES:
* Component schema: CreateHDwalletRequest, object
    BEGIN OF ty_createhdwalletrequest,
      purpose TYPE i,
    END OF ty_createhdwalletrequest .
  TYPES:
* Component schema: create_broadcast_transaction-AccountRequest, object
    BEGIN OF ty_create_broadcast_txn_ac,
      network_id TYPE string,
      key_id     TYPE string,
      to         TYPE string,
      value      TYPE i,
      user_id    TYPE string,
    END OF ty_create_broadcast_txn_ac .
  TYPES:
* Component schema: create_broadcast_transaction-WalletRequest, object
    BEGIN OF ty_create_broadcast_txn_wa,
      network_id         TYPE string,
      key_id             TYPE string,
      from               TYPE string,
      to                 TYPE string,
      value              TYPE i,
      wallet_id          TYPE string,
      hd_derivation_path TYPE string,
    END OF ty_create_broadcast_txn_wa .
  TYPES:
* Component schema: CompiledArtifact, object
    BEGIN OF ty_compiledartifact,
      contractname TYPE string,
      abi          TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      bytecode     TYPE string,
      source       TYPE string,
    END OF ty_compiledartifact .
  TYPES:
    BEGIN OF ty_pricefeed_compiledartifact,
      name TYPE zcasesensitive_str,
      "abi  TYPE zcasesensitive_str,
      abi  TYPE REF TO data,
    END OF ty_pricefeed_compiledartifact .
  TYPES:
* Component schema: Params, object
    BEGIN OF ty_params,
      account_id        TYPE string,
      compiled_artifact TYPE ty_compiledartifact,
    END OF ty_params .
  TYPES:
* Component schema: DeploycontractRequest, object
    BEGIN OF ty_deploycontractrequest,
      application_id TYPE string,
      network_id     TYPE string,
      name           TYPE string,
      address        TYPE string,
      params         TYPE ty_params,
    END OF ty_deploycontractrequest .
  TYPES:
* Component schema: Abi, object
    BEGIN OF ty_abi,
      anonymous       TYPE abap_bool,
      inputs          TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      name            TYPE string,
      type            TYPE string,
      outputs         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      statemutability TYPE string,
    END OF ty_abi .
  TYPES:
* Component schema: Input, object
    BEGIN OF ty_input,
      indexed      TYPE abap_bool,
      internaltype TYPE string,
      name         TYPE string,
      type         TYPE string,
    END OF ty_input .
  TYPES:
* Component schema: Assembly, object
    BEGIN OF ty_subassembly_data,
      dummy_workaround TYPE i,
    END OF ty_subassembly_data .
  TYPES:
    BEGIN OF ty_assembly,
      _code TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      _data TYPE ty_subassembly_data,
    END OF ty_assembly .
  TYPES:
* Component schema: CompiledArtifact1, object
    BEGIN OF ty_compiledartifact1,
      abi         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      assembly    TYPE ty_assembly,
      bytecode    TYPE string,
      deps        TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      fingerprint TYPE string,
      name        TYPE string,
      opcodes     TYPE string,
      raw         TYPE string,
      source      TYPE string,
    END OF ty_compiledartifact1 .
  TYPES:
* Component schema: Params1, object
    BEGIN OF ty_params1,
      account_id        TYPE string,
      compiled_artifact TYPE ty_compiledartifact1,
    END OF ty_params1 .
  TYPES:
* Component schema: BaselinedeployregistrycontractRequest, object
    BEGIN OF ty_baselineregistrycontract,
      application_id TYPE string,
      network_id     TYPE string,
      name           TYPE string,
      address        TYPE string,
      params         TYPE ty_params1,
    END OF ty_baselineregistrycontract .
  TYPES:
* Component schema: Abi1, object
    BEGIN OF ty_abi1,
      inputs          TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      payable         TYPE abap_bool,
      statemutability TYPE string,
      type            TYPE string,
      anonymous       TYPE abap_bool,
      name            TYPE string,
    END OF ty_abi1 .
  TYPES:
* Component schema: Input1, object
    BEGIN OF ty_input1,
      indexed TYPE abap_bool,
      name    TYPE string,
      type    TYPE string,
    END OF ty_input1 .
  TYPES:
* Component schema: Code, object
    BEGIN OF ty_code,
      begin TYPE i,
      end   TYPE i,
      name  TYPE string,
      value TYPE string,
    END OF ty_code .
  TYPES:
* Component schema: generatedObject, object
    BEGIN OF ty_generatedobject,
      _auxdata TYPE string,
      _code    TYPE STANDARD TABLE OF string WITH EMPTY KEY,
    END OF ty_generatedobject .
  TYPES:
* Component schema: generatedObject1, object
    BEGIN OF ty_subgeneratedobject1_data,
      dummy_workaround TYPE i,
    END OF ty_subgeneratedobject1_data .
  TYPES:
    BEGIN OF ty_generatedobject1,
      _code TYPE STANDARD TABLE OF string WITH EMPTY KEY,
    END OF ty_generatedobject1 .
  TYPES:
* Component schema: Dep, object
    BEGIN OF ty_dep,
      abi         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      assembly    TYPE ty_assembly,
      bytecode    TYPE string,
      deps        TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      fingerprint TYPE string,
      name        TYPE string,
      opcodes     TYPE string,
      raw         TYPE string,
      source      TYPE string,
    END OF ty_dep .
  TYPES:
* Component schema: Abi2, object
    BEGIN OF ty_abi2,
      constant        TYPE abap_bool,
      inputs          TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      name            TYPE string,
      outputs         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      payable         TYPE abap_bool,
      statemutability TYPE string,
      type            TYPE string,
      anonymous       TYPE abap_bool,
    END OF ty_abi2 .
  TYPES:
* Component schema: Input2, object
    BEGIN OF ty_input2,
      name    TYPE string,
      type    TYPE string,
      indexed TYPE abap_bool,
    END OF ty_input2 .
  TYPES:
* Component schema: Output, object
    BEGIN OF ty_output,
      name TYPE string,
      type TYPE string,
    END OF ty_output .
  TYPES:
* Component schema: Assembly2, object
    BEGIN OF ty_subassembly2_data,
      dummy_workaround TYPE i,
    END OF ty_subassembly2_data .
  TYPES:
    BEGIN OF ty_assembly2,
      _code TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      _data TYPE ty_subassembly2_data,
    END OF ty_assembly2 .
  TYPES:
* Component schema: Dep1, object
    BEGIN OF ty_dep1,
      abi         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      assembly    TYPE ty_assembly2,
      bytecode    TYPE string,
      deps        TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      fingerprint TYPE string,
      name        TYPE string,
      opcodes     TYPE string,
      raw         TYPE string,
      source      TYPE string,
    END OF ty_dep1 .
  TYPES:
* Component schema: Abi3, object
    BEGIN OF ty_abi3,
      constant        TYPE abap_bool,
      inputs          TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      name            TYPE string,
      outputs         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      payable         TYPE abap_bool,
      statemutability TYPE string,
      type            TYPE string,
      anonymous       TYPE abap_bool,
    END OF ty_abi3 .
  TYPES:
* Component schema: Dep2, object
    BEGIN OF ty_dep2,
      abi         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      assembly    TYPE string,
      bytecode    TYPE string,
      deps        TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      fingerprint TYPE string,
      name        TYPE string,
      opcodes     TYPE string,
      raw         TYPE string,
      source      TYPE string,
    END OF ty_dep2 .
  TYPES:
* Component schema: Abi4, object
    BEGIN OF ty_abi4,
      constant        TYPE abap_bool,
      inputs          TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      name            TYPE string,
      outputs         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      payable         TYPE abap_bool,
      statemutability TYPE string,
      type            TYPE string,
    END OF ty_abi4 .
  TYPES:
* Component schema: Input4, object
    BEGIN OF ty_input4,
      name TYPE string,
      type TYPE string,
    END OF ty_input4 .
  TYPES:
* Component schema: Dep3, object
    BEGIN OF ty_dep3,
      abi         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      assembly    TYPE string,
      bytecode    TYPE string,
      deps        TYPE string,
      fingerprint TYPE string,
      name        TYPE string,
      opcodes     TYPE string,
      raw         TYPE string,
      source      TYPE string,
    END OF ty_dep3 .
  TYPES:
* Component schema: Abi5, object
    BEGIN OF ty_abi5,
      constant        TYPE abap_bool,
      inputs          TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      name            TYPE string,
      outputs         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      payable         TYPE abap_bool,
      statemutability TYPE string,
      type            TYPE string,
    END OF ty_abi5 .
  TYPES:
* Component schema: ExecutecontractRequest, object
    BEGIN OF ty_executecontractrequest,
      method    TYPE string,
      params    TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      value     TYPE i,
      wallet_id TYPE string,
    END OF ty_executecontractrequest,
    BEGIN OF ty_executecontractreq_account,
      method     TYPE string,
      params     TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      value      TYPE i,
      account_id TYPE string,
    END OF ty_executecontractreq_account.
  TYPES:
* Component schema: ExecutereadonlycontractRequest, object
    BEGIN OF ty_executereadonlycontractreq,
      method     TYPE string,
      params     TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      account_id TYPE string,
    END OF ty_executereadonlycontractreq .
  TYPES:
    BEGIN OF ty_pricefeed_req_params,
      argv              TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      wallet_id         TYPE zcasesensitive_str,
      compiled_artifact TYPE ty_pricefeed_compiledartifact,
    END OF ty_pricefeed_req_params .
  TYPES:
    BEGIN OF ty_chainlinkpricefeed_req,
      address    TYPE zcasesensitive_str,
      name       TYPE zcasesensitive_str,
      network_id TYPE zcasesensitive_str,
      params     TYPE ty_pricefeed_req_params,
      type       TYPE zcasesensitive_str,
    END OF ty_chainlinkpricefeed_req .
  TYPES:
    BEGIN OF ty_create_contract_req,
      address    TYPE zcasesensitive_str,
      name       TYPE zcasesensitive_str,
      network_id TYPE zcasesensitive_str,
      params     TYPE ty_pricefeed_req_params,
      type       TYPE zcasesensitive_str,
    END OF ty_create_contract_req .
  TYPES:
    BEGIN OF ty_hdwalletcreate_resp,
      id              TYPE string,
      created_at      TYPE string,
      organization_id TYPE string,
      vault_id        TYPE string,
      key_id          TYPE string,
      purpose         TYPE i,
      public_key      TYPE string,
    END OF ty_hdwalletcreate_resp .
  TYPES:
    BEGIN OF ty_executecontract_resp,
      confidence TYPE zcasesensitive_str,
      ref        TYPE zcasesensitive_str,
      response   TYPE REF TO data,
    END OF ty_executecontract_resp .
  TYPES:
    BEGIN OF ty_executecontract_summary,
      nchain_network_id     TYPE zprvd_nchain_networkid,
      smartcontract_addr    TYPE zprvd_smartcontract_addr,
      prvd_stack_contractid TYPE string,
      walletid              TYPE zprvd_smartcontract_addr,
    END OF ty_executecontract_summary .

  TYPES: BEGIN OF ty_contract_approval,
           data       TYPE string,
           signature  TYPE string,
           signer     TYPE zprvd_smartcontract_addr,
           network_id TYPE zprvd_nchain_networkid,
           value      TYPE string,
           to         TYPE string,
         END OF ty_contract_approval.

  TYPES: BEGIN OF ty_wallet,
           id              TYPE zcasesensitive_str,
           created_at      TYPE zcasesensitive_str,
           organization_id TYPE zcasesensitive_str,
           vault_id        TYPE zcasesensitive_str,
           key_id          TYPE zcasesensitive_str,
           purpose         TYPE integer,
           public_key      TYPE zcasesensitive_str,
         END OF ty_wallet.
  TYPES: ty_wallet_list TYPE STANDARD TABLE OF ty_wallet.

  TYPES: BEGIN OF ty_account,
           id              TYPE zcasesensitive_str,
           created_at      TYPE zcasesensitive_str,
           network_id      TYPE zcasesensitive_str,
           organization_id TYPE zcasesensitive_str,
           vault_id        TYPE zcasesensitive_str,
           key_id          TYPE zcasesensitive_str,
           public_key      TYPE zcasesensitive_str,
           address         TYPE zcasesensitive_str,
         END OF ty_account.
  TYPES: ty_account_list TYPE STANDARD TABLE OF ty_account.

  types: BEGIN OF ty_basic_txn_details,
            id type zcasesensitive_str,
            ref type zcasesensitive_str,
            network_id type zprvd_nchain_networkid,
            hash type zcasesensitive_str,
         END OF ty_basic_txn_details.


  "! GET - "List connectors"
  "! Operation id: Listconnectors
  "! Parameter: public, required, query
  "! Parameter: content-type, required, header
  "! Response: 200
  METHODS listconnectors
    IMPORTING
      !iv_public           TYPE abap_bool
      !iv_content_type     TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! POST - "Create connector"
  "! Operation id: Createconnector
  "! Response: 200
  "! Body ref: #/components/schemas/CreateconnectorRequest
  METHODS createconnector
    IMPORTING
      !is_body             TYPE ty_createconnectorrequest
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! GET - "Get connector details"
  "! Operation id: Getconnectordetails
  "! Parameter: connector_id, required, path
  "! Response: 200
  METHODS getconnectordetails
    IMPORTING
      !iv_connector_id     TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! DELETE - "Delete connector"
  "! Operation id: Deleteconnector
  "! Parameter: connector_id, required, path
  "! Response: 200
  METHODS deleteconnector
    IMPORTING
      !iv_connector_id     TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! PUT - "Update network"
  "! Operation id: Updatenetwork
  "! Parameter: connector_id, required, path
  "! Response: 200
  METHODS updatenetwork
    IMPORTING
      !iv_connector_id     TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! GET - "Get load balancer details"
  "! Operation id: Getloadbalancerdetails
  "! Parameter: connector_id, required, path
  "! Response: 200
  METHODS getloadbalancerdetails
    IMPORTING
      !iv_connector_id     TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! GET - "List networks"
  "! Operation id: Listnetworks
  "! Parameter: network_id, required, path
  "! Response: 200
  METHODS listnetworks
    IMPORTING
      !iv_network_id       TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! GET - "Get network detail"
  "! Operation id: Getnetworkdetail
  "! Parameter: connector_id, required, path
  "! Response: 200
  METHODS getnetworkdetail
    IMPORTING
      !iv_connector_id     TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! GET - "Get network status"
  "! Operation id: Getnetworkstatus
  "! Parameter: network_id, required, path
  "! Response: 200
  METHODS getnetworkstatus
    IMPORTING
      !iv_network_id       TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! POST - "Create network"
  "! Operation id: Createnetwork
  "! Response: 200
  "! Body ref: #/components/schemas/CreatenetworkRequest
  METHODS createnetwork
    IMPORTING
      !is_body             TYPE ty_createnetworkrequest
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! POST - "Create accounts"
  "! Operation id: Createaccounts
  "! Response: 200
  "! Body ref: #/components/schemas/CreateaccountsRequest
  METHODS createaccounts
    IMPORTING
      !is_body             TYPE ty_createaccountsrequest
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! GET - "List accounts"
  "! Operation id: Listaccounts
  "! Parameter: content-type, required, header
  "! Response: 200
  METHODS listaccounts
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! GET - "Get account details"
  "! Operation id: Getaccountdetails
  "! Parameter: content-type, required, header
  "! Parameter: account_id, required, path
  "! Response: 200
  METHODS getaccountdetails
    IMPORTING
      !iv_content_type     TYPE string
      !iv_account_id       TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! GET - "List HD wallets"
  "! Operation id: ListHDwallets
  "! Parameter: content-type, required, header
  "! Response: 200
  METHODS listhdwallets
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! POST - "Create HD wallet"
  "! Operation id: CreateHDwallet
  "! Response: 200
  "! Body ref: #/components/schemas/CreateHDwalletRequest
  METHODS createhdwallet
    IMPORTING
      !is_walletrequest    TYPE ty_createhdwalletrequest
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! GET - "List HD wallet accounts"
  "! Operation id: ListHDwalletaccounts
  "! Parameter: page, required, query
  "! Parameter: content-type, required, header
  "! Parameter: wallet_id, required, path
  "! Response: 200
  METHODS listhdwalletaccounts
    IMPORTING
      !iv_page             TYPE i
      !iv_content_type     TYPE string
      !iv_wallet_id        TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! GET - "List transactions"
  "! Operation id: Listtransactions
  "! Parameter: content-type, required, header
  "! Response: 200
  METHODS listtransactions
    IMPORTING
      !iv_content_type     TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! POST - "Create & broadcast transaction - Account"
  "! Operation id: create_broadcast_transaction-Account
  "! Response: 200
  "! Body ref: #/components/schemas/Create%26broadcasttransaction-AccountRequest
  METHODS create_broadcast_txn_ac
    IMPORTING is_nchain_txn        TYPE zif_prvd_nchain=>ty_create_broadcast_txn_ac
    EXPORTING
              !ev_apiresponsestr   TYPE string
              !ev_apiresponse      TYPE REF TO data
              !ev_httpresponsecode TYPE i
    RAISING
              cx_static_check .
  "! GET - "Get transaction details"
  "! Operation id: Gettransactiondetails
  "! Parameter: content-type, required, header
  "! Parameter: transaction_id, required, path
  "! Response: 200
  METHODS gettransactiondetails
    IMPORTING
      !iv_transaction_id   TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! GET - "List contracts"
  "! Operation id: Listcontracts
  "! Parameter: content-type, required, header
  "! Response: 200
  METHODS listcontracts
    IMPORTING
      !iv_content_type     TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! POST - "Deploy contract"
  "! Operation id: Deploycontract
  "! Response: 200
  "! Body ref: #/components/schemas/DeploycontractRequest
  METHODS deploycontract
    IMPORTING
      !is_body             TYPE ty_deploycontractrequest
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! GET - "Get contract detail"
  "! Operation id: Getcontractdetail
  "! Parameter: content-type, required, header
  "! Parameter: contract_id, required, path
  "! Response: 200
  METHODS getcontractdetail
    IMPORTING
      !iv_content_type     TYPE string
      !iv_contract_id      TYPE string
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! POST - "Execute contract"
  "! Operation id: Executecontract
  "! Parameter: contract_id, required, path
  "! Response: 200
  "! Body ref: #/components/schemas/ExecutecontractRequest
  METHODS executecontract_by_wallet
    IMPORTING
      !iv_contract_id      TYPE zcasesensitive_str
      !is_execcontractreq  TYPE ty_executecontractrequest
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponsexstr  TYPE xstring
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
    "! POST - "Execute contract"
  "! Operation id: Executecontract
  "! Parameter: contract_id, required, path
  "! Response: 200
  "! Body ref: #/components/schemas/ExecutecontractRequest
  METHODS executecontract_by_account
    IMPORTING
      !iv_contract_id      TYPE zcasesensitive_str
      !is_execcontractreq  TYPE ty_executecontractreq_account
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponsexstr  TYPE xstring
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  "! POST - "Execute read only contract"
  "! Operation id: Executereadonlycontract
  "! Response: 200
  "! Body ref: #/components/schemas/ExecutereadonlycontractRequest
  METHODS executereadonlycontract
    IMPORTING
      !is_body             TYPE ty_executereadonlycontractreq
    EXPORTING
      !ev_apiresponsestr   TYPE string
      !ev_apiresponse      TYPE REF TO data
      !ev_httpresponsecode TYPE i
    RAISING
      cx_static_check .
  METHODS createpricefeedcontract
    IMPORTING
      !iv_smartcontractaddr TYPE zprvd_smartcontract_addr
      !is_pricefeedcontract TYPE ty_chainlinkpricefeed_req
    EXPORTING
      !ev_apiresponsestr    TYPE string
      !ev_apiresponse       TYPE REF TO data
      !ev_httpresponsecode  TYPE i
    RAISING
      cx_static_check .
  METHODS create_contract
    IMPORTING
      !iv_smartcontractaddr TYPE zprvd_smartcontract_addr
      !is_contract          TYPE ty_create_contract_req
    EXPORTING
      !ev_apiresponsestr    TYPE string
      !ev_apiresponse       TYPE REF TO data
      !ev_httpresponsecode  TYPE i
    RAISING
      cx_static_check .
  METHODS approve_smart_contract
    IMPORTING
      !is_contract_approval TYPE ty_contract_approval
    EXPORTING
      !ev_apiresponsestr    TYPE string
      !ev_apiresponse       TYPE REF TO data
      !ev_httpresponsecode  TYPE i
    RAISING
      cx_static_check .
ENDINTERFACE.
