interface ZIF_PRVD_NCHAIN
  public .


  types:
* Component schema: CreateconnectorRequest, object
    BEGIN OF ty_createconnectorrequest,
           name       TYPE string,
           network_id TYPE string,
           type       TYPE string,
           config     TYPE config,
         END OF ty_createconnectorrequest .
  types:
* Component schema: Credentials, object
    BEGIN OF ty_credentials,
           aws_access_key_id     TYPE string,
           aws_secret_access_key TYPE string,
         END OF ty_credentials .
  types:
* Component schema: m00000, object
    BEGIN OF ty_m00000,
           tcp TYPE STANDARD TABLE OF string WITH EMPTY KEY,
           udp TYPE STANDARD TABLE OF string WITH EMPTY KEY,
         END OF ty_m00000 .
  types:
* Component schema: Ingress, object
    BEGIN OF ty_ingress,
           ingress TYPE ty_m00000,
         END OF ty_ingress .
  types:
* Component schema: Security, object
    BEGIN OF ty_security,
           egress  TYPE string,
           ingress TYPE ty_ingress,
         END OF ty_security .
  types:
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
  types:
* Component schema: Chainspec, object
    BEGIN OF ty_subchainspec_alloc,
           dummy_workaround TYPE i,
         END OF ty_subchainspec_alloc .
  types:
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
  types:
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
  types:
* Component schema: Config1, object
    BEGIN OF ty_config1,
           native_currency TYPE string,
           platform        TYPE string,
           engine_id       TYPE string,
           chain           TYPE string,
           protocol_id     TYPE string,
           chainspec       TYPE ty_chainspec,
         END OF ty_config1 .
  types:
* Component schema: CreatenetworkRequest, object
    BEGIN OF ty_createnetworkrequest,
           name      TYPE string,
           cloneable TYPE abap_bool,
           config    TYPE ty_config1,
         END OF ty_createnetworkrequest .
  types:
* Component schema: CreateaccountsRequest, object
    BEGIN OF ty_createaccountsrequest,
           network_id TYPE string,
         END OF ty_createaccountsrequest .
  types:
* Component schema: CreateHDwalletRequest, object
    BEGIN OF ty_createhdwalletrequest,
           purpose TYPE i,
         END OF ty_createhdwalletrequest .
  types:
* Component schema: create_broadcast_transaction-AccountRequest, object
    BEGIN OF ty_create_broadcast_txn_ac,
           network_id TYPE string,
           key_id     TYPE string,
           to         TYPE string,
           value      TYPE i,
           account_id TYPE string,
         END OF ty_create_broadcast_txn_ac .
  types:
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
  types:
* Component schema: CompiledArtifact, object
    BEGIN OF ty_compiledartifact,
           contractname TYPE string,
           abi          TYPE STANDARD TABLE OF string WITH EMPTY KEY,
           bytecode     TYPE string,
           source       TYPE string,
         END OF ty_compiledartifact .
  types:
    BEGIN OF ty_pricefeed_compiledartifact,
           name TYPE zcasesensitive_str,
           "abi  TYPE zcasesensitive_str,
           abi  TYPE REF TO data,
         END OF ty_pricefeed_compiledartifact .
  types:
* Component schema: Params, object
    BEGIN OF ty_params,
           account_id        TYPE string,
           compiled_artifact TYPE ty_compiledartifact,
         END OF ty_params .
  types:
* Component schema: DeploycontractRequest, object
    BEGIN OF ty_deploycontractrequest,
           application_id TYPE string,
           network_id     TYPE string,
           name           TYPE string,
           address        TYPE string,
           params         TYPE ty_params,
         END OF ty_deploycontractrequest .
  types:
* Component schema: Abi, object
    BEGIN OF ty_abi,
           anonymous       TYPE abap_bool,
           inputs          TYPE STANDARD TABLE OF string WITH EMPTY KEY,
           name            TYPE string,
           type            TYPE string,
           outputs         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
           statemutability TYPE string,
         END OF ty_abi .
  types:
* Component schema: Input, object
    BEGIN OF ty_input,
           indexed      TYPE abap_bool,
           internaltype TYPE string,
           name         TYPE string,
           type         TYPE string,
         END OF ty_input .
  types:
* Component schema: Assembly, object
    BEGIN OF ty_subassembly_data,
           dummy_workaround TYPE i,
         END OF ty_subassembly_data .
  types:
    BEGIN OF ty_assembly,
           _code TYPE STANDARD TABLE OF string WITH EMPTY KEY,
           _data TYPE ty_subassembly_data,
         END OF ty_assembly .
  types:
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
  types:
* Component schema: Params1, object
    BEGIN OF ty_params1,
           account_id        TYPE string,
           compiled_artifact TYPE ty_compiledartifact1,
         END OF ty_params1 .
  types:
* Component schema: BaselinedeployregistrycontractRequest, object
    BEGIN OF ty_baselineregistrycontract,
           application_id TYPE string,
           network_id     TYPE string,
           name           TYPE string,
           address        TYPE string,
           params         TYPE ty_params1,
         END OF ty_baselineregistrycontract .
  types:
* Component schema: Abi1, object
    BEGIN OF ty_abi1,
           inputs          TYPE STANDARD TABLE OF string WITH EMPTY KEY,
           payable         TYPE abap_bool,
           statemutability TYPE string,
           type            TYPE string,
           anonymous       TYPE abap_bool,
           name            TYPE string,
         END OF ty_abi1 .
  types:
* Component schema: Input1, object
    BEGIN OF ty_input1,
           indexed TYPE abap_bool,
           name    TYPE string,
           type    TYPE string,
         END OF ty_input1 .
  types:
* Component schema: Code, object
    BEGIN OF ty_code,
           begin TYPE i,
           end   TYPE i,
           name  TYPE string,
           value TYPE string,
         END OF ty_code .
  types:
* Component schema: generatedObject, object
    BEGIN OF ty_generatedobject,
           _auxdata TYPE string,
           _code    TYPE STANDARD TABLE OF string WITH EMPTY KEY,
         END OF ty_generatedobject .
  types:
* Component schema: generatedObject1, object
    BEGIN OF ty_subgeneratedobject1_data,
           dummy_workaround TYPE i,
         END OF ty_subgeneratedobject1_data .
  types:
    BEGIN OF ty_generatedobject1,
           _code TYPE STANDARD TABLE OF string WITH EMPTY KEY,
         END OF ty_generatedobject1 .
  types:
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
  types:
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
  types:
* Component schema: Input2, object
    BEGIN OF ty_input2,
           name    TYPE string,
           type    TYPE string,
           indexed TYPE abap_bool,
         END OF ty_input2 .
  types:
* Component schema: Output, object
    BEGIN OF ty_output,
           name TYPE string,
           type TYPE string,
         END OF ty_output .
  types:
* Component schema: Assembly2, object
    BEGIN OF ty_subassembly2_data,
           dummy_workaround TYPE i,
         END OF ty_subassembly2_data .
  types:
    BEGIN OF ty_assembly2,
           _code TYPE STANDARD TABLE OF string WITH EMPTY KEY,
           _data TYPE ty_subassembly2_data,
         END OF ty_assembly2 .
  types:
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
  types:
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
  types:
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
  types:
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
  types:
* Component schema: Input4, object
    BEGIN OF ty_input4,
           name TYPE string,
           type TYPE string,
         END OF ty_input4 .
  types:
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
  types:
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
  types:
* Component schema: ExecutecontractRequest, object
    BEGIN OF ty_executecontractrequest,
           method    TYPE string,
           params    TYPE STANDARD TABLE OF string WITH EMPTY KEY,
           value     TYPE i,
           wallet_id TYPE string,
         END OF ty_executecontractrequest .
  types:
* Component schema: ExecutereadonlycontractRequest, object
    BEGIN OF ty_executereadonlycontractreq,
           method     TYPE string,
           params     TYPE STANDARD TABLE OF string WITH EMPTY KEY,
           account_id TYPE string,
         END OF ty_executereadonlycontractreq .
  types:
    BEGIN OF ty_pricefeed_req_params,
           argv              TYPE STANDARD TABLE OF string WITH EMPTY KEY,
           wallet_id         TYPE zcasesensitive_str,
           compiled_artifact TYPE ty_pricefeed_compiledartifact,
         END OF ty_pricefeed_req_params .
  types:
    BEGIN OF ty_chainlinkpricefeed_req,
           address    TYPE zcasesensitive_str,
           name       TYPE zcasesensitive_str,
           network_id TYPE zcasesensitive_str,
           params     TYPE ty_pricefeed_req_params,
           type       TYPE zcasesensitive_str,
         END OF ty_chainlinkpricefeed_req .
  types:
    BEGIN OF ty_hdwalletcreate_resp,
           id              TYPE string,
           created_at      TYPE string,
           organization_id TYPE string,
           vault_id        TYPE string,
           key_id          TYPE string,
           purpose         TYPE i,
           public_key      TYPE string,
         END OF ty_hdwalletcreate_resp .
  types:
    BEGIN OF ty_executecontract_resp,
           confidence TYPE zcasesensitive_str,
           ref        TYPE zcasesensitive_str,
           response   TYPE REF TO data,
         END OF ty_executecontract_resp .
  types:
    BEGIN OF ty_executecontract_summary,
            nchain_network_id TYPE zprvd_nchain_networkid,
            smartcontract_addr TYPE zprvd_smartcontract_addr,
            prvd_stack_contractid TYPE string,
            walletid TYPE zprvd_smartcontract_addr,
         END OF ty_executecontract_summary .

"! GET - "List connectors"
"! Operation id: Listconnectors
"! Parameter: public, required, query
"! Parameter: content-type, required, header
"! Response: 200
  methods LISTCONNECTORS
    importing
      !IV_PUBLIC type ABAP_BOOL
      !IV_CONTENT_TYPE type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! POST - "Create connector"
"! Operation id: Createconnector
"! Response: 200
"! Body ref: #/components/schemas/CreateconnectorRequest
  methods CREATECONNECTOR
    importing
      !IS_BODY type TY_CREATECONNECTORREQUEST
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! GET - "Get connector details"
"! Operation id: Getconnectordetails
"! Parameter: connector_id, required, path
"! Response: 200
  methods GETCONNECTORDETAILS
    importing
      !IV_CONNECTOR_ID type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! DELETE - "Delete connector"
"! Operation id: Deleteconnector
"! Parameter: connector_id, required, path
"! Response: 200
  methods DELETECONNECTOR
    importing
      !IV_CONNECTOR_ID type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! PUT - "Update network"
"! Operation id: Updatenetwork
"! Parameter: connector_id, required, path
"! Response: 200
  methods UPDATENETWORK
    importing
      !IV_CONNECTOR_ID type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! GET - "Get load balancer details"
"! Operation id: Getloadbalancerdetails
"! Parameter: connector_id, required, path
"! Response: 200
  methods GETLOADBALANCERDETAILS
    importing
      !IV_CONNECTOR_ID type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! GET - "List networks"
"! Operation id: Listnetworks
"! Parameter: network_id, required, path
"! Response: 200
  methods LISTNETWORKS
    importing
      !IV_NETWORK_ID type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! GET - "Get network detail"
"! Operation id: Getnetworkdetail
"! Parameter: connector_id, required, path
"! Response: 200
  methods GETNETWORKDETAIL
    importing
      !IV_CONNECTOR_ID type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! GET - "Get network status"
"! Operation id: Getnetworkstatus
"! Parameter: network_id, required, path
"! Response: 200
  methods GETNETWORKSTATUS
    importing
      !IV_NETWORK_ID type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! POST - "Create network"
"! Operation id: Createnetwork
"! Response: 200
"! Body ref: #/components/schemas/CreatenetworkRequest
  methods CREATENETWORK
    importing
      !IS_BODY type TY_CREATENETWORKREQUEST
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! POST - "Create accounts"
"! Operation id: Createaccounts
"! Response: 200
"! Body ref: #/components/schemas/CreateaccountsRequest
  methods CREATEACCOUNTS
    importing
      !IS_BODY type TY_CREATEACCOUNTSREQUEST
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! GET - "List accounts"
"! Operation id: Listaccounts
"! Parameter: content-type, required, header
"! Response: 200
  methods LISTACCOUNTS
    importing
      !IV_CONTENT_TYPE type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! GET - "Get account details"
"! Operation id: Getaccountdetails
"! Parameter: content-type, required, header
"! Parameter: account_id, required, path
"! Response: 200
  methods GETACCOUNTDETAILS
    importing
      !IV_CONTENT_TYPE type STRING
      !IV_ACCOUNT_ID type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! GET - "List HD wallets"
"! Operation id: ListHDwallets
"! Parameter: content-type, required, header
"! Response: 200
  methods LISTHDWALLETS
    importing
      !IV_CONTENT_TYPE type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! POST - "Create HD wallet"
"! Operation id: CreateHDwallet
"! Response: 200
"! Body ref: #/components/schemas/CreateHDwalletRequest
  methods CREATEHDWALLET
    importing
      !IS_WALLETREQUEST type TY_CREATEHDWALLETREQUEST
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! GET - "List HD wallet accounts"
"! Operation id: ListHDwalletaccounts
"! Parameter: page, required, query
"! Parameter: content-type, required, header
"! Parameter: wallet_id, required, path
"! Response: 200
  methods LISTHDWALLETACCOUNTS
    importing
      !IV_PAGE type I
      !IV_CONTENT_TYPE type STRING
      !IV_WALLET_ID type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! GET - "List transactions"
"! Operation id: Listtransactions
"! Parameter: content-type, required, header
"! Response: 200
  methods LISTTRANSACTIONS
    importing
      !IV_CONTENT_TYPE type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! POST - "Create & broadcast transaction - Account"
"! Operation id: create_broadcast_transaction-Account
"! Response: 200
"! Body ref: #/components/schemas/Create%26broadcasttransaction-AccountRequest
  methods CREATE_BROADCAST_TXN_AC
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! GET - "Get transaction details"
"! Operation id: Gettransactiondetails
"! Parameter: content-type, required, header
"! Parameter: transaction_id, required, path
"! Response: 200
  methods GETTRANSACTIONDETAILS
    importing
      !IV_CONTENT_TYPE type STRING
      !IV_TRANSACTION_ID type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! GET - "List contracts"
"! Operation id: Listcontracts
"! Parameter: content-type, required, header
"! Response: 200
  methods LISTCONTRACTS
    importing
      !IV_CONTENT_TYPE type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! POST - "Deploy contract"
"! Operation id: Deploycontract
"! Response: 200
"! Body ref: #/components/schemas/DeploycontractRequest
  methods DEPLOYCONTRACT
    importing
      !IS_BODY type TY_DEPLOYCONTRACTREQUEST
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! GET - "Get contract detail"
"! Operation id: Getcontractdetail
"! Parameter: content-type, required, header
"! Parameter: contract_id, required, path
"! Response: 200
  methods GETCONTRACTDETAIL
    importing
      !IV_CONTENT_TYPE type STRING
      !IV_CONTRACT_ID type STRING
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! POST - "Execute contract"
"! Operation id: Executecontract
"! Parameter: contract_id, required, path
"! Response: 200
"! Body ref: #/components/schemas/ExecutecontractRequest
  methods EXECUTECONTRACT
    importing
      !IV_CONTRACT_ID type ZCASESENSITIVE_STR
      !IS_EXECCONTRACTREQ type TY_EXECUTECONTRACTREQUEST
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSEXSTR type XSTRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
"! POST - "Execute read only contract"
"! Operation id: Executereadonlycontract
"! Response: 200
"! Body ref: #/components/schemas/ExecutereadonlycontractRequest
  methods EXECUTEREADONLYCONTRACT
    importing
      !IS_BODY type TY_EXECUTEREADONLYCONTRACTREQ
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
  methods CREATEPRICEFEEDCONTRACT
    importing
      !IV_SMARTCONTRACTADDR type ZPRVD_SMARTCONTRACT_ADDR
      !IS_PRICEFEEDCONTRACT type TY_CHAINLINKPRICEFEED_REQ
    exporting
      !EV_APIRESPONSESTR type STRING
      !EV_APIRESPONSE type ref to DATA
      !EV_HTTPRESPONSECODE type I
    raising
      CX_STATIC_CHECK .
endinterface.
