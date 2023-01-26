"INHERITING FROM zcl_proubc_api_helper
class ZCL_PRVD_VAULT_HELPER definition
  public
  final
  create public

  global friends ZCL_PRVD_API_HELPER
                 ZCL_PRVD_NCHAIN_HELPER .

public section.

    "! Constructor method to return PRVD Vault Helper class instance
  methods CONSTRUCTOR
    importing
      !IO_API_HELPER type ref to ZCL_PRVD_API_HELPER optional
      !IV_ORG_ID type ZPRVDTENANTID optional
      !IV_SUBJECT_ACCOUNT_ID type ZPRVDTENANTID optional
      !IV_WORKGROUP_ID type ZPRVDTENANTID optional
      !IV_VAULT_API_URL type STRING optional .
    "! Method to create a key
  methods CREATE_KEY .
    "! Lists the PRVD Vault(s) created for the user
  methods LIST_VAULTS
    exporting
      !ET_VAULT_LIST type ZIF_PRVD_VAULT=>TTY_VAULT_QUERY .
    "! Creates a PRVD Vault for the user
  methods CREATE_VAULT .
    "! Derives a key for
  methods DERIVE_KEY .
  methods LIST_KEYS .
    "! Deletes the specified user key upon user request
  methods DELETE_KEYS .
    "! Encrypts data
  methods ENCRYPT .
    "! Decrypts data
  methods DECRYPT .
    "! Used to cryptographically sign data
  methods SIGN .
    "! Used to cryptographically verify data
  methods VERIFY .
    "! Initializes other aspects of the vault helper class to ensure connectivity to Vault microservice
  methods SETUP_VAULT_MSGS .
    "! Retrieves the wallet address per Vault data input specs
  methods GET_WALLET_ADDRESS
    returning
      value(RV_WALLET_ADDRESS) type ZPRVD_SMARTCONTRACT_ADDR .
  PROTECTED SECTION.
    DATA: mo_api_helper    TYPE REF TO zcl_prvd_api_helper,
          mv_tenant        TYPE zprvdtenantid,
          mo_http_client   TYPE REF TO if_http_client,
          mo_vault_api     TYPE REF TO zcl_prvd_vault,
          mv_vault_api_url TYPE string.
  PRIVATE SECTION.
    METHODS: get_vault_client RETURNING VALUE(ro_vault_client) TYPE REF TO zcl_prvd_vault.
ENDCLASS.



CLASS ZCL_PRVD_VAULT_HELPER IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    IF io_api_helper IS BOUND.
      mo_api_helper = io_api_helper.
    ELSE.
      mo_api_helper = NEW zcl_prvd_api_helper( iv_tenant          = iv_org_id
                                                 iv_subject_acct_id = iv_subject_account_id
                                                 iv_workgroup_id    = iv_workgroup_id ).
    ENDIF.

    mv_vault_api_url = 'https://vault.provide.services'.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = mv_vault_api_url
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

    "todo fix params
    "mo_vault_api = NEW zcl_proubc_vault( ii_client = mo_http_client iv_tenant = mv_tenant iv_bpitoken = lv_bpitoken  ).

  ENDMETHOD.


  METHOD create_key.
    mo_vault_api = me->get_vault_client( ).
*    mo_vault_api->create_key(
*      EXPORTING
*        authorization =
*        content_type  =
*        vault_id      =
*        body          =
*    ).
*    CATCH cx_static_check.
  ENDMETHOD.


  METHOD create_vault.
    mo_vault_api = me->get_vault_client( ).
*    mo_vault_api->create_vault(
*      EXPORTING
*        content_type  =
*        authorization =
*        body          =
*    ).
*    CATCH cx_static_check.
  ENDMETHOD.


  METHOD decrypt.
    mo_vault_api = me->get_vault_client( ).
  ENDMETHOD.


  METHOD delete_keys.
    mo_vault_api = me->get_vault_client( ).
  ENDMETHOD.


  METHOD derive_key.
    mo_vault_api = me->get_vault_client( ).
  ENDMETHOD.


  METHOD encrypt.
    mo_vault_api = me->get_vault_client( ).
  ENDMETHOD.


  METHOD get_vault_client.
    IF mo_vault_api IS BOUND.
      cl_http_client=>create_by_url(
    EXPORTING
      url                = mv_vault_api_url
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
      mo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client value = '100' ).

      "todo fix params
      "mo_vault_api = NEW zcl_proubc_vault( ii_client = mo_http_client iv_tenant = mv_tenant iv_bpitoken = lv_bpitoken  ).
      ro_vault_client = mo_vault_api.
    ELSE.
      ro_vault_client = mo_vault_api.
    ENDIF.
  ENDMETHOD.


  METHOD get_wallet_address.
    rv_wallet_address = ''.
  ENDMETHOD.


  METHOD list_keys.
    mo_vault_api = me->get_vault_client( ).
  ENDMETHOD.


  METHOD list_vaults.
    mo_vault_api = me->get_vault_client( ).
    mo_vault_api->zif_prvd_vault~list_vaults( IMPORTING
        "authorization =
        et_vault_list = et_vault_list
    ).
*    CATCH cx_static_check.
  ENDMETHOD.


  METHOD setup_vault_msgs.
  ENDMETHOD.


  METHOD sign.
    mo_vault_api = me->get_vault_client( ).
  ENDMETHOD.


  METHOD verify.
    mo_vault_api = me->get_vault_client( ).
  ENDMETHOD.
ENDCLASS.
