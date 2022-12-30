"INHERITING FROM zcl_proubc_api_helper
CLASS zcl_prvd_vault_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_prvd_api_helper
                 zcl_prvd_nchain_helper .

  PUBLIC SECTION.

    "! Constructor method to return PRVD Vault Helper class instance
    METHODS constructor
      IMPORTING
        !io_api_helper         TYPE REF TO zcl_prvd_api_helper OPTIONAL
        !iv_org_id             TYPE zprvdtenantid OPTIONAL
        !iv_subject_account_id TYPE zprvdtenantid OPTIONAL
        !iv_workgroup_id       TYPE zprvdtenantid OPTIONAL
        !iv_vault_api_url      TYPE string OPTIONAL .
    "! Method to create a key
    METHODS create_key .
    "! Lists the PRVD Vault(s) created for the user
    METHODS list_vaults
      EXPORTING
        !et_vault_list TYPE zif_prvd_vault=>tty_vault_query .
    "! Creates a PRVD Vault for the user
    METHODS create_vault .
    "! Derives a key for
    METHODS derive_key .
    METHODS list_keys .
    "! Deletes the specified user key upon user request
    METHODS delete_keys .
    "! Encrypts data
    METHODS encrypt .
    "! Decrypts data
    METHODS decrypt .
    "! Used to cryptographically sign data
    METHODS sign .
    "! Used to cryptographically verify data
    METHODS verify .
    "! Initializes other aspects of the vault helper class to ensure connectivity to Vault microservice
    METHODS setup_vault_msgs .
    "! Retrieves the wallet address per Vault data input specs
    METHODS get_wallet_address
      RETURNING
        VALUE(rv_wallet_address) TYPE zproubc_smartcontract_addr .
  PROTECTED SECTION.
    DATA: mo_api_helper    TYPE REF TO zcl_prvd_api_helper,
          mv_tenant        TYPE zprvdtenantid,
          mo_http_client   TYPE REF TO if_http_client,
          mo_vault_api     TYPE REF TO zcl_prvd_vault,
          mv_vault_api_url TYPE string.
  PRIVATE SECTION.
    METHODS: get_vault_client RETURNING VALUE(ro_vault_client) TYPE REF TO zcl_prvd_vault.
ENDCLASS.



CLASS zcl_prvd_vault_helper IMPLEMENTATION.


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
