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
        !io_prvd_api_helper    TYPE REF TO zcl_prvd_api_helper OPTIONAL
        !iv_org_id             TYPE zprvdtenantid OPTIONAL
        !iv_subject_account_id TYPE zprvdtenantid OPTIONAL
        !iv_workgroup_id       TYPE zprvdtenantid OPTIONAL
        !iv_vault_api_url      TYPE string OPTIONAL .
    "! Method to create a key
    METHODS create_key .
    "! Lists the PRVD Vault(s) created for the user
    METHODS list_vaults
      RETURNING VALUE(rt_vault_list) TYPE zif_prvd_vault=>tty_vault_query .
    "! Creates a PRVD Vault for the user
    METHODS create_vault .
    "! Derives a key for
    METHODS derive_key .
    METHODS list_keys IMPORTING iv_vault_id TYPE zprvdvaultid RETURNING VALUE(rt_vault_keys) TYPE zif_prvd_vault=>ty_vault_keys_list.
    "! Deletes the specified user key upon user request
    METHODS delete_keys .
    "! Encrypts data
    METHODS encrypt .
    "! Decrypts data
    METHODS decrypt .
    "! Used to cryptographically sign data
    METHODS sign IMPORTING iv_vault_id type zprvdvaultid
                           is_message type zif_prvd_vault=>ty_signed_message
                 RETURNING VALUE(rs_vault_signed_message) type zif_prvd_vault=>ty_signature .
    "! Used to cryptographically verify data
    METHODS verify .
    "! Initializes other aspects of the vault helper class to ensure connectivity to Vault microservice
    METHODS setup_vault_msgs .
    "! Retrieves the wallet address per Vault data input specs
    METHODS get_wallet_address
      RETURNING
        VALUE(rv_wallet_address) TYPE zprvd_smartcontract_addr .
    "! Retrives the access token
    METHODS get_access_token RETURNING VALUE(rv_access_token) TYPE zprvdrefreshtoken.
  PROTECTED SECTION.
    DATA: mo_prvd_api_helper    TYPE REF TO zcl_prvd_api_helper,
          mv_tenant             TYPE zprvdtenantid,
          mo_http_client        TYPE REF TO if_http_client,
          mo_vault_api          TYPE REF TO zcl_prvd_vault,
          mv_vault_api_url      TYPE string,
          mv_org_id             TYPE zprvdtenantid,
          mv_subject_account_id TYPE zprvdtenantid,
          mv_workgroup_id       TYPE zprvdtenantid,
          mv_prvd_token         TYPE zprvdrefreshtoken.
  PRIVATE SECTION.
    METHODS: get_vault_client RETURNING VALUE(ro_vault_client) TYPE REF TO zcl_prvd_vault.
ENDCLASS.



CLASS zcl_prvd_vault_helper IMPLEMENTATION.


  METHOD constructor.
    DATA: lv_jwt    TYPE REF TO data,
          lv_status TYPE i.
    super->constructor( ).

    IF io_prvd_api_helper IS BOUND.
      mo_prvd_api_helper = io_prvd_api_helper.

    ELSE.
      mo_prvd_api_helper = NEW zcl_prvd_api_helper( iv_tenant          = iv_org_id
                                                      iv_subject_acct_id = iv_subject_account_id
                                                      iv_workgroup_id    = iv_workgroup_id ).
    ENDIF.

    mv_org_id = mo_prvd_api_helper->get_default_tenant( ).
    mv_subject_account_id = mo_prvd_api_helper->get_subject_account( ).
    mv_workgroup_id = mo_prvd_api_helper->get_workgroup( ).

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

    mo_vault_api = NEW zcl_prvd_vault( ii_client   = mo_http_client
                                           iv_tenant   = mv_org_id
                                           iv_bpitoken = mv_prvd_token ).





*    IF io_api_helper IS BOUND.
*      mo_api_helper = io_api_helper.
*    ELSE.
*      mo_api_helper = NEW zcl_prvd_api_helper( iv_tenant          = iv_org_id
*                                                 iv_subject_acct_id = iv_subject_account_id
*                                                 iv_workgroup_id    = iv_workgroup_id ).
*    ENDIF.
*
*    mv_vault_api_url = 'https://vault.provide.services'.
*
*    cl_http_client=>create_by_url(
*      EXPORTING
*        url                = mv_vault_api_url
*      IMPORTING
*        client             = mo_http_client
*      EXCEPTIONS
*        argument_not_found = 1
*        plugin_not_active  = 2
*        internal_error     = 3
*        OTHERS             = 4 ).
*    IF sy-subrc <> 0.
*      " error handling
*    ENDIF.
*
*    mo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.
*    mo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client
*                                               value = '100' ).

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
    DATA: lv_apiresponsestr   TYPE string,
          lv_apiresponse      TYPE REF TO data,
          lv_httpresponsecode TYPE i.
    mo_vault_api = me->get_vault_client( ).
    mo_vault_api->zif_prvd_vault~list_keys(
      EXPORTING
        iv_vault_id            = iv_vault_id
      IMPORTING
        ev_apiresponsestr   = lv_apiresponsestr
        ev_apiresponse      = lv_apiresponse
        ev_httpresponsecode = lv_httpresponsecode
    ).
    if lv_httpresponsecode eq 200.
      /ui2/cl_json=>deserialize(
        EXPORTING
          json             = lv_apiresponsestr
        CHANGING
          data             = rt_vault_keys
      ).
    endif.
*    CATCH cx_static_check.
  ENDMETHOD.


  METHOD list_vaults.
    DATA: lv_apiresponsestr   TYPE string,
          lv_apiresponse      TYPE REF TO data,
          lv_httpresponsecode TYPE i.
    mo_vault_api = me->get_vault_client( ).
    mo_vault_api->zif_prvd_vault~list_vaults(
        IMPORTING
            ev_apiresponsestr   = lv_apiresponsestr
            ev_apiresponse      = lv_apiresponse
            ev_httpresponsecode = lv_httpresponsecode
    ).
    IF lv_httpresponsecode EQ 200.
      /ui2/cl_json=>deserialize(
        EXPORTING
          json             = lv_apiresponsestr
        CHANGING
          data             = rt_vault_list
      ).
    ENDIF.

*    CATCH cx_static_check.
  ENDMETHOD.


  METHOD setup_vault_msgs.
  ENDMETHOD.


  METHOD sign.
      DATA: lv_apiresponsestr   TYPE string,
          lv_apiresponse      TYPE REF TO data,
          lv_httpresponsecode TYPE i.
    mo_vault_api = me->get_vault_client( ).
    mo_vault_api->zif_prvd_vault~sign(
      EXPORTING
        iv_vaultid          = iv_vault_id
        is_message          = is_message
        iv_content_type     = 'json'
      IMPORTING
        ev_apiresponsestr   = lv_apiresponsestr
        ev_apiresponse      = lv_apiresponse
       ev_httpresponsecode = lv_httpresponsecode
    ).
    case lv_httpresponsecode.
        when 201.
        when others.
        "todo log error.
    ENDCASE.
*    CATCH cx_static_check.
  ENDMETHOD.


  METHOD verify.
    mo_vault_api = me->get_vault_client( ).
  ENDMETHOD.

  METHOD get_access_token.
    rv_access_token = mv_prvd_token.
  ENDMETHOD.
ENDCLASS.
