CLASS zcl_proubc_vault_helper DEFINITION
  PUBLIC
  INHERITING FROM zcl_proubc_api_helper
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING  !iv_tenant      TYPE zPRVDTENANTID OPTIONAL,
      create_key,
      derive_key,
      list_keys,
      delete_keys,
      encrypt,
      decrypt,
      sign,
      verify,
      setup_vault_msgs.
  PROTECTED SECTION.
    DATA: lv_tenant        TYPE zprvdtenantid,
          lo_http_client   TYPE REF TO if_http_client,
          lo_vault_api     TYPE REF TO zif_proubc_vault,
          lv_vault_api_url TYPE string.
  PRIVATE SECTION.
    METHODS: get_vault_client RETURNING VALUE(ro_vault_client) TYPE REF TO zif_proubc_vault.
ENDCLASS.



CLASS zcl_proubc_vault_helper IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    lv_vault_api_url = 'https://vault.provide.services'.

    IF iv_tenant IS NOT INITIAL.
      lv_tenant = iv_tenant.
    ELSE.
      lv_tenant = lv_defaulttenant.
    ENDIF.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = lv_vault_api_url
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

    lo_vault_api = NEW zcl_proubc_vault( ii_client = lo_http_client iv_tenant = lv_tenant iv_bpitoken = lv_bpitoken  ).

  ENDMETHOD.
  METHOD create_key.
    lo_vault_api = me->get_vault_client( ).
  ENDMETHOD.
  METHOD derive_key.
  lo_vault_api = me->get_vault_client( ).
  ENDMETHOD.
  METHOD list_keys.
  lo_vault_api = me->get_vault_client( ).
  ENDMETHOD.
  METHOD delete_keys.
  lo_vault_api = me->get_vault_client( ).
  ENDMETHOD.
  METHOD encrypt.
  lo_vault_api = me->get_vault_client( ).
  ENDMETHOD.
  METHOD decrypt.
  lo_vault_api = me->get_vault_client( ).
  ENDMETHOD.
  METHOD sign.
  lo_vault_api = me->get_vault_client( ).
  ENDMETHOD.
  METHOD verify.
  lo_vault_api = me->get_vault_client( ).
  ENDMETHOD.
  METHOD setup_vault_msgs.
  ENDMETHOD.
  METHOD get_vault_client.
    IF lo_vault_api IS BOUND.
      cl_http_client=>create_by_url(
    EXPORTING
      url                = lv_vault_api_url
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

      lo_vault_api = NEW zcl_proubc_vault( ii_client = lo_http_client iv_tenant = lv_tenant iv_bpitoken = lv_bpitoken  ).
      ro_vault_client = lo_vault_api.
    ELSE.
      ro_vault_client = lo_vault_api.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
