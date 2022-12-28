CLASS zcl_prvd_ident_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.
  METHODS: "! Creates instance of the Ident helper class
            constructor,
            "! Creates a PRVD Ident user with the given parameters
            create_ident_user IMPORTING iv_firstname TYPE string
                                         iv_lastname TYPE string
                                         iv_email TYPE string
                                         iv_password TYPE string OPTIONAL,
            "! Creates the PRVD Org to be affiliated to the user
            create_user_organization IMPORTING iv_tenantid TYPE zprvdtenantid
                                                iv_subjacctid TYPE zprvdtenantid
                                                iv_orgname TYPE string.

PROTECTED SECTION.
  DATA: mo_ident_client TYPE REF TO zif_prvd_ident.
PRIVATE SECTION.
ENDCLASS.

CLASS zcl_prvd_ident_helper IMPLEMENTATION.
  METHOD constructor.
    DATA: lv_identurl TYPE string.
    DATA: lo_http_client TYPE REF TO if_http_client,
        lo_ident_api       TYPE REF TO zif_prvd_ident.

    lv_identurl = 'https://ident.provide.services'.
    cl_http_client=>create_by_url(
          EXPORTING
            url                = lv_identurl
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
    lo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client
                                               value = '100' ).

    lo_ident_api = NEW zcl_prvd_ident( ii_client = lo_http_client
                                         iv_tenant = ''
                                         iv_refreshtoken = '' ).
    mo_ident_client = lo_ident_api.
  ENDMETHOD.
  METHOD create_ident_user.
    DATA: ls_create_user_req TYPE zif_prvd_ident=>createuserrequest,
          ls_prvdtenant TYPE zsprvdtenant,
          lt_prvdtenant TYPE zttprvdtenant.

    ls_create_user_req-first_name = iv_firstname.
    ls_create_user_req-last_name = iv_lastname.
    ls_create_user_req-email = iv_email.
    mo_ident_client->createuser( body = ls_create_user_req  ).

    zcl_proubc_prvdtenants=>create_prvdtenant( it_prvdtenant = lt_prvdtenant ).
  ENDMETHOD.
  METHOD create_user_organization.
  ENDMETHOD.
ENDCLASS.
