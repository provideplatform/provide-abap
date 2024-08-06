CLASS zcl_prvd_baseline DEFINITION
  PUBLIC
  CREATE PUBLIC .

* Baseline API, v1.0.0
  PUBLIC SECTION.

    INTERFACES zif_prvd_baseline .

    "! Method to return PRVD Baseline Proxy class object
    METHODS constructor
      IMPORTING
        !ii_client        TYPE REF TO if_http_client
        !iv_bpitenant_url TYPE string
        !iv_bpitoken      TYPE zprvdrefreshtoken .
  PROTECTED SECTION.
    DATA mi_client TYPE REF TO if_http_client.
    DATA mv_bpitenant_url TYPE string.
    DATA authtoken TYPE zprvdrefreshtoken.
    DATA bpitoken TYPE zprvdrefreshtoken.
    METHODS send_receive RETURNING VALUE(rv_code) TYPE i.
    METHODS set_bearer_token IMPORTING iv_tokenstring TYPE string.
    METHODS get_bearer_token
      RAISING cx_static_check.
    METHODS set_bpi_token IMPORTING iv_tokenstring TYPE zprvdrefreshtoken.
    METHODS get_bpi_token
      RAISING cx_static_check.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_prvd_baseline IMPLEMENTATION.


  METHOD constructor.
    mi_client = ii_client.
    mv_bpitenant_url = iv_bpitenant_url.
  ENDMETHOD.


  METHOD get_bearer_token.
    DATA lv_bearertoken TYPE string.
    "todo check auth token is valid (not empty or expired)
    CONCATENATE 'Bearer' authtoken INTO lv_bearertoken SEPARATED BY space.
    mi_client->request->set_header_field( name  = 'Authorization' value = lv_bearertoken ).
  ENDMETHOD.


  METHOD get_bpi_token.
    DATA lv_bearertoken TYPE string.
    "todo check auth token is valid (not empty or expired)
    CONCATENATE 'Bearer' bpitoken INTO lv_bearertoken SEPARATED BY space.
    mi_client->request->set_header_field( name  = 'Authorization' value = lv_bearertoken ).
  ENDMETHOD.


  METHOD send_receive.
    mi_client->send( EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2 ).
    IF sy-subrc = 0.
      mi_client->receive( EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3 ).
      IF sy-subrc NE 0.
        rv_code = 500.
      ENDIF.
    ELSE.
      rv_code = 500.
    ENDIF.
    mi_client->response->get_status( IMPORTING code = rv_code ).
  ENDMETHOD.


  METHOD set_bearer_token.
    "todo add method implementation to retrive auth token from refresh token
    authtoken = iv_tokenstring.
  ENDMETHOD.


  METHOD set_bpi_token.
    "todo add method implementation to retrive auth token from refresh token
    bpitoken = iv_tokenstring.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listwellknownkeys.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/.well-known/keys'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listwallets.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/wallets'.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listwalletaccounts.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/wallets/{id}/accounts'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listvaults.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/vaults'.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listusers.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/users'.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        RETURN.

      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listtransactions.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/transactions'.
    lv_temp = filter_contract_creations.
    CONDENSE lv_temp.
    IF filter_contract_creations IS SUPPLIED.
      mi_client->request->set_form_field( name = 'filter_contract_creations'
                                         value = lv_temp ).
    ENDIF.
    IF status IS SUPPLIED.
      mi_client->request->set_form_field( name = 'status'
                                         value = status ).
    ENDIF.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listtokens.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/tokens'.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        RETURN.
        " application/json,#/components/schemas/response_listtokens

      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listsecrets.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/vaults/{id}/secrets'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listorganizations.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/organizations'.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listoracles.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/oracles'.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listopenidconfiguration.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/.well-known/openid-configuration'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listnetworks.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/networks'.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = public.
    CONDENSE lv_temp.
    IF public IS SUPPLIED.
      mi_client->request->set_form_field( name = 'public'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listkeys.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/vaults/{id}/keys'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listcontracts.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/contracts'.
    lv_temp = filter_tokens.
    CONDENSE lv_temp.
    IF filter_tokens IS SUPPLIED.
      mi_client->request->set_form_field( name = 'filter_tokens'
                                         value = lv_temp ).
    ENDIF.
    IF sort IS SUPPLIED.
      mi_client->request->set_form_field( name = 'sort'
                                         value = sort ).
    ENDIF.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listconnectors.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/connectors'.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = public.
    CONDENSE lv_temp.
    IF public IS SUPPLIED.
      mi_client->request->set_form_field( name = 'public'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " OK
        " application/json,#/components/schemas/response_listconnectors

      WHEN 422.
        " Unprocessable Entity
        " application/json,#/components/schemas/Error

        " todo, raise
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listcircuits.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/circuits'.
    IF curve IS SUPPLIED.
      mi_client->request->set_form_field( name = 'curve'
                                         value = curve ).
    ENDIF.
    IF identifier IS SUPPLIED.
      mi_client->request->set_form_field( name = 'identifier'
                                         value = identifier ).
    ENDIF.
    IF provider IS SUPPLIED.
      mi_client->request->set_form_field( name = 'provider'
                                         value = provider ).
    ENDIF.
    IF proving_scheme IS SUPPLIED.
      mi_client->request->set_form_field( name = 'proving scheme'
                                         value = proving_scheme ).
    ENDIF.
    IF status IS SUPPLIED.
      mi_client->request->set_form_field( name = 'status'
                                         value = status ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listaccounts.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/accounts'.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200. " OK
      WHEN 401.
        " todo, raise
      WHEN 500.
        " todo, raise
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listworkgroups.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/workgroups'.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
        " application/json,#/components/schemas/response_listworkgroups
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~listworkgroupusers.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/workgroups/{id}/users'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    lv_temp = page.
    CONDENSE lv_temp.
    IF page IS SUPPLIED.
      mi_client->request->set_form_field( name = 'page'
                                         value = lv_temp ).
    ENDIF.
    lv_temp = rpp.
    CONDENSE lv_temp.
    IF rpp IS SUPPLIED.
      mi_client->request->set_form_field( name = 'rpp'
                                         value = lv_temp ).
    ENDIF.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
        " application/json,#/components/schemas/response_listworkgroupusers

      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~prove.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/circuits/{id}/prove'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/ProveRequest
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 422.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~retrievesecret.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/vaults/{id}/secrets/{secret_id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    lv_temp = secret_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{secret_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~revoketoken.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/tokens/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~send_bpiobjects_msg.
    "old version. probably not needed
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/objects'.
    DATA: lv_requeststr  TYPE string,
          lv_responsestr TYPE string.
    DATA lv_protocolmsg TYPE REF TO data.

    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bpi_token( ).

    zcl_prvd_api_helper=>copy_data_to_ref( EXPORTING is_data = body
                                              CHANGING cr_data = lv_protocolmsg ).

    lv_requeststr = /ui2/cl_json=>serialize( data =  lv_protocolmsg
                                      pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata( data = lv_requeststr ).

    lv_code = send_receive( ).
    statuscode = lv_code.
    lv_responsestr = mi_client->response->get_cdata( ).
    apiresponsestr = lv_responsestr.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = lv_responsestr
      CHANGING
        data             = apiresponse
    ).
    CASE lv_code.
      WHEN 202.
        " The request was successful
      WHEN 404.
        " may be more than one reason for this...
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_prvd_baseline~send_protocol_msg.
*https://gist.github.com/kthomas/459381e98c808febea9c1bb51408bbde
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/api/v1/protocol_messages'.
    DATA: lv_requeststr  TYPE string,
          lv_responsestr TYPE string.
    DATA lv_protocolmsg TYPE REF TO data.

    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    IF bpitoken IS INITIAL.
      set_bpi_token( EXPORTING iv_tokenstring = iv_bpitoken ).
    ENDIF.
    get_bpi_token( ).

    zcl_prvd_api_helper=>copy_data_to_ref( EXPORTING is_data = iv_body
                                              CHANGING cr_data = lv_protocolmsg ).

    lv_requeststr = /ui2/cl_json=>serialize( data        = lv_protocolmsg
                                             pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata( data =  lv_requeststr ).

    lv_code = send_receive( ).
    ev_statuscode = lv_code.
    lv_responsestr = mi_client->response->get_cdata( ).
    ev_apiresponsestr = lv_responsestr.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_responsestr
      CHANGING
        data = ev_apiresponse ).
    CASE lv_code.
      WHEN 202.
        " The request was successful
      WHEN 401.
        " check if correct token was provided or was expired
      WHEN 404.
        " may be more than one reason for this...
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_prvd_baseline~status.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/status'.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    lv_code = send_receive( ).
    statuscode = lv_code.
  ENDMETHOD.


  METHOD zif_prvd_baseline~storesecret.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/vaults/{id}/secrets'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/Secret
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201.
        " The request was successful and a new entity was created
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~tokenauthorization.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/tokens'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/Token
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201.
        " Request successfully authorized a `Token`
        " application/json,#/components/schemas/Token

      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~unsealvault.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/unseal'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/UnsealVaultRequest
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 204.
        " The request was successful but did not return a response
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~updatebaselinebusinessobject.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/business_objects/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/BusinessObject
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~updatenetwork.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/networks/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/Network
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 204.
        " The request was successful but did not return a response
        " application/json,#/components/responses/NoContent
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~updateoracle.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/oracles/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~updateorganizationdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/organizations/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/Organization
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~updateuser.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/users/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/User
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
        " application/json,#/components/schemas/User

      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~updateworkgroup.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/workgroups/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'PUT' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/Workgroup
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 204.
        " The request was successful but did not return a response
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~verify.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/circuits/{id}/verify'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/VerifyProofRequest
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~getworkgroupdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/workgroups/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~associateworkgroupuser.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/workgroups/{id}/users'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
* todo, set body, #/components/schemas/AssociateWorkgroupUserRequest
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201.
        " The request was successful and a new entity was created
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~authentication.

    "TODO add the code for basic auth - this code works for /tokens but not /authenticate
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/authenticate'.
    DATA lv_authpayload TYPE zif_prvd_ident=>authorize_access_refreshtoken.
    DATA lv_longtermrequestdata TYPE REF TO data.
    DATA lv_requeststr TYPE string.

    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    lv_authpayload-scope = 'offline_access'.
    lv_authpayload-organization_id = iv_tenantid.

    zcl_prvd_api_helper=>copy_data_to_ref( EXPORTING is_data = lv_authpayload
                                              CHANGING cr_data = lv_longtermrequestdata ).

    lv_requeststr = /ui2/cl_json=>serialize( data        = lv_longtermrequestdata
                                             pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata( data = lv_requeststr ).


    set_bearer_token( EXPORTING iv_tokenstring = body ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201. "## Created
        " application/json,#/components/schemas/AuthenticationResponse

      WHEN 401.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~bearerauthentication.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/tokens'.
    DATA lv_authpayload TYPE zif_prvd_ident=>authorize_access_refreshtoken.
    DATA lv_longtermrequestdata TYPE REF TO data.
    DATA lv_requeststr TYPE string.
    DATA lv_authresponsestr TYPE string.
    DATA lv_bpiauthreq      TYPE zif_prvd_baseline=>authenticationrequest.
    FIELD-SYMBOLS: <fs_bpiauthreq>  TYPE any,
                   <fs_bpiauthreq2> TYPE string.

    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    lv_authpayload-scope = 'offline_access'.
    lv_authpayload-organization_id = iv_tenantid.

    zcl_prvd_api_helper=>copy_data_to_ref( EXPORTING is_data = lv_authpayload
                                             CHANGING cr_data  = lv_longtermrequestdata  ).

    lv_requeststr = /ui2/cl_json=>serialize( EXPORTING data        = lv_longtermrequestdata
                                                       pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata( data = lv_requeststr ).

    set_bearer_token( iv_tokenstring = body ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    "
    code = lv_code.
    CASE lv_code.
      WHEN 201. " Created
        lv_authresponsestr = mi_client->response->get_cdata( ).
        /ui2/cl_json=>deserialize( EXPORTING json = lv_authresponsestr CHANGING data =  apiresponse ).
        ASSIGN apiresponse->* TO FIELD-SYMBOL(<ls_data>).
        ASSIGN COMPONENT 'ACCESS_TOKEN' OF STRUCTURE <ls_data> TO <fs_bpiauthreq>.
        IF sy-subrc = 0.
          ASSIGN <fs_bpiauthreq>->* TO <fs_bpiauthreq2>.
          lv_bpiauthreq = <fs_bpiauthreq2>.
        ENDIF.
      WHEN 401.
        " todo, raise authorization failure
      WHEN 404.
        "check URI, BPI tenant
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~createaccount.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/accounts'.
    DATA lv_bearertoken TYPE string.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/Account
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201.
        " The request was successful and a new `Account` was created
      WHEN 404 OR 403.
        " todo, raise
      WHEN 500.
        " todo, raise
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~createbaselinebusinessobject.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/business_objects'.
    DATA: lv_requeststr  TYPE string,
          lv_responsestr TYPE string.
    DATA lv_busobjmsg TYPE REF TO data.

    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                         value  = lv_uri ).
    get_bpi_token( ).

    zcl_prvd_api_helper=>copy_data_to_ref( EXPORTING is_data = body
                                              CHANGING cr_data = lv_busobjmsg ).

    lv_requeststr = /ui2/cl_json=>serialize( data        = lv_busobjmsg
                                             pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    mi_client->request->set_cdata(
      EXPORTING
        data = lv_requeststr
    ).

    lv_code        = send_receive( ).
    statuscode     = lv_code.
    lv_responsestr = mi_client->response->get_cdata( ).
    apiresponsestr = lv_responsestr.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_responsestr
      CHANGING
        data = apiresponse
    ).
    
    CASE lv_code.
      WHEN 202.
        " Accepted
        " application/json,#/components/responses/Accepted
        " todo, raise
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " The specified resource was not found.
      WHEN 407.
        "check strust
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~createcircuit.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/circuits'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/Circuit
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
        "# The request was successful
      WHEN 200.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~createconnector.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/connectors'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
* todo, set body, #/components/schemas/Connector
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
        " The request was successful and a new entity was created
      WHEN 201.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~createkey.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/vaults/{id}/keys'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
* todo, set body, #/components/schemas/Key
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201.
        " The request was successful and a new entity was created
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~createnetwork.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/networks'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/Network
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201.
        " The request was successful and a new entity was created
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~createoracle.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/oracles'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~createorganization.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/organizations'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
* todo, set body, #/components/schemas/Organization
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201.
        RETURN.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~createtransaction.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/transactions'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
* todo, set body, #/components/schemas/Transaction
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201.
        " The request was successful and a new entity was created
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~createuser.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/users'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/User
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201.
        " User created
        " application/json,#/components/schemas/User
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~createvault.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/vaults'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
* todo, set body, #/components/schemas/Vault
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201.
        " The request was successful and a new entity was created
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~createwallet.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/wallets'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
* todo, set body, #/components/schemas/Wallet
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201.
        " The request was successful and a new entity was created
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~createworkgroup.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/workgroups'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
* todo, set body, #/components/schemas/Workgroup
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
        " application/json,#/components/schemas/Workgroup

      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~deleteakey.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/vaults/{id}/keys/{key_id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    lv_temp = key_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{key_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 204.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~deleteconnector.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/connectors/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 204.
        " The request was successful but did not return a response.
      WHEN 401.
        " todo, raise
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~deleteoracle.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/oracles/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~deletesecret.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/vaults/{id}/secrets/{secret_id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    lv_temp = secret_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{secret_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name  = '~request_uri'
                                          value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 204.
        " The request was successful but did not return a response
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~deleteuserrequest.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/users/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " Target `User` deleted
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~deleteworkgroup.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/workgroups/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'DELETE' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~deploycontract.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/contracts'.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/Contract
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201.
        " The request was successful and a new entity was created
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 422. " Unprocessable Entity.
        " application/json,#/components/schemas/Error

        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~deriveakeyrequest.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/vaults/{id}/keys/{key_id}/derive'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    lv_temp = key_id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{key_id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/Key
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 201.
        " The request was successful and a new entity was created
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~executecontract.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/contracts/{id}/execute'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'POST' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
* todo, set body, #/components/schemas/ExecuteContractRequest
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 202.
        RETURN.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~getaccountdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/accounts/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " OK
        " application/json,#/components/schemas/Account

      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~getconnectordetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/connectors/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~getcontractdetail.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/contracts/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~getloadbalancerdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/connectors/{id}/load_balancers'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~getnetworkstatus.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/networks/{id}/status'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~getoracledetail.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/oracles/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~getorganizationdetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/organizations/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~getstorevalue.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/circuits/{id}/store/{index}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    lv_temp = index.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{index}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~gettransactiondetails.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/transactions/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
        " todo catch generically
    ENDCASE.
  ENDMETHOD.


  METHOD zif_prvd_baseline~getuserdetail.
    DATA lv_code TYPE i.
    DATA lv_temp TYPE string.
    DATA lv_uri TYPE string VALUE '/users/{id}'.
    lv_temp = id.
    lv_temp = cl_http_utility=>escape_url( condense( lv_temp ) ).
    REPLACE ALL OCCURRENCES OF '{id}' IN lv_uri WITH lv_temp.
    mi_client->request->set_method( 'GET' ).
    mi_client->request->set_header_field( name = '~request_uri'
                                         value = lv_uri ).
    get_bearer_token( ).
    lv_code = send_receive( ).
    
    CASE lv_code.
      WHEN 200.
        " The request was successful
      WHEN 401.
        " todo, raise
      WHEN 403.
        " todo, raise
      WHEN 404.
        " todo, raise
      WHEN 503.
        " todo, raise
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
