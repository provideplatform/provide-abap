INTERFACE zif_proubc_unittests
  PUBLIC .

  "! Ident reachability
  METHODS: is_ident_reachable RETURNING VALUE(rv_result) TYPE abap_bool.
  "! Ident authentication
  METHODS: is_ident_auth_ok RETURNING VALUE(rv_result) TYPE abap_bool.
  "! BPI reachability
  METHODS: is_bpi_reachable RETURNING VALUE(rv_result) TYPE abap_bool.
  "! Sending a protocol message
  METHODS: is_protocol_message_ok RETURNING VALUE(rv_result) TYPE abap_bool.
  "! general http expected check
  METHODS: is_expected_http_status IMPORTING i_expected_stat TYPE i
                                   RETURNING VALUE(rv_result)   TYPE abap_bool.

ENDINTERFACE.
