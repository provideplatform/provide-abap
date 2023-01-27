*"* use this source file for your ABAP unit test classes

CLASS ltc_proubc_api_helper DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    METHODS _01_ident_reachable_success FOR TESTING.
    METHODS _02a_ident_auth_success FOR TESTING.
    METHODS _02b_ident_auth_failure FOR TESTING.
    METHODS _03_bpi_reachable_success FOR TESTING.
    METHODS _04a_protocol_message_success FOR TESTING.
    METHODS _04b_protocol_message_failure FOR TESTING.
  PRIVATE SECTION.
    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS setup.
    METHODS teardown.
    DATA: mo_cut TYPE REF TO zcl_prvd_api_helper.
ENDCLASS.

CLASS ltc_proubc_api_helper IMPLEMENTATION.
  METHOD class_setup.
  ENDMETHOD.
  METHOD class_teardown.
  ENDMETHOD.
  METHOD setup.
  ENDMETHOD.
  METHOD teardown.
  ENDMETHOD.

  METHOD _01_ident_reachable_success.
  ENDMETHOD.
  METHOD _02a_ident_auth_success.
  ENDMETHOD.
  METHOD _02b_ident_auth_failure.
  ENDMETHOD.
  METHOD _03_bpi_reachable_success.
  ENDMETHOD.
  METHOD _04a_protocol_message_success.
  ENDMETHOD.
  METHOD _04b_protocol_message_failure.
  ENDMETHOD.
ENDCLASS.
