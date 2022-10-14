*&---------------------------------------------------------------------*
*& Report zproubc_vault_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zproubc_vault_test.

INCLUDE zproubc_vault_test_top.
INCLUDE zproubc_vault_test_f01.

DATA: lo_vault_helper TYPE REF TO zcl_proubc_vault_helper.

START-OF-SELECTION.
  CALL SCREEN 100.
  CREATE OBJECT lo_vault_helper.

  lo_vault_helper->setup_protocol_msg( ).
  "lo_vault_helper->setup_vault_msgs( ). "should it be this one or that one?

  CASE abap_true.
    WHEN rb1.
      lo_vault_helper->list_vaults( ).
    WHEN rb2.
      lo_vault_helper->create_vault( ).
    WHEN rb3.
      "lo_vault_helper->unseal_vault
    WHEN rb4.
      "lo_vault_helper->delete_vault
    WHEN rb5.
      lo_vault_helper->create_key( ).
    WHEN rb6.
      "lo_vault_helper->unseal_key
    WHEN rb7.
      lo_vault_helper->delete_keys( ).
    WHEN rb8.
    WHEN rb9.
    WHEN rb10.
  ENDCASE.
*lo_vault_helper->list_keys( ).
*lo_vault_helper->create_key( ).
*lo_vault_helper->list_vaults( ).
*lo_vault_helper->create_vault( ).
*lo_vault_helper->derive_key( ).
*lo_vault_helper->encrypt( ).
*lo_vault_helper->decrypt(  ).
*lo_vault_helper->sign( ).
*lo_vault_helper->verify( ).

MODULE pbo OUTPUT.
ENDMODULe.

module pai input.
ENDMODULE.
