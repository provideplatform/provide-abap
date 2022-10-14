*&---------------------------------------------------------------------*
*& Report ZPROUBC_SHUTTLEIDOC_EXMPL
*&---------------------------------------------------------------------*
*& proUBC iDoc Baseline Example program
*&---------------------------------------------------------------------*
REPORT zproubc_shuttleidoc_exmpl.

INCLUDE zproubc_shuttleidoc_exmpl_top.
INCLUDE zproubc_shuttleidoc_exmpl_f01.

INITIALIZATION.
GET PARAMETER ID 'ZPRVDTENANT' FIELD p_tenant.
GET PARAMETER ID 'ZPRVDSUBJACCTID' FIELD p_sbjact.

START-OF-SELECTION.

  DATA: lo_idoc_moni TYPE REF TO lcl_idoc_moni.

  lo_idoc_moni = NEW lcl_idoc_moni( ).
  lo_idoc_moni->baseline_idocs( ).
