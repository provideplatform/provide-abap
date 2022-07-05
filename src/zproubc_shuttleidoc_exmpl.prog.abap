*&---------------------------------------------------------------------*
*& Report zproubc_shuttleidocex
*&---------------------------------------------------------------------*
*& proUBC iDoc Baseline Example program
*&---------------------------------------------------------------------*
REPORT zproubc_shuttleidocex.

INCLUDE ZPROUBC_SHUTTLEIDOC_EXMPL_TOP.
*INCLUDE z100085_proubc_shuttleidoc_top.
INCLUDE ZPROUBC_SHUTTLEIDOC_EXMPL_F01.
*INCLUDE z100085_proubc_shuttleidoc_f01.

INITIALIZATION.

START-OF-SELECTION.

  DATA: lo_idoc_moni TYPE REF TO lcl_idoc_moni.

  lo_idoc_moni = NEW lcl_idoc_moni( ).
  lo_idoc_moni->baseline_idocs( ).

  "WRITE: 'idocs baselined!'.
  "todo write messages to spool
