*&---------------------------------------------------------------------*
*& Report z100085_proubc_shuttleidocex
*&---------------------------------------------------------------------*
*& proUBC iDoc Baseline Example program
*&---------------------------------------------------------------------*
REPORT z100085_proubc_shuttleidocex.

INCLUDE z100085_proubc_shuttleidoc_top.
INCLUDE z100085_proubc_shuttleidoc_f01.

INITIALIZATION.

START-OF-SELECTION.

  DATA: lo_idoc_moni TYPE REF TO lcl_idoc_moni.

  lo_idoc_moni = NEW lcl_idoc_moni( ).
  lo_idoc_moni->baseline_idocs( ).

  "WRITE: 'idocs baselined!'.
  "todo write messages to spool
