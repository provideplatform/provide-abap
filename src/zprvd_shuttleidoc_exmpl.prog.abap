*&---------------------------------------------------------------------*
*& Report ZPRVD_SHUTTLEIDOC_EXMPL
*&---------------------------------------------------------------------*
*& PRVD Connector iDoc Baseline Example program
*&---------------------------------------------------------------------*
REPORT zprvd_shuttleidoc_exmpl.

INCLUDE ZPRVD_SHUTTLEIDOC_EXMPL_TOP.
INCLUDE ZPRVD_SHUTTLEIDOC_EXMPL_F01.

INITIALIZATION.
  GET PARAMETER ID 'ZPRVDTENANT' FIELD p_tenant.
  GET PARAMETER ID 'ZPRVDSUBJACCTID' FIELD p_sbjact.
  GET PARAMETER ID 'ZPRVDWRKGRPID' FIELD p_wrkgrp.

START-OF-SELECTION.

  DATA: lo_idoc_moni TYPE REF TO lcl_idoc_moni.

  lo_idoc_moni = NEW lcl_idoc_moni( ).
  lo_idoc_moni->baseline_idocs( ).
