*&---------------------------------------------------------------------*
*& Include zprvd_shuttleidoc_f01
*&---------------------------------------------------------------------*

CLASS lcl_idoc_moni DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
      baseline_idocs.
    DATA: lo_idochlpr TYPE REF TO  zif_prvd_blidochlper.
  PROTECTED SECTION.
    DATA: lt_poebeln TYPE zif_prvd_blidochlper=>tty_r_ebeln,
          lt_idocnum TYPE zif_prvd_blidochlper=>tty_r_idocnum,
          lt_idoctyp TYPE zif_prvd_blidochlper=>tty_r_idoctype.

  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_idoc_moni IMPLEMENTATION.
  METHOD constructor.
    me->lo_idochlpr = NEW zcl_prvd_idochlpr( iv_tenant          = p_tenant
                                               iv_subject_acct_id = p_sbjact
                                               iv_workgroup_id    = p_wrkgrp ).
    MOVE-CORRESPONDING  s_ebeln[] TO lt_poebeln[].
    MOVE-CORRESPONDING s_idoc[] TO lt_idocnum[].
    MOVE-CORRESPONDING s_ityp[] TO lt_idoctyp[].
  ENDMETHOD.
  METHOD baseline_idocs.
  lo_idochlpr->shuttle_idocs( it_idoctype   = lt_idoctyp
                              it_idocnum    = lt_idocnum
                              it_ebeln      = lt_poebeln
                              iv_direct     = p_dir
                              iv_idocstatus = p_stat
                              iv_idocmestyp = p_mestyp
                              iv_idoctp     = p_idoctp ).
  ENDMETHOD.
ENDCLASS.
*** INCLUDE ZPROUBC_SHUTTLEIDOC_EXMPL_F01
*** INCLUDE ZPROUBC_SHUTTLEIDOC_EXMPL_F01
