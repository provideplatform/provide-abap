*&---------------------------------------------------------------------*
*& Include z100085_proubc_shuttleidoc_f01
*&---------------------------------------------------------------------*

CLASS lcl_idoc_moni DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
      baseline_idocs.
    DATA: lo_idochlpr TYPE REF TO  z100085_zif_proubc_blidochlper.
  PROTECTED SECTION.
    DATA: lt_poebeln TYPE z100085_zif_proubc_blidochlper=>tty_r_ebeln,
          lt_idocnum TYPE z100085_zif_proubc_blidochlper=>tty_r_idocnum,
          lt_idoctyp TYPE z100085_zif_proubc_blidochlper=>tty_r_idoctype.

  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_idoc_moni IMPLEMENTATION.
  METHOD constructor.
    me->lo_idochlpr = NEW z100085_zcl_proubc_idochlpr( iv_tenant = p_tenant ).
    MOVE-CORRESPONDING  s_ebeln[] TO lt_poebeln[].
    MOVE-CORRESPONDING s_idoc[] TO lt_idocnum[].
    MOVE-CORRESPONDING s_ityp[] TO lt_idoctyp[].
  ENDMETHOD.
  METHOD baseline_idocs.
  lo_idochlpr->shuttle_idocs( EXPORTING it_idoctype = lt_idoctyp
                                      it_idocnum = lt_idocnum
                                      it_ebeln = lt_poebeln
                                      iv_direct = p_dir
                                      iv_idocstatus = p_stat
                                      iv_idocmestyp = p_mestyp
                                      iv_idoctp = p_idoctp ).
  ENDMETHOD.
ENDCLASS.
*** INCLUDE ZPROUBC_SHUTTLEIDOC_EXMPL_F01
*** INCLUDE ZPROUBC_SHUTTLEIDOC_EXMPL_F01
