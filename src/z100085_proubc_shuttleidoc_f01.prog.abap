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
    MOVE-CORRESPONDING r_poebeln[] TO lt_poebeln[].
    MOVE-CORRESPONDING r_idocnum[] TO lt_idocnum[].
    MOVE-CORRESPONDING r_idoctype[] TO lt_idoctyp[].



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
