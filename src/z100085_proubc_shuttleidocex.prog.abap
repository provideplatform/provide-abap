*&---------------------------------------------------------------------*
*& Report z100085_proubc_shuttleidocex
*&---------------------------------------------------------------------*
*& proUBC iDoc Baseline Example program
*&---------------------------------------------------------------------*
REPORT z100085_proubc_shuttleidocex.

selection-SCREEN BEGIN OF BLOCK 1.
RANGES: r_poebeln for ekko-ebeln,
        r_idocnum FOR edidc-docnum,
        r_idoctype FOR edidc-idoctp. "default to ORDERS05
selection-screen end OF BLOCK 1.

CLASS lcl_idoc_moni DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
      baseline_idocs.
    DATA: lo_idochlpr TYPE REF TO  Z100085_ZIF_PROUBC_BLIDOCHLPER.
  PROTECTED SECTION.
    data: lt_poebeln type z100085_zif_proubc_blidochlper=>tty_r_ebeln,
          lt_idocnum type z100085_zif_proubc_blidochlper=>tty_r_idocnum,
          lt_idoctyp type z100085_zif_proubc_blidochlper=>tty_r_idoctype.

  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_idoc_moni IMPLEMENTATION.
  METHOD constructor.
    me->lo_idochlpr = NEW z100085_zcl_proubc_idochlpr( ).
    MOVE-CORRESPONDING r_poebeln[] to lt_poebeln[].
    MOVE-CORRESPONDING r_idocnum[] to lt_idocnum[].
    MOVE-CORRESPONDING r_idoctype[] to lt_idoctyp[].

    lo_idochlpr->get_idocs( exporting it_idoctype = lt_idoctyp
                                      it_idocnum = lt_idocnum
                                      it_ebeln = lt_poebeln ).

  ENDMETHOD.
  METHOD baseline_idocs.

  ENDMETHOD.
ENDCLASS.

INITIALIZATION.

START-OF-SELECTION.

data: lo_idoc_moni type ref to lcl_idoc_moni.

lo_idoc_moni = new lcl_idoc_moni( ).
