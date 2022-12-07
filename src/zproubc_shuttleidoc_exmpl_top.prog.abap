*&---------------------------------------------------------------------*
*& Include zproubc_shuttleidoc_top
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-001.
PARAMETERS: cb_test  AS CHECKBOX DEFAULT '',
            p_dir    TYPE edidc-direct DEFAULT '1',
            p_stat   TYPE edids-status DEFAULT '03',
            p_mestyp TYPE edidc-mestyp DEFAULT 'ORDERS',
              p_idoctp TYPE edidc-idoctp DEFAULT 'ORDERS05',
              p_tenant TYPE zprvdtenantid,
              p_sbjact TYPE zprvdtenantid,
              p_wrkgrp TYPE zprvdtenantid.
TYPES: r_poebeln LIKE RANGE OF ekko-ebeln,
          r_idocnum LIKE RANGE OF edidc-docnum,
          r_idoctype LIKE RANGE OF edidc-idoctp. "## default to ORDERS05
SELECT-OPTIONS:
          s_ebeln FOR r_poebeln,
          s_idoc FOR r_idocnum,
          s_ityp FOR r_idoctype.
SELECTION-SCREEN END OF BLOCK 1.
