*&---------------------------------------------------------------------*
*& Include zprvd_shuttleidoc_top
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
  TYPES: r_poebeln  TYPE RANGE OF ekko-ebeln,
         r_idocnum  LIKE RANGE OF edidc-docnum,
         r_idoctype LIKE RANGE OF edidc-idoctp. "## default to ORDERS05

  TYPES: BEGIN OF ty_ebeln_range,
           sign   TYPE ddsign,
           option TYPE ddoption,
           low    TYPE ebeln,
           high   TYPE ebeln,
         END OF ty_ebeln_range,
         BEGIN OF ty_idocnum_range,
           sign   TYPE ddsign,
           option TYPE ddoption,
           low    TYPE edidc-docnum,
           high   TYPE edidc-docnum,
         END OF ty_idocnum_range,
         BEGIN OF ty_idoctype_range,
           sign   TYPE ddsign,
           option TYPE ddoption,
           low    TYPE edidc-idoctp,
           high   TYPE edidc-idoctp,
         END OF ty_idoctype_range.
  DATA: lr_ebeln_range TYPE ty_ebeln_range,
        lr_idocnum_range type ty_idocnum_range,
        lr_idoctype_range type ty_idoctype_range.
  SELECT-OPTIONS:
            s_ebeln FOR lr_ebeln_range,
            s_idoc FOR lr_idocnum_range,
            s_ityp FOR lr_idoctype_range .
SELECTION-SCREEN END OF BLOCK 1.
