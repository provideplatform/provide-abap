INTERFACE zif_proubc_blidochlper
  PUBLIC .


  TYPES:
    BEGIN OF ty_r_idoctype,
      sign   TYPE ddsign,
      option TYPE ddoption,
      low    TYPE edidc-idoctp,
      high   TYPE edidc-idoctp,
    END OF ty_r_idoctype .
  TYPES:
    tty_r_idoctype TYPE TABLE OF ty_r_idoctype .
  TYPES:
    BEGIN OF ty_r_idocdocnum,
      sign   TYPE ddsign,
      option TYPE ddoption,
      low    TYPE edidc-docnum,
      high   TYPE edidc-docnum,
    END OF ty_r_idocdocnum .
  TYPES:
    tty_r_idocnum TYPE TABLE OF ty_r_idocdocnum .
  TYPES:
    BEGIN OF ty_r_ebeln,
      sign   TYPE ddsign,
      option TYPE ddoption,
      low    TYPE string, "ekko-ebeln,
      high   TYPE string, "ekko-ebeln,
    END OF ty_r_ebeln .
  TYPES:
    tty_r_ebeln TYPE TABLE OF ty_r_ebeln .
  TYPES:
    BEGIN OF ty_proubc_idocs,
      "object_id  TYPE zbpiobj-object_id,
      idocnum    TYPE edidc-docnum,
      idoctp     TYPE edidc-idoctp,
      idocstat   TYPE edidc-status,
      createdate TYPE edidc-credat,
      createtim  TYPE edidc-cretim,
      upddate    TYPE edidc-upddat,
      updtime    TYPE edidc-updtim,
    END OF ty_proubc_idocs .
  TYPES:
    tty_proubc_idocs TYPE TABLE OF ty_proubc_idocs .
  TYPES:
    BEGIN OF ty_idoc_xmllist,
      object_id TYPE Zbpiobj-object_id,
      idocnum   TYPE edidc-docnum,
      idoc      TYPE REF TO cl_idoc_xml1,
    END OF ty_idoc_xmllist .
  TYPES:
    tty_idoc_xmllist TYPE TABLE OF ty_idoc_xmllist .

  METHODS shuttle_idocs
    IMPORTING
      !it_idoctype   TYPE tty_r_idoctype
      !it_idocnum    TYPE tty_r_idocnum
      !it_ebeln      TYPE tty_r_ebeln
      !iv_direct     TYPE edidc-direct
      !iv_idocstatus TYPE edidc-status
      !iv_idocmestyp TYPE edidc-mestyp
      !iv_idoctp     TYPE edidc-idoctp .
ENDINTERFACE.
