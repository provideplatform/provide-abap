interface ZIF_PRVD_BLIDOCHLPER
  public .


  types:
    BEGIN OF ty_r_idoctype,
      sign   TYPE ddsign,
      option TYPE ddoption,
      low    TYPE edidc-idoctp,
      high   TYPE edidc-idoctp,
    END OF ty_r_idoctype .
  types:
    tty_r_idoctype TYPE TABLE OF ty_r_idoctype .
  types:
    BEGIN OF ty_r_idocdocnum,
      sign   TYPE ddsign,
      option TYPE ddoption,
      low    TYPE edidc-docnum,
      high   TYPE edidc-docnum,
    END OF ty_r_idocdocnum .
  types:
    tty_r_idocnum TYPE TABLE OF ty_r_idocdocnum .
  types:
    BEGIN OF ty_r_ebeln,
      sign   TYPE ddsign,
      option TYPE ddoption,
      low    TYPE string,
      high   TYPE string,
    END OF ty_r_ebeln .
  types:
    tty_r_ebeln TYPE TABLE OF ty_r_ebeln .
  types:
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
  types:
    tty_proubc_idocs TYPE TABLE OF ty_proubc_idocs .
  types:
    BEGIN OF ty_idoc_segment_field,
           segmenttype      TYPE string,
           position         TYPE i,
           fieldname        TYPE string,
           fielddescription TYPE string,
           length           TYPE i,
         END OF ty_idoc_segment_field .
  types:
    tty_idoc_segment_field TYPE STANDARD TABLE OF ty_idoc_segment_field .
  types:
    BEGIN OF ty_idoc_segment,
           segment_type   TYPE string,
           minoccurs     TYPE i,
           maxoccurs     TYPE i,
           parentsegment TYPE REF TO data,
           childsegments TYPE REF TO data,
           description   TYPE string,
           fields        TYPE STANDARD TABLE OF ty_idoc_segment_field WITH NON-UNIQUE DEFAULT KEY,
         END OF ty_idoc_segment .
  types:
    BEGIN OF ty_idoc_xmllist,
      object_id TYPE zbpiobj-object_id,
      idocnum   TYPE edidc-docnum,
      idoc      TYPE REF TO cl_idoc_xml1,
    END OF ty_idoc_xmllist .
  types:
    tty_idoc_xmllist TYPE TABLE OF ty_idoc_xmllist .

  "! Method to initiate PRVD Baseline zk proof creation for selection of iDocs
  methods SHUTTLE_IDOCS
    importing
      !IT_IDOCTYPE type TTY_R_IDOCTYPE
      !IT_IDOCNUM type TTY_R_IDOCNUM
      !IT_EBELN type TTY_R_EBELN
      !IV_DIRECT type EDIDC-DIRECT
      !IV_IDOCSTATUS type EDIDC-STATUS
      !IV_IDOCMESTYP type EDIDC-MESTYP
      !IV_IDOCTP type EDIDC-IDOCTP .
endinterface.
