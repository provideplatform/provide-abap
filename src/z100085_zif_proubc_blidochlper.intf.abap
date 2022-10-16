INTERFACE z100085_zif_proubc_blidochlper
  PUBLIC .

  TYPES: BEGIN OF ty_r_idoctype,
           sign   TYPE ddsign,
           option TYPE ddoption,
           low    TYPE edidc-idoctp,
           high   TYPE edidc-idoctp,
         END OF ty_r_idoctype.
  TYPES: tty_r_idoctype TYPE TABLE OF ty_r_idoctype.

  TYPES: BEGIN OF ty_r_idocdocnum,
           sign   TYPE ddsign,
           option TYPE ddoption,
           low    TYPE edidc-docnum,
           high   TYPE edidc-docnum,
         END OF ty_r_idocdocnum.
  TYPES: tty_r_idocnum TYPE TABLE OF ty_r_idocdocnum.

  TYPES: BEGIN OF ty_r_ebeln,
           sign   TYPE ddsign,
           option TYPE ddoption,
           low    TYPE ekko-ebeln,
           high   TYPE ekko-ebeln,
         END OF ty_r_ebeln.
  TYPES: tty_r_ebeln TYPE TABLE OF ty_r_ebeln.

  TYPES: BEGIN OF ty_proubc_idocs,
           "object_id  TYPE z100085_bpiobj-object_id,
           idocnum    TYPE edidc-docnum,
           idoctp     TYPE edidc-idoctp,
           idocstat   TYPE edidc-status,
           createdate TYPE edidc-credat,
           createtim  TYPE edidc-cretim,
           upddate    TYPE edidc-upddat,
           updtime    TYPE edidc-updtim,
         END OF ty_proubc_idocs.
  TYPES: tty_proubc_idocs TYPE TABLE OF ty_proubc_idocs.

  TYPES: BEGIN OF ty_proubc_json_idoc_seg,
           segmentid   TYPE string,
           segmenttype TYPE string,
         END OF ty_proubc_json_idoc_seg.

  "build out tree structure from parent level down

  "Default - add the control record

  "Segment type
  "Min occurs
  "Max occurs
  "Parent segment
  "Child segments
  "Description

  TYPES: BEGIN OF ty_idoc_segment_field,
           segmenttype      TYPE string,
           position         TYPE i,
           fieldname        TYPE string,
           fielddescription TYPE string,
           length           TYPE i,
         END OF ty_idoc_segment_field.

  TYPES: BEGIN OF ty_idoc_segment,
           segmenttype   TYPE string,
           minoccurs     TYPE i,
           maxoccurs     TYPE i,
           parentsegment TYPE ref to data,
           childsegments TYPE ref to data,
           description   TYPE string,
           fields        TYPE STANDARD TABLE OF ty_idoc_segment_field WITH NON-UNIQUE DEFAULT KEY,
         END OF ty_idoc_segment.


  "**** Field-level data ****
  "Position - sort the fields by this
  "## segmentstruct/field_attrib/position
  "Fieldname
  "## segmentstruct/fieldname
  "Field description
  "## segmentstruct/field_attrib/descrp
  "ABAP Dictionary type
  "Length
  " ## segmentstruct/field_attrib/intlen
  "Decimals ~ only applies to some numeric types
  "## segmentstruct/field_attrib/decimals

  "Bonus field level data (future enh)
  "value helper table
  "## segmentstruct/field_attrib/valuetab
  "offset ~ used for EDI-native handling



  METHODS shuttle_idocs IMPORTING it_idoctype   TYPE tty_r_idoctype
                                  it_idocnum    TYPE tty_r_idocnum
                                  it_ebeln      TYPE tty_r_ebeln
                                  iv_direct     TYPE edidc-direct
                                  Iv_idocstatus TYPE edidc-status
                                  iv_idocmestyp TYPE edidc-mestyp
                                  iv_idoctp     TYPE edidc-idoctp.

  METHODS mock_shuttle_idocs IMPORTING it_idoctype   TYPE tty_r_idoctype
                                       it_idocnum    TYPE tty_r_idocnum
                                       it_ebeln      TYPE tty_r_ebeln
                                       iv_direct     TYPE edidc-direct
                                       Iv_idocstatus TYPE edidc-status
                                       iv_idocmestyp TYPE edidc-mestyp
                                       iv_idoctp     TYPE edidc-idoctp.


ENDINTERFACE.
