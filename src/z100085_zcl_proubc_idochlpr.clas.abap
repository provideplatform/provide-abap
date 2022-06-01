CLASS z100085_zcl_proubc_idochlpr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: z100085_zif_proubc_blidochlper.
    TYPES: tty_edidd TYPE TABLE OF edidd.
    DATA: loaded_idocs   TYPE z100085_zif_proubc_blidochlper=>tty_idoc_xmllist,
          selected_idocs TYPE Z100085_Zif_proubc_blidochlper=>tty_proubc_idocs.

    CLASS-METHODS:
      get_objid IMPORTING iv_schema TYPE string
                          it_edidd  TYPE tty_edidd
                          iv_idoc   TYPE REF TO data
                EXPORTING ev_objid  TYPE z100085_bpiobj-object_id,
      get_DUMMY_objid IMPORTING iv_schema TYPE string
                      EXPORTING ev_objid  TYPE z100085_bpiobj-object_id
                                EV_NEWIDOCNUM TYPE EDIDD-docnum
                      CHANGING  Ct_edidd  TYPE tty_edidd.
    METHODS: idoc_to_json,
      get_idocs_for_object,
      validate_idocstat_to_baseline,
      shuttle_idoc,
      load_idocs.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_idochlpr IMPLEMENTATION.
  METHOD idoc_to_json.
    DATA: lo_api_helper       TYPE REF TO z100085_zcl_proubc_api_helper,
          lo_ident_api        TYPE REF TO z100085_zif_proubc_ident,
          lo_baseline_api     TYPE REF TO z100085_zif_proubc_baseline,
          ls_protocol_msg_req TYPE z100085_zif_proubc_baseline=>protocolmessage_req.



    lo_api_helper = NEW z100085_zcl_proubc_api_helper( ).

    "sets the default tenant and ident/baseline api tokens
    lo_api_helper->setup_protocol_msg( ).


    LOOP AT loaded_idocs ASSIGNING FIELD-SYMBOL(<fs_loaded_idoc>).
      DATA: lv_idoc TYPE REF TO data.
      CLEAR: ls_protocol_msg_req.


      DATA:
        lv_idocnum      TYPE edidc-docnum,
        lt_edids        TYPE TABLE OF edids,
        lt_edidd        TYPE TABLE OF edidd,
        wa_idoc_control TYPE edidc,
        lv_status       TYPE i.

      CLEAR: lt_edids, lt_edidd, lv_idocnum.

      lv_idocnum = <fs_loaded_idoc>-idocnum.
      CALL FUNCTION 'IDOC_READ_COMPLETELY'
        EXPORTING
          document_number = lv_idocnum
        IMPORTING
          idoc_control    = wa_idoc_control
        TABLES
          int_edids       = lt_edids
          int_edidd       = lt_edidd
        EXCEPTIONS
          OTHERS          = 1.

      DATA: lv_idocjson TYPE string.
      lv_idocjson = /ui2/cl_json=>serialize(
         EXPORTING
           data             = lt_edidd
*            compress         =
*            name             =
*            pretty_name      =
*            type_descr       =
*            assoc_arrays     =
*            ts_as_iso8601    =
*            expand_includes  =
*            assoc_arrays_opt =
*            numc_as_string   =
*            name_mappings    =
*            conversion_exits =
*          RECEIVING
*            r_json           =
       ).

      ls_protocol_msg_req-payload = lv_idocjson.
      ls_protocol_msg_req-payload_mimetype = 'json'.
      ls_protocol_msg_req-type = 'ORDERS05'.


      "TODO get the object id (eg PO number) from the idoc
      z100085_zcl_proubc_idochlpr=>get_objid( EXPORTING iv_schema = 'ORDERS05'
                               it_edidd = lt_edidd
                               iv_idoc = lv_idoc
                     IMPORTING ev_objid = ls_protocol_msg_req-id ).

      lo_api_helper->send_protocol_msg( EXPORTING body = ls_protocol_msg_req IMPORTING statuscode = lv_status  ). "should return 202

*    data: lv_jsondata type ref to data.
*
*         " assign lt_edids to lv_jsondata->*.
*         " assign lt_edidd to lv_jsondata->*.



* deserialization try #1
*      <fs_loaded_idoc>-idoc->get_xmldata_as_string( IMPORTING
*       data_string = DATA(lv_idocxml)  ).
*
*      DATA: data_json TYPE REF TO data.
*
*      " Deserialize XML
*      CALL TRANSFORMATION id
*         SOURCE xml       = lv_idocxml
*         RESULT data_node = data_json.

*
*     <fs_loaded_idoc>-idoc->get_xmldata_as_table( importing data_table = data(lt_datatable)
*                                                              len = data(lv_len) ).

      "get access token from ident. This SAP client / SAP system user == a tenant.

      "call baseline API /api/v1/protocolmessage


*https://gist.github.com/kthomas/459381e98c808febea9c1bb51408bbde
*type Message struct {
*    ID              *string          `sql:"-" json:"id,omitempty"`
*    BaselineID      *uuid.UUID       `sql:"-" json:"baseline_id,omitempty"` // don't need this optional; when included, can be used to map outbound message just-in-time
*    Errors          []*api.Error     `sql:"-" json:"errors,omitempty"`
*    MessageID       *string          `sql:"-" json:"message_id,omitempty"` dont need this. but
*    Payload         interface{}      `sql:"-" json:"payload,omitempty"` THE IDOC. need this
*     payload_mimetype 'application/xml'
*m    ProtocolMessage *ProtocolMessage `sql:"-" json:"protocol_essage,omitempty"`. don't need this.
*    Recipients      []*Participant   `sql:"-" json:"recipients"` don't need this
*    Status          *string          `sql:"-" json:"status,omitempty"` don't need this. shuttle gives this back to us.
*    Type            *string          `sql:"-" json:"type,omitempty"`
*}


*essentials
      "id", "payload", "payload_mimetype", "type" (eg. ORDERS05)

      "data_json = lt_datatable.


*      DATA(lv_idocjson) = /ui2/cl_json=>serialize(
*        EXPORTING
*          data             =  data_json
*       compress         =
*       name             =
*       pretty_name      =
*       type_descr       =
*       assoc_arrays     =
*       ts_as_iso8601    =
*       expand_includes  =
*       assoc_arrays_opt =
*       numc_as_string   =
*       name_mappings    =
*       conversion_exits =
*     RECEIVING
*       r_json           =
*      ).

    ENDLOOP.
  ENDMETHOD.
  METHOD get_idocs_for_object.
    " me->z100085_zif_proubc_blidochlper~get_idocs( it_doctype =
    "                                               it_docnum =
    "                                               it_ebeln =  ).
  ENDMETHOD.
  METHOD validate_idocstat_to_baseline.
  ENDMETHOD.
  METHOD shuttle_idoc.
  ENDMETHOD.
  METHOD z100085_zif_proubc_blidochlper~get_idocs.
    "object_id  TYPE z100085_bpiobj-object_id,
    SELECT docnum,
    idoctp,
    status,
    credat,
    cretim,
    upddat,
    updtim
    FROM edidc
    "inner join EDID4 as b on a~docnum = b~docnum
    INTO TABLE @selected_idocs
    WHERE direct = '1'
    AND status = '03'
    AND mestyp = 'ORDERS'
    AND idoctp = 'ORDERS05'.

    me->load_idocs( ).
    me->idoc_to_json(  ).

  ENDMETHOD.
  METHOD load_idocs.
    DATA: wa_loadedidoc TYPE Z100085_Zif_proubc_blidochlper=>ty_idoc_xmllist.
    LOOP AT selected_idocs ASSIGNING FIELD-SYMBOL(<fs_sel_idoc>).
      CLEAR: wa_loadedidoc.
      wa_loadedidoc-idocnum = <fs_sel_idoc>-idocnum.
      "wa_loadedidoc-object_id = <fs_sel_idoc>-object_id.
      DATA: lo_idoc TYPE REF TO cl_idoc_xml1.
      CREATE OBJECT lo_idoc
        EXPORTING
          docnum             = <fs_sel_idoc>-idocnum
        EXCEPTIONS
          error_loading_idoc = 1
          error_building_xml = 2
          OTHERS             = 3.
      IF sy-subrc = 0.
        wa_loadedidoc-idoc = lo_idoc.
        APPEND wa_loadedidoc TO loaded_idocs.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_objid.
    CASE iv_schema.
      WHEN 'ORDERS05'.
        "data record E1EDK01 - BELNR
        DATA: lv_headersegment TYPE e1edk01.
        READ TABLE it_edidd WITH KEY segnam = 'E1EDK01' ASSIGNING FIELD-SYMBOL(<fs_header>).
        IF sy-subrc = 0.
          lv_headersegment = <fs_header>-sdata.
          ev_objid = lv_headersegment-belnr.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
  METHOD get_DUMMY_objid.
    CASE iv_schema.
      WHEN 'ORDERS05'.

      DATA: LV_DUMMY_PO TYPE EKKO-EBELN,
            LV_DUMMY_IDOCNUM TYPE EDIDD-docnum,
            LV_RETURNCD TYPE inri-returncode.
      "get a new po number
*      CALL FUNCTION 'NUMBER_GET_NEXT'
*    EXPORTING
*      NR_RANGE_NR                   = hnrran   "Number range interval
*      OBJECT                        = 'EINKBELEG' "Number range object (SNRO)
**     QUANTITY                      = '1'       "No. of numbers
**     SUBOBJECT                     = ' '
**     TOYEAR                        = '0000'
**     IGNORE_BUFFER                 = ' '
*    IMPORTING
*      NUMBER                        = LV_DUMMY_PO
**     QUANTITY                      =
*      RETURNCODE                    = LV_RETURNCD
*    EXCEPTIONS
*      INTERVAL_NOT_FOUND            = 1
*      NUMBER_RANGE_NOT_INTERN       = 2
*      OBJECT_NOT_FOUND              = 3
*      QUANTITY_IS_0                 = 4
*      QUANTITY_IS_NOT_1             = 5
*      INTERVAL_OVERFLOW             = 6
*      BUFFER_OVERFLOW               = 7
*      OTHERS                        = 8.
*
*      CHECK SY-SUBRC = 0.

*      "get a new idoc number
*      CALL FUNCTION 'NUMBER_GET_NEXT'
*    EXPORTING
*      NR_RANGE_NR                   = '1'       "Number range interval
*      OBJECT                        = '/AIF/IFA' "Number range object (SNRO)
**     QUANTITY                      = '1'       "No. of numbers
**     SUBOBJECT                     = ' '
**     TOYEAR                        = '0000'
**     IGNORE_BUFFER                 = ' '
*    IMPORTING
*      NUMBER                        = LV_DUMMY_IDOCNUM
**     QUANTITY                      =
*      RETURNCODE                    = LV_RETURNCD
*    EXCEPTIONS
*      INTERVAL_NOT_FOUND            = 1
*      NUMBER_RANGE_NOT_INTERN       = 2
*      OBJECT_NOT_FOUND              = 3
*      QUANTITY_IS_0                 = 4
*      QUANTITY_IS_NOT_1             = 5
*      INTERVAL_OVERFLOW             = 6
*      BUFFER_OVERFLOW               = 7
*      OTHERS                        = 8.
*
*     CHECK SY-SUBRC = 0.

      "EV_NEWIDOCNUM = LV_DUMMY_IDOCNUM.

      DATA(r) = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                      min  = 1
                                      max = 10000 ).

      DATA(r2) = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                      min  = 1
                                      max = 10000 ).
        "data record E1EDK01 - BELNR
        DATA: lv_headersegment TYPE e1edk01,
              LV_HEADERSEGMENT2 TYPE E1EDK02.
        READ TABLE Ct_edidd WITH KEY segnam = 'E1EDK01' ASSIGNING FIELD-SYMBOL(<fs_header>).
        IF sy-subrc = 0.
          lv_headersegment = <fs_header>-sdata.
          data lv_belnr_int type int8.
         move lv_headersegment-belnr to lv_belnr_int.
          lv_belnr_int += r->get_next( ).
          lv_dummy_po = lv_belnr_int.
          lv_headersegment-belnr = LV_DUMMY_PO.
          EV_OBJID = LV_DUMMY_PO.

          data lv_idoc_int type int8.
          lv_idoc_int = <fs_header>-docnum.
          lv_idoc_int += r2->get_next( ).
          lv_dummy_idocnum = lv_idoc_int.
        ENDIF.
        READ TABLE CT_EDIDD WITH KEY SEGNAM = 'E1EDK02' ASSIGNING FIELD-SYMBOL(<fs_header_EXT>).
          lv_headersegment2 = <fs_header>-sdata.
          lv_headersegment2-belnr = LV_DUMMY_PO.
      WHEN OTHERS.
    ENDCASE.

    LOOP AT CT_EDIDD ASSIGNING FIELD-SYMBOL(<FS_EDIDD>).
        <FS_EDIDD>-docnum = LV_DUMMY_IDOCNUM.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
