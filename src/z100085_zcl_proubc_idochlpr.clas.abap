CLASS z100085_zcl_proubc_idochlpr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: z100085_zif_proubc_blidochlper.
    DATA: loaded_idocs   TYPE z100085_zif_proubc_blidochlper=>tty_idoc_xmllist,
          selected_idocs TYPE Z100085_Zif_proubc_blidochlper=>tty_proubc_idocs.
    METHODS: idoc_to_json,
      get_idocs_for_object,
      validate_idocstat_to_baseline,
      shuttle_idoc,
      load_idocs.
  PROTECTED SECTION.

    METHODS: get_objid IMPORTING iv_schema TYPE string
                                 it_edidd  TYPE tty_edidd
                                 iv_idoc   TYPE REF TO data
                       EXPORTING ev_objid  TYPE z100085_bpiobj-object_id.
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

        data: lv_idocjson type string.
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
      me->get_objid( EXPORTING iv_schema = 'ORDERS05'
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
ENDCLASS.
