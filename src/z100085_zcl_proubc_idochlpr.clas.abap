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
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_idochlpr IMPLEMENTATION.
  METHOD idoc_to_json.
    LOOP AT loaded_idocs ASSIGNING FIELD-SYMBOL(<fs_loaded_idoc>).

*    data: lt_edids type table of edids,
*          lt_edidd type table of edidd.
*
*    call function 'IDOC_READ_COMPLETELY'
*        importing DOCUMENT_NUMBER = <fs_loaded_idoc>-idocnum
*        tables INT_EDIDS = lt_edids
*               INT_EDIDD = lt_edidd.
*
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


     <fs_loaded_idoc>-idoc->get_xmldata_as_table( importing data_table = data(lt_datatable)
                                                              len = data(lv_len) ).

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
ENDCLASS.
