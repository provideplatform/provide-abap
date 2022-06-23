CLASS z100085_zcl_proubc_idochlpr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: z100085_zif_proubc_blidochlper.
    TYPES: tty_edidd TYPE TABLE OF edidd.
    DATA: lo_api_helper    TYPE REF TO z100085_zcl_proubc_api_helper,
          selected_idocs   TYPE Z100085_Zif_proubc_blidochlper=>tty_proubc_idocs.

    CLASS-METHODS:
      get_objid IMPORTING iv_schema TYPE string
                          it_edidd  TYPE tty_edidd
                          iv_idoc   TYPE REF TO data
                EXPORTING ev_objid  TYPE z100085_bpiobj-object_id,
      get_DUMMY_objid IMPORTING iv_schema     TYPE string
                      EXPORTING ev_objid      TYPE z100085_bpiobj-object_id
                                ev_newidocnum TYPE edidd-docnum
                      CHANGING  Ct_edidd      TYPE tty_edidd.
    METHODS:
      constructor IMPORTING iv_tenant TYPE z100085_prvdtenantid,
      launch_idoc_to_baseline.
  PROTECTED SECTION.
    data: lv_setup_success TYPE boolean,
          return_messages type table of bapiret2.
  methods: add_message importing iv_msg type bapiret2,
           clear_messages.
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_idochlpr IMPLEMENTATION.
  METHOD constructor.
    lo_api_helper = NEW z100085_zcl_proubc_api_helper( iv_tenant = iv_tenant ).

    "sets the default tenant and ident/baseline api tokens
    lo_api_helper->setup_protocol_msg( IMPORTING setup_success = lv_setup_success ).
    "TODO pass back error message to spool if unsuccessful
    CHECK lv_setup_success = abap_true.

  ENDMETHOD.
  METHOD launch_idoc_to_baseline.
    DATA:
      lo_ident_api         TYPE REF TO z100085_zif_proubc_ident,
      lo_baseline_api      TYPE REF TO z100085_zif_proubc_baseline,
      ls_protocol_msg_req  TYPE z100085_zif_proubc_baseline=>protocolmessage_req,
      "ls_bpiobjects_req    TYPE z100085_zif_proubc_baseline=>bpiobjects_req,
       ls_bpiobjects_req    TYPE z100085_zif_proubc_baseline=>businessobject,
      lt_updatedbpis       TYPE TABLE OF z100085_bpiobj,
      lt_newbpis           TYPE TABLE OF z100085_bpiobj,
      lt_final_updatedbpis TYPE TABLE OF z100085_bpiobj,
      lt_final_newbpis     TYPE TABLE OF z100085_bpiobj.


    LOOP AT selected_idocs ASSIGNING FIELD-SYMBOL(<fs_selected_idoc>).
      DATA: lv_idoc TYPE REF TO data.
      CLEAR: ls_protocol_msg_req.


      DATA:
        lv_idocnum      TYPE edidc-docnum,
        lt_edids        TYPE TABLE OF edids,
        lt_edidd        TYPE TABLE OF edidd,
        wa_idoc_control TYPE edidc,
        lv_status       TYPE i.

      CLEAR: lt_edids, lt_edidd, lv_idocnum.

      lv_idocnum = <fs_selected_idoc>-idocnum.
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

      "only keeping this around in case I need to change the payload string yet
      "data: lv_idoc_data type ref to data.
      "lv_idoc_data = lt_edidd.

      DATA: lv_idocjson TYPE string.
      lv_idocjson = /ui2/cl_json=>serialize(
         EXPORTING
           data             = lt_edidd
       ).

      "request to /api/v1/protocol_messages
      ls_protocol_msg_req-payload = lv_idocjson.
      ls_protocol_msg_req-payload_mimetype = 'json'.
      ls_protocol_msg_req-type = wa_idoc_control-idoctp. "should be orders05 for demo purposes


      "TODO handle errors if mapping to id is not implemented yet
      z100085_zcl_proubc_idochlpr=>get_objid( EXPORTING iv_schema = ls_protocol_msg_req-type
                               it_edidd = lt_edidd
                               iv_idoc = lv_idoc
                     IMPORTING ev_objid = ls_protocol_msg_req-id ).

      "request to /objects OR       "request to /business_objects
      ls_bpiobjects_req-id = ls_protocol_msg_req-id.
      ls_bpiobjects_req-type =  wa_idoc_control-idoctp. "maybe needs to be purchase_order instead of ORDERS05?
      ls_bpiobjects_req-payload = lv_idocjson.

*https://gist.github.com/kthomas/459381e98c808febea9c1bb51408bbde
      "call baseline API /api/v1/protocolmessage
      "this method keeps sending 404. is really implemented?
      "lo_api_helper->send_protocol_msg( EXPORTING body = ls_protocol_msg_req IMPORTING statuscode = lv_status  ). "should return 202
      lo_api_helper->create_businessobjects_msg(
        EXPORTING
          body           =  ls_bpiobjects_req
        IMPORTING
          statuscode     = lv_status
      ).

      "this appears to be the actual endpoint live today based on https://app.swaggerhub.com/apis/prvd/Baseline/v1.0.0#/info
      "lo_api_helper->send_bpiobjects_msg( exporting body = ls_bpiobjects_req importing statuscode = lv_status ).
      IF lv_status = '202'.
        DATA: wa_bpiobj    TYPE z100085_bpiobj,
              lv_timestamp TYPE timestampl.
        CLEAR: wa_bpiobj.
        SELECT SINGLE * FROM z100085_bpiobj INTO wa_bpiobj WHERE object_id = ls_protocol_msg_req-id.
        IF sy-subrc = 0.
          "todo validate same baseline_id is received for this object
          "wa_bpiobj-baseline_id = ''. "To be provided by api
          wa_bpiobj-proof = ''. "To be provided by api
          wa_bpiobj-status = ''. "To be determined by api response
          wa_bpiobj-object_id = ls_protocol_msg_req-id.
          wa_bpiobj-created_by = sy-uname.
          wa_bpiobj-created_at = lv_timestamp.
          wa_bpiobj-schematype = 'IDOC'.
          wa_bpiobj-schema_id = wa_idoc_control-mestyp.
          APPEND wa_bpiobj TO lt_updatedbpis.
        ELSE.
          GET TIME STAMP FIELD lv_timestamp.
          wa_bpiobj-baseline_id = ''. "To be provided by api
          wa_bpiobj-proof = ''. "To be provided by api
          wa_bpiobj-status = ''. "To be determined by api response
          wa_bpiobj-object_id = ls_protocol_msg_req-id.
          wa_bpiobj-created_by = sy-uname.
          wa_bpiobj-created_at = lv_timestamp.
          wa_bpiobj-schematype = 'IDOC'.
          wa_bpiobj-schema_id = wa_idoc_control-mestyp.
          APPEND wa_bpiobj TO lt_newbpis.
        ENDIF.
      ELSE. "log error message
      ENDIF.

    ENDLOOP.

    z100085_zcl_proubc_busobjhlpr=>validate_object_create(
      EXPORTING
        it_objects = lt_newbpis
      IMPORTING
        et_objects = lt_final_newbpis
    ).
    z100085_zcl_proubc_busobjhlpr=>create_object(
      EXPORTING
        it_objects = lt_final_newbpis
*      IMPORTING
*        et_objects =
    ).
    z100085_zcl_proubc_busobjhlpr=>validate_object_update(
      EXPORTING
        it_objects = lt_updatedbpis
      IMPORTING
        et_objects = lt_final_updatedbpis
    ).
    z100085_zcl_proubc_busobjhlpr=>update_object(
      EXPORTING
        it_objects = lt_final_updatedbpis
*      IMPORTING
*        et_objects =
    ).
    "TODO capture system messages for spooler/logging


  ENDMETHOD.

  METHOD z100085_zif_proubc_blidochlper~shuttle_idocs.
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
    WHERE direct = @iv_direct
    AND status = @iv_idocstatus
    AND mestyp = @iv_idocmestyp
    AND idoctp = @iv_idoctp
    AND docnum IN @it_idocnum.

    IF sy-subrc EQ 0.
      me->launch_idoc_to_baseline(  ).
    ENDIF.

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
      WHEN OTHERS. "TODO configure object id determinations, throw errors if missing
    ENDCASE.
  ENDMETHOD.
  METHOD get_DUMMY_objid.
    CASE iv_schema.
      WHEN 'ORDERS05'.

        DATA: lv_dummy_po      TYPE ekko-ebeln,
              lv_dummy_idocnum TYPE edidd-docnum,
              lv_returncd      TYPE inri-returncode.

        DATA(r) = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                        min  = 1
                                        max = 10000 ).

        DATA(r2) = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                        min  = 1
                                        max = 10000 ).
        "data record E1EDK01 - BELNR
        DATA: lv_headersegment  TYPE e1edk01,
              lv_headersegment2 TYPE e1edk02.
        READ TABLE Ct_edidd WITH KEY segnam = 'E1EDK01' ASSIGNING FIELD-SYMBOL(<fs_header>).
        IF sy-subrc = 0.
          lv_headersegment = <fs_header>-sdata.
          DATA lv_belnr_int TYPE int8.
          MOVE lv_headersegment-belnr TO lv_belnr_int.
          lv_belnr_int += r->get_next( ).
          lv_dummy_po = lv_belnr_int.
          lv_headersegment-belnr = lv_dummy_po.
          ev_objid = lv_dummy_po.

          DATA lv_idoc_int TYPE int8.
          lv_idoc_int = <fs_header>-docnum.
          lv_idoc_int += r2->get_next( ).
          lv_dummy_idocnum = lv_idoc_int.
        ENDIF.
        READ TABLE ct_edidd WITH KEY segnam = 'E1EDK02' ASSIGNING FIELD-SYMBOL(<fs_header_EXT>).
        lv_headersegment2 = <fs_header>-sdata.
        lv_headersegment2-belnr = lv_dummy_po.
      WHEN OTHERS.
    ENDCASE.

    LOOP AT ct_edidd ASSIGNING FIELD-SYMBOL(<fs_edidd>).
      <fs_edidd>-docnum = lv_dummy_idocnum.
    ENDLOOP.
  ENDMETHOD.

  method add_message.
    append iv_msg to return_messages.
  endmethod.
  method clear_messages.
    clear: return_messages.
  endmethod.
ENDCLASS.
