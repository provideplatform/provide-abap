CLASS z100085_zcl_proubc_idochlpr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: z100085_zif_proubc_blidochlper.
    TYPES: tty_edidd TYPE TABLE OF edidd.
    DATA: lo_api_helper  TYPE REF TO z100085_zcl_proubc_api_helper,
          selected_idocs TYPE Z100085_Zif_proubc_blidochlper=>tty_proubc_idocs.

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
      constructor IMPORTING iv_tenant          TYPE z100085_prvdtenantid
                            iv_subject_acct_id TYPE z100085_prvdtenantid,
      launch_idoc_to_baseline,
      create_mock_bpiobjs.
  PROTECTED SECTION.

    TYPES: BEGIN OF ty_idoc_struct_parent_child,
             parent TYPE edilsegtyp,
             child  TYPE edilsegtyp,
           END OF ty_idoc_struct_parent_child.
    TYPES: tty_idoc_struct_parent_child TYPE STANDARD TABLE OF ty_idoc_struct_parent_child.

    DATA lv_setup_success TYPE boolean .
    DATA:
      return_messages  TYPE TABLE OF bapiret2 .

    METHODS add_message
      IMPORTING
        !iv_msg TYPE bapiret2 .
    METHODS clear_messages .
    METHODS idoc_to_json
      IMPORTING
        !iv_idoc_basictype TYPE string
        !it_idoc_segments  TYPE idoc_data
      EXPORTING
        !ev_flattened_idoc TYPE REF TO data.
  PRIVATE SECTION.
    METHODS generate_child_idoc_segdata IMPORTING !iv_childsegmenttype      TYPE edilsegtyp
                                                  !iv_childrawsegment       TYPE edidd
                                                  !it_segmentstruct         TYPE ledid_t_segment_struct
                                                  !it_parentchild           TYPE tty_idoc_struct_parent_child
                                                  !iv_parent_json_segmentid TYPE string
                                                  !it_idoc_data_copy        TYPE idoc_data
                                        EXPORTING !ev_child_json_segmentid  TYPE string
                                                  !ev_child_json_segment    TYPE REF TO data
                                        CHANGING  !cv_parent_segment        TYPE REF TO data
                                                  !ct_parent_comp_tab       TYPE cl_abap_structdescr=>component_table. "may need this 2x for grandkids
    METHODS generate_idoc_segdata IMPORTING !iv_segmenttype      TYPE edilsegtyp
                                            !iv_rawsegment       TYPE edidd
                                            !it_segmentstruct    TYPE ledid_t_segment_struct
                                            !it_parentchild      TYPE tty_idoc_struct_parent_child
                                            !it_idoc_data_copy   TYPE idoc_data
                                  EXPORTING !ev_json_segmentdata TYPE REF TO data
                                            !ev_json_segmentid   TYPE string
                                            !et_comp_tab         TYPE cl_abap_structdescr=>component_table.
    METHODS get_component_tab      IMPORTING !iv_object   TYPE REF TO data
                                   EXPORTING !ev_comp_tab TYPE cl_abap_structdescr=>component_table.
ENDCLASS.



CLASS z100085_zcl_proubc_idochlpr IMPLEMENTATION.


  METHOD constructor.
    lo_api_helper = NEW z100085_zcl_proubc_api_helper( iv_tenant = iv_tenant iv_subject_acct_id = iv_subject_acct_id ).

    "sets the default tenant and ident/baseline api tokens
    lo_api_helper->setup_protocol_msg( IMPORTING setup_success = lv_setup_success ).
    "TODO pass back error message to spool if unsuccessful
    CHECK lv_setup_success = abap_true.

  ENDMETHOD.

  METHOD generate_child_idoc_segdata.
    DATA: ls_json_child_segment    TYPE REF TO data,
          lv_json_child_segmentid  TYPE string,
          ls_json_gchild_segment   TYPE REF TO data,
          lv_json_gchild_segmentid TYPE string.

    DATA: dataref       TYPE REF TO data,
          dataref_b     TYPE REF TO data,
          dataref_child TYPE REF TO data.
    DATA: comp_tab_child      TYPE cl_abap_structdescr=>component_table,
          comp_tab_parent_upd TYPE cl_abap_structdescr=>component_table,
          comp_wa             LIKE LINE OF comp_tab_child,
          struct_type         TYPE REF TO cl_abap_structdescr,
          struct_type_b       TYPE REF TO cl_abap_structdescr,
          struct_type_child   TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS: <segment_type>             TYPE any,
                   <segment_type_b>           TYPE any,
                   <segment_type_child>       TYPE any,
                   <segment_type_child_clone> TYPE any,
                   <segmentdata>              TYPE any,
                   <segmentdata_b>            TYPE any,
                   <segmentdata_child>        TYPE any.


    me->generate_idoc_segdata(
      EXPORTING
        iv_segmenttype      = iv_childsegmenttype
        iv_rawsegment       = iv_childrawsegment
        it_segmentstruct    =  it_segmentstruct
        it_parentchild      = it_parentchild
        it_idoc_data_copy =  it_idoc_data_copy
      IMPORTING
        ev_json_segmentdata = ls_json_child_segment
        ev_json_segmentid   = lv_json_child_segmentid
        et_comp_tab = comp_tab_child
    ).

    ev_child_json_segmentid = lv_json_child_segmentid.
    ev_child_json_segment = ls_json_child_segment.


    "DATA(components) =
    "  CAST cl_abap_structdescr(
    "  cl_abap_typedescr=>describe_by_data( ls_json_child_segment )
    ")->components.

    "need the components of the parent json segment here
    "might be to just pass it
*    DATA: lv_parent_segmentjson_type TYPE REF TO cl_abap_datadescr,
*          lv_psegmentjson_structdesc type ref to cl_abap_structdescr.
*
*    lv_parent_segmentjson_type ?= cl_abap_typedescr=>describe_by_data( ls_json_child_segment ).
*    CREATE DATA dataref TYPE HANDLE lv_parent_segmentjson_type.
*
*    lv_psegmentjson_structdesc ?= CAST cl_abap_structdescr(
*    lv_parent_segmentjson_type
*    ).

    "cl_abap_structdescr=>
    "comp_tab = cl_abap_datadescr=>describe_by_data( ls_json_child_segment )->c.

    comp_tab_parent_upd = ct_parent_comp_tab.

    "add a component to json type the child segment type
    comp_wa-name = lv_json_child_segmentid.
    "comp_wa-type ?= cl_abap_datadescr=>describe_by_data( ls_json_child_segment ).
    comp_wa-type ?= cl_abap_structdescr=>create( comp_tab_child ).
    APPEND comp_wa TO  comp_tab_parent_upd.

    "update the data ref to incorporate the child segment
    struct_type_b = cl_abap_structdescr=>create( comp_tab_parent_upd ).
    CREATE DATA dataref_b TYPE HANDLE struct_type_b.
    ASSIGN dataref_b->* TO <segmentdata_b>.

    dataref =  cv_parent_segment.
    ASSIGN dataref->* TO <segmentdata>.

    LOOP AT ct_parent_comp_tab ASSIGNING FIELD-SYMBOL(<fs_parent_segment_comp>).
      IF <segment_type> IS ASSIGNED.
        UNASSIGN <segment_type>.
      ENDIF.
      IF <segment_type_b> IS ASSIGNED.
        UNASSIGN <segment_type_b>.
      ENDIF.
      ASSIGN COMPONENT <fs_parent_segment_comp>-name OF STRUCTURE <segmentdata> TO <segment_type>.
      ASSIGN COMPONENT <fs_parent_segment_comp>-name OF STRUCTURE <segmentdata_b> TO <segment_type_b>.
      <segment_type_b> = <segment_type>.
      UNASSIGN: <segment_type>, <segment_type_b>.
    ENDLOOP.

    ct_parent_comp_tab = comp_tab_parent_upd.

    "generate child segment type attached to parent
*    struct_type_child = cl_abap_structdescr=>create( comp_tab_child ).
*    CREATE DATA dataref_child TYPE HANDLE struct_type_child.
*    dataref_child = ls_json_child_segment.

* grandkids!
    LOOP AT it_parentchild
        ASSIGNING FIELD-SYMBOL(<fs_parentchild>) WHERE parent = iv_childsegmenttype.

      LOOP AT it_idoc_data_copy ASSIGNING FIELD-SYMBOL(<fs_grandchild>) WHERE segnam = <fs_parentchild>-child
                                                                          AND psgnum = iv_childrawsegment-segnum.
        me->generate_child_idoc_segdata(
        EXPORTING
          iv_parent_json_segmentid = lv_json_child_segmentid
          iv_childsegmenttype      = <fs_parentchild>-child
          iv_childrawsegment       = <fs_grandchild>
          it_parentchild           = it_parentchild
          it_idoc_data_copy = it_idoc_data_copy
          it_segmentstruct         = it_segmentstruct
       IMPORTING
         ev_child_json_segmentid  = lv_json_gchild_segmentid
         ev_child_json_segment    = ls_json_gchild_segment
       CHANGING
         cv_parent_segment = ls_json_child_segment
         ct_parent_comp_tab = comp_tab_child
      ).
      ENDLOOP.
    ENDLOOP.



    "ASSIGN dataref_child->* TO <segmentdata_child>.
    ASSIGN ls_json_child_segment->* TO <segmentdata_child>.

    ASSIGN COMPONENT lv_json_child_segmentid  OF STRUCTURE <segmentdata_b> TO <segment_type_b>.

    <segment_type_b> = <segmentdata_child>.






    cv_parent_segment = dataref_b.

*    LOOP AT comp_tab_child ASSIGNING FIELD-SYMBOL(<fs_child_segment_comp>).
*      IF <segment_type_child> IS ASSIGNED.
*        UNASSIGN <segment_type_child>.
*      ENDIF.
*      IF <segment_type_child_clone> IS ASSIGNED.
*        UNASSIGN <segment_type_child_clone>.
*      ENDIF.
*      ASSIGN COMPONENT <fs_child_segment_comp>-name OF STRUCTURE <segmentdata_child> TO <segment_type_child>.
*      ASSIGN COMPONENT <fs_child_segment_comp>-name of STRUCTURE <segment_type_b> to <segment_type_child_clone>.
*      <segment_type_child_clone> = <segment_type_child>.
*      UNASSIGN: <segment_type_child>, <segment_type_child_clone>.
*    ENDLOOP.
*
*


*    DATA:  dataref TYPE REF TO data.
*    DATA: comp_tab    TYPE cl_abap_structdescr=>component_table,
*          comp_wa     LIKE LINE OF comp_tab,
*          struct_type TYPE REF TO cl_abap_structdescr. "Structure
*    FIELD-SYMBOLS: <segmentdata>  TYPE any,
*                   <segment_type> TYPE any,
*                   <docnum>       TYPE any,
*                   <segnum>       TYPE any.
*
*    comp_wa-name = ev_json_segmentid .
*    comp_wa-type ?= cl_abap_datadescr=>describe_by_name( iv_rawsegment-segnam ).
*    APPEND comp_wa TO comp_tab.
*    comp_wa-name = 'segment_type'.
*    comp_wa-type ?= cl_abap_datadescr=>describe_by_data( iv_rawsegment-segnam )  .
*    APPEND comp_wa TO comp_tab.
*    comp_wa-name = 'docnum'.
*    comp_wa-type ?= cl_abap_datadescr=>describe_by_data( iv_rawsegment-docnum ).
*    APPEND comp_wa TO comp_tab.
*    comp_wa-name = 'segnum'.
*    comp_wa-type ?= cl_abap_datadescr=>describe_by_data( iv_rawsegment-segnum ).
*    APPEND comp_wa TO comp_tab.
*
*    "Create Dynamic table using component table
*    struct_type = cl_abap_structdescr=>create( comp_tab ).
*    CREATE DATA dataref TYPE HANDLE struct_type.
*    ASSIGN dataref->* TO <segmentdata>. "Dyanmic Structure


  ENDMETHOD.

  METHOD generate_idoc_segdata.
    "create a variable of the data type of the segment
    DATA: lv_json_segment TYPE REF TO data.
    FIELD-SYMBOLS: <fs_json_segment> TYPE any.
    "data: lv_segment type iv_segmenttype.
    DATA(lv_segment_type) = cl_abap_classdescr=>describe_by_name( iv_segmenttype ). "cast #( iv_segmenttype ).
    DATA: lv_segment TYPE REF TO data.
    CREATE DATA lv_segment TYPE (iv_segmenttype).

    CONCATENATE iv_rawsegment-segnam '_' iv_rawsegment-segnum INTO ev_json_segmentid.

    DATA:  dataref TYPE REF TO data.
    DATA: comp_tab    TYPE cl_abap_structdescr=>component_table,
          comp_wa     LIKE LINE OF comp_tab,
          struct_type TYPE REF TO cl_abap_structdescr. "Structure
    FIELD-SYMBOLS: <segmentdata>  TYPE any,
                   <segment_type> TYPE any,
                   <docnum>       TYPE any,
                   <segnum>       TYPE any,
                   <psgnum>       TYPE any.

    comp_wa-name = ev_json_segmentid .
    comp_wa-type ?= cl_abap_datadescr=>describe_by_name( iv_rawsegment-segnam ).
    APPEND comp_wa TO comp_tab.
    comp_wa-name = 'segment_type'.
    comp_wa-type ?= cl_abap_datadescr=>describe_by_data( iv_rawsegment-segnam )  .
    APPEND comp_wa TO comp_tab.
    comp_wa-name = 'docnum'.
    comp_wa-type ?= cl_abap_datadescr=>describe_by_data( iv_rawsegment-docnum ).
    APPEND comp_wa TO comp_tab.
    comp_wa-name = 'segnum'.
    comp_wa-type ?= cl_abap_datadescr=>describe_by_data( iv_rawsegment-segnum ).
    APPEND comp_wa TO comp_tab.
    comp_wa-name = 'psgnum'.
    comp_wa-type ?= cl_abap_datadescr=>describe_by_data( iv_rawsegment-psgnum ).
    APPEND comp_wa TO comp_tab.


    "Create Dynamic table using component table
    struct_type = cl_abap_structdescr=>create( comp_tab ).
    CREATE DATA dataref TYPE HANDLE struct_type.
    ASSIGN dataref->* TO <segmentdata>. "Dyanmic Structure


    FIELD-SYMBOLS: <fs_segment_raw> TYPE any.
    ASSIGN iv_rawsegment-sdata TO <fs_segment_raw> CASTING TYPE (iv_rawsegment-segnam).
    <segmentdata> = <fs_segment_raw>.


    ASSIGN COMPONENT 'SEGMENT_TYPE' OF STRUCTURE <segmentdata> TO <segment_type>.
    <segment_type> = iv_rawsegment-segnam.
    ASSIGN COMPONENT 'DOCNUM' OF STRUCTURE <segmentdata> TO <docnum>.
    <docnum> = iv_rawsegment-docnum.
    ASSIGN COMPONENT 'SEGNUM' OF STRUCTURE <segmentdata> TO <segnum>.
    <segnum> = iv_rawsegment-segnUm.
    ASSIGN COMPONENT 'PSGNUM' OF STRUCTURE <segmentdata> TO <psgnum>.
    <psgnum> = iv_rawsegment-psgnum.


    et_comp_tab = comp_tab.
    ev_json_segmentdata = dataref.

*    ASSIGN iv_rawsegment-segnam to <segment_type> casting type edilsegtyp.
*    <segment_type> = iv_rawsegment-segnam.


*    ASSIGN dataref->* to <docnum>.
*    ASSIGN iv_rawsegment-docnum TO <docnum> CASTING TYPE (iv_rawsegment-docnum).
*    ASSIGN dataref->* to <segnum>.
*    ASSIGN iv_rawsegment-segnum TO <segnum> CASTING TYPE (iv_rawsegment-segnum).

*    "assign COMPONENT ev_json_segmentid
*
*    FIELD-SYMBOLS: <attribute> TYPE any.
*
*    ASSIGN  lv_json_segment->(ev_json_segmentid) TO <attribute>.
*    "assign component 'SDATA' OF STRUCTURE LV_JSON_SEGMENT TO <FS_SEGMENT_RAW>.
*    "lv_json_segment['SDATA'] = <fs_segment_raw>.
*
*    DATA(components) =
*       CAST cl_abap_structdescr(
*       cl_abap_typedescr=>describe_by_name( iv_segmenttype )
*    )->components.
*
*    DATA components TYPE cl_abap_structdescr=>component_table.
*    DATA(components2) = cl_abap_typedescr=>describe_by_name( iv_segmenttype ).

    "struct_type = cl_abap_structdescr=>create( components2- ).
*
*    LOOP AT it_segmentstruct ASSIGNING FIELD-SYMBOL(<fs_segmentstruct>) WHERE segment_type = iv_segmenttype.
*      ASSIGN COMPONENT <fs_segmentstruct>-fieldname OF STRUCTURE <fs_segmentstruct> TO <fs_json_segment>.
*    ENDLOOP.



  ENDMETHOD.


  METHOD launch_idoc_to_baseline.
    DATA:
      lo_ident_api         TYPE REF TO z100085_zif_proubc_ident,
      lo_baseline_api      TYPE REF TO z100085_zif_proubc_baseline,
      ls_protocol_msg_req  TYPE z100085_zif_proubc_baseline=>protocolmessage_req,
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
*      lv_idocjson = /ui2/cl_json=>serialize(
*         EXPORTING
*           data             = lt_edidd
*       ).


      DATA: lv_flattened_idoc TYPE REF TO data.
      me->idoc_to_json(
        EXPORTING
          iv_idoc_basictype = 'ORDERS05'
          it_idoc_segments  =  lt_edidd
         IMPORTING
           ev_flattened_idoc = lv_flattened_idoc
      ).
      lv_idocjson = /ui2/cl_json=>serialize(
        EXPORTING
            data = lv_flattened_idoc
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


*https://gist.github.com/kthomas/459381e98c808febea9c1bb51408bbde
      "call baseline API /api/v1/protocolmessage
      lo_api_helper->send_protocol_msg( EXPORTING body = ls_protocol_msg_req IMPORTING statuscode = lv_status  ). "should return 202


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


  METHOD create_mock_bpiobjs.
    DATA:
      lo_ident_api         TYPE REF TO z100085_zif_proubc_ident,
      lo_baseline_api      TYPE REF TO z100085_zif_proubc_baseline,
      ls_protocol_msg_req  TYPE z100085_zif_proubc_baseline=>protocolmessage_req,
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

      "Baseline API should be able to determine this on its w/o SAP help
      SELECT SINGLE * FROM z100085_bpiobj INTO @DATA(wa_bpiobj) WHERE object_id = @ls_protocol_msg_req-id.
      DATA: lv_update TYPE char1.
      IF sy-subrc EQ 0.
        ls_protocol_msg_req-baselineid = wa_bpiobj-baseline_id.
        lv_update = 'X'.
        "append ls_protocol_msg_req to lt_updatedbpis.
      ELSE.
        "just for demo purposes we'll make baseline = purch order. should be globally unique
        ls_protocol_msg_req-baselineid = ls_protocol_msg_req-id.
        lv_update = ''.
        "append ls_protocol_msg_req to lt_newbpis.
      ENDIF.

      IF lv_update = 'X'.
        z100085_zcl_proubc_busobjhlpr=>validate_object_update(
          EXPORTING
            it_objects = lt_updatedbpis
          IMPORTING
            et_objects = lt_final_updatedbpis
        ).
        z100085_zcl_proubc_busobjhlpr=>update_mock_object(
          EXPORTING
            it_objects = lt_final_updatedbpis
            iv_payload = lv_idocjson
*      IMPORTING
*        et_objects =
        ).
      ELSE.
        z100085_zcl_proubc_busobjhlpr=>validate_object_create(
           EXPORTING
             it_objects = lt_newbpis
           IMPORTING
             et_objects = lt_final_newbpis
         ).
        z100085_zcl_proubc_busobjhlpr=>create_mock_object(
          EXPORTING
            it_objects = lt_final_newbpis
            iv_payload = lv_idocjson
*      IMPORTING
*        et_objects =
        ).
      ENDIF.
      CLEAR: ls_protocol_msg_req, lt_updatedbpis, lt_newbpis.

    ENDLOOP.
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


  METHOD z100085_zif_proubc_blidochlper~mock_shuttle_idocs.
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
      me->create_mock_bpiobjs( ).
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


  METHOD add_message.
    APPEND iv_msg TO return_messages.
  ENDMETHOD.


  METHOD clear_messages.
    CLEAR: return_messages.
  ENDMETHOD.


  METHOD idoc_to_json.

    DATA: lv_flattened_idoc TYPE REF TO data.

    "get the idoc metadata
    DATA: lv_selectedbasictype TYPE string,
          ls_basictypes        TYPE z100085_zif_idocapi_typelist=>ty_basictype,
          ls_responsedata      TYPE z100085_zif_idocapi_typelist=>ty_basictype_w_segments,
          lv_idoc_type_in      TYPE ledid_idoctype,
          lv_idoc_type_out     TYPE ledid_idoc_type,
          lt_IDOC_STRUCT       TYPE ledid_t_idoc_struct,
          lt_SEGMENTS          TYPE ledid_t_segment,
          lt_SEGMENT_STRUCT    TYPE ledid_t_segment_struct.

    lv_idoc_type_in = iv_idoc_basictype.

    "get the idoc segments
    CALL FUNCTION 'IDOC_TYPE_COMPLETE_READ'
      EXPORTING
        struct_type    = 'B'
        idoctype       = lv_idoc_type_in
        release        = ''
        applrel        = ''
        version        = '3'
      IMPORTING
        idoc_type      = lv_idoc_type_out
      TABLES
        idoc_struct    = lt_idoc_struct
        segments       = lt_segments
        segment_struct = lt_segment_struct
      EXCEPTIONS
        OTHERS         = 1.

    DATA: lt_idoc_struct_parent_child TYPE TABLE OF ty_idoc_struct_parent_child.

    "look up parent-child structure of the idoc
    LOOP AT lt_idoc_struct ASSIGNING FIELD-SYMBOL(<fs_idoc_struct>) WHERE syntax_attrib-parseg IS NOT INITIAL.
      DATA ls_idoc_struct_parent_child TYPE ty_idoc_struct_parent_child.
      CLEAR ls_idoc_struct_parent_child.
      ls_idoc_struct_parent_child-parent = <fs_idoc_struct>-syntax_attrib-parseg.
      ls_idoc_struct_parent_child-child = <fs_idoc_struct>-segment_type.
      APPEND ls_idoc_struct_parent_child TO lt_idoc_struct_parent_child.
    ENDLOOP.

    "needed for generating structure of the idoc json
    DATA: comp_tab   TYPE cl_abap_structdescr=>component_table,
          comp_tab_b TYPE cl_abap_structdescr=>component_table,
          comp_wa    LIKE LINE OF comp_tab.
    "lt_idoc_json_segment type any table.
    DATA lr_ref TYPE REF TO data.
    FIELD-SYMBOLS: <fs_flattened_idoc> TYPE any,
                   <fs_ls_idoc_json>   TYPE any,
                   <fs_lt_idoc_json>   TYPE STANDARD TABLE.

    "CREATE DATA lr_ref LIKE STANDARD TABLE OF <fs_ls_idoc_json>.
    "ASSIGN lr_ref->* TO <fs_lt_idoc_json>.

    DATA: struct_type   TYPE REF TO cl_abap_structdescr,
          struct_type_b TYPE REF TO cl_abap_structdescr.


    LOOP AT lt_idoc_struct ASSIGNING <fs_idoc_struct> WHERE syntax_attrib-parseg IS INITIAL. "top level segments only
      "handle multiple children

      "IF sy-subrc = 4. "normal segment


      DATA ls_json_segmentdata TYPE REF TO data.
      DATA lv_json_segmentid TYPE string.

      DATA: lt_idoc_segments_copy TYPE idoc_data.
      lt_idoc_segments_copy = it_idoc_segments.

      "get the raw segment
      DATA: wa_rawsegment      LIKE LINE OF it_idoc_segments,
            wa_rawchildsegment LIKE LINE OF it_idoc_segments,
            lt_segment_comptab TYPE cl_abap_structdescr=>component_table.
      "READ TABLE it_idoc_segments INTO wa_rawsegment WITH KEY segnam = <fs_idoc_struct>-segment_type.
      LOOP AT it_idoc_segments INTO wa_rawsegment WHERE segnam = <fs_idoc_struct>-segment_type.
        "IF sy-subrc = 0.
        CLEAR:  lt_segment_comptab.
        "create the segment json
        me->generate_idoc_segdata(
          EXPORTING
            iv_segmenttype      = <fs_idoc_struct>-segment_type
            iv_rawsegment       = wa_rawsegment
            it_segmentstruct    = lt_segment_struct
            it_parentchild = lt_idoc_struct_parent_child
            it_idoc_data_copy = lt_idoc_segments_copy
          IMPORTING
            ev_json_segmentdata = ls_json_segmentdata
            ev_json_segmentid = lv_json_segmentid
            et_comp_tab = lt_segment_comptab
        ).

        "add any child segments
        DATA: ls_json_child_segment   TYPE REF TO data,
              lv_json_child_segmentid TYPE string.
        LOOP AT lt_idoc_struct_parent_child
                    ASSIGNING FIELD-SYMBOL(<fs_idoc_struct_parent_child>) WHERE parent = <fs_idoc_struct>-segment_type.

          LOOP AT lt_idoc_segments_copy INTO wa_rawchildsegment WHERE segnam = <fs_idoc_struct_parent_child>-child
                                                                 AND  psgnum = wa_rawsegment-segnum.
            me->generate_child_idoc_segdata(
              EXPORTING
                iv_childsegmenttype = <fs_idoc_struct_parent_child>-child
                iv_childrawsegment = wa_rawchildsegment
                iv_parent_json_segmentid = lv_json_segmentid
                it_parentchild           = lt_idoc_struct_parent_child
                it_segmentstruct = lt_segment_struct
                it_idoc_data_copy = lt_idoc_segments_copy
             IMPORTING
               ev_child_json_segmentid  = lv_json_child_segmentid
               ev_child_json_segment    = ls_json_child_segment
             CHANGING
              cv_parent_segment = ls_json_segmentdata
              ct_parent_comp_tab = lt_segment_comptab
            ).
          ENDLOOP.

        ENDLOOP.

        "add segment(s) to the json output
        ASSIGN ls_json_segmentdata->* TO <fs_ls_idoc_json>.

        DATA: dataref   TYPE REF TO data,
              dataref_b TYPE REF TO data.
        FIELD-SYMBOLS: <segmentdata>   TYPE any,
                       <segmentdata_b> TYPE any,
                       <mappedsegment> TYPE any,
                       <targetsegment> TYPE any.
        "IF lr_ref IS INITIAL.

        IF dataref IS INITIAL AND dataref_b IS INITIAL.
          comp_wa-name = lv_json_segmentid.
          comp_wa-type ?= cl_abap_datadescr=>describe_by_data( ls_json_segmentdata ).
          APPEND comp_wa TO comp_tab.
          comp_tab_b = comp_tab.

          struct_type = cl_abap_structdescr=>create( comp_tab ).
          "struct_type_b = struct_type.
          CREATE DATA dataref TYPE HANDLE struct_type.
          ASSIGN dataref->* TO <segmentdata>.
          ASSIGN COMPONENT lv_json_segmentid OF STRUCTURE <segmentdata> TO <targetsegment>.
          <targetsegment> = ls_json_segmentdata.
          UNASSIGN <targetsegment>.
        ELSEIF dataref IS NOT INITIAL AND dataref_b IS INITIAL.
          comp_wa-name = lv_json_segmentid.
          comp_wa-type ?= cl_abap_datadescr=>describe_by_data( ls_json_segmentdata ).
          APPEND comp_wa TO comp_tab_b.

          struct_type_b = cl_abap_structdescr=>create( comp_tab_b ).
          CREATE DATA dataref_b TYPE HANDLE struct_type_b.
          ASSIGN dataref_b->* TO <segmentdata_b>.
          "<segmentdata_b> = <segmentdata>. dumps
          "UNASSIGN <segmentdata>.
          "FREE: dataref, struct_type.
*          DATA(components) = "dumpy
*            CAST cl_abap_structdescr(
*            cl_abap_typedescr=>describe_by_data( dataref )
*          )->components.
*          LOOP AT components ASSIGNING FIELD-SYMBOL(<fs_component>).
*          ENDLOOP.
          LOOP AT comp_tab ASSIGNING FIELD-SYMBOL(<fs_component>).
            ASSIGN COMPONENT <fs_component>-name OF STRUCTURE <segmentdata_b> TO <targetsegment>.
            IF <targetsegment> IS ASSIGNED.
              ASSIGN COMPONENT <fs_component>-name OF STRUCTURE <segmentdata> TO <mappedsegment>.
              IF <mappedsegment> IS ASSIGNED.
                <targetsegment> = <mappedsegment>.
              ENDIF.
            ENDIF.
          ENDLOOP.
          comp_tab = comp_tab_b.

          ASSIGN COMPONENT lv_json_segmentid OF STRUCTURE <segmentdata_b> TO <targetsegment>.
          IF <targetsegment> IS ASSIGNED.
            <targetsegment> = ls_json_segmentdata.
            UNASSIGN <targetsegment>.
          ENDIF.

          UNASSIGN <segmentdata>.
          FREE dataref.
          CREATE DATA dataref TYPE HANDLE struct_type_b.
          ASSIGN dataref->* TO <segmentdata>.
          <segmentdata> = <segmentdata_b>.
          UNASSIGN <segmentdata_b>.
          FREE dataref_b.
          ASSIGN dataref->* TO <segmentdata>.

        ENDIF.
      ENDLOOP.

    ENDLOOP.

    FIELD-SYMBOLS: <fs_final_flattened_idoc> TYPE any.

    CREATE DATA lv_flattened_idoc TYPE HANDLE struct_type_b.
    ASSIGN lv_flattened_idoc->* TO <fs_final_flattened_idoc>.

    <fs_final_flattened_idoc> = <segmentdata>.

    ev_flattened_idoc = lv_flattened_idoc.

  ENDMETHOD.

  METHOD get_component_tab.
  ENDMETHOD.

ENDCLASS.
