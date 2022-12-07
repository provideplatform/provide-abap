CLASS zcl_proubc_idochlpr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_proubc_blidochlper .

    TYPES:
      tty_edidd TYPE TABLE OF edidd .

    DATA lo_api_helper TYPE REF TO zcl_proubc_api_helper .
    DATA selected_idocs TYPE zif_proubc_blidochlper=>tty_proubc_idocs .

    CLASS-METHODS get_objid
      IMPORTING
        !iv_schema TYPE string
        !it_edidd  TYPE tty_edidd
        !iv_idoc   TYPE REF TO data
      EXPORTING
        !ev_objid  TYPE zbpiobj-object_id .
    CLASS-METHODS idoc_schema_to_json_tree
      IMPORTING
        !it_idoc_struct           TYPE ledid_t_idoc_struct
        !it_segments              TYPE ledid_t_segment
        !it_segment_struct        TYPE ledid_t_segment_struct
      EXPORTING
        !ev_idoc_schema_json_tree TYPE REF TO data .
    CLASS-METHODS get_dummy_objid
      IMPORTING
        !iv_schema     TYPE string
      EXPORTING
        !ev_objid      TYPE zbpiobj-object_id
        !ev_newidocnum TYPE edidd-docnum
      CHANGING
        !ct_edidd      TYPE tty_edidd .
    CLASS-METHODS generate_segment_fields
      IMPORTING
        !it_target_segment_struct TYPE ledid_t_segment_struct
      EXPORTING
        !et_field_data            TYPE zif_proubc_blidochlper=>tty_idoc_segment_field .
    CLASS-METHODS generate_child_segment_schema
      CHANGING
        !cs_segment_schema TYPE zif_proubc_blidochlper=>ty_idoc_segment .
    METHODS constructor
      IMPORTING
        !iv_tenant          TYPE zprvdtenantid OPTIONAL
        !iv_subject_acct_id TYPE zprvdtenantid OPTIONAL
        !iv_workgroup_id    TYPE zprvdtenantid OPTIONAL .
    METHODS launch_idoc_to_baseline
      IMPORTING
        VALUE(iv_idocmesty) TYPE edi_mestyp
        VALUE(iv_idoctp)    TYPE edi_idoctp .
    METHODS check_for_illegal_characters
      IMPORTING
        iv_string             TYPE string
      EXPORTING
        ev_illegal_char_index TYPE i
        ev_illegal_char_found TYPE c
        ev_safestring         TYPE char30.
    METHODS build_safestring
      IMPORTING
        iv_string     TYPE string
      EXPORTING
        ev_safestring TYPE char30.
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_idoc_struct_parent_child,
             parent TYPE edilsegtyp,
             child  TYPE edilsegtyp,
           END OF ty_idoc_struct_parent_child.
    TYPES: tty_idoc_struct_parent_child TYPE STANDARD TABLE OF ty_idoc_struct_parent_child.
    DATA: lv_setup_success TYPE boolean,
          return_messages  TYPE TABLE OF bapiret2.
    METHODS: add_message IMPORTING iv_msg TYPE bapiret2,
      clear_messages,
      idoc_to_json
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
ENDCLASS.



CLASS zcl_proubc_idochlpr IMPLEMENTATION.


  METHOD add_message.
    APPEND iv_msg TO return_messages.
  ENDMETHOD.


  METHOD clear_messages.
    CLEAR: return_messages.
  ENDMETHOD.


  METHOD constructor.
    "authenticates to PRVD APIs / authority checks use of the PRVD Tenant to SAP
    "used for all PRVD API calls
    lo_api_helper = NEW zcl_proubc_api_helper( iv_tenant = iv_tenant iv_subject_acct_id = iv_subject_acct_id iv_workgroup_id = iv_workgroup_id ).

    "Sets the ident/baseline api tokens
    lo_api_helper->setup_protocol_msg( IMPORTING setup_success = lv_setup_success ).
    CHECK lv_setup_success = abap_true.

  ENDMETHOD.


  METHOD get_dummy_objid.
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
        READ TABLE ct_edidd WITH KEY segnam = 'E1EDK01' ASSIGNING FIELD-SYMBOL(<fs_header>).
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
        READ TABLE ct_edidd WITH KEY segnam = 'E1EDK02' ASSIGNING FIELD-SYMBOL(<fs_header_ext>).
        lv_headersegment2 = <fs_header>-sdata.
        lv_headersegment2-belnr = lv_dummy_po.
      WHEN OTHERS.
    ENDCASE.

    LOOP AT ct_edidd ASSIGNING FIELD-SYMBOL(<fs_edidd>).
      <fs_edidd>-docnum = lv_dummy_idocnum.
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
      WHEN  'INTERNAL_ORDER01'.
        DATA: lv_intordersegment TYPE e1bp2075_masterdata_ale.
        READ TABLE it_edidd WITH KEY segnam = 'E1BP2075_MASTERDATA_ALE' ASSIGNING FIELD-SYMBOL(<fs_header_intord>).
        IF sy-subrc = 0.
          lv_intordersegment = <fs_header_intord>-sdata.
          ev_objid = lv_intordersegment-orderid.
        ENDIF.
      WHEN OTHERS. "TODO configure object id determinations, throw errors if missing
    ENDCASE.
  ENDMETHOD.


  METHOD idoc_schema_to_json_tree.

    DATA: lv_idoc_json_tree_data TYPE REF TO data,
          comp_tab               TYPE cl_abap_structdescr=>component_table,
          comp_wa                LIKE LINE OF comp_tab,
          struct_type            TYPE REF TO cl_abap_structdescr,
          dataref                TYPE REF TO data.

    "map the parent-child relationships
    DATA: lt_idoc_struct_parent_child TYPE TABLE OF ty_idoc_struct_parent_child,
          lt_idoc_struct_copy         TYPE ledid_t_idoc_struct.

    "summarize parent-child structure of the idoc
    LOOP AT it_idoc_struct ASSIGNING FIELD-SYMBOL(<fs_idoc_struct>) WHERE syntax_attrib-parseg IS NOT INITIAL.
      DATA ls_idoc_struct_parent_child TYPE ty_idoc_struct_parent_child.
      CLEAR ls_idoc_struct_parent_child.
      ls_idoc_struct_parent_child-parent = <fs_idoc_struct>-syntax_attrib-parseg.
      ls_idoc_struct_parent_child-child = <fs_idoc_struct>-segment_type.
      APPEND ls_idoc_struct_parent_child TO lt_idoc_struct_parent_child.
    ENDLOOP.

    "get the type descriptor for zif_proubc_blidochlper=>ty_idoc_segment
    DATA: ls_dummy_segment TYPE zif_proubc_blidochlper=>ty_idoc_segment.

    "create top level components
    LOOP AT it_idoc_struct ASSIGNING <fs_idoc_struct> WHERE syntax_attrib-parseg IS INITIAL.
      comp_wa-name = <fs_idoc_struct>-segment_type.
      comp_wa-type ?= cl_abap_datadescr=>describe_by_data( ls_dummy_segment )  .
      APPEND comp_wa TO comp_tab.
      CLEAR comp_wa.
    ENDLOOP.

    FIELD-SYMBOLS: <fs_idoc_json_tree_data> TYPE any,
                   <fs_dataref>             TYPE any.

    struct_type = cl_abap_structdescr=>create( comp_tab ).
    CREATE DATA dataref TYPE HANDLE struct_type.
    ASSIGN dataref->* TO <fs_dataref>.

    "assign data from tree structure from parent level down
    LOOP AT it_idoc_struct ASSIGNING <fs_idoc_struct> WHERE syntax_attrib-parseg IS INITIAL.
      "map the data
      DATA: ls_idoc_struct TYPE zif_proubc_blidochlper=>ty_idoc_segment.
      ls_idoc_struct-segment_type = <fs_idoc_struct>-segment_type.
      ls_idoc_struct-description = <fs_idoc_struct>-segment_type_attrib-descrp.
      ls_idoc_struct-minoccurs = <fs_idoc_struct>-syntax_attrib-occmin.
      ls_idoc_struct-maxoccurs = <fs_idoc_struct>-syntax_attrib-occmax.
      "parent segment ~ not needed here
      "generate child segments TODO
      "generate fields
      DATA: lt_mapped_segment_struct TYPE ledid_t_segment_struct.
      lt_mapped_segment_struct = VALUE ledid_t_segment_struct( FOR line IN it_segment_struct WHERE ( segment_type EQ <fs_idoc_struct>-segment_type  ) ( line ) ).
      zcl_proubc_idochlpr=>generate_segment_fields(
        EXPORTING
          it_target_segment_struct =  lt_mapped_segment_struct
        IMPORTING
          et_field_data            = ls_idoc_struct-fields
      ).
      FIELD-SYMBOLS: <fs_targetsegment> TYPE any,
                     <fs_mappedsegment> TYPE any.
      "TODO this is dumpin but why?
      ASSIGN COMPONENT <fs_idoc_struct>-segment_type OF STRUCTURE <fs_dataref> TO <fs_targetsegment>.
      DATA: ls_mappedsegment_data TYPE REF TO data.
      GET REFERENCE OF ls_idoc_struct INTO ls_mappedsegment_data.
      ASSIGN ls_mappedsegment_data->* TO <fs_mappedsegment>.
      <fs_targetsegment> = <fs_mappedsegment>.
    ENDLOOP.

    GET REFERENCE OF dataref INTO lv_idoc_json_tree_data.

    ASSIGN lv_idoc_json_tree_data->* TO <fs_idoc_json_tree_data>.
    <fs_idoc_json_tree_data> = <fs_dataref>.

    ev_idoc_schema_json_tree = lv_idoc_json_tree_data.

  ENDMETHOD.


  METHOD launch_idoc_to_baseline.
    DATA:
      lo_ident_api         TYPE REF TO zif_proubc_ident,
      lo_baseline_api      TYPE REF TO zif_proubc_baseline,
      ls_protocol_msg_req  TYPE zif_proubc_baseline=>protocolmessage_req,
      "ls_bpiobjects_req    TYPE zif_proubc_baseline=>bpiobjects_req,
      ls_bpiobjects_req    TYPE zif_proubc_baseline=>businessobject,
      lt_updatedbpis       TYPE TABLE OF zbpiobj,
      lt_newbpis           TYPE TABLE OF zbpiobj,
      lt_final_updatedbpis TYPE TABLE OF zbpiobj,
      lt_final_newbpis     TYPE TABLE OF zbpiobj.


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

      DATA: lv_flattened_idoc TYPE REF TO data.
      DATA: lv_idoc_basictype TYPE string.
      lv_idoc_basictype = wa_idoc_control-idoctp.
      me->idoc_to_json(
        EXPORTING
          iv_idoc_basictype = lv_idoc_basictype
          it_idoc_segments  =  lt_edidd
         IMPORTING
           ev_flattened_idoc = lv_flattened_idoc
      ).

      "request to /api/v1/protocol_messages
      ls_protocol_msg_req-payload = lv_flattened_idoc.
      ls_protocol_msg_req-payload_mimetype = 'json'.
      ls_protocol_msg_req-type = wa_idoc_control-idoctp.
      zcl_proubc_idochlpr=>get_objid( EXPORTING iv_schema = ls_protocol_msg_req-type
                               it_edidd = lt_edidd
                               iv_idoc = lv_idoc
                     IMPORTING ev_objid = ls_protocol_msg_req-id ).

      "creates the Baseline Protocol / zk-proof of the idoc
      DATA: lv_apiresponse    TYPE REF TO data,
            lv_apiresponsestr TYPE string.
      lo_api_helper->send_protocol_msg( EXPORTING body = ls_protocol_msg_req IMPORTING statuscode = lv_status
                                                                                       apiresponse = lv_apiresponse
                                                                                       apiresponsestr = lv_apiresponsestr  ). "should return 202

      IF lv_status = '202'.
        DATA: ls_protocol_msg_resp TYPE zif_proubc_baseline=>protocolmessage_resp.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_apiresponsestr  CHANGING data = ls_protocol_msg_resp ).

        DATA: wa_bpiobj    TYPE zbpiobj,
              lv_timestamp TYPE timestampl.
        CLEAR: wa_bpiobj.
        SELECT SINGLE * FROM zbpiobj INTO wa_bpiobj WHERE object_id = ls_protocol_msg_req-id
                                                      and baseline_id = ls_protocol_msg_resp-baseline_id.
        IF sy-subrc = 0.
          wa_bpiobj-baseline_id = ls_protocol_msg_resp-baseline_id. "To be provided by api
          wa_bpiobj-proof = ls_protocol_msg_resp-proof. "To be provided by api
          "wa_bpiobj-status = ls_protocol_msg_resp-. "To be determined by api response
          wa_bpiobj-object_id = ls_protocol_msg_req-id.
          wa_bpiobj-changed_by = sy-uname.
          wa_bpiobj-changed_at = lv_timestamp.
          wa_bpiobj-schematype = 'IDOC'.
          wa_bpiobj-schema_id = wa_idoc_control-mestyp.
          wa_bpiobj-workgroup_id = ls_protocol_msg_resp-workgroup_id.
          wa_bpiobj-subject_account_id = ls_protocol_msg_resp-subject_account_id.
          APPEND wa_bpiobj TO lt_updatedbpis.
        ELSE.
          GET TIME STAMP FIELD lv_timestamp.
          wa_bpiobj-baseline_id = ls_protocol_msg_resp-baseline_id. "To be provided by api
          wa_bpiobj-proof = ls_protocol_msg_resp-proof. "To be provided by api
          "wa_bpiobj-status = ''. "To be determined by api response
          wa_bpiobj-object_id = ls_protocol_msg_req-id.
          wa_bpiobj-created_by = sy-uname.
          wa_bpiobj-created_at = lv_timestamp.
          wa_bpiobj-schematype = 'IDOC'.
          wa_bpiobj-schema_id = wa_idoc_control-mestyp.
          wa_bpiobj-workgroup_id = ls_protocol_msg_resp-workgroup_id.
          wa_bpiobj-subject_account_id = ls_protocol_msg_resp-subject_account_id.
          APPEND wa_bpiobj TO lt_newbpis.
        ENDIF.
      ELSE. "log error message
      ENDIF.

    ENDLOOP.

    zcl_proubc_busobjhlpr=>validate_object_create(
      EXPORTING
        it_objects = lt_newbpis
      IMPORTING
        et_objects = lt_final_newbpis
    ).
    zcl_proubc_busobjhlpr=>create_object(
      EXPORTING
        it_objects = lt_final_newbpis
*      IMPORTING
*        et_objects =
    ).
    zcl_proubc_busobjhlpr=>validate_object_update(
      EXPORTING
        it_objects = lt_updatedbpis
      IMPORTING
        et_objects = lt_final_updatedbpis
    ).
    zcl_proubc_busobjhlpr=>update_object(
      EXPORTING
        it_objects = lt_final_updatedbpis
*      IMPORTING
*        et_objects =
    ).
    "TODO capture system messages for spooler/logging


  ENDMETHOD.


  METHOD zif_proubc_blidochlper~shuttle_idocs.
    "object_id  TYPE bpiobj-object_id,
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
      me->launch_idoc_to_baseline( EXPORTING iv_idoctp = iv_idoctp
                                             iv_idocmesty = iv_idocmestyp  ).
    ENDIF.

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
    comp_tab_parent_upd = ct_parent_comp_tab.

    "add a component to json type the child segment type
    comp_wa-name = lv_json_child_segmentid.
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

* grandkids! and n-level generation
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

    ASSIGN ls_json_child_segment->* TO <segmentdata_child>.
    ASSIGN COMPONENT lv_json_child_segmentid  OF STRUCTURE <segmentdata_b> TO <segment_type_b>.

    <segment_type_b> = <segmentdata_child>.
    cv_parent_segment = dataref_b.

  ENDMETHOD.


  METHOD generate_idoc_segdata.
    "create a variable of the data type of the segment
    DATA: lv_json_segment TYPE REF TO data.
    FIELD-SYMBOLS: <fs_json_segment> TYPE any.
    DATA(lv_segment_type) = cl_abap_classdescr=>describe_by_name( iv_segmenttype ). "cast #( iv_segmenttype ).
    DATA: lv_segment TYPE REF TO data.
    DATA: lv_segnumint TYPE i.
    DATA: lv_segnumstring   TYPE string,
          lv_segnam         TYPE string,
          lv_json_segmentid TYPE string.
    CREATE DATA lv_segment TYPE (iv_segmenttype).

    lv_segnumint = iv_rawsegment-segnum.
    lv_segnumstring = lv_segnumint.
    lv_segnam = iv_rawsegment-segnam.
    "replace all occurrences of '_' in lv_segnam with ''.
    "translate lv_segnam to lower case.
    SELECT SINGLE segnam FROM edid4 WHERE segnam = @lv_segnam INTO @DATA(lv_encodedsegnmam).
    REPLACE ALL OCCURRENCES OF '_' IN lv_encodedsegnmam WITH ''.
    CONCATENATE lv_encodedsegnmam lv_segnumstring INTO lv_json_segmentid SEPARATED BY '_'.
    DATA: lv_safestring TYPE char30.
    me->check_for_illegal_characters(
      EXPORTING
        iv_string             = lv_json_segmentid
      IMPORTING
*        EV_ILLEGAL_CHAR_INDEX =
*        EV_ILLEGAL_CHAR_FOUND =
        ev_safestring         =  lv_safestring
    ).
    ev_json_segmentid = lv_safestring.

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
    "struct_type = cl_abap_structdescr=>create( comp_tab ).
    cl_abap_structdescr=>create( "TODO discover wth this dumps on component name
      EXPORTING
        p_components = comp_tab
        p_strict     = ''
      RECEIVING
        p_result     = struct_type
    ).

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
    <segnum> = iv_rawsegment-segnum.
    ASSIGN COMPONENT 'PSGNUM' OF STRUCTURE <segmentdata> TO <psgnum>.
    <psgnum> = iv_rawsegment-psgnum.

    et_comp_tab = comp_tab.
    ev_json_segmentdata = dataref.

  ENDMETHOD.


  METHOD idoc_to_json.

    DATA: lv_flattened_idoc TYPE REF TO data.

    "get the idoc metadata
    DATA: lv_selectedbasictype TYPE string,
          ls_basictypes        TYPE zif_idocapi_typelist=>ty_basictype,
          ls_responsedata      TYPE zif_idocapi_typelist=>ty_basictype_w_segments,
          lv_idoc_type_in      TYPE ledid_idoctype,
          lv_idoc_type_out     TYPE ledid_idoc_type,
          lt_idoc_struct       TYPE ledid_t_idoc_struct,
          lt_segments          TYPE ledid_t_segment,
          lt_segment_struct    TYPE ledid_t_segment_struct.

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

    DATA: struct_type   TYPE REF TO cl_abap_structdescr,
          struct_type_b TYPE REF TO cl_abap_structdescr.


    LOOP AT lt_idoc_struct ASSIGNING <fs_idoc_struct> WHERE syntax_attrib-parseg IS INITIAL. "top level segments only
      "handle multiple children

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
          comp_wa-name = lv_json_segmentid. "TODO only handle up to 30 chars
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


  METHOD generate_segment_fields.
    DATA: ls_field_data TYPE zif_proubc_blidochlper=>ty_idoc_segment_field,
          lt_field_data TYPE zif_proubc_blidochlper=>tty_idoc_segment_field.

    LOOP AT it_target_segment_struct ASSIGNING FIELD-SYMBOL(<fs_target_segment>).
      "**** Field-level data ****
      "Position - sort the fields by this
      "## segmentstruct/field_attrib/position
      ls_field_data-position = <fs_target_segment>-field_attrib-position.
      "Fieldname
      "## segmentstruct/fieldname
      ls_field_data-fieldname = <fs_target_segment>-fieldname.
      "Field description
      "## segmentstruct/field_attrib/descrp
      ls_field_data-fielddescription = <fs_target_segment>-field_attrib-descrp.
      "ABAP Dictionary type
      "Length
      " ## segmentstruct/field_attrib/intlen
      ls_field_data-length = <fs_target_segment>-field_attrib-intlen.
      "Decimals ~ only applies to some numeric types
      "## segmentstruct/field_attrib/decimals
      "ls_field_data- TODO add decimals to the types

      "Bonus field level data (future enh)
      "value helper table
      "## segmentstruct/field_attrib/valuetab
      "offset ~ used for EDI-native handling
      APPEND ls_field_data TO lt_field_data.
      CLEAR ls_field_data.
    ENDLOOP.

    et_field_data = lt_field_data.

  ENDMETHOD.


  METHOD generate_child_segment_schema.
  ENDMETHOD.

  METHOD check_for_illegal_characters.
    DATA: lv_safestring    TYPE char30,
          lv_checkedstring TYPE string.
    IF iv_string CN 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789#$%&*-/;<=>?@^{|}'.
      "do something to fix it!
      lv_checkedstring = iv_string.
      me->build_safestring( EXPORTING iv_string = lv_checkedstring
                            IMPORTING ev_safestring = lv_safestring ).
      ev_safestring = lv_safestring.
    ELSE.
      DATA(lv_string_len) = strlen( lv_checkedstring ).
      IF lv_string_len GE 30.
        ev_safestring = iv_string(30).
      ELSE.
        ev_safestring = iv_string.
      ENDIF.
    ENDIF.

*  data: lv_strlen type i.
*        lv_strlen = strlen( iv_string ).
*  data: lv_offset type i.
*  lv_offset = lv_strlen.

  ENDMETHOD.
  METHOD build_safestring.
    DATA: it_split      TYPE TABLE OF string,
          it_safestring TYPE TABLE OF string,
          wa_safestring TYPE string.
    CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
      EXPORTING
        text        = iv_string
        line_length = 1
      TABLES
        text_tab    = it_split.
    LOOP AT it_split ASSIGNING FIELD-SYMBOL(<fs_split>).
      IF <fs_split> CO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789#$%&*-/;<=>?@^{|}'.
        wa_safestring = <fs_split>.
        APPEND wa_safestring TO it_safestring.
      ENDIF.
    ENDLOOP.
    DATA: lv_safestring TYPE string.
    CONCATENATE LINES OF it_safestring INTO lv_safestring.
    DATA(lv_safestring_len) = strlen( lv_safestring ).
    IF lv_safestring_len GE 30.
      ev_safestring = lv_safestring(30).
    ELSE.
      ev_safestring = lv_safestring.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
