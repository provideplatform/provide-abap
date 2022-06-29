class ZCL_PROUBC_API_HELPER definition
  public
  final
  create public .

public section.

  class-methods BUILD_REFRESH_TOKEN
    importing
      !IV_REFRESHTOKEN1 type STRING
      !IV_REFRESHTOKEN2 type STRING
    exporting
      !EV_TOKENLENGTH type INT4
      !EV_REFRESHTOKEN type zPRVDREFRESHTOKEN .
  class-methods COPY_DATA_TO_REF
    importing
      !IS_DATA type ANY
    changing
      !CR_DATA type ref to DATA .
  methods CONSTRUCTOR
    importing
      !IV_TENANT type zPRVDTENANTID optional .
  methods CALL_IDENT_API
    importing
      !IV_TENANT type zPRVDTENANTID
    exporting
      !EV_AUTHTOKEN type ref to DATA
      !STATUS type I
      !EV_BPIENDPOINT type STRING .
  methods CALL_BASELINE_API .
  methods AUTHENTICATE_IDENT_API_BASIC
    importing
      !IV_USERID type STRING
      !IV_PASSWORD type zCASESENSITIVE_STR
    exporting
      !AUTHTOKEN type ref to DATA .
  methods BASELINE_HEALTH_CHECK
    importing
      !IV_TENANT type zPRVDTENANTID
    exporting
      !EV_ISREACHABLE type BOOLEAN .
  methods SETUP_PROTOCOL_MSG
    exporting
      !SETUP_SUCCESS type BOOLEAN .
  methods SEND_PROTOCOL_MSG
    importing
      !BODY type ZIF_PROUBC_BASELINE=>PROTOCOLMESSAGE_REQ
    exporting
      !STATUSCODE type I
      !APIRESPONSESTR type STRING
      !APIRESPONSE type ref to DATA .
  methods SEND_BPIOBJECTS_MSG
    importing
      !BODY type ZIF_PROUBC_BASELINE=>BPIOBJECTS_REQ
    exporting
      !STATUSCODE type I
      !APIRESPONSESTR type STRING
      !APIRESPONSE type ref to DATA .
  methods CREATE_BUSINESSOBJECTS_MSG
    importing
      !BODY type ZIF_PROUBC_BASELINE=>BUSINESSOBJECT
    exporting
      !STATUSCODE type I
      !APIRESPONSESTR type STRING
      !APIRESPONSE type ref to DATA .
  methods GET_DEFAULT_TENANT
    returning
      value(EV_DEFAULTTENANT) type Zprvdtenants-tenant_ID .
  methods GET_DEFAULT_TENANT_BPIENDPOINT
    returning
      value(EV_BPIENDPOINT) type Zprvdtenants-BPI_ENDPOINT .
  methods BUILD_DUMMY_IDOC_PROTOCOL_MSG
    returning
      value(ES_DUMMY_IDOC_MSG) type ZIF_PROUBC_BASELINE=>PROTOCOLMESSAGE_REQ .
  methods LIST_BPI_ACCOUNTS .
  PROTECTED SECTION.
    DATA: lv_defaulttenant        TYPE zprvdtenants-tenant_id VALUE 'e41dea7b-3510-4ffa-8ff4-53f3b158c8b4',
          lv_defaultidenttoken    TYPE REF TO data,
          lv_defaultbaselinetoken TYPE REF TO data,
          lv_bpitoken             TYPE zprvdrefreshtoken,
          lv_default_bpiendpoint  TYPE string,
          lo_ident_client         TYPE REF TO zif_proubc_ident,
          lo_baseline_client      TYPE REF TO zif_proubc_baseline.
    METHODS: set_default_tenant IMPORTING iv_defaulttenant TYPE zprvdtenants-tenant_id OPTIONAL.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PROUBC_API_HELPER IMPLEMENTATION.


  METHOD BUILD_DUMMY_IDOC_PROTOCOL_MSG.
    DATA ls_dummy_idoc_protocol_msg TYPE zif_proubc_baseline=>protocolmessage_req.
    SELECT docnum,
       idoctp,
       status,
       credat,
       cretim,
       upddat,
       updtim
       FROM edidc
       UP TO 1 ROWS
       "inner join EDID4 as b on a~docnum = b~docnum
       INTO TABLE @DATA(lt_selected_idocs)
       WHERE direct = '1'
       AND status = '03'
       AND mestyp = 'ORDERS'
       AND idoctp = 'ORDERS05'.

    CHECK sy-subrc = 0.

    READ TABLE lt_selected_idocs INTO DATA(wa_selected_idoc) INDEX 1.

    DATA:
      lv_idocnum      TYPE edidc-docnum,
      lv_newidocnum   TYPE   edidc-docnum,
      lt_edids        TYPE TABLE OF edids,
      lt_edidd        TYPE TABLE OF edidd,
      wa_idoc_control TYPE edidc,
      lv_status       TYPE i.

    CLEAR: lt_edids, lt_edidd, lv_idocnum.


    CALL FUNCTION 'IDOC_READ_COMPLETELY'
      EXPORTING
        document_number = wa_selected_idoc-docnum
      IMPORTING
        idoc_control    = wa_idoc_control
      TABLES
        int_edids       = lt_edids
        int_edidd       = lt_edidd
      EXCEPTIONS
        OTHERS          = 1.

    ls_dummy_idoc_protocol_msg-payload_mimetype = 'json'.
    ls_dummy_idoc_protocol_msg-type = 'ORDERS05'.

    zcl_proubc_idochlpr=>get_DUMMY_objid( EXPORTING iv_schema = 'ORDERS05'
                   IMPORTING ev_objid = ls_dummy_idoc_protocol_msg-id
                             ev_newidocnum = lv_newidocnum
                    CHANGING ct_edidd = lt_edidd ).

    DATA: lv_idocjson TYPE string.
    lv_idocjson = /ui2/cl_json=>serialize(
       EXPORTING
         data             = lt_edidd
     ).
    ls_dummy_idoc_protocol_msg-payload = lv_idocjson.

    es_dummy_idoc_msg = ls_dummy_idoc_protocol_msg.
  ENDMETHOD.
ENDCLASS.
