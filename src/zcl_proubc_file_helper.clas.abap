CLASS zcl_proubc_file_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      get_smartcontract_abi IMPORTING !iv_nchain_networkid      TYPE zprvd_nchain_networkid
                                      !iv_smartcontract_address TYPE zproubc_smartcontract_addr
                            EXPORTING !ev_abi_str               TYPE zcasesensitive_str
                                      !ev_abi_data              TYPE REF TO data ,
      open_abiregistry IMPORTING !is_abi_registry TYPE zprvdabiregistry
                       EXPORTING !ev_filecontent  TYPE zcasesensitive_str ,
      open_file_generic IMPORTING !iv_file_location TYPE string
                        EXPORTING !ev_filecontent_x TYPE xstring
                                  !ev_filecontent   TYPE zcasesensitive_str,
      write_file_generic,
      transfer_file_to_ipfs IMPORTING iv_filecontent_x type xstring
                            EXPORTING ev_contentid type string,
      read_file_from_ipfs.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_proubc_file_helper IMPLEMENTATION.
  METHOD get_smartcontract_abi.
    DATA: ls_abi_registry     TYPE zprvdabiregistry,
          lv_abifile_path     TYPE zprvdabiregistry-abi_location,
          lt_abifile_contents TYPE zif_proubc_file=>tty_filecontent,
          lv_filestr          TYPE string,
          lv_abi_data         TYPE REF TO data.
    SELECT SINGLE * FROM zprvdabiregistry
       INTO @ls_abi_registry
       WHERE nchain_networkid = @iv_nchain_networkid
       AND smartcontract_address = @iv_smartcontract_address.
    IF sy-subrc = 0.
      zcl_proubc_file_helper=>open_abiregistry( EXPORTING is_abi_registry = ls_abi_registry
                                                IMPORTING ev_filecontent = lv_filestr ).
      ev_abi_str = lv_filestr.
      /ui2/cl_json=>deserialize( EXPORTING json = lv_filestr CHANGING data = lv_abi_data ).
      ev_abi_data = lv_abi_data.
    ELSE.
      MESSAGE e014(zclproubcmsg) WITH iv_nchain_networkid iv_smartcontract_address.
    ENDIF.
  ENDMETHOD.

  METHOD open_abiregistry.
    DATA: it_filecontent TYPE TABLE OF zif_proubc_file=>ty_filecontent.
    DATA: wa_tab TYPE REF TO data. "zif_proubc_file=>ty_filecontent.
    DATA: xstr TYPE xstring.
    OPEN DATASET is_abi_registry-abi_location FOR INPUT IN BINARY MODE.
    READ DATASET is_abi_registry-abi_location INTO xstr ACTUAL LENGTH DATA(bytes).
    CLOSE DATASET is_abi_registry-abi_location.
    cl_bcs_convert=>xstring_to_string(
    EXPORTING
      iv_xstr   = xstr
      iv_cp     =  1100                " SAP character set identification
    RECEIVING
      rv_string = DATA(lv_string)
    ).
    IF sy-subrc = 0.
      ev_filecontent = lv_string.
    ENDIF.
  ENDMETHOD.

  METHOD open_file_generic.
    OPEN DATASET iv_file_location FOR INPUT IN BINARY MODE.
    READ DATASET iv_file_location INTO ev_filecontent_x ACTUAL LENGTH DATA(bytes).
    CLOSE DATASET iv_file_location.
    cl_bcs_convert=>xstring_to_string(
    EXPORTING
      iv_xstr   = ev_filecontent_x
      iv_cp     =  1100                " SAP character set identification
    RECEIVING
      rv_string = DATA(lv_string)
    ).
    IF sy-subrc = 0.
      ev_filecontent = lv_string.
    ENDIF.
  ENDMETHOD.

  METHOD write_file_generic.
  ENDMETHOD.

  method read_file_from_ipfs.
  ENDMETHOD.

  method transfer_file_to_ipfs.
  ENDMETHOD.

ENDCLASS.
