CLASS zcl_proubc_file_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      "! Retrieves a EVM smart contract ABI from the registry + AL11 based on the smart contract address and network ID
      get_smartcontract_abi IMPORTING !iv_nchain_networkid      TYPE zprvd_nchain_networkid
                                      !iv_smartcontract_address TYPE zproubc_smartcontract_addr
                            EXPORTING !ev_abi_str               TYPE zcasesensitive_str
                                      !ev_abi_data              TYPE REF TO data ,
      "! Retrieves the ABI file contents based upon the ABI registry table entry
      open_abiregistry IMPORTING !is_abi_registry TYPE zprvdabiregistry
                       EXPORTING !ev_filecontent  TYPE zcasesensitive_str ,
      "! General purpose method to open a file from AL11
      open_file_generic IMPORTING !iv_file_location TYPE string
                        EXPORTING !ev_filecontent_x TYPE xstring
                                  !ev_filecontent   TYPE zcasesensitive_str
                                  !ev_length        TYPE i,
      "! general purpose method to write a file to AL11
      write_file_generic IMPORTING iv_filename    TYPE string
                                   iv_filecontent TYPE xstring
                                   iv_directory   TYPE string,
      "! Transers file content to IPFS
      transfer_file_to_ipfs IMPORTING iv_filecontent_x  TYPE xstring
                                      iv_filecontent    TYPE string
                                      iv_filename       TYPE string
                                      iv_filetype       TYPE string
                                      iv_ipfsprojid     TYPE string
                                      iv_ipfsapikey     TYPE string
                                      iv_xcontentlength TYPE i
                            EXPORTING ev_contentid      TYPE string,
      "! Reads a file from IPFS for a given content ID hash
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
                                                IMPORTING ev_filecontent  = lv_filestr ).
      ev_abi_str = lv_filestr.
      /ui2/cl_json=>deserialize( EXPORTING json = lv_filestr CHANGING data = lv_abi_data ).
      ev_abi_data = lv_abi_data.
    ELSE.
      MESSAGE e014(zclproubcmsg) WITH iv_nchain_networkid iv_smartcontract_address.
    ENDIF.
  ENDMETHOD.

  METHOD open_abiregistry.
    DATA: it_filecontent TYPE TABLE OF zif_proubc_file=>ty_filecontent.
    DATA: wa_tab TYPE REF TO data.
    DATA: xstr TYPE xstring.
    OPEN DATASET is_abi_registry-abi_location FOR INPUT IN BINARY MODE.
    READ DATASET is_abi_registry-abi_location INTO xstr ACTUAL LENGTH DATA(bytes).
    CLOSE DATASET is_abi_registry-abi_location.
    cl_bcs_convert=>xstring_to_string(
    EXPORTING
      iv_xstr   = xstr
      iv_cp     =  1100
    RECEIVING
      rv_string = DATA(lv_string)
    ).
    IF sy-subrc = 0.
      ev_filecontent = lv_string.
    ENDIF.
  ENDMETHOD.

  METHOD open_file_generic.
    OPEN DATASET iv_file_location FOR INPUT IN BINARY MODE.
    READ DATASET iv_file_location INTO ev_filecontent_x ACTUAL LENGTH DATA(lv_bytes).
    CLOSE DATASET iv_file_location.
    cl_bcs_convert=>xstring_to_string(
    EXPORTING
      iv_xstr   = ev_filecontent_x
      iv_cp     =  1100
    RECEIVING
      rv_string = DATA(lv_string)
    ).
    IF sy-subrc = 0.
      ev_filecontent = lv_string.
      ev_length = lv_bytes.
    ENDIF.

    OPEN DATASET iv_file_location FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    READ DATASET iv_file_location INTO ev_filecontent.
    CLOSE DATASET iv_file_location.

  ENDMETHOD.

  METHOD write_file_generic.
  ENDMETHOD.

  METHOD read_file_from_ipfs.
  ENDMETHOD.

  METHOD transfer_file_to_ipfs.
    DATA:
      lo_http_client      TYPE REF TO if_http_client,
      lo_prvd_ipfs_simple TYPE REF TO zcl_prvd_ipfs_simple.

    DATA:
      lv_xstr          TYPE xstring,
      lv_contentstring TYPE string,
      lv_rawbinary     TYPE string,
      lv_i             TYPE i,
      lv_ipfs_url      TYPE string.

    DATA: lv_ipfs_add_code TYPE i,
      lv_ipfs_add_resp TYPE string,
      lv_ipfs_add_data TYPE REF TO data.
    FIELD-SYMBOLS: <fs_contentid>     TYPE any,
                   <fs_contentid_str> TYPE string.

    lv_xstr = iv_filecontent_x.
    lv_rawbinary = lv_xstr.
    lv_contentstring = iv_filecontent.

    lv_ipfs_url = 'https://ipfs.infura.io:5001'.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = lv_ipfs_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3 ).

    lo_prvd_ipfs_simple = NEW zcl_prvd_ipfs_simple( ii_client   = lo_http_client
                                                    iv_ipfs_url = lv_ipfs_url ).

    lo_prvd_ipfs_simple->zif_prvd_ipfs_simple~add(
      EXPORTING
        iv_binarystring        = lv_xstr
        iv_contentstring       = lv_rawbinary
        iv_filename            = iv_filename
        iv_filetype            = iv_filetype
        iv_ipfsprojid           = iv_ipfsprojid
        iv_ipfsapikey           = iv_ipfsapikey
        iv_xcontentlength       = iv_xcontentlength
*        iv_quiet               =
*        iv_quieter             =
*        iv_silent              =
*        iv_progress            =
*        iv_trickle             =
*        iv_only_hash           =
*        iv_wrap_with_directory =
*        iv_chunker             =
*        iv_pin                 =
*        iv_raw_leaves          =
*        iv_nocopy              =
*        iv_fscache             =
*        iv_cid_version         =
*        iv_hash                =
*        iv_inline              =
*        iv_inline_limit        =
      IMPORTING
        ev_apiresponsestr      = lv_ipfs_add_resp
        ev_apiresponse         = lv_ipfs_add_data
        ev_httpresponsecode    = lv_ipfs_add_code
    ).
    IF lv_ipfs_add_code EQ 200.
      ASSIGN lv_ipfs_add_data->* TO FIELD-SYMBOL(<fs_ipfsresp>).
      ASSIGN COMPONENT 'NAME' OF STRUCTURE <fs_ipfsresp> TO <fs_contentid>.
      ASSIGN <fs_contentid>->* TO <fs_contentid_str>.
      ev_contentid = <fs_contentid_str>.
    ENDIF.


  ENDMETHOD.

ENDCLASS.
