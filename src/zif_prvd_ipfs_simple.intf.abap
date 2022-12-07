INTERFACE zif_prvd_ipfs_simple
  PUBLIC .

  METHODS: add IMPORTING iv_binarystring     TYPE xstring
                         iv_contentstring    type string
                         iv_ipfsprojid       type string
                         iv_ipfsapikey       type string
                         iv_filename         type string
                         iv_filetype         type string
                         iv_xcontentlength   type i
                         iv_quiet               TYPE abap_bool optional
                         iv_quieter             TYPE abap_bool OPTIONAL
                         iv_silent              TYPE abap_bool OPTIONAL
                         iv_progress            TYPE abap_bool OPTIONAL
                         iv_trickle             TYPE abap_bool OPTIONAL
                         iv_only_hash           TYPE abap_bool OPTIONAL
                         iv_wrap_with_directory TYPE abap_bool OPTIONAL
                         iv_chunker             TYPE string optional
                         iv_pin                 TYPE abap_bool OPTIONAL
                         iv_raw_leaves          TYPE abap_bool OPTIONAL
                         iv_nocopy              TYPE abap_bool OPTIONAL
                         iv_fscache             TYPE abap_bool OPTIONAL
                         iv_cid_version         TYPE i     OPTIONAL
                         iv_hash                TYPE string OPTIONAL
                         iv_inline              TYPE abap_bool OPTIONAL
                         iv_inline_limit        TYPE i OPTIONAL
               EXPORTING
                         ev_apiresponsestr   TYPE string
                         ev_apiresponse      TYPE REF TO data
                         ev_httpresponsecode TYPE i
               RAISING
                         cx_static_check.

ENDINTERFACE.
