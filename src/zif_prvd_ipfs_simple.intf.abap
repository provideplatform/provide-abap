INTERFACE zif_prvd_ipfs_simple
  PUBLIC .

  "! Calls the /
  METHODS: add IMPORTING iv_binarystring     TYPE xstring
                         iv_contentstring    TYPE string
                         iv_ipfsprojid       TYPE string
                         iv_ipfsapikey       TYPE string
                         iv_filename         TYPE string
                         iv_filetype         TYPE string
                         iv_xcontentlength   TYPE i
                         iv_quiet               TYPE abap_bool OPTIONAL
                         iv_quieter             TYPE abap_bool OPTIONAL
                         iv_silent              TYPE abap_bool OPTIONAL
                         iv_progress            TYPE abap_bool OPTIONAL
                         iv_trickle             TYPE abap_bool OPTIONAL
                         iv_only_hash           TYPE abap_bool OPTIONAL
                         iv_wrap_with_directory TYPE abap_bool OPTIONAL
                         iv_chunker             TYPE string OPTIONAL
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
