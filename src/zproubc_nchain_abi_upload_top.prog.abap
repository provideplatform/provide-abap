*&---------------------------------------------------------------------*
*& Include zproubc_nchain_abi_upload_top
*&---------------------------------------------------------------------*

DATA: ok_code LIKE sy-ucomm.

CLASS lcl_proubc_nchain_abi_upload DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_proubc_nchain_abi_upload.
    METHODS: get_data,
      refresh_data,
      save_data EXPORTING ev_success TYPE abap_bool,
      new_abi,
      load_abi,
      delete_abi,
      preview_abi IMPORTING iv_abi_al11_path TYPE string
                            iv_previewtype   TYPE c.
  PROTECTED SECTION.
    DATA: ls_currententry   TYPE zif_proubc_nchain_abi_upload=>ty_abi_registry,

          lt_update_staging TYPE TABLE OF zprvdabiregistry.
  PRIVATE SECTION.
ENDCLASS.
