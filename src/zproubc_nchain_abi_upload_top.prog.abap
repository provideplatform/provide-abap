*&---------------------------------------------------------------------*
*& Include zproubc_nchain_abi_upload_top
*&---------------------------------------------------------------------*

DATA: ok_code LIKE sy-ucomm,
      g_repid LIKE sy-repid,
      b_init type c,
      l_validto_calendar TYPE REF TO cl_gui_calendar,
      l_validfrom_calendar TYPE REF TO cl_gui_calendar,
      l_abitext_area type ref to  cl_gui_textedit,
      l_validto_container TYPE REF TO cl_gui_custom_container,
      l_validfrom_container TYPE REF TO cl_gui_custom_container,
      l_abitext_container type ref to cl_gui_custom_container,
      it_filetable TYPE filetable,
      ls_filetable LIKE LINE OF it_filetable.

CONSTANTS: c_basiccalendar_style             TYPE i VALUE 1,
           C_DEFAULT_ABI_DIR                TYPE STRING VALUE '/usr/sap/S4H/D00/data'.

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
