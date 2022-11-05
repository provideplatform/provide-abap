*&---------------------------------------------------------------------*
*& Include zproubc_nchain_abi_upload_f01
*&---------------------------------------------------------------------*

CLASS lcl_proubc_nchain_abi_upload IMPLEMENTATION.
  METHOD get_data.
  ENDMETHOD.
  METHOD refresh_data.
  ENDMETHOD.
  METHOD save_data.
    me->zif_proubc_nchain_abi_upload~upload_abi_file( ).
    me->zif_proubc_nchain_abi_upload~register_abi_file(  ).
  ENDMETHOD.
  METHOD delete_abi.
    me->zif_proubc_nchain_abi_upload~delete_file( ).
    me->zif_proubc_nchain_abi_upload~delete_registry( ).
  ENDMETHOD.
  METHOD preview_abi.
    CASE iv_previewtype.
      WHEN 'D'.
      WHEN 'S'.
    ENDCASE.
  ENDMETHOD.
  METHOD new_abi.
  ENDMETHOD.
  METHOD load_abi.
    DATA: lv_rc TYPE i.
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Load ABI File from Desktop'
*    default_extension       =
*    default_filename        =
*    file_filter             =
*    with_encoding           =
*    initial_directory       =
*    multiselection          =
    CHANGING
      file_table              = it_filetable
      rc                      = lv_rc
*    user_action             =
*    file_encoding           =
*  EXCEPTIONS
*    file_open_dialog_failed = 1
*    cntl_error              = 2
*    error_no_gui            = 3
*    not_supported_by_gui    = 4
*    others                  = 5
          .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ELSE.
    READ TABLE it_filetable INTO ls_filetable INDEX 1.
    "p_file = ls_filetable-filename.
  ENDIF.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~upload_abi_file.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~verify_abi_file.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~register_abi_file.
    DATA: ls_existingentry TYPE zprvdabiregistry,
          ls_stagingentry  TYPE zprvdabiregistry.
    "validate entry
    SELECT SINGLE * FROM zprvdabiregistry
     INTO ls_existingentry
     WHERE nchain_networkid = ls_currententry-nchain_networkid
                                         AND smartcontract_address = ls_currententry-smartcontract_address.
    IF sy-subrc = 0.
      ls_stagingentry-nchain_networkid = ls_existingentry-nchain_networkid.
      ls_stagingentry-smartcontract_address = ls_existingentry-smartcontract_address.
      ls_stagingentry-changed_by = sy-uname.
      GET TIME STAMP FIELD ls_stagingentry-changed_on.
    ELSEIF sy-subrc = 4.
      ls_stagingentry-nchain_networkid = ls_currententry-nchain_networkid.
      ls_stagingentry-smartcontract_address = ls_currententry-smartcontract_address.
      ls_stagingentry-created_by = sy-uname.
      GET TIME STAMP FIELD ls_stagingentry-created_on.
    ELSE.
      "todo error handling/logging
    ENDIF.

    ls_stagingentry-valid_from = ls_currententry-valid_from.
    ls_stagingentry-valid_to = ls_currententry-valid_to.
    ls_stagingentry-abi_location = ls_currententry-abi_location.
    ls_stagingentry-production_mainnet = ls_currententry-production_mainnet.

    APPEND ls_stagingentry TO lt_update_staging.

    IF lt_update_staging IS NOT INITIAL.
      MODIFY zprvdabiregistry FROM TABLE lt_update_staging.
      IF sy-subrc = 0.
        ev_success = abap_true.
      ELSE.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~unregister_abi_file.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~delete_file.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~delete_registry.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~update_validto.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~update_validfrom.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~validate_registry.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~set_al11_abi_file_path.
    DATA: lv_selected_folder TYPE string.
    cl_gui_frontend_services=>directory_browse( EXPORTING window_title = 'Select ABI File Directory'
                                                  CHANGING selected_folder = lv_selected_folder ).
  ENDMETHOD.
ENDCLASS.

FORM exit_program.

* Destroy Control.
  IF NOT l_validto_calendar IS INITIAL.
    CALL METHOD l_validto_calendar->free
      EXCEPTIONS
          OTHERS = 1.
    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
                titel = g_repid
                txt2  = space
                txt1  = 'error destroying cal 1'.
    ENDIF.
*   free ABAP object also
    FREE l_validto_calendar.
  ENDIF.


* destroy container
  IF NOT  l_validto_container IS INITIAL.
    CALL METHOD l_validto_container->free
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
*         MESSAGE E002 WITH F_RETURN.
    ENDIF.
*   free ABAP object also
    FREE l_validto_container.
  ENDIF.

* Destroy Control.
  IF NOT l_validfrom_calendar IS INITIAL.
    CALL METHOD l_validfrom_calendar->free
      EXCEPTIONS
          OTHERS = 1.
    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
                titel = g_repid
                txt2  = space
                txt1  = 'error destroyin cal 2'.
    ENDIF.
*   free ABAP object also
    FREE l_validfrom_calendar.
  ENDIF.


* destroy container
  IF NOT  l_validfrom_container IS INITIAL.
    CALL METHOD  l_validfrom_container->free
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
*         MESSAGE E002 WITH F_RETURN.
    ENDIF.
*   free ABAP object also
    FREE  l_validfrom_container.
  ENDIF.

* Destroy Control.
  IF NOT l_abitext_area IS INITIAL.
    CALL METHOD l_abitext_area->free
      EXCEPTIONS
          OTHERS = 1.
    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
                titel = g_repid
                txt2  = space
                txt1  = 'err destroying text area'.
    ENDIF.
*   free ABAP object also
    FREE l_abitext_area.
  ENDIF.


* destroy container
  IF NOT l_abitext_container IS INITIAL.
    CALL METHOD l_abitext_container->free
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
*         MESSAGE E002 WITH F_RETURN.
    ENDIF.
*   free ABAP object also
    FREE l_abitext_container.
  ENDIF.

* finally flush
  CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
          OTHERS = 1.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = g_repid
              txt2  = space
              txt1  = 'exit error'.
  ENDIF.

  CLEAR: b_init.
  LEAVE PROGRAM.
ENDFORM.

DATA: lo_proubc_nchain_abi_upload TYPE REF TO lcl_proubc_nchain_abi_upload.
