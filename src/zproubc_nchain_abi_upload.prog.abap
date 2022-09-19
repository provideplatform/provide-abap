*&---------------------------------------------------------------------*
*& Report zproubc_nchain_abi_upload
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zproubc_nchain_abi_upload.

INCLUDE zproubc_nchain_abi_upload_top.
INCLUDE zproubc_nchain_abi_upload_f01.

START-OF-SELECTION.
  CREATE OBJECT lo_proubc_nchain_abi_upload.
  CALL SCREEN 100.

MODULE pbo OUTPUT.
  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR '001'.
  IF b_init IS INITIAL.

    "Valid from date picker
    CREATE OBJECT l_validfrom_container
      EXPORTING
        container_name = 'P_VALIDFROM_DATE'.

    CREATE OBJECT l_validfrom_calendar
      EXPORTING
        parent     = l_validfrom_container
        view_style = c_basiccalendar_style.
    "Valid from time entry
    "TODO

    "Valid to date picker
    CREATE OBJECT l_validto_container
      EXPORTING
        container_name = 'P_VALIDTO_DATE'.

    CREATE OBJECT l_validto_calendar
      EXPORTING
        parent     = l_validto_container
        view_style = c_basiccalendar_style.
    "Valid to time entry
    "TODO

    "ABI text preview area
    CREATE OBJECT l_abitext_container
      EXPORTING
        container_name = 'P_ABI_PREVIEW'.

    CREATE OBJECT l_abitext_area
      EXPORTING
        parent                     = l_abitext_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    b_init = 'X'.
  ENDIF.

ENDMODULE.

MODULE pai INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'BACK'.
      IF NOT b_init IS INITIAL.
        PERFORM exit_program.
      ENDIF.
    WHEN 'EXIT'.
      IF NOT b_init IS INITIAL.
        PERFORM exit_program.
      ENDIF.
    WHEN 'SAVE'.
      lo_proubc_nchain_abi_upload->save_data( ).
    WHEN 'LOAD'.
      "lo_proubc_nchain_abi_upload->loa
    WHEN 'UPLOAD'.

    WHEN 'DELETE'.

      lo_proubc_nchain_abi_upload->delete_abi( ).
    WHEN 'REFRESH'.
    WHEN 'CLEAR'.
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.
