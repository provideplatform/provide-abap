*&---------------------------------------------------------------------*
*& Include zproubc_vault_test_f01
*&---------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*&      Form  EXIT_PROGRAM
*&---------------------------------------------------------------------*
FORM exit_program.
* Destroy Control.
  IF NOT g_editor IS INITIAL.
    CALL METHOD g_editor->free
      EXCEPTIONS
          OTHERS = 1.
    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
                titel = g_repid
                txt2  = space
                txt1  = text-012.
    ENDIF.
*   free ABAP object also
    FREE g_editor.
  ENDIF.


* destroy container
  IF NOT g_editor_container IS INITIAL.
    CALL METHOD g_editor_container->free
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
*         MESSAGE E002 WITH F_RETURN.
    ENDIF.
*   free ABAP object also
    FREE g_editor_container.
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
              txt1  = text-011.
  ENDIF.

  LEAVE PROGRAM.

ENDFORM.                               " EXIT_PROGRAM
