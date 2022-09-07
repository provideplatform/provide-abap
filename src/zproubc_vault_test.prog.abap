*&---------------------------------------------------------------------*
*& Report zproubc_vault_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zproubc_vault_test.

INCLUDE zproubc_vault_test_top.
INCLUDE zproubc_vault_test_f01.

DATA: lo_vault_helper TYPE REF TO zcl_proubc_vault_helper.

START-OF-SELECTION.
  CALL SCREEN 100.
  "GET PARAMETER ID 'ZPRVDTENANT' FIELD 'prvdtenantid'.
  CREATE OBJECT lo_vault_helper.

*  lo_vault_helper->setup_protocol_msg( ).
*  "lo_vault_helper->setup_vault_msgs( ). "should it be this one or that one?
*
*  CASE abap_true.
*    WHEN rb1.
*      lo_vault_helper->list_vaults( ).
*    WHEN rb2.
*      lo_vault_helper->create_vault( ).
*    WHEN rb3.
*      "lo_vault_helper->unseal_vault
*    WHEN rb4.
*      "lo_vault_helper->delete_vault
*    WHEN rb5.
*      lo_vault_helper->create_key( ).
*    WHEN rb6.
*      "lo_vault_helper->unseal_key
*    WHEN rb7.
*      lo_vault_helper->delete_keys( ).
*    WHEN rb8.
*    WHEN rb9.
*    WHEN rb10.
*  ENDCASE.
*lo_vault_helper->list_keys( ).
*lo_vault_helper->create_key( ).
*lo_vault_helper->list_vaults( ).
*lo_vault_helper->create_vault( ).
*lo_vault_helper->derive_key( ).
*lo_vault_helper->encrypt( ).
*lo_vault_helper->decrypt(  ).
*lo_vault_helper->sign( ).
*lo_vault_helper->verify( ).

MODULE pbo OUTPUT.
  IF g_editor IS INITIAL.

*   set status
    SET PF-STATUS 'MAIN100'.

*   initilize local variable with sy-repid, since sy-repid doesn't work
*    as parameter directly.
    g_repid = sy-repid.

*   create control container
    CREATE OBJECT g_editor_container
      EXPORTING
        container_name              = 'VAULTINPUTTXT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
*      add your handling
    ENDIF.


*   create calls constructor, which initializes, creats and links
*    a TextEdit Control
    CREATE OBJECT g_editor
      EXPORTING
        parent                     = g_editor_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true
      EXCEPTIONS
        OTHERS                     = 1.
    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = g_repid
          txt2  = space
          txt1  = TEXT-001.
    ENDIF.


*   create control container
    CREATE OBJECT g_editor_container2
      EXPORTING
        container_name              = 'VAULTOUTPUTTXT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
*      add your handling
    ENDIF.


*   create calls constructor, which initializes, creats and links
*    a TextEdit Control
    CREATE OBJECT g_editor2
      EXPORTING
        parent                     = g_editor_container2
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true
      EXCEPTIONS
        OTHERS                     = 1.
    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = g_repid
          txt2  = space
          txt1  = TEXT-001.
    ENDIF.

  ENDIF.                               " Editor is initial

ENDMODULE.

MODULE pai INPUT.

  CASE g_ok_code.

    WHEN 'EXIT'.
      PERFORM exit_program.

    WHEN 'SAVE'.
*     retrieve table from control
*      CALL METHOD g_editor->get_text_as_r3table
*          IMPORTING
*              table = g_mytable
*          EXCEPTIONS
*              OTHERS = 1.
*      IF sy-subrc NE 0.
*        CALL FUNCTION 'POPUP_TO_INFORM'
*             EXPORTING
*                  titel = g_repid
*                  txt2  = space
*                  txt1  = text-003.
*      ENDIF.

*     if you would like to work with the table contents
*     perform a explicit flush here allthough the method
*     flushes internally (at least up to release 4.6D).
*     The reason: don't rely on internal flushes of control
*     wrappers. These might vanish in the future leading to a
*     malfunction of your transaction. The additional flush here
*     does no harm. The autmation queue is empty and NO additional
*     roundtrip to the frontend will be triggered.
      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc NE 0.
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = g_repid
            txt2  = space
            txt1  = TEXT-002.
      ENDIF.

    WHEN 'LOAD'.
**     send table to control
*      CALL METHOD g_editor->set_text_as_r3table
*          EXPORTING
*              table = g_mytable
*          EXCEPTIONS
*              OTHERS = 1.
*      IF sy-subrc NE 0.
*        CALL FUNCTION 'POPUP_TO_INFORM'
*             EXPORTING
*                  titel = g_repid
*                  txt2  = space
*                  txt1  = text-004.
*      ENDIF.

*   no flush here:
*   the automatic flush at the end of PBO does the job
    WHEN 'VAULT'.
      CASE ABAP_TRUE.
        when 'LISTVAULTS'.
          lo_vault_helper->list_vaults( ).
        WHEN 'CREATEVAULT'.

      ENDCASE.
    WHEN 'NCHAIN'.
    WHEN 'PRIVACY'.

  ENDCASE.

  CLEAR g_ok_code.
ENDMODULE.
