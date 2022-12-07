*&---------------------------------------------------------------------*
*& Include zproubc_vault_test_top
*&---------------------------------------------------------------------*

 DATA:
        g_editor TYPE REF TO cl_gui_textedit,
        g_editor_container TYPE REF TO cl_gui_custom_container,
        g_editor2 TYPE REF TO cl_gui_textedit,
        g_editor_container2 TYPE REF TO cl_gui_custom_container,
*     other variables
        g_ok_code LIKE sy-ucomm,
        g_repid LIKE sy-repid.