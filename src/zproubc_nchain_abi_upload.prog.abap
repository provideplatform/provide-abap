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
ENDMODULE.

MODULE pai INPUT.
    case ok_code.
        when 'SAVE'.
            lo_proubc_nchain_abi_upload->save_data( ).
        when 'LOAD'.
            "lo_proubc_nchain_abi_upload->loa
        WHEN 'DELETE'.
           "lo_proubc_nchain_abi_upload->zif_proubc_nchain_abi_upload~
        when 'REFRESH'.
    ENDCASE.
ENDMODULE.
