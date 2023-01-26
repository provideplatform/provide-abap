*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZPRVDTABLMAINT
*   generation date: 01/26/2023 at 20:39:04
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZPRVDTABLMAINT     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
