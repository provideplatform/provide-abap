*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZPROUBCTABLMAINT
*   generation date: 10/13/2022 at 03:41:40
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZPROUBCTABLMAINT   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
