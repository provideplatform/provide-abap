*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 01/26/2023 at 20:39:06
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZPRVDABIREGISTRY................................*
DATA:  BEGIN OF STATUS_ZPRVDABIREGISTRY              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPRVDABIREGISTRY              .
CONTROLS: TCTRL_ZPRVDABIREGISTRY
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPRVDABIREGISTRY              .
TABLES: ZPRVDABIREGISTRY               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
