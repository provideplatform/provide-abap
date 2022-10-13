*---------------------------------------------------------------------*
*    view related data declarations
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
