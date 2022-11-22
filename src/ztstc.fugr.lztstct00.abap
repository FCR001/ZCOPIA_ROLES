*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTSTC...........................................*
DATA:  BEGIN OF STATUS_ZTSTC                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTSTC                         .
CONTROLS: TCTRL_ZTSTC
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTSTC                         .
TABLES: ZTSTC                          .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
