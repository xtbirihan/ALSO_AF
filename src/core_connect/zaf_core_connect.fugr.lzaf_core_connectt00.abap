*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZAF_OUT_CHANNEL.................................*
DATA:  BEGIN OF STATUS_ZAF_OUT_CHANNEL               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAF_OUT_CHANNEL               .
CONTROLS: TCTRL_ZAF_OUT_CHANNEL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZAF_OUT_CHANNEL               .
TABLES: ZAF_OUT_CHANNEL                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
