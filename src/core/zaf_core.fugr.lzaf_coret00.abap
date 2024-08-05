*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZAF_DOCUMENT_ID.................................*
DATA:  BEGIN OF STATUS_ZAF_DOCUMENT_ID               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAF_DOCUMENT_ID               .
CONTROLS: TCTRL_ZAF_DOCUMENT_ID
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZAF_EMAIL_SENDER................................*
DATA:  BEGIN OF STATUS_ZAF_EMAIL_SENDER              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAF_EMAIL_SENDER              .
CONTROLS: TCTRL_ZAF_EMAIL_SENDER
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZAF_LOGGING.....................................*
DATA:  BEGIN OF STATUS_ZAF_LOGGING                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAF_LOGGING                   .
CONTROLS: TCTRL_ZAF_LOGGING
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZAF_OUTPUT_CTRL.................................*
DATA:  BEGIN OF STATUS_ZAF_OUTPUT_CTRL               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAF_OUTPUT_CTRL               .
CONTROLS: TCTRL_ZAF_OUTPUT_CTRL
            TYPE TABLEVIEW USING SCREEN '0005'.
*.........table declarations:.................................*
TABLES: *ZAF_DOCUMENT_ID               .
TABLES: *ZAF_EMAIL_SENDER              .
TABLES: *ZAF_LOGGING                   .
TABLES: *ZAF_OUTPUT_CTRL               .
TABLES: ZAF_DOCUMENT_ID                .
TABLES: ZAF_EMAIL_SENDER               .
TABLES: ZAF_LOGGING                    .
TABLES: ZAF_OUTPUT_CTRL                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
