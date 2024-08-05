************************************************************************
*&  Name          : Logging Prüfung
*&  Transaction   : kleine
*&  Titel         : Logging Prüfung
*&  Request No.   : 180116-135917-AR
*&  Developer     : Achim Reichenberger
*&  E-Mail        : achim.reichenberger@also.com
*&  Creation Date : 17.07.2018
*&  Key           : ohne, da Neuanlage
************************************************************************
*&  Description (short)
*&  Pürfen, ob sich in der Tabelle ZAF_LOGGING aktive Logging Einträge
*&  befinden.
*&  Dieser Report läuft täglich als Job im PA1 System.
*&
*&  Check whether there are ZAF_LOGGING active logging entries in the table.
*&  This report runs daily as a job in the PA1 system.
************************************************************************
REPORT zaf_check_logging.

DATA: gt_logging   TYPE TABLE OF zaf_logging,
      gr_data      TYPE REF TO data,
      gs_tline     TYPE tline,
      gv_subject   TYPE sood-objdes,
      gt_body_text TYPE tline_tab.

FIELD-SYMBOLS: <it_tab> TYPE ANY TABLE.

PARAMETERS: p_to TYPE ad_smtpadr OBLIGATORY DEFAULT 'DEITBusinessProcesses@also.com'.

*----------------------------------------------------------------------*
*   START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  SELECT * FROM zaf_logging INTO TABLE gt_logging.

  CHECK lines( gt_logging ) > 0.

  GET REFERENCE OF gt_logging INTO gr_data.

* Bodytext Tabelle füllen
* Fill body text table
  gs_tline-tdline = 'Hallo BP-Team,'.
  gs_tline-tdformat = '*'.
  APPEND gs_tline TO gt_body_text.
  gs_tline-tdline = zcl_af_core_constants=>gc_cr_lf.
  gs_tline-tdformat = '*'.
  APPEND gs_tline TO gt_body_text.
  gs_tline-tdline = 'im Anhang befindet sich der Tabelleninhalt'.
  gs_tline-tdformat = '*'.
  APPEND gs_tline TO gt_body_text.
  gs_tline-tdline = zcl_af_core_constants=>gc_cr_lf.
  gs_tline-tdformat = '*'.
  APPEND gs_tline TO gt_body_text.
  gs_tline-tdline = 'Freundliche Grüße'.
  gs_tline-tdformat = '*'.
  APPEND gs_tline TO gt_body_text.
  gs_tline-tdline = 'Euer Job/Report ZAF_CHECK_LOGGING'.
  gs_tline-tdformat = '*'.
  APPEND gs_tline TO gt_body_text.

  " Betreff darf nur 50 Zeichen haben :) kein Witz...Oh SAP / Subject can only have 50 characters :) no joke... Oh SAP
  "            12345678901234567890123456789012345678901234567890
  gv_subject = 'In Tab. ZAF_LOGGING befinden sich Einträge im'.

  CONCATENATE gv_subject sy-sysid INTO gv_subject SEPARATED BY space.

  CALL METHOD zcl_af_core_con_email_backgr=>send_mail_easy
    EXPORTING
      im_c_sender            = zcl_af_core_constants=>gc_noreply_sender
      im_c_receiver          = p_to
      im_c_subject           = gv_subject
      im_it_body_text        = gt_body_text
      im_b_commit            = abap_true
      im_rf_it_att_data      = gr_data
      im_c_att_name          = 'ZAF_LOGGING'
      im_c_att_type          = 'CSV'
      im_c_att_headline_type = 'L'.


*----------------------------------------------------------------------*
*   END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
