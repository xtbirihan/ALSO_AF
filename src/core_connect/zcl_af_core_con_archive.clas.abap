class ZCL_AF_CORE_CON_ARCHIVE definition
  public
  final
  create public .

public section.

  interfaces ZIF_AF_CORE_CONNECTOR .

  aliases DELIVER
    for ZIF_AF_CORE_CONNECTOR~DELIVER .

  types:
    TY_IT_PDF_DATA TYPE STANDARD TABLE OF TBL1024 .

  data GV_LOG_OBJECT type BALOBJ_D .

  methods CONSTRUCTOR
    importing
      !IV_LOG_OBJECT type BALOBJ_D
      !IS_ARCHIVE_PARAMS type ZAF_S_CON_ARCHIVE_PARAMS .
protected section.

  data GS_ARCHIVE_PARAMS type ZAF_S_CON_ARCHIVE_PARAMS .
  data GS_FORMOUTPUT type FPFORMOUTPUT .
  data GV_PDF_LENGTH type NUM12 .
  data GT_PDF type TY_IT_PDF_DATA .
  data GV_ARC_DOC_ID type SAPB-SAPADOKID .
private section.

  methods ARCHIVE_DOCUMENT
    raising
      ZCX_AF_CORE_OUTPUT .
  methods INSERT_CONNECTION
    raising
      ZCX_AF_CORE_OUTPUT .
  methods SEND_ADMIN_EMAIL .
ENDCLASS.



CLASS ZCL_AF_CORE_CON_ARCHIVE IMPLEMENTATION.


  METHOD archive_document.
************************************************************************
* &  Key           : RP-180704
* &  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
* ***********************************************************************
* &  Description (short)
* &  Ablegen des Archivdokuments
* &  Storing the archive document
* ***********************************************************************


    DATA: lt_body_text TYPE tline_tab.
    DATA: ls_tline     TYPE tline.

    DATA: lv_msgv1 TYPE zcl_af_core_messages=>ts_msg-msgv1.

    CALL FUNCTION 'ARCHIVOBJECT_CREATE_TABLE'
      EXPORTING
        archiv_id                = gs_archive_params-archiv_id  " PFLICHT
        document_type            = 'PDF'                            " PFLICHT
        length                   = gv_pdf_length                   " NOTWENDIG
      IMPORTING
        archiv_doc_id            = gv_arc_doc_id                   " Wichtig für Tabelleneintrag
      TABLES
        binarchivobject          = gt_pdf                       " PDF als binary Tab.
      EXCEPTIONS
        error_archiv             = 1
        error_communicationtable = 2
        error_kernel             = 3
        blocked_by_policy        = 4
        OTHERS                   = 5.

    IF sy-subrc <> 0.

      send_admin_email( ).

      DATA(lo_arc_data) = NEW zcl_af_core_archive_data( iv_archiv_id     = gs_archive_params-archiv_id
                                                        iv_document_type = 'PDF'
                                                        iv_length        = gv_pdf_length
                                                        iv_object_id     = gs_archive_params-object_id
                                                        it_pdf           = gt_pdf
                                                        iv_arc_date      = sy-datum
                                                        iv_ar_object     = gs_archive_params-ar_object
                                                        iv_sap_object    = gs_archive_params-sap_object
                                                        iv_uname         = sy-uname ).

      WRITE sy-subrc TO lv_msgv1 LEFT-JUSTIFIED.
      zcx_af_core_output=>raise( iv_log_object = gv_log_object
                                 iv_msgno      = '085'
                                 iv_msgty      = 'E'
                                 iv_msgv1      = CONV #( lv_msgv1 ) ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Konstruktor
*&  Constructor
************************************************************************

    gs_archive_params = is_archive_params.

    gv_log_object = iv_log_object.

    zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                   iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_con
                                   iv_ext_ident = '> START - ZCL_AF_CORE_CON_ARCHIVE' ).

  ENDMETHOD.


  METHOD insert_connection.
************************************************************************
*&  Key           : RP-180704
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Erstellt Tabelleneintrag in entsprechender Archivtabelle mit
*&  Verbindung zum archivierten Dokument
*& Creates table entry in corresponding archive table with
*& Connection to the archived document
************************************************************************


    CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
      EXPORTING
        archiv_id             = gs_archive_params-archiv_id     " optional, wichtig aus Z-Tabelle
        arc_doc_id            = gv_arc_doc_id                   " PFLICHT, Dokumentanablage
        ar_date               = sy-datum                        " optional, sy-datum
        ar_object             = gs_archive_params-ar_object     " PFLICHT, Object type to be archived -> NACE->Nachricht->Ablagesystem->Doumentart -> ZSDO04FAKT
        object_id             = gs_archive_params-object_id     " PFLICHT, Unique object key of application (häufig Belegnummer) -> 10 stellige Rechnungsnummer (zB)
        sap_object            = gs_archive_params-sap_object    " PFLICHT, Calling application -> VBRK
        doc_type              = 'PDF'                           " notwendig, PDF
        mandant               = sy-mandt                        " notwendig, sy-mandt
        creator               = sy-uname
      EXCEPTIONS
        error_connectiontable = 1
        OTHERS                = 2.

    IF sy-subrc = 0.
      "Success handling
    ELSE.
      zcx_af_core_output=>raise( iv_log_object = gv_log_object
                                 iv_msgno      = '086'
                                 iv_msgty      = 'E'
                                 iv_msgv1      = CONV #( sy-subrc ) ).
    ENDIF.

  ENDMETHOD.


  METHOD send_admin_email.
************************************************************************
* &  Key           : AR-181019
* &  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
* ***********************************************************************
* &  Description (short)
* &  Bei Fehler E-Mail an Admins senden
* &  Send email to admins in case of error
* ***********************************************************************

    DATA: lt_body_text TYPE tline_tab,
          ls_tline     TYPE tline,
          lv_receiver  TYPE ad_smtpadr,
          lv_subject   TYPE string.

    SELECT COUNT(*) FROM zaf_archive_err UP TO 1 ROWS.
    IF sy-subrc <> 0.
      TRY.
          lv_receiver = 'sapbc@also.com'.
          lv_subject  = 'Adobe Forms: Archivproblems EWM system'.

          ls_tline-tdline = 'Hello Administrators,' .
          ls_tline-tdformat = '*'.
          APPEND ls_tline TO lt_body_text.
          ls_tline-tdline = 'As soon as the archive system is running.'.
          ls_tline-tdformat = '*'.
          APPEND ls_tline TO lt_body_text.
          ls_tline-tdline = 'Please start the Program ZAF_REPRODUCE_ARCHIV_DATA'.
          ls_tline-tdformat = '*'.
          APPEND ls_tline TO lt_body_text.
          ls_tline-tdline = 'to reproduce the documents once again.' .
          ls_tline-tdformat = '*'.
          APPEND ls_tline TO lt_body_text.
          ls_tline-tdline = 'Best regards BP-Team' .
          ls_tline-tdformat = '*'.
          APPEND ls_tline TO lt_body_text.

          zcl_core_email_background=>send_mail_easy( iv_sender     = zcl_af_core_constants=>gc_noreply_sender
                                                        iv_receiver   = lv_receiver
                                                        iv_subject    = lv_subject
                                                        it_body_text    = lt_body_text ).

        CATCH zcx_core_email_background.
        CATCH zcx_core_email_attachment.
        CATCH cx_address_bcs.
        CATCH cx_send_req_bcs.
        CATCH cx_bcs.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_af_core_connector~deliver.
************************************************************************
*&  Key           : AR-180420
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  DELIVER die Möglichkeit die PDF als XSTRING oder den Spool zu verarbeiten.
*&  In dem Spool sind zum Beispiel die PCL/Postscript/ZPL Daten zu finden.
*&
*& DELIVER the possibility to process the PDF as XSTRING or the spool.
*& In the spool, for example, the PCL/Postscript/ZPL data can be found.
************************************************************************

    DATA lv_arc_doc_id TYPE sapb-sapadokid.
    DATA lt_pdf_bin    TYPE TABLE OF solix.
    DATA lv_length     TYPE num12.
    DATA lt_pdf        TYPE TABLE OF tbl1024.

    gs_formoutput = is_formoutput.

    gv_pdf_length = xstrlen( gs_formoutput-pdf ).

    TRY.
        cl_bcs_convert=>xstring_to_xtab( EXPORTING iv_xstring = gs_formoutput-pdf
                                         IMPORTING et_xtab    = gt_pdf ).
      CATCH cx_bcs.
        zcx_af_core_output=>raise( iv_log_object = gv_log_object
                                   iv_msgno      = '088'
                                   iv_msgty      = 'E' ).
    ENDTRY.

    archive_document( ).

    insert_connection( ).

  ENDMETHOD.
ENDCLASS.
