class ZCL_AF_CORE_CON_PREVIEW definition
  public
  final
  create public .

public section.

  interfaces ZIF_AF_CORE_CONNECTOR .

  aliases DELIVER
    for ZIF_AF_CORE_CONNECTOR~DELIVER .

  data GV_LOG_OBJECT type BALOBJ_D .

  methods CONSTRUCTOR
    importing
      !IV_LOG_OBJECT type BALOBJ_D .
protected section.
private section.

  data GO_CUSTOM_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data GO_HTML_VIEWER type ref to CL_GUI_HTML_VIEWER .
  data GO_PDF_VIEWER type ref to CL_GUI_PDFVIEWER .
ENDCLASS.



CLASS ZCL_AF_CORE_CON_PREVIEW IMPLEMENTATION.


  METHOD constructor.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Hier im Konstruktor
*&
************************************************************************

    gv_log_object = iv_log_object.

    zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                   iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_con
                                   iv_ext_ident = '> START - ZCL_AF_CORE_CON_PREVIEW' ).

  ENDMETHOD.


  METHOD zif_af_core_connector~deliver.
************************************************************************
*&  Key           : AR-180420
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  DELIVER die Möglichkeit die PDF als XSTRING oder den Spool zu verarbeiten.
*&  In dem Spool sind zum Beispiel die PCL/Postscript/ZPL Daten zu finden.
************************************************************************

    CALL FUNCTION 'ISH_DALE_PDF_DISPLAY'
      EXPORTING
        i_pdf = is_formoutput-pdf
        title = 'Preview'.

  ENDMETHOD.
ENDCLASS.
