class ZCL_AF_CORE_CON_FILE_CO definition
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
      !IV_LOG_OBJECT type BALOBJ_D
      !IS_FILE_CO_PARAMS type ZAF_S_CON_CO_FILE_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
protected section.

  data GT_CONTENT type SOLIX_TAB .
  data GV_CONTENT_LENGTH type SO_OBJ_LEN .
  data GS_FILE_PARAMS type ZAF_S_CON_CO_FILE_PARAMS .
private section.

  data GS_FORMOUTPUT type FPFORMOUTPUT .
  data GV_SERVER_PATH type EPSDIRNAM .

  methods GET_SERVER_PATH
    raising
      ZCX_AF_CORE_OUTPUT .
ENDCLASS.



CLASS ZCL_AF_CORE_CON_FILE_CO IMPLEMENTATION.


  METHOD constructor.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Constructor
************************************************************************

    gs_file_params = is_file_co_params.

    gv_log_object = iv_log_object.

    zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                   iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_con
                                   iv_ext_ident = '> START - ZCL_AF_CORE_CON_CO_FILE' ).

  ENDMETHOD.


METHOD get_server_path.
************************************************************************
*&  Key           : AR-180523
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  select server path
************************************************************************

  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = gs_file_params-logical_filename "'ZBP_FROMSAP'
    IMPORTING
      file_name        = gv_server_path
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    " Logischer Dateiname & konnte nicht gefunden werden.
    zcx_af_core_output=>raise( iv_log_object = gv_log_object
                               iv_msgno      = '090'
                               iv_msgty      = 'E'
                               iv_msgv1      = CONV #( gs_file_params-logical_filename ) ).
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
************************************************************************

    zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                   iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_con
                                   iv_ext_ident = '> START - ZIF_AF_CORE_CON_FILE_CO->DELIVER' ).

    gs_formoutput = is_formoutput.

    " Logischen Dateinamen ermitteln
    get_server_path( ).

    " Pfad und Dateinamen zusammensetzen
    CONCATENATE gv_server_path gs_file_params-file_name INTO gv_server_path.

    " Anhand des Dteitypen die Quelldaten in die Datei übertragen
    CASE  gs_file_params-file_type.
      WHEN 'ASC'.
        " Datei auf dem Appl.-Server anlegen und öffnen
        OPEN DATASET gv_server_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
        LOOP AT gs_file_params-it_asc_text ASSIGNING FIELD-SYMBOL(<lv_asc_text>).
          TRANSFER <lv_asc_text> TO gv_server_path.
        ENDLOOP.
      WHEN 'PDF'.
        " Datei auf dem Appl.-Server anlegen und öffnen
        OPEN DATASET gv_server_path FOR OUTPUT IN BINARY MODE.
        TRANSFER gs_formoutput-pdf TO gv_server_path.
      WHEN OTHERS.
        " Datei auf dem Appl.-Server anlegen und öffnen
        OPEN DATASET gv_server_path FOR OUTPUT IN BINARY MODE.
        TRANSFER gs_formoutput-pdl TO gv_server_path.
    ENDCASE.

    " Datei auf dem Appl.-Server schließen
    CLOSE DATASET gv_server_path.

    zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                   iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_con
                                   iv_ext_ident = '> ENDE - ZIF_AF_CORE_CON_FILE_CO->DELIVER' ).

  ENDMETHOD.
ENDCLASS.
