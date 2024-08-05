class ZCL_AF_CORE_CON_PRINT_PCL definition
  public
  final
  create public .

public section.

  interfaces ZIF_AF_CORE_CONNECTOR .

  aliases DELIVER
    for ZIF_AF_CORE_CONNECTOR~DELIVER .

  data GO_LOGGER type ref to ZCL_AF_CORE_LOGGER .
  data GV_LOG_OBJECT type BALOBJ_D .

  methods CONSTRUCTOR
    importing
      !IM_LOG_OBJECT type BALOBJ_D
      !IM_WA_PRINT_PDL_PARAMS type ZAF_S_CON_PRINT_PDL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
protected section.

  data GS_PRINT_PARAMS type ZAF_S_CON_PRINT_PDL_PARAMS .
  data GV_HANDLE type SY-TABIX .
  data GV_SPOOLID type TSP01-RQIDENT .
  data GV_ADSPART type ADSPART .
  data GV_DESTFILE type TEXT1024 .
private section.

  methods OPEN_CONNECTION
    raising
      ZCX_AF_CORE_OUTPUT .
  methods GET_ADS_PATH
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CONFIRM_PRINT
    importing
      !IS_FORMOUTPUT type FPFORMOUTPUT
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CLOSE_CONECTION
    raising
      ZCX_AF_CORE_OUTPUT .
  methods TRANSFER_PDL
    importing
      !IM_PDL_XSTRING type XSTRING
    raising
      ZCX_AF_CORE_OUTPUT .
ENDCLASS.



CLASS ZCL_AF_CORE_CON_PRINT_PCL IMPLEMENTATION.


  METHOD close_conection.
************************************************************************
*&  Key           : RP-180509
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Close print spool
************************************************************************


    CALL FUNCTION 'ADS_SR_CLOSE'
      EXPORTING
        handle = gv_handle
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc NE 0.
      zcx_af_core_output=>raise( iv_log_object = gv_log_object
                                 iv_msgno      = '106'
                                 iv_msgty      = 'E' ).
    ENDIF.

  ENDMETHOD.


  METHOD confirm_print.
************************************************************************
*&  Key           : RP-180509
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Confirm the printing
************************************************************************


    DATA lv_filesize TYPE i.

    lv_filesize = xstrlen( is_formoutput-pdf ).
    CALL FUNCTION 'ADS_SR_CONFIRM'
      EXPORTING
        handle   = gv_handle
        partname = gv_adspart
        size     = lv_filesize
        pages    = is_formoutput-pages
        no_pdf   = abap_true
      EXCEPTIONS
        OTHERS   = 1.

    IF sy-subrc NE 0.
      zcx_af_core_output=>raise( iv_log_object = gv_log_object
                                 iv_msgno      = '105'
                                 iv_msgty      = 'E' ).
    ENDIF.

  ENDMETHOD.


  METHOD CONSTRUCTOR.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Hier im Konstruktor
*&
************************************************************************

    DATA: l_rf_msg TYPE REF TO zcl_actebis_messages.

    me->log_object = im_log_object.

    CALL METHOD zcl_af_core_logger=>write_log
      EXPORTING
        im_object    = im_log_object
        im_subobject = zcl_af_core_constants=>co_log_subobj_core_con
        im_ext_ident = '> START - ZCL_AF_CORE_CON_PRINT_PS'.

    me->wa_print_params = im_wa_print_pdl_params.

    " Strukturwerte mit Namen in eim Meldungsobjekt hinzufügen
    CALL METHOD zcl_af_core_util=>transfer_struc_val_to_msg_obj
      EXPORTING
        im_wa_structure = me->wa_print_params
      RECEIVING
        re_rf_msg       = l_rf_msg.

    " Log wegschreiben
    CALL METHOD zcl_af_core_logger=>write_log
      EXPORTING
        im_object    = im_log_object
        im_subobject = zcl_af_core_constants=>co_log_subobj_core_con
        im_ext_ident = 'CHANNEL_PARAMS gelesen'
        im_rf_msg    = l_rf_msg.

    CALL METHOD l_rf_msg->reset( ).


  ENDMETHOD.


  METHOD GET_ADS_PATH.
************************************************************************
*&  Key           : RP-180509
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Ermittelt den Dokumentenpfad auf dem ADS
*&
************************************************************************


    DATA l_globaldir TYPE text1024.

    CALL FUNCTION 'ADS_GET_PATH'
      IMPORTING
        ads_path = l_globaldir.

    CONCATENATE l_globaldir '/' me->adspart '.pdf' INTO me->destfile.

  ENDMETHOD.


  METHOD OPEN_CONNECTION.
************************************************************************
*&  Key           : RP-180509
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Erstellt einen temporären Spoolauftrag
*&
************************************************************************


    CALL FUNCTION 'ADS_SR_OPEN'
      EXPORTING
        dest            = me->wa_print_params-dest
        doctype         = 'ADSP'
        immediate_print = me->wa_print_params-reqimm "abap_true
        auto_delete     = me->wa_print_params-reqdel "abap_true
      IMPORTING
        handle          = me->handle
        spoolid         = me->spoolid
        partname        = me->adspart
      EXCEPTIONS
        OTHERS          = 1.
    IF sy-subrc NE 0.
      CALL METHOD zcx_af_core_output=>raise
        EXPORTING
          im_msgno      = '101'
          im_msgty      = 'E'
          im_msgv1      = CONV #( me->wa_print_params-dest )
          im_log_object = me->log_object.
    ENDIF.

  ENDMETHOD.


  METHOD TRANSFER_PDL.
************************************************************************
*&  Key           : RP-180620
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&
************************************************************************


    OPEN DATASET me->destfile FOR OUTPUT IN BINARY MODE.

    IF sy-subrc NE 0.
      CALL METHOD zcx_af_core_output=>raise
        EXPORTING
          im_log_object = me->log_object
          im_msgno      = '102'
          im_msgty      = 'E'
          im_msgv1      = CONV #( me->destfile ).

    ENDIF.

    TRANSFER im_pdl_xstring TO me->destfile.

    IF sy-subrc NE 0.
      CALL METHOD zcx_af_core_output=>raise
        EXPORTING
          im_log_object = me->log_object
          im_msgno      = '103'
          im_msgty      = 'E'
          im_msgv1      = CONV #( me->destfile ).
    ENDIF.

    CLOSE DATASET me->destfile.

    IF sy-subrc NE 0.
      CALL METHOD zcx_af_core_output=>raise
        EXPORTING
          im_log_object = me->log_object
          im_msgno      = '104'
          im_msgty      = 'E'
          im_msgv1      = CONV #( me->destfile ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_AF_CORE_CONNECTOR~DELIVER.
************************************************************************
*&  Key           : AR-180420
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  DELIVER druckt das eingehende PDF im XSTRING-Format.
************************************************************************

    DATA: l_wa_formoutput TYPE fpformoutput.

    " Anzahl Nachrichten verarbeiten
    IF me->wa_print_params-copies = 0.
      me->wa_print_params-copies = 1.
    ENDIF.

    DO me->wa_print_params-copies TIMES.

      CALL METHOD zcl_af_core_logger=>write_log
        EXPORTING
          im_object    = me->log_object
          im_subobject = zcl_af_core_constants=>co_log_subobj_core_con
          im_ext_ident = '> START - ZIF_AF_CORE_CON_PRINT_PDL->DELIVER'.

      l_wa_formoutput = im_wa_formoutput.

      CALL METHOD me->open_connection.

      CALL METHOD me->get_ads_path.

      CALL METHOD me->transfer_pdl
        EXPORTING
          im_pdl_xstring = l_wa_formoutput-pdl.

      CALL METHOD me->confirm_print
        EXPORTING
          im_wa_formoutput = im_wa_formoutput.

      CALL METHOD me->close_conection.

      CALL METHOD zcl_af_core_logger=>write_log
        EXPORTING
          im_object    = me->log_object
          im_subobject = zcl_af_core_constants=>co_log_subobj_core_con
          im_ext_ident = '> ENDE - ZIF_AF_CORE_CON_PRINT_PDL->DELIVER'.

    ENDDO.
  ENDMETHOD.
ENDCLASS.
