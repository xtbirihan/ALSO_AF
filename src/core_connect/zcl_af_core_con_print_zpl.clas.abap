class ZCL_AF_CORE_CON_PRINT_ZPL definition
  public
  final
  create public .

public section.

  interfaces ZIF_AF_CORE_CONNECTOR .

  aliases DELIVER
    for ZIF_AF_CORE_CONNECTOR~DELIVER .

  data RF_LOGGER type ref to ZCL_AF_CORE_LOGGER .
  data LOG_OBJECT type BALOBJ_D .

  methods CONSTRUCTOR
    importing
      !IV_LOG_OBJECT type BALOBJ_D
      !IS_PRINT_ZPL_PARAMS type ZAF_S_CON_PRINT_ZPL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
protected section.

  data WA_PRINT_PARAMS type ZAF_S_CON_PRINT_ZPL_PARAMS .
  data HANDLE type SY-TABIX .
  data SPOOLID type TSP01-RQIDENT .
  data ADSPART type ADSPART .
  data DESTFILE type TEXT1024 .
private section.

  methods CHANGE_ZPL_DATA_STD
    changing
      !CH_ZPL_PDL type FPCONTENT
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CLOSE_CONECTION
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CONFIRM_PRINT
    importing
      !IM_WA_FORMOUTPUT type FPFORMOUTPUT
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CONTROLL_ZPL_DATA_CHANGE
    importing
      !IM_LABEL_ID type ZAF_CORE_CON_LABEL_ID
    changing
      !CH_ZPL_PDL type FPCONTENT
    raising
      ZCX_AF_CORE_OUTPUT .
  methods GET_ADS_PATH
    raising
      ZCX_AF_CORE_OUTPUT .
  methods OPEN_CONNECTION
    raising
      ZCX_AF_CORE_OUTPUT .
  methods TRANSFER_PDL
    importing
      !IM_PDL_XSTRING type XSTRING
    raising
      ZCX_AF_CORE_OUTPUT .
ENDCLASS.



CLASS ZCL_AF_CORE_CON_PRINT_ZPL IMPLEMENTATION.


  METHOD change_zpl_data_std.
************************************************************************
*&  Key           : AR-180621
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Änderung des ZPL Druckstroms
************************************************************************


    DATA: l_source_string TYPE string.
    DATA: l_change_string TYPE string.
    DATA: l_hex_string    TYPE xstring.
    DATA: l_subrc         TYPE subrc.

    CONSTANTS: l_co_regex        TYPE string VALUE '\^BY\S+',
               l_co_module_width TYPE string VALUE '^BY2,2',
               l_co_print_speed  TYPE c      VALUE '6',
               l_co_label_size   TYPE string VALUE '^PW832'.

    " HEX nach String konvertieren
    CALL METHOD zcl_actebis_util_convert=>conv_hex_to_string
      EXPORTING
        im_value = ch_zpl_pdl
      IMPORTING
        ex_value = l_source_string.

    " Änderung auf String durchführen / Hier wird die Druckgeschwindigkeit von 2 (A) auf 6 (D) geändert.
    CONCATENATE l_source_string+0(7) l_co_print_speed l_source_string+8(7) l_co_label_size l_source_string+15 INTO l_change_string.

    " Hier ändern wir "^BY" - Bar Code Field Default / w = module width (in dots). Von ^BYw,2 auf ^BY3,2
    REPLACE ALL OCCURRENCES OF REGEX l_co_regex IN l_change_string WITH l_co_module_width.

    " String nach xstring konvertieren
    CALL METHOD zcl_actebis_util_convert=>conv_string_to_hex
      EXPORTING
        im_string  = l_change_string
      IMPORTING
        ex_xstring = l_hex_string
        ex_subrc   = l_subrc.

    IF l_subrc = 0.
      CLEAR ch_zpl_pdl.
      ch_zpl_pdl = l_hex_string.
    ELSE.
      CALL METHOD zcx_af_core_output=>raise
        EXPORTING
          im_msgno      = '101'
          im_msgty      = 'E'
          im_msgv1      = CONV #( me->wa_print_params-dest )
          im_log_object = me->log_object.
    ENDIF.

  ENDMETHOD.


  METHOD CLOSE_CONECTION.
************************************************************************
*&  Key           : RP-180509
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Durckauftrag abschließen
************************************************************************


    CALL FUNCTION 'ADS_SR_CLOSE'
      EXPORTING
        handle = me->handle
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc NE 0.
      CALL METHOD zcx_af_core_output=>raise
        EXPORTING
          im_log_object = me->log_object
          im_msgno     = '106'
          im_msgty     = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD CONFIRM_PRINT.
************************************************************************
*&  Key           : RP-180509
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Druckauftrag bestätigen und ausführen
************************************************************************


    DATA lv_filesize TYPE i.

    lv_filesize = xstrlen( im_wa_formoutput-pdf ).
    CALL FUNCTION 'ADS_SR_CONFIRM'
      EXPORTING
        handle   = me->handle
        partname = me->adspart
        size     = lv_filesize
        pages    = im_wa_formoutput-pages
        no_pdf   = abap_true
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc NE 0.
      CALL METHOD zcx_af_core_output=>raise
        EXPORTING
          im_log_object = me->log_object
          im_msgno      = '105'
          im_msgty      = 'E'.
    ENDIF.


  ENDMETHOD.


  METHOD constructor.
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
        im_ext_ident = '> START - ZCL_AF_CORE_CON_PRINT_ZPL'.

    me->wa_print_params = im_wa_print_zpl_params.

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


  METHOD controll_zpl_data_change.
************************************************************************
*&  Key           : AR-180621
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Kontrolle über ZEBRA-Druckstrom
*&  Änderungen bei unterschiedlichen Labels.
************************************************************************
*&  Lösung im CORE Connector ZCL_AF_CORE_CON_PRINT_ZPL eingebaut. Sollte das Label eine spez. Einstellung
*&  benötigen, kann man in den Parametern eine Label ID (ZAF_CORE_CON_LABEL_ID) vergeben. Wenn man keine
*&  angibt, wird der Default verwendet.
*&  Gesteuert wird das Ganze in der Methode CONTROLL_ZPL_DATA_CHANGE.
*&  Beispiel für neue Label ID:
*&  1. Im Wertebereich des Datenelements ZAF_CORE_CON_LABEL_ID die neue Label ID vergeben: 0001
*&  2. In der Klasse ZCL_AF_CORE_CON_PRINT_ZPL die Methode CHANGE_ZPL_DATA_STD kopieren und der
*&     neue Name ist dann CHANGE_ZPL_DATA_[LABEL_ID] als in diesem Beispiel: CHANGE_ZPL_DATA_0001.
*&  3. Nun kann man in der neuen Methode alle Änderungen am Datenstrom vornehmen.
************************************************************************

    DATA: l_method_name TYPE swferecmet.

    CONCATENATE 'CHANGE_ZPL_DATA_' im_label_id INTO l_method_name.

    IF im_label_id IS INITIAL.
      " Standardlabelgröße ^PW832 und Geschwindigkeit auf 6
      CALL METHOD me->change_zpl_data_std
        CHANGING
          ch_zpl_pdl = ch_zpl_pdl.
    ELSE.
      IF zcl_actebis_util_ddic=>is_method_exist( im_class_name = 'ZCL_AF_CORE_CON_PRINT_ZPL' im_method_name = l_method_name ) = abap_true.
        " Dynamischer Aufruf der Änderungmethode zur Label ID
        CALL METHOD me->(l_method_name)
          CHANGING
            ch_zpl_pdl = ch_zpl_pdl.
      ELSE.
        " Im Label-Connector wurde die Methode CONTROLL_ZPL_DATA_ & nicht gefunden.
        CALL METHOD zcx_af_core_output=>raise
          EXPORTING
            im_log_object = me->log_object
            im_msgno      = '087'
            im_msgty      = 'E'
            im_msgv1      = CONV #( im_label_id ).
      ENDIF.
    ENDIF.

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
        dest            = wa_print_params-dest
        doctype         = 'ADSP'
        immediate_print = wa_print_params-reqimm "abap_true
        auto_delete     = wa_print_params-reqdel "abap_true
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
          im_msgv1      = CONV #( wa_print_params-dest )
          im_log_object = me->log_object.
    ENDIF.

  ENDMETHOD.


  METHOD TRANSFER_PDL.
************************************************************************
*&  Key           : RP-180620
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  PDL-XSTRING-Datenstrom an ADS zur Verarbeitung senden
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


  METHOD zif_af_core_connector~deliver.
************************************************************************
*&  Key           : AR-180420
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  DELIVER druckt das eingehende PDF im XSTRING-Format.
************************************************************************

    DATA: l_wa_formoutput TYPE fpformoutput.
    DATA: l_ascii         TYPE string.
    DATA: l_xstring       TYPE xstring.

    " Anzahl Nachrichten verarbeiten
    IF me->wa_print_params-copies = 0.
      me->wa_print_params-copies = 1.
    ENDIF.

    DO me->wa_print_params-copies TIMES.

      CALL METHOD zcl_af_core_logger=>write_log
        EXPORTING
          im_object    = me->log_object
          im_subobject = zcl_af_core_constants=>co_log_subobj_core_con
          im_ext_ident = '> START - ZIF_AF_CORE_CON_PRINT_ZPL->DELIVER'.

      l_wa_formoutput = im_wa_formoutput.

      CALL METHOD me->open_connection.

      CALL METHOD me->get_ads_path.

      " Kontrolle über ZEBRA-Druckstrom Änderungen bei unterschiedlichen Labels.
      CALL METHOD controll_zpl_data_change
        EXPORTING
          im_label_id = me->wa_print_params-label_id
        CHANGING
          ch_zpl_pdl  = l_wa_formoutput-pdl.

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
          im_ext_ident = '> ENDE - ZIF_AF_CORE_CON_PRINT_ZPL->DELIVER'.

    ENDDO.

  ENDMETHOD.
ENDCLASS.
