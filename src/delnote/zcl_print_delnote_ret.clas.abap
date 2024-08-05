class ZCL_PRINT_DELNOTE_RET definition
  public
  inheriting from ZCL_AF_CORE_OUTPUT
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IS_CORE_PARAMS type ZAF_S_CORE_PARAMS
      !IV_LOG_OBJECT type BALOBJ_D
      !IS_VBDKL type VBDKL
      !IT_VBDPL type VBDPL_TT
    raising
      ZCX_AF_CORE_OUTPUT .

  methods CREATE_CHANNEL_PARAMS
    redefinition .
protected section.

  methods GET_PRINT_DATA_SPEC_REF
    redefinition .
private section.

  methods GENERATE_PRINT_DATA .
ENDCLASS.



CLASS ZCL_PRINT_DELNOTE_RET IMPLEMENTATION.


  METHOD constructor.
**********************************************************************
*& Key           : WG-230627
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& CONSTRUCTOR
*&
**********************************************************************

    CALL METHOD super->constructor
      EXPORTING
        is_core_params = is_core_params
        iv_log_object  = iv_log_object.

    gv_log_object = iv_log_object.

    CALL METHOD zcl_af_core_logger=>write_log
      EXPORTING
        iv_object    = iv_log_object
        iv_subobject = zcl_af_core_constants=>gc_log_subobj_spec
        iv_ext_ident = '@ START - ZCL_AF_LE_DELNOTE_RET'.

*    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    " SAP Standard Daten an Formularklassen übergeben
*    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    me->wa_vbdkl = im_wa_vbdkl.
*    me->it_vbdpl = im_it_vbdpl.
*    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    " Spezielle Datenbeschaffung
    generate_print_data( ).

  ENDMETHOD.


  method CREATE_CHANNEL_PARAMS.
*************************************************************************
**&  Key           : AR-190118
**&  Request No.   : 180116-140129-AR - Formularentwicklung Lieferschein
*************************************************************************
**&  Description (short)
**&  Gibt an, welche Channels genutzt werden sollen und füllt die
**&  Parameter für den jeweiligen Connector.
*************************************************************************
*
*    FIELD-SYMBOLS: <l_wa_chan> TYPE zaf_s_channel_params.
*
*    CLEAR: ex_it_channel_params.
*
*    " Auswahl des Ausgabekanals nach Nutzereingabe
*    " Später Auswahl über Customzing-Tabelle
*    IF me->wa_data_core-wa_core_params-wa_outputparams-preview = abap_true.
*      APPEND INITIAL LINE TO ex_it_channel_params ASSIGNING <l_wa_chan>.
*      <l_wa_chan>-channel_id = zcl_af_core_constants=>co_output_channel_preview.
*    ELSE.
*      " Nachricht: Gewünschten Kanäle+Parametrisierung
*      CALL METHOD zcl_af_le_delnote_util=>change_it_channel_params
*        EXPORTING
*          im_it_vbdpl          = me->it_vbdpl
*          im_wa_vbdkl          = me->wa_vbdkl
*          im_wa_data_core      = me->wa_data_core
*          im_log_object        = me->log_object
*          im_it_output_ctrl    = me->it_output_ctrl
*        CHANGING
*          ch_it_channel_params = ex_it_channel_params.
*    ENDIF.
*
  endmethod.


  method GENERATE_PRINT_DATA.


    " Übersetzungen in Struktur füllen
*    load_translations( CHANGING cs_transl = ms_transl ).


**********************************************************************
***    " Titel ermitteln
***    CALL METHOD zcl_af_le_delnote_util=>get_title
***      EXPORTING
***        im_wa_transl      = me->wa_transl
***        im_wa_data_core   = me->wa_data_core
***        im_it_output_ctrl = me->it_output_ctrl
***      RECEIVING
***        re_title          = me->wa_transl-title_vdelnote. " formtitle
***
***    " Zusätzlche Sonstige Daten ermitteln
***    CALL METHOD zcl_af_le_delnote_util=>get_print_data
***      EXPORTING
***        im_it_vbdpl       = me->it_vbdpl
***        im_wa_vbdkl       = me->wa_vbdkl
***        im_wa_transl      = me->wa_transl
***        im_it_output_ctrl = me->it_output_ctrl
***        im_wa_data_core   = me->wa_data_core
***        im_log_object     = me->log_object
***      IMPORTING
***        ex_wa_data_spec   = me->wa_data_spec.
***
***    " Texte ermitteln
***    CALL METHOD zcl_af_le_delnote_util=>get_texts
***      EXPORTING
***        im_it_vbdpl     = me->it_vbdpl
***        im_wa_vbdkl     = me->wa_vbdkl
***        im_wa_data_core = me->wa_data_core
***        im_log_object   = me->log_object
***      RECEIVING
***        re_it_texts     = me->wa_data_spec-it_texts.
***
***    " Adressen ermitteln
***    CALL METHOD zcl_af_le_delnote_util=>get_addresses
***      EXPORTING
***        im_it_vbdpl       = me->it_vbdpl
***        im_wa_vbdkl       = me->wa_vbdkl
***        im_wa_data_core   = me->wa_data_core
***        im_log_object     = me->log_object
***        im_it_output_ctrl = me->it_output_ctrl
***      IMPORTING
***        ex_it_adrs        = me->wa_data_spec-it_adrs
***        ex_wa_adrs        = me->wa_data_spec-wa_adrs_head_we.
***
***    " Adressüberschriften ermitteln
***    CALL METHOD zcl_af_le_delnote_util=>get_labelnames_addressblock
***      EXPORTING
***        im_log_object             = me->log_object
***        im_it_output_ctrl         = me->it_output_ctrl
***        im_wa_transl              = me->wa_transl
***      IMPORTING
***        ex_wa_addressblock_labels = me->wa_data_spec-wa_addressbock_labels.
***
  endmethod.


  METHOD get_print_data_spec_ref.
************************************************************************
*&  Key           : AR-190118
*&  Request No.   : 180116-140129-AR - Formularentwicklung Lieferschein
************************************************************************
*&  Description (short)
*&  Erzeugt Daten Referenzen für die Parametrisierung der Formular
*&  Schnittstelle.
*&
*&  !!!    ACHTUNG: Die Referenz darf nur auf Klassen-Attribute      !!!
*&  !!!    erzeugt werden. Wenn die Referenz auf eine lokale         !!!
*&  !!!    Variable aus der Methode zeigt, ist die Referenz          !!!
*&  !!!    nach dieser Methode direkt ungültig, und es können        !!!
*&  !!!    KEINE Daten an das Formular übergeben werden.             !!!
************************************************************************
    " Referenzen zu den Daten erzeugen und übergeben
*    GET REFERENCE OF me->wa_data_spec INTO ex_rf_data_spec.
*    GET REFERENCE OF me->wa_transl    INTO ex_rf_transl.
  ENDMETHOD.
ENDCLASS.
