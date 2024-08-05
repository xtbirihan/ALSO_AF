class ZCL_AF_CORE_OUTPUT definition
  public
  abstract
  create public .

public section.

  types:
    BEGIN OF ts_data_core,
        wa_core_params TYPE zaf_s_core_params,
        wa_org_data    TYPE zaf_s_org_data_values,
      END OF ts_data_core .

  data GV_LOG_OBJECT type BALOBJ_D .

  methods SET_ORG_DATA_VALUES
    importing
      !IS_ORG_DATA_VALUES type ZAF_S_ORG_DATA_VALUES .
  methods GET_CORE_PARAMS
    exporting
      value(ES_CORE_PARAMS) type ZAF_S_CORE_PARAMS .
  methods GENERATE_FORM
    importing
      !IS_OUTPUTPARAMS type SFPOUTPUTPARAMS
      !IS_DOCPARAMS type SFPDOCPARAMS
    exporting
      !ES_FORMOUTPUT type FPFORMOUTPUT
    raising
      ZCX_AF_CORE_OUTPUT .
  methods LOAD_ORG_DATA_VALUES
    importing
      !IV_CHANNEL type ZAF_OUTPUT_CHANNEL
    returning
      value(RS_ORG_DATA_VALUES) type ZAF_S_ORG_DATA_VALUES
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CONSTRUCTOR
    importing
      !IS_CORE_PARAMS type ZAF_S_CORE_PARAMS
      !IV_LOG_OBJECT type BALOBJ_D
    raising
      ZCX_AF_CORE_OUTPUT .
  methods GET_PRINT_DATA
    exporting
      !ES_DATA_CORE type TS_DATA_CORE
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CREATE_CHANNEL_PARAMS
  abstract
    exporting
      !ET_CHANNEL_PARAMS type ZAF_T_CHANNEL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
protected section.

  data GS_ORG_DATA_VALUES type ZAF_S_ORG_DATA_VALUES .
  data GS_DATA_CORE type TS_DATA_CORE .
  data GT_OUTPUT_CTRL type ZAF_T_OUTPUT_CTRL .

  methods GET_PRINT_DATA_REF
  final
    exporting
      !ER_DATA_CORE type ref to DATA
    raising
      ZCX_AF_CORE_OUTPUT .
  methods GET_PRINT_DATA_SPEC_REF
  abstract
    exporting
      value(ER_DATA_SPEC) type ref to DATA
      value(ER_TRANSL) type ref to DATA
    raising
      ZCX_AF_CORE_OUTPUT .
  methods LOAD_TRANSLATIONS
    changing
      value(CS_TRANSL) type ANY
    raising
      ZCX_AF_CORE_OUTPUT .
private section.

  methods LOAD_OUTPUT_CTRL_PARAMS
    importing
      !IV_VKORG type VKORG
      !IV_FORM_NAME type FPNAME
      !IV_LOG_OBJECT type BALOBJ_D
    exporting
      !ET_OUTPUT_CTRL type ZAF_T_OUTPUT_CTRL .
ENDCLASS.



CLASS ZCL_AF_CORE_OUTPUT IMPLEMENTATION.


  METHOD constructor.
************************************************************************
*&  Key           : AR-180606
*&  Request No.   : 180116-135917-AR - Formularentwicklung Service Label
************************************************************************
*&  Description (short)
*&  Instanziiert die spezialisierte Ausgabeklasse
*&  Instantiates the specialised output class
************************************************************************

    gv_log_object = iv_log_object.

    zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                   iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                   iv_ext_ident = '> START - ZCL_AF_CORE_OUTPUT' ).

    " CORE Parameter speichern
    " Save CORE parameters
    gs_data_core-wa_core_params = is_core_params.

    " Sicherstellen, dass alle Parameter übergeben wurden
    " Ensure that all parameters have been passed
    IF gs_data_core-wa_core_params-vkorg IS INITIAL OR
       gs_data_core-wa_core_params-output_date IS INITIAL OR
       gs_data_core-wa_core_params-langu IS INITIAL.

      zcx_af_core_output=>raise( iv_log_object = gv_log_object
                                 iv_msgno      = '057'
                                 iv_msgty      = 'E' ).
    ENDIF.

    " Ausgabesprache ermitteln
    " Determine output language
    gs_data_core-wa_core_params-langu = zcl_af_core_util=>get_output_language( iv_vkorg      = gs_data_core-wa_core_params-vkorg
                                                                               iv_comm_langu = gs_data_core-wa_core_params-langu ).

    " Adobe Forms - Dynamischen Steuerung von Formularausgaben
    " Adobe Forms - Dynamic control of form output
    load_output_ctrl_params( EXPORTING iv_vkorg       = gs_data_core-wa_core_params-vkorg
                                       iv_form_name   = gs_data_core-wa_core_params-form_name
                                       iv_log_object  = iv_log_object
                             IMPORTING et_output_ctrl = gt_output_ctrl ).

  ENDMETHOD.


  METHOD generate_form.
************************************************************************
*&  Key           : JR-180523
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Generiert das Formular zu der aktuellen Instanz. Formular-Name und
*&  Druckdaten werden über die Instanz beschaffen. Dabei werden auch
*&  Methoden der spezialisierten Output Klasse genutzt.
*&  Der Aufruf ist generisch und funktioniert nur, wenn das auszugebene
*&  Formular folgende Parameter hat:
*&    Importing:
*&      1. /1BCDWB/DOCPARAMS  (Standard Dok. Parameter, ist immer da)
*&      2. IM_WA_TRANSL       (Spezifische Übersetzungsstruktur)
*&      3. IM_WA_DATA_CORE    (Allgemeine CORE Formular Daten)
*&      4. IM_WA_DATA_SPEZ    (Spezifische Formular Daten)
*&    Exporting:
*&      1. /1BCDWB/FORMOUTPUT (Formular Erzeugnisse, ist immer da)
*&
*& Generates the form for the current instance. Form name and
*& print data are procured via the instance. The following are also used
*& methods of the specialised output class are used.
*& The call is generic and only works if the *& form to be output has the following parameters: *.
*& form has the following parameters:
*& 1. /1BCDWB/DOCPARAMS (standard doc. parameter, is always there)
*& 2. IM_WA_TRANSL (Specific translation structure)
*& 3. IM_WA_DATA_CORE (General CORE form data)
*& 4. IM_WA_DATA_SPEZ (Specific form data).
*& Importing:
*& 1. /1BCDWB/DOCPARAMS (standard doc. parameter, is always there)
*& 2. IM_WA_TRANSL (Specific translation structure)
*& 3. IM_WA_DATA_CORE (General CORE form data)
*& 4. IM_WA_DATA_SPEZ (Specific form data).
*& Exporting:
*& 1. /1BCDWB/FORMOUTPUT (Form products, is always there).
************************************************************************
    DATA: lt_params       TYPE abap_func_parmbind_tab,
          lt_exceptions   TYPE abap_func_excpbind_tab,
          ls_params       TYPE abap_func_parmbind,
          ls_exceptions   TYPE abap_func_excpbind,
          lr_docparams    TYPE REF TO data,
          ls_outputparams TYPE sfpoutputparams,
          lr_transl       TYPE REF TO data,
          lr_data_core    TYPE REF TO data,
          lr_data_spec    TYPE REF TO data,
          lr_formoutput   TYPE REF TO data,
          lv_funcname     TYPE funcname.

**********************************************************************
* 0. Daten beschaffen
* 0. Obtain data
**********************************************************************
    " CORE Daten beschaffen
    "Obtain CORE data
    get_print_data_ref( IMPORTING er_data_core = lr_data_core ).

    " Spez. Formular Daten beschaffen
    "Obtain special form data
    get_print_data_spec_ref( IMPORTING er_data_spec = lr_data_spec
                                       er_transl    = lr_transl ).

**********************************************************************
* 1. FP_JOB_OPEN
* 1. FP_JOB_OPEN
**********************************************************************
    " Technischen Formular-Namen beschaffen
    "Procure technical form name
    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
*       i_name     = im_formname
        i_name     = gs_data_core-wa_core_params-form_name
      IMPORTING
        e_funcname = lv_funcname.
    IF lv_funcname IS INITIAL.
      zcx_af_core_output=>raise( EXPORTING iv_log_object = gv_log_object
                                           iv_msgno = '058'
                                           iv_msgty = 'E'
                                           iv_msgv1 = CONV #( gs_data_core-wa_core_params-form_name ) ).
    ENDIF.

    ls_outputparams = is_outputparams.

    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        syster_error    = 3
        internal_error  = 4
        OTHERS          = 5.

    IF sy-subrc <> 0.
      zcx_af_core_output=>raise( EXPORTING iv_log_object = gv_log_object
                                           iv_msgno      = '059'
                                           iv_msgty      = 'E'
                                           iv_msgv1      = CONV #( gs_data_core-wa_core_params-form_name ) ).
    ENDIF.

**********************************************************************
* 2. Parameter Tabelle füllen
* 2. fill parameter table
**********************************************************************
    GET REFERENCE OF is_docparams  INTO lr_docparams.
    GET REFERENCE OF es_formoutput INTO lr_formoutput.

    ls_params-name = '/1BCDWB/DOCPARAMS'.
    ls_params-kind = abap_func_exporting.
    ls_params-value = lr_docparams.
    INSERT ls_params INTO TABLE lt_params.

    ls_params-name = 'IM_WA_DATA_CORE'.
    ls_params-kind = abap_func_exporting.
    ls_params-value = lr_data_core.
    INSERT ls_params INTO TABLE lt_params.

    ls_params-name = 'IM_WA_DATA_SPEC'.
    ls_params-kind = abap_func_exporting.
    ls_params-value = lr_data_spec.
    INSERT ls_params INTO TABLE lt_params.

    ls_params-name = 'IM_WA_TRANSL'.
    ls_params-kind = abap_func_exporting.
    ls_params-value = lr_transl.
    INSERT ls_params INTO TABLE lt_params.

    ls_params-name = '/1BCDWB/FORMOUTPUT'.
    ls_params-kind = abap_func_importing.
    ls_params-value = lr_formoutput.
    INSERT ls_params INTO TABLE lt_params.

**********************************************************************
* 3. Formular Baustein aufrufen
* 3. call up the form module
**********************************************************************
    " Formular generieren
    lt_exceptions = VALUE #( ( name = 'OTHERS' value = '1' ) ).
    CALL FUNCTION lv_funcname
      PARAMETER-TABLE lt_params
      EXCEPTION-TABLE lt_exceptions.
    IF sy-subrc <> 0.
      zcx_af_core_output=>raise( EXPORTING iv_log_object = gv_log_object
                                           iv_msgno      = '060'
                                           iv_msgty      = 'E'
                                           iv_msgv1      = CONV #( gs_data_core-wa_core_params-form_name ) ).
    ENDIF.

**********************************************************************
* 4. FP_JOB_CLOSE
**********************************************************************
    CALL FUNCTION 'FP_JOB_CLOSE'
*      IMPORTING
*        e_result       = ls_joboutput
      EXCEPTIONS
        usage_error    = 1
        syster_error   = 2
        internal_error = 3
        OTHERS         = 4.

    IF sy-subrc <> 0.
      zcx_af_core_output=>raise( iv_log_object = gv_log_object
                                 iv_msgno      = '061'
                                 iv_msgty      = 'E'
                                 iv_msgv1      = CONV #( gs_data_core-wa_core_params-form_name ) ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_CORE_PARAMS.
************************************************************************
*&  Key           : JR-180522
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Aktuelle Dok. Parameter zurückgeben.
*& Current doc. Return parameters.
************************************************************************
    es_core_params = gs_data_core-wa_core_params.
  ENDMETHOD.


  METHOD get_print_data.
************************************************************************
*&  Key           : JR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Ausgabedaten beschaffen
*&  Obtain output data
************************************************************************
    " Docparams
    es_data_core-wa_core_params = gs_data_core-wa_core_params.
  ENDMETHOD.


  METHOD get_print_data_ref.
************************************************************************
*&  Key           : JR-180523
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Gibt Referenz auf die CORE Daten zurück
*&  Returns reference to the CORE data
************************************************************************
    " Referenzen zu den Daten erzeugen und übergeben
    " Create and transfer references to the data
    GET REFERENCE OF gs_data_core INTO er_data_core.
  ENDMETHOD.


  METHOD load_org_data_values.
************************************************************************
*&  Key           : JR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Orga.-Daten beschaffem
*& Obtain Orga. data
************************************************************************
    DATA: ls_org_data_key TYPE zaf_s_org_data_search_keys,
          lo_org_data     TYPE REF TO zcl_af_core_org_data,
          ls_core_params  TYPE zaf_s_core_params.

    ls_core_params            = gs_data_core-wa_core_params.
    ls_org_data_key-vkorg     = ls_core_params-vkorg.
    ls_org_data_key-langu     = ls_core_params-langu.
    ls_org_data_key-form_name = ls_core_params-form_name.
    ls_org_data_key-channel   = iv_channel.
    ls_org_data_key-datum     = ls_core_params-output_date.

    " Instanz von Org. Data Klasse beschaffen
    "Obtain instance of Org. data class
    lo_org_data = zcl_af_core_org_data=>get_instance( iv_log_object    = gv_log_object
                                                      is_org_data_keys = ls_org_data_key ).

    IF lo_org_data IS NOT BOUND.
      zcx_af_core_output=>raise( iv_log_object = gv_log_object
                                 iv_msgno      = '051'
                                 iv_msgty      = 'E' ).
    ENDIF.

    " Org. Daten beschaffen und zurückgeben
    "Obtain and return org. data
    rs_org_data_values = lo_org_data->get_org_data( ).

  ENDMETHOD.


  METHOD load_output_ctrl_params.
************************************************************************
*&  Key           : AR-190207
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  AusgabeSteuerungsparameter beschaffen
*&  Obtain output control parameters
************************************************************************

    DATA: lo_msg TYPE REF TO zcl_af_core_messages.

    SELECT * FROM zaf_output_ctrl INTO CORRESPONDING FIELDS OF TABLE et_output_ctrl
                                 WHERE vkorg  = iv_vkorg
                                   AND procid = iv_form_name.

    IF sy-subrc = 0.
      LOOP AT et_output_ctrl ASSIGNING FIELD-SYMBOL(<ls_output_ctrl>).
        " Strukturwerte mit Namen in eim Meldungsobjekt hinzufügen
        " Add structure values with names in a message object
        lo_msg = zcl_af_core_util=>transfer_struc_val_to_msg_obj( is_structure = <ls_output_ctrl> ).

        " Log wegschreiben
        " Write log
        zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                       iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                       iv_ext_ident = 'Ausgabesteuerungsparameter (ZAF_OUTPUT_CTRL) gelesen'
                                       io_msg       = lo_msg ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD load_translations.
************************************************************************
*&  Key           : AR-190124
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Label-Übersetzungen in Struktur füllen
*&  Fill label translations in structure
************************************************************************

    DATA: ls_transl_key  TYPE zaf_s_transl_keys,
          ls_core_params TYPE zaf_s_core_params.

    ls_core_params = gs_data_core-wa_core_params.

    " Schlüssel für Übersetzung beschaffen
    " Obtain key for translation
    ls_transl_key-form_name = ls_core_params-form_name.
    ls_transl_key-vkorg     = ls_core_params-vkorg.
    ls_transl_key-langu     = ls_core_params-langu.

    " Translator Klasse instanziieren
    " Instantiate Translator Class
    data(lo_translator) = new zcl_af_core_translator( is_transl_keys = ls_transl_key
                                                      iv_log_object  = gv_log_object ).

    IF lo_translator IS NOT BOUND.
      zcx_af_core_output=>raise( iv_log_object = gv_log_object
                                 iv_msgno      = '056'
                                 iv_msgty      = 'E' ).
    ELSE.
      " Label-Übersetzungen in Struktur füllen
      " Fill label translations in structure
      lo_translator->fill_translations( CHANGING cs_transl = cs_transl ).
    ENDIF.


  ENDMETHOD.


  METHOD set_org_data_values.
************************************************************************
*&  Key           : JR-180523
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Setzt die Org. Daten zu der aktuellen Instanz
*& Sets the org. data for the current instance
************************************************************************
    gs_data_core-wa_org_data = is_org_data_values.
  ENDMETHOD.
ENDCLASS.
