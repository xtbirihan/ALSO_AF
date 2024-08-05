class ZCL_AF_CORE_TRANSLATOR definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_transl_1,
        adrs1   TYPE string,
        adrs2   TYPE string,
        adrs3   TYPE string,
        adrs4   TYPE string,
        adrs5   TYPE string,
        adrs6   TYPE string,
        adrs7   TYPE string,
        adrs8   TYPE string,
        adrs9   TYPE string,
        adrs10  TYPE string,
        info1   TYPE string,
        info2   TYPE string,
        info3   TYPE string,
        info4   TYPE string,
        info5   TYPE string,
        info6   TYPE string,
        info7   TYPE string,
        info8   TYPE string,
        info9   TYPE string,
        info10  TYPE string,
        info11  TYPE string,
        info12  TYPE string,
        info13  TYPE string,
        info14  TYPE string,
        info15  TYPE string,
        info16  TYPE string,
        info17  TYPE string,
        info18  TYPE string,
        info19  TYPE string,
        info20  TYPE string,
        head1   TYPE string,
        head2   TYPE string,
        head3   TYPE string,
        head4   TYPE string,
        head5   TYPE string,
        head6   TYPE string,
        head7   TYPE string,
        head8   TYPE string,
        head9   TYPE string,
        head10  TYPE string,
        head12  TYPE string,
        head13  TYPE string,
        head14  TYPE string,
        head15  TYPE string,
        head16  TYPE string,
        head17  TYPE string,
        head18  TYPE string,
        head19  TYPE string,
        head20  TYPE string,
        item1   TYPE string,
        item2   TYPE string,
        item3   TYPE string,
        item4   TYPE string,
        item5   TYPE string,
        item6   TYPE string,
        item7   TYPE string,
        item8   TYPE string,
        item9   TYPE string,
        item10  TYPE string,
        item11  TYPE string,
        item12  TYPE string,
        item13  TYPE string,
        item14  TYPE string,
        item15  TYPE string,
        item16  TYPE string,
        item17  TYPE string,
        item18  TYPE string,
        item19  TYPE string,
        item20  TYPE string,
        item21  TYPE string,
        item22  TYPE string,
        item23  TYPE string,
        item24  TYPE string,
        item25  TYPE string,
        item26  TYPE string,
        item27  TYPE string,
        item28  TYPE string,
        item29  TYPE string,
        item30  TYPE string,
        sonst1  TYPE string,
        sonst2  TYPE string,
        sonst3  TYPE string,
        sonst4  TYPE string,
        sonst5  TYPE string,
        sonst6  TYPE string,
        sonst7  TYPE string,
        sonst8  TYPE string,
        sonst9  TYPE string,
        sonst10 TYPE string,
        sonst11 TYPE string,
        sonst12 TYPE string,
        sonst13 TYPE string,
        sonst14 TYPE string,
        sonst15 TYPE string,
        sonst16 TYPE string,
        sonst17 TYPE string,
        sonst18 TYPE string,
        sonst19 TYPE string,
        sonst20 TYPE string,
      END OF ts_transl_1 .
  types:
    BEGIN OF ts_transl_2,
        transl1  TYPE string,
        transl2  TYPE string,
        transl3  TYPE string,
        transl4  TYPE string,
        transl5  TYPE string,
        transl6  TYPE string,
        transl7  TYPE string,
        transl8  TYPE string,
        transl9  TYPE string,
        transl10 TYPE string,
        transl11 TYPE string,
        transl12 TYPE string,
        transl13 TYPE string,
        transl14 TYPE string,
        transl15 TYPE string,
        transl16 TYPE string,
        transl17 TYPE string,
        transl18 TYPE string,
        transl19 TYPE string,
        transl20 TYPE string,
        transl21 TYPE string,
        transl22 TYPE string,
        transl23 TYPE string,
        transl24 TYPE string,
        transl25 TYPE string,
        transl26 TYPE string,
        transl27 TYPE string,
        transl28 TYPE string,
        transl29 TYPE string,
        transl30 TYPE string,
        transl31 TYPE string,
        transl32 TYPE string,
        transl33 TYPE string,
        transl34 TYPE string,
        transl35 TYPE string,
        transl36 TYPE string,
        transl37 TYPE string,
        transl38 TYPE string,
        transl39 TYPE string,
        transl40 TYPE string,
        transl41 TYPE string,
        transl42 TYPE string,
        transl43 TYPE string,
        transl44 TYPE string,
        transl45 TYPE string,
        transl46 TYPE string,
        transl47 TYPE string,
        transl48 TYPE string,
        transl49 TYPE string,
        transl50 TYPE string,
        transl51 TYPE string,
        transl52 TYPE string,
        transl53 TYPE string,
        transl54 TYPE string,
        transl55 TYPE string,
        transl56 TYPE string,
        transl57 TYPE string,
        transl58 TYPE string,
        transl59 TYPE string,
        transl60 TYPE string,
      END OF ts_transl_2 .
  types:
    BEGIN OF ts_label,
        labelname   TYPE zaf_label_value,
        translation TYPE zaf_transl_value,
      END OF ts_label .
  types:
    tt_labels TYPE STANDARD TABLE OF ts_label .

  methods CONSTRUCTOR
    importing
      !IS_TRANSL_KEYS type ZAF_S_TRANSL_KEYS
      !IV_LOG_OBJECT type BALOBJ_D optional .
  methods FILL_TRANSLATIONS
    changing
      !CS_TRANSL type ANY .
  methods FILL_TRANSLATIONS_2
    changing
      !CS_TRANSL type ANY .
  class-methods GET_SINGLE_TRANSLATION
    importing
      !IV_LABELNAME type ZAF_LABEL_VALUE
      !IV_LANGU type LANGU
      !IV_VKORG type VKORG optional
      !IV_FORM_NAME type FORM_NAME optional
    returning
      value(RV_TRANSLATION) type ZAF_TRANSL_VALUE .
protected section.
private section.

  data GT_TRANSLATIONS type ZAF_T_TRANSLATIONS .
  data GS_TRANSL_KEYS type ZAF_S_TRANSL_KEYS .
  data GT_LABELNAMES type TT_LABELS .
  data GV_LOG_OBJECT type BALOBJ_D .

  methods FIND_TRANSLATION
    changing
      !CS_LABEL type TS_LABEL .
  methods SELECT_TRANSLATIONS .
  methods CREATE_IT_LABELNAMES
    importing
      !IS_TRANSL type ANY .
  methods CREATE_IT_LABELNAMES_2
    importing
      !IS_TRANSL type ANY .
ENDCLASS.



CLASS ZCL_AF_CORE_TRANSLATOR IMPLEMENTATION.


  METHOD constructor.
************************************************************************
*&  Key           : AR-180606
*&  Request No.   : 180116-135917-AR - Formularentwicklung Service Label
************************************************************************
*&  Description (short)
*&  Instanziiert die Übersetzungsklasse
*&
*& Instantiates the translation class
************************************************************************

    gv_log_object  = iv_log_object.
    gs_transl_keys = is_transl_keys.

    IF gv_log_object  IS NOT INITIAL.
      zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = '> START - ZCL_AF_CORE_TRANSLATOR' ).
    ENDIF.
  ENDMETHOD.


  METHOD create_it_labelnames.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Übersetzungsstruktur füllen
*&  Der übergebene Strukturname muss in Tabelle gt_labelnames überführt werden.
*&
*& Fill the translation structure
*& The transferred structure name must be transferred to table gt_labelnames.
************************************************************************

    DATA: lo_struct TYPE REF TO cl_abap_structdescr,
          lt_comp   TYPE        cl_abap_structdescr=>component_table.

*    l_rf_struct ?= cl_abap_typedescr=>describe_by_name( me->ddic_strucname ).
    lo_struct ?= cl_abap_typedescr=>describe_by_data( is_transl ).
    lt_comp    = lo_struct->get_components( ).

    LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>) .
      APPEND INITIAL LINE TO gt_labelnames ASSIGNING FIELD-SYMBOL(<ls_labelname>) .
      <ls_labelname>-labelname = <ls_comp>-name.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_it_labelnames_2.
************************************************************************
*&  Key           : WG-230414
*&  Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
************************************************************************
*&  Description (short)
*&  Übersetzungsstruktur füllen
*&  Der übergebene Strukturname muss in Tabelle gt_labelnames überführt werden.
*&
*& Fill the translation structure
*& The transferred structure name must be transferred to table gt_labelnames.
************************************************************************

    DATA: lo_struct TYPE REF TO cl_abap_structdescr,
          lt_comp   TYPE        cl_abap_structdescr=>component_table.

    FIELD-SYMBOLS: <lv_field> TYPE any.

    lo_struct ?= cl_abap_typedescr=>describe_by_data( is_transl ).
    lt_comp    = lo_struct->get_components( ).

    LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
      UNASSIGN <lv_field>.
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE is_transl TO <lv_field>.
      CHECK <lv_field> IS ASSIGNED AND <lv_field> IS NOT INITIAL.

      APPEND INITIAL LINE TO gt_labelnames ASSIGNING FIELD-SYMBOL(<ls_labelname>) .
      <ls_labelname>-labelname = <lv_field>.
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_translations.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Übersetzungsstruktur füllen
*&  Ruft den Loop über die Ergebnis Tabelle aus Select auf
*&  Im Loop wird find n mal aufgerufen und die Tabelle
*&
*&  Fill the translation structure
*&  Calls the loop via the result table from Select
*&  In the loop find n times is called and the table
*&
*& Fill translation structure
*& Calls the loop over the result table from select
*& In the loop, find is called n times and the table
*&
*& Fill the translation structure
*& Calls the loop via the result table from Select
*& In the loop find n times is called and the table
************************************************************************

    create_it_labelnames( is_transl = cs_transl ).

    select_translations( ).

    FIELD-SYMBOLS: <lv_field> TYPE any.

    LOOP AT gt_labelnames ASSIGNING FIELD-SYMBOL(<ls_label>).

      find_translation( CHANGING cs_label = <ls_label> ).

      ASSIGN COMPONENT <ls_label>-labelname OF STRUCTURE cs_transl TO <lv_field>.
      <lv_field> = <ls_label>-translation.

    ENDLOOP.

    IF gv_log_object  IS NOT INITIAL.
      zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = '>> Übersetzung der LABELS abgeschlossen'
                                     io_msg       = zcl_af_core_util=>transfer_struc_val_to_msg_obj( is_structure = cs_transl ) ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_translations_2.
**********************************************************************
*& Key           : WG-230413
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*&  Description (short)
*&  Übersetzungsstruktur füllen
*&  Ruft den Loop über die Ergebnis Tabelle aus Select auf
*&  Im Loop wird find n mal aufgerufen und die Tabelle
*&
*&  Fill the translation structure
*&  Calls the loop via the result table from Select
*&  In the loop find n times is called and the table
************************************************************************

    DATA: lo_struct TYPE REF TO cl_abap_structdescr,
          lt_comp   TYPE        cl_abap_structdescr=>component_table,
          ls_label  TYPE ts_label.

    FIELD-SYMBOLS: <lv_value> TYPE any,
                   <lv_field> TYPE any.


    create_it_labelnames_2( is_transl = cs_transl ).
    CHECK lines( gt_labelnames ) > 0.

    select_translations( ).

    lo_struct ?= cl_abap_typedescr=>describe_by_data( cs_transl ).
    lt_comp    = lo_struct->get_components( ).

    LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
      UNASSIGN <lv_field>.
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE cs_transl TO <lv_field>.
      CHECK <lv_field> IS ASSIGNED AND <lv_field> IS NOT INITIAL.

      ls_label = VALUE #( labelname = <lv_field> ).

      find_translation( CHANGING cs_label = ls_label ).

      <lv_field> = ls_label-translation.

    ENDLOOP.

    IF gv_log_object  IS NOT INITIAL.
      zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = '>> Übersetzung der LABELS abgeschlossen'
                                     io_msg       = zcl_af_core_util=>transfer_struc_val_to_msg_obj( is_structure = cs_transl ) ).
    ENDIF.
  ENDMETHOD.


  METHOD find_translation.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Übersetzung finden
*&
*&  find translation
************************************************************************

    FIELD-SYMBOLS: <ls_translations> TYPE zaf_translations.

    READ TABLE gt_translations ASSIGNING <ls_translations> WITH KEY labelname   = cs_label-labelname
                                                                    langu       = gs_transl_keys-langu
                                                                    vkorg       = gs_transl_keys-vkorg
                                                                    form_name   = gs_transl_keys-form_name.
    IF sy-subrc <> 0.
      READ TABLE gt_translations ASSIGNING <ls_translations> WITH KEY labelname   = cs_label-labelname
                                                                      langu       = gs_transl_keys-langu
                                                                      vkorg       = gs_transl_keys-vkorg.
      IF sy-subrc <> 0.
        READ TABLE gt_translations ASSIGNING <ls_translations>  WITH KEY labelname   = cs_label-labelname
                                                                         langu       = gs_transl_keys-langu.
      ENDIF.
    ENDIF.

    IF <ls_translations> IS ASSIGNED.
      cs_label-translation = <ls_translations>-translation.
    ELSE.
      cs_label-translation = '##NO_TRANSL_FOR_LANG_FOUND##'.
    ENDIF.
**********************************************************************
*    READ TABLE gt_translations ASSIGNING <ls_translations> WITH KEY labelname   = cs_label-labelname
*                                                                langu       = gs_transl_keys-langu
*                                                                vkorg       = gs_transl_keys-vkorg
*                                                                form_name   = gs_transl_keys-form_name BINARY SEARCH.
*    IF sy-subrc <> 0.
*      READ TABLE gt_translations ASSIGNING <ls_translations> WITH KEY labelname   = cs_label-labelname
*                                                                      langu       = gs_transl_keys-langu
*                                                                      vkorg       = gs_transl_keys-vkorg BINARY SEARCH.
*      IF sy-subrc <> 0.
*        READ TABLE gt_translations ASSIGNING <ls_translations>  WITH KEY labelname   = cs_label-labelname
*                                                                         langu       = gs_transl_keys-langu BINARY SEARCH.
*      ENDIF.
*    ENDIF.
*
*    IF <ls_translations> IS ASSIGNED.
*      cs_label-translation = <ls_translations>-translation.
*    ELSE.
*      cs_label-translation = '##NO_TRANSL_FOR_LANG_FOUND##'.
*    ENDIF.
**********************************************************************
*    DATA: lt_translations TYPE zaf_t_translations.
*
*    lt_translations = VALUE #( FOR ls_translations IN gt_translations WHERE ( labelname   = cs_label-labelname
*                                                                          AND langu       = gs_transl_keys-langu
*                                                                          AND ( vkorg     = gs_transl_keys-vkorg OR vkorg = space )
*                                                                          AND ( form_name = gs_transl_keys-form_name OR form_name = space ) )
*                                                                       ( ls_translations ) ).
*
*    CASE lines( lt_translations ).
*      WHEN 0. cs_label-translation = '##NO_TRANSL_FOR_LANG_FOUND##'.
*      WHEN 1. cs_label-translation = VALUE #( lt_translationas[ 1 ]-translation OPTIONAL ).
*      WHEN OTHERS.
*        SORT lt_translations BY vkorg DESCENDING form_name DESCENDING.
*        cs_label-translation = VALUE #( lt_translationas[ 1 ]-translation OPTIONAL ).
*    ENDCASE.
**********************************************************************
  ENDMETHOD.


  METHOD get_single_translation.
************************************************************************
*&  Key           : WG-230414
*&  Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
************************************************************************
*&  Description (short)
*&  Übersetzungsstruktur füllen
*&  Der übergebene Strukturname muss in Tabelle gt_labelnames überführt werden.
*&
*& Fill the translation structure
*& Get single translation
************************************************************************

    DATA: lt_range_vkorg     TYPE RANGE OF vkorg,
          lt_range_form_name TYPE RANGE OF form_name.

    IF iv_vkorg IS NOT INITIAL.
      lt_range_vkorg = VALUE #( sign = 'I' option = 'EQ' ( low = space ) ( low = iv_vkorg ) ).
    ENDIF.

    IF iv_vkorg IS NOT INITIAL.
      lt_range_form_name = VALUE #( sign = 'I' option = 'EQ' ( low = space ) ( low = iv_form_name ) ).
    ENDIF.

    SELECT SINGLE translation
      FROM zaf_translations
      INTO rv_translation
      WHERE labelname =  iv_labelname
        AND langu     =  iv_langu
        AND vkorg     IN lt_range_vkorg
        AND form_name IN lt_range_form_name.

  ENDMETHOD.


  METHOD select_translations.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Übersetzung selektieren
*&
*& Select translation
************************************************************************

    IF lines( gt_labelnames ) > 0.
      SELECT *
        FROM zaf_translations
        INTO CORRESPONDING FIELDS OF TABLE gt_translations
        FOR ALL ENTRIES IN gt_labelnames
        WHERE langu     =  gs_transl_keys-langu
          AND labelname =  gt_labelnames-labelname.
**********************************************************************
*      SELECT *
*        FROM zaf_translations
*        INTO CORRESPONDING FIELDS OF TABLE gt_translations
*        FOR ALL ENTRIES IN gt_labelnames
*        WHERE langu     =  gs_transl_keys-langu
*          AND labelname =  gt_labelnames-labelname
*        ORDER BY PRIMARY KEY.
**********************************************************************
    ENDIF.

  ENDMETHOD.
ENDCLASS.
