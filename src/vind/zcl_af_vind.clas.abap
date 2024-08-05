class ZCL_AF_VIND definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF  ts_return,
             code    TYPE n LENGTH 2,
             message TYPE c LENGTH 180,
           END OF ts_return .
  types:
    tt_return TYPE STANDARD TABLE OF ts_return WITH EMPTY KEY .
  types:
    tt_vind_data TYPE STANDARD TABLE OF zaf_vind_data_i WITH EMPTY KEY .
  types TS_ORG_DATA type ZAF_S_ORG_DATA_VALUES .
  types:
    BEGIN OF ts_adrs_labels,
        name1 TYPE zaf_transl_value,
        name2 TYPE zaf_transl_value,
        name3 TYPE zaf_transl_value,
        name4 TYPE zaf_transl_value,
      END OF ts_adrs_labels .
  types:
    BEGIN OF ty_signatureblock,
        row1 TYPE zaf_transl_value,
        row2 TYPE zaf_transl_value,
        row3 TYPE zaf_transl_value,
        row4 TYPE zaf_transl_value,
      END OF ty_signatureblock .
  types:
    tt_signatureblock TYPE STANDARD TABLE OF ty_signatureblock WITH NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF ts_ccode_data_head,
        ccode_row1 TYPE zaf_transl_value,
        ccode_row2 TYPE zaf_transl_value,
        ccode_row3 TYPE zaf_transl_value,
      END OF ts_ccode_data_head .
  types:
    BEGIN OF ts_ccode_data_total,
        total_label TYPE zaf_transl_value,
        brgew       TYPE char30, " vbdpl-brgew
        ntgew       TYPE char30, " vbdpl-ntgew
        all_qty     TYPE zaf_transl_value,
      END OF ts_ccode_data_total .
  types:
    BEGIN OF ts_ccode_data,
        stawn TYPE vbdpl-stawn,
        brgew TYPE char30, " vbdpl-brgew
        ntgew TYPE char30, " vbdpl-ntgew
      END OF ts_ccode_data .
  types:
    tt_ccodes_data TYPE STANDARD TABLE OF ts_ccode_data WITH NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF ts_ccode,
        vbeln TYPE vbdpl-vbeln,
        stawn TYPE vbdpl-stawn,
        brgew TYPE vbdpl-brgew,
        ntgew TYPE vbdpl-ntgew,
        gewei TYPE vbdpl-gewei,
      END OF ts_ccode .
  types:
    tts_ccodes TYPE STANDARD TABLE OF ts_ccode WITH NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF ts_after_all_pos_vse,
        vsenr1 TYPE vhilm_ku,
        vsenr2 TYPE vhilm_ku,
        vsenr3 TYPE vhilm_ku,
        vsenr4 TYPE vhilm_ku,
      END OF ts_after_all_pos_vse .
  types:
    BEGIN OF ts_after_pos,
        vbeln  TYPE vbeln,
        posnr  TYPE posnr,
        zlabel TYPE zaf_transl_value,
        zvalue TYPE iacxmlpropertyvalue,
      END OF ts_after_pos .
  types:
    BEGIN OF ts_after_pos_barcode,
        vbeln  TYPE vbeln,
        posnr  TYPE posnr,
        zvalue TYPE iacxmlpropertyvalue,
      END OF ts_after_pos_barcode .
  types:
    BEGIN OF ts_after_pos_ident,
        posnr     TYPE posnr,
        selnum    TYPE c LENGTH 3, "zcs_selnum,
        sellabel  TYPE string,
        clip      TYPE char01,
        str_ident TYPE string,
      END OF ts_after_pos_ident .
  types:
    BEGIN OF ts_after_pos_ident2,
        posnr      TYPE posnr,
        str_ident1 TYPE string,
        str_ident2 TYPE string,
      END OF ts_after_pos_ident2 .
  types:
    BEGIN OF ts_after_pos_ident_barcode,
        posnr     TYPE posnr,
        selnum    TYPE c LENGTH 3, "zcs_selnum,
        str_ident TYPE string,
      END OF ts_after_pos_ident_barcode .
  types:
    BEGIN OF ts_after_pos_ident_pal,
        posnr   TYPE posnr,
        zlabel  TYPE zaf_transl_value,
        zvalue  TYPE iacxmlpropertyvalue,
        zvalue2 TYPE iacxmlpropertyvalue,
      END OF ts_after_pos_ident_pal .
  types:
    BEGIN OF ts_after_pos_retourentext,
        vbeln  TYPE vbeln,
        posnr  TYPE posnr,
        ztext1 TYPE text30,
        ztext2 TYPE text40,
      END OF ts_after_pos_retourentext .
  types:
    BEGIN OF ts_after_pos_sernr,
        posnr     TYPE posnr,
        zlabel    TYPE zaf_transl_value,
        str_sernr TYPE string,
      END OF ts_after_pos_sernr .
  types:
    BEGIN OF ts_after_pos_pick_p,
        posnr       TYPE posnr,
        tanum_label TYPE zaf_transl_value,
        tanum       TYPE ltap-tanum,
        tapos_label TYPE zaf_transl_value,
        tapos       TYPE ltap-tapos,
        vltyp_label TYPE zaf_transl_value,
        vltyp       TYPE ltap-vltyp,
        vlpla_label TYPE zaf_transl_value,
        vlpla       TYPE ltap-vlpla,
        pickq_label TYPE zaf_transl_value,
        pickq       TYPE char13,
      END OF ts_after_pos_pick_p .
  types:
    BEGIN OF ts_after_pos_sn_text,
        vbeln  TYPE vbeln,
        posnr  TYPE posnr,
        zlabel TYPE zaf_transl_value,
        zvalue TYPE iacxmlpropertyvalue,
      END OF ts_after_pos_sn_text .
  types:
    BEGIN OF ts_before_pos,
        vbeln  TYPE vbeln,
        posnr  TYPE posnr,
        zlabel TYPE zaf_transl_value,
        zvalue TYPE iacxmlpropertyvalue,
      END OF ts_before_pos .
  types:
    BEGIN OF ts_bundle_text,
        vbeln  TYPE vbeln,
        posnr  TYPE posnr,
        zlabel TYPE zaf_transl_value,
      END OF ts_bundle_text .
  types:
    BEGIN OF ts_head,
        vgbel              TYPE vbdkl-vgbel,
        sland              TYPE vbdkl-sland,
        adrnr_ag           TYPE vbdkl-adrnr_ag,
        adrnr_we           TYPE vbdkl-adrnr_we,
        vbeln              TYPE vbdkl-vbeln,
        erdat              TYPE vbdkl-erdat,
        kunag              TYPE vbdkl-kunag,
        nachnahme_label    TYPE zaf_transl_value,
        kunden_ref_label   TYPE zaf_transl_value,
        kunden_ref_barcode TYPE vbkd-bstkd,
        delivery_weight    TYPE zaf_transl_value,
        btgew              TYPE char30 , "vbdkl-btgew,
        unit               TYPE zaf_transl_value,
        decl_of_goods      TYPE zaf_transl_value,
        goods_discription  TYPE zaf_transl_value,
        iln_label          TYPE zaf_transl_value,
        iln                TYPE char13,
      END OF ts_head .
  types:
    BEGIN OF ts_info_data,
        label1  TYPE char40,
        value1  TYPE char40,
        label2  TYPE char40,
        value2  TYPE char40,
        label3  TYPE char40,
        value3  TYPE char40,
        label4  TYPE char40,
        value4  TYPE char40,
        label5  TYPE char40,
        value5  TYPE char40,
        label6  TYPE char40,
        value6  TYPE char40,
        label7  TYPE char40,
        value7  TYPE char40,
        label8  TYPE char40,
        value8  TYPE char40,
        label9  TYPE char40,
        value9  TYPE char40,
        label10 TYPE char40,
        value10 TYPE char40,
        label11 TYPE char40,
        value11 TYPE char40,
      END OF ts_info_data .
  types:
    BEGIN OF ts_key_params.
        INCLUDE TYPE stxh_key.
        INCLUDE TYPE zaf_s_text_params.
    TYPES: END OF ts_key_params .
  types:
    BEGIN OF ts_layout_posnr,
        vbeln TYPE vbeln,
        posnr TYPE posnr,
        uepos TYPE uepos,
        unpos TYPE posnr,
      END OF ts_layout_posnr .
  types:
    BEGIN OF ts_log_fields,
        system_id        TYPE sysysid,
        tcode            TYPE sytcode,
        form_name        TYPE fpwbformname,
        zstceg_versender TYPE stceg,
        kunag            TYPE kunag,
        zstceg_kunde     TYPE stceg,
        ansprechpartner  TYPE char70,
        ztelnr           TYPE telfx,
        ztelfx           TYPE telfx,
        smtp_empf        TYPE ad_smtpadr,
        versandstelle    TYPE bezei30,
        vbeln_vauf_label TYPE text30,
        vbeln_vauf       TYPE vbdkl-vgbel,
        kunden_ref_label TYPE text30,
        kunden_ref       TYPE vbkd-bstkd,
      END OF ts_log_fields .
  types:
    BEGIN OF ts_pos,
        posnr        TYPE char6,
        h_pos        TYPE char6,
        u_pos        TYPE char6,
        matnr        TYPE vbdpl-matnr,
        arktx        TYPE vbdpl-arktx,
        lfimg        TYPE char13,
        vrkme        TYPE vbdpl-vrkme,
        bundle_label TYPE zaf_transl_value,
        stawn_label  TYPE zaf_transl_value,
        stawn        TYPE vbdpl-stawn,
        brgew_label  TYPE zaf_transl_value,
        brgew        TYPE char30, " vbdpl-brgew,
        ntgew_label  TYPE zaf_transl_value,
        ntgew        TYPE char30, " vbdpl-ntgew,
      END OF ts_pos .
  types:
    BEGIN OF ts_translation,
        title_vdelnote          TYPE zaf_transl_value,
        title_vdelnote_mail     TYPE zaf_transl_value,
        title_versandbest       TYPE zaf_transl_value,
        lieferanschrift         TYPE zaf_transl_value,
        datum                   TYPE zaf_transl_value,
        lieferscheinnr          TYPE zaf_transl_value,
        ust1nr                  TYPE zaf_transl_value,
        kundennr                TYPE zaf_transl_value,
        ust2nr                  TYPE zaf_transl_value,
        ansprechpartner         TYPE zaf_transl_value,
        telefon                 TYPE zaf_transl_value,
        fax                     TYPE zaf_transl_value,
        versandstelle           TYPE zaf_transl_value,
        seite                   TYPE zaf_transl_value,
        kundenbestellnr         TYPE zaf_transl_value,
        auftragnr               TYPE zaf_transl_value,
        pos                     TYPE zaf_transl_value,
        artikelnummer1          TYPE zaf_transl_value,
        bezeichnung1            TYPE zaf_transl_value,
        menge1                  TYPE zaf_transl_value,
        herstellermat           TYPE zaf_transl_value,
        ean_upc_code            TYPE zaf_transl_value,
        serialnummer            TYPE zaf_transl_value,
        rma_nr                  TYPE zaf_transl_value,
        warranty_code           TYPE zaf_transl_value,
        serialnr                TYPE zaf_transl_value,
        versandelement          TYPE zaf_transl_value,
        vsebezeichnung1         TYPE zaf_transl_value,
        vsebezeichnung2         TYPE zaf_transl_value,
        kundenmat               TYPE zaf_transl_value,
        sn_text                 TYPE zaf_transl_value,
        set_positions_text      TYPE zaf_transl_value,
        bewertungsart           TYPE zaf_transl_value,
        stueck_kurz             TYPE zaf_transl_value,
        email                   TYPE zaf_transl_value,
        qm_nummer               TYPE zaf_transl_value,
        nachnahme               TYPE zaf_transl_value,
        statitische_warennummer TYPE zaf_transl_value,
        nettogewicht            TYPE zaf_transl_value,
        bruttogewicht           TYPE zaf_transl_value,
        gesamt_total            TYPE zaf_transl_value,
        alle_gewichte_in_kg     TYPE zaf_transl_value,
        ta_nr                   TYPE zaf_transl_value,
        ta_pos                  TYPE zaf_transl_value,
        lagertyp                TYPE zaf_transl_value,
        lagerplatz              TYPE zaf_transl_value,
        menge                   TYPE zaf_transl_value,
        absender                TYPE zaf_transl_value,
        unterschrift            TYPE zaf_transl_value,
        nachname                TYPE zaf_transl_value,
        fahrer                  TYPE zaf_transl_value,
        lkw_typ_nummernschild   TYPE zaf_transl_value,
        empfaenger              TYPE zaf_transl_value,
        absender_beladung       TYPE zaf_transl_value,
        spediteur               TYPE zaf_transl_value,
        warenempfaenger         TYPE zaf_transl_value,
        warenuebergabe          TYPE zaf_transl_value,
        absender_uebergabeplatz TYPE zaf_transl_value,
        rechnungsadresse        TYPE zaf_transl_value,
        liefergewicht           TYPE zaf_transl_value,
        kg                      TYPE zaf_transl_value,
        warenbeschreibung       TYPE zaf_transl_value,
        warenbeschr_text        TYPE zaf_transl_value,
        iln                     TYPE zaf_transl_value,
        kundenreferenz          TYPE zaf_transl_value,
      END OF ts_translation .
  types:
    BEGIN OF ts_vbeln_vauf_bstkd,
        posnr      TYPE posnr,
        vbeln_vauf TYPE vbeln_vauf,
        bstkd      TYPE bstkd,

      END OF ts_vbeln_vauf_bstkd .
  types:
    tt_after_all_pos_vse       TYPE STANDARD TABLE OF ts_after_all_pos_vse WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_after_pos               TYPE STANDARD TABLE OF ts_after_pos WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_after_pos_barcode       TYPE STANDARD TABLE OF ts_after_pos_barcode WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_after_pos_ident         TYPE STANDARD TABLE OF ts_after_pos_ident WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_after_pos_ident2        TYPE STANDARD TABLE OF ts_after_pos_ident2 WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_after_pos_ident_barcode TYPE STANDARD TABLE OF ts_after_pos_ident_barcode WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_after_pos_ident_pal     TYPE STANDARD TABLE OF ts_after_pos_ident_pal WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_after_pos_retourentexte TYPE STANDARD TABLE OF ts_after_pos_retourentext WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_after_pos_sernr         TYPE STANDARD TABLE OF ts_after_pos_sernr WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_after_pos_pick_p         TYPE STANDARD TABLE OF ts_after_pos_pick_p WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_after_pos_sn_texte      TYPE STANDARD TABLE OF ts_after_pos_sn_text WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_before_pos              TYPE STANDARD TABLE OF ts_before_pos WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_bundle_text             TYPE STANDARD TABLE OF ts_bundle_text WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_key_params              TYPE STANDARD TABLE OF ts_key_params .
  types:
    tt_layout_posnr            TYPE STANDARD TABLE OF ts_layout_posnr WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_pos                     TYPE STANDARD TABLE OF ts_pos WITH DEFAULT KEY .
  types:
    tt_vbeln_vauf_bstkd        TYPE STANDARD TABLE OF ts_vbeln_vauf_bstkd .
  types:
    BEGIN OF ts_vind_footer,
        line001 TYPE text100,
        line002 TYPE text100,
        line003 TYPE text100,
        line004 TYPE text100,
        line005 TYPE text100,
      END OF ts_vind_footer .
  types:
    BEGIN OF ts_vind_org_data,
        adrline001 TYPE text100,
        adrline002 TYPE text100,
        footer1    TYPE ts_vind_footer,
        footer2    TYPE ts_vind_footer,
        footer3    TYPE ts_vind_footer,
        footer4    TYPE ts_vind_footer,
        logopos_l  TYPE c LENGTH 1, " left
        logopos_m  TYPE c LENGTH 1, " middle
        logopos_r  TYPE c LENGTH 1, " right
        infopos_l  TYPE c LENGTH 1,
        infopos_r  TYPE c LENGTH 1,
      END OF ts_vind_org_data .
  types:
    BEGIN OF ts_data_spec,
        is_sample_printout           TYPE zaf_s_sample_printout,
        is_head                      TYPE ts_head,
        is_adrs_head_we              TYPE adrs_print,
        is_info_data                 TYPE ts_info_data,
        is_addressbock_labels        TYPE ts_adrs_labels,
        it_texts                     TYPE zaf_t_text,
        it_adrs                      TYPE zaf_t_adrs,
        it_adrs_left                 TYPE zaf_t_adrs,
        it_adrs_right                TYPE zaf_t_adrs,
        it_pos                       TYPE tt_pos,
        it_data_before_pos           TYPE tt_before_pos,
        it_data_after_pos            TYPE tt_after_pos,
        it_barcode_after_pos         TYPE tt_after_pos_barcode,
        it_after_pos_ident           TYPE tt_after_pos_ident,
        it_after_pos_ident_barcode   TYPE tt_after_pos_ident_barcode,
        it_after_pos_ident2          TYPE tt_after_pos_ident2,
        it_after_pos_ident_pal       TYPE tt_after_pos_ident_pal,
        it_after_pos_sernr           TYPE tt_after_pos_sernr,
        it_after_all_pos_pick_p      TYPE tt_after_pos_pick_p,
        it_after_all_pos_vse         TYPE tt_after_all_pos_vse,
        it_after_all_pos_ccodes      TYPE tt_ccodes_data,
        is_after_all_pos_ccode_head  TYPE ts_ccode_data_head,
        is_after_all_pos_ccode_total TYPE ts_ccode_data_total,
        it_after_pos_retourentexte   TYPE tt_after_pos_retourentexte,
        it_after_pos_sn_texte        TYPE tt_after_pos_sn_texte,
        it_bundle_text               TYPE tt_bundle_text,
        it_after_all_pos_signaturebl TYPE tt_signatureblock,
        is_vind_org_data             TYPE ts_vind_org_data,
      END OF ts_data_spec .

  data MO_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  class-data MO_HTML_CONTROL type ref to CL_GUI_HTML_VIEWER .
  data MV_RETURN type SYSUBRC read-only .

  methods CONSTRUCTOR
    importing
      !IV_KUNNR type KUNNR default '0000000815' .
  class-methods GET_VIND_DATA
    importing
      !IV_FORMULAR type FORMULAR optional
      !IV_FIELDNAME type FIELDNAME optional
      !IV_KUNNR type KUNNR
    returning
      value(RT_VIND_DATA) type TT_VIND_DATA .
  class-methods INS_VIND_DATA
    importing
      !IS_DATA type ZAF_RFC_VIND
    changing
      !CT_RETURN type TT_RETURN optional .
  methods CREATE_DELIVERY
    changing
      !CT_RETURN type TT_RETURN optional .
  methods OUTPUT_PDF .
  methods CREATE_CONTAINER .
  methods SAVE
    changing
      !CT_RETURN type TT_RETURN optional .
  methods COPY_LOGO_TO_BUFFER .
protected section.
private section.

  class-data MV_KUNNR type KUNNR .
  data MO_RET type ref to ZCL_INT_DELIVERY_PRINTING .
  data C_FORM type FORMNAME value 'ZAF_DELNOTE_VIND' ##NO_TEXT.
  data MS_TRANSLATIONS type TS_TRANSLATION .
  data MS_DATA_SPEC type TS_DATA_SPEC .
  data MS_DATA_CORE type ZCL_AF_CORE_OUTPUT=>TS_DATA_CORE .
  data CV_LOG_OBJECT type BALOBJ_D value 'DEL_NOTE' ##NO_TEXT.
  data MT_VIND_DATA type TT_VIND_DATA .
  data MV_FORMOUTPUT type FPFORMOUTPUT .
  data MV_LOGO type BCS_CONTENTS_BIN .

  methods CREATE_DATA
    changing
      !CT_RETURN type TT_RETURN .
  methods OUTPUT_DELIVERY
    changing
      !CT_RETURN type TT_RETURN .
  methods GET_TRANSLATIONS
    changing
      !CT_RETURN type TT_RETURN .
  methods GET_ADRS
    changing
      !CT_RETURN type TT_RETURN .
  methods GET_POS_DATA
    changing
      !CT_RETURN type TT_RETURN .
  methods SET_VIND_DATA
    importing
      !IT_VIND_DATA type ZCL_INT_PRINT_MANAGEMENT=>TT_VIND_DATA
    changing
      !CT_RETURN type TT_RETURN .
  methods GET_LOGO_VIND
    changing
      !CT_RETURN type TT_RETURN .
  methods GET_LOGO
    importing
      !IV_LOGONAME type SKWF_URLP
    changing
      !CT_RETURN type TT_RETURN
    returning
      value(EV_PICTURE) type BCS_CONTENTS_BIN .
  methods GET_INFO_DATA
    changing
      !CT_RETURN type TT_RETURN .
  methods CALL_SCREEN_PDF .
  methods OUTPUT_PDF_IN_CONTAINER .
  methods SHOW_PDF .
  methods CHANGE_LOGO
    importing
      !IV_PARAMETER type STRING .
  methods IS_VIND
    importing
      !IV_ACTIVE type BOOLEAN default ABAP_OFF
    returning
      value(RV_RETURN) type BOOLEAN .
ENDCLASS.



CLASS ZCL_AF_VIND IMPLEMENTATION.


  METHOD call_screen_pdf.

    PERFORM call_screen_vind IN PROGRAM saplzaf_vind IF FOUND USING me.

  ENDMETHOD.


  METHOD change_logo.
**********************************************************************
*& Key           : WG-230721
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Change logo
**********************************************************************

    DATA: rf_exec TYPE REF TO cl_gui_frontend_services.


    CONSTANTS: co_application TYPE string VALUE 'C:\Program Files\paint.net\paintdotnet.exe'.

    DATA(lo_exec) = NEW cl_gui_frontend_services( ).
    CHECK sy-subrc = 0 AND lo_exec IS BOUND..

*    DATA: lv_parameter TYPE string.
*    lv_parameter = |/p C:/TEMP/Versatel.bmp|.

    TRY.
        rf_exec->execute( EXPORTING  application            = co_application   " Pfad + Anwendungsname
                                     parameter              = iv_parameter     " Parameter für Anwendung
                                     operation              = 'OPEN'           " Reserviert: Verb für ShellExecute
                          EXCEPTIONS cntl_error             = 1                " Controlfehler
                                     error_no_gui           = 2                " Kein GUI verfügbar
                                     bad_parameter          = 3                " Falsche Parameterkombination
                                     file_not_found         = 4                " Datei nicht gefunden
                                     path_not_found         = 5                " Pfad nicht gefunden
                                     file_extension_unknown = 6                " Anwendung für angegebene Erweiterung wurde nicht gefunden
                                     error_execute_failed   = 7                " Durchführung der Anwendung oder des Dokuments fehlgeschlagen
                                     synchronous_failed     = 8                " Synchroner Aufruf der Applikation nicht möglich
                                     not_supported_by_gui   = 9                " Nicht unterstützt von GUI
                                     OTHERS                 = 10 ).

        IF sy-subrc <> 0.
          MESSAGE SWITCH #( sy-subrc WHEN 1 THEN 'Controlfehler'
                                     WHEN 2 THEN 'Kein GUI verfügbar'
                                     WHEN 3 THEN 'Falsche Parameterkombination'
                                     WHEN 4 THEN 'Datei nicht gefunden'
                                     WHEN 5 THEN 'Pfad nicht gefunden'
                                     WHEN 6 THEN 'Anwendung für angegebene Erweiterung wurde nicht gefunden'
                                     WHEN 7 THEN 'Durchführung der Anwendung oder des Dokuments fehlgeschlagen'
                                     WHEN 8 THEN 'Synchroner Aufruf der Applikation nicht möglich'
                                     WHEN 9 THEN 'Nicht unterstützt von GUI'
                                     WHEN 10 THEN 'Fehler unbekannt' ) TYPE 'W' DISPLAY LIKE 'A'.
        ENDIF.
      CATCH cx_sy_ref_is_initial.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.
**********************************************************************
*& Key           : WG-230721
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& CONSTRUCTOR
**********************************************************************

    mv_return = '99'.
    mv_kunnr = iv_kunnr.

  ENDMETHOD.


  METHOD COPY_LOGO_TO_BUFFER.
**********************************************************************
*& Key           : WG-230721
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Copy logo to buffer
**********************************************************************

    DATA lv_server_path TYPE epsdirnam.

    lv_server_path = |C:/TEMP/{ VALUE #( mt_vind_data[ 1 ]-kunnr OPTIONAL ) }.bmp|.

    TRY.
        OPEN DATASET lv_server_path FOR OUTPUT IN BINARY MODE.
        TRANSFER mv_logo TO lv_server_path.
        CLOSE DATASET lv_server_path.
      CATCH cx_sy_file_open_mode INTO DATA(lo_trans_error).

        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD create_container.

    mo_container = NEW cl_gui_custom_container( container_name = 'CONTAINER_VIND' ).

  ENDMETHOD.


  METHOD create_data.
**********************************************************************
*& Key           : WG-230721
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Create data
**********************************************************************

    get_translations( CHANGING ct_return = ct_return ).
    get_info_data( CHANGING ct_return = ct_return ).
    get_adrs( CHANGING ct_return = ct_return ).
    get_pos_data( CHANGING ct_return = ct_return ).

    IF is_vind( ) = abap_true.
      set_vind_data( EXPORTING it_vind_data = mt_vind_data CHANGING ct_return = ct_return ).
    ENDIF.

  ENDMETHOD.


  METHOD create_delivery.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Insert VIND data
**********************************************************************

    TYPES: BEGIN OF ts_vkorg_langu,
             vkorg TYPE vkorg,
             langu TYPE sylangu,
           END OF ts_vkorg_langu.

    CLEAR ct_return.

    DATA lt_vkorg_langu TYPE STANDARD TABLE OF ts_vkorg_langu.

    DATA(lv_langu) = sy-langu.

    IF is_vind( ) = abap_true.
      mt_vind_data = zcl_af_vind=>get_vind_data( mv_kunnr ).
    ENDIF.

    CHECK lines( mt_vind_data ) > 0.

    lt_vkorg_langu = VALUE #( ( vkorg = '1010' langu = 'DE' )
                              ( vkorg = '6110' langu = 'DE' )
                            ).

    sy-langu = VALUE #( lt_vkorg_langu[ vkorg = VALUE #( mt_vind_data[ fieldname = 'VKORG' ]-value  OPTIONAL ) ]-langu OPTIONAL ).

    IF sy-langu IS INITIAL.
      sy-langu = 'EN'.
    ENDIF.

    create_data( CHANGING ct_return = ct_return ).

    output_delivery( CHANGING ct_return = ct_return ).

    " Ausgabe PDF in Container
    call_screen_pdf( ).

    sy-langu = lv_langu.

    IF lines( ct_return ) = 0.
      ct_return = VALUE #( ( message = 'no problem!' ) ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_ADRS.
**********************************************************************
*& Key           : WG-230721
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get adress data / Recipient of goods
**********************************************************************
    CONSTANTS: cv_separator TYPE c LENGTH 1 VALUE ';'.

    TYPES: BEGIN OF ts_adr_line,
             line TYPE c LENGTH 50,
           END OF ts_adr_line.

    DATA: lt_adrs_lines TYPE STANDARD TABLE OF ts_adr_line,
          lv_fieldname  TYPE fieldname,
          lv_line       TYPE n LENGTH 1.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    lt_adrs_lines = VALUE #( ( |Firma| )
                             ( |Max Mustermann| )
                             ( |Mustermann Strasse 999| )
                             ( |99999 Musterhausen| )
                             ( |Deutschland| ) ).

    LOOP AT lt_adrs_lines ASSIGNING FIELD-SYMBOL(<ls_adrs_line>).
      lv_line = sy-tabix - 1.
      lv_fieldname = |line{ lv_line }|.
      UNASSIGN <lv_value>.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE ms_data_spec-is_adrs_head_we TO <lv_value>.
      CHECK <lv_value> IS  ASSIGNED.
      <lv_value> = <ls_adrs_line>-line.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_info_data.
**********************************************************************
*& Key           : WG-230721
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get logo / postmark
**********************************************************************

    ms_data_spec-is_info_data = VALUE #( BASE  ms_data_spec-is_info_data
                                         value1 = |{ sy-datum DATE = USER }|
                                         value2 = '123456789'
                                         value3 = '987654321'
                                         value4 = ' '
                                         value5 = VALUE #( mt_vind_data[ 1 ]-kunnr OPTIONAL )
                                         value6 = ' '
                                         value7 = ' '
                                         value8 = ' '
                                         value9 = ' '
                                         value10 = ' '
                                         value11 = '' ).

    ms_data_spec-is_head-vbeln = ms_data_spec-is_info_data-value2.

  ENDMETHOD.


  METHOD get_logo.
**********************************************************************
*& Key           : WG-230721
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get logo / postmark
**********************************************************************
    CONSTANTS: c_path    TYPE skwf_url  VALUE zcl_af_core_constants=>gc_mime_repository_logo_path.  " '/SAP/BC/fp/graphics/PUBLIC/ADOBE_FORMS/'

    TRY.
        CALL METHOD zcl_af_core_util=>get_logo
          EXPORTING
            iv_path       = c_path
            iv_log_object = cv_log_object
            iv_logoname   = iv_logoname
          RECEIVING
            rv_logo       = ev_picture.

      CATCH zcx_af_core_output INTO data(cx_error).
        ct_return = VALUE #( BASE ct_return ( code = sy-subrc message = cx_error->get_text( ) ) ).
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD get_logo_vind.
**********************************************************************
*& Key           : WG-230721
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get logo / VIND
**********************************************************************
    " LOGO von dem Appl.-Server einlesen.

    DATA: lv_path        TYPE salfile-longname,
          lv_server_path TYPE epsdirnam,
          lv_logo_path   TYPE string.

    " Pfadname zum log. Pfad holen
    CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
        logical_filename = 'ZDIR_LOGOS'
      IMPORTING
        file_name        = lv_path
      EXCEPTIONS
        file_not_found   = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
      ct_return = VALUE #( BASE ct_return ( code = 2  message = 'Get filename / ZDIR_LOGOS' ) ).
      RETURN.
    ENDIF.

    IF NOT line_exists( mt_vind_data[ fieldname  = 'LOGONAME' ] ).
      lv_server_path = |{ lv_path }FROM_DLS/{ VALUE #( mt_vind_data[ 1 ]-kunnr OPTIONAL ) }.bmp|.
    ELSE.
      lv_server_path = |{ lv_path }FROM_DLS/{ VALUE #( mt_vind_data[ fieldname = 'LOGONAME' ]-value OPTIONAL ) }|.
    ENDIF.

*    lv_server_path = |/mnt/external/logos/TE1/FROM_DLS/viatec.bmp|.  " TEST

    TRY.
        OPEN DATASET lv_server_path FOR INPUT IN BINARY MODE.
        READ DATASET lv_server_path INTO mv_logo.
        CLOSE DATASET lv_server_path.
      CATCH cx_sy_file_open_mode INTO DATA(lo_trans_error).
        ct_return = VALUE #( BASE ct_return ( code = 02 message = lo_trans_error->get_text( ) ) ).
        RETURN.
    ENDTRY.

    copy_logo_to_buffer( ).

*--- TEST ----------------------------------------------------
    lv_logo_path = lv_server_path.

    change_logo( lv_logo_path ).

  ENDMETHOD.


  METHOD get_pos_data.
**********************************************************************
*& Key           : WG-230721
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get positions data
**********************************************************************

    DO 20 TIMES.
      ms_data_spec-it_pos = VALUE #( BASE ms_data_spec-it_pos ( posnr = sy-index
                                                                h_pos = sy-index
                                                                matnr = |MATERIAL { sy-index }|
                                                                arktx = |Testmaterial { sy-index }|
                                                                lfimg = sy-index
                                                                vrkme = |ST|
                                                              ) ).
    ENDDO.

  ENDMETHOD.


  METHOD get_translations.
**********************************************************************
*& Key           : WG-230721
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get translations
**********************************************************************
    " Collect data for translations

    DATA lv_vkorg TYPE vkorg.

    lv_vkorg = VALUE #( mt_vind_data[ fieldname = 'VKORG' ]-value OPTIONAL ).

    DATA(lo_translation) = NEW zcl_af_core_translator( is_transl_keys = VALUE #( vkorg     = lv_vkorg
                                                                                 form_name = c_form
                                                                                 langu     = sy-langu ) ).

    lo_translation->fill_translations( CHANGING cs_transl = ms_translations ).

    " Translations / INFO Window
    ms_data_spec-is_info_data = VALUE #(
      label1  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'DATUM'           iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = c_form )
      label2  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'LIEFERSCHEINNR'  iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = c_form )
      label3  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'AUFTRAGNR'       iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = c_form )
      label4  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'UST1NR'          iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = c_form )
      label5  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'KUNDENNR'        iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = c_form )
      label6  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'REFERENZ'        iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = c_form )
      label7  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'UST2NR'          iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = c_form )
      label8  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'ANSPRECHPARTNER' iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = c_form )
      label9  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'TELEFON'         iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = c_form )
      label10 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'FAX'             iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = c_form )
      label11 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'VERSANDSTELLE'   iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = c_form ) ).

  ENDMETHOD.


  METHOD get_vind_data.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get VIND data
**********************************************************************
    DATA: lt_ra_formular  TYPE RANGE OF formular,
          lt_ra_fieldname TYPE RANGE OF fieldname.

    CLEAR rt_vind_data.

    IF iv_formular IS NOT INITIAL.
      lt_ra_formular = VALUE #( sign = 'I' option = 'EQ' ( low = iv_formular )
                                                         ( ) ).
    ENDIF.

    IF iv_fieldname IS NOT INITIAL.
      lt_ra_fieldname = VALUE #( sign = 'I' option = 'EQ' ( low = iv_fieldname ) ).
    ENDIF.

    SELECT * FROM zaf_vind_data_i
      INTO TABLE @rt_vind_data
      WHERE kunnr     =  @iv_kunnr
        AND formular  IN @lt_ra_formular
        AND fieldname IN @lt_ra_fieldname
      ORDER BY kunnr,
               formular DESCENDING,
               fieldname,
               lfdnr.

  ENDMETHOD.


  METHOD ins_vind_data.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Insert VIND data
**********************************************************************
    DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lt_components   TYPE abap_component_tab,
          lv_name         TYPE fieldname,
          lv_line         TYPE n LENGTH 3,
          lv_insert       TYPE boolean,
          ls_vind_data    TYPE zaf_vind_data_i,
          ls_rfc_data     TYPE zaf_rfc_vind,
          lt_vind_data    TYPE STANDARD TABLE OF zaf_vind_data_i,
          ls_vind_data_h  TYPE zaf_vind_data_h.

    FIELD-SYMBOLS: <lv_value>     TYPE data,
                   <ls_component> TYPE abap_componentdescr.

    ls_rfc_data = is_data.
    IF ls_rfc_data-logoname_1 IS INITIAL.
      ls_rfc_data-logoname_1 = |{ ls_rfc_data-kunnr }.bmp|.
    ENDIF.

    lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( is_data ).
    CHECK lo_struct_descr IS BOUND.

    lt_components = lo_struct_descr->get_components( ).

    LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_comp>) WHERE name <> 'KUNNR' AND name <> 'LOGO'.

      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE ls_rfc_data TO <lv_value>.
      CHECK <lv_value> IS ASSIGNED AND <lv_value> IS NOT INITIAL.

      IF lv_insert = abap_off.
        ls_vind_data-kunnr = is_data-kunnr.
        lv_insert = abap_on.
      ENDIF.


      SPLIT  <ls_comp>-name AT '_' INTO lv_name lv_line.

      SHIFT lv_line RIGHT DELETING TRAILING space.
      OVERLAY lv_line  WITH '000'.

      ls_vind_data-fieldname = lv_name.
      ls_vind_data-lfdnr = lv_line.
      ls_vind_data-value = <lv_value>.

      lt_vind_data = VALUE #( BASE lt_vind_data ( ls_vind_data ) ).
    ENDLOOP.

    DELETE FROM zaf_vind_data_i WHERE kunnr = is_data-kunnr.

    IF lines( lt_vind_data ) > 0.
      INSERT zaf_vind_data_i FROM TABLE lt_vind_data ACCEPTING DUPLICATE KEYS.
      IF sy-subrc = 0.
        ls_vind_data_h = CORRESPONDING #( VALUE #( lt_vind_data[ 1 ] OPTIONAL ) ).
        ls_vind_data_h-aedat = sy-datum.
        ls_vind_data_h-aezet = sy-uzeit.
        ls_vind_data_h-aenam = sy-uname.
        MODIFY zaf_vind_data_h FROM ls_vind_data_h.
      ENDIF.
    ENDIF.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.


  METHOD is_vind.
**********************************************************************
*& Key           : WG-230721
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Is customer VIND
**********************************************************************
    rv_return = abap_off.

    SELECT COUNT(*) FROM zaf_vind_data_h
      WHERE kunnr  = mv_kunnr
        AND active = iv_active.

    IF sy-subrc = 0.
      rv_return = abap_on.
    ENDIF.

  ENDMETHOD.


  METHOD output_delivery.
**********************************************************************
*& Key           : WG-230721
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Óutput delivery
**********************************************************************

    DATA: lv_function_name  TYPE funcname,
          lv_interface_type TYPE fpinterfacetype,
          ls_outputparams   TYPE sfpoutputparams,
          ls_job_result     TYPE sfpjoboutput.

    " Get generated name for Form function module
    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = c_form
      IMPORTING
        e_funcname = lv_function_name.

    ls_outputparams-device = 'PRINTER'.
    ls_outputparams-dest   = 'LP01'.
    ls_outputparams-getpdf = abap_on. " Eserfolgt kein Priview


    " call function to Open the form  *****
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.

    IF sy-subrc <> 0.
      ct_return = VALUE #( BASE ct_return ( code = sy-subrc message = |Printer / JOB open error!| ) ).
      RETURN.
    ENDIF.

    TRY.
        " Call Form function module
        CALL FUNCTION lv_function_name
          EXPORTING
            is_transl          = ms_translations
            is_data_core       = ms_data_core
            is_data_spec       = ms_data_spec
          IMPORTING
            /1bcdwb/formoutput = mv_formoutput.
      CATCH cx_sy_dyn_call_param_missing INTO DATA(lo_error).
        ct_return = VALUE #( BASE ct_return ( code = 2  message = lo_error->get_text( ) ) ).
        RETURN.
    ENDTRY.

    " call function to Close the form  *****
    CALL FUNCTION 'FP_JOB_CLOSE'
      IMPORTING
        e_result       = ls_job_result
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.

    IF sy-subrc <> 0.
      ct_return = VALUE #( BASE ct_return ( code = sy-subrc message = |Printer / JOB close error!| ) ).
      RETURN.
    ENDIF.
*
**    " Datei (PDF) auf dem Appl.-Server ablegen
*    DATA lv_server_path TYPE epsdirnam.
*    lv_server_path = |/mnt/external/logos/TE1/TO_EWM/{ VALUE #( mt_vind_data[ 1 ]-kunnr OPTIONAL ) }.pdf|.
*    TRY.
*        OPEN DATASET lv_server_path FOR OUTPUT IN BINARY MODE.
*        TRANSFER mv_formoutput-pdf TO lv_server_path.
*        CLOSE DATASET lv_server_path.
*      CATCH cx_sy_file_open_mode INTO DATA(lo_trans_error).
*        ct_return = VALUE #( BASE ct_return ( code = 2  message = lo_trans_error->get_text( ) ) ).
*        RETURN.
*    ENDTRY.

  ENDMETHOD.


  METHOD output_pdf.

    IF mo_container IS NOT BOUND.
      mo_container = NEW cl_gui_custom_container( container_name = 'CONTAINER_VIND' ).
    ENDIF.

*    output_pdf_in_container( ).
    show_pdf( ).

  ENDMETHOD.


  METHOD output_pdf_in_container.

    DATA: lv_pdf_datei TYPE c LENGTH 256,
          lv_show      TYPE boolean.

    lv_pdf_datei = |/mnt/external/logos/TE1/TO_EWM/{ VALUE #( mt_vind_data[ 1 ]-kunnr OPTIONAL ) }.pdf|.

    IF mo_html_control IS NOT BOUND.
      mo_html_control = NEW cl_gui_html_viewer( parent = mo_container ).
      lv_show = abap_off.
    ELSE.
      lv_show = abap_on.
    ENDIF.

    IF sy-subrc = 0.
      IF lv_show = abap_on.
        mo_html_control->do_refresh( EXCEPTIONS cntl_error = 1
                                                OTHERS     = 2 ).
      ENDIF.

      mo_html_control->show_url( EXPORTING  url                    = lv_pdf_datei
                                            in_place               = abap_on  " Darstellung in Container
                                 EXCEPTIONS cntl_error             = 1
                                            cnht_error_not_allowed = 2
                                            cnht_error_parameter   = 3
                                            dp_error_general       = 4
                                            OTHERS                 = 5 ).

    ENDIF.
*
*    IF sy-subrc <> 0.
*      CLEAR mv_last_pdf_datei.
*      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'W'.
*    ELSE.
*      mv_last_pdf_datei = lv_pdf_datei.
*    ENDIF.

  ENDMETHOD.


  METHOD save.
**********************************************************************
*& Key           : WG-230721
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& SAVE Document (PDF) / LOGOG
**********************************************************************

    DATA lv_server_path TYPE epsdirnam.

    " Sichern PDF
    lv_server_path = |/mnt/external/logos/TE1/TO_EWM/{ VALUE #( mt_vind_data[ 1 ]-kunnr OPTIONAL ) }.pdf|.
    TRY.
        OPEN DATASET lv_server_path FOR OUTPUT IN BINARY MODE.
        TRANSFER mv_formoutput-pdf TO lv_server_path.
        CLOSE DATASET lv_server_path.
      CATCH cx_sy_file_open_mode INTO DATA(lo_trans_error).
        ct_return = VALUE #( BASE ct_return ( code = 2  message = lo_trans_error->get_text( ) ) ).
        RETURN.
    ENDTRY.

    mv_return = '00'.

*    "Sichern LOGO
*    lv_server_path = |/mnt/external/logos/TE1/TO_EWM/{ VALUE #( mt_vind_data[ 1 ]-kunnr OPTIONAL ) }.bmp|.
*
*    TRY.
*        OPEN DATASET lv_server_path FOR OUTPUT IN BINARY MODE.
*        TRANSFER mv_logo TO lv_server_path.
*        CLOSE DATASET lv_server_path.
*      CATCH cx_sy_file_open_mode INTO lo_trans_error.
*        ct_return = VALUE #( BASE ct_return ( code = 2  message = lo_trans_error->get_text( ) ) ).
*        RETURN.
*    ENDTRY.

  ENDMETHOD.


  METHOD set_vind_data.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Set VIND data
**********************************************************************

    CHECK lines( it_vind_data ) > 0.

    DATA: lv_fieldname TYPE text100.

    FIELD-SYMBOLS: <lv_spec_field> TYPE data.

    CLEAR ms_data_spec-is_vind_org_data.

    LOOP AT it_vind_data ASSIGNING FIELD-SYMBOL(<ls_vind_data>).
      UNASSIGN <lv_spec_field>.
      IF <ls_vind_data>-fieldname CS 'FOOTER'.
        lv_fieldname = |MS_DATA_SPEC-IS_VIND_ORG_DATA-{ <ls_vind_data>-fieldname }-LINE{ <ls_vind_data>-lfdnr }|.
      ENDIF.

      lv_fieldname = COND #( WHEN <ls_vind_data>-fieldname CS 'FOOTER'   THEN |MS_DATA_SPEC-IS_VIND_ORG_DATA-{ <ls_vind_data>-fieldname }-LINE{ <ls_vind_data>-lfdnr }|
                             WHEN <ls_vind_data>-fieldname =  'LOGOPOS'  THEN |MS_DATA_SPEC-IS_VIND_ORG_DATA-LOGOPOS_{ <ls_vind_data>-value }|
                             WHEN <ls_vind_data>-fieldname =  'INFOPOS'  THEN |MS_DATA_SPEC-IS_VIND_ORG_DATA-INFOPOS_{ <ls_vind_data>-value }|
                                                                         ELSE |MS_DATA_SPEC-IS_VIND_ORG_DATA-{ <ls_vind_data>-fieldname }{ <ls_vind_data>-lfdnr }| ).

      CHECK lv_fieldname IS NOT INITIAL.

      ASSIGN (lv_fieldname) TO <lv_spec_field>.
      CHECK <lv_spec_field> IS ASSIGNED.

      <lv_spec_field> = <ls_vind_data>-value.

    ENDLOOP.

    " AGB
    DATA: lt_textlines TYPE tsftext,
          lt_agb       TYPE STANDARD TABLE OF text100.

    LOOP AT it_vind_data ASSIGNING <ls_vind_data> WHERE fieldname = 'AGBLINE'.
      lt_textlines = VALUE #( BASE lt_textlines ( tdformat = '*' tdline = <ls_vind_data>-value ) ).
    ENDLOOP.
    IF NOT line_exists( ms_data_spec-it_texts[ filter = 'AGB' ] ).
      ms_data_spec-it_texts = VALUE #( BASE ms_data_spec-it_texts ( filter = 'AGB' ) ).
    ENDIF.
    ms_data_spec-it_texts[ filter = 'AGB' ]-textlines = lt_textlines.

    " LOGO
    IF ms_data_spec-is_vind_org_data-logopos_l IS INITIAL AND
       ms_data_spec-is_vind_org_data-logopos_m IS INITIAL.
      ms_data_spec-is_vind_org_data-logopos_r = abap_on.
    ENDIF.

    get_logo_vind( CHANGING ct_return = ct_return ).
    ms_data_core-wa_org_data-logo = mv_logo.

  ENDMETHOD.


  METHOD show_pdf.

    "Global Data Definations
    DATA: go_pdf_object TYPE REF TO cl_gui_html_viewer,
          go_pdf_dialog TYPE REF TO cl_gui_dialogbox_container.
*          go_pdf_handler TYPE REF TO gc_pdf_handler.

    " in the subroutine get pdf file and convert to binary
    DATA: gv_fm_name         TYPE rs38l_fnam,
          gs_fp_docparams    TYPE sfpdocparams,
          gs_fp_outputparams TYPE sfpoutputparams,
          gs_fp_outputforms  TYPE fpformoutput.

    " binary itab definations
    TYPES: BEGIN OF ty_itab,
             line TYPE x LENGTH 255,
           END OF ty_itab.
    DATA: lt_itab TYPE STANDARD TABLE OF x255, "ty_itab,
          lv_url  TYPE char255,
          lv_size TYPE i VALUE 0.

    gs_fp_outputparams-getpdf     = abap_true.

    TRY.
        " xstring to binary
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = mv_formoutput-pdf "xml
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_itab.

        " create dialogbox and set data
*        CREATE OBJECT go_pdf_dialog
*          EXPORTING
*            width  = 1200
*            height = 750.
*    CREATE OBJECT go_pdf_handler.
*    SET HANDLER   go_pdf_handler->close_pdf_view FOR go_pdf_dialog.
        CREATE OBJECT go_pdf_object
          EXPORTING
            parent = mo_container. "go_pdf_dialog.

        CALL METHOD go_pdf_object->load_data
          EXPORTING
            type                   = 'APPLICATION'  " 'TEXT'
            subtype                = 'PDF'          " 'HTML'
            size                   = lv_size
          IMPORTING
            assigned_url           = lv_url
          CHANGING
            data_table             = lt_itab
          EXCEPTIONS
            dp_invalid_parameter   = 1
            dp_error_general       = 2
            cntl_error             = 3
            html_syntax_notcorrect = 4
            OTHERS                 = 5.

        CALL METHOD go_pdf_object->show_data
          EXPORTING
            url      = lv_url
            in_place = abap_true.

        CALL METHOD go_pdf_dialog->set_visible
          EXPORTING
            visible = abap_true.
      CATCH cx_sy_ref_is_initial INTO DATA(lo_error).
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'A'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
