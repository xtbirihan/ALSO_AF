class ZCL_AF_CORE_ORG_DATA definition
  public
  final
  create public .

public section.

  data GV_LOG_OBJECT type BALOBJ_D .

  methods CONSTRUCTOR
    importing
      !IS_ORG_DATA_KEYS type ZAF_S_ORG_DATA_SEARCH_KEYS
      !IV_LOG_OBJECT type BALOBJ_D .
  class-methods GET_INSTANCE
    importing
      !IV_LOG_OBJECT type BALOBJ_D
      !IS_ORG_DATA_KEYS type ZAF_S_ORG_DATA_SEARCH_KEYS
    returning
      value(RO_ORG_DATA) type ref to ZCL_AF_CORE_ORG_DATA .
  methods GET_ORG_DATA
    changing
      value(CO_LOGGER) type ref to ZCL_AF_CORE_LOGGER optional
    returning
      value(RS_ORG_DATA_VALUES) type ZAF_S_ORG_DATA_VALUES
    raising
      ZCX_AF_CORE_OUTPUT .
protected section.
private section.

  data GS_ORG_DATA_SEARCH_KEYS type ZAF_S_ORG_DATA_SEARCH_KEYS .
  data GS_ORG_DATA type ZAF_ORG_DATA .

  methods GET_ORG_DATA_KEY
    returning
      value(RV_ORG_DATA_KEY) type CHAR40
    raising
      ZCX_AF_CORE_OUTPUT .
  methods GET_ORG_DATA_VALUES
    importing
      !IV_ORG_DATA_KEY type CHAR40
    exporting
      !EV_ORG_DATA_FOUND type ABAP_BOOL
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CHANGE_ORG_DATA_VALUES .
  methods CHECK_DELETE_LOGO
    returning
      value(RV_DELETE_LOGO) type XFLAG .
  methods CHECK_DELETE_FOOTER
    returning
      value(RV_DELETE_FOOTER) type XFLAG .
  methods CHECK_DELETE_SENDER
    returning
      value(RV_DELETE_SENDER) type XFLAG .
ENDCLASS.



CLASS ZCL_AF_CORE_ORG_DATA IMPLEMENTATION.


  METHOD change_org_data_values.
************************************************************************
*&  Key           : TS-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Es gibt diverse Einstellungen zum Deaktivieren bestimmter Felder n
*&  den Organisationsdaten. Diese Methode passt die ermittelten Organi-
*&  sationsdaten entsprechend an.
*&
*& There are various settings to disable certain fields n
*& the organizational data. This method adapts to the identified organi.
*& sations data accordingly.
************************************************************************

* Logo
    IF check_delete_logo( ) = abap_true.
      CLEAR gs_org_data-logoname.
    ENDIF.

* Sender
    IF check_delete_sender( ) = abap_true.
      CLEAR gs_org_data-sender.
    ENDIF.

* Footer
    IF check_delete_footer( ) = abap_true.
      CLEAR gs_org_data-footer.
    ENDIF.

  ENDMETHOD.


  METHOD check_delete_footer.
************************************************************************
*&  Key           : TS-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Prüfung, ob der Footer gelöscht werden soll, damit er nicht
*&  angedruckt wird.
*&
*& Check whether the footer should be deleted so that it does not
*& is printed.
************************************************************************

    CHECK gs_org_data-footer IS NOT INITIAL.

    CASE gs_org_data_search_keys-channel.
      WHEN zcl_af_core_constants=>gc_output_channel_print.
        rv_delete_footer = gs_org_data-print_skip_footer.

      WHEN zcl_af_core_constants=>gc_output_channel_preview.
        rv_delete_footer = gs_org_data-preview_skip_footer.

      WHEN zcl_af_core_constants=>gc_output_channel_archive.
        rv_delete_footer = gs_org_data-archive_skip_footer.

      WHEN OTHERS.
        rv_delete_footer = abap_false.
    ENDCASE.

  ENDMETHOD.


  METHOD check_delete_logo.
************************************************************************
*&  Key           : TS-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Prüfung, ob das Logo gelöscht werden soll, damit es nicht angedruckt
*&  wird.
*&
*& Check whether the logo should be deleted so that it does not print will.
************************************************************************

    CHECK gs_org_data-logoname IS NOT INITIAL.

    CASE gs_org_data_search_keys-channel.
      WHEN zcl_af_core_constants=>gc_output_channel_print.
        rv_delete_logo = gs_org_data-print_skip_logo.

      WHEN zcl_af_core_constants=>gc_output_channel_preview.
        rv_delete_logo = gs_org_data-preview_skip_logo.

      WHEN zcl_af_core_constants=>gc_output_channel_archive.
        rv_delete_logo = gs_org_data-archive_skip_logo.

      WHEN OTHERS.
        rv_delete_logo = abap_false.

    ENDCASE.

  ENDMETHOD.


  METHOD check_delete_sender.
************************************************************************
*&  Key           : TS-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Prüfung, ob der Sender gelöscht werden soll, damit er nicht
*&  angedruckt wird.
*&
*& Check whether the channel should be deleted so that it does not
*& is printed.
************************************************************************

    CHECK gs_org_data-sender IS NOT INITIAL.

    CASE gs_org_data_search_keys-channel.
      WHEN zcl_af_core_constants=>gc_output_channel_print.
        rv_delete_sender = gs_org_data-print_skip_sender.

      WHEN zcl_af_core_constants=>gc_output_channel_preview.
        rv_delete_sender = gs_org_data-preview_skip_sender.

      WHEN zcl_af_core_constants=>gc_output_channel_archive.
        rv_delete_sender = gs_org_data-archive_skip_sender.

      WHEN OTHERS.
        rv_delete_sender = abap_false.
    ENDCASE.

  ENDMETHOD.


  METHOD constructor.
************************************************************************
*&  Key           : AR-180606
*&  Request No.   : 180116-135917-AR - Formularentwicklung Service Label
************************************************************************
*&  Description (short)
*&  Instanziiert die spezialisierte Organisationdatenklasse
*&  Instantiates the specialised organisation data class
************************************************************************

    gs_org_data_search_keys = is_org_data_keys.
    gv_log_object           = iv_log_object.

*    zcl_af_core_logger=>write_log( iv_object    = iv_log_object
*                                   iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
*                                   iv_ext_ident = '> START - ZCL_AF_CORE_ORG_DATA' ).

  ENDMETHOD.


  METHOD get_instance.
************************************************************************
*&  Key           : TS-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Instanz für die Ermittlung der Orgdaten erstellen.
*&  Übergabe der Selektionskriterien zur Ermittlung der Orgdaten.
*&
*&  Create instance for the determination of the org data.
*&  Transfer the selection criteria for the determination of the org. data.
************************************************************************

    ro_org_data = NEW zcl_af_core_org_data( is_org_data_keys = is_org_data_keys
                                            iv_log_object    = iv_log_object ).

  ENDMETHOD.


  METHOD get_org_data.
************************************************************************
*&  Key           : TS-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Rückgabe der ermittelten Organisationsdaten
*& Return of the ascertained organisational data
************************************************************************

    DATA: lv_org_data_key   TYPE char40.
    DATA: lv_org_data_found TYPE abap_bool.

    " Ermittlung des Zugriffschlüssel
    " Determination of the access key
    lv_org_data_key = get_org_data_key( ).

    " Ermittlung der Organisationsdaten mit Zugriffschlüssel
    " Determination of the organisational data with access key
    get_org_data_values( EXPORTING iv_org_data_key   = lv_org_data_key
                         IMPORTING ev_org_data_found = lv_org_data_found ).

    IF lv_org_data_found = abap_true.
      " Organisationsdaten anhand der Logikflags anpassen
      "Adjust organisational data using the logic flags
      change_org_data_values( ).

      " Rückgabestruktur füllen
      "Fill return structure
      rs_org_data_values-sender = gs_org_data-sender.
      rs_org_data_values-footer = gs_org_data-footer.

      IF gs_org_data-logoname IS NOT INITIAL.
        " Logo aus MIME Repository lesen
        "Read logo from MIME repository
        rs_org_data_values-logo = zcl_af_core_util=>get_logo( EXPORTING iv_log_object = gv_log_object iv_logoname = gs_org_data-logoname ).

        IF rs_org_data_values-logo IS INITIAL.
          zcx_af_core_output=>raise( iv_log_object = gv_log_object
                                     iv_msgno      = '054'
                                     iv_msgty      = 'E'
                                     iv_msgv1      = CONV #( gs_org_data-logoname ) ).

        ENDIF.
      ENDIF.
    ELSE.
      zcx_af_core_output=>raise( iv_log_object = gv_log_object
                                 iv_msgno      = '053'
                                 iv_msgty      = 'E'
                                 iv_msgv1      = CONV #( lv_org_data_key ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_org_data_key.
************************************************************************
*&  Key           : TS-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Zugriffschlüssel zur Ermittlung der Organisationsdaten selektieren.
*&  Diese Methode muss immer dann erweitert werden, wenn es neue Zugriff-
*&  tabellen gibt.
*&
*& Select access keys to determine the organisational data.
*& This method must always be extended if there are new access *& tables.
*& tables exist.
************************************************************************

    SELECT SINGLE org_data_key
      FROM zaf_org_data_k01
      INTO rv_org_data_key
      WHERE vkorg = gs_org_data_search_keys-vkorg
      AND   channel = gs_org_data_search_keys-channel
      AND   form_name = gs_org_data_search_keys-form_name
      AND   datbi >= gs_org_data_search_keys-datum
      AND   datab <= gs_org_data_search_keys-datum.

    IF rv_org_data_key IS INITIAL.
      SELECT SINGLE org_data_key
      FROM zaf_org_data_k01
      INTO rv_org_data_key
      WHERE vkorg = gs_org_data_search_keys-vkorg
      AND   channel = gs_org_data_search_keys-channel
      AND   form_name = ''
      AND   datbi >= gs_org_data_search_keys-datum
      AND   datab <= gs_org_data_search_keys-datum.

      IF rv_org_data_key IS INITIAL.
        SELECT SINGLE org_data_key
        FROM zaf_org_data_k01
        INTO rv_org_data_key
        WHERE vkorg = gs_org_data_search_keys-vkorg
        AND   channel = ''
        AND   form_name = ''
        AND   datbi >= gs_org_data_search_keys-datum
        AND   datab <= gs_org_data_search_keys-datum.
      ENDIF.
    ENDIF.

    IF rv_org_data_key IS INITIAL.
      zcx_af_core_output=>raise( iv_log_object = gv_log_object
                                 iv_msgno      = '052'
                                 iv_msgty      = 'E'
                                 iv_msgv1      = CONV #( gs_org_data_search_keys-vkorg )
                                 iv_msgv2      = CONV #( gs_org_data_search_keys-form_name ) ).

    ENDIF.

  ENDMETHOD.


  METHOD get_org_data_values.
************************************************************************
*&  Key           : TS-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Mit dem Zugriffschlüssel werden die Organisationsdaten gelesen.
*& The access key is used to read the organisational data.
************************************************************************

    SELECT SINGLE *
      FROM zaf_org_data
      INTO gs_org_data
      WHERE org_data_key = iv_org_data_key.

    IF sy-subrc = 0.
      ev_org_data_found = abap_true.
    ELSE.
      ev_org_data_found = abap_false.
    ENDIF.
*<-TS-190625

  ENDMETHOD.
ENDCLASS.
