class ZCL_AF_CORE_UTIL definition
  public
  final
  create public .

public section.

  class-methods CHECK_CHANNEL_ORGDATA_EQUAL
    importing
      !IT_CHANNEL_PARAMS type ZAF_T_CHANNEL_PARAMS
    returning
      value(RV_EQUAL) type ABAP_BOOL .
  class-methods GET_STRUCT_NAME
    importing
      !IS_STRUCT type ANY
    returning
      value(RV_STRUCT_NAME) type STRING
    exceptions
      TRANSLATION_NO_STRUCT .
  class-methods GET_OUTPUT_LANGUAGE
    importing
      !IV_VKORG type VKORG
      !IV_COMM_LANGU type LANGU
    returning
      value(RV_OUT_LANGU) type LANGU .
  class-methods GET_LOGO
    importing
      !IV_PATH type SKWF_URL default ZCL_AF_CORE_CONSTANTS=>GC_MIME_REPOSITORY_LOGO_PATH
      !IV_LOG_OBJECT type BALOBJ_D
      !IV_LOGONAME type SKWF_URLP
    returning
      value(RV_LOGO) type BCS_CONTENTS_BIN
    raising
      ZCX_AF_CORE_OUTPUT .
  class-methods GET_NAST_DATA
    exporting
      !ES_NAST type NAST
      !ES_TNAPR type TNAPR
      !EV_IS_NAST_USED type ABAP_BOOL .
  class-methods GET_OUTPUTPARAMS_FROM_NAST
    importing
      !IS_NAST type NAST
      !IV_ADRNR type AD_ADDRNUM optional
      !IV_PERNR type AD_PERSNUM optional
      !IV_RECEIVER_COUNTRY type LAND1 optional
      !IV_LOG_OBJECT type BALOBJ_D
    exporting
      !ES_OUTPUTPARAMS type SFPOUTPUTPARAMS
      !ES_COMM_VALUES type SZADR_COMM_VALUES
      !ES_EXTENDED_PARAMS type ZAF_S_EXTENDED_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
  class-methods TRANSFER_STRUC_VAL_TO_MSG_OBJ
    importing
      !IS_STRUCTURE type ANY
    returning
      value(RO_MSG) type ref to ZCL_AF_CORE_MESSAGES .
  class-methods GET_KEY_VALUES_FROM_ZDEV_PREP
    importing
      !IV_LINK type ZLINK
      !IS_SOURCE_VALUES type ANY
      !IV_LOG_OBJECT type BALOBJ_D
    exporting
      !ET_KEY_VALUE type ZAF_T_TEXTS_KEY_VALUE .
  class-methods GET_ARCHIVE_KEYS_SD
    importing
      !IV_VKORG type VKORG
      !IV_VBTYP type VBTYP
      !IV_LOG_OBJECT type BALOBJ_D optional
    exporting
      !EV_AR_OBJECT type SAEOBJART
      !EV_ARCHIV_ID type SAEARCHIVI .
  class-methods GET_ARCHIVE_KEYS_LE
    importing
      !IV_VKORG type VKORG
      !IV_KEY type ZAF_ARCHIVE_KEY
      !IV_LOG_OBJECT type BALOBJ_D optional
    exporting
      !EV_AR_OBJECT type SAEOBJART
      !EV_ARCHIV_ID type SAEARCHIVI .
  class-methods GET_COMPANY_NAME
    importing
      !IV_VKORG type VKORG
      !IV_LOG_OBJECT type BALOBJ_D
    exporting
      !EV_COMPANY_NAME type SDEMO_COMPANY_NAME
    raising
      ZCX_AF_CORE_OUTPUT .
  class-methods GET_EMAIL_SENDER
    importing
      !IV_VKORG type VKORG
      !IV_VARKEY type ZAF_VARKEY optional
    exporting
      !EV_SENDER type AD_SMTPADR .
  class-methods GET_OUT_INFO
    importing
      !IV_VKORG type VKORG
      !IV_FILTER type SWC_FILVAL
      !IV_OBJKEY type OBJKEY
    exporting
      !ES_OUT_INFO type ZAF_OUT_INFO .
  class-methods GET_FORM_ID
    importing
      !IV_VKORG type VKORG
      !IV_DOCID type ZAF_DOCID
      !IV_DEST type RSPOPNAME
      !IV_LOG_OBJECT type BALOBJ_D optional
    exporting
      !EV_FORM_ID type STRING .
  class-methods GET_OUT_CHANNEL
    importing
      !IV_PROCID type PYC_PROC_ID
      !IV_FILTER type SWC_FILVAL
      !IV_LOG_OBJECT type BALOBJ_D
    exporting
      !ES_OUT_CHANNEL type ZAF_OUT_CHANNEL .
  class-methods GET_SAMPLE_PRINTOUT_DATA
    importing
      !IV_FORM_NAME type FORM_NAME
    exporting
      !ES_SAMPLE_PRINTOUT type ZAF_S_SAMPLE_PRINTOUT .
  class-methods SET_TABLE_DATA
    importing
      !IV_TABNAME type TABNAME
      !IT_DATA type DATA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AF_CORE_UTIL IMPLEMENTATION.


  METHOD check_channel_orgdata_equal.
************************************************************************
*&  Key           : JR-180515
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Prüft, ob die Org.Daten aller übergebenen Channel Parameter gleich
*&  sind.
*&
*&  Checks whether the organizational data of all transferred channel
*&  parameters is the same are.
************************************************************************
    DATA: ls_org_data_previous TYPE zaf_s_org_data_values.

    " Assume all are equal
    rv_equal = abap_true.

    LOOP AT it_channel_params ASSIGNING FIELD-SYMBOL(<ls_channel_param>).
      IF ls_org_data_previous IS NOT INITIAL AND
        ( ls_org_data_previous-footer <> <ls_channel_param>-wa_org_data_values-footer OR
          ls_org_data_previous-sender <> <ls_channel_param>-wa_org_data_values-sender OR
          ls_org_data_previous-logo   <> <ls_channel_param>-wa_org_data_values-logo ).
        rv_equal = abap_false.
        return.
      ENDIF.

      ls_org_data_previous = <ls_channel_param>-wa_org_data_values.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_archive_keys_le.
************************************************************************
*&  Key           : TS-180709
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Schlüsselfelder für die Archivierung ermitteln.
*&  Modul LE - Eindeutige Logik über Lieferart
*& identify key fields for archiving.
*& Module LE - Unambiguous logic about delivery method
************************************************************************

    DATA: lv_text       TYPE bdc_vtext1,
          lv_object_key TYPE char6.


    CLEAR: ev_ar_object, ev_archiv_id.

    CONCATENATE 'LE' iv_key INTO lv_object_key.

    " Lesen der Archivschlüsel
    " Reading the Archive Keys
    zcl_af_core_util_archivedoc=>get_archive_object( EXPORTING iv_bukrs      = iv_vkorg
                                                               iv_object_key = lv_object_key
                                                     IMPORTING ev_ar_object  = ev_ar_object
                                                               ev_archiv_id  = ev_archiv_id ).

    "Protokollierung
    "Logging
    IF iv_log_object IS NOT INITIAL.
      IF ev_archiv_id IS INITIAL.
        "Keine Archivschlüssel zu Buchungskreis &1 und Schlüssel &2 gefunden.
        "No archive keys for company code &1 and keys &2 found.
        MESSAGE ID 'ZAF_CORE' TYPE 'E' NUMBER '009'
                INTO lv_text
                WITH iv_vkorg lv_object_key.

        CONCATENATE '>>> GET_ARCHIVE_KEYS: ' lv_text INTO lv_text.

        zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                       iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                       iv_ext_ident = lv_text ).

      ELSE.
        "Archivschlüssel ermittelt: Dokumentart &1, Archiv-ID &2.
        "Archive key determined: Document type &1, archive ID &2.
        MESSAGE ID 'ZAF_CORE' TYPE 'I' NUMBER '010'
                INTO lv_text
                WITH ev_ar_object ev_archiv_id.

        CONCATENATE '>>> GET_ARCHIVE_KEYS: ' lv_text INTO lv_text.

        zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                       iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                       iv_ext_ident = lv_text ).

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_archive_keys_sd.
************************************************************************
*&  Key           : TS-180709
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Schlüsselfelder für die Archivierung ermitteln.
*&  Modul SD - Eindeutige Logik über Vertriebsbelegtyp
*&
*& identify key fields for archiving.
*& Module SD - Unambiguous logic via sales document type
************************************************************************

    DATA: lv_text       TYPE bdc_vtext1,
          lv_object_key TYPE char6.


    CLEAR: ev_ar_object, ev_archiv_id.

    CONCATENATE 'SD_' iv_vbtyp INTO lv_object_key.

    "Lesen der Archivschlüsel
    "Reading the Archive Keys
    zcl_af_core_util_archivedoc=>get_archive_object( EXPORTING iv_bukrs      = iv_vkorg
                                                               iv_object_key = lv_object_key
                                                     IMPORTING ev_ar_object  = ev_ar_object
                                                               ev_archiv_id  = ev_archiv_id ).

    "Protokollierung
    "Logging
    IF iv_log_object IS NOT INITIAL.
      IF ev_archiv_id IS INITIAL.
        "Keine Archivschlüssel zu Buchungskreis &1 und Schlüssel &2 gefunden.
        "No archive keys for company code &1 and keys &2 found.
        MESSAGE ID 'ZAF_CORE' TYPE 'E' NUMBER '009'
                INTO lv_text
                WITH iv_vkorg lv_object_key.

        CONCATENATE '>>> GET_ARCHIVE_KEYS: ' lv_text INTO lv_text.

        zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                       iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                       iv_ext_ident = lv_text ).

      ELSE.
        "Archivschlüssel ermittelt: Dokumentart &1, Archiv-ID &2.
        "Archive key determined: Document type &1, Archive ID &2..
        MESSAGE ID 'ZAF_CORE' TYPE 'I' NUMBER '010'
                INTO lv_text
                WITH ev_ar_object ev_archiv_id.

        CONCATENATE '>>> GET_ARCHIVE_KEYS: ' lv_text INTO lv_text.

        zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                       iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                       iv_ext_ident = lv_text ).

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_company_name.
************************************************************************
*&  Key           : AR-180719
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Firmennamen ermitteln.
*&  Company Name.
************************************************************************

    DATA: lv_adrnr     TYPE tvko-adrnr,
          ls_sadr      TYPE sadr,
          ls_addr1_sel TYPE addr1_sel.

    CLEAR: ev_company_name.

    " Addressnummer der VKORG ermitteln
    " Determine the address number of the VKORG
    SELECT SINGLE adrnr FROM tvko INTO lv_adrnr WHERE vkorg = iv_vkorg.

    IF sy-subrc = 0.

      ls_addr1_sel-addrnumber = lv_adrnr.

      CALL FUNCTION 'ADDR_GET'
        EXPORTING
          address_selection = ls_addr1_sel
        IMPORTING
          sadr              = ls_sadr
        EXCEPTIONS
          parameter_error   = 1
          address_not_exist = 2
          version_not_exist = 3
          internal_error    = 4
          address_blocked   = 5
          OTHERS            = 6.

      IF sy-subrc = 0.
        " Firmenname
        " Company name
        ev_company_name = ls_sadr-name1.

      ELSE.
        " VKORG wird als Firmenname dann eingetragen
        " VKORG is then registered as company name
        ev_company_name = CONV #( iv_vkorg ).

        " Mit der Adressnummer & konnte kein Firmenname ermittelt werden.
        " With the address number & no company name could be determined.
        zcx_af_core_output=>raise( iv_log_object = iv_log_object
                                   iv_msgno      = '094'
                                   iv_msgty      = 'E'
                                   iv_msgv1      = CONV #( lv_adrnr ) ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_email_sender.
************************************************************************
*&  Key           : AR-180719
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Absender E-Mail aus Tabelle ZAF_EMAIL_SENDE ermitteln.
*& Determine sender e-mail from table ZAF_EMAIL_SENDE.
************************************************************************

    CLEAR: ev_sender.

    " Absender E-Mail der VKORG und Var. Schlüssel ermitteln
    " Sender e-mail of VKORG and Var. Determine Key
    SELECT SINGLE sender FROM zaf_email_sender
                         INTO ev_sender
                        WHERE vkorg  = iv_vkorg
                          AND varkey = iv_varkey.

    IF sy-subrc <> 0.
      ev_sender = zcl_af_core_constants=>gc_noreply_sender.
    ENDIF.

  ENDMETHOD.


  METHOD get_form_id.
************************************************************************
*&  Key           : AR-180918
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Formular ID ermitteln
*&  AF_1010_00001
*&  AF_VKORG_DOCID
*&
*& Determine Form ID
*& AF_1010_00001
*& AF_VKORG_DOCID
************************************************************************

    DATA: lv_text       TYPE bdc_vtext1,
          lv_docid      TYPE zaf_docid,
          lv_dest       TYPE rspopname,
          lv_object_key TYPE char6.

    CLEAR: ev_form_id.

    lv_dest = iv_dest.

    SELECT SINGLE docid FROM zaf_document_id
                        INTO lv_docid
                       WHERE docid = iv_docid
                         AND vkorg = iv_vkorg.

    IF sy-subrc = 0.
      " Damit im Form ID
      " Thus in the form ID
      IF lv_dest IS INITIAL.
        lv_dest = 'PREV'.
      ENDIF.
      CONCATENATE 'AF' iv_vkorg lv_docid lv_dest INTO ev_form_id SEPARATED BY '_'.
    ENDIF.

    " Protokollierung
    " Logging
    IF iv_log_object IS NOT INITIAL.
      IF ev_form_id IS INITIAL.
        " Keine Formular ID zu Buchungskreis & und Schlüssel & gefunden.
        " No form ID for company code & and key & found.
        MESSAGE ID 'ZAF_CORE' TYPE 'E' NUMBER '011'
                INTO lv_text
                WITH iv_vkorg iv_docid.

        CONCATENATE '>>> GET_FORM_ID: ' lv_text INTO lv_text.

        zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                       iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                       iv_ext_ident = lv_text ).

      ELSE.
        " Formular ID ermittelt: &.
        " Form ID determined: &.
        MESSAGE ID 'ZAF_CORE' TYPE 'I' NUMBER '012'
                INTO lv_text
                WITH ev_form_id.

        CONCATENATE '>>> GET_FORM_ID: ' lv_text INTO lv_text.

        zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                       iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                       iv_ext_ident = lv_text ).

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_key_values_from_zdev_prep.
************************************************************************
*&  Key           : TS-180709 / MB-180709
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Ermittlung der Key-Value Paare aus der Tabelle ZDEV_PREP
*&  HINWEIS: Grundlage ist FuBa Z_DEV_PREP_TXT
*&
*& Determination of key-value pairs from table ZDEV_PREP
*& NOTE: The basis is FuBa Z_DEV_PREP_TXT
************************************************************************

    DATA: lt_cust      TYPE STANDARD TABLE OF zdev_prep,
          lv_tabname   TYPE tabname,
          lv_fieldname TYPE fieldname,
          lv_value     TYPE text2048.

    FIELD-SYMBOLS: <ls_cust>      TYPE zdev_prep,
                   <ls_key_value> TYPE zaf_s_texts_key_value,
                   <lv_value>     TYPE any.

    "Initialisierung
    "Initialization
    CLEAR: et_key_value[], lt_cust[].

    "Anhand IM_LINK alle relevanten Sätze aus ZDEV_PREP selektieren
     "Use IM_LINK to select all relevant sentences from ZDEV_PREP
    SELECT * FROM zdev_prep INTO TABLE lt_cust
      WHERE zlink = iv_link.

    "EX_IT_KEY_VALUE füllen
    "EX_IT_KEY_VALUE fill
    LOOP AT lt_cust ASSIGNING <ls_cust>.

      APPEND INITIAL LINE TO et_key_value ASSIGNING <ls_key_value>.
      "Platzhalter in EX_IT_KEY_VALUE
       "Placeholders in EX_IT_KEY_VALUE
      "
      "Placeholders in EX_IT_KEY_VALUE
       "Placeholders in EX_IT_KEY_VALUE
      <ls_key_value>-placeholder = <ls_cust>-ztextid.
      "Feldnamen aus ZSAPFIELD ermitteln
      "Determining Field Names from ZSAPFIELD
      CLEAR: lv_tabname, lv_fieldname.
      SPLIT <ls_cust>-zsapfield AT '-' INTO lv_tabname lv_fieldname.
      "Wert aus dem Feld in EX_IT_KEY_VALUE
       "Value from field in EX_IT_KEY_VALUE
      UNASSIGN: <lv_value>.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE is_source_values TO <lv_value>.
      IF <lv_value> IS ASSIGNED.
        <ls_key_value>-value = <lv_value>.
*      "HINWEIS:
*      "evtl. Datum- oder Währungsfeld noch konvertieren
*      "(für Datumskonvertierung evtl.Methode ZCL_ACTEBIS_UTIL_CONVERT=>CONV_DATE_INT_TO_EXT)
*      "bis jetzt nicht notwendig
*      WRITE <l_value> TO l_value. "falls Datum- oder Währungsfeld
*      <ls_key_value>-value = l_value.
*
* "HINWEIS:
* "possibly date or currency field still convert
* "(for date conversion, possibly. Method ZCL_ACTEBIS_UTIL_CONVERT=>CONV_DATE_INT_TO_EXT)
* "bis jetzt nicht bedarf
* WRITE <l_value> TO l_value. "if date or currency field
* <ls_key_value>-value = l_value.
        CONDENSE <ls_key_value>-value."Leerzeichen entfernen / Remove spaces
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_logo.
************************************************************************
*&  Key           : TS-180410
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Logo aus MIME Repository liefern
*&
*& Deliver logo from MIME repository
************************************************************************

    DATA: lo_mime TYPE REF TO if_mr_api,
          lv_url  TYPE skwf_url.

    lo_mime = cl_mime_repository_api=>get_api( ).

    CONCATENATE iv_path iv_logoname INTO lv_url.
    CONDENSE lv_url NO-GAPS.

    lo_mime->get( EXPORTING i_url              = lv_url
                            i_check_authority  = '' "Keine Berechtigungsprüfung / No authorization check
                  IMPORTING "e_is_folder        =
                            e_content          = rv_logo
                            "e_content_last_changed =
                            "e_mime_type        =
                            "e_loio             =
                  "CHANGING c_language         =
                  EXCEPTIONS parameter_missing  = 1
                             error_occured      = 2
                             not_found          = 3
                             permission_failure = 4
                             OTHERS             = 5 ).

    IF sy-subrc <> 0.
      zcx_af_core_output=>raise( iv_log_object = iv_log_object
                                 iv_msgno     = '201'
                                 iv_msgty     = 'E' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_nast_data.
************************************************************************
*&  Key           : JR-180515
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Daten zur Nachrichtensteuerung per Dirty Assign beschaffen.
*&
*& data for message control via dirty assign.
************************************************************************

    FIELD-SYMBOLS: <ls_tnapr> TYPE tnapr,
                   <ls_nast>  TYPE nast.

    ASSIGN ('(RSNAST00)TNAPR') TO <ls_tnapr>.

    IF <ls_tnapr> IS ASSIGNED.
      es_tnapr = <ls_tnapr>.
    ENDIF.

    ASSIGN ('(RSNAST00)NAST') TO <ls_nast>.

    IF <ls_nast> IS ASSIGNED.
      es_nast         = <ls_nast>.
      ev_is_nast_used = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_outputparams_from_nast.
************************************************************************
*&  Key           : JR-180515
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Die Methode konvertiert die NAST in die SFPOUTPUTPARAMS. Darüber
*&  hinaus werden die für die Nachricht notwendigen Kommunikations-
*&  daten aus dem Standard ermittelt.
*&
*& The method converts the NAST into the SFP output params. Over it
*& In addition, the communication necessary for the message
*& data determined from the standard.
************************************************************************

    DATA: lv_text                   TYPE bdc_vtext1,
          ls_addr_key               TYPE addr_key,
          ls_wzre_message           TYPE wzre_message,
          lo_cx_wzre_internal_error TYPE REF TO cx_wzre_internal_error.

    " Logische Prüfung der Importparamter
    " Logical check of import parameters
    IF iv_pernr IS NOT INITIAL.
      IF iv_adrnr IS INITIAL.
        "Kommunikationsdaten können nur für PERNR mit ADRNR ermittelt werden.
        "Communication data can only be obtained for PERNR with ADRNR.
        MESSAGE ID zcl_af_core_constants=>gc_msgid_default TYPE 'E' NUMBER '202'
                INTO lv_text.

        CONCATENATE '>>> GET_OUTPUTPARAMS_FROM_NAST: ' lv_text INTO lv_text.

        zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                       iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                       iv_ext_ident = lv_text ).

      ENDIF.
    ENDIF.

    IF ( iv_adrnr IS NOT INITIAL OR
         iv_pernr IS NOT INITIAL ) AND
      iv_receiver_country IS INITIAL.
      "Zur Ermittlung der Kommunikationsdaten fehlt IM_RECEIVER_COUNTRY.
      "There is no IM_RECEIVER_COUNTRY to determine the communication data.
      MESSAGE ID zcl_af_core_constants=>gc_msgid_default TYPE 'E' NUMBER '203'
              INTO lv_text.

      CONCATENATE '>>> GET_OUTPUTPARAMS_FROM_NAST: ' lv_text INTO lv_text.

      zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = lv_text ).

    ENDIF.


    " Füllen der Adressstruktur
    " Filling the address structure
    IF iv_adrnr IS NOT INITIAL.
      ls_addr_key-addrnumber = iv_adrnr.
      ls_addr_key-addr_type  = '1'.  "Organisation
    ENDIF.

    IF iv_pernr IS NOT INITIAL.
      ls_addr_key-persnumber = iv_pernr.
      ls_addr_key-addr_type  = '2'.  "Person
    ENDIF.


    " Aufruf der SAP Standard Konvertierung
    " Calling SAP Standard Conversion
    TRY.
        cl_wzre_service_nast_print=>get_outputparams( EXPORTING i_nast             = is_nast
                                                                i_addr_key         = ls_addr_key
                                                                i_receiver_country = iv_receiver_country
                                                                "i_xscreen          = ' '
                                                                i_formtype         = '2'   "PDF
                                                      IMPORTING e_outputparams     = es_outputparams
                                                                e_comm_values      = es_comm_values ).

      CATCH cx_wzre_internal_error  INTO lo_cx_wzre_internal_error.
        lo_cx_wzre_internal_error->get_message( IMPORTING e_message = ls_wzre_message ).

        zcx_af_core_output=>raise( EXPORTING iv_log_object = iv_log_object
                                             iv_msgid      = ls_wzre_message-msgid
                                             iv_msgno      = CONV #( ls_wzre_message-msgno )
                                             iv_msgty      = ls_wzre_message-msgty
                                             iv_msgv1      = CONV #( ls_wzre_message-msgv1 )
                                             iv_msgv2      = CONV #( ls_wzre_message-msgv2 )
                                             iv_msgv3      = CONV #( ls_wzre_message-msgv3 )
                                             iv_msgv4      = CONV #( ls_wzre_message-msgv4 ) ).

    ENDTRY.

    " Die Telefaxdaten werden im Standard nur bei Sendemedium 5 externes Senden
    " in die COMM_VALUES gefüllt. Bei Fax haben wir aber Sendemedium 2 Telefax.
    " Daher werden die Informationen hier nachträglich übertragen.
    "
    " The fax data are transmitted in the standard only with transmission medium 5 external transmission
    " in the COMM_VALUES. With fax, however, we have transmission medium 2 fax.
    " Therefore, the information is transferred here subsequently.
    IF es_outputparams-device = zcl_af_core_constants=>gc_sap_device_telefax AND
       es_comm_values-adfax-fax_number IS INITIAL.
      es_comm_values-adfax-fax_number = is_nast-telfx.
      es_comm_values-adfax-country    = is_nast-tland.
    ENDIF.


    " Ausgabestruktur Erweiterung
    " Output Structure Extension
    es_extended_params-kappl   = is_nast-kappl.
    es_extended_params-kschl   = is_nast-kschl.
    es_extended_params-objtype = is_nast-objtype.
    es_extended_params-parvw   = is_nast-parvw.

  ENDMETHOD.


  METHOD get_output_language.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Ausgabesprache ermitteln
*& Determine output language
************************************************************************

    DATA: lt_output_langu TYPE STANDARD TABLE OF zaf_output_langu.

    FIELD-SYMBOLS: <ls_output_langu> TYPE zaf_output_langu.

    SELECT * FROM zaf_output_langu INTO TABLE lt_output_langu
                                        WHERE vkorg = iv_vkorg.
    IF sy-subrc = 0.
      READ TABLE lt_output_langu ASSIGNING <ls_output_langu> WITH KEY langu = iv_comm_langu.
      IF sy-subrc = 0.
        rv_out_langu = <ls_output_langu>-langu.
      ELSE.
        READ TABLE lt_output_langu ASSIGNING <ls_output_langu> WITH KEY def_flag = abap_true.
        IF sy-subrc = 0.
          rv_out_langu = <ls_output_langu>-langu.
        ENDIF.
      ENDIF.
    ELSE.
      rv_out_langu = zcl_af_core_constants=>gc_group_of_companies_language.
    ENDIF.

  ENDMETHOD.


  METHOD get_out_channel.
**********************************************************************
*& Key           : AR-180920
*& Request No.   : 180116-140129-AR - Formularentwicklung Lieferschein
**********************************************************************
*& Description (short)
*& Kanäle zu der Prozess ID und zum Filter ermitteln
*&
*& Determine channels to the process ID and filter
**********************************************************************

    CLEAR: es_out_channel.
    SELECT SINGLE * FROM zaf_out_channel
                    INTO es_out_channel
                   WHERE procid   = iv_procid
                     AND filter   = iv_filter.

    " Log wegschreiben
    " Write away log
    zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                   iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_con
                                   iv_ext_ident = 'Steuertabelle für Kanäle (ZAF_OUT_CHANNEL) gelesen'
                                   io_msg       = zcl_af_core_util=>transfer_struc_val_to_msg_obj( is_structure = es_out_channel ) ).
  ENDMETHOD.


  METHOD get_out_info.
**********************************************************************
*& Key           : TS-180725
*& Request No.   : 180116-135440-AR - Formularentwicklung Rechnung Schweiz
**********************************************************************
*& Description (short)
*& Informationen liefern, die im OUT_INFO Connector abgelegt wurden.
*& provide information stored in the OUT_INFO Connector.
**********************************************************************

    CLEAR: es_out_info.

    SELECT SINGLE *
      INTO es_out_info
      FROM zaf_out_info
      WHERE vkorg  = iv_vkorg
        AND filter = iv_filter
        AND objkey = iv_objkey.

  ENDMETHOD.


  METHOD get_sample_printout_data.
**********************************************************************
*& Key           : AR-190314
*& Request No.   : 180116-140129-AR
**********************************************************************
*& Description (short)
*& SAMPLE PRINTOUT Daten ermitteln / SAMPLE PRINTOUT data
**********************************************************************

    CLEAR: es_sample_printout.

    IF sy-sysid = zcl_af_core_constants=>gc_syid_t_system OR
       sy-sysid = zcl_af_core_constants=>gc_syid_q_system.

      es_sample_printout-title        = 'SAMPLE - PRINTOUT'.
      es_sample_printout-uname_lb     = 'User:'.
      es_sample_printout-uname        = sy-uname.
      es_sample_printout-tcode_lb     = 'Transaction:'.
      es_sample_printout-tcode        = sy-tcode.
      es_sample_printout-form_name_lb = 'Document:'.
      es_sample_printout-form_name    = iv_form_name.
      es_sample_printout-sysid_lb     = 'System:'.
      es_sample_printout-sysid        = sy-sysid.
      es_sample_printout-datum_lb     = 'Printdate:'.
      es_sample_printout-datum        = sy-datum.

    ENDIF.

  ENDMETHOD.


  METHOD get_struct_name.
************************************************************************
*&  Key           : RP-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Name der generischen Struktur beschaffen
*&
*& obtain the name of the generic structure
************************************************************************

    DATA: lo_strucdescr  TYPE REF TO cl_abap_structdescr,
          lo_typedescr   TYPE REF TO cl_abap_typedescr,
          ls_ddic_header TYPE x030l.

    TRY .
        " RTTI Descriptor zur Struktur laden
      " Load RTTI descriptor to structure
        lo_strucdescr ?= cl_abap_typedescr=>describe_by_data( is_struct ).
      CATCH cx_sy_move_cast_error.
        RAISE translation_no_struct.
    ENDTRY.

    " DDIC-Header zur Struktur beschaffen
    " Get DDIC header for structure
    lo_strucdescr->get_ddic_header( RECEIVING  p_header     = ls_ddic_header    " DDIC Header
                                    EXCEPTIONS not_found    = 1
                                               no_ddic_type = 2
                                               OTHERS       = 3 ).

    IF sy-subrc = 0 AND ls_ddic_header IS NOT INITIAL.
      " Strukturnamen zurückgeben
      " Return structure name
      rv_struct_name = ls_ddic_header-tabname.
    ENDIF.
  ENDMETHOD.


  METHOD set_table_data.
    TYPES: BEGIN OF ts_where,
             line TYPE tdline,
           END OF ts_where.

    DATA lt_where TYPE STANDARD TABLE OF ts_where.

    DATA: lr_data       TYPE REF TO data,
          lr_data_ins   TYPE REF TO data,
          lr_data_upd   TYPE REF TO data,
          lr_data_del   TYPE REF TO data,
          lo_tabledescr TYPE REF TO cl_abap_tabledescr,
          lv_commit     TYPE boolean VALUE abap_off,
          lv_rollback   TYPE boolean VALUE abap_off.

    FIELD-SYMBOLS: <lt_data>     TYPE STANDARD TABLE,
                   <lt_data_ins> TYPE STANDARD TABLE,
                   <lt_data_upd> TYPE STANDARD TABLE,
                   <lt_data_del> TYPE STANDARD TABLE,
                   <ls_data>     TYPE data,
                   <lv_value>    TYPE data.

    CREATE: DATA lr_data     TYPE STANDARD TABLE OF (iv_tabname),
            DATA lr_data_ins TYPE STANDARD TABLE OF (iv_tabname),
            DATA lr_data_upd TYPE STANDARD TABLE OF (iv_tabname),
            DATA lr_data_del TYPE STANDARD TABLE OF (iv_tabname).

    ASSIGN: lr_data->*     TO <lt_data>,
            lr_data_ins->* TO <lt_data_ins>,
            lr_data_upd->* TO <lt_data_upd>,
            lr_data_del->* TO <lt_data_del>.

    lo_tabledescr ?= cl_abap_tabledescr=>describe_by_name( iv_tabname ).

    " read translations
    SELECT * FROM (iv_tabname)
      INTO TABLE <lt_data>.

    SORT: <lt_data>.

    " check translations is new or changed
    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data_imp>).
      LOOP AT it_data ASSIGNING <ls_data> WHERE (lt_where).
      ENDLOOP.
    ENDLOOP.
*
*
*
*    READ TABLE lt_data ASSIGNING <ls_data> WITH KEY labelname = <ls_translations>-labelname
*                                                    langu     = <ls_translations>-langu
*                                                    vkorg     = <ls_translations>-vkorg
*                                                    form_name = <ls_translations>-form_name BINARY SEARCH.
*    IF sy-subrc = 0.
*      CHECK <ls_translations>-translation <> <ls_trans>-translation.
*      " translations is changed
*      lt_translations_upd = VALUE #( BASE lt_translations_upd ( labelname   = <ls_translations>-labelname
*                                                                langu       = <ls_translations>-langu
*                                                                vkorg       = <ls_translations>-vkorg
*                                                                form_name   = <ls_translations>-form_name
*                                                                translation = CONV #( <ls_translations>-translation ) ) ).
*    ELSE.
*      " translations is new
*      lt_translations_ins = VALUE #( BASE lt_translations_ins ( labelname   = <ls_translations>-labelname
*                                                                langu       = <ls_translations>-langu
*                                                                vkorg       = <ls_translations>-vkorg
*                                                                form_name   = <ls_translations>-form_name
*                                                                translation = CONV #( <ls_translations>-translation ) ) ).
*    ENDIF.
*  ENDLOOP.
*
*  SORT: it_translations BY labelname langu vkorg form_name.
*
*  " check translations delete
*  LOOP AT lt_translations ASSIGNING FIELD-SYMBOL(<ls_translations2>).
*    READ TABLE it_translations TRANSPORTING NO FIELDS WITH KEY labelname = <ls_translations2>-labelname
*                                                               langu     = <ls_translations2>-langu
*                                                               vkorg     = <ls_translations2>-vkorg
*                                                               form_name = <ls_translations2>-form_name BINARY SEARCH.
*    CHECK sy-subrc <> 0.
*    lt_translations_del = VALUE #( BASE lt_translations_del ( <ls_translations2> ) ).
*  ENDLOOP.
*
*  " change translations
*  IF lines( lt_translations_upd ) > 0.
*    UPDATE zaf_translations FROM TABLE lt_translations_upd.
*    IF sy-subrc = 0.
*      lv_commit = abap_on.
*    ELSE.
*      lv_rollback = abap_on.
*    ENDIF.
*  ENDIF.
*
*  " insert translations
*  IF lv_rollback = abap_off AND lines( lt_translations_ins ) > 0.
*    INSERT zaf_translations FROM TABLE lt_translations_ins ACCEPTING DUPLICATE KEYS.
*    IF sy-subrc = 0.
*      lv_commit = abap_on.
*    ELSE.
*      lv_rollback = abap_on.
*      lv_commit   = abap_off.
*    ENDIF.
*  ENDIF.
*
*  " delete translations
*  IF lv_rollback = abap_off AND lines( lt_translations_del ) > 0.
*    DELETE zaf_translations FROM TABLE lt_translations_del.
*    IF sy-subrc = 0.
*      lv_commit = abap_on.
*    ELSE.
*      lv_rollback = abap_on.
*      lv_commit   = abap_off.
*    ENDIF.
*  ENDIF.
*
*  IF lv_commit = abap_on.
*    COMMIT WORK.
*    ev_return = 0.
*  ELSEIF lv_rollback = abap_on.
*    ROLLBACK WORK.
*    ev_return = 4.
*  ELSE.
*    ev_return = 0.
*  ENDIF.

*}   INSERT
  ENDMETHOD.


  METHOD transfer_struc_val_to_msg_obj.
************************************************************************
*&  Key           : AR-180618
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Strukturwerte mit Namen in eim Meldungsobjekt hinzufügen
*&
*& Add structure values with names in a message object
************************************************************************

    DATA: lo_struct TYPE REF TO cl_abap_structdescr,
          lt_comp   TYPE        cl_abap_structdescr=>component_table,
          lv_text   TYPE        bdc_vtext1.

    TRY.
        lo_struct ?= cl_abap_typedescr=>describe_by_data( is_structure ).
        lt_comp    = lo_struct->get_components( ).

        ro_msg = NEW zcl_af_core_messages( ).

        LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>) .
          ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE is_structure TO FIELD-SYMBOL(<lv_feldvalue>).
          IF <ls_comp>-name IS NOT INITIAL.
            lv_text = |{ <ls_comp>-name } = { <lv_feldvalue> }|.
            ro_msg->add_note( iv_note = lv_text ).
          ENDIF.
        ENDLOOP.

      CATCH cx_root INTO DATA(lo_err).
        DATA(lo_msg) = NEW zcl_af_core_messages( ).
        lv_text = lo_err->get_text( ).
        lo_msg->add_note( iv_note = lv_text iv_msgtyp = 'W' ).
        " Log wegschreiben
        " Write away log
        zcl_af_core_logger=>write_log( iv_object    = zcl_af_core_constants=>gc_logger_object_af
                                       iv_subobject = zcl_af_core_constants=>gc_logger_subobject_error
                                       iv_ext_ident = lv_text
                                       io_msg       = lo_msg ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
