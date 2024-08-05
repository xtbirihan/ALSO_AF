*----------------------------------------------------------------------*
*       CLASS ZCL_AF_CORE_LOGGER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_AF_CORE_LOGGER definition
  public
  create private .

public section.
  type-pools ABAP .

  interfaces IF_MESSAGE .
  interfaces IF_T100_MESSAGE .

  data GS_HEADER type BAL_S_LOG read-only .
  data GV_HANDLE type BALLOGHNDL read-only .
  data GV_DB_NUMBER type BALOGNR read-only .

  class-methods NAST_PROTOCOL_UPDATE
    importing
      !IV_MSGID like SY-MSGID
      !IV_MSGNO like SY-MSGNO
      !IV_MSGTY like SY-MSGTY
      !IV_MSGV1 like SY-MSGV1 optional
      !IV_MSGV2 like SY-MSGV2 optional
      !IV_MSGV3 like SY-MSGV3 optional
      !IV_MSGV4 like SY-MSGV4 optional .
  class-methods GET_INSTANCE
    importing
      !IV_OBJECT type BALOBJ_D default ZCL_AF_CORE_CONSTANTS=>GC_LOGGER_OBJECT_AF
      !IV_SUBOBJECT type BALSUBOBJ
      !IV_DESC type BALNREXT optional
      !IV_AUTO_SAVE type ABAP_BOOL default ABAP_FALSE
      !IV_UPDATE_NAST_PROTOCOL type ABAP_BOOL default ABAP_FALSE
      !IV_LOGLEVEL type LOGLEVEL optional
    returning
      value(RO_LOGGER) type ref to ZCL_AF_CORE_LOGGER .
  methods ADD_LOG
    importing
      !IV_OBJ_TO_LOG type ANY optional
      !IV_TYPE type BALTMSGTY optional
      !IV_IMPORTANCE type BALPROBCL optional
    preferred parameter IV_OBJ_TO_LOG
    returning
      value(RO_LOGGER) type ref to ZCL_AF_CORE_LOGGER
    raising
      ZCX_AF_CORE_OUTPUT .
  methods DISPLAY_LOG_DATA_MODAL .
  methods DISPLAY_LOG_DATA_FULLSCREEN .
  methods EXPORT_TO_TABLE
    returning
      value(RT_BAPIRET) type BAPIRETTAB .
  methods GET_AUTOSAVE
    returning
      value(RV_AUTO_SAVE) type ABAP_BOOL .
  methods SET_AUTOSAVE
    importing
      !IV_AUTO_SAVE type ABAP_BOOL .
  methods SAVE_LOG .
  class-methods WRITE_LOG
    importing
      !IV_OBJECT type BALOBJ_D
      !IV_SUBOBJECT type BALSUBOBJ optional
      !IV_EXT_IDENT type BALNREXT
      !IO_MSG type ref to ZCL_AF_CORE_MESSAGES optional .
  class-methods WRITE_LOG_OOP
    importing
      !IV_OBJECT type BALOBJ_D
      !IV_SUBOBJECT type BALSUBOBJ
      !IV_EXT_IDENT type BALNREXT
      !IO_MSG type ref to ZCL_AF_CORE_MESSAGES optional .
  PROTECTED SECTION.
*"* protected components of class ZCL_AF_CORE_LOGGER
*"* do not include other source files here!!!
private section.

  types:
* Local type for hrpad_message as it is not available in an ABAP Development System
    BEGIN OF hrpad_message_field_list_alike,
             scrrprfd TYPE scrrprfd.
    TYPES: END OF hrpad_message_field_list_alike .
  types:
    BEGIN OF ts_hrpad_message_alike,
             cause(32)    TYPE c,              "original: hrpad_message_cause
             detail_level TYPE ballevel.
        INCLUDE TYPE symsg .
    TYPES: field_list TYPE STANDARD TABLE OF hrpad_message_field_list_alike
           WITH NON-UNIQUE KEY scrrprfd.
    TYPES: END OF ts_hrpad_message_alike .

  constants GC_GENERIC_MSGNO type SYST_MSGNO value '999' ##NO_TEXT.
  data GV_LOGLEVEL type LOGLEVEL .
  data GV_UPDATE_NAST_PROTOCOL type ABAP_BOOL .
  data GV_AUTO_SAVE type ABAP_BOOL .
  data GV_SEC_CONNECTION type ABAP_BOOL .
  data GV_SEC_CONNECT_COMMIT type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_AF_CORE_LOGGER IMPLEMENTATION.


  METHOD add_log.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Log Eintrag hinzufügen. Folgende Objekte dürfen in iv_OBJ_TO_LOG
*&  mitgegeben werden:
*&    1. INITIAL:
*&       In diesem Fall wird aus den SY-Feldern eine Nachricht erzeugt,
*&       ( per SY-MSGID usw.).
*&    2. Zeichenartige Variable: String, C, alles mögliche
*&    3. Exception-Instanz:
*&       Eine Instanz zu einer beliebigen Exception Klasse.
*&       Hinweis: Exception-Instanzen werden nicht in das NAST Protokoll
*&       geschrieben.
*&    4. Tabelle:
*&       Eine Tabelle mit einer Spalte von Objekten, die per
*&       iv_OBJ_TO_LOG verarbeitet werden können.
*&    5. Struktur BAPIRET2: BAPI Message Struktur
*&    6. Struktur BDCMSGCOLL: Batch Message Struktur
*&    7. Struktur HRPAD_MESSAGE: HR Stammdaten Meldung
*&
*& Add log entry. The following objects may be used in iv_OBJ_TO_LOG
*& are given:
*& 1st INITIAL:
*& In this case, a message is generated from the SY fields,
*& ( via SY-MSGID etc.).
*& 2. Character-like variable: string, C, all sorts of things
*& 3rd exception instance:
*& An instance to any exception class.
*& Note: Exception instances are not included in the NAST log
*& written.
*& 4th table:
*& A table with a column of objects defined by
*& iv_OBJ_TO_LOG can be processed.
*& 5. Structure BAPIRET2: BAPI Message Structure
*& 6. Structure BDCMSGCOLL: Batch Message Structure
*& 7. Structure HRPAD_MESSAGE: HR Master Data Reporting
************************************************************************

    DATA: lv_free_text_msg     TYPE char200,
          ls_detailed_msg      TYPE bal_s_msg,
          lo_ctx_type          TYPE REF TO cl_abap_typedescr,
          ls_ctx_ddic_header   TYPE x030l,
          lo_msg_type          TYPE REF TO cl_abap_typedescr,
          lo_msg_table_type    TYPE REF TO cl_abap_tabledescr,
          ls_exception_data    TYPE bal_s_exc,
          lt_log_numbers       TYPE bal_t_lgnm,
          lt_log_handles       TYPE bal_t_logh,
          ls_log_number        TYPE bal_s_lgnm,
          ls_formatted_context TYPE bal_s_cont,
          ls_formatted_params  TYPE bal_s_parm.

    FIELD-SYMBOLS: <lt_table_of_messages> TYPE ANY TABLE,
                   <ls_message_line>      TYPE any,
                   <ls_bapi_msg>          TYPE bapiret2,
                   <ls_bdc_msg>           TYPE bdcmsgcoll,
                   <ls_hrpad_msg>         TYPE ts_hrpad_message_alike,
                   <ls_zmsg>              TYPE zcl_af_core_messages=>ts_msg,
                   <lv_context_val>       TYPE any.

**********************************************************************
* Regelwerk Logging:
* Type 'E'=Error:     Level 1
* Type 'A'=Abbruch:   Level 1
* Type 'W'=Warnung:   Level 1-2
* Type 'I'=Info:      Level 1-3
* Type 'S'=Success:   Level 1-4
*
* Regulations Logging:
* Type 'E'=Error: Level 1
* Type 'A'=Abort: Level 1
* Type 'W'=Warning: Level 1-2
* Type 'I'=Info: Level 1-3
* Type 'S'=Success: Level 1-4
**********************************************************************
    CHECK iv_type <> 'W' OR gv_loglevel > 1.
    CHECK iv_type <> 'I' OR gv_loglevel > 2.
    CHECK iv_type <> 'S' OR gv_loglevel > 3.

    " Per RTTI den Typ von iv_obj_to_log bestimmen
    " Using RTTI to determine the type of iv_obj_to_log
    lo_msg_type = cl_abap_typedescr=>describe_by_data( iv_obj_to_log ).

    IF iv_obj_to_log IS INITIAL.
      " Wenn nichts übergeben wurde, letzte Message aus SY ziehen
      " If nothing has been passed, pull the last message from SY
      ls_detailed_msg-msgty = sy-msgty.
      ls_detailed_msg-msgid = sy-msgid.
      ls_detailed_msg-msgno = sy-msgno.
      ls_detailed_msg-msgv1 = sy-msgv1.
      ls_detailed_msg-msgv2 = sy-msgv2.
      ls_detailed_msg-msgv3 = sy-msgv3.
      ls_detailed_msg-msgv4 = sy-msgv4.
    ELSEIF lo_msg_type->type_kind = cl_abap_typedescr=>typekind_oref.
      " Wenn es eine Objektreferenz ist, annehmen, dass es eine Exception Instanz war
      " If it is an object reference, assume that it was an Exception instance
      ls_exception_data-exception = iv_obj_to_log.
      ls_exception_data-msgty = iv_type.
      ls_exception_data-probclass = iv_importance.
    ELSEIF lo_msg_type->type_kind = cl_abap_typedescr=>typekind_table.
      " Wenn es eine Tabelle ist, annehmen, dass diese nur eine Spalte hat und jede
      " Zeile einzeln (rekursiv) verarbeiten
      " If it is a table, assume that it has only one column and each
      " Process row individually (recursively)
      ASSIGN iv_obj_to_log TO <lt_table_of_messages>.
      LOOP AT <lt_table_of_messages> ASSIGNING <ls_message_line>.
        add_log( <ls_message_line> ).
      ENDLOOP.
      RETURN.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPIRET2'.
      " BAPIRET2 Message
      ASSIGN iv_obj_to_log TO <ls_bapi_msg>.
      ls_detailed_msg-msgty = <ls_bapi_msg>-type.
      ls_detailed_msg-msgid = <ls_bapi_msg>-id.
      ls_detailed_msg-msgno = <ls_bapi_msg>-number.
      ls_detailed_msg-msgv1 = <ls_bapi_msg>-message_v1.
      ls_detailed_msg-msgv2 = <ls_bapi_msg>-message_v2.
      ls_detailed_msg-msgv3 = <ls_bapi_msg>-message_v3.
      ls_detailed_msg-msgv4 = <ls_bapi_msg>-message_v4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BDCMSGCOLL'.
      " Batch Message
      ASSIGN iv_obj_to_log TO <ls_bdc_msg>.
      ls_detailed_msg-msgty = <ls_bdc_msg>-msgtyp.
      ls_detailed_msg-msgid = <ls_bdc_msg>-msgid.
      ls_detailed_msg-msgno = <ls_bdc_msg>-msgnr.
      ls_detailed_msg-msgv1 = <ls_bdc_msg>-msgv1.
      ls_detailed_msg-msgv2 = <ls_bdc_msg>-msgv2.
      ls_detailed_msg-msgv3 = <ls_bdc_msg>-msgv3.
      ls_detailed_msg-msgv4 = <ls_bdc_msg>-msgv4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=HRPAD_MESSAGE'.
      " HR Stammdaten Message
      " HR Master Data Message
      ASSIGN iv_obj_to_log TO <ls_hrpad_msg>.
      ls_detailed_msg-msgty = <ls_hrpad_msg>-msgty.
      ls_detailed_msg-msgid = <ls_hrpad_msg>-msgid.
      ls_detailed_msg-msgno = <ls_hrpad_msg>-msgno.
      ls_detailed_msg-msgv1 = <ls_hrpad_msg>-msgv1.
      ls_detailed_msg-msgv2 = <ls_hrpad_msg>-msgv2.
      ls_detailed_msg-msgv3 = <ls_hrpad_msg>-msgv3.
      ls_detailed_msg-msgv4 = <ls_hrpad_msg>-msgv4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=ZMSG'.
      " Eigene Message-Struktur
      " Own Message Structure
      ASSIGN iv_obj_to_log TO <ls_zmsg>.
      ls_detailed_msg-msgty = <ls_zmsg>-msgtyp.
      IF <ls_zmsg>-msgid IS INITIAL.
        ls_detailed_msg-msgid = 'ZAF_CORE'.
      ELSE.
        ls_detailed_msg-msgid = <ls_zmsg>-msgid.
      ENDIF.

      ls_detailed_msg-msgno = <ls_zmsg>-msgnr.
      ls_detailed_msg-msgv1 = <ls_zmsg>-msgv1.
      ls_detailed_msg-msgv2 = <ls_zmsg>-msgv2.
      ls_detailed_msg-msgv3 = <ls_zmsg>-msgv3.
      ls_detailed_msg-msgv4 = <ls_zmsg>-msgv4.
    ELSE.
      " Freitext
      " Free text
      lv_free_text_msg = iv_obj_to_log.
    ENDIF.

    " Freitext loggen, falls gefüllt
    " Log free text, if filled
    IF lv_free_text_msg IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_log_handle = gv_handle
          i_msgty      = iv_type
          i_probclass  = iv_importance
          i_text       = lv_free_text_msg
          i_s_context  = ls_formatted_context
          i_s_params   = ls_formatted_params.

      IF gv_update_nast_protocol = abap_true.
        " Nachrichtenprotokoll updaten
        " Update message log
        nast_protocol_update( iv_msgid = zcl_af_core_constants=>gc_msgid_default
                              iv_msgno = zcl_af_core_constants=>gc_msgno_generic
                              iv_msgty = ls_detailed_msg-msgty
                              iv_msgv1 = CONV syst_msgv( lv_free_text_msg ) ).
      ENDIF.
    ELSEIF ls_exception_data IS NOT INITIAL.
      " Wenn nicht, Exception Klasse loggen, falls gefüllt
      " If not, log exception class, if filled
      CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
        EXPORTING
          i_log_handle = gv_handle
          i_s_exc      = ls_exception_data.

      IF gv_update_nast_protocol = abap_true.
        " Exception-Nachricht als Textzeile rausholen
        " Get the exception message out as a line of text
        ls_exception_data-exception->get_text( RECEIVING result = lv_free_text_msg ).

        " Nachrichtenprotokoll updaten
        " Update message log
        nast_protocol_update( iv_msgid = zcl_af_core_constants=>gc_msgid_default
                              iv_msgno = zcl_af_core_constants=>gc_msgno_generic
                              iv_msgty = ls_exception_data-msgty
                              iv_msgv1 = CONV syst_msgv( lv_free_text_msg ) ).
      ENDIF.
    ELSEIF ls_detailed_msg IS NOT INITIAL.
      " Wenn nicht, Message loggen, falls gefüllt
      " If not, message log, if filled
      ls_detailed_msg-context = ls_formatted_context.
      ls_detailed_msg-params = ls_formatted_params.
      ls_detailed_msg-probclass = iv_importance.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = gv_handle
          i_s_msg          = ls_detailed_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.

      IF gv_update_nast_protocol = abap_true.
        " Nachrichtenprotokoll updaten
        " Update message log
        nast_protocol_update( iv_msgid = ls_detailed_msg-msgid
                              iv_msgno = ls_detailed_msg-msgno
                              iv_msgty = ls_detailed_msg-msgty
                              iv_msgv1 = ls_detailed_msg-msgv1
                              iv_msgv2 = ls_detailed_msg-msgv2
                              iv_msgv3 = ls_detailed_msg-msgv3
                              iv_msgv4 = ls_detailed_msg-msgv4 ).
      ENDIF.
    ENDIF.

    " Wenn Autosave gesetzt ist, jedes mal in den BAL speichern.
    " ACHTUNG: Sehr Performance kritisch!
    " If Autosave is set, save to BAL each time.
    " ATTENTION: Very performance critical!
    IF gv_auto_save = abap_true.
      save_log( ).
    ENDIF.

    ro_logger = me.
  ENDMETHOD.


  METHOD display_log_data_fullscreen.
************************************************************************
*&  Key           : JR-180424
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Anzeige der Loginformationen im Vollbild Dialog
*&  Display of log information in fullscreen dialog
************************************************************************

    DATA: ls_profile     TYPE bal_s_prof,
          lt_log_handles TYPE bal_t_logh.

    APPEND gv_handle TO lt_log_handles.

    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = ls_profile.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = ls_profile
        i_t_log_handle      = lt_log_handles.

  ENDMETHOD.


  METHOD display_log_data_modal.
************************************************************************
*&  Key           : JR-180424
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Anzeige der Loginformationen im modalen Dialog
*&
*&  Display of log information in modal dialog
************************************************************************

    DATA: ls_profile     TYPE bal_s_prof,
          lt_log_handles TYPE bal_t_logh.

    APPEND gv_handle TO lt_log_handles.

    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_l_wa_profile = ls_profile.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_l_wa_profile = ls_profile
        i_t_log_handle           = lt_log_handles.

  ENDMETHOD.


  METHOD export_to_table.
************************************************************************
*&  Key           : JR-180424
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Loginformationen in BAPIRETTAB exportieren
*&  Export log information to BAPIRETTAB
************************************************************************

    DATA: lt_log_handle         TYPE bal_t_logh,
          lt_ls_message_handles TYPE bal_t_msgh,
          ls_message            TYPE bal_s_msg,
          ls_bapiret            TYPE bapiret2.

    FIELD-SYMBOLS <ls_msg_handle> TYPE balmsghndl.

    INSERT gv_handle INTO TABLE lt_log_handle.

    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_l_it_log_handle = lt_log_handle
      IMPORTING
        e_t_msg_handle      = lt_ls_message_handles
      EXCEPTIONS
        msg_not_found       = 0.

    LOOP AT lt_ls_message_handles ASSIGNING <ls_msg_handle>.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <ls_msg_handle>
        IMPORTING
          e_s_msg        = ls_message
        EXCEPTIONS
          OTHERS         = 3.
      IF sy-subrc IS INITIAL.
        MESSAGE ID ls_message-msgid
                TYPE ls_message-msgty
                NUMBER ls_message-msgno
                INTO ls_bapiret-message
                WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.

        ls_bapiret-type          = ls_message-msgty.
        ls_bapiret-id            = ls_message-msgid.
        ls_bapiret-number        = ls_message-msgno.
        ls_bapiret-log_no        = <ls_msg_handle>-log_handle. "last 2 chars missing!!
        ls_bapiret-log_msg_no    = <ls_msg_handle>-msgnumber.
        ls_bapiret-message_v1    = ls_message-msgv1.
        ls_bapiret-message_v2    = ls_message-msgv2.
        ls_bapiret-message_v3    = ls_message-msgv3.
        ls_bapiret-message_v4    = ls_message-msgv4.
        ls_bapiret-system        = sy-sysid.
        APPEND ls_bapiret TO rt_bapiret.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_autosave.
************************************************************************
*&  Key           : JR-180424
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Autosicherung Flag holen
*&  Get Auto Fuse Flag
************************************************************************

    rv_auto_save = gv_auto_save.

  ENDMETHOD.


  METHOD get_instance.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Instanzerzeugung für Logger-Objekt
*&
*&  get instance
************************************************************************
    FIELD-SYMBOLS <lv_context_val> TYPE c.
    DATA lv_loglevel TYPE fpadstrl.

    " ADS Tracelevel aus Memory ziehen
    IF iv_loglevel IS INITIAL.
      GET PARAMETER ID 'FPTRACELEVEL' FIELD lv_loglevel.
    ELSE.
      lv_loglevel = CONV #( iv_loglevel ).
    ENDIF.

    ro_logger = NEW zcl_af_core_logger( ).

    ro_logger->gs_header-object = iv_object.
    ro_logger->gs_header-subobject = iv_subobject.
    ro_logger->gs_header-extnumber = iv_desc.
    ro_logger->gv_auto_save = iv_auto_save.
    ro_logger->gv_loglevel = CONV #( lv_loglevel ).

    IF iv_update_nast_protocol IS NOT SUPPLIED.
      zcl_af_core_util=>get_nast_data( IMPORTING ev_is_nast_used = ro_logger->gv_update_nast_protocol ).    " 'X' = NAST wird verwendet
    ELSE.
      ro_logger->gv_update_nast_protocol = iv_update_nast_protocol.
    ENDIF.

    " Neue Connection zum BAL LOG erstellen
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = ro_logger->gs_header
      IMPORTING
        e_log_handle = ro_logger->gv_handle.

    " Kopf-Informationen zum neuen BAL LOG lesen
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = ro_logger->gv_handle
      IMPORTING
        e_s_log      = ro_logger->gs_header.

  ENDMETHOD.


  method IF_MESSAGE~GET_LONGTEXT.
  endmethod.


  method IF_MESSAGE~GET_TEXT.
  endmethod.


  METHOD nast_protocol_update.
************************************************************************
*&  Key           : JR-180424
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Übergebene Message Parameter ins NAST Protokoll speichern.
*&  Der Baustein funktioniert nur, wenn der Logger im Kontext
*&  der Nachrichtensteuerung aufgerufen wurde (irgendwo im Stack).
*&
*& Save passed message parameters into the NAST protocol.
*& The block only works if the logger is in the context of
*& of the message control (somewhere in the stack).
************************************************************************

    DATA: lv_text   TYPE bdc_vtext1.

    CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
      EXPORTING
        msg_arbgb              = iv_msgid
        msg_nr                 = iv_msgno
        msg_ty                 = iv_msgty
        msg_v1                 = iv_msgv1
        msg_v2                 = iv_msgv2
        msg_v3                 = iv_msgv3
        msg_v4                 = iv_msgv4
      EXCEPTIONS
        message_type_not_valid = 1
        no_sy_message          = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.

      MESSAGE ID zcl_af_core_constants=>gc_msgid_default
            TYPE zcl_af_core_constants=>gc_logger_typ_w
          NUMBER '001'
            INTO lv_text.

      CONCATENATE '>>> NAST_PROTOCOL_UPDATE: ' lv_text INTO lv_text.

      zcl_af_core_logger=>write_log( iv_object    = zcl_af_core_constants=>gc_logger_object_af
                                     iv_subobject = zcl_af_core_constants=>gc_logger_subobject_error
                                     iv_ext_ident = lv_text ).

    ENDIF.

  ENDMETHOD.


  METHOD save_log.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Löst den Speichervorgang für den BAL LOG Handle aus der Instanz aus.
*&  Triggers the save operation for the BAL LOG handle from the instance.
************************************************************************

    DATA: lt_handles TYPE bal_t_logh,
          lt_numbers TYPE bal_t_lgnm,
          ls_number  TYPE bal_s_lgnm.

    CHECK gv_auto_save = abap_false.

    APPEND gv_handle TO lt_handles.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle       = lt_handles
        i_2th_connection     = gv_sec_connection
        i_2th_connect_commit = gv_sec_connect_commit
      IMPORTING
        e_new_lognumbers     = lt_numbers.

    IF gv_db_number IS INITIAL.
      READ TABLE lt_numbers INDEX 1 INTO ls_number.
      gv_db_number = ls_number-lognumber.
    ENDIF.

  ENDMETHOD.


  METHOD set_autosave.
************************************************************************
*&  Key           : JR-180424
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Autosicherung Flag setzen
*&  Auto Safety Flag Set Flag
************************************************************************

    gv_auto_save = iv_auto_save.

  ENDMETHOD.


  METHOD write_log.
************************************************************************
*&  Key           : AR-180614
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Legt ein neues Objekt im Application Log an, fügt ggf.
*&  Nachricht hinzu und speichert sie ab.
*&  Creates a new object in the application log, adds if necessary.
*&  message and saves it.
************************************************************************

    DATA: lt_msg_tab      TYPE zcl_af_core_messages=>tt_msg,
          lv_loglevel     TYPE loglevel,
          ls_detailed_msg TYPE bal_s_msg,
          lt_handles      TYPE bal_t_logh,
          lv_error_text   TYPE balnrext.

    TRANSLATE sy-uname TO UPPER CASE.

    " Loglevel aus Z-Tabelle ziehen. Wenn kein Eintrag, dann loglevel = 0.
    SELECT SINGLE loglevel FROM zaf_logging INTO lv_loglevel WHERE uname = sy-uname.

    CHECK lv_loglevel > 0.

    " Objektinstanz erzeugen
    DATA(ro_logger) = NEW zcl_af_core_logger( ).
    ro_logger->gs_header-object    = iv_object.
    ro_logger->gs_header-subobject = iv_subobject.
    ro_logger->gs_header-extnumber = iv_ext_ident.
    ro_logger->gv_auto_save        = abap_true.
    ro_logger->gv_loglevel         = lv_loglevel.

    " Neue Connection zum BAL LOG erstellen
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = ro_logger->gs_header
      IMPORTING
        e_log_handle = ro_logger->gv_handle.

    " Kopf-Informationen zum neuen BAL LOG lesen
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = ro_logger->gv_handle
      IMPORTING
        e_s_log      = ro_logger->gs_header.

    " Eigene Message-Struktur aufbauen, wenn Nachrichteninformationen übergeben
    IF io_msg IS BOUND.
      " Meldungstab. ermitteln
      io_msg->get_it_msg_tab(
        RECEIVING
          rt_msg_tab = lt_msg_tab ).
    ENDIF.

    " Meldungen aus dem Meldungsobjekt in den Log schreiben
    LOOP AT lt_msg_tab ASSIGNING FIELD-SYMBOL(<ls_msg>).
      CLEAR: ls_detailed_msg.

      ls_detailed_msg-msgty = <ls_msg>-msgtyp.
      ls_detailed_msg-msgid = <ls_msg>-msgid.
      ls_detailed_msg-msgno = <ls_msg>-msgnr.
      ls_detailed_msg-msgv1 = <ls_msg>-msgv1.
      ls_detailed_msg-msgv2 = <ls_msg>-msgv2.
      ls_detailed_msg-msgv3 = <ls_msg>-msgv3.
      ls_detailed_msg-msgv4 = <ls_msg>-msgv4.

      " Lognachricht in Objekt speichern
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = ro_logger->gv_handle
          i_s_msg          = ls_detailed_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.

      IF sy-subrc <> 0.

        CASE sy-subrc.
          WHEN 1.
            lv_error_text = 'Log nicht gefunden!'.
          WHEN 2.
            lv_error_text = 'Meldung inkonsistent!'.
          WHEN 3.
            lv_error_text = 'Log ist voll!'.
          WHEN OTHERS.
            lv_error_text = 'Im Log ist ein Unbekannter Fehler aufgetreten!'.
        ENDCASE.

        zcl_af_core_logger=>write_log( iv_object    = zcl_af_core_constants=>gc_logger_object_af
                                       iv_subobject = zcl_af_core_constants=>gc_logger_subobject_error
                                       iv_ext_ident = lv_error_text ).

      ENDIF.

      IF  <ls_msg>-msgtyp = zcl_af_core_constants=>gc_logger_typ_e.
        " Nachrichtenprotokoll updaten
        zcl_af_core_logger=>nast_protocol_update( iv_msgid = <ls_msg>-msgid
                                                  iv_msgno = CONV #( <ls_msg>-msgnr )
                                                  iv_msgty = <ls_msg>-msgtyp
                                                  iv_msgv1 = CONV #( <ls_msg>-msgv1 )
                                                  iv_msgv2 = CONV #( <ls_msg>-msgv2 )
                                                  iv_msgv3 = CONV #( <ls_msg>-msgv3 )
                                                  iv_msgv4 = CONV #( <ls_msg>-msgv4 ) ).
      ENDIF.

    ENDLOOP.

    " Logobjekt abspeichern
    APPEND ro_logger->gv_handle TO lt_handles.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle       = lt_handles
        i_2th_connection     = abap_true
        i_2th_connect_commit = abap_true.

  ENDMETHOD.


  METHOD write_log_oop.
************************************************************************
*&  Key           : AR-180614
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Legt ein neues Objekt im Application Log an, fügt ggf.
*&  Nachricht hinzu und speichert sie ab.
*&  Creates a new object in the application log, adds if necessary.
*&  message and saves it.
************************************************************************

    DATA: lv_message    TYPE bapi_msg,
          lt_msg_tab    TYPE zcl_af_core_messages=>tt_msg,
          lv_loglevel   TYPE loglevel,
          lv_error_text TYPE balnrext.

    " Loglevel aus Z-Tabelle ziehen. Wenn kein Eintrag, dann loglevel = 0.
    " Pull log level from Z table. If no entry, then loglevel = 0.
    SELECT SINGLE loglevel FROM zaf_logging INTO lv_loglevel WHERE uname = sy-uname.

    CHECK lv_loglevel > 0.

    TRY.
        DATA(lo_logger) = NEW cl_bal_logobj( i_log_object        = iv_object
                                             i_default_subobject = iv_subobject
                                             i_debug_subobject   = iv_subobject
                                            "i_reorg_in_days     = '3'
                                             i_extnumber         = iv_ext_ident
                                             "i_loghandle         =
                                            ).

        " Eigene Message-Struktur aufbauen, wenn Nachrichteninformationen übergeben
        " Build your own message structure when transferring message information
        IF io_msg IS BOUND.
          io_msg->get_it_msg_tab( RECEIVING rt_msg_tab = lt_msg_tab ).
        ELSE.
          lo_logger->add_errortext( i_errortext = CONV #( iv_ext_ident )
                                    i_detlevel  = CONV #( lv_loglevel ) ).
        ENDIF.

        LOOP AT lt_msg_tab ASSIGNING FIELD-SYMBOL(<ls_msg>).

          MESSAGE ID <ls_msg>-msgid TYPE <ls_msg>-msgtyp NUMBER <ls_msg>-msgnr
          WITH <ls_msg>-msgv1 <ls_msg>-msgv2 <ls_msg>-msgv3 <ls_msg>-msgv4
          INTO lv_message.

          lo_logger->add_errortext( i_errortext = CONV #( lv_message )
                                    i_detlevel  = CONV #( lv_loglevel ) ).

        ENDLOOP.

        lo_logger->save( EXPORTING
                         "i_client             =
                         "i_in_update_task     =
                         i_save_all           = 'X'
                         i_2th_connection     = abap_true
                         i_2th_connect_commit = abap_true
                         "i_link2job           = 'X'
                         "IMPORTING
                         "et_lognumbers        =
                        ).

      CATCH cx_bal_exception .

        zcl_af_core_logger=>write_log( iv_object    = zcl_af_core_constants=>gc_logger_object_af
                                       iv_subobject = zcl_af_core_constants=>gc_logger_subobject_error
                                       iv_ext_ident = lv_error_text ).

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
