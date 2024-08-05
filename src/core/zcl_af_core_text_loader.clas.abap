class ZCL_AF_CORE_TEXT_LOADER definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_LOG_OBJECT type BALOBJ_D .
  class-methods GET_INSTANCE
    importing
      !IV_LOG_OBJECT type BALOBJ_D
    returning
      value(RO_TEXT_LOADER) type ref to ZCL_AF_CORE_TEXT_LOADER .
  methods ADD_TEXT
    importing
      !IS_TEXT_KEY type ZAF_S_TEXT_KEY
      !IS_TEXT_PARAMS type ZAF_S_TEXT_PARAMS
      !IT_KEY_VALUE type ZAF_T_TEXTS_KEY_VALUE optional
      !IV_PLACEHOLDER_STARTKEY type KENNZ1 default '<'
      !IV_PLACEHOLDER_ENDKEY type KENNZ1 default '>'
      !IV_EMPTY_LINES_AFTER_TEXT type EMPTYLINES default 1
    raising
      ZCX_AF_CORE_OUTPUT .
  methods GET_TEXT_TABLE
    returning
      value(RT_TEXT) type ZAF_T_TEXT .
  methods CHANGE_TEXT
    importing
      !IT_KEY_VALUE type ZAF_T_TEXTS_KEY_VALUE
      !IV_PLACEHOLDER_STARTKEY type CHAR1 default '<'
      !IV_PLACEHOLDER_ENDKEY type CHAR1 default '>'
    changing
      !CT_TEXTLINES type TSFTEXT .
  methods DELETE_EMPTY_LINES
    changing
      !CT_TEXTLINES type TSFTEXT .
  methods CHANGE_UNUSED_VARIABLES
    changing
      !CT_TEXTLINES type TSFTEXT .
  methods DELETE_CRLF
    changing
      !CT_TEXTLINES type TSFTEXT .
  methods SET_TEXT_TABLE
    importing
      value(IT_TEXT) type ZAF_T_TEXT .
protected section.
private section.

  data GT_TEXT type ZAF_T_TEXT .
  data GV_LOG_OBJECT type BALOBJ_D .

  methods READ_TEXT
    importing
      !IS_TEXT_KEY type ZAF_S_TEXT_KEY
    returning
      value(RT_TEXTLINES) type TSFTEXT .
  methods PREPARE_PLACEHOLDER
    importing
      !IT_KEY_VALUE type ZAF_T_TEXTS_KEY_VALUE
      !IV_PLACEHOLDER_STARTKEY type CHAR1
      !IV_PLACEHOLDER_ENDKEY type CHAR1
    returning
      value(RT_KEY_VALUE) type ZAF_T_TEXTS_KEY_VALUE .
  methods DELETE_EMPTY_LINES_PER_BLOCK .
  methods MERGE_SAME_BLOCKS .
ENDCLASS.



CLASS ZCL_AF_CORE_TEXT_LOADER IMPLEMENTATION.


  METHOD add_text.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  SO10 Text an Instanztabelle hinzufügen.
*&  Es besteht die Möglichkeit, über eine alternative Sprache erneut zu
*&  lesen, wenn der Text in der angeforderten Sprache nicht vorhanden ist.
*&  Darüber hinaus besteht die Möglichkeit, den Text in der ersten ange-
*&  legten Sprache zu lesen (Feld is_SO10_KEY-TDSPRAS leer lassen).
*&
*&  Über IM_IT_KEY_VALUE können Platzhalter durch Belegwerte ersetzt werden.
*&  Die Start- und Endezeichen zum Erkennen der Platzhalter können als
*&  Parameter vom Aufrufer bestimmt werden, Default ist <...>.
*&
*& SO10 Add text to instance table.
*& It is possible to reread via an alternative language
*& if the text is not available in the requested language.
*& In addition, it is possible to read the text in the first *& language requested (field *&).
*& (leave field is_SO10_KEY-TDSPRAS empty).
*&
*& Via IM_IT_KEY_VALUE placeholders can be replaced by document values.
*& The start and end characters for recognising the placeholders can be specified as
*& parameters by the caller, default is <...>.
************************************************************************

    DATA: lv_text        TYPE bdc_vtext1,
          ls_single_text TYPE zaf_s_text,
          ls_text_key    TYPE stxh_key,
          ls_msg         TYPE zcl_af_core_messages=>ts_msg.

    FIELD-SYMBOLS: <ls_textline>   TYPE LINE OF tsftext.

    " Übergabeparameter müssen für einen eindeutigen Text gepflegt sein
    " Transfer parameters must be maintained for a unique text
    CHECK is_text_key-tdobject IS NOT INITIAL AND
          is_text_key-tdname   IS NOT INITIAL AND
          is_text_key-tdid     IS NOT INITIAL.


    ls_text_key = is_text_key.

    " Wenn die Sprache nicht übergeben wird, wird die Sprache aus dem ersten
    " gefundene Text selektiert.
    "
    " If the language is not passed, the language is selected from the first text found.
    " text found is selected.
    IF is_text_key-tdspras IS INITIAL.
      SELECT SINGLE tdspras FROM stxh
        INTO ls_text_key-tdspras
        WHERE tdobject = ls_text_key-tdobject
          AND tdname   = ls_text_key-tdname
          AND tdid     = ls_text_key-tdid.
    ENDIF.


    DO 2 TIMES.
      IF sy-index = 2.
        " Falls kein Text gefunden ggf. erneut mit Defaultsprache suchen
        " If no text is found, search again with the default language.
        ls_text_key-tdspras = is_text_params-alt_lang.
      ENDIF.

      CHECK ls_text_key-tdspras IS NOT INITIAL.

      "SO10-Text auslesen
      "Read out SO10 text
      ls_single_text-textlines = me->read_text( is_text_key  = ls_text_key ).

      IF lines( ls_single_text-textlines ) > 0.
        EXIT.
      ELSE.
        IF sy-index = 2.

          MESSAGE ID 'ZAF_CORE' TYPE 'I' NUMBER '063'
                  INTO lv_text
                  WITH ls_text_key ls_text_key-tdspras.

          CONCATENATE '>>> ADD_SO10_TEXT: ' lv_text INTO lv_text.

          zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                         iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                         iv_ext_ident = lv_text ).

        ENDIF.
      ENDIF.
    ENDDO.

    " Weiter, wenn ein Text gefunden wurde
    " Continue when a text has been found
    CHECK lines( ls_single_text-textlines ) > 0.

    " Platzhalter austauschen
    " Replace placeholder
    IF lines( it_key_value ) > 0 AND
       lines( ls_single_text-textlines ) > 0.

      change_text( EXPORTING it_key_value            = it_key_value
                             iv_placeholder_startkey = iv_placeholder_startkey
                             iv_placeholder_endkey   = iv_placeholder_endkey
                   CHANGING  ct_textlines            = ls_single_text-textlines ).

    ENDIF.

    " Übergabestruktur befüllen
    " Fill transfer structure
    ls_single_text-key1   = is_text_params-key1.
    ls_single_text-key2   = is_text_params-key2.
    ls_single_text-key3   = is_text_params-key3.
    ls_single_text-key4   = is_text_params-key4.
    ls_single_text-sort   = is_text_params-sort.
    ls_single_text-filter = is_text_params-filter.

    "Im StreamServe wurde insbesondere bei AGB-Texten noch das Zeichen <crlf>
    "in den Texten hinterlegt und später durch CR LF ausgetauscht. Da die SO10
    "Texte nicht geändert werden können, da sie in StreamServe noch benötigt
    "werden, wird an dieser Stelle das Zeichen entfernt bzw. zum Austausch mitgegeben.
    "
    "In StreamServe, the character <crlf> was still used in particular for GTC texts.
    "was still stored in the texts and later replaced by CR LF. Since the SO10
    "texts cannot be changed, as they are still required in StreamServe.
    "the character is removed at this point or given for replacement.
    delete_crlf( CHANGING ct_textlines = ls_single_text-textlines ).

    " Leerzeilen am Ende des Textes entfernen
    "Remove blank lines at the end of the text
    delete_empty_lines( CHANGING ct_textlines = ls_single_text-textlines ).

    " Leerzeilen zur Trennung der Texte am Ende des Textes einfügen
    " Insert blank lines to separate the texts at the end of the text.
    DO iv_empty_lines_after_text TIMES.
      APPEND INITIAL LINE TO ls_single_text-textlines ASSIGNING <ls_textline>.
      <ls_textline>-tdformat = '*'.  "Default-Absatz / "Default paragraph
    ENDDO.

    " Nicht ersetzte Variablen ändern
    " Change variables that have not been replaced
    change_unused_variables( CHANGING ct_textlines = ls_single_text-textlines ).

    " Text an Tabelle anhängen
    " Attach text to table
    APPEND ls_single_text TO Gt_text.

  ENDMETHOD.


  METHOD change_text.
************************************************************************
*&  Key           : TS-180411
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  In den SO10 Texten können Platzhalter eingetragen werden, die zur
*&  Laufzeit in dieser Methode durch Inhalte von Variablen ersetzt werden.
*&  Als Kennzeichen für einen Platzhalter wurde diese Definition verein-
*&  bart: PlatzhalterVariablePlatzhalter z.B. <BELNR>
*&  Es ist zu beachten, dass
*&  - es mehrere Platzhalter in einer Zeile geben kann,
*&  - Platzhalter über das Zeilenende hinaus in zwei Zeilen stehen und
*&  - durch die Ersetzung des Platzhalters der Zeileninhalt nicht mehr
*&    in die Zeile passt. In diesem Fall wird eine neue Zeile eingefügt
*&    und als Fließtext mit dem Steuerzeichen = gekennzeichnet.
*&
*& Placeholders can be entered in the SO10 texts, which are replaced by the
*& are replaced by the contents of variables at runtime in this method.
*& This definition was agreed upon as an indicator for a placeholder.
*& agreed: PlaceholderVariablePlaceholder e.g. <BELNR>.
*& It should be noted that
*& - there can be several placeholders in one line,
*& - placeholders are beyond the end of the line in two lines and
*& - by replacing the placeholder, the line content no longer
*&   fit into the line. In this case, a new line is inserted
*&   and marked as continuous text with the control character.
************************************************************************

    DATA(lo_text_module) = NEW zcl_af_core_text_module( ).

    IF lo_text_module IS BOUND.
      lo_text_module->change_text( EXPORTING it_key_value            = it_key_value
                                             iv_placeholder_startkey = iv_placeholder_startkey
                                             iv_placeholder_endkey   = iv_placeholder_endkey
                                   CHANGING  ct_textlines            = ct_textlines ).
    ENDIF.

  ENDMETHOD.


  METHOD change_unused_variables.
************************************************************************
*&  Key           : TS-180719
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Wenn Variablen in SO10 Texten nicht ersetzt wurden, dann kommt es
*&  zum Abbruch FPUIFB020 bei der Formulargenerierung. Um diesem Abbruch
*&  vorzubeugen, werden die Variablen so verändert, dass sie nicht mehr
*&  als Variablen zu erkennen sind.
*&  &VAR& -> <leer>
*&  <VAR> -> <leer>
*&
*&  Wenn Variablen in SO10 Texten nicht ersetzt wurden, dann kommt es
*&  zum Abbruch FPUIFB020 bei der Formulargenerierung. Um diesem Abbruch
*&  vorzubeugen, werden die Variablen so verändert, dass sie nicht mehr
*&  als Variablen zu erkennen sind.
*&  &VAR& -> <leer>
*&  <VAR> -> <leer>
************************************************************************

    DATA: lt_symbols  TYPE STANDARD TABLE OF itcst,
          lv_variable TYPE string.

    FIELD-SYMBOLS: <ls_symbols> TYPE itcst.


    " Text wird nach Variablen durchsucht
    " Text is searched for variables
    CALL FUNCTION 'TEXT_SYMBOL_COLLECT'
      TABLES
        lines   = ct_textlines
        symbols = lt_symbols.

    " Austausch der gefundenen Variablen
    " Exchange of the variables found
    LOOP AT lt_symbols ASSIGNING <ls_symbols>.

      " 1. Suche: &Variable&
      " 1. search: &variable&
      CONCATENATE '&' <ls_symbols>-name '&' INTO lv_variable.
      REPLACE ALL OCCURRENCES OF lv_variable IN TABLE ct_textlines WITH ' '.

      " 2. Suche: <Variable>
      " 2. search: <variable>
      CONCATENATE '<' <ls_symbols>-name '>' INTO lv_variable.
      REPLACE ALL OCCURRENCES OF lv_variable IN TABLE ct_textlines WITH ' '.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
************************************************************************
*&  Key           : AR-180606
*&  Request No.   : 180116-135917-AR - Formularentwicklung Service Label
************************************************************************
*&  Description (short)
*&  Instanziiert die Textermittlungsklasse
*&
*&  Instantiates the text discovery class
************************************************************************

    gv_log_object = iv_log_object.

*    zcl_af_core_logger=>write_log( iv_object    = iv_log_object
*                                   iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
*                                   iv_ext_ident = '> START - ZCL_AF_CORE_TEXT_LOADER' ).

  ENDMETHOD.


  METHOD delete_crlf.
************************************************************************
*&  Key           : TS-181001
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*& Im StreamServe wurde insbesondere bei AGB-Texten noch das Zeichen <crlf>
*& in den Texten hinterlegt und später durch CR LF ausgetauscht. Da die SO10
*& Texte nicht geändert werden können, da sie in StreamServe noch benötigt
*& werden, wird an dieser Stelle das Zeichen entfernt.
*&
*& In StreamServe, the character <crlf> *& was still stored in the texts, especially in GTC texts.
*& was still stored in the texts and later replaced by CR LF. Since the SO10
*& texts cannot be changed, as they are still required in StreamServe.
*& in StreamServe, the character is removed at this point.
************************************************************************

    DATA: ls_key_value TYPE zaf_s_texts_key_value,
          lt_key_value TYPE STANDARD TABLE OF zaf_s_texts_key_value.

    FIELD-SYMBOLS: <ls_key_value> TYPE zaf_s_texts_key_value.

    APPEND INITIAL LINE TO lt_key_value ASSIGNING <ls_key_value>.
    <ls_key_value>-placeholder = 'CRLF'.

    change_text( EXPORTING it_key_value = lt_key_value
                           "iv_placeholder_startkey = '<'
                           "iv_placeholder_endkey   = '>'
                 CHANGING  ct_textlines = ct_textlines ).


  ENDMETHOD.


  METHOD delete_empty_lines.
************************************************************************
*&  Key           : TS-180711
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Leere Zeilen am Ende des Textes werden entfernt.
*&
*&  Empty lines at the end of the text are removed.
************************************************************************

    DATA: lv_index TYPE sy-index.

    FIELD-SYMBOLS: <ls_textline>   TYPE LINE OF tsftext.

    lv_index = lines( ct_textlines ).

    WHILE lv_index > 0.
      READ TABLE ct_textlines ASSIGNING <ls_textline> INDEX lv_index.
      IF <ls_textline>-tdline IS INITIAL.
        DELETE ct_textlines INDEX lv_index.
      ELSE.
        lv_index = 0.
      ENDIF.
      lv_index = lv_index - 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD delete_empty_lines_per_block.
************************************************************************
*&  Key           : TS-190318
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Bevor die komplette Texttabelle von der speziellen Klasse abgerufen
*&  wird, soll bei gleichem Schlüssel am Ende jeder Gruppe die Leerzeilen
*&  entfernt werden. Das ist notwendig, damit nicht ungewollt ein Überlauf
*&  auf eine neue Seite erfolgt. Im Layout sollen Leerzeilen vor und nach
*&  den jeweiligen Textblöcken selbst bestimmt werden.
*&
*& Before the complete text table is retrieved from the special class.
*& The blank lines at the end of each group with the same key are to be
*& should be removed. This is necessary to avoid an unintentional overflow
*& to a new page. In the layout, blank lines before and after
*& the respective text blocks themselves.
************************************************************************

    FIELD-SYMBOLS: <ls_text>     LIKE LINE OF gt_text,
                   <it_textline> TYPE tsftext.


    SORT gt_text.

    LOOP AT gt_text ASSIGNING <ls_text>.
      ASSIGN <ls_text>-textlines TO <it_textline>.

      AT END OF filter.
        " Nun haben wir den letzten Eintrag eines Blockes und können hier die
        " Leerzeilen am Ende löschen.
        " Now we have the last entry of a block and can
        " delete blank lines at the end.
        delete_empty_lines( CHANGING ct_textlines = <it_textline> ).
      ENDAT.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_instance.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Instanz des Text_Loaders bekommen
*&
*&  Get instance of the Text_Loader
************************************************************************

    ro_text_loader = NEW zcl_af_core_text_loader( iv_log_object = iv_log_object ).

  ENDMETHOD.


  METHOD get_text_table.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Texttabelle ermitteln
*&
*&  Determine text table
************************************************************************

    merge_same_blocks( ).
    delete_empty_lines_per_block( ).

    rt_text = gt_text.

  ENDMETHOD.


  METHOD merge_same_blocks.
************************************************************************
*&  Key           : TS-190514
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Textblöcke mit identischem Schlüssel können zusammengefasst werden.
*&
*&  Text blocks with identical keys can be combined.
************************************************************************

    FIELD-SYMBOLS: <ls_text>       LIKE LINE OF gt_text.

    DATA: lt_text LIKE gt_text,
          ls_text LIKE LINE OF lt_text.

    lt_text = gt_text.
    CLEAR gt_text[].

    SORT lt_text. " Feld SORT wird auch berücksichtigt / Field SORT is also taken into account

    LOOP AT lt_text ASSIGNING <ls_text>.
      APPEND LINES OF <ls_text>-textlines TO ls_text-textlines.

      AT END OF filter.
        " Nun haben wir den letzten Eintrag eines Blockes
        " Now we have the last entry of a block
        ls_text-key1   = <ls_text>-key1.
        ls_text-key2   = <ls_text>-key2.
        ls_text-key3   = <ls_text>-key3.
        ls_text-key4   = <ls_text>-key4.
        ls_text-filter = <ls_text>-filter.
        APPEND ls_text TO gt_text.
        CLEAR: ls_text.
      ENDAT.

    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_placeholder.
************************************************************************
*&  Key           : TS-180411
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Diese Methode bereitet die Platzhalter-Variablen so auf, dass in
*&  der weiteren Verarbeitung direkt gesucht werden kann. Darüber
*&  hinaus berücksichtigt diese Methode auch die Logik, dass in den
*&  SO10 Texten abhängig vom Editor Variablen automatisch in dieser
*&  Form im SO10 Text abgelegt werden:  <(>&<)>Variable<(>&<)>
*&
*& This method prepares the placeholder variables in such a way that in
*& can be searched for directly in further processing. Furthermore
*& in addition, this method also takes into account the logic that in the
*& SO10 texts automatically in this
*& form in the SO10 text, depending on the editor:  <(>&<)>Variable<(>&<)>
************************************************************************

    DATA: ls_key_value TYPE zaf_s_texts_key_value.

    FIELD-SYMBOLS: <ls_key_value_prepared> TYPE zaf_s_texts_key_value.

    LOOP AT it_key_value INTO ls_key_value.
      " Aufbau des Platzhalters für die Suche im Text
      " Structure of the placeholder for the search in the text
      APPEND INITIAL LINE TO rt_key_value ASSIGNING <ls_key_value_prepared>.
      CONCATENATE iv_placeholder_startkey ls_key_value-placeholder
                  iv_placeholder_endkey INTO <ls_key_value_prepared>-placeholder.
      <ls_key_value_prepared>-value = ls_key_value-value.

      " Zusätzlich die Logik für die Standardvariablen berücksichtigen
      " Additionally consider the logic for the standard variables
      APPEND INITIAL LINE TO rt_key_value ASSIGNING <ls_key_value_prepared>.
      CONCATENATE '<(>&<)>' ls_key_value-placeholder
                  '<(>&<)>' INTO <ls_key_value_prepared>-placeholder.
      <ls_key_value_prepared>-value = ls_key_value-value.

    ENDLOOP.

  ENDMETHOD.


  METHOD read_text.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  SO10 Text lesen
*&
*&  SO10 Read text
************************************************************************

    DATA: lv_err_text TYPE balnrext,
          lv_text     TYPE bdc_vtext1,
          lv_note     TYPE bdc_vtext1,
          lv_index    TYPE char10,
          ls_msg      TYPE zcl_af_core_messages=>ts_msg.

    DATA(lo_msg) = NEW zcl_af_core_messages( ).
    DATA(lo_msg_s) = NEW zcl_af_core_messages( ).

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = is_text_key-tdid
        language                = is_text_key-tdspras
        name                    = is_text_key-tdname
        object                  = is_text_key-tdobject
      TABLES
        lines                   = rt_textlines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF rt_textlines IS INITIAL.

      " Strukturwerte mit Namen in eim Meldungsobjekt hinzufügen
      " Add structure values with names in a message object
      lo_msg = zcl_af_core_util=>transfer_struc_val_to_msg_obj( EXPORTING is_structure = is_text_key ).

      CONCATENATE 'READ_TEXT:' is_text_key-tdname 'ID:' is_text_key-tdid 'nicht vorhanden.' INTO lv_err_text SEPARATED BY space.

      zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = lv_err_text
                                     io_msg       = lo_msg ).

    ELSE.

      LOOP AT rt_textlines ASSIGNING FIELD-SYMBOL(<ls_textline>).

        WRITE sy-tabix TO lv_index.
        CONCATENATE 'Textzeile - ' lv_index INTO lv_note SEPARATED BY space.
        lo_msg_s->add_note( iv_note = lv_note ).

        " Strukturwerte mit Namen in eim Meldungsobjekt hinzufügen
        " Add structure values with names in a message object
        lo_msg = zcl_af_core_util=>transfer_struc_val_to_msg_obj( is_structure = <ls_textline> ).

        " Meldungen sammeln
        " Collect messages
        lo_msg_s->add_messages_from_object( io_msg = lo_msg ).

      ENDLOOP.

      CONCATENATE 'READ_TEXT:' is_text_key-tdname 'ID:' is_text_key-tdid 'erfolgreich gelesen.' INTO lv_err_text SEPARATED BY space.

      " Log wegschreiben
      " Write away log
      zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = lv_err_text
                                     io_msg       = lo_msg_s ).

    ENDIF.


  ENDMETHOD.


  METHOD SET_TEXT_TABLE.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Texttabelle setzen
*&
*&  Set text table
************************************************************************

    gt_text = it_text.

  ENDMETHOD.
ENDCLASS.
