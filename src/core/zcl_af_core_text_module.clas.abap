class ZCL_AF_CORE_TEXT_MODULE definition
  public
  final
  create public .

public section.

  methods CHANGE_TEXT
    importing
      !IT_KEY_VALUE type ZAF_GT_TEXTS_KEY_VALUE
      !IV_PLACEHOLDER_STARTKEY type CHAR1
      !IV_PLACEHOLDER_ENDKEY type CHAR1
    changing
      !CT_TEXTLINES type TSFTEXT .
protected section.
private section.

  methods PREPARE_PLACEHOLDER
    importing
      !IT_KEY_VALUE type ZAF_GT_TEXTS_KEY_VALUE
      !IV_PLACEHOLDER_STARTKEY type CHAR1
      !IV_PLACEHOLDER_ENDKEY type CHAR1
    returning
      value(RT_KEY_VALUE) type ZAF_GT_TEXTS_KEY_VALUE .
ENDCLASS.



CLASS ZCL_AF_CORE_TEXT_MODULE IMPLEMENTATION.


  METHOD change_text.
***********************************************************************
**& Key           : WG-230216
**& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase - Developme
***********************************************************************
**& Description (short)
**& Change text
***********************************************************************

    TYPES: BEGIN OF l_ty_longlines,
             tdformat    TYPE tdformat,
             tdline_long TYPE string,
           END OF l_ty_longlines.

    DATA: lt_longlines TYPE STANDARD TABLE OF l_ty_longlines,
          lt_key_value TYPE zaf_gt_texts_key_value.

    FIELD-SYMBOLS: <ls_longlines> TYPE l_ty_longlines,
                   <ls_textlines> TYPE LINE OF tsftext,
                   <ls_key_value> TYPE ZAF_Gs_TEXTS_KEY_VALUE.


    " Ein Austausch macht nur mit gefüllter Key-Value Tabelle Sinn
    " An exchange only makes sense with a filled key-value table.
    CHECK lines( it_key_value ) > 0.

    " Platzhaltervariablen aufbereiten
    " Prepare placeholder variables
    lt_key_value = prepare_placeholder( it_key_value            = it_key_value
                                        iv_placeholder_startkey = iv_placeholder_startkey
                                        iv_placeholder_endkey   = iv_placeholder_endkey ).

    " Überführen der Langzeilen in einen String
    " TS-181016: Überarbeitung der Logik, da im IFbA Leerzeichen fehlten
    "
    " Converting the long lines into a string
    " TS-181016: Revision of the logic, as blanks were missing in the IFbA
    LOOP AT ct_textlines ASSIGNING <ls_textlines>.
      IF sy-tabix = 1.
        "Es beginnt eine neue Langzeile
        "A new long line begins
        APPEND INITIAL LINE TO lt_longlines ASSIGNING <ls_longlines>.
        <ls_longlines>-tdformat = <ls_textlines>-tdformat.
        <ls_longlines>-tdline_long = <ls_textlines>-tdline.
      ELSE.
        CASE <ls_textlines>-tdformat.
          WHEN '='. "Fliesstext direkt hintereinander / Continuous text directly one after the other
            CONCATENATE <ls_longlines>-tdline_long <ls_textlines>-tdline INTO <ls_longlines>-tdline_long.
          WHEN ' '. "Fliesstext, Leerzeichen einfügen / Continuous text, insert spaces
            CONCATENATE <ls_longlines>-tdline_long <ls_textlines>-tdline INTO <ls_longlines>-tdline_long
            SEPARATED BY space.
          WHEN OTHERS. "Neue Zeile/ New line
            APPEND INITIAL LINE TO lt_longlines ASSIGNING <ls_longlines>.
            <ls_longlines>-tdformat    = <ls_textlines>-tdformat.
            <ls_longlines>-tdline_long = <ls_textlines>-tdline.
        ENDCASE.
      ENDIF.
    ENDLOOP.

    "Austausch der Variablen
    "Exchange of variables
    LOOP AT lt_key_value ASSIGNING <ls_key_value>.
      LOOP AT lt_longlines ASSIGNING <ls_longlines>.
        REPLACE ALL OCCURRENCES OF <ls_key_value>-placeholder
                IN <ls_longlines>-tdline_long
                WITH <ls_key_value>-value
                IGNORING CASE.
      ENDLOOP.
    ENDLOOP.

    "Zurückschreiben der Langzeilen
    "Writing back the long lines
    CLEAR ct_textlines.

    LOOP AT lt_longlines ASSIGNING <ls_longlines>.
      IF strlen( <ls_longlines>-tdline_long ) = 0.
        " Leere Zeile muss übernommen werden
        " Empty line must be taken over
        APPEND INITIAL LINE TO ct_textlines ASSIGNING <ls_textlines>.
        <ls_textlines>-tdformat = <ls_longlines>-tdformat.
      ELSE.
        WHILE strlen( <ls_longlines>-tdline_long ) > 0.
          APPEND INITIAL LINE TO ct_textlines ASSIGNING <ls_textlines>.
          IF sy-index = 1.
            <ls_textlines>-tdformat = <ls_longlines>-tdformat.
          ELSE.
            <ls_textlines>-tdformat = '='.   "Langzeile / Longline
          ENDIF.
          <ls_textlines>-tdline = <ls_longlines>-tdline_long.
*->TS-190516
* Ist die 132te Stelle ein Leerzeichen, dann wird sie nicht in <ls_textlines>-tdline
* übernommen. In diesem Fall wird um ein Zeichen (das Leerzeichen) weniger verschoben.
*          SHIFT <ls_longlines>-tdline_long BY 132 PLACES LEFT.
*
* If the 132nd character is a space, it is not transferred to <ls_textlines>-tdline.
* In this case, one less character (the space) is shifted.
* SHIFT <ls_longlines>-tdline_long BY 132 PLACES LEFT.
          IF strlen( <ls_longlines>-tdline_long ) >= 132.
            IF <ls_textlines>-tdline+131(1) IS INITIAL.
              SHIFT <ls_longlines>-tdline_long BY 131 PLACES LEFT.
            ELSE.
              SHIFT <ls_longlines>-tdline_long BY 132 PLACES LEFT.
            ENDIF.
          ELSE.
            SHIFT <ls_longlines>-tdline_long BY 132 PLACES LEFT.
          ENDIF.
*<-TS-190516
        ENDWHILE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_placeholder.
**********************************************************************
*& Key           : WG-230216
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase - Developme
**********************************************************************
*& Description (short)
*& Preparing placeholder variables
**********************************************************************

    DATA: ls_key_value TYPE zaf_gs_texts_key_value.

    FIELD-SYMBOLS: <ls_key_value_prepared> TYPE zaf_gs_texts_key_value.

    LOOP AT it_key_value INTO ls_key_value.
      APPEND INITIAL LINE TO rt_key_value ASSIGNING <ls_key_value_prepared>.
      CONCATENATE iv_placeholder_startkey ls_key_value-placeholder
                  iv_placeholder_endkey INTO <ls_key_value_prepared>-placeholder.
      <ls_key_value_prepared>-value = ls_key_value-value.

      APPEND INITIAL LINE TO rt_key_value ASSIGNING <ls_key_value_prepared>.
      CONCATENATE '<(>&<)>' ls_key_value-placeholder
                  '<(>&<)>' INTO <ls_key_value_prepared>-placeholder.
      <ls_key_value_prepared>-value = ls_key_value-value.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
