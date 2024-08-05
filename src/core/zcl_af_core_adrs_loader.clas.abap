class ZCL_AF_CORE_ADRS_LOADER definition
  public
  final
  create public .

public section.

  data GT_ADRS type ZAF_T_ADRS .
  data GV_LOG_OBJECT type BALOBJ_D .

  class-methods GET_INSTANCE
    importing
      !IV_LOG_OBJECT type BALOBJ_D
    returning
      value(RO_ADRS_LOADER) type ref to ZCL_AF_CORE_ADRS_LOADER
    raising
      ZCX_AF_CORE_OUTPUT .
  methods ADD_LINE_TO_ADRS
    importing
      !IS_ADRS_PARAMS type ZAF_S_ADRS_PARAMS
      !IV_LINE type LINES
    raising
      ZCX_AF_CORE_OUTPUT .
  methods ADD_ADRS
    importing
      !IS_ADRS_KEY type ZAF_S_ADRS_KEY
      !IS_ADRS_PARAMS type ZAF_S_ADRS_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CONSTRUCTOR
    importing
      !IV_LOG_OBJECT type BALOBJ_D .
  methods GET_ADRS_STRUCTURE
    importing
      !IV_FILTER type ZAF_S_ADRS_PARAMS-FILTER
      !IV_DELETE_ADRS_ENTRY type ABAP_BOOL default ABAP_TRUE
    returning
      value(RS_ADRS) type ADRS_PRINT .
  methods GET_ADRS_TABLE
    returning
      value(RT_ADRS) type ZAF_T_ADRS .
  methods GET_ADRS_LINE
    importing
      !IV_FILTER type ZAF_S_ADRS_PARAMS-FILTER
      !IV_KEY1 type ZAF_S_ADRS_PARAMS-KEY1 default ' '
      !IV_SEPARATOR type CHAR1 default ''
      !IV_DELETE_ADRS_ENTRY type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_ADRS) type ADDRGP_KK .
  methods ADD_ADRS_FROM_TABLE
    importing
      !IT_ADRS_LINES type ZAF_T_LINE
      !IS_ADRS_PARAMS type ZAF_S_ADRS_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
protected section.
private section.

  methods CHANGE_ADRS
    importing
      !IV_SPRAS type SPRAS
    changing
      !CS_ADRS_PRINT type ADRS_PRINT .
  methods READ_ADRS
    importing
      !IS_ADRS_KEY type ZAF_S_ADRS_KEY
    returning
      value(RS_ADDRESS_PRINTFORM) type ADRS_PRINT
    raising
      ZCX_AF_CORE_OUTPUT .
  methods REPLACE_IN_ADRS
    importing
      !IV_FIND_STR type STRING
      !IV_REPLACE_STR type STRING
    changing
      !CS_ADRS_PRINT type ADRS_PRINT .
  methods DELETE_EMPTY_LINES
    changing
      !CT_ADDRESSLINES type ZAF_T_LINE .
ENDCLASS.



CLASS ZCL_AF_CORE_ADRS_LOADER IMPLEMENTATION.


  METHOD add_adrs.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Adresse an Instanztabelle hinzufügen.
*&  Add address to instance table.
************************************************************************

    DATA: ls_single_adrs TYPE zaf_s_adrs,
          ls_adrs_key    TYPE zaf_s_adrs_key,
          ls_adrs_line   TYPE zaf_s_line,
          ls_adrs_print  TYPE adrs_print.

    FIELD-SYMBOLS: <lv_fieldvalue> TYPE data.

    " Übergabeparameter müssen für einen eindeutigen Text gepflegt sein
    " Transfer parameters must be maintained for a unique text
    CHECK is_adrs_key-address_type               IS NOT INITIAL AND
          is_adrs_key-address_number             IS NOT INITIAL AND
          is_adrs_key-sender_country             IS NOT INITIAL AND
          is_adrs_key-receiver_language          IS NOT INITIAL AND
          is_adrs_key-number_of_lines            IS NOT INITIAL AND
          is_adrs_key-line_priority              IS NOT INITIAL AND
          is_adrs_key-country_name_in_rec_langu  IS NOT INITIAL AND
          is_adrs_key-language_for_country_name  IS NOT INITIAL.

    ls_adrs_key = is_adrs_key.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_adrs_key-address_number
      IMPORTING
        output = ls_adrs_key-address_number.

    " Adresse auslesen
    " Read address
    read_adrs( EXPORTING is_adrs_key          = ls_adrs_key
               RECEIVING rs_address_printform = ls_adrs_print ).

    " Adresse für überarbeiten
    " Address for revise
    change_adrs( EXPORTING iv_spras      = is_adrs_key-receiver_language
                 CHANGING  cs_adrs_print = ls_adrs_print ).

    ls_adrs_line-line = ls_adrs_print-line0.
    APPEND ls_adrs_line TO ls_single_adrs-addresslines.
    ls_adrs_line-line = ls_adrs_print-line1.
    APPEND ls_adrs_line TO ls_single_adrs-addresslines.
    ls_adrs_line-line = ls_adrs_print-line2.
    APPEND ls_adrs_line TO ls_single_adrs-addresslines.
    ls_adrs_line-line = ls_adrs_print-line3.
    APPEND ls_adrs_line TO ls_single_adrs-addresslines.
    ls_adrs_line-line = ls_adrs_print-line4.
    APPEND ls_adrs_line TO ls_single_adrs-addresslines.
    ls_adrs_line-line = ls_adrs_print-line5.
    APPEND ls_adrs_line TO ls_single_adrs-addresslines.
    ls_adrs_line-line = ls_adrs_print-line6.
    APPEND ls_adrs_line TO ls_single_adrs-addresslines.
    ls_adrs_line-line = ls_adrs_print-line7.
    APPEND ls_adrs_line TO ls_single_adrs-addresslines.
    ls_adrs_line-line = ls_adrs_print-line8.
    APPEND ls_adrs_line TO ls_single_adrs-addresslines.
    ls_adrs_line-line = ls_adrs_print-line9.
    APPEND ls_adrs_line TO ls_single_adrs-addresslines.

    " Übergabestruktur befüllen
    " Fill the transfer structure
    ls_single_adrs-key1   = is_adrs_params-key1.
    ls_single_adrs-key2   = is_adrs_params-key2.
    ls_single_adrs-key3   = is_adrs_params-key3.
    ls_single_adrs-key4   = is_adrs_params-key4.
    ls_single_adrs-sort   = is_adrs_params-sort.
    ls_single_adrs-filter = is_adrs_params-filter.

    delete_empty_lines( CHANGING ct_addresslines = ls_single_adrs-addresslines ).

    " Adresse an Tabelle anhängen
    " Append address to table
    APPEND ls_single_adrs TO gt_adrs.
  ENDMETHOD.


  METHOD add_adrs_from_table.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Adresse an Instanztabelle hinzufügen.
*&
*&  Add address to instance table.
************************************************************************

    DATA: ls_single_adrs TYPE zaf_s_adrs.

    " Adresstabelle übergeben
    " Passing Address Table
    ls_single_adrs-addresslines[] = it_adrs_lines[].

    " Übergabestruktur befüllen
    " Fill the transfer structure
    ls_single_adrs-key1   = is_adrs_params-key1.
    ls_single_adrs-key2   = is_adrs_params-key2.
    ls_single_adrs-key3   = is_adrs_params-key3.
    ls_single_adrs-key4   = is_adrs_params-key4.
    ls_single_adrs-sort   = is_adrs_params-sort.
    ls_single_adrs-filter = is_adrs_params-filter.

    " Leerzeilen am Ende der Adresse entfernen
    " Remove blank lines at the end of the address
    delete_empty_lines( CHANGING ct_addresslines = ls_single_adrs-addresslines ).

    " Adresse an Tabelle anhängen
    " Append address to table
    APPEND ls_single_adrs TO gt_adrs.

  ENDMETHOD.


  METHOD add_line_to_adrs.
************************************************************************
*&  Key           : AR-211202
*&  Request No.   : 200518-144411-AR - Adobe Forms: Bestellung - Entwicklung und Layout
************************************************************************
*&  Description (short)
*&  Einzelne Zeile an Instanztabelle hinzufügen.
*&  Add single row to instance table.
************************************************************************

    IF iv_line IS NOT INITIAL.
      READ TABLE gt_adrs ASSIGNING FIELD-SYMBOL(<ls_adrs>)
                                           WITH KEY key1 = is_adrs_params-key1
                                                    key2 = is_adrs_params-key2
                                                    key3 = is_adrs_params-key3
                                                    key4 = is_adrs_params-key4
                                                  filter = is_adrs_params-filter
                                                    sort = is_adrs_params-sort.

      IF <ls_adrs> IS ASSIGNED.
        APPEND INITIAL LINE TO <ls_adrs>-addresslines ASSIGNING FIELD-SYMBOL(<ls_addressline>).
        <ls_addressline>-line = iv_line.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD change_adrs.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Adresse aufbauen:
*&  Es werden 10 Adresszeilen übergeben (aus FuBa Address_Into_Printform).
*&  In der Funktion findet der Adressaufbau in einem Feld statt und die
*&  Übersetzung des Titels. In SAP wird der Titel nur in den Sprachen
*&  DE und EN übergeben. Die Änderung der Bezeichnung "Postfach" kann
*&  nachträglich eingebaut werden.
*&
*&  Build address:
*&  10 address lines are passed (from FuBa Address_Into_Printform).
*&  In the function, the address structure takes place in a field and the
*&  Translation of the title. In SAP, the title is only available in the languages
*&  DE and EN. Changing the label "mailbox" may:
*&  can be retrofitted later.
************************************************************************

    DATA: lv_find_str TYPE string.
    DATA: lv_spras_iso TYPE spras_iso.

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
      EXPORTING
        input  = iv_spras
      IMPORTING
        output = lv_spras_iso.

    " Bezeichung des Titels ändern
    " Change title name
    IF cs_adrs_print-line0 = 'Firma'   OR
       cs_adrs_print-line0 = 'Company' OR
       cs_adrs_print-line0 = 'Société' OR
       cs_adrs_print-line0 = 'Ditta'.

      CASE lv_spras_iso.
        WHEN 'DE'. cs_adrs_print-line0 = 'Firma'.
        WHEN 'EN'. cs_adrs_print-line0 = 'Company'.
        WHEN 'FR'. cs_adrs_print-line0 = 'Société'.
        WHEN 'IT'. cs_adrs_print-line0 = 'Spett. le'.
        WHEN 'NL'. cs_adrs_print-line0 = 'Bedrijf'.
        WHEN 'DA'. cs_adrs_print-line0 = ''.
        WHEN 'NO'. cs_adrs_print-line0 = ''.
        WHEN 'SV'. cs_adrs_print-line0 = ''.
        WHEN 'FI'. cs_adrs_print-line0 = 'yhtiö'.
        WHEN 'ET'. cs_adrs_print-line0 = 'Ettevõte'.
        WHEN OTHERS. cs_adrs_print-line0 = 'Company'.
      ENDCASE.
    ENDIF.

    IF cs_adrs_print-line0 = 'Herr' OR cs_adrs_print-line0 = 'Mr.'.
      CASE lv_spras_iso.
        WHEN 'DE'. cs_adrs_print-line0 = 'Herr'.
        WHEN 'EN'. cs_adrs_print-line0 = 'Mr.'.
        WHEN 'FR'. cs_adrs_print-line0 = 'M'.
        WHEN 'IT'. cs_adrs_print-line0 = 'Signor'.
        WHEN 'NL'. cs_adrs_print-line0 = 'Mijnheer'.
        WHEN 'DA'. cs_adrs_print-line0 = 'Firma'.
        WHEN 'NO'. cs_adrs_print-line0 = 'Firma'.
        WHEN 'SV'. cs_adrs_print-line0 = 'Foretag'.
        WHEN 'FI'. cs_adrs_print-line0 = 'herra'.
        WHEN 'ET'. cs_adrs_print-line0 = 'Hr.'.
        WHEN OTHERS. cs_adrs_print-line0 = 'Mr.'.
      ENDCASE.
    ENDIF.

    IF cs_adrs_print-line0 = 'Frau' OR cs_adrs_print-line0 = 'Mrs'.
      CASE lv_spras_iso.
        WHEN 'DE'. cs_adrs_print-line0 = 'Frau'.
        WHEN 'EN'. cs_adrs_print-line0 = 'Mrs'.
        WHEN 'FR'. cs_adrs_print-line0 = 'Mme'.
        WHEN 'IT'. cs_adrs_print-line0 = 'Signora'.
        WHEN 'NL'. cs_adrs_print-line0 = 'Mevrouw'.
        WHEN 'DA'. cs_adrs_print-line0 = 'Firma'.
        WHEN 'NO'. cs_adrs_print-line0 = 'Firma'.
        WHEN 'SV'. cs_adrs_print-line0 = 'Foretag'.
        WHEN 'FI'. cs_adrs_print-line0 = 'rouva'.
        WHEN 'ET'. cs_adrs_print-line0 = 'Pr.'.
        WHEN OTHERS. cs_adrs_print-line0 = 'Mrs'.
      ENDCASE.
    ENDIF.

    " Bezeichnung Postfach ändern
    " Change Name Mailbox
    lv_find_str = 'Postfach'.

    IF  lv_spras_iso = 'EN'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'PO Box' CHANGING cs_adrs_print = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'FR'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'Boîte postale' CHANGING  cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'IT'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'Casella Postale' CHANGING  cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'NL'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'Postbus' CHANGING  cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'DA'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'BOX' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'NO'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'Postboks' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'SV'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'BOX' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'FI'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'PL' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'ET'.
      replace_in_adrs( EXPORTING iv_find_str    = lv_find_str iv_replace_str = 'Postkast' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.


    lv_find_str = 'PO Box'.

    IF lv_spras_iso = 'DE'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'Postfach' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'FR'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'Boîte postale' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'IT'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'Casella Postale' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'NL'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'Postbus' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'DA'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'BOX' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'NO'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'Postboks' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'SV'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'BOX' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'FI'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'PL' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.

    IF lv_spras_iso = 'ET'.
      replace_in_adrs( EXPORTING iv_find_str = lv_find_str iv_replace_str = 'Postkast' CHANGING cs_adrs_print  = cs_adrs_print ).
    ENDIF.

  ENDMETHOD.


    METHOD constructor.
************************************************************************
*&  Key           : AR-180606
*&  Request No.   : 180116-135917-AR - Formularentwicklung Service Label
************************************************************************
*&  Description (short)
*&  Instanziiert die Adressklasse.
*&  Instantiates the address class.
************************************************************************

      gv_log_object = iv_log_object.

      zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = '> START - ZCL_AF_CORE_ADRS_LOADER' ).

    ENDMETHOD.


  METHOD delete_empty_lines.
************************************************************************
*&  Key           : TS-180711
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Leere Zeilen am Ende des Textes werden entfernt.
*&
*&  Empty lines at the end of the text will be removed.
************************************************************************

    DATA: lv_index TYPE sy-index.

    FIELD-SYMBOLS: <ls_addressline>   TYPE zaf_s_line.

    lv_index = lines( ct_addresslines ).

    WHILE lv_index > 0.
      READ TABLE ct_addresslines ASSIGNING <ls_addressline> INDEX lv_index.
      IF <ls_addressline>-line IS INITIAL.
        DELETE ct_addresslines INDEX lv_index.
      ELSE.
        lv_index = 0.
      ENDIF.
      lv_index = lv_index - 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD get_adrs_line.
************************************************************************
*&  Key           : TS-180831
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Eine Adresse in einer Zeile ermitteln.
*&
*&  Find an address on one line.
************************************************************************

    DATA: lv_index TYPE syindex,
          ls_msg   TYPE zcl_af_core_messages=>ts_msg.

    DATA(lo_msg) = NEW zcl_af_core_messages( ).

    FIELD-SYMBOLS: <ls_adrs>      LIKE LINE OF gt_adrs,
                   <ls_adrs_line> TYPE zaf_s_line.

    CLEAR: rv_adrs.

    "Adresse anhand des Filters lesen
    "Read address using the filter
    READ TABLE gt_adrs ASSIGNING <ls_adrs> WITH KEY filter = iv_filter
                                                    key1   = iv_key1.

    IF sy-subrc <> 0.
      ls_msg-msgid = zcl_af_core_constants=>gc_msgid_default.
      ls_msg-msgnr = '089'.
      ls_msg-msgtyp = 'E'.
      ls_msg-msgv1 = iv_filter.

      lo_msg->add_message( is_msg = ls_msg ).

      zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = 'ERROR in GET_ADRS_LINE'
                                     io_msg       = lo_msg ).
      RETURN.
    ENDIF.

    lv_index = sy-tabix.

    "Adressfelder der Rückgabestruktur füllen
    "Fill address fields of the return structure
    LOOP AT <ls_adrs>-addresslines ASSIGNING <ls_adrs_line>.
      CHECK <ls_adrs_line>-line IS NOT INITIAL.
      IF iv_separator IS NOT INITIAL AND
        rv_adrs IS NOT INITIAL.
        CONCATENATE rv_adrs iv_separator INTO rv_adrs.
      ENDIF.
      CONCATENATE rv_adrs <ls_adrs_line>-line INTO rv_adrs SEPARATED BY space.
    ENDLOOP.
    CONDENSE rv_adrs.

    IF rv_adrs IS NOT INITIAL.
      " Log wegschreiben
      " Write away log
      zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = '>> Address erfolgreich in Zeile gelesen'
                                     io_msg       = lo_msg ).
    ELSE.
      " Log wegschreiben
      " Write away log
      zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = '>> Addresszeile ist leer'
                                     io_msg       = lo_msg ).
    ENDIF.

    "Adresszeile aus der Adresstabelle löschen, damit die Adresse in der
    "Adresstabelle nicht zusätzlich übergeben wird, da sie in der Regel
    "dort nicht mehr benötigt wird.
    "Delete the address line from the address table so that the address in the
    "Address table is not passed additionally, because it is usually
    "is no longer needed there.
    CHECK iv_delete_adrs_entry = abap_true.
    DELETE gt_adrs INDEX lv_index.

  ENDMETHOD.


  METHOD get_adrs_structure.
************************************************************************
*&  Key           : TS-180704
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Eine Adresse als Struktur ermitteln. Der FILTER Ausdruck sollte
*&  eindeutig sein, damit auch genau eine Adresse zurückgeliefert
*&  werden kann.
*&
*&  Determine an address as a structure. The FILTER expression should
*&  unique, so that exactly one address is returned
*&  can become.
************************************************************************

    DATA: lv_index  TYPE syindex.
    DATA: lv_text   TYPE bdc_vtext1.
    DATA: lo_msg    TYPE REF TO zcl_af_core_messages.
    DATA: ls_msg    TYPE zcl_af_core_messages=>ts_msg.

    lo_msg = NEW zcl_af_core_messages( ).

    FIELD-SYMBOLS: <ls_adrs>       LIKE LINE OF gt_adrs,
                   <ls_adrs_line>  TYPE zaf_s_line,
                   <ls_adrs_field> TYPE zaf_s_line-line.

    "Adresse anhand des Filters lesen
    "Read address using the filter
    READ TABLE gt_adrs ASSIGNING <ls_adrs> WITH KEY filter = iv_filter.
    IF sy-subrc <> 0.
      ls_msg = VALUE #( msgid = zcl_af_core_constants=>gc_msgid_default
                        msgnr = '089'
                        msgtyp = 'E'
                        msgv1 = iv_filter ).

      lo_msg->add_message( is_msg = ls_msg ).

      zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = 'ERROR in GET_ADRS_STRUCTURE'
                                     io_msg       = lo_msg ).

      RETURN.
    ENDIF.

    lv_index = sy-tabix.

    "Adressfelder der Rückgabestruktur füllen
    "Fill address fields of the return structure
    LOOP AT <ls_adrs>-addresslines ASSIGNING <ls_adrs_line>.
      ASSIGN COMPONENT sy-tabix OF STRUCTURE rs_adrs TO <ls_adrs_field>.
      CHECK sy-subrc = 0.
      <ls_adrs_field> = <ls_adrs_line>-line.
    ENDLOOP.

    IF rs_adrs IS NOT INITIAL.
      " Strukturwerte mit Namen in eim Meldungsobjekt hinzufügen
      " Add structure values with names in a message object
      zcl_af_core_util=>transfer_struc_val_to_msg_obj( EXPORTING is_structure = rs_adrs
                                                       RECEIVING ro_msg       = lo_msg ).

      " Log wegschreiben
      " Write away log
      zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = '>> Address erfolgreich in Struktur gelesen'
                                     io_msg       = lo_msg ).

    ELSE.
      " Log wegschreiben
      " Write away log
      zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = '>> Address Struktur ist leer'
                                     io_msg       = lo_msg ).
    ENDIF.

    "Adresszeile aus der Adresstabelle löschen, damit die Adresse in der
    "Adresstabelle nicht zusätzlich übergeben wird, da sie in der Regel
    "dort nicht mehr benötigt wird.
    "Delete the address line from the address table so that the address in the
    "Address table is not passed additionally, because it is usually
    "is no longer needed there.
    CHECK iv_delete_adrs_entry = abap_true.
    DELETE gt_adrs INDEX lv_index.

  ENDMETHOD.


  METHOD get_adrs_table.
************************************************************************
*&  Key           : AR-180605
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Adressentabelle ermitteln
*&  Determine address table
************************************************************************

    rt_adrs = gt_adrs.

    SORT rt_adrs.

  ENDMETHOD.


  METHOD get_instance.
************************************************************************
*&  Key           : AR-180605
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Instanz des Adress_Loaders bekommen
*&  get instance
************************************************************************

    ro_adrs_loader = NEW zcl_af_core_adrs_loader( iv_log_object ).

  ENDMETHOD.


  METHOD read_adrs.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Adressaufbereitung gemäss Postvorschriften lesen
*6
*&  Read address preparation according to postal regulations
************************************************************************

    DATA: l_text TYPE bdc_vtext1,
          ls_msg TYPE zcl_af_core_messages=>ts_msg.

    DATA(lo_msg) = NEW zcl_af_core_messages( ).

    CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
      EXPORTING
        adrswa_in                      = is_adrs_key-adrswa_in
        address_1                      = is_adrs_key-address_1
        address_2                      = is_adrs_key-address_2
        address_3                      = is_adrs_key-address_3
        address_type                   = is_adrs_key-address_type
        address_number                 = is_adrs_key-address_number
        address_handle                 = is_adrs_key-address_handle
        person_number                  = is_adrs_key-person_number
        person_handle                  = is_adrs_key-person_handle
        sender_country                 = is_adrs_key-sender_country
        receiver_language              = is_adrs_key-receiver_language
        number_of_lines                = is_adrs_key-number_of_lines
        street_has_priority            = is_adrs_key-street_has_priority
        line_priority                  = is_adrs_key-line_priority
        country_name_in_receiver_langu = is_adrs_key-country_name_in_rec_langu
        language_for_country_name      = is_adrs_key-language_for_country_name
        no_upper_case_for_city         = is_adrs_key-no_upper_case_for_city
        iv_nation                      = is_adrs_key-iv_nation
        iv_nation_space                = is_adrs_key-iv_nation_space
        iv_person_above_organization   = is_adrs_key-iv_person_above_organization
        is_bupa_time_dependency        = is_adrs_key-is_bupa_time_dependency
        iv_langu_crea                  = is_adrs_key-iv_langu_crea
      IMPORTING
        address_printform              = rs_address_printform.

    IF rs_address_printform IS INITIAL.

      ls_msg-msgid = zcl_af_core_constants=>gc_msgid_default.
      ls_msg-msgnr = '062'.
      ls_msg-msgtyp = 'E'.
      ls_msg-msgv1 = is_adrs_key-address_number.

      lo_msg->add_message( is_msg = ls_msg ).

      zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = 'ERROR in READ_ADRS'
                                     io_msg       = lo_msg ).
    ELSE.

      " Strukturwerte mit Namen in eim Meldungsobjekt hinzufügen
      " Add structure values with names in a message object
      lo_msg =  zcl_af_core_util=>transfer_struc_val_to_msg_obj( is_structure = rs_address_printform ).

      " Log wegschreiben
      " Write away log
      zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                     iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                     iv_ext_ident = '>> Address erfolgreich gelesen'
                                     io_msg       = lo_msg ).

    ENDIF.

  ENDMETHOD.


  method REPLACE_IN_ADRS.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Suchen und Ersetzen in der Adresse
*&  Search and replace in the address
************************************************************************

      REPLACE iv_find_str IN cs_adrs_print-line1 WITH iv_replace_str.
      REPLACE iv_find_str IN cs_adrs_print-line2 WITH iv_replace_str.
      REPLACE iv_find_str IN cs_adrs_print-line3 WITH iv_replace_str.
      REPLACE iv_find_str IN cs_adrs_print-line4 WITH iv_replace_str.
      REPLACE iv_find_str IN cs_adrs_print-line5 WITH iv_replace_str.
      REPLACE iv_find_str IN cs_adrs_print-line6 WITH iv_replace_str.
      REPLACE iv_find_str IN cs_adrs_print-line7 WITH iv_replace_str.
      REPLACE iv_find_str IN cs_adrs_print-line8 WITH iv_replace_str.
      REPLACE iv_find_str IN cs_adrs_print-line9 WITH iv_replace_str.

  endmethod.
ENDCLASS.
