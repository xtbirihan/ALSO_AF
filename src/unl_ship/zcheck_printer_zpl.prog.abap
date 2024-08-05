*&---------------------------------------------------------------------*
*& Report ZCHECK_PRINTRE
*&---------------------------------------------------------------------*
*& Testen Zebra-Drucker
*&---------------------------------------------------------------------*
REPORT zcheck_printer_zpl.

TYPES: BEGIN OF ts_data,
         matnr TYPE matnr,
         maktx TYPE maktx,
       END OF ts_data.

TYPES: tt_data TYPE STANDARD TABLE OF ts_data WITH EMPTY KEY.

DATA: lt_zpl    TYPE STANDARD TABLE OF tdline,
      lv_field1 TYPE fieldname VALUE '*p_barco*',
      lv_field  TYPE fieldname VALUE 'P',
      lv_layout TYPE char30.

FIELD-SYMBOLS: <ls_zpl> LIKE LINE OF lt_zpl.

PARAMETERS: p_device TYPE nast-ldest  DEFAULT 'DESO', " The device short name
            p_barco  TYPE c LENGTH 10 DEFAULT '1234567890',
            p_copie  TYPE n LENGTH 3  DEFAULT '1'.

PARAMETERS: p_data TYPE tt_data NO-DISPLAY.


"ZPL / Labeldisign
lt_zpl = VALUE #( BASE lt_zpl ( '^XA ^PW800' )
                              ( '^FO50,60^A0,40^FDHello World!!^fs' )
                              ( '^FO60,120^BY3^BCN,60,,,,A^fd*p_barco*^fs' )
                              ( '^FO25,25^GB480,200,2^FS' )
                              ( '^XZ' ) ).

DATA: lt_result TYPE match_result_tab.
" Austausch der Platzhalter im Label durch Daten
FIND ALL OCCURRENCES OF lv_field1 IN TABLE lt_zpl RESULTS lt_result.
LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
  READ TABLE lt_zpl INDEX <ls_result>-line ASSIGNING <ls_zpl>.
  CHECK sy-subrc = 0.
  REPLACE lv_field1 IN <ls_zpl> WITH p_barco.
ENDLOOP.

CHECK sy-subrc = 0.

lv_layout = |Test / Printe { p_device }|.
" Druckanstoß
NEW-PAGE PRINT ON DESTINATION p_device
                  COPIES                  1
                  LIST NAME               'printer test (ZPL)'
                  LIST DATASET            space
                  IMMEDIATELY             abap_on
                  KEEP IN SPOOL           abap_on
                  ARCHIVE MODE            '1'
                  LINE-COUNT              60
                  LINE-SIZE               100
                  LAYOUT                  lv_layout
                  NEW LIST IDENTIFICATION abap_on
                  SAP COVER PAGE          space
                  NO DIALOG
                  NO-TITLE
                  NO-HEADING.

IF p_copie < 1.
  p_copie = 1.
ENDIF.

DO p_copie TIMES.
  LOOP AT lt_zpl ASSIGNING <ls_zpl>.
    WRITE: / <ls_zpl>.
  ENDLOOP.
ENDDO.

NEW-PAGE PRINT OFF.
