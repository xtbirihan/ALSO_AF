class ZCL_AF_CORE_UTIL_ARCHIVEDOC definition
  public
  final
  create public .

public section.

  class-methods GET_ARCHIVE_OBJECT
    importing
      !IV_BUKRS type BUKRS
      !IV_OBJECT_KEY type CHAR6
    exporting
      !EV_AR_OBJECT type SAEOBJART
      !EV_ARCHIV_ID type SAEARCHIVI .
  class-methods GET_ARCHIVE_KEYS_FROM_BUKRS
    importing
      !IV_BUKRS type BUKRS
    exporting
      !EV_BUSINESS_KEY type CHAR2
      !EV_ARCHIV_ID type SAEARCHIVI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AF_CORE_UTIL_ARCHIVEDOC IMPLEMENTATION.


  METHOD get_archive_keys_from_bukrs.
**********************************************************************
*& Key           : WG-230216
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase - Developme
**********************************************************************
*& Description (short)
*& Deliver Business Abbreviations for Company Code
**********************************************************************

    CLEAR: ev_business_key, ev_archiv_id.

    SELECT SINGLE business_key archiv_id
      FROM zarchive_ids
      INTO ( ev_business_key, ev_archiv_id )
      WHERE bukrs = iv_bukrs.

  ENDMETHOD.


  METHOD get_archive_object.
***********************************************************************
**& Key           : WG-230216
**& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase - Developme
***********************************************************************
**& Description (short)
**& Get archive object
***********************************************************************


    DATA: lv_business_key TYPE zgb,
          lv_ar_object    TYPE char10.

    CLEAR: ev_ar_object, ev_archiv_id.

    "Geschäftsbereich und Archiv_id ermitteln
    "Determine business area and Archiv_id
    zcl_af_core_util_archivedoc=>get_archive_keys_from_bukrs( EXPORTING iv_bukrs        = iv_bukrs
                                                              IMPORTING ev_business_key = lv_business_key
                                                                        ev_archiv_id    = ev_archiv_id ).

    "Buchungskreisübergreifende Dokumentart aus Steuertabelle lesen.
    "Read cross-company code document type from tax table.
    SELECT SINGLE ar_object INTO lv_ar_object
      FROM zarchive_object
      WHERE object_key = iv_object_key.

    CHECK sy-subrc = 0.

    "Platzhalter ## durch Geschäftsbereich ersetzen
    "Replace placeholder ## with business unit
    REPLACE '##' IN lv_ar_object WITH lv_business_key.

    "Prüfung der ermittelten Dokumentart gegen das Customizing
    "Checking the determined document type against Customizing
    SELECT SINGLE ar_object FROM toadv
      INTO ev_ar_object
      WHERE ar_object = lv_ar_object.

  ENDMETHOD.
ENDCLASS.
