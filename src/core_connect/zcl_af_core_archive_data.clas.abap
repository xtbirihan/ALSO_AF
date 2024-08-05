class ZCL_AF_CORE_ARCHIVE_DATA definition
  public
  final
  create public .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .

  data GV_ARCHIV_ID type SAEARCHIVI .
  data GV_DOCUMENT_TYPE type SAEDOKTYP .
  data GV_LENGTH type NUM12 .
  data GV_OBJECT_ID type SAEOBJID .
  data GT_PDF type FKK_TBL1024_T .
  data GV_ARC_DATE type SYST_DATUM .
  data GV_AR_OBJECT type SAEOBJART .
  data GV_SAP_OBJECT type SAEANWDID .
  data GV_UNAME type UNAME .
  data GS_ARCHIVE_ERR type ZAF_ARCHIVE_ERR .

  methods CONSTRUCTOR
    importing
      !IV_ARCHIV_ID type SAEARCHIVI
      !IV_DOCUMENT_TYPE type SAEDOKTYP
      !IV_LENGTH type NUM12
      !IV_OBJECT_ID type SAEOBJID
      !IT_PDF type FKK_TBL1024_T
      !IV_ARC_DATE type SYST_DATUM
      !IV_AR_OBJECT type SAEOBJART
      !IV_SAP_OBJECT type SAEANWDID
      !IV_UNAME type UNAME .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AF_CORE_ARCHIVE_DATA IMPLEMENTATION.


  METHOD constructor.
************************************************************************
*&  Key           : AR-181018
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Hier wird Konstruktor schon alles gemacht.
*&  Daten übergeben und als Objekt gespeichert.
*&
*& Here Konstruktor everything is already done.
*& Data and saved as an object.
************************************************************************

    DATA: lv_obj_serialized TYPE string,
          ls_archive_err    TYPE zaf_archive_err.

    gv_archiv_id     = iv_archiv_id.
    gv_document_type = iv_document_type.
    gv_length        = iv_length.
    gv_object_id     = iv_object_id.
    gt_pdf           = it_pdf.
    gv_arc_date      = iv_arc_date.
    gv_ar_object     = iv_ar_object.
    gv_sap_object    = iv_sap_object.
    gv_uname         = iv_uname.

    CALL TRANSFORMATION id
                 SOURCE model = me
             RESULT XML lv_obj_serialized.

    ls_archive_err-archiv_id     = gv_archiv_id.
    ls_archive_err-document_type = gv_document_type.
    ls_archive_err-length        = gv_length.
    ls_archive_err-object_id     = gv_object_id.
    ls_archive_err-arc_data_obj  = lv_obj_serialized.
    ls_archive_err-arc_date      = gv_arc_date.
    ls_archive_err-ar_object     = gv_ar_object.
    ls_archive_err-sap_object    = gv_sap_object.
    ls_archive_err-uname         = gv_uname.

    INSERT zaf_archive_err FROM ls_archive_err.

  ENDMETHOD.
ENDCLASS.
