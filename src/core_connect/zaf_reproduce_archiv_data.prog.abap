************************************************************************
*&  Name          : Nacharbeiten von Archivdaten
*&  Transaction   : Wird direkt von der Basis aufgerufen
*&  Titel         : Archivdatennachverarbeitung
*&  Request No.   : 180116-143713-AR
*&  Developer     : Achim Reichenberger
*&  E-Mail        : achim.reichenberger@also.com
*&  Creation Date : 18.10.2018
*&  Key           : ohne, da Neuanlage
************************************************************************
*&  Description (short)
*&  Dieser Report verarbeitet die Archivbeleg, die in der
*&  Tab. ZAF_ARCHIVE_ERR stehen, nach.
************************************************************************
************************************************************************
*&  Erweiterung TS-190919
*&  Falls die Archiv-Zuordnung noch nicht vorhanden war, kann sie nun
*&  über das Selektionsbild mitgegeben werden. Dazu kann es notwendig
*&  sein, die Belege auszuwählen.
************************************************************************

REPORT zaf_reproduce_archiv_data.

TABLES: toa01.

DATA: g_rf_arc_data    TYPE REF TO zcl_af_core_archive_data,
      g_it_archive_del TYPE STANDARD TABLE OF zaf_archive_err,
      g_it_archive_err TYPE STANDARD TABLE OF zaf_archive_err,
      g_obj_serialized TYPE string,
      g_arc_doc_id     TYPE saeardoid.

*->TS-190919
*----------------------------------------------------------------------*
*   SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK sel_01 WITH FRAME.
SELECT-OPTIONS: s_objid FOR toa01-object_id.
PARAMETERS: p_arcid TYPE toaar-archiv_id,
            p_arcob TYPE toadv-ar_object.
SELECTION-SCREEN END OF BLOCK   sel_01.
*<-TS-190919
*----------------------------------------------------------------------*
*   START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  SELECT * FROM zaf_archive_err INTO TABLE g_it_archive_err
    WHERE object_id IN s_objid.                             "TS-190919

  CHECK lines( g_it_archive_err ) > 0.

  LOOP AT g_it_archive_err ASSIGNING FIELD-SYMBOL(<wa_archive_err>).

    " Instanz "zurückholen" (deserialisieren)
    g_obj_serialized = <wa_archive_err>-arc_data_obj.
    CALL TRANSFORMATION id
             SOURCE XML g_obj_serialized
                 RESULT model = g_rf_arc_data.

*->TS-190919
    IF g_rf_arc_data->archiv_id IS INITIAL AND p_arcid IS NOT INITIAL.
      g_rf_arc_data->archiv_id = p_arcid.
    ENDIF.
    IF g_rf_arc_data->ar_object IS INITIAL AND p_arcob IS NOT INITIAL.
      g_rf_arc_data->ar_object = p_arcob.
    ENDIF.
*<-TS-190919

    CALL FUNCTION 'ARCHIVOBJECT_CREATE_TABLE'
      EXPORTING
        archiv_id                = g_rf_arc_data->archiv_id
        document_type            = g_rf_arc_data->document_type
        length                   = g_rf_arc_data->length
      IMPORTING
        archiv_doc_id            = g_arc_doc_id
      TABLES
        binarchivobject          = g_rf_arc_data->it_pdf
      EXCEPTIONS
        error_archiv             = 1
        error_communicationtable = 2
        error_kernel             = 3
        blocked_by_policy        = 4
        OTHERS                   = 5.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
      EXPORTING
        archiv_id             = g_rf_arc_data->archiv_id
        arc_doc_id            = g_arc_doc_id
        ar_date               = sy-datum
        ar_object             = g_rf_arc_data->ar_object
        object_id             = g_rf_arc_data->object_id
        sap_object            = g_rf_arc_data->sap_object
        doc_type              = g_rf_arc_data->document_type
        mandant               = sy-mandt
        creator               = g_rf_arc_data->uname
      EXCEPTIONS
        error_connectiontable = 1
        OTHERS                = 2.

    CHECK sy-subrc = 0.

    APPEND <wa_archive_err> TO g_it_archive_del.

  ENDLOOP.

  IF lines( g_it_archive_del ) > 0.
    TRY.
        CALL METHOD zcl_actebis_display_data=>display_data_easy
          EXPORTING
            im_display_table = g_it_archive_del
            im_display_modal = 'X'
            im_start_column  = 1
            im_end_column    = 120
            im_start_line    = 1
            im_end_line      = 22.

      CATCH zcx_actebis_display_data.
    ENDTRY.

    DELETE zaf_archive_err FROM TABLE g_it_archive_del.
  ENDIF.

*----------------------------------------------------------------------*
*   END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
