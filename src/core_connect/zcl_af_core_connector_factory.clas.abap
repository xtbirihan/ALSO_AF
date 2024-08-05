class ZCL_AF_CORE_CONNECTOR_FACTORY definition
  public
  final
  create public .

public section.

  data GT_CHANNEL_PARAMS type ZAF_T_CHANNEL_PARAMS .
  data GT_CHANNEL_REF type ZAF_T_CHANNEL_REF .
  data GV_LOG_OBJECT type BALOBJ_D .

  methods CONSTRUCTOR
    importing
      !IV_LOG_OBJECT type BALOBJ_D
      !IT_CHANNEL_PARAMS type ZAF_T_CHANNEL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
  methods GET_OUTPUT_PARAMS
    returning
      value(RS_OUTPUT_PARAMS) type SFPOUTPUTPARAMS .
  methods GET_CHANNELS
    returning
      value(RT_RF_CHANNELS) type ZAF_T_CHANNEL_REF .
protected section.
private section.

  data GS_OUTPUT_PARAMS type SFPOUTPUTPARAMS .

  methods CREATE_CHANNELS
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CREATE_CON_FAX
    importing
      !IS_CHANNEL_PARAMS type ZAF_S_CHANNEL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CREATE_CON_PREVIEW
    importing
      !IS_CHANNEL_PARAMS type ZAF_S_CHANNEL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CREATE_CON_PRINT
    importing
      !IS_CHANNEL_PARAMS type ZAF_S_CHANNEL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CREATE_CON_ARCHIVE
    importing
      !IS_CHANNEL_PARAMS type ZAF_S_CHANNEL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CREATE_CON_PRINT_ZPL
    importing
      !IS_CHANNEL_PARAMS type ZAF_S_CHANNEL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CREATE_CON_PRINT_PS
    importing
      !IS_CHANNEL_PARAMS type ZAF_S_CHANNEL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CREATE_CON_EMAIL_CO
    importing
      !IS_CHANNEL_PARAMS type ZAF_S_CHANNEL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CREATE_CON_FILE_CO
    importing
      !IS_CHANNEL_PARAMS type ZAF_S_CHANNEL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CREATE_CON_OUT_INFO
    importing
      !IS_CHANNEL_PARAMS type ZAF_S_CHANNEL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CREATE_CON_PRINT_PCL
    importing
      !IS_CHANNEL_PARAMS type ZAF_S_CHANNEL_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
ENDCLASS.



CLASS ZCL_AF_CORE_CONNECTOR_FACTORY IMPLEMENTATION.


  METHOD constructor.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*& Konstruktor
*& constructor
************************************************************************

    gv_log_object = iv_log_object.

    zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                   iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_con
                                   iv_ext_ident = '> START - ZCL_AF_CORE_CONNECTOR_FACTORY' ).

    gt_channel_params         = it_channel_params.

    LOOP AT it_channel_params ASSIGNING FIELD-SYMBOL(<ls_channel_params>).
      " DEVICE muss für PDF Generierung immer auf PRINTER stehen
      " DEVICE must always be set to PRINTER for PDF generation
      <ls_channel_params>-wa_outputparams-device   = zcl_af_core_constants=>gc_sap_device_printer.
      " Der Flag muss ich auf FALSE stehe, damit nicht bei der Generierung schon der PREVIEW ausgelöst wird
      " I have to set the flag to FALSE so that the PREVIEW is not triggered during generation
      <ls_channel_params>-wa_outputparams-preview  = abap_false.
      " Es wird hier keinen Dialog mehr geben. Wenn muss man sich vorher eine Druckdialog aufrufen.
      " There will be no more dialogue here. If you have to call up a print dialog beforehand.
      <ls_channel_params>-wa_outputparams-nodialog = abap_true.
      " Always generate PDF
      <ls_channel_params>-wa_outputparams-getpdf   = abap_true.
      " Always generate PDF
      <ls_channel_params>-wa_outputparams-getpdl   = abap_true.
      " Always generate PDF
      <ls_channel_params>-wa_outputparams-getxml   = abap_true.
    ENDLOOP.

    " Connector Instanzen erzeugen
    " Create Connector Instances
    create_channels( ).

  ENDMETHOD.


  METHOD create_channels.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Kanäle erzeugen.
*&
*&  create channels.
************************************************************************

    DATA: lo_msg TYPE REF TO zcl_af_core_messages.
    DATA: ls_msg TYPE zcl_af_core_messages=>ts_msg.

    CREATE OBJECT lo_msg.

    LOOP AT gt_channel_params ASSIGNING FIELD-SYMBOL(<ls_channel_params>).

      CASE <ls_channel_params>-channel_id.
        WHEN zcl_af_core_constants=>gc_output_channel_preview.
          " Druckansicht / Printable version
          create_con_preview( is_channel_params = <ls_channel_params> ).

        WHEN zcl_af_core_constants=>gc_output_channel_print.
          " Druck / printing
          create_con_print( is_channel_params = <ls_channel_params> ).

        WHEN zcl_af_core_constants=>gc_output_channel_print_zpl.
          " ZPL Druck / ZPL printing
          create_con_print_zpl( is_channel_params = <ls_channel_params> ).

        WHEN zcl_af_core_constants=>gc_output_channel_print_ps.
          " Postscript Druck / Postscript Printing
          create_con_print_ps( is_channel_params = <ls_channel_params> ).

        WHEN zcl_af_core_constants=>gc_output_channel_print_pcl.
          " PCL Druck / PCL printing
          create_con_print_pcl( is_channel_params = <ls_channel_params> ).

        WHEN zcl_af_core_constants=>gc_output_channel_mail_co.
          " E-Mail
          create_con_email_co( is_channel_params = <ls_channel_params> ).

        WHEN zcl_af_core_constants=>gc_output_channel_fax.
          " Fax
          create_con_fax( is_channel_params = <ls_channel_params> ).

        WHEN zcl_af_core_constants=>gc_output_channel_archive.
          " Archiv / Archive
          create_con_archive( is_channel_params = <ls_channel_params> ).

        WHEN zcl_af_core_constants=>gc_output_channel_file_co.
          " Dateiablage /  File storage
          create_con_file_co( is_channel_params = <ls_channel_params> ).

        WHEN zcl_af_core_constants=>gc_output_channel_out_info.
          " Out-Info Sicherung / Out-Info Backup
          create_con_out_info( is_channel_params = <ls_channel_params> ).

        WHEN OTHERS.

          ls_msg-msgid = zcl_af_core_constants=>gc_msgid_default.
          ls_msg-msgnr = '107'.
          ls_msg-msgtyp = 'E'.
          lS_msg-msgv1 = <ls_channel_params>-channel_id.

          lo_msg->add_message( is_msg = ls_msg ).

          zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                         iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_con
                                         iv_ext_ident = 'ERROR in CREATE_CHANNELS' ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_con_archive.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Archiv Connector erzeugt
*&  generate Archive Connector
************************************************************************

    DATA: lo_archive_channel TYPE REF TO zcl_af_core_con_archive.
    DATA: ls_channel_ref TYPE zaf_s_channel_ref.
    DATA: lv_channel_params TYPE zaf_channel_sdata.

    lo_archive_channel = NEW zcl_af_core_con_archive( iv_log_object     = gv_log_object
                                                      is_archive_params = is_channel_params-wa_archive_params ).

    ls_channel_ref-object ?= lo_archive_channel.
    ls_channel_ref-params = is_channel_params.

    APPEND ls_channel_ref TO gt_channel_ref.

  ENDMETHOD.


  METHOD create_con_email_co.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Unternehmens E-Mail Kanal erzeugt
*&  Company Email Channel Generated
************************************************************************

* If it is needed, this connector should be implemented. Can be copied
* from the TA1 system.

*    DATA: lo_email_channel TYPE REF TO zcl_af_core_con_email_co.
*    DATA: ls_channel_ref TYPE zaf_s_channel_ref.
*
*    lo_email_channel = NEW zcl_af_core_con_email_co( iv_log_object        = gv_log_object
*                                                     is_co_mail_params = is_channel_params-wa_mail_co_params ).
*
*    ls_channel_ref-object ?= lo_email_channel.
*    ls_channel_ref-params = is_channel_params.
*
*    APPEND ls_channel_ref TO gt_channel_ref.

  ENDMETHOD.


  METHOD create_con_fax.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Fax Kanal erzeugt
*&
*&  Create a Fax channel
************************************************************************

* If it is needed, this connector should be implemented. Can be copied
* from the TA1 system.

*    DATA: lo_fax_channel TYPE REF TO zcl_af_core_con_fax.
*    DATA: ls_channel_ref TYPE zaf_s_channel_ref.
*
*    lo_fax_channel = NEW zcl_af_core_con_fax( iv_log_object     = gv_log_object
*                                              is_telefax_params = is_channel_params-wa_telefax_params ).
*
*    ls_channel_ref-object ?= lo_fax_channel.
*    ls_channel_ref-params = is_channel_params.
*
*    APPEND ls_channel_ref TO gt_channel_ref.

  ENDMETHOD.


  METHOD create_con_file_co.
************************************************************************
*&  Key           : AR-180706
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Unternehmens Dateiablage Kanal erzeugt
*&  generate file Channel
************************************************************************

    DATA: lo_file_channel TYPE REF TO zcl_af_core_con_file_co.
    DATA: ls_channel_ref  TYPE zaf_s_channel_ref.

    lo_file_channel = NEW zcl_af_core_con_file_co( iv_log_object     = gv_log_object
                                                   is_file_co_params = is_channel_params-wa_file_co_params ).

    ls_channel_ref-object ?= lo_file_channel.
    ls_channel_ref-params = is_channel_params.

    APPEND ls_channel_ref TO gt_channel_ref.

  ENDMETHOD.


  METHOD create_con_out_info.
************************************************************************
*&  Key           : TS-180725
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Sicherung der Ausgabeinformationen
*&  Saving Output Information
************************************************************************

* If it is needed, this connector should be implemented. Can be copied
* from the TA1 system.

*    DATA: lo_out_info_channel TYPE REF TO zcl_af_core_con_out_info.
*    DATA: ls_channel_ref  TYPE zaf_s_channel_ref.
*
*    lo_out_info_channel = NEW zcl_af_core_con_out_info( iv_log_object         = gv_log_object
*                                                        is_out_info_params = is_channel_params-wa_out_info_params ).
*
*    ls_channel_ref-object ?= lo_out_info_channel.
*    ls_channel_ref-params = is_channel_params.
*
*    APPEND ls_channel_ref TO gt_channel_ref.

  ENDMETHOD.


  METHOD create_con_preview.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Druckansicht Kanal erzeugt
*&  create a preview channel
************************************************************************

    DATA: lo_preview_channel TYPE REF TO zcl_af_core_con_preview.
    DATA: ls_channel_ref TYPE zaf_s_channel_ref.

    lo_preview_channel = NEW zcl_af_core_con_preview( iv_log_object = gv_log_object ).

    ls_channel_ref-object ?= lo_preview_channel.
    ls_channel_ref-params = is_channel_params.

    APPEND ls_channel_ref TO gt_channel_ref.

  ENDMETHOD.


  METHOD create_con_print.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Druck Kanal erzeugt
*&  generate print channel
************************************************************************

    DATA: lo_print_channel TYPE REF TO zcl_af_core_con_print_pdf,
          ls_channel_ref   TYPE zaf_s_channel_ref.

    lo_print_channel = NEW zcl_af_core_con_print_pdf( iv_log_object          = gv_log_object
                                                      is_print_pdf_params = is_channel_params-wa_print_pdf_params ).

    ls_channel_ref-object ?= lo_print_channel.
    ls_channel_ref-params = is_channel_params.

    APPEND ls_channel_ref TO gt_channel_ref.

  ENDMETHOD.


  METHOD create_con_print_pcl.
************************************************************************
*&  Key           : TS-191216
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Druck Kanal erzeugt
*&  generate print channel
************************************************************************

    DATA: lo_print_channel TYPE REF TO zcl_af_core_con_print_pdl.
    DATA: ls_channel_ref   TYPE zaf_s_channel_ref.

    lo_print_channel = NEW zcl_af_core_con_print_pdl( iv_log_object          = gv_log_object
                                                      is_print_pdl_params = is_channel_params-wa_print_pdl_params ).

    ls_channel_ref-object ?= lo_print_channel.
    ls_channel_ref-params = is_channel_params.

    APPEND ls_channel_ref TO gt_channel_ref.

  ENDMETHOD.


  METHOD create_con_print_ps.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Druck Kanal erzeugt
*&  generate print channel
************************************************************************

    DATA: lo_print_channel TYPE REF TO zcl_af_core_con_print_pdl.
    DATA: ls_channel_ref   TYPE zaf_s_channel_ref.

    lo_print_channel = NEW zcl_af_core_con_print_pdl( iv_log_object       = gv_log_object
                                                      is_print_pdl_params = is_channel_params-wa_print_pdl_params ).

    ls_channel_ref-object ?= lo_print_channel.
    ls_channel_ref-params = is_channel_params.

    APPEND ls_channel_ref TO gt_channel_ref.

  ENDMETHOD.


  METHOD create_con_print_zpl.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Druck Kanal erzeugt
*&  generate print channel
************************************************************************

    DATA: lo_print_channel TYPE REF TO zcl_af_core_con_print_zpl.
    DATA: ls_channel_ref   TYPE zaf_s_channel_ref.

    lo_print_channel = NEW zcl_af_core_con_print_zpl( iv_log_object       = gv_log_object
                                                      is_print_zpl_params = is_channel_params-wa_print_zpl_params ).

    ls_channel_ref-object ?= lo_print_channel.
    ls_channel_ref-params = is_channel_params.

    APPEND ls_channel_ref TO gt_channel_ref.

  ENDMETHOD.


  METHOD GET_CHANNELS.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Bestellte Kanäle ermitteln.
*&
*& Determine ordered channels.
************************************************************************

    rt_rf_channels = gt_channel_ref.

  ENDMETHOD.


  METHOD get_output_params.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Ausgabeparameter ermitteln.
*&
*& Determine output parameters.
************************************************************************

    rs_output_params = gs_output_params.

  ENDMETHOD.
ENDCLASS.
