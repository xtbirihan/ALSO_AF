class ZCL_AF_CORE_CONTROLLER definition
  public
  abstract
  create protected .

public section.

  types:
    BEGIN OF ts_delivery_set,
           form_output TYPE fpformoutput,
           channel_ref TYPE zaf_s_channel_ref,
         END OF ts_delivery_set .
  types:
    TT_DELIVERY_SET type table of ts_delivery_set .

  data GV_LOG_OBJECT type BALOBJ_D .

  methods PROCESS
    raising
      ZCX_AF_CORE_OUTPUT .
protected section.

  data GO_OUTPUT type ref to ZCL_AF_CORE_OUTPUT .
  data GS_CORE_PARAMS type ZAF_S_CORE_PARAMS .
  data GV_SPEC_OUTPUT_CLASS_NAME type STRING .
  data GV_SPEC_FORM_NAME type STRING .
  data GV_VKORG type VKORG .

  methods CONSTRUCTOR
    importing
      !IV_LOG_OBJECT type BALOBJ_D
      !IS_DOCPARAMS type SFPDOCPARAMS
      !IS_OUTPUTPARAMS type SFPOUTPUTPARAMS optional
      !IS_COMM_VALUES type ZAF_S_COMM_VALUES optional
      !IV_VKORG type VKORG
      !IS_EXTENDED_PARAMS type ZAF_S_EXTENDED_PARAMS
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CREATE_CORE_PARAMS .
  methods CREATE_OUTPUT_CLASS
  abstract
    exporting
      !EO_SPEC_OUTPUT_CLASS type ref to ZCL_AF_CORE_OUTPUT
    raising
      ZCX_AF_CORE_OUTPUT .
  methods CREATE_CHANNEL_REFS
  final
    exporting
      !ET_CHANNEL_REFS type ZAF_T_CHANNEL_REF
    raising
      ZCX_AF_CORE_OUTPUT .
private section.

  methods DELIVER_ALL
    importing
      !IT_DELIVERY_SETS type TT_DELIVERY_SET
    raising
      ZCX_AF_CORE_OUTPUT .
  methods GENERATE_ALL
    importing
      !IO_OUTPUT type ref to ZCL_AF_CORE_OUTPUT
      !IT_CHANNEL_REFS type ZAF_T_CHANNEL_REF
      !IS_DOCPARAMS type SFPDOCPARAMS
    exporting
      !ET_DELIVERY_SETS type TT_DELIVERY_SET
    raising
      ZCX_AF_CORE_OUTPUT .
  methods IS_ORG_DATA_UNIQUE
    importing
      !IT_CHANNEL_REFS type ZAF_T_CHANNEL_REF
    returning
      value(RV_IS_UNIQUE) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_AF_CORE_CONTROLLER IMPLEMENTATION.


METHOD constructor.
************************************************************************
*&  Key           : AR-180606
*&  Request No.   : 180116-135917-AR - Formularentwicklung Service Label
************************************************************************
*&  Description (short)
*&  Instanziiert die Controllerklasse
*&  Instantiates the controller class
************************************************************************

  gv_log_object = iv_log_object.

  zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                 iv_subobject = zcl_af_core_constants=>gc_log_subobj_ctrl
                                 iv_ext_ident = '> START - ZCL_AF_CORE_CONTROLLER' ).

  " Typen für die Kommunikationsstrategien speichern
  " Storing Types for the Communication Strategies
  gs_core_params-wa_comm_values     = is_comm_values.
  gs_core_params-wa_extended_params = is_extended_params.
  " DOCPARAMS speichern
  " Save DOCPARAMS
  gs_core_params-wa_docparams       = is_docparams.

  " VKORG speichern
  " Save VKORG
  IF iv_vkorg IS INITIAL.

    zcx_af_core_output=>raise( iv_log_object = gv_log_object
                               iv_msgno      = '303'
                               iv_msgty      = 'E' ).

  ELSE.
    gv_vkorg = iv_vkorg.
  ENDIF.

  gs_core_params-wa_outputparams = is_outputparams.

ENDMETHOD.


  METHOD create_channel_refs.
************************************************************************
*&  Key           : JR-180530
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Beschafft sich die gewünschten Ausgabekanäle von der spez. Output
*&  Klasse und bestellt die Connectoren bei der Factory.
*&
*& Procures the desired output channels from the spec. Output
*& class and orders the connectors from the factory.
************************************************************************
    DATA: lt_channel_params TYPE zaf_t_channel_params,
          lo_exception      TYPE REF TO zcx_af_core_output.

    FIELD-SYMBOLS: <ls_channel_param> TYPE zaf_s_channel_params.

    TRY.
        " Connector Parametrisierung laden
        " Load Connector Parameterization
        go_output->create_channel_params( IMPORTING et_channel_params = lt_channel_params ).

        " Für jeden Ausgabekanal die Org.Data Values generieren.
        " Darf erst hier passieren, da die Org. Data Values von dem
        " gewählten Connector abhängig sind.
        "
        " Generate the Org.Data Values for each output channel.
        " May only happen here, because the org. Data Values of the
        " are dependent on the selected connector.
        LOOP AT lt_channel_params  ASSIGNING <ls_channel_param>.
          go_output->load_org_data_values( EXPORTING iv_channel         = <ls_channel_param>-channel_id
                                           RECEIVING rs_org_data_values = <ls_channel_param>-wa_org_data_values ).

          zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                         iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                         iv_ext_ident = '> ENDE - ZCL_AF_CORE_ORG_DATA' ).

        ENDLOOP.

        " Factory für Connectoren erstellen
        " Create a factory for connectors
        DATA(lo_con_factory) = NEW zcl_af_core_connector_factory( iv_log_object        = gv_log_object
                                                                  "im_wa_output_params  = me->wa_core_params-wa_outputparams
                                                                  it_channel_params = lt_channel_params ).

        " Referenzen zu Connectoren holen
        " Get references to connectors
        lo_con_factory->get_channels( RECEIVING  rt_rf_channels = et_channel_refs ).

        " Geänderte Outputparameter beschaffen
        " Obtaining Changed Output Parameters
        lo_con_factory->get_output_params( RECEIVING rs_output_params = gs_core_params-wa_outputparams ).

      CATCH zcx_af_core_output INTO lo_exception.

        RAISE EXCEPTION lo_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD create_core_params.
************************************************************************
*&  Key           : JR-180530
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Stellt die CORE Parameter für die Output-Klasse zusammen.
*&  Compiles the CORE parameters for the output class.
************************************************************************

    gs_core_params-output_date     = sy-datum.
    gs_core_params-output_sysid    = sy-sysid.
    gs_core_params-output_tcode    = sy-tcode.
    gs_core_params-output_uname    = sy-uname.
    gs_core_params-langu           = gs_core_params-wa_docparams-langu.
    gs_core_params-vkorg           = gv_vkorg.

  ENDMETHOD.


  METHOD deliver_all.
************************************************************************
*&  Key           : JR-180523
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Liefert die übergebenen Delivery Sets (Connector+Formularerzeugnis)
*&  aus.
*&
*& Delivers the delivered delivery sets (connector+form product)
*& off.
************************************************************************
    FIELD-SYMBOLS: <ls_delivery_set> TYPE ts_delivery_set.

    " Delivery Sets abarbeiten
    " Delivery Sets.
    LOOP AT it_delivery_sets ASSIGNING <ls_delivery_set>.
      TRY.
          " Formular an Connector übergeben
          <ls_delivery_set>-channel_ref-object->deliver( is_formoutput = <ls_delivery_set>-form_output ).
        CATCH zcx_af_core_output.
          " Wenn hier ein Fehler abgefangen wird, wurde
          " der Fehler bereits geloggt und vom Connector verarbeitet.
          " Hier kann die Entscheidung gefällt werden was passieren soll,
          " wenn einer von X Connectoren fehlschlägt.
          "
          " If an error is caught here,
          " the error has already been logged and processed by the connector.
          " Here the decision can be made what should happen,
          " if one of X connectors fails.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD generate_all.
************************************************************************
*&  Key           : JR-180523
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Liefert das übergebene Formular mit den übergebenen Connectoren aus.
*&  Mit _SFP_ benamte Parameter kennzeichnen die SAP Standard
*&  Strukturen. Alle anderen Daten werden aus der Output Klasse gezogen.
*&
*& Delivers the passed form with the passed connectors.
*& Parameters named _SFP_ identify the SAP standard
*& structures. All other data is pulled from the output class.
************************************************************************
    DATA: lv_is_unique    TYPE abap_bool,
          ls_formoutput   TYPE fpformoutput,
          ls_outputparams TYPE sfpoutputparams,
          ls_delivery_set TYPE ts_delivery_set.

    FIELD-SYMBOLS: <ls_channel>      TYPE zaf_s_channel_ref,
                   <ls_delivery_set> TYPE ts_delivery_set.

    " Prüfen, ob alle Org. Daten gleich sind
    " Check if all org data is the same
    is_org_data_unique( EXPORTING it_channel_refs = it_channel_refs
                        RECEIVING rv_is_unique    = lv_is_unique ).

    LOOP AT it_channel_refs ASSIGNING <ls_channel>.
      " Bei erstem Durchlauf immer das Formular erzeugen.
      " Im zweiten und folgenden Durchläufen nur das Formular neu
      " erzeugen, falls die Connectoren unterschiedliche Formular-
      " Ausgaben brauchen (bezüglich Org. Daten).
      "
      " Always create the form on the first pass.
      " In the second and subsequent passes, only the form is new
      " if the connectors have different form
      " Need expenses (regarding org. data).
      IF lv_is_unique = abap_false
        OR sy-tabix = '1'.

        " Setzt die Org. Data Values pro Connector neu, damit ggf.
        " Logos usw. übersteuert werden können.
        "
        " Resets the Org. Data Values per Connector so that if necessary.
        " Logos, etc. can be overridden.
        io_output->set_org_data_values( is_org_data_values = <ls_channel>-params-wa_org_data_values ).

        ls_outputparams = <ls_channel>-params-wa_outputparams.

        " Formular generieren
        " Generate Form
        io_output->generate_form( EXPORTING is_docparams    = is_docparams
                                            is_outputparams = ls_outputparams
                                  IMPORTING es_formoutput   = ls_formoutput ).

      ENDIF.

      " Formular-Erzeugnis mit Connector speichern, damit
      " jeder Connector eine 1:1 Zuordnung zu einem fertig generierten
      " Formular hat.
      "
      " Save form product with connector so that
      " each connector has a 1:1 assignment to a ready-generated
      " form.
      ls_delivery_set-channel_ref = <ls_channel>.
      ls_delivery_set-form_output = ls_formoutput.
      APPEND ls_delivery_set TO et_delivery_sets.

    ENDLOOP.

  ENDMETHOD.


  METHOD is_org_data_unique.
************************************************************************
*&  Key           : JR-180523
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Prüft ob die Org. Daten aller übergebenen Connectoren gleich sind.
*&  Das kann passieren, wenn zum Beispiel ein Connector kein Logo will,
*&  ein anderer allerdings eins haben will.
*&
*& Checks whether the org data of all transferred connectors is the same.
*& This can happen if, for example, a connector does not want a logo,
*& another, however, wants one.
************************************************************************
    DATA: ls_org_data_prev     TYPE zaf_s_org_data_values,
          ls_outputparams_prev TYPE sfpoutputparams.

    " Annehmen, dass alle gleich sind
    " Assume that everyone is equal
    rv_is_unique = abap_true.

    LOOP AT it_channel_refs ASSIGNING FIELD-SYMBOL(<ls_channel>).
      IF ls_org_data_prev IS NOT INITIAL AND
        ( ls_org_data_prev-footer NE <ls_channel>-params-wa_org_data_values-footer OR
          ls_org_data_prev-sender NE <ls_channel>-params-wa_org_data_values-sender OR
          ls_org_data_prev-logo   NE <ls_channel>-params-wa_org_data_values-logo ).

        " Wenn irgendein Parameter nicht gleich ist, Flag setzen und abbrechen
        " If any parameter is not the same, set flag and cancel
        rv_is_unique = abap_false.
        EXIT.
      ENDIF.

      " Wert für Vergleich in nächstem Durchgang speichern
      " Save value for comparison in next run
      ls_org_data_prev = <ls_channel>-params-wa_org_data_values.
    ENDLOOP.

    CHECK rv_is_unique = abap_true.

    LOOP AT it_channel_refs ASSIGNING <ls_channel>.
      IF ls_outputparams_prev IS NOT INITIAL AND
       ( ls_outputparams_prev-dest NE <ls_channel>-params-wa_outputparams-dest ).

        " Wenn irgendein Parameter nicht gleich ist, Flag setzen und abbrechen
        "If any parameter is not equal, set flag and cancel
        rv_is_unique = abap_false.
        EXIT.
      ENDIF.

      " Wert für Vergleich in nächstem Durchgang speichern
      " Save value for comparison in next run
      ls_outputparams_prev = <ls_channel>-params-wa_outputparams.
    ENDLOOP.

  ENDMETHOD.


  METHOD process.
************************************************************************
*&  Key           : JR-180530
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Beschafft den Formularnamen, Output-Klassennamen, erzeugt eine
*&  Instanz der Output-Klasse und liefert diese aus.
*&  Die Methode ist generisch, kann aber bei einem hohen Grad von
*&  Spezialisierung redefiniert werden.
*&
*&  Obtains the form name, output class name, creates a
*&  instance of the output class and delivers it.
*&  The method is generic, but can be used at a high degree of
*&  specialization.
************************************************************************
    DATA: lt_channel_refs TYPE zaf_t_channel_ref,
          lt_delivery_set TYPE tt_delivery_set,
          lo_exception    TYPE REF TO zcx_af_core_output.

    TRY .
**********************************************************************
* 1. Vorbereitungen
* 1. Preparations
**********************************************************************
        zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                       iv_subobject = zcl_af_core_constants=>gc_log_subobj_ctrl
                                       iv_ext_ident = '>> Controller->Prozess 1. Vorbereitungen' ).

        " CORE Parameter erstellen
        " Create CORE parematers
        create_core_params( ).

**********************************************************************
* 2. Output Klasse erzeugen
* 2. Create output class
**********************************************************************
        zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                       iv_subobject = zcl_af_core_constants=>gc_log_subobj_ctrl
                                       iv_ext_ident = '>> Controller->Prozess 2. Output Klasse erzeugen' ).

        " Outputklasse instanziieren
        " Instantiate Output Class
        create_output_class( IMPORTING eo_spec_output_class = go_output ).

**********************************************************************
* 3. Connectoren erzeugen
* 3. Create connectors
**********************************************************************
        zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                       iv_subobject = zcl_af_core_constants=>gc_log_subobj_ctrl
                                       iv_ext_ident = '>> Controller->Prozess 3. Connectoren erzeugen' ).

        " Auslieferung starten
        create_channel_refs( IMPORTING et_channel_refs = lt_channel_refs ).

**********************************************************************
* 4. Generierung
* 4. Generation
**********************************************************************
        zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                       iv_subobject = zcl_af_core_constants=>gc_log_subobj_ctrl
                                       iv_ext_ident = '>> Controller->Prozess 4. Generierung' ).

        " Delivery Sets generieren
        " Generate Delivery Sets
        generate_all( EXPORTING io_output         = go_output
                                it_channel_refs   = lt_channel_refs
                                is_docparams      = gs_core_params-wa_docparams
                      IMPORTING et_delivery_sets  = lt_delivery_set ).

**********************************************************************
* 5. Auslieferung
* 5. Delivery
**********************************************************************
        zcl_af_core_logger=>write_log( iv_object    = gv_log_object
                                       iv_subobject = zcl_af_core_constants=>gc_log_subobj_ctrl
                                       iv_ext_ident = '>> Controller->Prozess 5. Auslieferung' ).

        " Delivery Sets ausliefern
        " Deliver Delivery Sets
        deliver_all( it_delivery_sets = lt_delivery_set ).

**********************************************************************
* 6. Fehlerbehandlung
* 6. Error handling
**********************************************************************
      CATCH zcx_af_core_output INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
