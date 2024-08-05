class ZCX_AF_CORE_OUTPUT definition
  public
  inheriting from ZCX_AF_CORE_EXCEPTION
  create private .

public section.

  constants:
*  interfaces IF_T100_MESSAGE .
    begin of FORM_NOT_FOUND,
      msgid type symsgid value 'ZAF_CORE',
      msgno type symsgno value '301',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of FORM_NOT_FOUND .
  constants CONNECTOR_NOT_FOUND type SOTR_CONC value '2E2DCE645E851EE8998ABB00038EC803' ##NO_TEXT.
  constants:
    begin of NO_VKORG,
      msgid type symsgid value 'ZAF_CORE',
      msgno type symsgno value '303',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_VKORG .
  constants:
    begin of MAIL_SEND_ERROR,
      msgid type symsgid value 'ZAF_CORE',
      msgno type symsgno value '304',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MAIL_SEND_ERROR .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV1 optional
      !MSGV2 type SYMSGV2 optional
      !MSGV3 type SYMSGV3 optional
      !MSGV4 type SYMSGV4 optional .
  class-methods RAISE
    importing
      !IV_MSGID type MSGID optional
      !IV_MSGNO type MSGNR optional
      !IV_MSGTY type MSGTY optional
      !IV_MSGV1 type SYMSGV1 optional
      !IV_MSGV2 type SYMSGV2 optional
      !IV_MSGV3 type SYMSGV3 optional
      !IV_MSGV4 type SYMSGV4 optional
      !IV_EVENTID type BTCEVENTID optional
      !IV_PARAMETER type BTCEVTPARM optional
      !IV_PREVIOUS like PREVIOUS optional
      !IV_LOG_OBJECT type BALOBJ_D
      !IV_TEXTID type SOTR_CONC optional
      !IS_T100KEY type SCX_T100KEY optional
    raising
      ZCX_AF_CORE_OUTPUT .
  class-methods RAISE_EVENT
    importing
      !IV_EVENTID type BTCEVENTID
      !IV_PARAMETER type BTCEVTPARM
      !IV_LOG_OBJECT type BALOBJ_D
    exceptions
      ZCX_AF_CORE_OUTPUT .
protected section.
private section.
ENDCLASS.



CLASS ZCX_AF_CORE_OUTPUT IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD raise.
************************************************************************
*&  Key           : AR-180406
*&  Request No.   : 180116-143713-AR - Adobe Forms CORE Entwicklung
************************************************************************
*&  Description (short)
*&  Ausnahmebehandlungsmethode, erzeugt eine Instanz von sich selbst.
*&  Schreibt die übergebene Nachricht in den Logger, falls einer
*&  übergeben wurde.
*&
*& exception handling method, creates an instance of itself.
*& Writes the passed message to the logger if a
*& was handed over.
************************************************************************
    DATA: lo_exception TYPE REF TO zcx_af_core_output,
          ls_t100key   TYPE scx_t100key,
          ls_msg       TYPE zcl_af_core_messages=>ts_msg.

    DATA(lo_msg) = NEW zcl_af_core_messages( ).

    " Message Struktur füllen
    IF is_t100key IS NOT INITIAL.
      ls_t100key = is_t100key.
    ELSE.
      ls_t100key-msgid = iv_msgid.
      ls_t100key-msgno = iv_msgno.
      ls_t100key-attr1 = iv_msgv1.
      ls_t100key-attr2 = iv_msgv2.
      ls_t100key-attr3 = iv_msgv3.
      ls_t100key-attr4 = iv_msgv4.
    ENDIF.

    lo_exception = NEW zcx_af_core_output( textid   = ls_t100key
                                           previous = iv_previous
                                           msgv1    = iv_msgv1
                                           msgv2    = iv_msgv2
                                           msgv3    = iv_msgv3
                                           msgv4    = iv_msgv4 ).

    ls_msg-msgtyp = iv_msgty.
    ls_msg-msgid = iv_msgid.
    ls_msg-msgnr = iv_msgno.
    ls_msg-msgv1 = iv_msgv1.
    ls_msg-msgv2 = iv_msgv2.
    ls_msg-msgv3 = iv_msgv3.
    ls_msg-msgv4 = iv_msgv4.

    lo_msg->add_message( is_msg = ls_msg ).

    zcl_af_core_logger=>write_log( iv_object    = iv_log_object
                                   iv_subobject = zcl_af_core_constants=>gc_log_subobj_core_output
                                   iv_ext_ident = '>>> ZCX_AF_CORE_OUTPUT->RAISE:  Siehe Meldung! <<<'
                                   io_msg       = lo_msg ).

    IF iv_eventid IS SUPPLIED AND iv_eventid IS NOT INITIAL.
      TRY .
          zcx_af_core_output=>raise_event( iv_eventid    = iv_eventid
                                           iv_parameter  = iv_parameter
                                           iv_log_object = iv_log_object ).
        CATCH zcx_af_core_output.

      ENDTRY.
    ENDIF.

    RAISE EXCEPTION lo_exception.

  ENDMETHOD.


  METHOD raise_event.

    CALL FUNCTION 'BP_EVENT_RAISE'
      EXPORTING
        eventid                = iv_eventid
        eventparm              = iv_parameter
*       TARGET_INSTANCE        = ' '
*       TARGET_MODE            = ' '
      EXCEPTIONS
        bad_eventid            = 1
        eventid_does_not_exist = 2
        eventid_missing        = 3
        raise_failed           = 4
        OTHERS                 = 5.
    IF sy-subrc <> 0.
      zcx_af_core_output=>raise(
        EXPORTING
          iv_msgno           = '002'
          iv_msgty           = 'W'
          iv_msgv1           = CONV #( iv_eventid )
          iv_log_object      = iv_log_object
      ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
