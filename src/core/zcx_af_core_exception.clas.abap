class ZCX_AF_CORE_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  aliases T100KEY
    for IF_T100_MESSAGE~T100KEY .

  constants:
    begin of ZCX_AF_CORE_EXCEPTION,
      msgid type symsgid value 'ZAF_CORE',
      msgno type symsgno value '501',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_AF_CORE_EXCEPTION .
  constants:
    begin of CONDITION_EXISTS,
      msgid type symsgid value 'ZAF_CORE',
      msgno type symsgno value '502',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of CONDITION_EXISTS .
  constants:
    begin of OPEN_DEV,
      msgid type symsgid value 'ZAF_CORE',
      msgno type symsgno value '503',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of OPEN_DEV .
  constants:
    begin of UNKNOWN_ERROR,
      msgid type symsgid value 'ZAF_CORE',
      msgno type symsgno value '504',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of UNKNOWN_ERROR .
  data MSGV1 type SYMSGV1 .
  data MSGV2 type SYMSGV2 .
  data MSGV3 type SYMSGV3 .
  data MSGV4 type SYMSGV4 .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV1 optional
      !MSGV2 type SYMSGV2 optional
      !MSGV3 type SYMSGV3 optional
      !MSGV4 type SYMSGV4 optional .
  methods ADD_ERROR_TO_RF_MSG
    importing
      !IO_MSG type ref to ZCL_AF_CORE_MESSAGES
      !IV_MSG_TYP type ZCL_AF_CORE_MESSAGES=>TS_MSG-MSGTYP .
  methods WRITE_MSG
    importing
      !IV_MSG_TYP type ZCL_AF_CORE_MESSAGES=>TS_MSG-MSGTYP .
protected section.
private section.
ENDCLASS.



CLASS ZCX_AF_CORE_EXCEPTION IMPLEMENTATION.


  METHOD add_error_to_rf_msg.

    TYPES: BEGIN OF ts_msg,
             msgv1 TYPE char50,
             msgv2 TYPE char50,
             msgv3 TYPE char50,
             msgv4 TYPE char50,
           END OF ts_msg.

    DATA: lv_short_txt TYPE string.
    DATA: ls_msg      TYPE ts_msg.

    lv_short_txt = get_text( ).

    MOVE lv_short_txt TO ls_msg.

    io_msg->add_message_direct( iv_typ = iv_msg_typ
                                iv_nr  = '000'
                                iv_id  = 'oo'
                                iv_v1  = ls_msg-msgv1
                                iv_v2  = ls_msg-msgv2
                                iv_v3  = ls_msg-msgv3
                                iv_v4  = ls_msg-msgv4 ).

  ENDMETHOD.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_AF_CORE_EXCEPTION .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD write_msg.

    DATA: lv_short_txt TYPE string.

    lv_short_txt = get_text( ).

    MESSAGE lv_short_txt TYPE iv_msg_typ.

  ENDMETHOD.
ENDCLASS.
