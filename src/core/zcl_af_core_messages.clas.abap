class ZCL_AF_CORE_MESSAGES definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_msg,
             msgtyp TYPE  bdc_mart,
             msgid  TYPE  bdc_mid,
             msgnr  TYPE  bdc_mnr,
             msgv1  TYPE  bdc_vtext1,
             msgv2  TYPE  bdc_vtext1,
             msgv3  TYPE  bdc_vtext1,
             msgv4  TYPE  bdc_vtext1,
           END OF ts_msg .
  types:
    tt_msg type STANDARD TABLE OF TS_MsG with EMPTY KEY .

  methods ADD_MESSAGE
    importing
      !IS_MSG type TS_MSG .
  methods ADD_MESSAGE_DIRECT
    importing
      !IV_TYP type BDC_MART
      !IV_NR type BDC_MNR
      !IV_ID type BDC_MID
      !IV_V1 type SIMPLE optional
      !IV_V2 type SIMPLE optional
      !IV_V3 type SIMPLE optional
      !IV_V4 type SIMPLE optional .
  methods ADD_NOTE
    importing
      !IV_NOTE type BDC_VTEXT1
      !IV_MSGTYP type BDC_MART default 'S'
      !IV_MSGID type BDC_MID default 'SD'
      !IV_MSGNR type BDC_MNR default '899' .
  methods RESET .
  methods GET_IT_MSG_TAB
    returning
      value(RT_MSG_TAB) type TT_MSG .
  methods ADD_MESSAGES_FROM_OBJECT
    importing
      !IO_MSG type ref to ZCL_AF_CORE_MESSAGES .
  methods GET_MESSAGES
    importing
      !IV_MSGTYPE type TS_MSG-MSGTYP optional
    returning
      value(RT_MSG) type TT_MSG .
  methods ADD_MESSAGES
    importing
      !IT_MSG type TT_MSG .
protected section.
private section.

  data GT_MSG type TT_MSG .
ENDCLASS.



CLASS ZCL_AF_CORE_MESSAGES IMPLEMENTATION.


METHOD add_message.
************************************************************************
*&  Key           : WG_230306
*&  Request No.   : 220929-095431-IA - Integration ECC - EWM - AF Core
************************************************************************
*&  Description (short)
*&  Add Message
************************************************************************

    gt_msg = VALUE #( BASE gt_msg
                    ( msgtyp = is_msg-msgtyp
                      msgid  = is_msg-msgid
                      msgnr  = is_msg-msgnr
                      msgv1  = is_msg-msgv1
                      msgv2  = is_msg-msgv2
                      msgv3  = is_msg-msgv3
                      msgv4  = is_msg-msgv4 ) ).

  ENDMETHOD.


  METHOD add_messages.
************************************************************************
*&  Key           : WG_230306
*&  Request No.   : 220929-095431-IA - Integration ECC - EWM - AF Core
************************************************************************
*&  Description (short)
*&  Add messages
************************************************************************

    DO lines( it_msg ) TIMES.
      add_message( is_msg = it_msg[ sy-index ] ).
    ENDDO.

  ENDMETHOD.


  METHOD add_messages_from_object.
************************************************************************
*&  Key           : WG_230306
*&  Request No.   : 220929-095431-IA - Integration ECC - EWM - AF Core
************************************************************************
*&  Description (short)
*&  Add messages from object
************************************************************************

    DATA: lt_msg TYPE tt_msg.

    CLEAR lt_msg.

    io_msg->get_messages( RECEIVING rt_msg = lt_msg ).

    add_messages( it_msg = lt_msg ).

  ENDMETHOD.


  METHOD add_message_direct.
************************************************************************
*&  Key           : WG_230306
*&  Request No.   : 220929-095431-IA - Integration ECC - EWM - AF Core
************************************************************************
*&  Description (short)
*&  Add Message direct
************************************************************************

    DATA: ls_mes TYPE ts_msg.

    ls_mes-msgtyp  = iv_typ.
    ls_mes-msgnr   = iv_nr.
    ls_mes-msgid   = iv_id.

    TRANSLATE ls_mes-msgtyp TO UPPER CASE.
    TRANSLATE ls_mes-msgid  TO UPPER CASE.

    TRY.
        WRITE iv_v1 TO ls_mes-msgv1 LEFT-JUSTIFIED.
      CATCH cx_root.
    ENDTRY.

    TRY.
        WRITE iv_v2 TO ls_mes-msgv2 LEFT-JUSTIFIED.
      CATCH cx_root.
    ENDTRY.

    TRY.
        WRITE iv_v3 TO ls_mes-msgv3 LEFT-JUSTIFIED.
      CATCH cx_root.
    ENDTRY.

    TRY.
        WRITE iv_v4 TO ls_mes-msgv4 LEFT-JUSTIFIED.
      CATCH cx_root.
    ENDTRY.

    add_message( is_msg = ls_mes ).

  ENDMETHOD.


  METHOD add_note.
************************************************************************
*&  Key           : WG_230306
*&  Request No.   : 220929-095431-IA - Integration ECC - EWM - AF Core
************************************************************************
*&  Description (short)
*&  Add note
************************************************************************
    gt_msg = VALUE #( BASE gt_msg ( msgtyp = iv_msgtyp msgid = iv_msgid msgnr = iv_msgnr msgv1 = iv_note ) ).

  ENDMETHOD.


  METHOD get_it_msg_tab.
************************************************************************
*&  Key           : WG_230306
*&  Request No.   : 220929-095431-IA - Integration ECC - EWM - AF Core
************************************************************************
*&  Description (short)
*&  Get messages table
************************************************************************

    rt_msg_tab = gt_msg.

  ENDMETHOD.


  METHOD get_messages.
************************************************************************
*&  Key           : WG_230306
*&  Request No.   : 220929-095431-IA - Integration ECC - EWM - AF Core
************************************************************************
*&  Description (short)
*&  Get messages
************************************************************************

    IF iv_msgtype IS SUPPLIED.
      rt_msg = VALUE #( FOR ls_msg IN gt_msg WHERE ( msgtyp = iv_msgtype ) ( ls_msg ) ).
    ELSE.
      rt_msg = VALUE #( FOR ls_msg IN gt_msg ( ls_msg ) ).
    ENDIF.

  ENDMETHOD.


  METHOD reset.
************************************************************************
*&  Key           : WG_230306
*&  Request No.   : 220929-095431-IA - Integration ECC - EWM - AF Core
************************************************************************
*&  Description (short)
*&  Reset
************************************************************************

    CLEAR gt_msg.

  ENDMETHOD.
ENDCLASS.
