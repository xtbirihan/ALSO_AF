FUNCTION ZAF_UNL_SHIP_PRINT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_VKORG) TYPE  VKORG DEFAULT '1010'
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM DEFAULT 'DE50'
*"     REFERENCE(IT_HU) TYPE  ZCL_AF_UNL_SHIP=>TT_HU
*"     REFERENCE(IV_REF) TYPE  ZCL_AF_UNL_SHIP=>TV_REF
*"     REFERENCE(IV_DATE) TYPE  SYDATUM DEFAULT SY-DATUM
*"     REFERENCE(IV_TIME) TYPE  SYUZEIT DEFAULT SY-UZEIT
*"     REFERENCE(IV_USER) TYPE  SYUNAME DEFAULT SY-UNAME
*"     REFERENCE(IV_LANE) TYPE  ZCL_AF_UNL_SHIP=>TV_LANE
*"     REFERENCE(IV_IS) TYPE  ZCL_AF_UNL_SHIP=>TV_IS
*"     REFERENCE(IV_LAST_HU) TYPE  BOOLEAN DEFAULT ABAP_OFF
*"     REFERENCE(IV_PREVIEW) TYPE  BOOLEAN DEFAULT ABAP_OFF
*"  EXPORTING
*"     REFERENCE(EV_SUBRC) TYPE  SYSUBRC
*"----------------------------------------------------------------------

**********************************************************************
*& Key           : WG-230207
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase - Development
**********************************************************************
*& Description (short)
*& unload shipment / print label / Adobe Forms
************************************************************************

  ev_subrc = zcl_af_unl_ship=>main( iv_vkorg   = iv_vkorg
                                    iv_lgnum   = iv_lgnum
                                    it_hu      = it_hu
                                    iv_ref     = iv_ref
                                    iv_lane    = iv_lane
                                    iv_is      = iv_is
                                    iv_last_hu = iv_last_hu
                                    iv_preview = iv_preview ).

ENDFUNCTION.
