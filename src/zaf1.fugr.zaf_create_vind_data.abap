FUNCTION zaf_create_vind_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_DATA) LIKE  ZAF_RFC_VIND STRUCTURE  ZAF_RFC_VIND
*"  EXPORTING
*"     VALUE(RV_RETURN) TYPE  SY-SUBRC
*"----------------------------------------------------------------------

  rv_return = 99.

  DATA lt_return TYPE zcl_af_vind=>tt_return.

  zcl_af_vind=>ins_vind_data( EXPORTING is_data = is_data CHANGING ct_return = lt_return ).

  DATA(lo_vind) = NEW zcl_af_vind( is_data-kunnr ).

  CHECK lo_vind IS BOUND.
  lo_vind->create_delivery( CHANGING ct_return = lt_return ).
  rv_return = lo_vind->mv_return.

*  cl_demo_output=>display( lt_return ).


ENDFUNCTION.
