FUNCTION zaf_set_output_ctrl .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_RETURN) TYPE  SYSUBRC
*"  TABLES
*"      IT_DATA STRUCTURE  ZAF_OUTPUT_CTRL
*"----------------------------------------------------------------------

***********************************************************************
**& Key           : WG-230215
**& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase - Developme
***********************************************************************
**& Description (short)
**& change Adobe Forms output control / table "ZAF_OUTPUT_CTRL"
***********************************************************************

  ev_return = zcl_dev_core_util=>set_table_data( iv_tabname = 'ZAF_OUTPUT_CTRL' ir_data_imp = REF #( it_data[] ) ).

ENDFUNCTION.
