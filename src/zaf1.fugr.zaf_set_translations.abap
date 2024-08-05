FUNCTION zaf_set_translations .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_RETURN) TYPE  SYSUBRC
*"  TABLES
*"      IT_DATA STRUCTURE  ZAF_GS_TRANSLATIONS
*"----------------------------------------------------------------------

***********************************************************************
**& Key           : WG-230131
**& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase - Developme
***********************************************************************
**& Description (short)
**& change Adobe Forms translations / table "ZAF_TRANSLATIONS"
***********************************************************************

  ev_return = zcl_dev_core_util=>set_table_data( iv_tabname = 'ZAF_TRANSLATIONS' ir_data_imp = REF #( it_data[] ) ).

ENDFUNCTION.
