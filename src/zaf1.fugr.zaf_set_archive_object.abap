FUNCTION ZAF_SET_ARCHIVE_OBJECT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_RETURN) TYPE  SYSUBRC
*"  TABLES
*"      IT_DATA STRUCTURE  ZARCHIVE_OBJECT
*"----------------------------------------------------------------------

***********************************************************************
**& Key           : WG-230216
**& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase - Developme
***********************************************************************
**& Description (short)
**& change Adobe Forms  / table "ZAF_ARCIVE_OBJECT"
***********************************************************************

  ev_return = zcl_dev_core_util=>set_table_data( iv_tabname = 'ZARCHIVE_OBJECT' ir_data_imp = REF #( it_data[] ) ).

ENDFUNCTION.
