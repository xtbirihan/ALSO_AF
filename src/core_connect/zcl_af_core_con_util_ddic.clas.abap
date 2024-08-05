class ZCL_AF_CORE_CON_UTIL_DDIC definition
  public
  final
  create public .

public section.

  class-methods GET_DDIC_INFO
    importing
      !IR_ITAB type ref to DATA
    returning
      value(RT_DDIC) type DDFIELDS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AF_CORE_CON_UTIL_DDIC IMPLEMENTATION.


  METHOD get_ddic_info.
**********************************************************************
*& Key           : WG-230217
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase - Developme
**********************************************************************
*& Description (short)
*& Obtaining DDIC information via the international table
**********************************************************************

    DATA: ref_root  TYPE REF TO cl_abap_typedescr,
          ref_table TYPE REF TO cl_abap_tabledescr,
          ref_data  TYPE REF TO cl_abap_datadescr,
          ref_struc TYPE REF TO cl_abap_structdescr,
          l_it_fcat TYPE lvc_t_fcat.

    DATA: l_objname TYPE string.

    FIELD-SYMBOLS: <l_it_data> TYPE STANDARD TABLE.

* Tabellenbeschreibungsobjekt erzeugen:
    ref_root = cl_abap_tabledescr=>describe_by_data_ref(
                  ir_itab ).
    ref_table ?= ref_root.

* Name des Tabellentyps holen
    ref_data = ref_table->get_table_line_type( ).
    l_objname = ref_data->get_relative_name( ).

    IF l_objname IS NOT INITIAL.
      CALL METHOD cl_abap_structdescr=>describe_by_name
        EXPORTING
          p_name         = l_objname
        RECEIVING
          p_descr_ref    = ref_root
        EXCEPTIONS
          type_not_found = 1
          OTHERS         = 2.
      IF sy-subrc = 0.
        ref_struc ?= ref_root.
        rt_ddic = ref_struc->get_ddic_field_list( ).
      ELSE.
        CLEAR l_objname.
      ENDIF.
    ENDIF.

    IF l_objname IS INITIAL.
      ASSIGN ir_itab->* TO <l_it_data>.
      IF <l_it_data> IS ASSIGNED.
        CLEAR l_it_fcat.
        TRY.
            cl_salv_table=>factory( IMPORTING r_salv_table   = DATA(salv_table)
                                    CHANGING  t_table        = <l_it_data> ).
            l_it_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                r_columns      = salv_table->get_columns( )         " ALV Filter
                r_aggregations = salv_table->get_aggregations( ) ). " ALV Aggregations
            rt_ddic = VALUE #( FOR wa_fcat IN l_it_fcat ( CORRESPONDING #( wa_fcat ) ) ).
          CATCH cx_root.
        ENDTRY.

      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
