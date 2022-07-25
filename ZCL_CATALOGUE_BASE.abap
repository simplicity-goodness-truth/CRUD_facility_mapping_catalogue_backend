class ZCL_CATALOGUE_BASE definition
 public
  create public .

public section.

  interfaces ZIF_CATALOGUE .

  class-methods CATALOGUE_FACTORY
    importing
      !IP_CATALOGUE_NAME type STRING
    returning
      value(RR_CATALOGUE) type ref to ZCL_CATALOGUE_BASE .
  methods CONSTRUCTOR
    importing
      !IP_CATALOGUE_GUID_TYPE type TYPENAME
      !IP_CATALOGUE_TABLE type TABNAME16
      !IP_CATALOGUE_GUID_FIELD type NAME_FELD .
protected section.
private section.

  types:
    begin of ty_catalogue_guids_by_type,
           type_numc4       type numc4,
           type_sysuuid_x16 type sysuuid_x16,
         end of ty_catalogue_guids_by_type .

  data MV_CATALOGUE_TABLE type TABNAME16 .
  data MV_CATALOGUE_GUID_TYPE type TYPENAME .
  data MV_CATALOGUE_GUID_FIELD type NAME_FELD .

  methods CREATE_NEW_GUID
    returning
      value(RR_GUID) type ref to DATA .
  methods GENERATE_X16_GUID
    returning
      value(RP_SYSUUID_X16_GUID) type SYSUUID_X16 .
  methods GET_NEXT_NUMC4_GUID
    returning
      value(RP_NUMC4_GUID) type NUMC4 .
  methods PUT_CATALOGUE_FIELDS_TO_STRING
    importing
      !IP_SEPARATOR type CHAR1 optional
    returning
      value(RS_STRINGIFIEDFIELDS) type STRING .
  methods SET_CATALOGUE_GUID_FIELD
    importing
      !IP_CATALOGUE_GUID_FIELD type NAME_FELD .
  methods SET_CATALOGUE_GUID_TYPE
    importing
      !IP_CATALOGUE_GUID_TYPE type TYPENAME .
  methods SET_CATALOGUE_TABLE
    importing
      !IP_CATALOGUE_TABLE type TABNAME16 .
  methods PUT_GUID_TO_STRING
    importing
      !IR_GUID type ref to DATA
    returning
      value(RS_GUID) type STRING .
ENDCLASS.



CLASS ZCL_CATALOGUE_BASE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CATALOGUE_BASE=>CATALOGUE_FACTORY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_CATALOGUE_NAME              TYPE        STRING
* | [<-()] RR_CATALOGUE                   TYPE REF TO ZCL_CATALOGUE_BASE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method catalogue_factory.

    data lv_catalogue_subclass type string.

    concatenate 'ZCL_CATALOGUE_' ip_catalogue_name into lv_catalogue_subclass.

    create object rr_catalogue type (lv_catalogue_subclass).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_BASE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_CATALOGUE_GUID_TYPE         TYPE        TYPENAME
* | [--->] IP_CATALOGUE_TABLE             TYPE        TABNAME16
* | [--->] IP_CATALOGUE_GUID_FIELD        TYPE        NAME_FELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.

    set_catalogue_guid_type( ip_catalogue_guid_type ).

    set_catalogue_guid_field( ip_catalogue_guid_field ).

    set_catalogue_table( ip_catalogue_table ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CATALOGUE_BASE->CREATE_NEW_GUID
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RR_GUID                        TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_new_guid.

    data:
      lv_sysuuid_x16_guid type sysuuid_x16,
      lv_numc4_guid       type numc4.

    field-symbols <fs_result> type any.

    case mv_catalogue_guid_type.

      when 'NUMC4'.

        lv_numc4_guid = get_next_numc4_guid( ).

        create data rr_guid type numc4.
        assign rr_guid->* to <fs_result>.
        <fs_result> = lv_numc4_guid.

      when 'SYSUUID_X16'.

        lv_sysuuid_x16_guid = generate_x16_guid( ).

        create data rr_guid type sysuuid_x16.
        assign rr_guid->* to <fs_result>.
        <fs_result> = lv_sysuuid_x16_guid.

    endcase.



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CATALOGUE_BASE->GENERATE_X16_GUID
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RP_SYSUUID_X16_GUID            TYPE        SYSUUID_X16
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method generate_x16_guid.

    " Standard generation of X16 GUID

    try.
        rp_sysuuid_x16_guid = cl_system_uuid=>create_uuid_x16_static( ).

      catch cx_uuid_error.
        rp_sysuuid_x16_guid = '0'.
    endtry.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CATALOGUE_BASE->GET_NEXT_NUMC4_GUID
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RP_NUMC4_GUID                  TYPE        NUMC4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_next_numc4_guid.

    data: lv_numc4_guid         type numc4,
          lv_order_by_condition type string.

    lv_order_by_condition = |{ mv_catalogue_guid_field }| && | | && |descending|.

    try.

        select (mv_catalogue_guid_field) into lv_numc4_guid from (mv_catalogue_table) up to 1 rows
           order by (lv_order_by_condition).

          rp_numc4_guid = lv_numc4_guid + 1.

        endselect.

      catch cx_sy_dynamic_osql_error.

        rp_numc4_guid = 1.

    endtry.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CATALOGUE_BASE->PUT_CATALOGUE_FIELDS_TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_SEPARATOR                   TYPE        CHAR1(optional)
* | [<-()] RS_STRINGIFIEDFIELDS           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method put_catalogue_fields_to_string.

    data:
      lo_descr           type ref to cl_abap_tabledescr,
      lo_type            type ref to cl_abap_datadescr,
      lo_struct          type ref to cl_abap_structdescr,
      lt_components      type cl_abap_structdescr=>component_table,
      lr_catalogue_table type ref to data.

    field-symbols <lr_catalogue_table> type any table.

    create data lr_catalogue_table type standard table of (mv_catalogue_table).

    assign lr_catalogue_table->* to <lr_catalogue_table>.

    lo_descr ?= cl_abap_typedescr=>describe_by_data( <lr_catalogue_table> ).
    lo_type = lo_descr->get_table_line_type( ).
    lo_struct ?= cl_abap_typedescr=>describe_by_name( lo_type->absolute_name ).
    lt_components = lo_struct->get_components( ).

    loop at lt_components assigning field-symbol(<ls_component>).

      if rs_stringifiedfields is not initial.

        if ip_separator is initial.
          rs_stringifiedfields = |{ rs_stringifiedfields }| && | | && |{ <ls_component>-name }|.
        else.
          rs_stringifiedfields = |{ rs_stringifiedfields }| && |{ ip_separator }| && |{ <ls_component>-name }|.
        endif.

      else.

        rs_stringifiedfields = |{ <ls_component>-name }|.

      endif. " if rs_stringifiedfields is not initial

    endloop. " loop at lt_components assigning field-symbol(<ls_component>)

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CATALOGUE_BASE->PUT_GUID_TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_GUID                        TYPE REF TO DATA
* | [<-()] RS_GUID                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method put_guid_to_string.

    field-symbols <lv_guid> type any.

    assign ir_guid->* to <lv_guid>.

    rs_guid = <lv_guid>.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CATALOGUE_BASE->SET_CATALOGUE_GUID_FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_CATALOGUE_GUID_FIELD        TYPE        NAME_FELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SET_CATALOGUE_GUID_FIELD.

    mv_catalogue_guid_field = ip_catalogue_guid_field.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CATALOGUE_BASE->SET_CATALOGUE_GUID_TYPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_CATALOGUE_GUID_TYPE         TYPE        TYPENAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method set_catalogue_guid_type.

    mv_catalogue_guid_type = ip_catalogue_guid_type.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CATALOGUE_BASE->SET_CATALOGUE_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_CATALOGUE_TABLE             TYPE        TABNAME16
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method set_catalogue_table.

    mv_catalogue_table = ip_catalogue_table.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_BASE->ZIF_CATALOGUE~APPEND_NEW_RECORD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_PAYLOAD                     TYPE        ZCATALOGUE_TT_PAYLOAD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_catalogue~append_new_record.

    data: lr_guid               type ref to data,
          lr_catalogue_table_wa type ref to data,
          ls_zservessrulesmap   type zservessrulesmap,
          lr_zservessrulesmap   type ref to data.


    field-symbols:
      <fs_catalogue_record> type any,
      <fs_guid_field>       type any,
      <fs_guid_value>       type any,
      <fs_field>            type any,
      <fs_field_value>      type any.

    " Get a new GUID value and prepare a FS

    lr_guid = create_new_guid( ).

    assign lr_guid->* to <fs_guid_value>.

    " Create work area based on a table and prepare a FS

    create data lr_catalogue_table_wa type (mv_catalogue_table).
    assign lr_catalogue_table_wa->* to <fs_catalogue_record>.

    " Decode payload

    loop at it_payload assigning field-symbol(<ls_payload>).

      if <fs_field>  is assigned.
        unassign <fs_field>.
      endif.


      if <fs_field_value>  is assigned.
        unassign <fs_field_value>.
      endif.

      assign component <ls_payload>-key of structure <fs_catalogue_record> to <fs_field>.

      assign <ls_payload>-value->* to <fs_field_value>.

      <fs_field> = <fs_field_value>.

    endloop. " loop at it_payload assigning field-symbol(<ls_payload>)

    " Assigning a GUID

    assign component mv_catalogue_guid_field of structure <fs_catalogue_record> to <fs_guid_field>.
    <fs_guid_field> = <fs_guid_value>.

    " Inserting a record

    if <fs_catalogue_record> is assigned.

      insert (mv_catalogue_table) from <fs_catalogue_record>.

      if sy-subrc <> 0.

        raise exception type zcx_catalogue_base_exc
          exporting
            textid = zcx_catalogue_base_exc=>cant_insert_new_record.

      endif.

    endif. " if <fs_catalogue_record> is assigned

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_BASE->ZIF_CATALOGUE~DELETE_ALL_RECORDS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_CATALOGUE~DELETE_ALL_RECORDS.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_BASE->ZIF_CATALOGUE~DELETE_RECORD_BY_GUID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_GUID                        TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_catalogue~delete_record_by_guid.

    data:
      lv_where_condition      type string,
      lv_where_condition_guid type string.

    lv_where_condition_guid = put_guid_to_string( ir_guid ).
    lv_where_condition = |{ mv_catalogue_guid_field }| && | = '| && |{ lv_where_condition_guid }| && |'|.

    try.

        delete from (mv_catalogue_table) where (lv_where_condition).

        if sy-subrc <> 0.
          raise exception type zcx_catalogue_base_exc
            exporting
              textid = zcx_catalogue_base_exc=>cant_delete_records.
        endif.

      catch cx_sy_dynamic_osql_error.

    endtry.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_BASE->ZIF_CATALOGUE~GET_ALL_RECORDS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_RECORDS                     TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_catalogue~get_all_records.

    data:
      lr_catalogue_table  type ref to data,
      lv_catalogue_fields type string.

    field-symbols <lr_catalogue_table> type any table.

    create data lr_catalogue_table type standard table of (mv_catalogue_table).

    assign lr_catalogue_table->* to <lr_catalogue_table>.

    lv_catalogue_fields = put_catalogue_fields_to_string( ).

    try.

        select (lv_catalogue_fields)
           into corresponding fields of table <lr_catalogue_table>
            from (mv_catalogue_table).

        if sy-subrc <> 0.
          raise exception type zcx_catalogue_base_exc
            exporting
              textid = zcx_catalogue_base_exc=>cant_get_records.

        endif.

      catch cx_sy_dynamic_osql_error.

    endtry.

    get reference of <lr_catalogue_table> into rt_records.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_BASE->ZIF_CATALOGUE~GET_DISTINCT_COLUMN_VALUES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_COLUMN                      TYPE        NAME_FELD
* | [<-()] RR_DISTINCT_COLUMN_VALUES      TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_catalogue~get_distinct_column_values.

    data:
      lr_selection_table type ref to data,
      lv_table_column    type string,
      lo_components      type abap_component_tab,
      lo_struct_desc     type ref to cl_abap_structdescr,
      lo_table_desc      type ref to cl_abap_tabledescr.

    field-symbols:
      <fs_selection> type any table.

    lv_table_column = |{ mv_catalogue_table }-{ ip_column }|.

    lo_components = value cl_abap_structdescr=>component_table(

                        (
                          name = ip_column
                          type = cast #( cl_abap_elemdescr=>describe_by_name( lv_table_column ) )
                        )
                    ).

    lo_struct_desc = cl_abap_structdescr=>create( lo_components ).

    lo_table_desc = cl_abap_tabledescr=>create(
                            p_line_type  = lo_struct_desc
                            p_table_kind = cl_abap_tabledescr=>tablekind_std
                            p_unique     = abap_false
                            p_key        = value #(
                                             ( name = ip_column )
                                           )
                            p_key_kind   = cl_abap_tabledescr=>keydefkind_user
                       ).

    " Creating selection table

    create data lr_selection_table type handle lo_table_desc.

    assign lr_selection_table->* to <fs_selection>.

    select distinct (ip_column) from (mv_catalogue_table) into corresponding fields of table <fs_selection>.

    if sy-subrc <> 0.

      raise exception type zcx_catalogue_base_exc
        exporting
          textid = zcx_catalogue_base_exc=>cant_get_dist_column_records.

    endif.

    sort <fs_selection> by (ip_column).

    get reference of <fs_selection> into rr_distinct_column_values.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_BASE->ZIF_CATALOGUE~GET_RECORDS_BY_FILTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SET_FILTERS                 TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [<-()] RT_RECORDS                     TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_catalogue~get_records_by_filter.


    data:
      lr_catalogue_table  type ref to data,
      lv_catalogue_fields type string,
      lv_where_clause     type string.

    field-symbols <lr_catalogue_table> type any table.

    create data lr_catalogue_table type standard table of (mv_catalogue_table).

    assign lr_catalogue_table->* to <lr_catalogue_table>.

    lv_catalogue_fields = put_catalogue_fields_to_string( ).

    loop at it_set_filters assigning field-symbol(<ls_set_filter>).

      loop at <ls_set_filter>-select_options assigning field-symbol(<ls_select_option>).

        if lv_where_clause is not initial.

          lv_where_clause = |{ lv_where_clause }| && | | && |AND| && | | && |{ <ls_set_filter>-property }| &&
          | | && |{ <ls_select_option>-option }| && | | && |'{ <ls_select_option>-low }'| .

        else.

          lv_where_clause = |{ <ls_set_filter>-property }| &&
          | | && |{ <ls_select_option>-option }| && | | && |'{ <ls_select_option>-low }'|.

        endif.

        if <ls_select_option>-high is not initial.

          lv_where_clause = |{ lv_where_clause }| && | | && |AND| &&
          | | && | | && |'{ <ls_select_option>-low }'|.
        endif.

      endloop. "  loop at <ls_set_filter>-select_options assigning field-symbol(<ls_select_option>)

    endloop. " loop at it_set_filters assigning field-symbol(<ls_set_filter>)

    replace all occurrences of 'CP' in lv_where_clause with 'LIKE'.
    replace all occurrences of '*' in lv_where_clause with '%'.
    replace all occurrences of 'BT' in lv_where_clause with 'BETWEEN'.

    try.

        select (lv_catalogue_fields)
          into corresponding fields of table <lr_catalogue_table>
           from (mv_catalogue_table) where (lv_where_clause).

        if sy-subrc <> 0.
          raise exception type zcx_catalogue_base_exc
            exporting
              textid = zcx_catalogue_base_exc=>cant_get_records.

        endif.

      catch cx_sy_dynamic_osql_error.

    endtry.

    get reference of <lr_catalogue_table> into rt_records.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_BASE->ZIF_CATALOGUE~GET_RECORD_BY_GUID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_GUID                        TYPE REF TO DATA
* | [<-()] RT_RECORDS                     TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_catalogue~get_record_by_guid.

    data:
      lr_catalogue_table      type ref to data,
      lv_where_condition      type string,
      lv_where_condition_guid type string,
      lv_catalogue_fields     type string.

    field-symbols <lr_catalogue_table> type any table.

    lv_where_condition_guid = put_guid_to_string( ir_guid ).
    lv_where_condition = |{ mv_catalogue_guid_field }| && | = '| && |{ lv_where_condition_guid }| && |'|.

    create data lr_catalogue_table type standard table of (mv_catalogue_table).

    assign lr_catalogue_table->* to <lr_catalogue_table>.

    lv_catalogue_fields = put_catalogue_fields_to_string( ).
    try.

        select (lv_catalogue_fields)
           into corresponding fields of table <lr_catalogue_table>
            from (mv_catalogue_table) where (lv_where_condition).

      catch cx_sy_dynamic_osql_error.

    endtry.

    get reference of <lr_catalogue_table> into rt_records.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_BASE->ZIF_CATALOGUE~SORT_RECORDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_UNSORTED_RECORDS_SET        TYPE REF TO DATA
* | [--->] IT_SORTING_ORDER               TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [<-()] RR_SORTED_RECORDS_SET          TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_catalogue~sort_records.

    data: lo_table_ref type ref to data,
          lt_order_tab type abap_sortorder_tab,
          ls_order_tab type abap_sortorder.

    field-symbols:
      <fs_table> type any table,
      <fs_value> type any.


    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Decode incoming abstract entity
    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    create data lo_table_ref type standard table of (mv_catalogue_table).
    assign lo_table_ref->* to <fs_table>.

    if ( ir_unsorted_records_set is bound ).

      assign ir_unsorted_records_set->* to <fs_table>.

    endif. " if ( ir_unsorted_records_set is bound )

    loop at it_sorting_order assigning field-symbol(<ls_order>).

      if <ls_order>-order = 'desc'.
        ls_order_tab-descending = abap_true.
      else.
        ls_order_tab-descending = abap_false.
      endif.

      search <ls_order>-property for 'Date' .
      if sy-subrc <> 0.
        ls_order_tab-astext = abap_true.
      endif.

      ls_order_tab-name = <ls_order>-property.

      translate ls_order_tab-name to upper case.

      append ls_order_tab to lt_order_tab.

      clear:
        ls_order_tab.

    endloop. " loop at it_sorting_order assigning field-symbol(<ls_order>)


    try.
        sort <fs_table> by (lt_order_tab).
      catch cx_sy_dyn_table_ill_comp_val.
    endtry.

    get reference of <fs_table> into rr_sorted_records_set.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_BASE->ZIF_CATALOGUE~UPDATE_RECORD_BY_GUID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_GUID                        TYPE REF TO DATA
* | [--->] IT_PAYLOAD                     TYPE        ZCATALOGUE_TT_PAYLOAD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_catalogue~update_record_by_guid.

    data:
      lv_where_condition      type string,
      lv_where_condition_guid type string,
      lv_set_settings         type string.

    field-symbols:
      <fs_field_value>      type any.


    " Preparing WHERE condition

    lv_where_condition_guid = put_guid_to_string( ir_guid ).
    lv_where_condition = |{ mv_catalogue_guid_field }| && | = '| && |{ lv_where_condition_guid }| && |'|.


    " Preparing SET settings

    loop at it_payload assigning field-symbol(<ls_payload>).

      if <fs_field_value>  is assigned.
        unassign <fs_field_value>.
      endif.

      assign <ls_payload>-value->* to <fs_field_value>.

      if <fs_field_value> is initial.
        continue.
      endif.

      if lv_set_settings is not initial.

        lv_set_settings = |{ lv_set_settings }| && | | && |{ <ls_payload>-key }| && | = | && |'{ <fs_field_value> }'|.

      else.

        lv_set_settings = |{ <ls_payload>-key }| && | = | && |'{ <fs_field_value> }'|.

      endif.

    endloop. " loop at it_payload assigning field-symbol(<ls_payload>)

    try.

        if lv_set_settings is not initial.

          update (mv_catalogue_table) set (lv_set_settings) where (lv_where_condition).

          if sy-subrc <> 0.

            raise exception type zcx_catalogue_base_exc
              exporting
                textid = zcx_catalogue_base_exc=>cant_update_record.

          endif.

        endif.

      catch cx_sy_dynamic_osql_error.

    endtry.


  endmethod.
ENDCLASS.