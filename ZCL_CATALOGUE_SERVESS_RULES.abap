class ZCL_CATALOGUE_SERVESS_RULES definition
  public
  inheriting from ZCL_CATALOGUE_BASE
  create public .

public section.

  methods CREATE_RULE
    importing
      !IS_RULE_PAYLOAD type ZSERVESS_TS_RULE
    raising
      ZCX_CATALOGUE_BASE_EXC .
  methods DELETE_RULE
    importing
      !IP_GUID type NUMC4
    raising
      ZCX_CATALOGUE_BASE_EXC .
  methods GET_RULES_COLLECTION
    returning
      value(RT_RULES_COLLECTION) type ZSERVESS_TT_RULES_COLLECTION
    raising
      ZCX_CATALOGUE_BASE_EXC .
  methods UPDATE_RULE
    importing
      !IS_RULE_PAYLOAD type ZSERVESS_TS_RULE
      !IP_GUID type NUMC4 .
  methods GET_ALL_RULES
    returning
      value(ET_RULES) type ZSERVESS_TT_RULES
    exceptions
      ZCX_CATALOGUE_BASE_EXC .
  methods GET_RULES_BY_FILTER
    importing
      !IT_SET_FILTERS type /IWBEP/T_MGW_SELECT_OPTION
    returning
      value(ET_RULES) type ZSERVESS_TT_RULES
    raising
      ZCX_CATALOGUE_BASE_EXC .
  methods GET_RULE_BY_ID
    importing
      !IP_GUID type NUMC4
    returning
      value(ES_RULE) type ZSERVESS_TS_RULE .
  methods CONSTRUCTOR .
protected section.
private section.

  types:
    tt_servessrulesmap type table of zservessrulesmap .

  methods CONVERT_TO_CATALOGUE_PAYLOAD
    importing
      !IS_ENTITY type ZSERVESS_TS_RULE
    returning
      value(RT_CATALOGUE_PAYLOAD) type ZCATALOGUE_TT_PAYLOAD .
ENDCLASS.



CLASS ZCL_CATALOGUE_SERVESS_RULES IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_SERVESS_RULES->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.

    super->constructor(
      ip_catalogue_guid_type = 'NUMC4'
      ip_catalogue_table = 'ZSERVESSRULESMAP'
      ip_catalogue_guid_field = 'RECORDID'
      ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CATALOGUE_SERVESS_RULES->CONVERT_TO_CATALOGUE_PAYLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_ENTITY                      TYPE        ZSERVESS_TS_RULE
* | [<-()] RT_CATALOGUE_PAYLOAD           TYPE        ZCATALOGUE_TT_PAYLOAD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method convert_to_catalogue_payload.



    data:
      lo_descr                  type ref to cl_abap_tabledescr,
      lo_type                   type ref to cl_abap_datadescr,
      lo_struct                 type ref to cl_abap_structdescr,
      lt_components             type cl_abap_structdescr=>component_table,
      wa_catalogue_payload_line type zcatalogue_ts_payload_pair,
      lt_entity                 type table of zservess_ts_rule.

    field-symbols: <fs_field_value> type any.

    append is_entity to lt_entity.

    lo_descr ?= cl_abap_typedescr=>describe_by_data( lt_entity ).
    lo_type = lo_descr->get_table_line_type( ).
    lo_struct ?= cl_abap_typedescr=>describe_by_name( lo_type->absolute_name ).
    lt_components = lo_struct->get_components( ).

    loop at lt_components assigning field-symbol(<ls_component>).

      wa_catalogue_payload_line-key = <ls_component>-name.

      if <fs_field_value>  is assigned.
        unassign <fs_field_value> .
      endif.

      assign component <ls_component>-name of structure is_entity to <fs_field_value>.

      get reference of <fs_field_value> into wa_catalogue_payload_line-value.

      append wa_catalogue_payload_line to rt_catalogue_payload.

    endloop.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_SERVESS_RULES->CREATE_RULE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_RULE_PAYLOAD                TYPE        ZSERVESS_TS_RULE
* | [!CX!] ZCX_CATALOGUE_BASE_EXC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_rule.

    data: lt_catalogue_payload   type zcatalogue_tt_payload,
          lcx_catalogue_base_exc type ref to zcx_catalogue_base_exc,
          lv_error_text          type bapi_msg.

    lt_catalogue_payload = convert_to_catalogue_payload( is_rule_payload ).

    try.


        me->zif_catalogue~append_new_record( lt_catalogue_payload ).


      catch zcx_catalogue_base_exc into lcx_catalogue_base_exc .
        lv_error_text = lcx_catalogue_base_exc->get_text( ).
    endtry.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_SERVESS_RULES->DELETE_RULE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_GUID                        TYPE        NUMC4
* | [!CX!] ZCX_CATALOGUE_BASE_EXC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method delete_rule.

    data:
      lr_guid                type ref to data,
      lcx_catalogue_base_exc type ref to zcx_catalogue_base_exc,
      lv_error_text          type bapi_msg.

    get reference of ip_guid into lr_guid.

    try.

        me->zif_catalogue~delete_record_by_guid( lr_guid ).

      catch zcx_catalogue_base_exc into lcx_catalogue_base_exc .
        lv_error_text = lcx_catalogue_base_exc->get_text( ).

    endtry.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_SERVESS_RULES->GET_ALL_RULES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] ET_RULES                       TYPE        ZSERVESS_TT_RULES
* | [EXC!] ZCX_CATALOGUE_BASE_EXC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_all_rules.

    data:
      lr_catalogue_records   type ref to data,
      lt_rules               type table of zservessrulesmap,
      lcx_catalogue_base_exc type ref to zcx_catalogue_base_exc,
      lv_error_text          type bapi_msg.

    field-symbols <fs_catalogue_table> type any.

    try.

        lr_catalogue_records = me->zif_catalogue~get_all_records( ).

        assign lr_catalogue_records->* to <fs_catalogue_table>.

        et_rules = <fs_catalogue_table>.

      catch zcx_catalogue_base_exc into lcx_catalogue_base_exc .
        lv_error_text = lcx_catalogue_base_exc->get_text( ).

    endtry.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_SERVESS_RULES->GET_RULES_BY_FILTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SET_FILTERS                 TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [<-()] ET_RULES                       TYPE        ZSERVESS_TT_RULES
* | [!CX!] ZCX_CATALOGUE_BASE_EXC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_rules_by_filter.

    data:
      lr_catalogue_records   type ref to data,
      lt_rules               type table of zservessrulesmap,
      lcx_catalogue_base_exc type ref to zcx_catalogue_base_exc,
      lv_error_text          type bapi_msg.


    field-symbols <fs_catalogue_table> type any.

    try.

        lr_catalogue_records = me->zif_catalogue~get_records_by_filter( it_set_filters = it_set_filters ).

        assign lr_catalogue_records->* to <fs_catalogue_table>.

        et_rules = <fs_catalogue_table>.

      catch zcx_catalogue_base_exc into lcx_catalogue_base_exc .
        lv_error_text = lcx_catalogue_base_exc->get_text( ).

    endtry.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_SERVESS_RULES->GET_RULES_COLLECTION
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_RULES_COLLECTION            TYPE        ZSERVESS_TT_RULES_COLLECTION
* | [!CX!] ZCX_CATALOGUE_BASE_EXC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_rules_collection.

    data:
      lr_column_records      type ref to data,
      lcx_catalogue_base_exc type ref to zcx_catalogue_base_exc,
      lv_error_text          type bapi_msg
      .

    field-symbols: <fs_column_records> type any.

    try.

        lr_column_records = me->zif_catalogue~get_distinct_column_values( ip_column = 'RULEID' ).

        assign lr_column_records->* to <fs_column_records>.

        rt_rules_collection = <fs_column_records>.

      catch zcx_catalogue_base_exc into lcx_catalogue_base_exc .
        lv_error_text = lcx_catalogue_base_exc->get_text( ).

    endtry.



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_SERVESS_RULES->GET_RULE_BY_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_GUID                        TYPE        NUMC4
* | [<-()] ES_RULE                        TYPE        ZSERVESS_TS_RULE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_rule_by_id.

    data:
      lr_catalogue_records type ref to data,
      lr_guid              type ref to data,
      lt_rules             type table of zservessrulesmap.

    field-symbols <fs_catalogue_table> type any.

    get reference of ip_guid into lr_guid.

    lr_catalogue_records = me->zif_catalogue~get_record_by_guid( lr_guid ).

    assign lr_catalogue_records->* to <fs_catalogue_table>.

    lt_rules = <fs_catalogue_table>.

    read table lt_rules into es_rule index 1.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CATALOGUE_SERVESS_RULES->UPDATE_RULE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_RULE_PAYLOAD                TYPE        ZSERVESS_TS_RULE
* | [--->] IP_GUID                        TYPE        NUMC4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method update_rule.

    data:
      lr_guid                type ref to data,
      lt_catalogue_payload   type zcatalogue_tt_payload,
      lcx_catalogue_base_exc type ref to zcx_catalogue_base_exc,
      lv_error_text          type bapi_msg.

    get reference of ip_guid into lr_guid.

    lt_catalogue_payload = convert_to_catalogue_payload( is_rule_payload ).

    try.

        me->zif_catalogue~update_record_by_guid(
          exporting
            ir_guid    = lr_guid
            it_payload = lt_catalogue_payload
        ).

      catch zcx_catalogue_base_exc into lcx_catalogue_base_exc .
        lv_error_text = lcx_catalogue_base_exc->get_text( ).

    endtry.

  endmethod.
ENDCLASS.