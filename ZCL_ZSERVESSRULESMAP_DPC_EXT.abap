class ZCL_ZSERVESSRULESMAP_DPC_EXT definition
  public
  inheriting from ZCL_ZSERVESSRULESMAP_DPC
  create public .

public section.
protected section.

  methods RULESET_CREATE_ENTITY
    redefinition .
  methods RULESET_DELETE_ENTITY
    redefinition .
  methods RULESET_GET_ENTITY
    redefinition .
  methods RULESET_GET_ENTITYSET
    redefinition .
  methods RULESET_UPDATE_ENTITY
    redefinition .
  methods RULESCOLLECTIONS_GET_ENTITYSET
    redefinition .
private section.

  methods GET_FILTER_SELECT_OPTIONS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
      !IP_PROPERTY type STRING
    returning
      value(RT_SELECT_OPTIONS) type /IWBEP/T_COD_SELECT_OPTIONS .
ENDCLASS.



CLASS ZCL_ZSERVESSRULESMAP_DPC_EXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZSERVESSRULESMAP_DPC_EXT->GET_FILTER_SELECT_OPTIONS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET
* | [--->] IP_PROPERTY                    TYPE        STRING
* | [<-()] RT_SELECT_OPTIONS              TYPE        /IWBEP/T_COD_SELECT_OPTIONS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_filter_select_options.

    data(it_filter_so) = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    if line_exists( it_filter_so[ property = ip_property ] ).

      rt_select_options = it_filter_so[ property = ip_property ]-select_options.

    endif.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZSERVESSRULESMAP_DPC_EXT->RULESCOLLECTIONS_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZSERVESSRULESMAP_MPC=>TT_RULESCOLLECTION
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* | [!CX!] ZCX_CATALOGUE_BASE_EXC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method rulescollections_get_entityset.

    data:
      lo_catalogue               type ref to zcl_catalogue_base,
      lo_catalogue_servess_rules type ref to zcl_catalogue_servess_rules.


    lo_catalogue = zcl_catalogue_base=>catalogue_factory( 'SERVESS_RULES' ).

    lo_catalogue_servess_rules ?= lo_catalogue.

    et_entityset = lo_catalogue_servess_rules->get_rules_collection( ).



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZSERVESSRULESMAP_DPC_EXT->RULESET_CREATE_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY_C(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IO_DATA_PROVIDER               TYPE REF TO /IWBEP/IF_MGW_ENTRY_PROVIDER(optional)
* | [<---] ER_ENTITY                      TYPE        ZCL_ZSERVESSRULESMAP_MPC=>TS_RULE
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* | [!CX!] ZCX_CATALOGUE_BASE_EXC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ruleset_create_entity.

    data:
      lo_catalogue               type ref to zcl_catalogue_base,
      lo_catalogue_servess_rules type ref to zcl_catalogue_servess_rules,
      ls_rule_payload            type zservess_ts_rule.

    io_data_provider->read_entry_data( importing es_data = er_entity ).

    lo_catalogue = zcl_catalogue_base=>catalogue_factory( 'SERVESS_RULES' ).

    lo_catalogue_servess_rules ?= lo_catalogue.

    move-corresponding er_entity to  ls_rule_payload.

    lo_catalogue_servess_rules->create_rule( ls_rule_payload ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZSERVESSRULESMAP_DPC_EXT->RULESET_DELETE_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY_D(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* | [!CX!] ZCX_CATALOGUE_BASE_EXC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ruleset_delete_entity.

    data:
      lo_catalogue               type ref to zcl_catalogue_base,
      lo_catalogue_servess_rules type ref to zcl_catalogue_servess_rules,
      ls_key_tab                 like line of it_key_tab,
      lv_guid                    type numc4.

    field-symbols <fs_catalogue_table> type any.

    read table it_key_tab into ls_key_tab with key name = 'RecordID'.

    if ls_key_tab is initial.
      return.
    endif.

    lv_guid = ls_key_tab-value.

    lo_catalogue = zcl_catalogue_base=>catalogue_factory( 'SERVESS_RULES' ).

    lo_catalogue_servess_rules ?= lo_catalogue.

    lo_catalogue_servess_rules->delete_rule( lv_guid ).


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZSERVESSRULESMAP_DPC_EXT->RULESET_GET_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IO_REQUEST_OBJECT              TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [<---] ER_ENTITY                      TYPE        ZCL_ZSERVESSRULESMAP_MPC=>TS_RULE
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* | [!CX!] ZCX_CATALOGUE_BASE_EXC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method RULESET_GET_ENTITY.

    data:
      lo_catalogue               type ref to zcl_catalogue_base,
      lo_catalogue_servess_rules type ref to zcl_catalogue_servess_rules,
      lt_zservessrulesmap        type zservess_tt_rules,
      ls_zservessrulesmap        type zservess_ts_rule,
      ls_key_tab                 like line of it_key_tab,
      lv_guid                    type numc4.

    field-symbols <fs_catalogue_table> type any.

    read table it_key_tab into ls_key_tab with key name = 'RecordID'.

    if ls_key_tab is initial.
      return.
    endif.

    lv_guid = ls_key_tab-value.

    lo_catalogue = zcl_catalogue_base=>catalogue_factory( 'SERVESS_RULES' ).

    lo_catalogue_servess_rules ?= lo_catalogue.

    ls_zservessrulesmap = lo_catalogue_servess_rules->get_rule_by_id( lv_guid ).

    move-corresponding ls_zservessrulesmap to er_entity.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZSERVESSRULESMAP_DPC_EXT->RULESET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZSERVESSRULESMAP_MPC=>TT_RULE
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* | [!CX!] ZCX_CATALOGUE_BASE_EXC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ruleset_get_entityset.

    data:
      lo_catalogue               type ref to zcl_catalogue_base,
      lo_catalogue_servess_rules type ref to zcl_catalogue_servess_rules,
      lt_zservessrulesmap        type zservess_tt_rules,
      lr_unsorted_table          type ref to data,
      lr_sorted_table            type ref to data,
      lt_set_filters             type /iwbep/t_mgw_select_option.


    field-symbols: <fs_catalogue_table> type any,
                   <lt_sorted_table>    type any table.

    lo_catalogue = zcl_catalogue_base=>catalogue_factory( 'SERVESS_RULES' ).

    lo_catalogue_servess_rules ?= lo_catalogue.


    " Getting filters

    lt_set_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    if  lt_set_filters is initial.

      lt_zservessrulesmap = lo_catalogue_servess_rules->get_all_rules( ).

    else.

       lt_zservessrulesmap = lo_catalogue_servess_rules->get_rules_by_filter( lt_set_filters ).

    endif.

    move-corresponding  lt_zservessrulesmap to et_entityset.



    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Executing sorting
    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    clear lr_unsorted_table.

    if it_order is not initial.

      get reference of et_entityset into lr_unsorted_table.

      lr_sorted_table = lo_catalogue->zif_catalogue~sort_records(
        exporting
          ir_unsorted_records_set = lr_unsorted_table
          it_sorting_order        =     it_order
      ).


      assign lr_sorted_table->* to <lt_sorted_table>.

      et_entityset = <lt_sorted_table>.

    else.

      sort et_entityset by recordid.

    endif. " if it_order is not initial


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZSERVESSRULESMAP_DPC_EXT->RULESET_UPDATE_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY_U(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IO_DATA_PROVIDER               TYPE REF TO /IWBEP/IF_MGW_ENTRY_PROVIDER(optional)
* | [<---] ER_ENTITY                      TYPE        ZCL_ZSERVESSRULESMAP_MPC=>TS_RULE
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* | [!CX!] ZCX_CATALOGUE_BASE_EXC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ruleset_update_entity.


    data:
      lo_catalogue               type ref to zcl_catalogue_base,
      lo_catalogue_servess_rules type ref to zcl_catalogue_servess_rules,
*      lt_zservessrulesmap        type zservess_tt_rules,
*      ls_zservessrulesmap        type zservess_ts_rule,
      ls_key_tab                 like line of it_key_tab,
      lv_guid                    type numc4,
      ls_rule_payload            type zservess_ts_rule..

    field-symbols <fs_catalogue_table> type any.

    read table it_key_tab into ls_key_tab with key name = 'RecordID'.
    io_data_provider->read_entry_data( importing es_data = er_entity ).

    if ( ls_key_tab is initial ) or ( er_entity is initial ).
      return.
    endif.

    lv_guid = ls_key_tab-value.

    lo_catalogue = zcl_catalogue_base=>catalogue_factory( 'SERVESS_RULES' ).

    lo_catalogue_servess_rules ?= lo_catalogue.

    move-corresponding er_entity to  ls_rule_payload.

    lo_catalogue_servess_rules->update_rule(
      exporting
        is_rule_payload =     ls_rule_payload
        ip_guid         =     lv_guid
    ).

  endmethod.
ENDCLASS.