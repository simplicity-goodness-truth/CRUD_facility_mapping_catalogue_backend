interface ZIF_CATALOGUE
  public .


  methods APPEND_NEW_RECORD
    importing
      !IT_PAYLOAD type ZCATALOGUE_TT_PAYLOAD .
  methods GET_RECORD_BY_GUID
    importing
      !IR_GUID type ref to DATA
    returning
      value(RT_RECORDS) type ref to DATA .
  methods GET_ALL_RECORDS
    returning
      value(RT_RECORDS) type ref to DATA .
  methods DELETE_RECORD_BY_GUID
    importing
      !IR_GUID type ref to DATA .
  methods DELETE_ALL_RECORDS .
  methods UPDATE_RECORD_BY_GUID
    importing
      !IR_GUID type ref to DATA
      !IT_PAYLOAD type ZCATALOGUE_TT_PAYLOAD .
  methods SORT_RECORDS
    importing
      !IR_UNSORTED_RECORDS_SET type ref to DATA
      !IT_SORTING_ORDER type /IWBEP/T_MGW_SORTING_ORDER
    returning
      value(RR_SORTED_RECORDS_SET) type ref to DATA .
  methods GET_DISTINCT_COLUMN_VALUES
    importing
      !IP_COLUMN type NAME_FELD
    returning
      value(RR_DISTINCT_COLUMN_VALUES) type ref to DATA .
  methods GET_RECORDS_BY_FILTER
    importing
      !IT_SET_FILTERS type /IWBEP/T_MGW_SELECT_OPTION
    returning
      value(RT_RECORDS) type ref to DATA .
endinterface.