INTERFACE zif_ida_query
  PUBLIC .

  METHODS set_range
    IMPORTING
      !i_column    TYPE csequence
      !i_range_tab TYPE STANDARD TABLE.

  METHODS set_date_valid_range
    IMPORTING
      !i_valid_from_field TYPE string
      !i_valid_to_field   TYPE string
      !i_date             TYPE dats
      !i_only_date        TYPE crmt_boolean OPTIONAL.


  METHODS exec
    IMPORTING
      !i_max_rows     TYPE i OPTIONAL
    CHANGING
      !c_query_result TYPE STANDARD TABLE .

  METHODS count
    CHANGING
      !r_result TYPE i.


  METHODS add_filter_list
    IMPORTING
      !i_filter_tab TYPE /iwbep/t_mgw_select_option .

  METHODS set_required_fields
    IMPORTING i_required_field_tab TYPE string_table.


  METHODS set_sort_fields
    IMPORTING i_sort_element_tab TYPE if_salv_ida_types_int=>yt_db_sort_rule.
ENDINTERFACE.
