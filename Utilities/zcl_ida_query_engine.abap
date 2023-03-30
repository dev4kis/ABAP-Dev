CLASS zcl_ida_query_engine DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_exas_query .

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        i_cds_view_parameter_tab TYPE if_salv_ida_types_int=>yt_parameter
        !i_query_entity          TYPE if_sadl_entity=>ty_entity_type
        !i_query_entity_type     TYPE if_sadl_entity=>ty_entity_id DEFAULT cl_sadl_entity_factory=>co_type-cds .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA cds_view_parameter     TYPE if_salv_ida_types_int=>yt_parameter .
    DATA requested_field_tab    TYPE string_table .
    DATA sort_element_tab       TYPE if_salv_ida_types_int=>yt_db_sort_rule.

    DATA query_entity TYPE if_sadl_entity=>ty_entity_type .
    DATA query_entity_type TYPE if_sadl_entity=>ty_entity_id .
    DATA range_tab_collector TYPE REF TO cl_salv_range_tab_collector .


    METHODS set_system_parameters
      IMPORTING
        i_entity TYPE REF TO if_sadl_entity
      RAISING
        cx_sadl_entity.

    METHODS init_query_engine
      EXPORTING
        e_query_engine     TYPE REF TO cl_salv_ida_query_engine
        e_requested_field  TYPE string_table
        e_sort_element_tab TYPE if_salv_ida_types_int=>yt_db_sort_rule
      RAISING
        cx_sadl_entity
        cx_salv_db_connection
        cx_salv_ida_contract_violation.
ENDCLASS.



CLASS ZCL_IDA_QUERY_ENGINE IMPLEMENTATION.


  METHOD constructor.


    me->query_entity        = i_query_entity.
    me->query_entity_type   = i_query_entity_type.
    me->cds_view_parameter  = i_cds_view_parameter_tab.

    me->range_tab_collector = NEW cl_salv_range_tab_collector( ).


  ENDMETHOD.


  METHOD init_query_engine.

    DATA v_requested_field  TYPE string_table.



*-- CREATE IDA service
    cl_salv_ida_services=>create_entity_and_abqi( EXPORTING
                                                    iv_entity_id   = me->query_entity
                                                    iv_entity_type = me->query_entity_type
                                                  IMPORTING
                                                    eo_entity      = DATA(v_entity)
                                                    eo_fetch       = DATA(v_fetch)
                                              ).

*-- GET structure components
    DATA(v_ida_structdescr) = cl_salv_ida_structdescr=>create_for_sadl_entity(  io_entity = v_entity ).

*-- Initialize CDS Views system parameters if not provided
    set_system_parameters( v_entity ).


    DATA(v_sadl_entity) = v_ida_structdescr->get_sadl_entity( ).

    DATA(v_text_search) = cl_salv_ida_text_search_prov=>create_4_ida_api( v_ida_structdescr ).

    e_query_engine  = NEW cl_salv_ida_query_engine( io_sadl_entity          = v_sadl_entity
                                                    io_sadl_fetch           = v_fetch
                                                    io_salv_ida_text_search = v_text_search
                                                    ).


*-- GET request fields to build select statment column list / SELECT ( columns in v_requested_field_tab)
    e_requested_field = me->requested_field_tab.

    IF e_requested_field IS INITIAL.

      v_sadl_entity->get_elements(  IMPORTING et_elements = DATA(v_cds_elements) ).
      e_requested_field = VALUE #(  FOR v_element IN  v_cds_elements (  v_element-name   )  ).


    ENDIF.


*-- GET sort elements
    e_sort_element_tab = me->sort_element_tab[].


*-- GET condition to build SQL statment WHERE clause
    me->range_tab_collector->get_collected_ranges( IMPORTING et_named_ranges = DATA(v_ranges_tab) ).
    e_query_engine->if_salv_ida_query_engine~set_selection_range_tab( it_ranges = v_ranges_tab ).

*-- SET CDS View parameter
    IF me->cds_view_parameter IS NOT INITIAL.
      e_query_engine->if_salv_ida_query_engine~set_view_parameters( it_parameters	= me->cds_view_parameter ).

    ENDIF.

  ENDMETHOD.


  METHOD set_system_parameters.

    i_entity->get_parameters( IMPORTING et_parameters = DATA(v_cds_parameters) ).

    LOOP AT v_cds_parameters ASSIGNING FIELD-SYMBOL(<fs_parameter>).

*-- Replace system parameter if not provided
      IF NOT line_exists(  me->cds_view_parameter[ name = <fs_parameter>-name ] ).

        i_entity->get_annotations_for_parameter( EXPORTING iv_parameter = <fs_parameter>-name IMPORTING et_annotations = DATA(v_annotation_tab) ).

        LOOP AT v_annotation_tab ASSIGNING FIELD-SYMBOL(<fs_annotatoin>)
         WHERE value CP   'SYSTEM*'.
          CASE <fs_annotatoin>-value.
            WHEN 'SYSTEM_LANGUAGE'.
              APPEND VALUE #( name = <fs_parameter>-name value = if_sadl_query_types=>co_system_variable_placeholder-logon_language  ) TO me->cds_view_parameter.
            WHEN 'SYSTEM_DATE'.
              APPEND VALUE #( name = <fs_parameter>-name value = if_sadl_query_types=>co_system_variable_placeholder-client_date  ) TO me->cds_view_parameter.
          ENDCASE.

        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_exas_query~add_filter_list.

    LOOP AT i_filter_tab ASSIGNING FIELD-SYMBOL(<fs_filter>).

      me->range_tab_collector->add_ranges_for_name( iv_name   = <fs_filter>-property
                                                    it_ranges = <fs_filter>-select_options
                                           ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_exas_query~count.

    DATA v_query_engine TYPE REF TO cl_salv_ida_query_engine.

    init_query_engine( IMPORTING e_query_engine    = v_query_engine ).

    r_result = v_query_engine->select_row_count( )-result_size.


  ENDMETHOD.


  METHOD zif_exas_query~exec.

    DATA v_query_engine TYPE REF TO cl_salv_ida_query_engine.
    DATA v_requested_field TYPE string_table.
    DATA v_sort_element_tab TYPE if_salv_ida_types_int=>yt_db_sort_rule.

    init_query_engine(
          IMPORTING
            e_query_engine      = v_query_engine
            e_requested_field   = v_requested_field
            e_sort_element_tab  = v_sort_element_tab
            ).

    v_query_engine->select_data( EXPORTING  iv_from             = 1
                                            iv_to               = i_max_rows
                                            iv_fill_row_count   = abap_true
                                            it_requested_fields = v_requested_field
                                            it_sort_elements    = v_sort_element_tab
                                 IMPORTING  et_data =  c_query_result
                               ).


  ENDMETHOD.


  METHOD zif_exas_query~set_date_valid_range.

    IF i_only_date = abap_true.
      me->range_tab_collector->add_ranges_for_name( iv_name = i_valid_from_field
                                                    it_ranges =  VALUE /iwbep/t_cod_select_options( ( sign = 'I' option = 'LE' low = sy-datum ) )
                                                    ).

      me->range_tab_collector->add_ranges_for_name(  iv_name    = i_valid_to_field
                                                     it_ranges = VALUE /iwbep/t_cod_select_options( ( sign = 'I' option = 'GT' low = sy-datum  )
                                                                                                    ( sign = 'I' option = 'EQ' low = '00000000' ) ) ).

    ELSE.
      me->range_tab_collector->add_ranges_for_name( iv_name = i_valid_from_field
                                                    it_ranges =  VALUE /iwbep/t_cod_select_options( ( sign = 'I' option = 'LE' low = sy-datum && sy-uzeit ) )
                                                    ).

      me->range_tab_collector->add_ranges_for_name(  iv_name    = i_valid_to_field
                                                     it_ranges = VALUE /iwbep/t_cod_select_options( ( sign = 'I' option = 'GT' low = sy-datum && sy-uzeit )
                                                                                                    ( sign = 'I' option = 'EQ' low = '' )
                                                                                                    ( sign = 'I' option = 'EQ' low = '00000000000000' ) ) ).
    ENDIF.



  ENDMETHOD.


  METHOD zif_exas_query~set_range.

    me->range_tab_collector->add_ranges_for_name( iv_name = i_column
                                                  it_ranges = i_range_tab
                                                  ).


  ENDMETHOD.


  METHOD zif_exas_query~set_required_fields.

    me->requested_field_tab = i_required_field_tab.

  ENDMETHOD.


  METHOD zif_exas_query~set_sort_fields.

    me->sort_element_tab = i_sort_element_tab.

  ENDMETHOD.
ENDCLASS.
