class ZCL_API_UTIL definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_dyn_struct_props,
             field_name TYPE char100,
             size       TYPE i,
             ftype      TYPE char1,
           END OF ty_dyn_struct_props .
  types:
    ty_dyn_struct_props_tt TYPE STANDARD TABLE OF ty_dyn_struct_props WITH KEY field_name .

  class-methods EXTRACT_BPATH_FILTER
    importing
      !I_FILTER type STRING
    returning
      value(FILTER_DATA) type ZCA_KEY_VALUE_STRING_TT .
  class-methods GET_AND_CREATE_ENTITY_BY_BPATH
    importing
      !I_ENTITY type ref to CL_CRM_BOL_ENTITY
      !I_BPATH type STRING
      !I_CREATE type CRMT_BOOLEAN
    returning
      value(R_ENTITY) type ref to CL_CRM_BOL_ENTITY .
  class-methods CREATE_CSV_XLXS_FROM_ITAB
    importing
      !IT_FIELDCAT type LVC_T_FCAT optional
      !IT_SORT type LVC_T_SORT optional
      !IT_FILT type LVC_T_FILT optional
      !IS_LAYOUT type LVC_S_LAYO optional
      !IT_HYPERLINKS type LVC_T_HYPE optional
      !I_FORMAT type IF_SALV_BS_LEX_FORMAT=>YS_FORMAT default IF_SALV_BS_LEX_FORMAT=>MC_FORMAT_CSV
      value(IT_DATA) type STANDARD TABLE
    returning
      value(R_XSTRING) type XSTRING .
  class-methods ITAB_TO_XLSX
    importing
      !IOBJ_DATA type ref to DATA
    returning
      value(RV_XSTRING) type XSTRING .
  class-methods CREATE_DYNAMIC_STRUCTURE
    importing
      !I_DYN_STRUCT_PROPS type TY_DYN_STRUCT_PROPS_TT
    returning
      value(E_DYN_STRUCT) type ref to CL_ABAP_STRUCTDESCR .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_API_UTIL IMPLEMENTATION.


  METHOD create_csv_xlxs_from_itab.
    DATA(lt_data) = REF #( it_data ).

    IF it_fieldcat IS INITIAL.
      FIELD-SYMBOLS: <tab> TYPE STANDARD TABLE.
      ASSIGN lt_data->* TO <tab>.
      TRY.
          cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false
          IMPORTING
            r_salv_table = DATA(salv_table)
          CHANGING
            t_table      = <tab> ).

          DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                   r_columns      = salv_table->get_columns( )
                                   r_aggregations = salv_table->get_aggregations( ) ).
        CATCH cx_salv_msg.
          RETURN.
      ENDTRY.

    ELSE.
      lt_fcat = it_fieldcat.
    ENDIF.

    cl_salv_bs_lex=>export_from_result_data_table(
      EXPORTING
        is_format            = i_format
        ir_result_data_table =  cl_salv_ex_util=>factory_result_data_table(
                                                r_data                      = lt_data
                                                s_layout                    = is_layout
                                                t_fieldcatalog              = lt_fcat
                                                t_sort                      = it_sort
                                                t_filter                    = it_filt
                                                t_hyperlinks                = it_hyperlinks )
      IMPORTING
        er_result_file       = r_xstring ).

  ENDMETHOD.


  METHOD create_dynamic_structure.

    DATA:
      l_comp_tab    TYPE cl_abap_structdescr=>component_table,

      l_comp_tab2   TYPE cl_abap_structdescr=>component_table,

      l_comp        TYPE cl_abap_structdescr=>component,

      l_comp2       TYPE cl_abap_structdescr=>component,

      lv_size       TYPE i,  "field size

      lv_decm       TYPE i,  "field decimal places.

      lv_tabix      TYPE numc1,  "field decimal places.

      lv_tab_name   TYPE char10,

      lo_new_tab    TYPE REF TO cl_abap_tabledescr,

      l_estrutura   TYPE REF TO cl_abap_structdescr,

      l_struc_final TYPE REF TO data,
      v_message     TYPE string.

    LOOP AT i_dyn_struct_props ASSIGNING FIELD-SYMBOL(<props>).


      ADD 1 TO lv_tabix.



      l_comp-name = <props>-field_name.

      CASE <props>-ftype.
        WHEN 'I'.
          l_comp-type = cl_abap_elemdescr=>get_i( ).
        WHEN 'C'.
          l_comp-type = cl_abap_elemdescr=>get_c( p_length = <props>-size ).

        WHEN 'N'.
          l_comp-type = cl_abap_elemdescr=>get_n( p_length = <props>-size ).
        WHEN 'D'.
          l_comp-type = cl_abap_elemdescr=>get_d( ).

*          When OTHERS.
          "Extend as desired to accomodate other Abap Types: 8 = INT8, F = Float, P = PACKED ,T = Time,X = Rawstring ...
      ENDCASE.


      APPEND l_comp TO l_comp_tab.
      CLEAR l_comp.
    ENDLOOP.


    IF l_comp_tab[] IS NOT INITIAL.
      SORT l_comp_tab.
      DELETE ADJACENT DUPLICATES FROM l_comp_tab.

      CLEAR: l_estrutura, lo_new_tab.

      TRY .

          l_estrutura = cl_abap_structdescr=>create( l_comp_tab ).

          TRY.

              lo_new_tab = cl_abap_tabledescr=>create( p_line_type  = l_estrutura

                                                       p_table_kind = cl_abap_tabledescr=>tablekind_std

                                                       p_unique     = abap_false ).

            CATCH cx_sy_table_attributes INTO DATA(v_tbl_attr_exc).
              v_message = v_tbl_attr_exc->get_text( ).
              " error message…

            CATCH cx_sy_table_key_specification INTO DATA(v_tbl_spec).
              " error message…
              v_message = v_tbl_spec->get_text( ).


          ENDTRY.

        CATCH cx_sy_struct_attributes INTO DATA(v_str_attr).
          " error message…
          v_message = v_str_attr->get_text( ).
        CATCH cx_sy_struct_comp_name INTO DATA(v_name_exc).
          v_message = v_name_exc->get_text(
*                              preserve_newlines =
                         ).
          " error message…

        CATCH cx_sy_struct_comp_type INTO DATA(v_type_excp).
          v_message = v_type_excp->get_longtext(
*                             preserve_newlines =
                   ).
          " error message…

        CATCH cx_sy_struct_suffix_name INTO DATA(v_str_sfx_name_exc).
          " error message…
          v_message = v_str_sfx_name_exc->get_text(
*                              preserve_newlines =
                                  ).
      ENDTRY.

    ENDIF.
    TRY.

*    CLEAR: l_estrutura.

*    l_estrutura = cl_abap_structdescr=>create( l_comp_tab2 ).

        CREATE DATA l_struc_final TYPE HANDLE l_estrutura.

      CATCH cx_sy_struct_comp_name INTO v_name_exc.
        " error message…
        v_message = v_name_exc->get_text(
*                              preserve_newlines =
                        ).

      CATCH cx_sy_struct_creation INTO DATA(v_str_crea_exc).
        " error message…
        v_message = v_str_crea_exc->get_text(
*                              preserve_newlines =
                        ).

      CATCH cx_sy_table_creation INTO DATA(v_tbl_crea_exc).
        " error message…
        v_message = v_tbl_crea_exc->get_text( ).
    ENDTRY.
    CHECK l_struc_final IS BOUND.
*    ASSIGN l_struc_final->* to
    e_dyn_struct = l_estrutura.
  ENDMETHOD.


  METHOD extract_bpath_filter.
    SPLIT i_filter AT '&' INTO TABLE DATA(filters).

    LOOP AT filters ASSIGNING FIELD-SYMBOL(<single_filter>).
      SPLIT <single_filter> AT '=' INTO TABLE DATA(entity_filter_detail).
      READ TABLE entity_filter_detail INDEX 1 ASSIGNING FIELD-SYMBOL(<filter_field>).
      REPLACE ALL OCCURRENCES OF '@' IN <filter_field> WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN <filter_field> WITH ''.
      READ TABLE entity_filter_detail INDEX 2 ASSIGNING FIELD-SYMBOL(<filter_value>).
      REPLACE ALL OCCURRENCES OF ']' IN <filter_value> WITH ''.
      REPLACE ALL OCCURRENCES OF '"' IN <filter_value> WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN <filter_value> WITH ''.
      APPEND VALUE #( s_key = <filter_field> s_value = <filter_value> ) TO filter_data.
    ENDLOOP.




  ENDMETHOD.


  METHOD get_and_create_entity_by_bpath.

    DATA: temp_bpath             TYPE string,
          entity_pattern         TYPE string,
          to_be_created_entities TYPE TABLE OF string,
          lv_sub_string_entity   TYPE string.
    TRY.
        r_entity = i_entity->get_related_entities_by_bpath( iv_bpath_statement = i_bpath )->get_first( ).
      CATCH cx_root INTO DATA(v_exception).
        DATA(v_ignore) = abap_true.
    ENDTRY.
    IF r_entity IS INITIAL AND i_create = abap_true.
      entity_pattern ='/'.
      SPLIT i_bpath AT entity_pattern INTO TABLE DATA(entity_split_table).
      DESCRIBE TABLE entity_split_table LINES DATA(entity_lines).
      WHILE entity_lines > 0.
        READ TABLE  entity_split_table INDEX entity_lines - 1 ASSIGNING FIELD-SYMBOL(<to_create_entity_and_filter>).
        CLEAR temp_bpath.

        LOOP AT entity_split_table FROM 0 TO entity_lines  - 2 ASSIGNING FIELD-SYMBOL(<single_entity_matche>).
          IF temp_bpath IS INITIAL.
            temp_bpath = <single_entity_matche>.
          ELSE.
            temp_bpath = temp_bpath && entity_pattern && <single_entity_matche>.
          ENDIF.
        ENDLOOP.

        temp_bpath = temp_bpath && entity_pattern && '*$'.


        DATA(temp_entity) = i_entity->get_related_entities_by_bpath( iv_bpath_statement = temp_bpath )->get_first(  ).
        IF temp_entity IS BOUND AND temp_entity IS NOT INITIAL.

          SPLIT <to_create_entity_and_filter> AT '[' INTO TABLE DATA(entity_and_filter_table).
          READ TABLE entity_and_filter_table INDEX 1 ASSIGNING FIELD-SYMBOL(<to_create_entity>).
          r_entity = temp_entity->create_related_entity( iv_relation_name = CONV crmt_relation_name( <to_create_entity> )  ).
          READ TABLE entity_and_filter_table INDEX 2 ASSIGNING FIELD-SYMBOL(<to_create_entity_filter_val>).
          IF sy-subrc = 0 AND <to_create_entity_filter_val> IS ASSIGNED.
            DATA(filter_data) = zcl_api_util=>extract_bpath_filter(   <to_create_entity_filter_val> ).
            LOOP AT filter_data ASSIGNING FIELD-SYMBOL(<filter_data>).
              r_entity->set_property_as_string( iv_attr_name = CONV name_komp( <filter_data>-s_key ) iv_value = <filter_data>-s_value  ).
            ENDLOOP.
          ENDIF.

          entity_lines = 0.
        ELSE.
          INSERT <to_create_entity_and_filter> INTO TABLE to_be_created_entities.
        ENDIF.
        entity_lines = entity_lines - 1.
      ENDWHILE..

      LOOP AT to_be_created_entities ASSIGNING <to_create_entity_and_filter>.
*         r_entity = r_entity->create_related_entity( )..
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD itab_to_xlsx.
    FIELD-SYMBOLS: <fs_data> TYPE ANY TABLE.

    CLEAR rv_xstring.
    ASSIGN iobj_data->* TO <fs_data>.

    TRY.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = DATA(lo_table)
          CHANGING  t_table      = <fs_data> ).

        DATA(lt_fcat) =
          cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns      = lo_table->get_columns( )
            r_aggregations = lo_table->get_aggregations( ) ).

        DATA(lo_result) =
          cl_salv_ex_util=>factory_result_data_table(
            r_data         = iobj_data
            t_fieldcatalog = lt_fcat ).

        cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
          EXPORTING
            xml_type      = if_salv_bs_xml=>c_type_xlsx
            xml_version   = cl_salv_bs_a_xml_base=>get_version( )
            r_result_data = lo_result
            xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
            gui_type      = if_salv_bs_xml=>c_gui_type_gui
          IMPORTING
            xml           = rv_xstring ).
      CATCH cx_root.
        CLEAR rv_xstring.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
