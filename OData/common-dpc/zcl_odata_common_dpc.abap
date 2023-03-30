class ZCL_ODATA_COMMON_DPC definition
  public
  inheriting from /IWBEP/CL_MGW_ABS_DATA
  create public

  global friends ZCL_ODATA_DPC_INJECTOR .

public section.

  constants CO_REQUESTED_ENTITY type STRING value 'REQUESTED_ENTITY' ##NO_TEXT.
  constants CO_REQUESTED_ACTION type STRING value 'REQUESTED_ACTION' ##NO_TEXT.
  constants CO_SYSTEM_ALIAS_INFO type STRING value 'DEFAULT_SYS_ALIAS' ##NO_TEXT.
  constants:
    BEGIN OF co_action,
        read  TYPE string VALUE 'READ' ##NO_TEXT,
        write TYPE string VALUE 'WRITE' ##NO_TEXT,
      END OF co_action .

  methods CONSTRUCTOR .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_STREAM
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~DELETE_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~UPDATE_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_CORE_SRV_RUNTIME~INIT
    redefinition .
  PROTECTED SECTION.


  PRIVATE SECTION.



    DATA odata_model                TYPE REF TO /iwbep/if_mgw_odata_re_model .
    DATA service_application        TYPE REF TO zif_odata_application.
    DATA odata_modules              TYPE zif_odata_module=>ty_module_registration.
    DATA v_batch_mode               TYPE crmt_boolean.
    DATA logger TYPE REF TO /iwbep/cl_cos_logger.
    DATA default_sys_alias_info TYPE /iwbep/s_defi_alias_info.

    METHODS get_model
      RETURNING
        VALUE(rv_result) TYPE REF TO /iwbep/if_mgw_odata_re_model  .

    METHODS get_internal_service_info
      EXPORTING
        out_service_name    TYPE /iwbep/med_grp_technical_name
        out_service_version TYPE /iwbep/med_grp_version.

    METHODS get_annotation_data
      IMPORTING
        i_anno_tech_name TYPE /iwfnd/med_mdl_va_file_te_name
        i_anno_version   TYPE /iwfnd/med_mdl_va_file_version
      RETURNING
        VALUE(r_result)  TYPE /iwcor/if_ds_ext_types=>schema_annotations_t .


    METHODS get_service_application
      IMPORTING
        i_service_name    TYPE /iwbep/med_grp_technical_name
        i_service_version TYPE /iwbep/med_grp_version
      RETURNING
        VALUE(r_result)   TYPE  REF TO zif_odata_application.


    METHODS get_entity_implementation
      IMPORTING
        i_entity_name   TYPE /iwbep/mgw_tech_name
      RETURNING
        VALUE(r_result) TYPE REF TO zif_odata_entity
      RAISING
        /iwbep/cx_mgw_not_impl_exc.


    METHODS create_mpc_entity_struct
      IMPORTING
        i_entity_name   TYPE /iwbep/mgw_tech_name
      RETURNING
        VALUE(r_result) TYPE REF TO data
      RAISING
        /iwbep/cx_mgw_tech_exception.
    METHODS get_default_modules
      IMPORTING
        i_odata_app     TYPE REF TO zif_odata_application
      RETURNING
        VALUE(r_result) TYPE zif_odata_module=>ty_module_registration.

    METHODS create_mpc_entity_table
      IMPORTING
                i_entity_name   TYPE /iwbep/mgw_tech_name
      RETURNING VALUE(r_result) TYPE REF TO data .
    METHODS exec_pre_request_md.

    METHODS process_children
      IMPORTING
        i_main_node             TYPE  /iwbep/if_mgw_odata_fw_prop=>ty_t_mgw_odata_properties
        i_navigation_properties TYPE /iwbep/if_mgw_med_odata_types=>ty_t_med_reference
        i_property_mapping      TYPE /iwbep/if_mgw_med_odata_types=>ty_t_med_mapping
        i_main_entity_attr      TYPE any
*        i_nav_property_name     type string
      CHANGING
        i_source_structure      TYPE any
        i_children_tab          TYPE /iwbep/if_mgw_odata_expand=>ty_t_node_children.
    METHODS do_before_request
      IMPORTING
        i_entity_name TYPE csequence
        i_action      TYPE csequence
      RAISING
        /iwbep/cx_mgw_tech_exception.
    METHODS do_after_request
      IMPORTING
        i_entity_data TYPE data OPTIONAL.

    METHODS exec_post_request_md
      IMPORTING
        i_entity_data TYPE data OPTIONAL.

ENDCLASS.



CLASS ZCL_ODATA_COMMON_DPC IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.
    me->v_batch_mode = abap_true.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_end.
    IF me->v_batch_mode = abap_true.
      TRY.
          me->service_application->finish_process(  ).
        CATCH /iwbep/cx_mgw_busi_exception INTO DATA(v_busi_exception).
          RAISE EXCEPTION v_busi_exception.
        CATCH zcx_gateway_exception INTO DATA(v_exception).
          v_exception->throw_gateway_exception( ) .
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

    DATA v_child_node TYPE REF TO /iwbep/cl_mgw_expand_node.
    DATA v_expanded_node TYPE REF TO /iwbep/cl_mgw_expand_node.
    DATA v_odata_model2 TYPE REF TO /iwbep/cl_mgw_odata_model.
    DATA v_tutorial_mode_flag TYPE crmt_boolean.

    DATA:
      lo_struct_descr  TYPE REF TO cl_abap_structdescr,
      lr_source_struct TYPE REF TO data.
    FIELD-SYMBOLS: <fs_child_entities> TYPE ANY TABLE.
    FIELD-SYMBOLS: <fs_child_entity_struc> TYPE any.

    FIELD-SYMBOLS:    <ls_source_structure> TYPE any.

*-- GET entity name
    DATA(v_entity_name)    = io_tech_request_context->get_entity_type_name( ).

*-- Parse and Pre process HTTP Request
    me->do_before_request( i_entity_name = v_entity_name i_action = zcl_odata_common_dpc=>co_action-write  ).

    DATA(v_children_tab) = io_expand->get_children( ).


    v_expanded_node ?= io_expand.

    v_expanded_node->check_conversions( ).
    lo_struct_descr ?= v_expanded_node->create_data_descriptor( ).

**********************************************************************
* first start with main entity
**********************************************************************

    CREATE DATA lr_source_struct TYPE HANDLE lo_struct_descr.
    ASSIGN lr_source_struct->* TO <ls_source_structure>.

    io_data_provider->read_entry_data( IMPORTING es_data = <ls_source_structure> ).



    DATA(v_entity_hanlder) = get_entity_implementation(  v_entity_name ).

    DATA(v_entity_struct)  = create_mpc_entity_struct( v_entity_name ).

    ASSIGN v_entity_struct->* TO FIELD-SYMBOL(<fs_entity_attr>).

    IF sy-subrc IS NOT INITIAL.
      ASSERT 1 = 2.

    ENDIF.

    MOVE-CORRESPONDING <ls_source_structure> TO <fs_entity_attr>.



    TRY.
        DATA(v_contract_guid) = me->service_application->get_current_user(  )->get_portal_contract( )->get_id(  ).
        v_tutorial_mode_flag = me->service_application->get_context_attribute( zcl_odata_authentication_md=>co_tutorial_mode_flag ).
      CATCH cx_sy_ref_is_initial.
    ENDTRY.
    TRY .

*--- in case of tutorial mode, raise error, because any change is not allowed in tutorial mode
        IF v_tutorial_mode_flag EQ abap_true.
          RAISE EXCEPTION TYPE zcx_gateway_exception
            EXPORTING
              textid        = zcx_cua_btx_general=>error
              error_msg     = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = zif_const_bf_cua=>gc_cua_otr-tutorial_mode
                                                                            language = sy-langu )
              callstack     = zcl_cua_bxt_tools=>get_callstack( )
              kind_of_error = 'B'.

        ENDIF.

        v_entity_hanlder->create( EXPORTING
                                     i_new_entity   = <fs_entity_attr>
                                     i_entity_name  = v_entity_name
                                     i_contract_guid = v_contract_guid
                                  IMPORTING
                                    e_entity       =  <fs_entity_attr>
         ).

        MOVE-CORRESPONDING <fs_entity_attr> TO <ls_source_structure>.
      CATCH zcx_gateway_exception INTO DATA(v_exception).
        v_exception->throw_gateway_exception( ) .
    ENDTRY.


**********************************************************************
* loop over child, and set key and call single entity
**********************************************************************
    DATA(v_odata_model)      = me->get_model( ).
    v_odata_model2 ?= me->get_model( ).

    DATA(v_navigation_properties) = v_odata_model->get_navigation_properties( ).
    DATA(v_property_mapping) = v_odata_model2->get_mapping(  ).

    DATA(v_main_node_properties) = v_expanded_node->get_properties(  ).

    TRY.

        me->process_children( EXPORTING
                                  i_main_node = v_main_node_properties
                                  i_navigation_properties = v_navigation_properties
                                  i_property_mapping    = v_property_mapping
                                  i_main_entity_attr    = <fs_entity_attr>
                               CHANGING
                                  i_source_structure    = <ls_source_structure>
                                  i_children_tab = v_children_tab ).
      CATCH zcx_gateway_exception INTO v_exception.
        v_exception->throw_gateway_exception( ) .
    ENDTRY.



    IF me->v_batch_mode = abap_false.
      TRY.
          me->service_application->finish_process(  ).
        CATCH zcx_gateway_exception INTO v_exception.
          v_exception->throw_gateway_exception( ) .
      ENDTRY.
*      zcl_crm_transaction_bol=>get_instance( )->save(  ).
    ENDIF.

    MOVE-CORRESPONDING <fs_entity_attr> TO <ls_source_structure>.
    copy_data_to_ref(
         EXPORTING
         is_data = <ls_source_structure>
         CHANGING
         cr_data = er_deep_entity
         ).



*-- Do post Request processing HTTP Request
    me->do_after_request( i_entity_data = <ls_source_structure> ).

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_entity.
    "#TODO handle exception for create entity
    "#TODO Handle metadata validation,  only fields marked as creatable will be mapped
    "#TODO Handle user authorization
    DATA: v_contract_guid TYPE crmt_object_guid.
    DATA: v_tutorial_mode_flag TYPE crmt_boolean.
    DATA: v_tech_request_context TYPE REF TO /iwbep/cl_mgw_request,
          v_objref               TYPE REF TO object,
          t_parameter_tab        TYPE abap_parmbind_tab,
          t_callstack            TYPE abap_callstack.

*-- GET Entity name
    DATA(v_entity_name)    = io_tech_request_context->get_entity_type_name( ).

*-- Parse and Preprocess HTTP Request
    me->do_before_request( i_entity_name = v_entity_name i_action = zcl_odata_common_dpc=>co_action-write ).

*-- Identify which Entity Handler should handle the request based on the OData entity name
    TRY.

        DATA(v_entity_handler) = get_entity_implementation(  v_entity_name ).

        DATA(v_entity_struct)  = create_mpc_entity_struct( v_entity_name ).

        ASSIGN v_entity_struct->* TO FIELD-SYMBOL(<fs_entity_attr>).

        IF sy-subrc IS NOT INITIAL.
          ASSERT 1 = 2.

        ENDIF.

        io_data_provider->read_entry_data( IMPORTING es_data = <fs_entity_attr> ).


        TRY.
            v_contract_guid = me->service_application->get_current_user(  )->get_portal_contract( )->get_id(  ).
            v_tutorial_mode_flag = me->service_application->get_context_attribute( zcl_odata_authentication_md=>co_tutorial_mode_flag ).
          CATCH cx_sy_ref_is_initial.
        ENDTRY.

*--- in case of tutorial mode, raise error, because any change is not allowed in tutorial mode
        IF v_tutorial_mode_flag EQ abap_true.
          RAISE EXCEPTION TYPE zcx_gateway_exception
            EXPORTING
              textid        = zcx_cua_btx_general=>error
              error_msg     = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = zif_const_bf_cua=>gc_cua_otr-tutorial_mode
                                                                            language = sy-langu )
              callstack     = zcl_cua_bxt_tools=>get_callstack( )
              kind_of_error = 'B'.
        ENDIF.

        TRY.

            v_entity_handler->create( EXPORTING
                                         i_new_entity    = <fs_entity_attr>
                                         i_contract_guid = v_contract_guid
                                         i_tutorial_mode = ''
                                         i_entity_name   = v_entity_name
                                      IMPORTING
                                        e_entity       =  <fs_entity_attr>
             ).
          CATCH zcx_gateway_exception INTO DATA(v_exception).
            v_exception->throw_gateway_exception( ) .
          CATCH cx_root INTO DATA(v_root).

            RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception .
        ENDTRY.


*-- DO post Request processing HTTP Request
        me->do_after_request( i_entity_data = <fs_entity_attr> ).

        IF me->v_batch_mode = abap_false.
          TRY.
              me->service_application->finish_process(  ).
            CATCH zcx_gateway_exception INTO v_exception.
              v_exception->throw_gateway_exception( ) .
          ENDTRY.

        ENDIF.


        er_entity = v_entity_struct.
      CATCH /iwbep/cx_mgw_not_impl_exc INTO DATA(lx_common_dpc_exception).
*-- Fallback with a try to call the standard DPC class to handle the request
        TRY.
            v_tech_request_context ?= io_tech_request_context.
            DATA(v_service_doc_name) = v_tech_request_context->get_request_details( )-service_doc_name.
            DATA(v_standard_dpc_class) = 'ZCL_' && substring_before( val = v_service_doc_name sub = '_SRV' ) && '_DPC_EXT'.
            CREATE OBJECT v_objref TYPE (v_standard_dpc_class).
            CALL FUNCTION 'SYSTEM_CALLSTACK'
              EXPORTING
                max_level = 1
              IMPORTING
                callstack = t_callstack.

            DATA(v_method_name) = t_callstack[ 1 ]-blockname.
            t_parameter_tab = VALUE #(
                  ( name  = 'IV_ENTITY_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_entity_name ) )
                  ( name  = 'IV_ENTITY_SET_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_entity_set_name ) )
                  ( name  = 'IV_SOURCE_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_source_name ) )
                  ( name  = 'IO_DATA_PROVIDER'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( io_data_provider ) )
                  ( name  = 'IT_KEY_TAB'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( it_key_tab ) )
                  ( name  = 'IT_NAVIGATION_PATH'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( it_navigation_path ) )
                  ( name  = 'IO_TECH_REQUEST_CONTEXT'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( io_tech_request_context ) )
                  ( name  = 'ER_ENTITY'
                    kind  = cl_abap_objectdescr=>importing
                    value = REF #( er_entity ) ) ).

            CALL METHOD v_objref->(v_method_name)
              PARAMETER-TABLE
              t_parameter_tab.
          CATCH cx_sy_dyn_call_error cx_sy_itab_line_not_found.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc MESSAGE e001(zodata_cdpc).
        ENDTRY.
    ENDTRY.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_stream.
    DATA: v_entity_name    TYPE /iwbep/mgw_tech_name.

    v_entity_name          = io_tech_request_context->get_entity_type_name( ).

*-- Parse and Preprocess HTTP Request
    me->do_before_request( i_entity_name = v_entity_name i_action = zcl_odata_common_dpc=>co_action-write ).

*DATA(is_media) = io_tech_request_context->
    DATA(v_entity_struct)  = create_mpc_entity_struct( v_entity_name ).

    ASSIGN v_entity_struct->* TO FIELD-SYMBOL(<fs_entity_attr>).

    IF sy-subrc IS NOT INITIAL.
      ASSERT 1 = 2.

    ENDIF.

    TRY.

*-- Get Entity handler which will process the request
        DATA(v_entity_handler) = get_entity_implementation( v_entity_name ).


        v_entity_handler->create_stream( EXPORTING
                                               i_key_tab        = it_key_tab
                                               i_new_stream     = VALUE #( file_name = iv_slug
                                                                           content   = is_media_resource-value
                                                                           mime_type = is_media_resource-mime_type )
                                         IMPORTING
                                               e_stream = v_entity_struct
                                         ).
      CATCH zcx_gateway_exception INTO DATA(v_exception).
        v_exception->throw_gateway_exception( ) .
      CATCH cx_root.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception .
    ENDTRY.


*-- DO post Request processing HTTP Request
    me->do_after_request( i_entity_data = <fs_entity_attr> ).

    IF me->v_batch_mode = abap_false.
      TRY.
          me->service_application->finish_process(  ).
        CATCH zcx_gateway_exception INTO v_exception.
          v_exception->throw_gateway_exception( ) .
      ENDTRY.

    ENDIF.


    er_entity = v_entity_struct.


  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~delete_entity.



*-- Parse and Preprocess HTTP Request
    "    me->do_before_request( i_action = zcl_odata_common_dpc=>co_action-write ).

    DATA: v_entity_name TYPE /iwbep/mgw_tech_name.
    DATA: v_tutorial_mode_flag TYPE crmt_boolean.
    DATA: v_tech_request_context TYPE REF TO /iwbep/cl_mgw_request,
          v_objref               TYPE REF TO object,
          t_parameter_tab        TYPE abap_parmbind_tab,
          t_callstack            TYPE abap_callstack.

*-- Get requested Entity name
    v_entity_name          = io_tech_request_context->get_entity_type_name( ).

*-- Parse and Preprocess HTTP Request
    me->do_before_request(  i_entity_name = v_entity_name i_action = zcl_odata_common_dpc=>co_action-write ).



    TRY.

*-- Get Entity handler which will process the request
        DATA(v_entity_handler) = get_entity_implementation( v_entity_name ).

*        DATA(v_entity_struct)  = create_mpc_entity_struct( v_entity_name ).
*
*        ASSIGN v_entity_struct->* TO FIELD-SYMBOL(<fs_entity_attr>).

        IF sy-subrc IS NOT INITIAL.
          ASSERT 1 = 2.

        ENDIF.

        TRY.
            v_tutorial_mode_flag = me->service_application->get_context_attribute( zcl_odata_authentication_md=>co_tutorial_mode_flag ).
          CATCH cx_sy_ref_is_initial.
        ENDTRY.

*--- in case of tutorial mode, raise error, because any change is not allowed in tutorial mode
        IF v_tutorial_mode_flag EQ abap_true.
          RAISE EXCEPTION TYPE zcx_gateway_exception
            EXPORTING
              textid        = zcx_cua_btx_general=>error
              error_msg     = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = zif_const_bf_cua=>gc_cua_otr-tutorial_mode
                                                                            language = sy-langu )
              callstack     = zcl_cua_bxt_tools=>get_callstack( )
              kind_of_error = 'B'.
        ENDIF.


        v_entity_handler->delete(  EXPORTING
                                   i_key                    = io_tech_request_context->get_keys( )
                                   i_key_tab                = it_key_tab
                                   i_entity_name            = v_entity_name
                               ).


*-- Do post Request processing HTTP Request

        me->do_after_request(  ).

        IF me->v_batch_mode = abap_false.
          me->service_application->finish_process(  ).
        ENDIF.

*-- Check if the user has authorization to Read the Entity

      CATCH zcx_gateway_exception INTO DATA(v_exception).
        v_exception->throw_gateway_exception( ) .
      CATCH /iwbep/cx_mgw_not_impl_exc INTO DATA(lx_common_dpc_exception).
*-- Fallback with a try to call the standard DPC class to handle the request
        TRY.
            v_tech_request_context ?= io_tech_request_context.
            DATA(v_service_doc_name) = v_tech_request_context->get_request_details( )-service_doc_name.
            DATA(v_standard_dpc_class) = 'ZCL_' && substring_before( val = v_service_doc_name sub = '_SRV' ) && '_DPC_EXT'.
            CREATE OBJECT v_objref TYPE (v_standard_dpc_class).
            CALL FUNCTION 'SYSTEM_CALLSTACK'
              EXPORTING
                max_level = 1
              IMPORTING
                callstack = t_callstack.

            DATA(v_method_name) = t_callstack[ 1 ]-blockname.
            t_parameter_tab = VALUE #(
                  ( name  = 'IV_ENTITY_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_entity_name ) )
                  ( name  = 'IV_ENTITY_SET_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_entity_set_name ) )
                  ( name  = 'IV_SOURCE_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_source_name ) )
                  ( name  = 'IT_KEY_TAB'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( it_key_tab ) )
                  ( name  = 'IT_NAVIGATION_PATH'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( it_navigation_path ) )
                  ( name  = 'IO_TECH_REQUEST_CONTEXT'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( io_tech_request_context ) ) ).

            CALL METHOD v_objref->(v_method_name)
              PARAMETER-TABLE
              t_parameter_tab.
          CATCH cx_sy_dyn_call_error cx_sy_itab_line_not_found.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc MESSAGE e001(zodata_cdpc).
        ENDTRY.
      CATCH cx_root INTO DATA(v_root_exception).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            previous = v_root_exception.
    ENDTRY.
*-- Parse and Preprocess HTTP Request
    "me->do_before_request( i_entity_name = v_entity_name i_action = zcl_odata_common_dpc=>co_action-write  ).

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.

    DATA: io_context  TYPE REF TO /iwbep/cl_mgw_request,
          lv_act_name TYPE /iwbep/if_mgw_med_odata_types=>ty_e_med_entity_name.

    DATA: v_tech_request_context TYPE REF TO /iwbep/cl_mgw_request,
          v_objref               TYPE REF TO object,
          t_parameter_tab        TYPE abap_parmbind_tab,
          t_callstack            TYPE abap_callstack.

    FIELD-SYMBOLS <fs_results> TYPE STANDARD TABLE.

    io_context ?= io_tech_request_context.


    DATA(v_req_details) =  io_context->get_request_details( ).

    DATA(v_entity_name) = v_req_details-source_entity.

    DATA(lo_model)   =  me->get_model( ).


*-- Parse and Pre-process HTTP Request
    me->do_before_request( i_entity_name = iv_action_name
                           i_action = zcl_odata_common_dpc=>co_action-read ).


    IF lo_model IS BOUND.
      lv_act_name  = iv_action_name.

      CASE lo_model->get_action( lv_act_name )->get_return_multiplicity( ).

        WHEN /iwbep/if_mgw_med_odata_types=>gcs_cardinality-cardinality_0_1 OR
                     /iwbep/if_mgw_med_odata_types=>gcs_cardinality-cardinality_1_1.
          DATA(v_return_is_table) = abap_false.
*          DATA(v_entity_struct)  = create_mpc_entity_struct( CONV #( v_entity_name ) ).
*
*          ASSIGN v_entity_struct->* TO FIELD-SYMBOL(<fs_entity_attr>).

        WHEN /iwbep/if_mgw_med_odata_types=>gcs_cardinality-cardinality_0_n OR
                   /iwbep/if_mgw_med_odata_types=>gcs_cardinality-cardinality_1_n.
*          DATA(v_entity_table)  = create_mpc_entity_table( CONV #( v_entity_name ) ).
          v_return_is_table = abap_true.
*          ASSIGN v_entity_table->* TO <fs_entity_attr>.
      ENDCASE.
    ENDIF.

    DATA(v_entity_struct)  = create_mpc_entity_struct( CONV #( v_entity_name ) ).
    ASSIGN v_entity_struct->* TO FIELD-SYMBOL(<fs_entity_attr>).

    DATA(v_entity_table)  = create_mpc_entity_table( CONV #( v_entity_name ) ).
    ASSIGN v_entity_table->* TO <fs_results>.
    TRY.
        DATA(v_entity_handler) = get_entity_implementation( CONV #( v_entity_name ) ).

        TRY.
            v_entity_handler->handle_action(

               EXPORTING
                    i_action        = iv_action_name  " Character Field Length = 10
                    i_parameters    = it_parameter   " table for name value pairs
                    i_entity_name   = CONV #( v_entity_name )
                   IMPORTING er_entity =  <fs_results>
            ).


*-- Do post Request processing HTTP Request
            "me->do_after_request( i_entity_data = <fs_results> ).


          CATCH zcx_gateway_exception INTO DATA(v_exception).
            v_exception->throw_gateway_exception( ) .
          CATCH zcx_gateway_simple_exception INTO DATA(o_simple_exception).
            o_simple_exception->throw_gateway_exception( ).
          CATCH /iwbep/cx_mgw_busi_exception INTO DATA(v_busi_exception).
            RAISE EXCEPTION v_busi_exception.
          CATCH cx_root INTO DATA(v_root_exception).

            RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
              EXPORTING
                previous = v_root_exception.
        ENDTRY.

        CASE v_return_is_table.
          WHEN abap_true.
            IF line_exists( <fs_results>[ 1 ] ).
              copy_data_to_ref(
                       EXPORTING
                         is_data = <fs_results>
                       CHANGING
                         cr_data = er_data
                     ).
            ENDIF.

          WHEN abap_false.
            IF line_exists( <fs_results>[ 1 ] ).
              copy_data_to_ref(
                        EXPORTING
                          is_data = <fs_results>[ 1 ]
                        CHANGING
                          cr_data = er_data
                      ).
            ENDIF.

          WHEN OTHERS.
        ENDCASE.
*    COND #( WHEN v_entity_table IS NOT INITIAL THEN v_entity_table
*    WHEN <fs_entity_attr> IS NOT INITIAL THEN <fs_entity_attr> ).
        IF me->v_batch_mode = abap_false.
          TRY.
              me->service_application->finish_process(  ).
            CATCH zcx_gateway_exception INTO v_exception.
              v_exception->throw_gateway_exception( ) .
          ENDTRY.
        ENDIF.
      CATCH /iwbep/cx_mgw_not_impl_exc INTO DATA(lx_common_dpc_exception).
*-- Fallback with a try to call the standard DPC class to handle the request
        TRY.
            v_tech_request_context ?= io_tech_request_context.
            DATA(v_service_doc_name) = v_tech_request_context->get_request_details( )-service_doc_name.
            DATA(v_standard_dpc_class) = 'ZCL_' && substring_before( val = v_service_doc_name sub = '_SRV' ) && '_DPC_EXT'.
            CREATE OBJECT v_objref TYPE (v_standard_dpc_class).
            CALL FUNCTION 'SYSTEM_CALLSTACK'
              EXPORTING
                max_level = 1
              IMPORTING
                callstack = t_callstack.

            DATA(v_method_name) = t_callstack[ 1 ]-blockname.
            t_parameter_tab = VALUE #(
                  ( name  = 'IV_ACTION_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_action_name ) )
                  ( name  = 'IT_PARAMETER'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( it_parameter ) )
                  ( name  = 'IO_TECH_REQUEST_CONTEXT'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( io_tech_request_context ) )
                  ( name  = 'ER_DATA'
                    kind  = cl_abap_objectdescr=>importing
                    value = REF #( er_data ) ) ).

            CALL METHOD v_objref->(v_method_name)
              PARAMETER-TABLE
              t_parameter_tab.
          CATCH cx_sy_dyn_call_error cx_sy_itab_line_not_found.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc MESSAGE e001(zodata_cdpc).
        ENDTRY.
    ENDTRY.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entity.
    DATA: v_entity_name TYPE /iwbep/mgw_tech_name.
    DATA: v_tech_request_context TYPE REF TO /iwbep/cl_mgw_request,
          v_objref               TYPE REF TO object,
          t_parameter_tab        TYPE abap_parmbind_tab,
          t_callstack            TYPE abap_callstack.


*-- Get requested Entity name
    v_entity_name          = io_tech_request_context->get_entity_type_name( ).

*-- Parse and Preprocess HTTP Request
    me->do_before_request(  i_entity_name = v_entity_name i_action = zcl_odata_common_dpc=>co_action-read ).



    TRY.

*-- Get Entity handler which will process the request
        DATA(v_entity_handler) = get_entity_implementation( v_entity_name ).


        DATA(v_entity_property_tab) = me->get_model( )->get_entity_type( v_entity_name )->get_properties( ).
        DATA(v_select_tab)          = io_tech_request_context->get_select( ).

        IF v_select_tab[] IS INITIAL.

          v_select_tab = VALUE #( FOR v_property IN v_entity_property_tab (  v_property-technical_name  ) ).

        ENDIF.

        DATA(v_entity_struct) = create_mpc_entity_struct( v_entity_name ).

        ASSIGN v_entity_struct->* TO FIELD-SYMBOL(<fs_entity_attr>).


*-- Convert keys to ABAP internal representation
        DATA(v_converted_key_tab) = io_tech_request_context->get_keys( ).

        io_tech_request_context->get_converted_keys( IMPORTING es_key_values = <fs_entity_attr> ).

        LOOP AT v_converted_key_tab ASSIGNING FIELD-SYMBOL(<fs_converted_key>).
          ASSIGN COMPONENT <fs_converted_key>-name OF STRUCTURE <fs_entity_attr> TO FIELD-SYMBOL(<fs_value>).
          IF <fs_value> IS ASSIGNED AND <fs_value> IS NOT INITIAL.
            <fs_converted_key>-value = <fs_value>.
          ENDIF.
        ENDLOOP.





*-- Dispatch the call to entity handler which will perform the read operation
        DATA v_value TYPE zcl_crm_exas_mpc=>ts_admissionrequest.
        io_tech_request_context->get_converted_keys( IMPORTING es_key_values = v_value  ).

        v_entity_handler->read(  EXPORTING
                                   i_key                    = v_converted_key_tab
                                   i_navigation_tab         = it_navigation_path
                                   i_key_tab                = it_key_tab
                                   i_select_column_tab      = CONV #( v_select_tab )
                                   i_source_key_tab         = io_tech_request_context->get_source_keys( )
                                   i_entity_name            = v_entity_name
                                  IMPORTING
                                  e_entity                  = <fs_entity_attr>
                               ).


*-- Do post Request processing HTTP Request
        me->do_after_request( i_entity_data = <fs_entity_attr> ).
*        IF me->v_batch_mode = abap_false.
*          me->service_application->finish_process(  ).
*        ENDIF.

        er_entity = v_entity_struct.


      CATCH /iwbep/cx_mgw_not_impl_exc INTO DATA(lx_common_dpc_exception).
*-- Fallback with a try to call the standard DPC class to handle the request
        TRY.
            v_tech_request_context ?= io_tech_request_context.
            DATA(v_service_doc_name) = v_tech_request_context->get_request_details( )-service_doc_name.
            DATA(v_standard_dpc_class) = 'ZCL_' && substring_before( val = v_service_doc_name sub = '_SRV' ) && '_DPC_EXT'.
            CREATE OBJECT v_objref TYPE (v_standard_dpc_class).
            CALL FUNCTION 'SYSTEM_CALLSTACK'
              EXPORTING
                max_level = 1
              IMPORTING
                callstack = t_callstack.

            DATA(v_method_name) = t_callstack[ 1 ]-blockname.
            t_parameter_tab = VALUE #( ( name  = 'IV_ENTITY_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_entity_name ) )
                  ( name  = 'IV_ENTITY_SET_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_entity_set_name ) )
                  ( name  = 'IV_SOURCE_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_source_name ) )
                    ( name  = 'IT_KEY_TAB'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( it_key_tab ) )
                    ( name  = 'IT_NAVIGATION_PATH'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( it_navigation_path ) )
                    ( name  = 'IO_TECH_REQUEST_CONTEXT'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( io_tech_request_context ) )
                    ( name  = 'ER_ENTITY'
                    kind  = cl_abap_objectdescr=>importing
                    value = REF #( er_entity ) )
                  ( name  = 'ES_RESPONSE_CONTEXT'
                    kind  = cl_abap_objectdescr=>importing
                    value = REF #( es_response_context ) ) ).

            CALL METHOD v_objref->(v_method_name)
              PARAMETER-TABLE
              t_parameter_tab.
          CATCH cx_sy_dyn_call_error cx_sy_itab_line_not_found.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc MESSAGE e001(zodata_cdpc).
        ENDTRY.

*-- Check if the user has authorization to Read the Entity
      CATCH zcx_gateway_exception INTO DATA(o_gw_exception).
        o_gw_exception->throw_gateway_exception( ) .

      CATCH cx_root INTO DATA(v_exception).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            previous = v_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.

    DATA v_entity_name  TYPE /iwbep/mgw_tech_name .
    DATA v_entity_count TYPE i.
    DATA v_odata_model2 TYPE REF TO /iwbep/cl_mgw_odata_model.
    DATA: v_unit_code_tab     TYPE /iwbep/cl_mgw_data_conv_util=>ty_t_unit_code,
          v_currency_code_tab TYPE /iwbep/cl_mgw_data_conv_util=>ty_t_currency_codes,
          v_property_info_tab TYPE /iwbep/cl_mgw_data_conv_util=>ty_t_property_info,
          t_order_tab         TYPE /iwbep/t_mgw_sorting_order.
    DATA: v_tech_request_context TYPE REF TO /iwbep/cl_mgw_request,
          v_objref               TYPE REF TO object,
          t_parameter_tab        TYPE abap_parmbind_tab,
          t_callstack            TYPE abap_callstack.



    FIELD-SYMBOLS: <fs_query_result> TYPE STANDARD TABLE.

    TRY.

*-- Get requested Entity name
        v_entity_name        = io_tech_request_context->get_entity_type_name( ).

*-- Parse and Preprocess HTTP Request

        me->do_before_request( i_entity_name = v_entity_name
                               i_action      = zcl_odata_common_dpc=>co_action-read ).



*-- GET entity handler which will process the request
        DATA(v_entity_handler) = get_entity_implementation( v_entity_name ).

*-- Create entitySet return table
        DATA(v_enityset_tab)    = create_mpc_entity_table( i_entity_name = v_entity_name  ).
        DATA(v_entity_struct)   = create_mpc_entity_struct( v_entity_name ).

        ASSIGN v_enityset_tab->* TO <fs_query_result>.
        IF sy-subrc IS NOT INITIAL.
          RETURN.
        ENDIF.

*-- GET entity properties
        DATA(v_entity_property_tab) = me->get_model( )->get_entity_type( v_entity_name )->get_properties( ).


*-- GET request filters
        DATA(v_filter_tab)   = io_tech_request_context->get_filter( )->get_filter_select_options( ).

        v_odata_model2 ?= me->get_model( ).

        DATA(v_select_tab)          = io_tech_request_context->get_select( ).

        IF v_select_tab[] IS INITIAL.

          v_select_tab = VALUE #( FOR v_property IN v_entity_property_tab (  v_property-technical_name  ) ).

        ENDIF.

*-- Include Key for navigation scenarios
        LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<fs_key>).

          TRY.

              DATA(mapped_db_key_name) = v_odata_model2->mt_entities[ name = v_entity_name ]-properties[ external_name =  <fs_key>-name ]-name.

              APPEND VALUE #(
                property = mapped_db_key_name "<fs_key>-name
                select_options  = VALUE #( ( sign = 'I' option = 'EQ' low = <fs_key>-value  ) )
              ) TO v_filter_tab.

            CATCH cx_sy_itab_line_not_found INTO DATA(v_exception).

              mapped_db_key_name = <fs_key>-name.
              APPEND VALUE #(
                property = mapped_db_key_name "<fs_key>-name
                select_options  = VALUE #( ( sign = 'I' option = 'EQ' low = <fs_key>-value  ) )
              ) TO v_filter_tab.

          ENDTRY.



        ENDLOOP.

        IF 1 = 2.


          RAISE EXCEPTION TYPE zcx_gateway_technic_exception
            EXPORTING
              textid    = zcx_cua_btx_general=>error
              error_msg = v_exception->get_text( )
              callstack = zcl_cua_bxt_tools=>get_callstack( ).

        ENDIF.



*-- Convert filters values to internal format
        ASSIGN v_entity_struct->* TO FIELD-SYMBOL(<fs_entity_attr>).

        LOOP AT v_filter_tab ASSIGNING FIELD-SYMBOL(<fs_filter>).

          READ TABLE v_entity_property_tab WITH KEY technical_name = <fs_filter>-property
           INTO DATA(v_entity_property).
          IF sy-subrc IS INITIAL.

            ASSIGN COMPONENT to_upper( <fs_filter>-property ) OF STRUCTURE <fs_entity_attr> TO FIELD-SYMBOL(<fs_struct_value>).

            LOOP AT <fs_filter>-select_options ASSIGNING FIELD-SYMBOL(<fs_filter_options>).

              IF <fs_filter_options>-low IS NOT INITIAL.

                /iwbep/cl_mgw_data_conv_util=>convert_prop_inbound_with_ext( EXPORTING it_unit_code                = v_unit_code_tab
                                                                                       it_currency_code            = v_currency_code_tab
                                                                                       it_property_info            = v_property_info_tab
                                                                                       iv_source_property          = <fs_filter_options>-low
                                                                                       io_property                 = CAST #( v_entity_property-property  )     " OData Property FPI
                                                                             IMPORTING
                                                                                    ev_target_property          = <fs_struct_value>
                                                                                   ).

                <fs_filter_options>-low = <fs_struct_value>.

              ELSEIF <fs_filter_options>-high IS NOT INITIAL.

                /iwbep/cl_mgw_data_conv_util=>convert_prop_inbound_with_ext( EXPORTING  it_unit_code                = v_unit_code_tab
                                                                                        it_currency_code            = v_currency_code_tab
                                                                                        it_property_info            = v_property_info_tab
                                                                                        iv_source_property          = <fs_filter_options>-high
                                                                                        io_property                 = CAST #( v_entity_property-property  )     " OData Property FPI

                                                                              IMPORTING
                                                                                        ev_target_property          = <fs_struct_value>
                                                                                ).

                <fs_filter_options>-high = <fs_struct_value>.

              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDLOOP.

*-- Get Open SQL WHERE Clause from $filter
        DATA(v_osql_where)   = io_tech_request_context->get_osql_where_clause( ).

        io_tech_request_context->get_osql_where_clause_convert( ).


*-- Handle $skip and $top query string parameters
        DATA(v_top) =  COND #(  WHEN io_tech_request_context->get_top( ) IS INITIAL
                                THEN 100 "If $top was not sent, set maximum to 100 rows
                                ELSE io_tech_request_context->get_top( ) ).

        DATA(v_skip)       = io_tech_request_context->get_skip( ).
        DATA(v_total)      = v_top + v_skip.

*-- Handle $count query parameter
        DATA(v_has_count)    = io_tech_request_context->has_count( ).
        DATA(v_inline_count) = io_tech_request_context->has_inlinecount( ).

*-- Handle $orderby
        DATA(v_orderby_tab) = io_tech_request_context->get_orderby( ).

        IF v_orderby_tab[] IS NOT INITIAL.

          DATA  v_sort_element_tab  TYPE if_salv_ida_types_int=>yt_db_sort_rule.

          v_sort_element_tab = VALUE #(  FOR v_orderby IN v_orderby_tab (  field_path = v_orderby-property
                                                                           descending =  COND #( WHEN to_upper(  v_orderby-order ) EQ 'DESC' THEN  abap_true ELSE abap_false )  )

                                                                        ).

          "/iwbep/cl_mgw_data_util=>orderby( EXPORTING it_order = t_order_tab CHANGING ct_data = <fs_query_result>  ).

        ENDIF.


        IF v_has_count EQ abap_true.
          TRY.
              v_entity_count = v_entity_handler->count( EXPORTING
                                                            i_select_option_tab = v_filter_tab
                                                            i_osql_where        = v_osql_where
                                                            i_entity_name       = v_entity_name ).
            CATCH zcx_gateway_exception INTO DATA(v_gw_exception).
              v_gw_exception->throw_gateway_exception( ) .
            CATCH cx_root.

              RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception .
          ENDTRY.


        ELSE.
          TRY.

              v_entity_handler->query( EXPORTING
                                            i_select_option_tab = v_filter_tab
                                            i_osql_where        = v_osql_where
                                            i_top               = v_top
                                            i_skip              = v_skip
                                            i_select_column_tab = CONV #( v_select_tab )
                                            i_sort_element_tab  = v_sort_element_tab
                                            i_navigation_path   = it_navigation_path
                                            i_entity_name       = v_entity_name
                                         CHANGING
                                            c_query_result      = <fs_query_result>
                                        ).
            CATCH zcx_gateway_exception INTO v_gw_exception.
              v_gw_exception->throw_gateway_exception( ) .
            CATCH cx_root INTO DATA(v_root_exception).

              RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception .
          ENDTRY.
          IF v_inline_count EQ abap_true.
            v_entity_count = v_entity_handler->count( EXPORTING
                                                       i_select_option_tab = v_filter_tab
                                                       i_entity_name = v_entity_name ).
          ENDIF.
        ENDIF.

        IF v_has_count EQ abap_true.
          es_response_context-count = v_entity_count.
        ENDIF.

        IF v_inline_count EQ abap_true.
          es_response_context-inlinecount = v_entity_count.
        ENDIF.

        IF v_skip  IS NOT INITIAL.

          DELETE <fs_query_result> TO v_skip.

        ENDIF.



        LOOP AT <fs_query_result> ASSIGNING FIELD-SYMBOL(<fs_entity>).

*-- Do post Request processing HTTP Request
          me->do_after_request( i_entity_data = <fs_entity> ).

        ENDLOOP.

        er_entityset = v_enityset_tab.

      CATCH zcx_gateway_exception INTO DATA(v_gateway_exception).
        v_gateway_exception->throw_gateway_exception( ) .
      CATCH /iwbep/cx_mgw_not_impl_exc INTO DATA(lx_common_dpc_exception).
*-- Fallback with a try to call the standard DPC class to handle the request
        TRY.
            v_tech_request_context ?= io_tech_request_context.
            DATA(v_service_doc_name) = v_tech_request_context->get_request_details( )-service_doc_name.
            DATA(v_standard_dpc_class) = 'ZCL_' && substring_before( val = v_service_doc_name sub = '_SRV' ) && '_DPC_EXT'.
            CREATE OBJECT v_objref TYPE (v_standard_dpc_class).
            CALL FUNCTION 'SYSTEM_CALLSTACK'
              EXPORTING
                max_level = 1
              IMPORTING
                callstack = t_callstack.

            DATA(v_method_name) = t_callstack[ 1 ]-blockname.
            t_parameter_tab = VALUE #( ( name  = 'IV_ENTITY_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_entity_name ) )
                  ( name  = 'IV_ENTITY_SET_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_entity_set_name ) )
                  ( name  = 'IV_SOURCE_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_source_name ) )
                    ( name  = 'IT_FILTER_SELECT_OPTIONS'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( it_filter_select_options ) )
                    ( name  = 'IT_ORDER'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( it_order ) )
                    ( name  = 'IS_PAGING'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( is_paging ) )
                    ( name  = 'IT_NAVIGATION_PATH'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( it_navigation_path ) )
                    ( name  = 'IT_KEY_TAB'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( it_key_tab ) )
                    ( name  = 'IV_FILTER_STRING'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_filter_string ) )
                    ( name  = 'IV_SEARCH_STRING'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_search_string ) )
                    ( name  = 'IO_TECH_REQUEST_CONTEXT'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( io_tech_request_context ) )
                    ( name  = 'ER_ENTITYSET'
                    kind  = cl_abap_objectdescr=>importing
                    value = REF #( er_entityset ) )
                  ( name  = 'ES_RESPONSE_CONTEXT'
                    kind  = cl_abap_objectdescr=>importing
                    value = REF #( es_response_context ) ) ).

            CALL METHOD v_objref->(v_method_name)
              PARAMETER-TABLE
              t_parameter_tab.
          CATCH cx_sy_dyn_call_error cx_sy_itab_line_not_found.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc MESSAGE e001(zodata_cdpc).
        ENDTRY.
    ENDTRY.


  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset.




    DATA v_entity_name  TYPE /iwbep/mgw_tech_name .
    DATA v_entity_count TYPE i.
    DATA v_odata_model2 TYPE REF TO /iwbep/cl_mgw_odata_model.
    DATA: v_unit_code_tab     TYPE /iwbep/cl_mgw_data_conv_util=>ty_t_unit_code,
          v_currency_code_tab TYPE /iwbep/cl_mgw_data_conv_util=>ty_t_currency_codes,
          v_property_info_tab TYPE /iwbep/cl_mgw_data_conv_util=>ty_t_property_info,
          t_order_tab         TYPE /iwbep/t_mgw_sorting_order.



    FIELD-SYMBOLS: <fs_query_result> TYPE STANDARD TABLE.



*-- Get requested Entity name
    v_entity_name        = io_tech_request_context->get_entity_type_name( ).
    IF v_entity_name = 'Navigation'.
*-- Parse and Preprocess HTTP Request
      TRY.
          me->do_before_request( i_entity_name = v_entity_name
                                 i_action      = zcl_odata_common_dpc=>co_action-read ).



*-- GET entity handler which will process the request
          DATA(v_entity_handler) = get_entity_implementation( v_entity_name ).

*-- Create entitySet return table
          DATA(v_enityset_tab)    = create_mpc_entity_table( i_entity_name = v_entity_name  ).
          DATA(v_entity_struct)   = create_mpc_entity_struct( v_entity_name ).

          ASSIGN v_enityset_tab->* TO <fs_query_result>.
          IF sy-subrc IS NOT INITIAL.
            RETURN.
          ENDIF.

*-- GET entity properties
          DATA(v_entity_property_tab) = me->get_model( )->get_entity_type( v_entity_name )->get_properties( ).

*-- GET request filters
          DATA(v_filter_tab)   = io_tech_request_context->get_filter( )->get_filter_select_options( ).

          v_odata_model2 ?= me->get_model( ).

          DATA(v_select_tab)          = io_tech_request_context->get_select( ).

          IF v_select_tab[] IS INITIAL.

            v_select_tab = VALUE #( FOR v_property IN v_entity_property_tab (  v_property-technical_name  ) ).

          ENDIF.

*-- Include Key for navigation scenarios
          LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<fs_key>).

            TRY.

                DATA(mapped_db_key_name) = v_odata_model2->mt_entities[ name = v_entity_name ]-properties[ external_name =  <fs_key>-name ]-name.

                APPEND VALUE #(
                  property = mapped_db_key_name "<fs_key>-name
                  select_options  = VALUE #( ( sign = 'I' option = 'EQ' low = <fs_key>-value  ) )
                ) TO v_filter_tab.

              CATCH cx_sy_itab_line_not_found INTO DATA(v_exception).

                mapped_db_key_name = <fs_key>-name.
                APPEND VALUE #(
                  property = mapped_db_key_name "<fs_key>-name
                  select_options  = VALUE #( ( sign = 'I' option = 'EQ' low = <fs_key>-value  ) )
                ) TO v_filter_tab.

            ENDTRY.

          ENDLOOP.

*-- Convert filters values to internal format
          ASSIGN v_entity_struct->* TO FIELD-SYMBOL(<fs_entity_attr>).

          LOOP AT v_filter_tab ASSIGNING FIELD-SYMBOL(<fs_filter>).

            READ TABLE v_entity_property_tab WITH KEY technical_name = <fs_filter>-property
             INTO DATA(v_entity_property).
            IF sy-subrc IS INITIAL.

              ASSIGN COMPONENT to_upper( <fs_filter>-property ) OF STRUCTURE <fs_entity_attr> TO FIELD-SYMBOL(<fs_struct_value>).

              LOOP AT <fs_filter>-select_options ASSIGNING FIELD-SYMBOL(<fs_filter_options>).

                IF <fs_filter_options>-low IS NOT INITIAL.

                  /iwbep/cl_mgw_data_conv_util=>convert_prop_inbound_with_ext( EXPORTING it_unit_code                = v_unit_code_tab
                                                                                         it_currency_code            = v_currency_code_tab
                                                                                         it_property_info            = v_property_info_tab
                                                                                         iv_source_property          = <fs_filter_options>-low
                                                                                         io_property                 = CAST #( v_entity_property-property  )     " OData Property FPI
                                                                               IMPORTING
                                                                                      ev_target_property          = <fs_struct_value>
                                                                                     ).

                  <fs_filter_options>-low = <fs_struct_value>.

                ELSEIF <fs_filter_options>-high IS NOT INITIAL.

                  /iwbep/cl_mgw_data_conv_util=>convert_prop_inbound_with_ext( EXPORTING  it_unit_code                = v_unit_code_tab
                                                                                          it_currency_code            = v_currency_code_tab
                                                                                          it_property_info            = v_property_info_tab
                                                                                          iv_source_property          = <fs_filter_options>-high
                                                                                          io_property                 = CAST #( v_entity_property-property  )     " OData Property FPI

                                                                                IMPORTING
                                                                                          ev_target_property          = <fs_struct_value>
                                                                                  ).

                  <fs_filter_options>-high = <fs_struct_value>.

                ENDIF.

              ENDLOOP.

            ENDIF.

          ENDLOOP.

*-- Get Open SQL WHERE Clause from $filter
          DATA(v_osql_where)   = io_tech_request_context->get_osql_where_clause( ).

          io_tech_request_context->get_osql_where_clause_convert( ).


*-- Handle $skip and $top query string parameters
          DATA(v_top) =  COND #(  WHEN io_tech_request_context->get_top( ) IS INITIAL
                                  THEN 100 "If $top was not sent, set maximum to 100 rows
                                  ELSE io_tech_request_context->get_top( ) ).

          DATA(v_skip)       = io_tech_request_context->get_skip( ).
          DATA(v_total)      = v_top + v_skip.

*-- Handle $count query parameter
          DATA(v_has_count)    = io_tech_request_context->has_count( ).
          DATA(v_inline_count) = io_tech_request_context->has_inlinecount( ).

*-- Handle $orderby
          DATA(v_orderby_tab) = io_tech_request_context->get_orderby( ).

          IF v_orderby_tab[] IS NOT INITIAL.

            DATA  v_sort_element_tab  TYPE if_salv_ida_types_int=>yt_db_sort_rule.

            v_sort_element_tab = VALUE #(  FOR v_orderby IN v_orderby_tab (  field_path = v_orderby-property
                                                                             descending =  COND #( WHEN to_upper(  v_orderby-order ) EQ 'DESC' THEN  abap_true ELSE abap_false )  )

                                                                          ).

            "/iwbep/cl_mgw_data_util=>orderby( EXPORTING it_order = t_order_tab CHANGING ct_data = <fs_query_result>  ).

          ENDIF.


          IF v_has_count EQ abap_true.
            TRY.
                v_entity_count = v_entity_handler->count( EXPORTING
                                                              i_select_option_tab = v_filter_tab
                                                              i_osql_where        = v_osql_where
                                                              i_entity_name       = v_entity_name ).
              CATCH zcx_gateway_exception INTO DATA(v_gw_exception).
                v_gw_exception->throw_gateway_exception( ) .
              CATCH cx_root.

                RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception .
            ENDTRY.


          ELSE.
            TRY.

                v_entity_handler->query_expand( EXPORTING
                                              i_select_option_tab = v_filter_tab
                                              i_osql_where        = v_osql_where
                                              i_top               = v_top
                                              i_skip              = v_skip
                                              i_select_column_tab = CONV #( v_select_tab )
                                              i_sort_element_tab  = v_sort_element_tab
                                              i_navigation_path   = it_navigation_path
                                              i_entity_name       = v_entity_name
                                           IMPORTING
                                              et_expanded_tech_clauses = et_expanded_tech_clauses
                                           CHANGING
                                              c_query_result      = <fs_query_result>
                                          ).
              CATCH zcx_gateway_exception INTO v_gw_exception.
                v_gw_exception->throw_gateway_exception( ) .
              CATCH  cx_sy_no_handler.
                super->/iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset(
                  EXPORTING
                    iv_entity_name            = iv_entity_name
                    iv_entity_set_name        = iv_entity_set_name
                    iv_source_name            = iv_source_name
                    it_navigation_path        = it_navigation_path
                    is_paging                 = is_paging
                    it_order                  = it_order
                    it_filter_select_options  = it_filter_select_options
                    it_key_tab                = it_key_tab
                    iv_filter_string          = iv_filter_string
                    iv_search_string          = iv_search_string
                    io_expand                 = io_expand
                    io_tech_request_context   = io_tech_request_context
                  IMPORTING
                    er_entityset              = er_entityset
                    et_expanded_clauses       = et_expanded_clauses             " obsolete, use et_expanded_tech_clauses instead
                    et_expanded_tech_clauses  = et_expanded_tech_clauses  " resolved expands (parts of it or whole expand) by application
                    es_response_context       = es_response_context ).
*    super->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~get_expanded_entityset

                RETURN.

              CATCH cx_root INTO DATA(v_root_exception).

                RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception .
            ENDTRY.
            IF v_inline_count EQ abap_true.
              v_entity_count = v_entity_handler->count( i_entity_name = v_entity_name ).
            ENDIF.
          ENDIF.

          IF v_has_count EQ abap_true OR v_inline_count EQ abap_true.
            es_response_context-count = v_entity_count.
          ENDIF.

          IF v_skip  IS NOT INITIAL.

            DELETE <fs_query_result> TO v_skip.

          ENDIF.



          LOOP AT <fs_query_result> ASSIGNING FIELD-SYMBOL(<fs_entity>).

*-- Do post Request processing HTTP Request
            me->do_after_request( i_entity_data = <fs_entity> ).

          ENDLOOP.

          er_entityset = v_enityset_tab.

        CATCH zcx_gateway_exception INTO DATA(v_gateway_exception).
          v_gateway_exception->throw_gateway_exception( ) .
      ENDTRY.



    ELSE.

      super->/iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset(
        EXPORTING
          iv_entity_name            = iv_entity_name
          iv_entity_set_name        = iv_entity_set_name
          iv_source_name            = iv_source_name
          it_navigation_path        = it_navigation_path
          is_paging                 = is_paging
          it_order                  = it_order
          it_filter_select_options  = it_filter_select_options
          it_key_tab                = it_key_tab
          iv_filter_string          = iv_filter_string
          iv_search_string          = iv_search_string
          io_expand                 = io_expand
          io_tech_request_context   = io_tech_request_context
        IMPORTING
          er_entityset              = er_entityset
          et_expanded_clauses       = et_expanded_clauses             " obsolete, use et_expanded_tech_clauses instead
          et_expanded_tech_clauses  = et_expanded_tech_clauses  " resolved expands (parts of it or whole expand) by application
          es_response_context       = es_response_context ).
*    super->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~get_expanded_entityset
    ENDIF.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.


    DATA: v_entity_name    TYPE /iwbep/mgw_tech_name,
          v_media_resource TYPE /iwbep/if_mgw_core_srv_runtime=>ty_s_media_resource.


*-- Get requested Entity name
    v_entity_name          = io_tech_request_context->get_entity_type_name( ).

*-- Parse and Preprocess HTTP Request
    me->do_before_request( i_entity_name = v_entity_name i_action = zcl_odata_common_dpc=>co_action-read ).

    TRY.

*-- Get Entity handler which will process the request
        DATA(v_entity_handler) = get_entity_implementation( v_entity_name ).

*-- Read unconverted entity Key from request
        DATA(v_key_values_tab) = io_tech_request_context->get_keys( ).


*-- Dispatch the call to entity handler which will perform the read operation
        DATA(v_stream_resource) = v_entity_handler->get_stream(  i_key_tab  = it_key_tab ).

        v_media_resource = VALUE #(  mime_type  = v_stream_resource-mime_type value = v_stream_resource-content  ).

        IF v_stream_resource-download_content EQ abap_true.
          set_header( is_header = VALUE #( name = 'content-disposition'  value = |outline; filename="{ v_stream_resource-file_name }" | ) ).
        ELSEIF v_stream_resource-preview_content EQ abap_true.
          set_header( is_header = VALUE #( name = 'content-disposition'  value = |inline; filename="{ v_stream_resource-file_name }" | ) ).
        ENDIF.

*-- DO post Request processing HTTP Request
        me->do_after_request(  ).

        copy_data_to_ref( EXPORTING is_data = v_media_resource
                          CHANGING  cr_data = er_stream ).


      CATCH zcx_gateway_exception INTO DATA(v_exception).
        v_exception->throw_gateway_exception( ) .

      CATCH cx_root INTO DATA(v_root_exception).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            previous = v_root_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~update_entity.

*-- Parse and Preprocess HTTP Request
    "    me->do_before_request( i_action = zcl_odata_common_dpc=>co_action-write ).

    DATA: v_entity_name         TYPE /iwbep/mgw_tech_name.
    DATA: v_tutorial_mode_flag TYPE crmt_boolean.
    DATA: v_tech_request_context TYPE REF TO /iwbep/cl_mgw_request,
          v_objref               TYPE REF TO object,
          t_parameter_tab        TYPE abap_parmbind_tab,
          t_callstack            TYPE abap_callstack.

*-- Get requested Entity name
    v_entity_name          = io_tech_request_context->get_entity_type_name( ).

*-- Parse and Preprocess HTTP Request
    me->do_before_request(  i_entity_name = v_entity_name i_action = zcl_odata_common_dpc=>co_action-write ).



    TRY.

*-- Get Entity handler which will process the request
        DATA(v_entity_handler) = get_entity_implementation( v_entity_name ).



        DATA(v_entity_struct)  = create_mpc_entity_struct( v_entity_name ).

        ASSIGN v_entity_struct->* TO FIELD-SYMBOL(<fs_entity_attr>).

        IF sy-subrc IS NOT INITIAL.
          ASSERT 1 = 2.

        ENDIF.

        io_data_provider->read_entry_data( IMPORTING es_data = <fs_entity_attr> ).

        TRY.
            v_tutorial_mode_flag = me->service_application->get_context_attribute( zcl_odata_authentication_md=>co_tutorial_mode_flag ).
          CATCH cx_sy_ref_is_initial.
        ENDTRY.

*--- in case of tutorial mode, raise error, because any change is not allowed in tutorial mode
        IF v_tutorial_mode_flag EQ abap_true.
          RAISE EXCEPTION TYPE zcx_gateway_exception
            EXPORTING
              textid        = zcx_cua_btx_general=>error
              error_msg     = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = zif_const_bf_cua=>gc_cua_otr-tutorial_mode
                                                                            language = sy-langu )
              callstack     = zcl_cua_bxt_tools=>get_callstack( )
              kind_of_error = 'B'.
        ENDIF.

        v_entity_handler->update(  EXPORTING
                                   i_key                    = io_tech_request_context->get_keys( )
                                   i_navigation_tab         = it_navigation_path
                                   i_key_tab                = it_key_tab
                                   i_update_entity          = <fs_entity_attr>
                                   i_entity_name            = v_entity_name
                                  IMPORTING
                                    e_entity                = <fs_entity_attr>
                               ).

        IF me->v_batch_mode = abap_false.
          me->service_application->finish_process(  ).
        ENDIF.

*-- Do post Request processing HTTP Request
        me->do_after_request( i_entity_data = <fs_entity_attr> ).

        er_entity = v_entity_struct.

*-- Check if the user has authorization to Read the Entity

      CATCH zcx_gateway_exception INTO DATA(v_exception).
        v_exception->throw_gateway_exception( ) .
      CATCH /iwbep/cx_mgw_not_impl_exc INTO DATA(lx_common_dpc_exception).
*-- Fallback with a try to call the standard DPC class to handle the request
        TRY.
            v_tech_request_context ?= io_tech_request_context.
            DATA(v_service_doc_name) = v_tech_request_context->get_request_details( )-service_doc_name.
            DATA(v_standard_dpc_class) = 'ZCL_' && substring_before( val = v_service_doc_name sub = '_SRV' ) && '_DPC_EXT'.
            CREATE OBJECT v_objref TYPE (v_standard_dpc_class).
            CALL FUNCTION 'SYSTEM_CALLSTACK'
              EXPORTING
                max_level = 1
              IMPORTING
                callstack = t_callstack.

            DATA(v_method_name) = t_callstack[ 1 ]-blockname.
            t_parameter_tab = VALUE #(
                  ( name  = 'IV_ENTITY_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_entity_name ) )
                  ( name  = 'IV_ENTITY_SET_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_entity_set_name ) )
                  ( name  = 'IV_SOURCE_NAME'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_source_name ) )
                  ( name  = 'IO_DATA_PROVIDER'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( io_data_provider ) )
                  ( name  = 'IT_KEY_TAB'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( it_key_tab ) )
                  ( name  = 'IT_NAVIGATION_PATH'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( it_navigation_path ) )
                  ( name  = 'IO_TECH_REQUEST_CONTEXT'
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( io_tech_request_context ) )
                  ( name  = 'ER_ENTITY'
                    kind  = cl_abap_objectdescr=>importing
                    value = REF #( er_entity ) ) ).

            CALL METHOD v_objref->(v_method_name)
              PARAMETER-TABLE
              t_parameter_tab.
          CATCH cx_sy_dyn_call_error cx_sy_itab_line_not_found.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc MESSAGE e001(zodata_cdpc).
        ENDTRY.
      CATCH /iwbep/cx_mgw_busi_exception INTO DATA(v_busi_exception).
        RAISE EXCEPTION v_busi_exception.
      CATCH cx_root INTO DATA(v_root_exception).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            previous = v_root_exception.

    ENDTRY.




  ENDMETHOD.


  METHOD /iwbep/if_mgw_core_srv_runtime~init.



*-- Core - Initializes the Runtime Implementation
    super->/iwbep/if_mgw_core_srv_runtime~init(
        iv_namespace             = iv_namespace
        iv_service_document_name = iv_service_document_name
        iv_version               = iv_version
        io_context               = io_context

     ).


*-- GET logger
    me->logger =  me->/iwbep/if_mgw_conv_srv_runtime~get_logger( ).


*-- Get service OData application. An OData application is logical grouping of related OData service
    "it includes and handle application specific configuration and it is how the application can enhance and influence
    "the request processing

    me->service_application =  get_service_application( i_service_name    = to_upper( iv_service_document_name )
                                                        i_service_version = to_upper(  iv_version )
                                                      ).

    me->service_application->init( i_service_name    = iv_service_document_name
                                   i_service_version = iv_version
                                   i_odata_context   = io_context
                                  ).

*-- Get registered modules for the current application

    me->odata_modules = get_default_modules( me->service_application ).

    me->service_application->register_odata_module( CHANGING c_registered_module = me->odata_modules ).


  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

  ENDMETHOD.


  METHOD create_mpc_entity_struct.

    DATA(v_model_srv) = CAST /iwbep/cl_mgw_dp_facade(  me->/iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ) ).

*-- Create return structure based on the Entity type defined in the OData Model
    r_result   = v_model_srv->/iwbep/if_mgw_dp_fw_facade~get_data_structure_by_name(  i_entity_name  ).


  ENDMETHOD.


  METHOD create_mpc_entity_table.

    DATA(v_model_srv) = CAST /iwbep/cl_mgw_dp_facade(  me->/iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ) ).


*-- Create return structure based on the Entity type defined in the OData Model
    r_result   = v_model_srv->/iwbep/if_mgw_dp_fw_facade~get_data_table_by_name(  i_entity_name   ).


  ENDMETHOD.


  METHOD do_after_request.

    TRY.

        exec_post_request_md( i_entity_data = i_entity_data ).

      CATCH zcx_http INTO DATA(v_error).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            previous = v_error.

    ENDTRY.

  ENDMETHOD.


  METHOD do_before_request.


*-- GET HTTP Header from Request
    DATA(v_http_header_tab) = me->/iwbep/if_mgw_conv_srv_runtime~get_dp_facade( )->get_request_header( ).

    LOOP AT v_http_header_tab ASSIGNING FIELD-SYMBOL(<fs_header>).
      me->service_application->set_context_attribute( i_attribute = <fs_header>-name i_value = <fs_header>-value ).
    ENDLOOP.

    me->service_application->set_model( me->get_model( ) ).

    me->service_application->set_context_attribute( i_attribute = co_requested_entity   i_value = i_entity_name ).
    me->service_application->set_context_attribute( i_attribute = co_requested_action   i_value = i_action ).


    TRY.

        exec_pre_request_md( ).

      CATCH zcx_http INTO DATA(v_error).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            previous = v_error.


    ENDTRY.

  ENDMETHOD.


  METHOD exec_post_request_md.

    IF NOT  me->odata_modules-authorization_md IS INITIAL.
      me->odata_modules-authorization_md->post_processing( i_entity_data = i_entity_data ).
    ENDIF.

  ENDMETHOD.


  METHOD exec_pre_request_md.

    "-- Authenticate user based on HTTP request header

    IF NOT  me->odata_modules-authentication_md IS INITIAL.
      me->odata_modules-authentication_md->process( ).
    ENDIF.

    IF NOT  me->odata_modules-authorization_md IS INITIAL.
      me->odata_modules-authorization_md->process( ).
    ENDIF.


  ENDMETHOD.


  METHOD get_annotation_data.

    DATA: lo_med_provider             TYPE REF TO /iwfnd/cl_med_mdl_provider.

    lo_med_provider ?= /iwfnd/cl_med_mdl_provider=>get_instance( ).

    TRY.


        DATA(v_service_id) = /iwfnd/cl_med_exploration=>get_vocan_main_service(
                             iv_anno_tech_name = i_anno_tech_name
                             iv_anno_version   = i_anno_version ).

        /iwfnd/cl_med_exploration=>get_srv_runtime_info_by_id(  EXPORTING
                                                                    iv_srv_identifier = v_service_id
                                                                IMPORTING
                                                                    ev_process_mode   = DATA(v_process_mode)  " Service Processing Mode
        ).

        "Co-Deployed Mode does not have system aliases
        IF v_process_mode <> /iwfnd/if_mgw_core_types=>gcs_process_mode-co_deployed_only.

          TRY.
              /iwfnd/cl_mgw_dest_finder=>get_single_sys_alias_with_info(
                EXPORTING
                  iv_service_id   = v_service_id
                  iv_user         = sy-uname
                IMPORTING
                  es_system_alias = DATA(v_system_alias)
                  ).

              DATA(v_destin_finder)     = /iwfnd/cl_destin_finder=>get_destination_finder( ).
              DATA(v_system_alias_info) = v_destin_finder->get_system_alias_info( v_system_alias-system_alias ).

            CATCH /iwfnd/cx_mgw_dest_finder /iwfnd/cx_destin_finder.
              RAISE EXCEPTION TYPE /iwfnd/cx_mgw_busi_med_catalog
                EXPORTING
                  textid                   = /iwfnd/cx_med_mdl_access=>vocan_routing_error
                  anno_tech_name           = i_anno_tech_name
                  anno_version             = i_anno_version
                  service_group_identifier = v_service_id.
          ENDTRY.
        ENDIF.

        lo_med_provider->get_vocan_model(
          EXPORTING
            iv_technical_name    = i_anno_tech_name    " Technical Vocabulary Annotation File Name
            iv_version           = i_anno_version      " Vocabulary Annotation File Version
            is_system_alias_info = v_system_alias_info
            iv_process_mode      = v_process_mode
          IMPORTING
            es_vocan_model       = DATA(v_vocan_model)       " Vocabulary Annotation Model
        ).

      CATCH /iwfnd/cx_med_mdl_access.
        RAISE EXCEPTION TYPE /iwfnd/cx_mgw_busi_med_catalog
          EXPORTING
            textid         = /iwfnd/cx_mgw_busi_med_catalog=>vocan_file_not_found
            anno_tech_name = i_anno_tech_name
            anno_version   = i_anno_version.
    ENDTRY.


    DATA(v_sodata_vocan_prov) = /iwfnd/cl_sodata_vocan_providr=>get_vocan_provider( ).

    v_sodata_vocan_prov->build_vocan_model(
      EXPORTING
        iv_schema_namespace         = space
        is_vocan_model              = v_vocan_model
      IMPORTING
        et_schema_vocab_annotations = r_result
        et_vocab_references         = DATA(v_references_tab)
        et_vocab_usings             = DATA(v_vocab_usings_tab)
      ).



  ENDMETHOD.


  METHOD get_default_modules.
*-- TODO Define user authorization OData Module
*-- TODO Define application log    OData Module
*-- TODO Define error handling     OData Module

    r_result-authentication_md = NEW zcl_odata_authentication_md( i_odata_app ).
    r_result-authorization_md  = NEW zcl_odata_authorization_md( i_odata_app ). .

  ENDMETHOD.


  METHOD get_entity_implementation.

    r_result = me->service_application->get_entity_handler( i_entity_name = i_entity_name ).

*-- init entity handler
    r_result->init( me->service_application  ).

  ENDMETHOD.


  METHOD get_internal_service_info.

*---get service details from context
    mo_context->get_parameter(
      EXPORTING
        iv_name  = /iwbep/if_mgw_context=>gc_param_isn
      IMPORTING
        ev_value = out_service_name ).

    mo_context->get_parameter(
      EXPORTING
        iv_name   = /iwbep/if_mgw_context=>gc_param_isv
      IMPORTING
        ev_value  = out_service_version ).

  ENDMETHOD.


  METHOD get_model.
    DATA: v_internal_service_name    TYPE /iwbep/med_grp_technical_name,
          v_internal_service_version TYPE /iwbep/med_grp_version.


    IF me->odata_model IS INITIAL.

      get_internal_service_info( IMPORTING out_service_name    = v_internal_service_name
                                           out_service_version = v_internal_service_version ).

      DATA(v_metadata_provider)         = /iwbep/cl_mgw_med_provider=>get_med_provider( ).
      DATA(v_default_sys_alias_info)    = me->mo_context->get_system_alias_info( ).

      v_metadata_provider->initialize(
        EXPORTING
          is_default_system_alias_info = v_default_sys_alias_info     " System Alias Information
          iv_is_busi_data_request      = abap_true                    " TRUE if the request is for busi data - smaller metadata
      ).

      TRY.
          me->odata_model =   v_metadata_provider->get_service_metadata(    iv_internal_service_name    = v_internal_service_name
                                                                            iv_internal_service_version = v_internal_service_version
                                                                        ).
          me->default_sys_alias_info = v_default_sys_alias_info.


        CATCH /iwbep/cx_mgw_med_exception.
          "handle exception
      ENDTRY.
    ENDIF.

    rv_result = me->odata_model.

  ENDMETHOD.


  METHOD get_service_application.

    r_result =  zcl_odata_entity_factory=>get_application_impl( i_service_name    = i_service_name
                                                                i_service_version = i_service_version
                                                               ).
    IF r_result IS INITIAL.

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc.

    ENDIF.


  ENDMETHOD.


  METHOD process_children.
    DATA v_child_node TYPE REF TO /iwbep/cl_mgw_expand_node.
    FIELD-SYMBOLS: <fs_child_entities> TYPE ANY TABLE.
    FIELD-SYMBOLS: <fs_child_entity_struc> TYPE any.
    FIELD-SYMBOLS: <fs_child_entity_key> TYPE any.

*    DATA i_children_tab TYPE /iwbep/if_mgw_odata_expand=>ty_t_node_children.
*    i_children_tab = x_children_tab.
* DATA(v_main_node_properties) = v_expanded_node->get_properties(  ).

    LOOP AT i_children_tab ASSIGNING FIELD-SYMBOL(<fs_child>).

      v_child_node ?= <fs_child>-node.

      DATA(v_child_node_properties) = v_child_node->get_properties(  ).
      READ TABLE i_navigation_properties WITH KEY name = <fs_child>-tech_nav_prop_name ASSIGNING FIELD-SYMBOL(<fs_navigation_property>).
      LOOP AT i_property_mapping ASSIGNING FIELD-SYMBOL(<fs_property_mapping>) WHERE reference_id = <fs_navigation_property>-target_entity_id.
        ASSIGN COMPONENT  i_main_node[ name = <fs_property_mapping>-source_ext_name ]-technical_name OF STRUCTURE i_main_entity_attr TO FIELD-SYMBOL(<fs_main_entity_key>).

        IF <fs_child>-node->get_multiplicity( ) NE '1' AND <fs_child>-node->get_multiplicity( ) NE '0'.
*
          UNASSIGN <fs_child_entities>.
          ASSIGN COMPONENT <fs_child>-tech_nav_prop_name OF STRUCTURE i_source_structure TO <fs_child_entities>.
*
          LOOP AT <fs_child_entities> ASSIGNING FIELD-SYMBOL(<fs_child_entity>).
            ASSIGN COMPONENT  v_child_node_properties[ name = <fs_property_mapping>-target_ext_name ]-technical_name OF STRUCTURE <fs_child_entity> TO <fs_child_entity_key>.
            <fs_child_entity_key> = <fs_main_entity_key>.
          ENDLOOP.
*
        ELSE.
*
**
*

          DATA(v_child_entity_tab) = create_mpc_entity_table( CONV #( v_child_node->get_entity_type( ) ) ).
          DATA(v_child_entity_struc) = create_mpc_entity_struct( CONV #( v_child_node->get_entity_type( ) ) ).

          ASSIGN v_child_entity_tab->* TO <fs_child_entities>.
          ASSIGN v_child_entity_struc->* TO <fs_child_entity_struc>.

          ASSIGN COMPONENT <fs_child>-tech_nav_prop_name OF STRUCTURE i_source_structure TO <fs_child_entity>.
          ASSIGN COMPONENT  v_child_node_properties[ name = <fs_property_mapping>-target_ext_name ]-technical_name OF STRUCTURE <fs_child_entity> TO FIELD-SYMBOL(<fs_child_entity_keyx>).
          <fs_child_entity_keyx> = <fs_main_entity_key>.

          MOVE-CORRESPONDING <fs_child_entity> TO <fs_child_entity_struc>.
          INSERT <fs_child_entity_struc> INTO TABLE <fs_child_entities>.

        ENDIF.
***

**
      ENDLOOP.



      DATA(v_entity_handler_child) = get_entity_implementation( i_entity_name = CONV #( v_child_node->get_entity_type( ) ) ).

      DATA(v_entity_child_struct)  = create_mpc_entity_struct( CONV #( v_child_node->get_entity_type( ) ) ).

      ASSIGN v_entity_child_struct->* TO FIELD-SYMBOL(<fs_entity_child_attr>).

      IF sy-subrc IS NOT INITIAL.
        ASSERT 1 = 2.

      ENDIF.
      LOOP AT <fs_child_entities> ASSIGNING <fs_child_entity>.

        MOVE-CORRESPONDING <fs_child_entity> TO <fs_entity_child_attr>.

        TRY.
            DATA(v_contract_guid) = me->service_application->get_current_user(  )->get_portal_contract( )->get_id(  ).
          CATCH cx_sy_ref_is_initial.
        ENDTRY.


        v_entity_handler_child->create( EXPORTING
                                     i_new_entity   = <fs_entity_child_attr>
                                     i_entity_name  = CONV #( v_child_node->get_entity_type( ) )
                                     i_contract_guid = v_contract_guid
                                  IMPORTING
                                    e_entity       =  <fs_entity_child_attr>
         ).



        MOVE-CORRESPONDING <fs_entity_child_attr> TO <fs_child_entity>.

*******
        v_child_node->get_children( IMPORTING et_children = DATA(v_children_tmp) ).

        DATA v_child_tab TYPE /iwbep/if_mgw_odata_expand=>ty_t_node_children.
**********************************************************************
* added clear, because otherwise we create to much
**********************************************************************
        CLEAR v_child_tab.

        IF v_children_tmp IS NOT INITIAL.
          LOOP AT v_children_tmp ASSIGNING FIELD-SYMBOL(<fs_children_tmp>).

            APPEND INITIAL LINE TO v_child_tab ASSIGNING FIELD-SYMBOL(<fs_children>).
            <fs_children>-node = <fs_children_tmp>-node.
            <fs_children>-tech_nav_prop_name = <fs_children_tmp>-tech_nav_prop_name.
          ENDLOOP.

          ASSIGN COMPONENT <fs_child>-tech_nav_prop_name OF STRUCTURE i_source_structure TO FIELD-SYMBOL(<fs_source_structure>).

          DESCRIBE FIELD <fs_source_structure> TYPE DATA(l_typ).

          IF l_typ = 'v'.
            me->process_children( EXPORTING
                                      i_main_node             = v_child_node_properties
                                      i_navigation_properties = i_navigation_properties
                                      i_property_mapping      = i_property_mapping
                                      i_main_entity_attr      = <fs_entity_child_attr>
                                   CHANGING
                                      i_source_structure    = <fs_source_structure>
                                      i_children_tab        = v_child_tab ).

          ELSE.

*            FIELD-SYMBOLS <fs_children_tab_tmp> TYPE ANY TABLE.
*            ASSIGN COMPONENT <fs_child>-tech_nav_prop_name OF STRUCTURE i_source_structure TO <fs_children_tab_tmp>.
*
*            LOOP AT <fs_children_tab_tmp> ASSIGNING FIELD-SYMBOL(<fs_children_tab_single_tmp>).
*              me->process_children( EXPORTING
*                                        i_main_node             = v_child_node_properties
*                                        i_navigation_properties = i_navigation_properties
*                                        i_property_mapping      = i_property_mapping
*                                        i_main_entity_attr      = <fs_entity_child_attr>
*                                     CHANGING
*                                        i_source_structure    = <fs_children_tab_single_tmp>
*                                        i_children_tab        = v_child_tab ).
*            ENDLOOP.

            me->process_children( EXPORTING
                                      i_main_node             = v_child_node_properties
                                      i_navigation_properties = i_navigation_properties
                                      i_property_mapping      = i_property_mapping
                                      i_main_entity_attr      = <fs_entity_child_attr>
                                   CHANGING
                                      i_source_structure    = <fs_child_entity>
                                      i_children_tab        = v_child_tab ).


          ENDIF.
        ENDIF.

      ENDLOOP.

*******


    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
