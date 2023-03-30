CLASS zcl_crm_order_item_bol DEFINITION
  PUBLIC
  INHERITING FROM zcl_crm_base_order_item
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_crm_order_bol .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !i_entity_root TYPE REF TO cl_crm_bol_entity .
    METHODS create_by_product
      IMPORTING
        !i_product TYPE any .
    METHODS load_by_product
      IMPORTING
        !i_product TYPE any .

    METHODS zif_crm_entity~get_entity_id
        REDEFINITION .
    METHODS zif_crm_order_item~add_extension_structure
        REDEFINITION .
    METHODS zif_crm_order_item~add_product_i
        REDEFINITION .

    METHODS zif_crm_order_item~modify_product_i
        REDEFINITION .
    METHODS zif_crm_order_item~add_reference_object
        REDEFINITION .

    METHODS zif_crm_order_item~remove_reference_object
        REDEFINITION .

    METHODS zif_crm_order_item~modify_reference_object
        REDEFINITION .

    METHODS zif_crm_order_item~get_extension_list
        REDEFINITION .
    METHODS zif_crm_order_item~get_orderadm_i_struct
        REDEFINITION .
    METHODS zif_crm_order_item~modify_extension_structure
        REDEFINITION .

    METHODS zif_crm_order_item~remove_extension_structure
        REDEFINITION .
    METHODS zif_crm_order_item~status_i
        REDEFINITION .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA v_entity_root TYPE REF TO cl_crm_bol_entity .
    DATA v_item_entity_root TYPE REF TO cl_crm_bol_entity .

    METHODS switch_to_change_mode .
    METHODS map_product_guid_to_product_id
      IMPORTING
        !i_product_guid     TYPE comt_product_guid
      RETURNING
        VALUE(r_product_id) TYPE comt_product_id .
ENDCLASS.



CLASS zcl_crm_order_item_bol IMPLEMENTATION.


  METHOD constructor.
    super->constructor(  ).
    me->v_entity_root = i_entity_root.
  ENDMETHOD.


  METHOD create_by_product.
    DATA: v_bpath TYPE string.
    DATA: v_param TYPE string.


    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_exception).
        RAISE EXCEPTION v_lock_exception.
    ENDTRY.

    TRY .

*        v_bpath = |./BTOrderHeader/BTHeaderBOSSet/BTRefObjSet_A/BTRefObjectAll|.
** Add filter
*        v_param = |[@PRODUCT_ID="{ i_product }"]|.
*        v_bpath = |{ v_bpath }{ v_param }/*$|.

        v_bpath = |./BTOrderHeader/BTHeaderItemsExt/BTOrderItemAll[@PRODUCT="{ i_product }"]/*$|.

        v_item_entity_root = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->v_entity_root i_bpath = v_bpath i_create = abap_true  ).

      CATCH cx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD load_by_product.

  ENDMETHOD.


  METHOD map_product_guid_to_product_id.

    SELECT SINGLE product_id
    INTO r_product_id
    FROM   comm_product
    WHERE product_guid = i_product_guid.


  ENDMETHOD.


  METHOD switch_to_change_mode.

    IF  me->v_entity_root->is_locked( ) = abap_false.

      IF me->v_entity_root->switch_to_change_mode( ) <> abap_true.
        zcx_crm_app=>raise_foreign_lock( i_text = '' ).
      ENDIF.

      me->v_entity_root->lock( ).

      IF me->v_entity_root->is_locked( ) <> 'X'.
        zcx_crm_app=>raise_foreign_lock( i_text = '' ).
      ENDIF.

      IF me->v_entity_root->is_changeable( ) <> 'X'.
        zcx_crm_app=>raise_foreign_lock( i_text = '' ).
      ENDIF.


      DATA v_transaction TYPE REF TO    zcl_crm_base_transaction.
      DATA v_context TYPE zcrm_transaction_data.

      TRY.
          v_transaction ?= zcl_crm_transaction_bol=>get_instance(  ).
          v_context-object = me->v_entity_root->get_name(  ).
          v_context-objectkey = me->zif_crm_entity~get_entity_id(  ).
          v_transaction->add( v_context ).
        CATCH cx_root.
          zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
      ENDTRY.


    ENDIF.

  ENDMETHOD.


  METHOD zif_crm_entity~get_entity_id.
    TRY.
        me->v_entity_root->get_property_as_value( EXPORTING iv_attr_name = 'CRM_GUID' IMPORTING ev_result = r_result  ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_order_item~add_extension_structure.

    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.

    DATA(v_bpath) = |./{ i_extension_name }/*$|.
    DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->v_item_entity_root i_bpath = v_bpath  i_create = abap_true  ).

    TRY.

        v_entity->set_properties( EXPORTING is_attributes = i_extension_value  ).
        cl_crm_bol_core=>get_instance( )->modify( ).
        DATA(v_record_guid) = v_entity->get_property_as_string( iv_attr_name      = |RECORD_ID| ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.




  ENDMETHOD.


  METHOD zif_crm_order_item~add_product_i.

    DATA: v_bpath TYPE string.
    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_exception).
        RAISE EXCEPTION v_lock_exception.
    ENDTRY.

    v_bpath = |./BTItemProductExt|.

    TRY.
        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->v_item_entity_root i_bpath = v_bpath i_create = abap_true  ).
        v_entity->set_properties( is_attributes = i_product_item ).

        cl_crm_bol_core=>get_instance( )->modify( ).

      CATCH cx_root INTO DATA(v_exc).
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order_item~add_reference_object.

    DATA: v_bpath TYPE string.
    DATA: v_param TYPE string.


    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.

    TRY .
        CASE i_field_name.

          WHEN 'IB_COMP_REF_GUID'.
*            DATA(v_param_property) = 'IB_COMP_REF_GUID'.
*            DATA(v_param_value) = me->map_ib_id_2_ib_guid( i_reference_object ).
            v_param = |[@{ i_field_name }="{ i_reference_object }"]|.

          WHEN 'PRODUCT_ID'.
*            v_param_property = 'PRODUCT_ID'.
            DATA(v_data_type) = cl_abap_typedescr=>describe_by_data( i_reference_object )->get_relative_name( ).
            DATA(v_product_id) = SWITCH comt_product_id( v_data_type
                      WHEN 'COMT_PRODUCT_GUID' THEN me->map_product_guid_to_product_id( i_reference_object )
                      WHEN 'COMT_PRODUCT_ID' THEN i_reference_object
                      ELSE  i_reference_object ).
            v_param = |[@{ i_field_name }="{ v_product_id }"]|.
          WHEN OTHERS.
        ENDCASE.




        v_bpath = |./BTItemBOSSet/BTRefObjSet_A/BTRefObjectAll|. "BTRefObjectProductsAll
** Add filter

        v_bpath = |{ v_bpath }{ v_param }/*$|.


        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->v_item_entity_root i_bpath = v_bpath i_create = abap_true  ).

        cl_crm_bol_core=>get_instance( )->modify( ).
*        v_entity->reread( ).
        r_ref_guid = v_entity->get_property_as_string(
                               iv_attr_name      = i_field_name ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.


  ENDMETHOD.


  METHOD zif_crm_order_item~get_extension_list.
    DATA: lr_attr_ref         TYPE REF TO data.
    FIELD-SYMBOLS: <attr_struc> TYPE any.

    TRY.
        DATA(v_coll) = me->v_item_entity_root->get_related_entities( iv_relation_name = CONV #( i_extension_name ) ).
*
        DATA(v_entity) = v_coll->get_first(  ).

        IF v_entity IS NOT BOUND.
          RETURN.
        ENDIF.

        DATA(v_struc_name) = v_entity->get_attr_struct_name(  ).
        CREATE DATA lr_attr_ref TYPE (v_struc_name).
        ASSIGN lr_attr_ref->* TO <attr_struc>.

        WHILE v_entity IS BOUND.
          v_entity->get_properties( IMPORTING es_attributes = <attr_struc> ).
          INSERT <attr_struc> INTO TABLE e_extension_list.
          v_entity = v_coll->get_next(  ).
        ENDWHILE.
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order_item~get_orderadm_i_struct.
    DATA(v_bpath) = |./*$|.

    TRY.
        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->v_item_entity_root i_bpath = v_bpath i_create = abap_false ).
        IF v_entity IS BOUND.
          v_entity->get_properties(
            IMPORTING
              es_attributes = r_order_i_struct
          ).
        ENDIF.

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_order_item~modify_extension_structure.

    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.

    DATA(v_bpath) = |./{ i_extension_name }/*$|.
    DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->v_item_entity_root i_bpath = v_bpath  i_create = abap_false  ).

    TRY.

        v_entity->set_properties( EXPORTING is_attributes = i_extension_value  ).
        cl_crm_bol_core=>get_instance( )->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order_item~modify_product_i.

    DATA: v_bpath TYPE string.
    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_exception).
        RAISE EXCEPTION v_lock_exception.
    ENDTRY.

    v_bpath = |./BTItemProductExt|.

    TRY.
        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->v_item_entity_root i_bpath = v_bpath i_create = abap_false  ).
        v_entity->set_properties( is_attributes = i_product_item ).

        cl_crm_bol_core=>get_instance( )->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order_item~modify_reference_object.
    DATA: v_bpath TYPE string.
    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.

    TRY .
*
        v_bpath = |./BTItemBOSSet/BTRefObjSet_A/BTRefObjectAll/*$|. "BTRefObjectProductsAll

        DATA(v_ref_obj_col) = me->v_item_entity_root->get_related_entities_by_bpath( EXPORTING  iv_bpath_statement =  v_bpath ).

        DATA(v_ref_obj) = v_ref_obj_col->get_first( ).

        WHILE v_ref_obj IS BOUND.
          DATA(v_ref_obj_val) = v_ref_obj->get_property_as_string(
                                iv_attr_name      = i_field_name
*
                            ).
          IF v_ref_obj_val   = i_old_refobj_val.
            "ref object to modify
            DATA(v_entity) = v_ref_obj.
            EXIT.
          ENDIF.
          v_ref_obj = v_ref_obj_col->get_next( ).
        ENDWHILE.

        CHECK v_entity IS BOUND.
        v_entity->set_properties( is_attributes =  i_new_refobj_value  ).
*        v_entity->set_property(
*          EXPORTING
*            iv_attr_name =      i_field_name            " Component Name
*            iv_value     =  i_new_refobj_value
*        ).
*        v_entity->set_property_as_string(
*          EXPORTING
*            iv_attr_name =      i_field_name            " Component Name
*            iv_value     = CONV #( i_new_refobj_value )
*        ).
*        CATCH cx_sy_conversion_error. " System Exception Involving Conversion Errors.
        cl_crm_bol_core=>get_instance( )->modify( ).
        DATA(v_) = v_entity->get_property_as_string(
                     iv_attr_name      = i_field_name
*             iv_use_iso_format = abap_false
                   ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.


  ENDMETHOD.


  METHOD zif_crm_order_item~remove_extension_structure.

    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.

    DATA(v_bpath) = |./{ i_extension_name }/*$|.
    DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->v_item_entity_root i_bpath = v_bpath  i_create = abap_false  ).

    TRY.

        v_entity->delete( ).
        cl_crm_bol_core=>get_instance( )->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order_item~remove_reference_object.
    DATA: v_bpath TYPE string.
    DATA: v_param TYPE string.


    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.

    TRY .
*        CASE i_field_name.
*
*          WHEN 'IB_COMP_REF_GUID'.
**            DATA(v_param_property) = 'IB_COMP_REF_GUID'.
*            DATA(v_ib_comp_guid) = CONV crmt_object_guid(  i_reference_object ).
**            DATA(v_param_value) = me->map_ib_id_2_ib_guid( i_reference_object ).
*            v_param = |[@{ i_field_name }={ v_ib_comp_guid }]|.
*
*          WHEN 'PRODUCT_ID'.
**            v_param_property = 'PRODUCT_ID'.
*            DATA(v_data_type) = cl_abap_typedescr=>describe_by_data( i_reference_object )->get_relative_name( ).
*            DATA(v_product_id) = SWITCH comt_product_id( v_data_type
*                      WHEN 'COMT_PRODUCT_GUID' THEN me->map_product_guid_to_product_id( i_reference_object )
*                      WHEN 'COMT_PRODUCT_ID' THEN i_reference_object
*                      ELSE  i_reference_object ).
*            v_param = |[@{ i_field_name }={ v_product_id }]|.
*          WHEN OTHERS.
*        ENDCASE.

        v_bpath = |./BTItemBOSSet/BTRefObjSet_A/BTRefObjectAll/*$|. "BTRefObjectProductsAll

        DATA(v_ref_obj_col) = me->v_item_entity_root->get_related_entities_by_bpath( EXPORTING  iv_bpath_statement =  v_bpath ).

        DATA(v_ref_obj) = v_ref_obj_col->get_first( ).

        WHILE v_ref_obj IS BOUND.
          DATA(v_ref_object_guid) = v_ref_obj->get_property_as_string(
                                iv_attr_name      = i_field_name
*
                            ).
          IF v_ref_object_guid   = i_refobj_val .
            "ref object to delete
            DATA(v_entity) = v_ref_obj.
            EXIT.
          ENDIF.
          v_ref_obj = v_ref_obj_col->get_next( ).
        ENDWHILE.

        CHECK v_entity IS BOUND.
        v_entity->delete( ).
        cl_crm_bol_core=>get_instance( )->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.


  ENDMETHOD.
  METHOD zif_crm_order_item~status_i.
    DATA: v_bpath TYPE string.
    DATA: v_param TYPE string.
    DATA v_entity TYPE REF TO cl_crm_bol_entity.

    IF i_new_status IS NOT INITIAL.
      TRY.
          me->switch_to_change_mode( ).
        CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
          RAISE EXCEPTION v_lock_excpetion.
      ENDTRY.

      v_bpath = |./BTItemStatusSet/BTStatusICurrent/*$|.

      TRY .
          v_entity = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->v_item_entity_root i_bpath = v_bpath  i_create = abap_true  ).

          IF v_entity IS NOT INITIAL.



            IF v_entity->is_locked( ).

              v_entity->set_property( iv_attr_name = 'ACT_STATUS' iv_value     = i_new_status ).
              v_entity->set_property( iv_attr_name = 'STATUS' iv_value     = i_new_status ).
              cl_crm_bol_core=>get_instance( )->modify( ).
              v_entity->set_property( iv_attr_name = 'ACT_STATUS' iv_value     = i_new_status ).
              v_entity->set_property( iv_attr_name = 'STATUS' iv_value     = i_new_status ).
              cl_crm_bol_core=>get_instance( )->modify( ).
            ENDIF.
          ENDIF.
        CATCH cx_root INTO DATA(lv_exception).
          zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
      ENDTRY.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
