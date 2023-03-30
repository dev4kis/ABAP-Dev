"! <p class="shorttext synchronized" lang="en">General Order API</p>
CLASS zcl_crm_order_bol DEFINITION
  PUBLIC
  INHERITING FROM zcl_crm_base_order
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_crm_api_entity_factory .

  PUBLIC SECTION.


    CLASS-METHODS class_constructor .

    METHODS zif_crm_order~get_docflow_list  REDEFINITION.
    METHODS  zif_crm_order~set_partner_address REDEFINITION.

    METHODS zif_crm_entity~get_attributes REDEFINITION.

    METHODS zif_crm_entity~load_as
        REDEFINITION .
    METHODS zif_crm_entity~create
        REDEFINITION .
    METHODS zif_crm_entity~get_entity_id
        REDEFINITION .
    METHODS zif_crm_entity~get_transaction_context
        REDEFINITION .

    METHODS zif_crm_order~get_customer_h_extension_field
        REDEFINITION .
    METHODS zif_crm_order~get_customer_h_extension_struc
        REDEFINITION .
    METHODS zif_crm_order~set_customer_h_extension_field
        REDEFINITION .


    METHODS zif_crm_order~get_orderadm_h_extension_field
        REDEFINITION .
    METHODS zif_crm_order~get_orderadm_h_extension_struc
        REDEFINITION .


    METHODS zif_crm_order~set_orderadm_h_extension_field
        REDEFINITION .

    METHODS zif_crm_order~set_extension_struc
        REDEFINITION .


    METHODS zif_crm_order~modify_partner
        REDEFINITION.
    METHODS zif_crm_order~get_partner_list
        REDEFINITION.
    METHODS zif_crm_order~add_partner
        REDEFINITION.
    METHODS zif_crm_order~delete_partner
        REDEFINITION.

    METHODS zif_crm_order~get_item_list
        REDEFINITION.
    METHODS zif_crm_order~modify_item
        REDEFINITION.
    METHODS zif_crm_order~add_item
        REDEFINITION.
    METHODS zif_crm_order~get_item_by_item_no
        REDEFINITION.
    METHODS zif_crm_order~delete_item
        REDEFINITION.



    METHODS zif_crm_order~get_text_list
        REDEFINITION.
    METHODS zif_crm_order~add_text
        REDEFINITION.
    METHODS zif_crm_order~modify_text
        REDEFINITION.
    METHODS zif_crm_order~delete_text
        REDEFINITION.
    METHODS zif_crm_order~add_reference_object
        REDEFINITION.
    METHODS zif_crm_order~delete_reference_object
        REDEFINITION.

    METHODS zif_crm_order~get_reference_object_list
        REDEFINITION.

    METHODS zif_crm_order~get_extension_struc
        REDEFINITION.
    METHODS zif_crm_order~get_extension_value
        REDEFINITION.
    METHODS zif_crm_order~set_extension_value
        REDEFINITION.

    METHODS zif_crm_order~set_extension_struc_2
        REDEFINITION.

    METHODS zif_crm_order~status
        REDEFINITION.

    METHODS zif_crm_order~get_dynamic_boolean
        REDEFINITION.

    METHODS zif_crm_order~set_dynamic_boolean
        REDEFINITION.

    METHODS zif_crm_order~get_appointment_list
        REDEFINITION.
    METHODS zif_crm_order~modify_appointment
        REDEFINITION.
    METHODS zif_crm_order~add_appointment
        REDEFINITION.
    METHODS zif_crm_order~add_predecessor
        REDEFINITION.
    METHODS zif_crm_order~delete_appointment
        REDEFINITION.

    METHODS zif_crm_order~get_partner_address REDEFINITION.


    METHODS zif_crm_order~get_extension_list
        REDEFINITION.
    METHODS zif_crm_order~modify_extension_list_entry
        REDEFINITION.
    METHODS zif_crm_order~add_extension_list_entry
        REDEFINITION.
    METHODS zif_crm_order~delete_extension_list_entry
        REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">BOL Core</p>
    CLASS-DATA bol_core TYPE REF TO cl_crm_bol_core .

    "! <p class="shorttext synchronized" lang="en">current root entity ( order)</p>
    DATA entity_root TYPE REF TO cl_crm_bol_entity .



    "! <p class="shorttext synchronized" lang="en">Component Set Name for Generic Interaction Layer</p>
    CONSTANTS c_btx_bol_component TYPE crmt_genil_appl VALUE 'BT' ##NO_TEXT.
    "! <p class="shorttext synchronized" lang="en">External Name of Object</p>
    CONSTANTS c_bol_root_element TYPE crmt_ext_obj_name VALUE 'BTOrder' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">initialize order based on given guid</p>
    "!
    "! @parameter i_object_guid | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_by_key
      IMPORTING
        !i_object_key TYPE any.
    "! <p class="shorttext synchronized" lang="en">create order based on process type </p>
    "!
    "! @parameter i_process_type | <p class="shorttext synchronized" lang="en">Process Type of an order</p>
    METHODS create_by_process_type
      IMPORTING
        !i_process_type TYPE crmt_process_type .

    "! <p class="shorttext synchronized" lang="en">change the status f an order to "changeable"</p>
    "!
    "! <p class="shorttext synchronized" lang="en">change the status f an order to "changeable"</p>
    METHODS switch_to_change_mode.

    "! <p class="shorttext synchronized" lang="en">helper method in order to get or create related entity</p>
    "!
    "! @parameter i_bpath | <p class="shorttext synchronized" lang="en">path of to relation ( including filter)</p>
    "! @parameter i_create | <p class="shorttext synchronized" lang="en">should the entity created ( if not available)</p>
    "! @parameter r_entity | <p class="shorttext synchronized" lang="en">returns created or found entity</p>
*    METHODS get_and_create_entity_by_bpath
*      IMPORTING
*        !i_bpath        TYPE string
*        !i_create       TYPE crmt_boolean
*      RETURNING
*        VALUE(r_entity) TYPE REF TO cl_crm_bol_entity.

    METHODS map_partner_to_partner_guid
      IMPORTING
        i_partner             TYPE bu_partner
      RETURNING
        VALUE(r_partner_guid) TYPE bu_partner_guid.

    METHODS map_partner_guid_to_partner
      IMPORTING
        i_partner_guid   TYPE bu_partner_guid
      RETURNING
        VALUE(r_partner) TYPE bu_partner.

    METHODS map_product_guid_to_product_id
      IMPORTING
        i_product_guid      TYPE comt_product_guid
      RETURNING
        VALUE(r_product_id) TYPE comt_product_id.

    METHODS convert_timestamp
      IMPORTING
        i_date          TYPE dats
        i_time          TYPE tims
        i_timezone      TYPE crmt_timezone
      RETURNING
        VALUE(r_result) TYPE timestamp.

    METHODS format_text
      IMPORTING
        i_text_object           TYPE tdobject
        i_tdid                  TYPE tdid
        i_text                  TYPE crmdt_conc_textlines
        i_langu                 TYPE langu
      RETURNING
        VALUE(r_formatted_text) TYPE string.


ENDCLASS.



CLASS ZCL_CRM_ORDER_BOL IMPLEMENTATION.


  METHOD class_constructor.

    zcl_crm_order_bol=>bol_core = cl_crm_bol_core=>get_instance( ).

    IF zcl_crm_order_bol=>bol_core->is_started EQ abap_false.

      zcl_crm_order_bol=>bol_core->start_up( iv_appl_name            = zcl_crm_order_bol=>c_btx_bol_component
                                         iv_display_mode_support = abap_true ).

    ENDIF.

    zcl_crm_order_bol=>bol_core->load_component( CONV crmt_component_name( zcl_crm_order_bol=>c_btx_bol_component ) ).

  ENDMETHOD.


  METHOD convert_timestamp.
    CONVERT DATE i_date TIME i_time INTO TIME STAMP r_result TIME ZONE i_timezone.
  ENDMETHOD.


  METHOD create_by_process_type.
    DATA: v_params_t TYPE crmt_name_value_pair_tab,
          v_params_s TYPE crmt_name_value_pair.
    DATA: v_order_factory TYPE REF TO cl_crm_bol_entity_factory.


    v_params_s-name  = 'PROCESS_TYPE'.
    v_params_s-value = i_process_type.

    APPEND v_params_s TO v_params_t.

    TRY.
        v_order_factory = zcl_crm_order_bol=>bol_core->get_entity_factory( me->c_bol_root_element ).
        me->entity_root = v_order_factory->create( v_params_t ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.



  ENDMETHOD.


  METHOD format_text.

    DATA v_converter TYPE REF TO if_crm_text_format_conversion.
    DATA: lr_obj                    TYPE REF TO object.
    DATA: v_text_head               TYPE thead.
    DATA: lt_return TYPE bapiret2_t .
    DATA: v_text TYPE string.

    DATA(v_class) =     cl_gstext_tools=>get_converter_class_name( iv_text_object = i_text_object
                                                                   iv_text_type   = i_tdid ).


    IF v_class IS INITIAL..
      v_class = 'CL_CRM_TEXT_FORMAT_CONVERSION'.
    ENDIF.

    CREATE OBJECT lr_obj TYPE (v_class).
    v_converter  ?= lr_obj.


    v_text_head-tdobject  = i_text_object.
    v_text_head-tdid  =   i_tdid.
    v_text_head-tdspras = i_langu.
    v_text_head-tdform  = 'SYSTEM'.
    v_text_head-tdstyle = 'SYSTEM'.

    v_converter->initialize( is_thead = v_text_head ).



    CALL METHOD v_converter->convert_html_to_itf
      EXPORTING
        iv_html_text = i_text
      IMPORTING
        ev_itf_text  = r_formatted_text
        et_return    = lt_return.

  ENDMETHOD.


  METHOD get_by_key.

    DATA: object_guid TYPE crmt_object_guid.

    object_guid = i_object_key.
    TRY.
        me->entity_root = zcl_crm_order_bol=>bol_core->get_root_entity( iv_object_name = me->c_bol_root_element
                                                                        iv_object_guid = object_guid ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD map_partner_guid_to_partner.
    SELECT SINGLE partner
     INTO r_partner
     FROM   but000
     WHERE partner = i_partner_guid.

  ENDMETHOD.


  METHOD map_partner_to_partner_guid.
    DATA: v_partner TYPE bu_partner.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_partner
      IMPORTING
        output = v_partner.


    SELECT SINGLE partner_guid
     INTO r_partner_guid
     FROM   but000
     WHERE partner = v_partner.

  ENDMETHOD.


  METHOD map_product_guid_to_product_id.
    SELECT SINGLE product_id
     INTO r_product_id
     FROM   comm_product
     WHERE product_guid = i_product_guid.

  ENDMETHOD.


  METHOD switch_to_change_mode.

    IF  me->entity_root->is_locked( ) = abap_false.

      IF me->entity_root->switch_to_change_mode( ) <> abap_true.
        zcx_crm_app=>raise_foreign_lock( i_text = '' ).
      ENDIF.

      me->entity_root->lock( ).

      IF me->entity_root->is_locked( ) <> 'X'.
        zcx_crm_app=>raise_foreign_lock( i_text = '' ).
      ENDIF.

      IF me->entity_root->is_changeable( ) <> 'X'.
        zcx_crm_app=>raise_foreign_lock( i_text = '' ).
      ENDIF.


      DATA v_transaction TYPE REF TO    zcl_crm_base_transaction.
      DATA v_context TYPE zcrm_transaction_data.

      TRY.
          v_transaction ?= zcl_crm_transaction_bol=>get_instance(  ).
          v_context-object = me->entity_root->get_name(  ).
          v_context-objectkey = me->zif_crm_entity~get_entity_id(  ).
          v_transaction->add( v_context ).
        CATCH cx_root.
          zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
      ENDTRY.


    ENDIF.


  ENDMETHOD.


  METHOD zif_crm_entity~create.
    FIELD-SYMBOLS <create_attr> TYPE crmst_order_create_btil.
    ASSIGN i_create_attr TO <create_attr>.

    TRY.
        super->zif_crm_entity~create(  i_create_attr = i_create_attr ).
        me->create_by_process_type( i_process_type =  <create_attr>-process_type ).
        cl_crm_bol_core=>get_instance( )->modify( ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_entity~get_attributes.

    TRY.

        DATA(v_entity) = me->entity_root->get_related_entity( iv_relation_name = 'BTOrderHeader').

        v_entity->get_properties( IMPORTING es_attributes = e_attributes ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.


  ENDMETHOD.


  METHOD zif_crm_entity~get_entity_id.
    TRY.
        me->entity_root->get_property_as_value( EXPORTING iv_attr_name = 'CRM_GUID' IMPORTING ev_result = r_result  ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_entity~get_transaction_context.
    TRY.
        r_result-object = me->entity_root->get_name(  ).
        r_result-objectkey = me->zif_crm_entity~get_entity_id(  ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_entity~load_as.
    TRY.
        super->zif_crm_entity~load_as( i_entity_id = i_entity_id ).

        me->get_by_key( i_object_key = i_entity_id ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~add_appointment.

  ENDMETHOD.


  METHOD zif_crm_order~add_extension_list_entry.

    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.


    TRY.

        DATA(v_entity) = me->entity_root->get_related_entity( 'BTOrderHeader' )->create_related_entity( iv_relation_name = CONV #( i_extension_name ) ).
        v_entity->set_properties( EXPORTING is_attributes = i_extension_value  ).
        cl_crm_bol_core=>get_instance( )->modify( ).

        DATA(v_record_guid) = v_entity->get_property_as_string( iv_attr_name      = |RECORD_ID| ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

    r_record_guid = v_record_guid.

  ENDMETHOD.


  METHOD zif_crm_order~add_item.

    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_exception).
        RAISE EXCEPTION v_lock_exception.
    ENDTRY.

    TRY .
        DATA(v_item_entity) = NEW zcl_crm_order_item_bol( i_entity_root = me->entity_root  ).
        v_item_entity->create_by_product(  i_item_properties-product ).
        v_item_entity->v_item_entity_root->set_properties( is_attributes = i_item_properties ).
        cl_crm_bol_core=>get_instance( )->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
    r_result = v_item_entity.
*

  ENDMETHOD.


  METHOD zif_crm_order~add_partner.

  ENDMETHOD.


  METHOD zif_crm_order~add_predecessor.
    DATA: v_entity TYPE REF TO cl_crm_bol_entity.
    DATA: v_bpath TYPE string.


    v_bpath = |./BTOrderHeader/BTHeaderDocFlowSet/BTDocFlowAll/*$|.


    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.
    TRY .

        IF v_entity IS INITIAL.

          v_entity = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_true  ).

        ENDIF.

        DATA(v_owm_header) = me->zif_crm_order~get_orderadm_h_extension_struc( ).

        IF v_entity->is_locked( ).
          v_entity->set_property( iv_attr_name = 'OBJKEY_A' iv_value     = i_parent_guid ).
          v_entity->set_property( iv_attr_name = 'OBJTYPE_A' iv_value     = i_parent_object_type ).

          v_entity->set_property( iv_attr_name = 'OBJKEY_B' iv_value     = v_owm_header-guid ).
          v_entity->set_property( iv_attr_name = 'OBJTYPE_B' iv_value     = v_owm_header-object_type ).

          v_entity->set_property( iv_attr_name = 'VONA_KIND' iv_value     = i_vona_kind ).
          v_entity->set_property( iv_attr_name = 'BREL_KIND' iv_value     = 'A' ). " Header - Header
          v_entity->set_property( iv_attr_name = 'RELTYPE' iv_value     = i_reltype ).

          me->bol_core->modify( ).
        ENDIF.
      CATCH cx_root INTO DATA(lv_exception).
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_order~add_reference_object.

    DATA: v_bpath TYPE string.
    DATA: v_param TYPE string.

    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.

    TRY .

        DATA(v_data_type) = cl_abap_typedescr=>describe_by_data( i_reference_object )->get_relative_name( ).
        DATA(v_product_id) = SWITCH comt_product_id( v_data_type
                  WHEN 'COMT_PRODUCT_GUID' THEN me->map_product_guid_to_product_id( i_reference_object )
                  WHEN 'COMT_PRODUCT_ID' THEN i_reference_object
                  ELSE  i_reference_object ).



        v_bpath = |./BTOrderHeader/BTHeaderBOSSet/BTRefObjSet_A/BTRefObjectAll|.
** Add filter
        v_param = |[@PRODUCT_ID="{ v_product_id }"]|.
        v_bpath = |{ v_bpath }{ v_param }/*$|.


        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath i_create = abap_true  ).

        cl_crm_bol_core=>get_instance( )->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.


  ENDMETHOD.


  METHOD zif_crm_order~add_text.
    DATA v_converter TYPE REF TO if_crm_text_format_conversion .
    DATA v_entity_attr TYPE zcrmst_text_btil_t.
    DATA v_bpath TYPE string.

    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.

    TRY .
        IF i_append = abap_true.
          v_bpath = |./BTOrderHeader/BTHeaderTextSet/ZBTTextHAll[(@TDID="{ i_tdid }")&(@TDSPRAS="{ i_langu }")]/*$|.
          DATA(entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_true  ).
        ELSE.
          DATA(v_tmp_object) = me->entity_root->get_related_entity( iv_relation_name  = 'BTOrderHeader' ).
          IF v_tmp_object IS BOUND.
            v_tmp_object = v_tmp_object->get_related_entity( iv_relation_name  = 'BTHeaderTextSet' ).
            IF  v_tmp_object IS BOUND.
              entity = v_tmp_object->create_related_entity( iv_relation_name = 'ZBTTextHAll' ).
            ENDIF.
          ENDIF.
        ENDIF.

        IF entity IS BOUND.
          entity->set_property( iv_attr_name = 'TDID' iv_value = i_tdid ).
          entity->set_property( iv_attr_name = 'TDSPRAS' iv_value = i_langu ).
*            entity->set_property( iv_attr_name = 'TDOBJECT' iv_value = 'CRM_ORDERH' ).
          IF i_formatted = abap_false.
            entity->set_property( iv_attr_name = 'CONC_LINES' iv_value = i_text ).
          ELSE.


            DATA: v_text TYPE string.

            v_text = format_text(
                       i_text_object = i_text_object
                       i_tdid        = i_tdid
                       i_text        = i_text
                       i_langu       = i_langu
                     ).
            entity->set_property( iv_attr_name = 'CONC_FORMATTED_LINES' iv_value = v_text ).
          ENDIF.
        ENDIF.




*        entity->set_property( iv_attr_name = 'PARTNER_NO' iv_value     = partner_id ).
        cl_crm_bol_core=>get_instance( )->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.



  ENDMETHOD.


  METHOD zif_crm_order~delete_appointment.

  ENDMETHOD.


  METHOD zif_crm_order~delete_extension_list_entry.

    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.


    TRY.
        DATA(v_coll) = me->entity_root->get_related_entity( 'BTOrderHeader' )->get_related_entities( iv_relation_name = CONV #( i_extension_name ) ).
        DATA(v_entity) = v_coll->get_first(  ).


        WHILE v_entity IS BOUND.
          IF v_entity->get_property_as_string( iv_attr_name = 'RECORD_ID' ) = i_record_guid.
            v_entity->delete(  ).
            cl_crm_bol_core=>get_instance( )->modify( ).
          ENDIF.
          v_entity = v_coll->get_next(  ).

        ENDWHILE.
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.


  ENDMETHOD.


  METHOD zif_crm_order~delete_item.

    DATA(v_bpath) = |./BTOrderHeader/BTHeaderItemsExt/BTOrderItemAll[@NUMBER_INT="{ i_item_number }"]/*$|.
    DATA(v_item_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_false  ).

    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_exception).
        RAISE EXCEPTION v_lock_exception.
    ENDTRY.

    IF v_item_entity IS BOUND.
      v_item_entity->delete( ).
    ENDIF.

    cl_crm_bol_core=>get_instance( )->modify( ).
    v_item_entity = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_false  ).
    IF v_item_entity IS INITIAL.
      r_success = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_crm_order~delete_partner.
    DATA: partner TYPE REF TO zif_crm_partner.
    DATA: partner_id TYPE bu_partner.
    DATA: entity TYPE REF TO cl_crm_bol_entity.
    DATA: bpath TYPE string.
    DATA: param TYPE string.


    DATA(typedescr) = cl_abap_typedescr=>describe_by_data( i_partner ).



    IF typedescr IS INSTANCE OF cl_abap_elemdescr.
      partner_id = SWITCH #( typedescr->get_relative_name( )
                        WHEN 'BU_PARTNER_GUID' THEN me->map_partner_guid_to_partner( i_partner_guid = CONV #( i_partner ) )
                        WHEN 'BU_PARTNER' THEN i_partner
                        ELSE  partner_id ).

    ELSEIF typedescr IS INSTANCE OF cl_abap_refdescr.
      partner ?= i_partner.
      partner_id = partner->get_number(  ).

    ENDIF.

    TRY .

        bpath = |./BTOrderHeader/BTHeaderPartnerSet/BTPartnerAll|.
        IF i_main = abap_true.
          param = |[((@PARTNER_FCT="{ i_partner_fct }")&(@PARTNER_NO="{ partner_id }"))&(@MAINPARTNER="X")]|. "Add Filter
        ELSE.
          param = |[(@PARTNER_FCT="{ i_partner_fct }")&(@PARTNER_NO="{ partner_id }")]|. "Add Filter
        ENDIF.
        bpath = |{ bpath }{ param }/*$|.

        entity = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = bpath  i_create = abap_false ).



        CHECK entity IS NOT INITIAL.
        TRY .
*
            me->switch_to_change_mode(  ).
            entity->delete( ).
            cl_crm_bol_core=>get_instance( )->modify( ).
          CATCH zcx_crm_app INTO DATA(v_exception).
            RAISE EXCEPTION v_exception.
        ENDTRY.
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~delete_reference_object.

    DATA: v_bpath TYPE string.
    DATA: v_param TYPE string.

    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.


    TRY .

        DATA(v_data_type) = cl_abap_typedescr=>describe_by_data( i_reference_object )->get_relative_name( ).
        DATA(v_product_id) = SWITCH comt_product_id( v_data_type
                 WHEN 'COMT_PRODUCT_GUID' THEN me->map_product_guid_to_product_id( i_reference_object )
                 WHEN 'COMT_PRODUCT_ID' THEN i_reference_object
                 ELSE  i_reference_object ).



        v_bpath = |./BTOrderHeader/BTHeaderBOSSet/BTRefObjSet_A/BTRefObjectAll|.
** Add filter
        v_param = |[@PRODUCT_ID="{ v_product_id }"]|.
        v_bpath = |{ v_bpath }{ v_param }/*$|.


        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath i_create = abap_false  ).
        v_entity->delete(  ).
        cl_crm_bol_core=>get_instance( )->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.



  ENDMETHOD.


  METHOD zif_crm_order~delete_text.
    TRY .
        me->switch_to_change_mode(  ).
      CATCH zcx_crm_app INTO DATA(v_exception).
        RAISE EXCEPTION v_exception.
    ENDTRY.

    DATA(v_bpath) = |./BTOrderHeader/BTHeaderTextSet/ZBTTextHAll[(@TDID="{ i_tdid }")&(@TDSPRAS="{ i_langu }")]/*$|.
    TRY.
        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_false ).
        IF v_entity IS NOT INITIAL.
          v_entity->delete( ).
        ENDIF.
        me->bol_core->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_order~get_appointment_list.

    DATA: v_bpath TYPE string.
    DATA: v_param TYPE string.
    TRY .

        v_bpath = |./BTOrderHeader/BTHeaderDatesSet/BTDatesAll|.

        IF i_type IS NOT INITIAL.
          v_param = |[@APPT_TYPE="{ i_type }"]|.
        ENDIF.

        v_bpath = v_bpath && v_param && '/*$'.

        DATA(v_collection) = me->entity_root->get_related_entities_by_bpath( iv_bpath_statement = v_bpath ).

        DATA(v_entity) = v_collection->get_first(  ).
        WHILE v_entity IS BOUND.


          r_result = VALUE #( (  appointment_guid = v_entity->get_property_as_string( iv_attr_name = 'APPT_GUID' )
                                 appointment_type = CONV crmt_apptype( v_entity->get_property_as_string( iv_attr_name = 'APPT_TYPE' ) )
                                 appointment_ref  = NEW zcl_crm_appointment_bol( i_entity_root = me->entity_root
                                                                                 i_appointment = CONV crmt_apptype( v_entity->get_property_as_string( iv_attr_name = 'APPT_TYPE' ) )
                                                                                 i_change_mode = abap_false  )  ) ).

          v_entity = v_collection->get_next(  ).
        ENDWHILE.

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.


  ENDMETHOD.


  METHOD zif_crm_order~get_customer_h_extension_field.
TRY.
        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = |./BTOrderHeader/BTHeaderCustExt/*$|  i_create = abap_false  ).

        IF v_entity IS BOUND.
          v_entity->get_property_as_value( EXPORTING iv_attr_name = i_field_name IMPORTING ev_result = e_result ).
        ENDIF.
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~get_customer_h_extension_struc.
   TRY.
        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = |./BTOrderHeader/BTHeaderCustExt/*$|  i_create = abap_false  ).
        IF v_entity IS BOUND.
          v_entity->get_properties( IMPORTING es_attributes = r_struc ).
        ENDIF.
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_order~get_docflow_list.

    DATA v_entity_attr TYPE crmst_docflow_btil.
    DATA(v_bpath)  = |./BTOrderHeader/BTHeaderDocFlowSet/BTDocFlowAll/*$|.

    TRY.

        DATA(v_result) = me->entity_root->get_related_entities_by_bpath( v_bpath ).
        DATA(v_entity) = v_result->get_first( ).

        DO.

          IF NOT v_entity IS BOUND.  EXIT. ENDIF.

          v_entity->get_properties( IMPORTING es_attributes = v_entity_attr ).

          APPEND v_entity_attr TO r_result.

          v_entity = v_result->get_next( ).

        ENDDO.

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~get_dynamic_boolean.

    DATA: v_bpath TYPE string.
    DATA: v_param TYPE string.

    TRY .
        v_bpath = |./BTOrderHeader/ZAEXT_BOL_RELAT00005T|.
* Add filter
        v_param = |[@ZZFLAG_KEY="{ i_dynamic_bool_key }"]|.
        v_bpath = |{ v_bpath }{ v_param }/*$|.

        DATA(v_entity) =  me->entity_root->get_related_entities_by_bpath( iv_bpath_statement = v_bpath )->get_first( ).
        IF v_entity IS NOT BOUND.
          r_value = abap_false.
          RETURN.
        ENDIF.
        r_value-flag_value = v_entity->get_property_as_string( iv_attr_name = 'ZZFLAG_VAL' ) .
         v_entity->get_property_as_value( EXPORTING iv_attr_name = 'ZZFLAG_VAL_DATE' IMPORTING ev_result =  r_value-date_of_value ) .
        r_value-flag_key = v_entity->get_property_as_string( iv_attr_name = 'ZZFLAG_KEY' ) .
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~get_extension_list.
    DATA: lr_attr_ref         TYPE REF TO data.
    FIELD-SYMBOLS: <attr_struc> TYPE any.

    TRY.
        DATA(v_coll) = me->entity_root->get_related_entity( 'BTOrderHeader' )->get_related_entities( iv_relation_name = CONV #( i_extension_name ) ).
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


  METHOD zif_crm_order~get_extension_struc.

    DATA: v_entity TYPE REF TO cl_crm_bol_entity.
    DATA: v_bpath TYPE string.

    v_bpath = |./BTOrderHeader/{ i_extension_name }/*$|.

    TRY.
        v_entity = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_false  ).
        IF v_entity IS BOUND.
          v_entity->get_properties( IMPORTING es_attributes = r_struc ).
        ENDIF.
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_order~get_extension_value.
    DATA: v_entity TYPE REF TO cl_crm_bol_entity.
    DATA: v_bpath TYPE string.

    v_bpath = |./BTOrderHeader/{ i_extension_name }/*$|.

    TRY.
        v_entity = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_false  ).
        IF v_entity IS BOUND.
          v_entity->get_property_as_value( EXPORTING iv_attr_name = i_extension_field IMPORTING ev_result = e_value ).
        ENDIF.
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~get_item_by_item_no.
    DATA: v_entity TYPE REF TO cl_crm_bol_entity.
    DATA: v_bpath TYPE string.

    DATA(v_item_no) = CONV crmt_item_no( i_key ) .

    v_bpath = |./BTOrderHeader/BTHeaderItemsExt/BTOrderItemAll[@NUMBER_INT="{ v_item_no }"]/*$|.

    DATA(v_item_entity) = NEW zcl_crm_order_item_bol( i_entity_root = me->entity_root  ).
    v_item_entity->v_item_entity_root = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_false  ).

    r_result = CAST #( v_item_entity ).
  ENDMETHOD.


  METHOD zif_crm_order~get_item_list.
    DATA: v_item_entity TYPE REF TO cl_crm_bol_entity.
    DATA v_item_props TYPE crmst_admini_btil.
    DATA v_item_api TYPE REF TO zcl_crm_order_item_bol.


    DATA(v_bpath) = |./BTOrderHeader/BTHeaderItemsExt/BTOrderItemAll/*$|.

    DATA(v_items_col) = me->entity_root->get_related_entities_by_bpath( v_bpath ).

*    DATA(v_item_api) = NEW zcl_crm_order_item_bol( i_entity_root = me->entity_root  ).

    v_item_entity = v_items_col->get_first( ).

    WHILE v_item_entity IS BOUND.
      v_item_entity->get_properties(
        IMPORTING
          es_attributes = v_item_props
      ).

      v_item_api = NEW #( i_entity_root = me->entity_root ).

      v_item_api->v_item_entity_root = v_item_entity.

      DATA(v_order_items) = VALUE zcrm_order_items( item_properties = v_item_props item_api = v_item_api ).
      APPEND v_order_items TO r_items.
      CLEAR: v_item_props
            ,v_item_api.

      v_item_entity = v_items_col->get_next( ).

    ENDWHILE.

  ENDMETHOD.


  METHOD zif_crm_order~get_orderadm_h_extension_field.
    TRY.
        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = |./BTOrderHeader/*$|  i_create = abap_true  ).

        IF v_entity IS BOUND.
          v_entity->get_property_as_value( EXPORTING iv_attr_name = i_field_name IMPORTING ev_result = e_result ).
        ENDIF.
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~get_orderadm_h_extension_struc.
    TRY.
        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = |./BTOrderHeader/*$|  i_create = abap_false  ).
        IF v_entity IS BOUND.
          v_entity->get_properties( IMPORTING es_attributes = r_struc ).
        ENDIF.
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~get_partner_address.


    DATA: v_bpath TYPE string.
    DATA: v_address_attr TYPE crmst_partneraddress_btil.


    TRY .

        v_bpath = |./BTOrderHeader/BTHeaderPartnerSet/BTPartnerAll[@PARTNER_FCT="{ i_partner_fct }"]/BTPartnerAddress/*$|.

        DATA(v_collection) = me->entity_root->get_related_entities_by_bpath(  iv_bpath_statement = v_bpath ).

        DATA(v_entity) = v_collection->get_first( ).

        IF v_entity IS NOT BOUND. RETURN. ENDIF.

        v_entity->get_properties( IMPORTING es_attributes = v_address_attr ).

        r_result = v_address_attr.

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~get_partner_list.
    DATA: entity TYPE REF TO cl_crm_bol_entity.
    DATA: bpath TYPE string.
    DATA: param TYPE string.
    DATA: v_partner_guid TYPE bu_partner_guid.
    DATA: v_partner_id TYPE bu_partner.

    bpath = |./BTOrderHeader/BTHeaderPartnerSet/BTPartnerAll|.
    IF i_partner_fct IS NOT INITIAL.
      param = |[@PARTNER_FCT="{ i_partner_fct }"]|. "Add Filter
    ENDIF.
    bpath = |{ bpath }{ param }/*$|.

    TRY .


        DATA(collection) = me->entity_root->get_related_entities_by_bpath(  iv_bpath_statement = bpath ).

        entity = collection->get_first(  ).
        WHILE entity IS BOUND.

          DATA(partner_id) =  entity->get_property_as_string(  iv_attr_name = 'PARTNER_NO' ).
          IF strlen( partner_id ) <= 10.
            v_partner_guid = me->map_partner_to_partner_guid( CONV bu_partner( partner_id ) ).
            v_partner_id   = partner_id.
          ELSE.
            v_partner_guid = partner_id..
            v_partner_id   = me->map_partner_guid_to_partner( CONV bu_partner_guid( partner_id ) ).
          ENDIF.


*
          IF v_partner_guid IS NOT INITIAL.
            APPEND  VALUE #( partner = |{ v_partner_id ALPHA = IN }|
                            partner_ref  = NEW zcl_crm_entity_api( i_access_method = '1' )->partner->read( i_partner_id = v_partner_guid )
                            partner_fct  = entity->get_property_as_string(  iv_attr_name = 'PARTNER_FCT' )
                            partner_guid = v_partner_guid
                            main        = entity->get_property_as_string(  iv_attr_name = 'MAINPARTNER' )
                            )        TO r_result.
          ENDIF.
          entity = collection->get_next(  ).
        ENDWHILE.


      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~get_reference_object_list.
    DATA: v_bpath TYPE string.
    DATA: v_crm_entity TYPE REF TO zcl_crm_entity_api.
    DATA: v_product TYPE zcrm_api_order_product_s.

    v_bpath = |./BTOrderHeader/BTHeaderBOSSet/BTRefObjSet_A/BTRefObjectAll/*$|.

*    me->switch_to_change_mode(  ).
    TRY .


        v_crm_entity = NEW zcl_crm_entity_api( i_access_method = '1' ).

        DATA(v_collection) = me->entity_root->get_related_entities_by_bpath( iv_bpath_statement = v_bpath ).

        DATA(v_entitiy) = v_collection->get_first(  ).
        WHILE v_entitiy IS BOUND.

          v_product-product_id = v_entitiy->get_property_as_string( iv_attr_name = 'PRODUCT_ID' ).
          v_product-product_ref ?=  v_crm_entity->product->get_product( i_key = v_product-product_id ).
          APPEND v_product TO r_result.
          v_entitiy = v_collection->get_next(  ).

        ENDWHILE.
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_order~get_text_list.
    DATA v_entity_attr TYPE crmst_text_btil.
    DATA v_para TYPE string.
    DATA(v_bpath)  = |./BTOrderHeader/BTHeaderTextSet/ZBTTextHAll|.
    IF i_filter_langu IS NOT INITIAL AND i_filter_tdid IS NOT INITIAL.
      v_para = |[(@TDID="{ i_filter_tdid }")&(@TDSPRAS="{ i_filter_langu }")]|.
    ENDIF.
    IF i_filter_langu IS NOT INITIAL AND i_filter_tdid IS INITIAL.
      v_para = |[@TDSPRAS="{ i_filter_langu }"]|.
    ENDIF.
    IF i_filter_langu IS  INITIAL AND i_filter_tdid IS NOT INITIAL.
      v_para = |[@TDID="{ i_filter_tdid }"]|.
    ENDIF.

    v_bpath = v_bpath && v_para && '/*$'.

    TRY.

        DATA(v_result) = me->entity_root->get_related_entities_by_bpath( v_bpath ).
        DATA(v_entity) = v_result->get_first( ).
        DO.

          IF NOT v_entity IS BOUND.  EXIT. ENDIF.

          v_entity->get_properties( IMPORTING es_attributes = v_entity_attr ).

          APPEND v_entity_attr TO r_result.

          v_entity = v_result->get_next( ).

        ENDDO.

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~modify_appointment.
    TRY.
        switch_to_change_mode( ).

        DATA(v_item_entity) = NEW zcl_crm_appointment_bol( i_entity_root = me->entity_root i_appointment = i_appointment i_change_mode = abap_true  ).
        v_item_entity->zif_crm_appointment~time_stamp_from(  i_from_ts ).
        v_item_entity->zif_crm_appointment~timezone_from( sy-zonlo ).

        cl_crm_bol_core=>get_instance( )->modify( ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_order~modify_extension_list_entry.

  ENDMETHOD.


  METHOD zif_crm_order~modify_item.

    cl_crm_bol_core=>get_instance( )->modify( ).
  ENDMETHOD.


  METHOD zif_crm_order~modify_partner.
    DATA: partner    TYPE REF TO zif_crm_partner,
          partner_id TYPE bu_partner,
          entity     TYPE REF TO cl_crm_bol_entity,
          bpath      TYPE string,
          param      TYPE string.

    DATA(typedescr) = cl_abap_typedescr=>describe_by_data( i_partner ).

    IF typedescr IS INSTANCE OF cl_abap_elemdescr.
      partner_id = SWITCH #( typedescr->get_relative_name( )
                        WHEN 'BU_PARTNER_GUID' THEN me->map_partner_guid_to_partner( i_partner_guid = CONV #( i_partner ) )
                        WHEN 'BU_PARTNER' THEN i_partner
                        ELSE  partner_id ).
    ELSEIF typedescr IS INSTANCE OF cl_abap_refdescr.
      partner    ?= i_partner.
      partner_id  = partner->get_number( ).
    ENDIF.

    TRY.
        bpath = |./BTOrderHeader/BTHeaderPartnerSet/BTPartnerAll|.

        IF i_add_partner = abap_true.
          IF i_main = abap_true.
            param = |[(@PARTNER_FCT="{ i_partner_fct }")&(@MAINPARTNER="X")&(@PARTNER_NO="{ partner_id }")]|. "Add Filter
          ELSE.
            param = |[(@PARTNER_FCT="{ i_partner_fct }")&(@PARTNER_NO="{ partner_id }")]|. "Add Filter
          ENDIF.
        ELSE.
          IF i_main = abap_true.
            param = |[(@PARTNER_FCT="{ i_partner_fct }")&(@MAINPARTNER="X")]|. "Add Filter
          ELSE.
            param = |[@PARTNER_FCT="{ i_partner_fct }"]|. "Add Filter
          ENDIF.
        ENDIF.

        bpath = |{ bpath }{ param }/*$|.

        TRY.
            switch_to_change_mode(  ).
          CATCH zcx_crm_app INTO DATA(v_exception).
            RAISE EXCEPTION v_exception.
        ENDTRY.

        entity = zcl_api_util=>get_and_create_entity_by_bpath( i_entity = entity_root
                                                               i_bpath  = bpath
                                                               i_create = abap_true  ).

        CHECK entity IS NOT INITIAL.

        entity->set_property( iv_attr_name = 'PARTNER_NO'
                              iv_value     = partner_id ).

        cl_crm_bol_core=>get_instance( )->modify( ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( i_text = cl_bsp_get_text_by_alias=>get_text( alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR'
                                                                                language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_order~modify_text.

    TRY .
        me->switch_to_change_mode(  ).
      CATCH zcx_crm_app INTO DATA(v_exception).
        RAISE EXCEPTION v_exception.
    ENDTRY.

    DATA(v_bpath) = |./BTOrderHeader/BTHeaderTextSet/ZBTTextHAll[(@TDID="{ i_tdid }")&(@TDSPRAS="{ i_langu }")]/*$|.
    TRY.
        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_false ).
        IF v_entity IS NOT INITIAL.
          v_entity->set_property( iv_attr_name = 'TDID' iv_value = i_tdid ).
          v_entity->set_property( iv_attr_name = 'TDSPRAS' iv_value = i_langu ).
*            entity->set_property( iv_attr_name = 'TDOBJECT' iv_value = 'CRM_ORDERH' ).
          IF i_formatted = abap_false.
            v_entity->set_property( iv_attr_name = 'CONC_LINES' iv_value = i_text ).
          ELSE.

            DATA v_text TYPE string.

            v_text = format_text( i_text_object = i_text_object
                                  i_tdid        = i_tdid
                                  i_text        = i_text
                                  i_langu       = i_langu
                                ).

            v_entity->set_property( iv_attr_name = 'CONC_FORMATTED_LINES' iv_value = v_text ).
          ENDIF.
        ENDIF.




*        entity->set_property( iv_attr_name = 'PARTNER_NO' iv_value     = partner_id ).
        me->bol_core->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~set_customer_h_extension_field.
    DATA: v_bpath TYPE string.

    v_bpath = |./BTOrderHeader/BTHeaderCustExt|.
    TRY.
        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_true  ).
        CHECK v_entity IS NOT INITIAL.
        TRY.
            me->switch_to_change_mode( ).
          CATCH zcx_crm_app INTO DATA(v_lock_exception).
            RAISE EXCEPTION v_lock_exception.
        ENDTRY.
        v_entity->set_property( iv_attr_name = i_field_name iv_value     = i_value ).
        cl_crm_bol_core=>get_instance( )->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
    cl_crm_bol_core=>get_instance( )->modify( ).

  ENDMETHOD.


  METHOD zif_crm_order~set_dynamic_boolean.
    DATA: v_bpath TYPE string.
    DATA: v_param TYPE string.
    DATA: v_flag TYPE zbtx_exas_request_3state_bool.

    TRY .
*
        me->switch_to_change_mode(  ).

      CATCH zcx_crm_app INTO DATA(v_exception).
        RAISE EXCEPTION v_exception.
    ENDTRY.


    IF i_value = abap_true.
      v_flag = 'Y'.
    ELSE.
      v_flag = i_value.
    ENDIF.

    v_bpath = |./BTOrderHeader/ZAEXT_BOL_RELAT00005T|.
* Add filter
    v_param = |[@ZZFLAG_KEY="{ i_dynamic_bool_key }"]|.
    v_bpath = |{ v_bpath }{ v_param }/*$|.

    TRY.
        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_true  ).
        v_entity->set_property( iv_attr_name = 'ZZFLAG_VAL' iv_value     = v_flag ).
        IF v_flag IS NOT INITIAL.

          v_entity->set_property(
            EXPORTING
              iv_attr_name =  'ZZFLAG_VAL_DATE'                " Component Name
              iv_value     = sy-datum
          ).

        ENDIF.
        cl_crm_bol_core=>get_instance( )->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.


  ENDMETHOD.


  METHOD zif_crm_order~set_extension_struc.


    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.

    DATA(v_bpath) = |./BTOrderHeader/{ i_extension_name }/*$|.
    DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_true  ).

    TRY.

        v_entity->set_properties( EXPORTING is_attributes = i_extension_value  ).
        cl_crm_bol_core=>get_instance( )->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.



  ENDMETHOD.


  METHOD zif_crm_order~set_extension_struc_2.
    DATA: v_entity TYPE REF TO cl_crm_bol_entity.
    DATA: v_bpath TYPE string .
    DATA: v_bpath_part TYPE string .
    DATA: lv_counter TYPE i.


    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
        RAISE EXCEPTION v_lock_excpetion.
    ENDTRY.

    TRY.
        v_entity = me->entity_root->get_related_entity( 'BTOrderHeader' ).
        LOOP AT i_extension_tab ASSIGNING FIELD-SYMBOL(<fs_extension>).

          IF <fs_extension>-filter IS NOT INITIAL.
            lv_counter = 0.
            v_bpath = |./{ <fs_extension>-extension  }[|.
            LOOP AT <fs_extension>-filter ASSIGNING FIELD-SYMBOL(<fs_filter_data>). " only one today supported
              lv_counter = lv_counter + 1.
              IF sy-tabix = 1.
                v_bpath_part = |(@{ <fs_filter_data>-name }="{ <fs_filter_data>-value }")|.
              ELSE.
                v_bpath_part = v_bpath_part && |&(@{ <fs_filter_data>-name }="{ <fs_filter_data>-value }")|.
              ENDIF.

              IF lv_counter MOD 2 = 0.

                v_bpath_part = '(' && v_bpath_part && ')'.

              ENDIF.

            ENDLOOP.
            v_bpath = v_bpath && v_bpath_part && |]/*$|.
          ELSE.
            v_bpath = |./{ <fs_extension>-extension }/*$|.
          ENDIF.
          DATA(v_coll) = v_entity->get_related_entities_by_bpath( iv_bpath_statement = v_bpath ).

          IF   v_coll->size(  ) = 0.
            IF <fs_extension>-create = abap_true.
              v_entity = v_entity->create_related_entity( iv_relation_name = <fs_extension>-extension ).
              v_entity->set_properties( i_extension_value ).
              cl_crm_bol_core=>get_instance( )->modify( ).
            ENDIF.
          ELSE.
            v_entity = v_coll->get_first(  ).
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~set_extension_value.
    DATA: v_entity TYPE REF TO cl_crm_bol_entity.
    DATA: v_bpath TYPE string.

    v_bpath = |./BTOrderHeader/{ i_extension_name }/*$|.
    TRY.
        v_entity = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_true  ).

        IF v_entity IS BOUND.
          v_entity->set_property( EXPORTING iv_attr_name = i_extension_field iv_value = i_value ).
        ENDIF.
        cl_crm_bol_core=>get_instance( )->modify( ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order~set_orderadm_h_extension_field.
    TRY.
        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = |./BTOrderHeader/*$|  i_create = abap_false  ).
        IF v_entity IS BOUND.
          v_entity->set_property( EXPORTING iv_attr_name = i_field_name iv_value = i_value ).
        ENDIF.
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_order~set_partner_address.

    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_exception).
        RAISE EXCEPTION v_lock_exception.
    ENDTRY.

    DATA(v_bpath) = |./BTOrderHeader/BTHeaderPartnerSet/BTPartnerAll[@PARTNER_FCT="{ i_partner_fct }"]/BTPartnerAddress/*$|.

    TRY .
        DATA(v_entity) = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root
                                                                        i_bpath  = v_bpath
                                                                        i_create = abap_true  ).


        v_entity->set_properties( EXPORTING is_attributes = i_address ).

        me->bol_core->modify( ).

      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.



  ENDMETHOD.


  METHOD zif_crm_order~status.
    DATA: v_entity TYPE REF TO cl_crm_bol_entity.
    DATA: v_bpath TYPE string.


    v_bpath = |./BTOrderHeader/BTHeaderStatusSet/BTStatusHCurrent/*$|.
    v_entity = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_false  ).

    IF i_new_status IS INITIAL.
      r_status = VALUE #( status = v_entity->get_property_as_string(  iv_attr_name = 'STATUS' )
                          txt30 = v_entity->get_property_as_string(  iv_attr_name = 'TXT30' ) ).

    ELSE.

      TRY.
          me->switch_to_change_mode( ).
        CATCH zcx_crm_app INTO DATA(v_lock_excpetion).
          RAISE EXCEPTION v_lock_excpetion.
      ENDTRY.
      TRY .

          IF v_entity IS INITIAL.

            v_entity = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = v_bpath  i_create = abap_true  ).

          ENDIF.

          IF v_entity->is_locked( ).

            v_entity->set_property( iv_attr_name = 'ACT_STATUS' iv_value     = i_new_status ).
            v_entity->set_property( iv_attr_name = 'STATUS' iv_value     = i_new_status ).
            cl_crm_bol_core=>get_instance( )->modify( ).
            v_entity->set_property( iv_attr_name = 'ACT_STATUS' iv_value     = i_new_status ).
            v_entity->set_property( iv_attr_name = 'STATUS' iv_value     = i_new_status ).
            cl_crm_bol_core=>get_instance( )->modify( ).
          ENDIF.
        CATCH cx_root INTO DATA(lv_exception).
          zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
      ENDTRY.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
