CLASS zcl_crm_base_partner DEFINITION
  ABSTRACT
  PUBLIC
  CREATE PROTECTED.


  PUBLIC SECTION.

    INTERFACES : zif_crm_partner.

  PROTECTED SECTION.


    METHODS create_partner ABSTRACT
      IMPORTING
        i_create_attr TYPE crmst_bp_create_buil.

    METHODS load_partner ABSTRACT
      IMPORTING
        i_entity_id    TYPE crmt_object_guid
      RETURNING
        VALUE(r_found) TYPE abap_bool.


  PRIVATE SECTION.
    DATA entity_key TYPE crmt_object_guid.
ENDCLASS.



CLASS ZCL_CRM_BASE_PARTNER IMPLEMENTATION.


  METHOD zif_crm_entity~create.
    DATA v_create_attr TYPE crmst_bp_create_buil.

    MOVE-CORRESPONDING  i_create_attr TO v_create_attr.

*-- Check if required parameters were provided
    create_partner( v_create_attr ).

  ENDMETHOD.


  METHOD zif_crm_entity~delete.


  ENDMETHOD.


  METHOD zif_crm_entity~get_attributes.

    e_attributes = me->zif_crm_partner~get_general_data( ).

  ENDMETHOD.


  METHOD zif_crm_entity~get_attribute_type_name.

  ENDMETHOD.


  METHOD zif_crm_entity~get_entity_id.
    r_result =  me->entity_key.
  ENDMETHOD.


  METHOD zif_crm_entity~get_transaction_context.

  ENDMETHOD.


  METHOD zif_crm_entity~load_as.

    IF  load_partner( i_entity_id ) EQ abap_false.
      zcx_crm_entity_api=>raise_not_found(  |Partner GUID { i_entity_id }| ).
    ENDIF.

    me->entity_key = i_entity_id.

  ENDMETHOD.


  METHOD zif_crm_entity~refresh.

  ENDMETHOD.


  METHOD zif_crm_entity~set_attribute_value.

  ENDMETHOD.


METHOD zif_crm_partner~create_extension.
  ENDMETHOD.


  method ZIF_CRM_PARTNER~DELETE_ENTENSION_RECORD.
  endmethod.


  METHOD zif_crm_partner~get_address_list.

  ENDMETHOD.


  METHOD zif_crm_partner~get_contact_relation.

  ENDMETHOD.


  METHOD zif_crm_partner~get_extension_table.

  ENDMETHOD.


  METHOD zif_crm_partner~get_general_data.

  ENDMETHOD.


  METHOD zif_crm_partner~get_guid.

    r_partner_guid = me->zif_crm_entity~get_entity_id( ).

  ENDMETHOD.


  METHOD zif_crm_partner~get_number.

   r_partner = me->zif_crm_partner~get_general_data( )-bp_number.

  ENDMETHOD.


  METHOD zif_crm_partner~get_relations_list.

  ENDMETHOD.


  METHOD zif_crm_partner~get_relation_by_category.

  ENDMETHOD.


  method ZIF_CRM_PARTNER~UPDATE_HEADER_DATA.
  endmethod.


  method ZIF_CRM_PARTNER~UPDATE_ID_NUMBERS.
  endmethod.
ENDCLASS.
