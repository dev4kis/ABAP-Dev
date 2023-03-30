"!SAP CRM 1Order API  - Access underlying CRM 1Order Entities
"!Expose collection of methods to easily interact with the underlying SAP CRM 1Order Entities
CLASS zcl_crm_order_api DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_crm_entity_api.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        i_entity_factory TYPE REF TO zcl_crm_api_entity_factory.

    METHODS create_order
      IMPORTING
        i_process_type  TYPE crmt_process_type
      RETURNING
        VALUE(r_result) TYPE REF TO zif_crm_order.

    METHODS get_order
      IMPORTING
        i_key           TYPE any
      RETURNING
        VALUE(r_result) TYPE REF TO zif_crm_order .


    METHODS
      save_changes.


  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA order_manager TYPE REF TO zif_crm_entity_mgr.
    DATA entity_factory TYPE REF TO zcl_crm_api_entity_factory.

    METHODS get_order_guid
      IMPORTING
        i_key           TYPE any
      RETURNING
        value(r_result) TYPE crmt_object_guid.


ENDCLASS.



CLASS ZCL_CRM_ORDER_API IMPLEMENTATION.


  METHOD constructor.
*-- TODO move the instance to a factory method

*-- Get Entity Manager based for given access method
* zcl_crm_api_entity_factory=>get_entity_manager( i_access_method = i_access_method ).
    me->entity_factory = i_entity_factory.
    me->order_manager  = i_entity_factory->get_entity_manager( ).
    "i_entity_factory->get_order_manager(  ).
  ENDMETHOD.


  METHOD create_order.
*-- TODO  avoid using BOL structure
    DATA v_create_attr TYPE crmst_order_create_btil.
    "--
*    TRY.

        v_create_attr-process_type = i_process_type.

        DATA(v_crm_entity) = me->order_manager->create_entity(

            i_entity_type = zif_crm_entity_mgr=>ty_entity_type-order
            i_create_attr = v_create_attr  ).

        r_result = CAST #( v_crm_entity ).

*      CATCH cx_root.
*    ENDTRY.


  ENDMETHOD.


  METHOD get_order.
 " Translate
        DATA(order_guid) = me->get_order_guid( i_key = i_key ).
        DATA(v_crm_entity) = me->order_manager->read_entity(
            i_entity_type = zif_crm_entity_mgr=>ty_entity_type-order
            i_entity_id   = order_guid  ).

        r_result = CAST #( v_crm_entity ).

  ENDMETHOD.


  METHOD save_changes.
    me->order_manager->save( ).
  ENDMETHOD.

  METHOD get_order_guid.
    r_result = i_key.
  ENDMETHOD.

ENDCLASS.
