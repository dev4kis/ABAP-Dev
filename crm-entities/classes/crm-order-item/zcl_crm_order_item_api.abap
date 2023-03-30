class ZCL_CRM_ORDER_ITEM_API definition
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_crm_entity_api.

public section.
protected section.
private section.

  data ORDER_MANAGER type ref to ZIF_CRM_ENTITY_MGR .
  data ENTITY_FACTORY type ref to ZCL_CRM_API_ENTITY_FACTORY .

  methods CONSTRUCTOR
    importing
      !I_ENTITY_FACTORY type ref to ZCL_CRM_API_ENTITY_FACTORY .
ENDCLASS.



CLASS ZCL_CRM_ORDER_ITEM_API IMPLEMENTATION.


  method CONSTRUCTOR.
*    me->entity_factory = i_entity_factory.
*    me->order_manager  = i_entity_factory->get_entity_manager( ).
  endmethod.
ENDCLASS.
