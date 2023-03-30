"!SAP CRM Business entity API
"!Expose a set of methods to easily interact with the underlying SAP CRM business Entities
"!Supported entity:
"!1Order
"!Partner
class ZCL_CRM_ENTITY_API definition
  public
  final
  create public .

public section.

  data ORDER type ref to ZCL_CRM_ORDER_API read-only .
  data PARTNER type ref to ZCL_CRM_PARTNER_API read-only .
  data PRODUCT type ref to ZCL_CRM_PRODUCT_API read-only .
  data DOCUMENT type ref to ZCL_CRM_ORDER_ATTACHM_API read-only .
  data ACCESS_METHOD type ZCRM_API_ACCESS_METHOD read-only .
  data TRANSACTION type ref to ZIF_CRM_TRANSACTION .
  constants:
    BEGIN OF  co_access_method,
        bol_context  TYPE zcrm_api_access_method VALUE 1,
        fuba_context TYPE zcrm_api_access_method VALUE 2,
        db_context   TYPE zcrm_api_access_method VALUE 3,
        json_context TYPE zcrm_api_access_method VALUE 2,
      END OF co_access_method .
  data APPOINTMENT type ref to ZCL_CRM_APPOINTMENT_API .

    "! Start SAP CRM API in a given context.
    "! The API will work in BOL context by default
    "! @parameter i_access_method | API Context 1,2,3 - BOL,Maintain, DB
  methods CONSTRUCTOR
    importing
      !I_ACCESS_METHOD type ZCRM_API_ACCESS_METHOD default '1' .
  methods SAVE .
  methods COMMIT .
  methods ROLLBACK .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA entity_factory TYPE REF TO zcl_crm_api_entity_factory.
    METHODS get_transaction.
ENDCLASS.



CLASS ZCL_CRM_ENTITY_API IMPLEMENTATION.


  METHOD commit.

    transaction->commit(  ).
*     ZCL_CRM_BASE_TRANSACTION=>get_instance( i_access_method = access_method )->commit(  ).
  ENDMETHOD.


  METHOD constructor.

*-- Validate Access Method

    IF  zcl_ddic_utils=>domain_value_exist( i_access_method ) EQ abap_false.
      zcx_crm_entity_api=>raise_not_supported( |access method:{ i_access_method }| ).
    ENDIF.

    access_method = i_access_method.

    CASE access_method  .
      WHEN '1'.
        me->transaction = zcl_crm_transaction_bol=>get_instance( ).
      WHEN OTHERS.
    ENDCASE.

    me->entity_factory = NEW zcl_crm_api_entity_factory( i_access_method = i_access_method
                                                         i_transaction   = me->transaction ).


*-- Initialize SAP CRM 1Order API
    order = NEW zcl_crm_order_api( me->entity_factory ).


*-- Initialize SAP CRM Partner API
    partner = NEW zcl_crm_partner_api( me->entity_factory ).

*-- Initialize SAP CRM Product API
    product = NEW zcl_crm_product_api( me->entity_factory ).

*-- Initialize SAP CRM Documenz API
    document = NEW zcl_crm_order_attachm_api( me->entity_factory ).

  ENDMETHOD.


  METHOD get_transaction.

  ENDMETHOD.


  METHOD rollback.
    transaction->rollback( ).
  ENDMETHOD.


  METHOD save.
    transaction->save(  ).
*     ZCL_CRM_BASE_TRANSACTION=>get_instance( i_access_method = access_method )->save(  ).
*     order->save_changes(  ).
* partner->save_changes(  ).
  ENDMETHOD.
ENDCLASS.
