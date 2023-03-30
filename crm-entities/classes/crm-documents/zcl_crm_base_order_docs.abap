CLASS zcl_crm_base_order_attachm DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_crm_order_attachm .

  PROTECTED SECTION.
*

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_crm_base_order_attachm IMPLEMENTATION.
  METHOD zif_crm_entity~create.

  ENDMETHOD.

  METHOD zif_crm_entity~delete.

  ENDMETHOD.

  METHOD zif_crm_entity~get_attributes.

  ENDMETHOD.

  METHOD zif_crm_entity~get_attribute_type_name.

  ENDMETHOD.

  METHOD zif_crm_entity~get_entity_id.

  ENDMETHOD.

  METHOD zif_crm_entity~get_transaction_context.

  ENDMETHOD.

  METHOD zif_crm_entity~load_as.

  ENDMETHOD.

  METHOD zif_crm_order_attachm~download_attachment.

  ENDMETHOD.
  METHOD zif_crm_order_attachm~upload_attachment.
  ENDMETHOD.

  METHOD zif_crm_entity~refresh.

  ENDMETHOD.

  METHOD zif_crm_entity~set_attribute_value.

  ENDMETHOD.

  ENDCLASS.
