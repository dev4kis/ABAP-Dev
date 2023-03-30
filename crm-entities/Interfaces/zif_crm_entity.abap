"! SAP CRM Business Entity
INTERFACE zif_crm_entity
  PUBLIC .


  "! Get business entity key
  "!
  "! @parameter r_result | <p class="shorttext synchronized" lang="en">Entity key as GUID</p>
  METHODS get_entity_id
    RETURNING VALUE(r_result) TYPE  crmt_object_guid.

  "! <p class="shorttext synchronized" lang="en">Get entity general information</p>
  "!
  "! @parameter e_attributes | <p class="shorttext synchronized" lang="en"></p>
  METHODS get_attributes
    EXPORTING
      e_attributes TYPE data.

  METHODS set_attribute_value
    IMPORTING
      i_field_name TYPE csequence
      i_value      TYPE data.

  "! Delete CRM Entity
  METHODS delete .

  "! Reread CRM Entity. Entity attributes will be read again from the underlying layers
  METHODS refresh .

  "! Use provided Key to Read entity attributes from the underlying layer
  METHODS load_as
    IMPORTING
      i_entity_id TYPE crmt_object_guid.

  "! Create and initialize a new business entity
  METHODS
    create
      IMPORTING
        i_create_attr TYPE data.

  METHODS get_attribute_type_name
    RETURNING
      VALUE(r_result) TYPE string.


  METHODS get_transaction_context
    Returning
      VALUE(r_result) type ZCRM_TRANSACTION_DATA.

ENDINTERFACE.
