"! SAP CRM Business Entity Manager - manage a set of related business entity
INTERFACE zif_crm_entity_mgr
  PUBLIC .
  CONSTANTS: BEGIN OF ty_entity_type,
               order    TYPE char1 VALUE '1',
               partner  TYPE char1 VALUE '2',
               product  TYPE char1 VALUE '3',
               document TYPE char1 VALUE '4',
             END  OF ty_entity_type.
  DATA context TYPE zcrm_api_access_method READ-ONLY .

  METHODS create_entity
    IMPORTING
      i_create_attr   TYPE data
      i_entity_type   TYPE csequence
    RETURNING
      VALUE(r_result) TYPE REF TO zif_crm_entity .

  METHODS delete_entity
    IMPORTING
      i_entity        TYPE crmt_object_guid
      i_entity_type   TYPE csequence
    RETURNING
      VALUE(r_result) TYPE REF TO zif_crm_entity .

  METHODS read_entity
    IMPORTING
      i_entity_id     TYPE crmt_object_guid
      i_entity_type   TYPE csequence
    RETURNING
      VALUE(r_result) TYPE REF TO zif_crm_entity .

  METHODS entity_exists
    IMPORTING
      i_entity_id     TYPE crmt_object_guid
      i_entity_type   TYPE csequence
    RETURNING
      VALUE(r_result) TYPE crmt_boolean.

  METHODS save.

ENDINTERFACE.
