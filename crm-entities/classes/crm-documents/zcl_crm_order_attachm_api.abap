CLASS zcl_crm_order_attachm_api DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_crm_entity_api.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        i_entity_factory TYPE REF TO zcl_crm_api_entity_factory.


    METHODS get_document
      IMPORTING
        i_key           TYPE any
      RETURNING
        VALUE(r_result) TYPE REF TO zif_crm_order_attachm .

    METHODS create_document
      IMPORTING i_instid        TYPE sibfboriid
                i_typeid        TYPE sibftypeid
                i_catid         TYPE sibfcatid
                i_name          TYPE string OPTIONAL
                i_description   TYPE string OPTIONAL
      RETURNING VALUE(r_result) TYPE REF TO zif_crm_order_attachm.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA manager TYPE REF TO zif_crm_entity_mgr.
    DATA entity_factory TYPE REF TO zcl_crm_api_entity_factory.

ENDCLASS.


CLASS zcl_crm_order_attachm_api IMPLEMENTATION.

  METHOD constructor.

    me->entity_factory = i_entity_factory.
    me->manager  = i_entity_factory->get_entity_manager( ).

  ENDMETHOD.

  METHOD get_document.
*    DATA(product_guid) = me->get_product_guid( i_key = i_key ).
    DATA(v_crm_entity) = me->manager->read_entity(    i_entity_type = zif_crm_entity_mgr=>ty_entity_type-document
                                                      i_entity_id   = i_key  ).

    r_result = CAST #( v_crm_entity ).
  ENDMETHOD.




  METHOD create_document.
*-- TODO  avoid using BOL structure
    DATA v_create_attr TYPE crmt_cmic_rfolder_attr.

    v_create_attr = VALUE #( instid         = i_instid
                             typeid         =  i_typeid
                             catid          =    i_catid
                             name           =    i_name
                             description    = i_description ).


    DATA(v_crm_entity) = me->manager->create_entity(
      EXPORTING
        i_create_attr = v_create_attr
        i_entity_type = zif_crm_entity_mgr=>ty_entity_type-document
    ).
    r_result      = CAST #( v_crm_entity ).
  ENDMETHOD.

ENDCLASS.





