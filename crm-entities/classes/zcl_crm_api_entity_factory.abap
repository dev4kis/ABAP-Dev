CLASS zcl_crm_api_entity_factory DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !i_access_method TYPE zcrm_api_access_method
        !i_transaction   TYPE REF TO zif_crm_transaction.


    METHODS get_transaction
      RETURNING
        VALUE(r_transaction) TYPE REF TO zcl_crm_base_transaction .

    METHODS get_order_entity
      RETURNING
        VALUE(r_result) TYPE REF TO zif_crm_order .
    METHODS get_partner_entity
      RETURNING
        VALUE(r_result) TYPE REF TO zif_crm_partner .
    METHODS get_product_entity
      RETURNING
        VALUE(r_result) TYPE REF TO zif_crm_product .

    METHODS get_document_entity
      RETURNING
        VALUE(r_result) TYPE REF TO zif_crm_order_attachm .


    METHODS get_partner_manager
      RETURNING
        VALUE(r_result) TYPE REF TO zif_crm_entity_mgr .
    METHODS get_crm_entity
      IMPORTING
        i_entity_name   TYPE csequence
      RETURNING
        VALUE(r_result) TYPE REF TO zif_crm_entity.
    METHODS get_entity_manager
      RETURNING
        VALUE(r_result) TYPE REF TO zif_crm_entity_mgr.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA access_method TYPE zcrm_api_access_method.
    DATA entity_manager TYPE REF TO zif_crm_entity_mgr.
    DATA transaction TYPE REF TO zcl_crm_base_transaction.
ENDCLASS.



CLASS zcl_crm_api_entity_factory IMPLEMENTATION.


  METHOD constructor.
    me->access_method = i_access_method.
    me->transaction ?= i_transaction.
  ENDMETHOD.


  METHOD get_crm_entity.

    CASE i_entity_name.
      WHEN zif_crm_entity_mgr=>ty_entity_type-order.
        r_result = me->get_order_entity( ).
      WHEN zif_crm_entity_mgr=>ty_entity_type-partner.
        r_result = me->get_partner_entity( ).
      WHEN zif_crm_entity_mgr=>ty_entity_type-product.
        r_result = me->get_product_entity( ).
      WHEN zif_crm_entity_mgr=>ty_entity_type-document.
        r_result = me->get_document_entity( ).
      WHEN OTHERS.

    ENDCASE.


  ENDMETHOD.


  METHOD get_entity_manager.

    IF me->entity_manager IS INITIAL.
      me->entity_manager = NEW zcl_crm_entity_mgr( i_entity_factory =  me
                                                   i_access_method = me->access_method
                                                  ).
    ENDIF.

    r_result = me->entity_manager.

  ENDMETHOD.


  METHOD get_order_entity.
*-- TODO add entities to FUBA scenario

    CASE me->access_method.
      WHEN 1.
        r_result = NEW zcl_crm_order_bol( ).
      WHEN 2.
        r_result = NEW zcl_crm_order_fuba( ).
      WHEN 3.

      WHEN 4.
        r_result = NEW zcl_crm_order_json( ).

    ENDCASE.

  ENDMETHOD.


  METHOD get_partner_entity.
*-- TODO add Partner wrapper for FUBA and JSON scenario
    CASE me->access_method.
      WHEN 1.
        r_result = NEW zcl_crm_partner_bol( ).
    ENDCASE.

  ENDMETHOD.


  METHOD get_partner_manager.
    " r_result = NEW zcl_crm_partner_entity_mgr( i_entity_factory = me i_access_method = me->access_method  ).
  ENDMETHOD.


  METHOD get_product_entity.
*-- TODO add Partner wrapper for FUBA and JSON scenario
    CASE me->access_method.
      WHEN 1.
        r_result = NEW zcl_crm_product_bol( ).
    ENDCASE.

  ENDMETHOD.


  METHOD get_transaction.
    r_transaction = me->transaction.
  ENDMETHOD.

  METHOD get_document_entity.
    CASE me->access_method.
      WHEN 1.
        r_result = NEW zcl_crm_order_attachm_bol( ).

      WHEN 2.

        r_result = NEW zcl_crm_order_attachm_fuba( ).

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
