CLASS zcl_crm_entity_mgr DEFINITION
  PUBLIC

  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_crm_entity_mgr.


    METHODS constructor
      IMPORTING
        i_entity_factory TYPE REF TO zcl_crm_api_entity_factory
        i_access_method  TYPE zcrm_api_access_method.

  PROTECTED SECTION.

    TYPES : BEGIN OF ty_instance,
              obj_key TYPE crmt_object_guid,
              obj_ref TYPE REF TO zif_crm_entity,
            END OF ty_instance,

            ty_instance_tab TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY obj_key.


    DATA instance_tab   TYPE ty_instance_tab .
    DATA entity_factory TYPE REF TO zcl_crm_api_entity_factory.
    DATA access_method  TYPE zcrm_api_access_method.

    METHODS add_entity2cache
      IMPORTING
        !i_entity TYPE REF TO zif_crm_entity .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRM_ENTITY_MGR IMPLEMENTATION.


  METHOD add_entity2cache.

    "--
    IF i_entity IS NOT BOUND. RETURN. ENDIF.

    INSERT VALUE #(
      obj_key = i_entity->get_entity_id( )
      obj_ref = i_entity
    ) INTO TABLE me->instance_tab.


  ENDMETHOD.


  METHOD constructor.


    me->entity_factory = i_entity_factory.
    me->access_method = i_access_method.

  ENDMETHOD.


  METHOD zif_crm_entity_mgr~create_entity.

    r_result = entity_factory->get_crm_entity( i_entity_type ).

    r_result->create( i_create_attr ).

*-- Add entity to cached instance( Entity Map  )
    me->add_entity2cache( r_result  ).

    me->entity_factory->get_transaction(  )->add(  r_result->get_transaction_context(  )  ).

  ENDMETHOD.


  METHOD zif_crm_entity_mgr~delete_entity.

  ENDMETHOD.


  METHOD zif_crm_entity_mgr~entity_exists.

  ENDMETHOD.


  METHOD zif_crm_entity_mgr~read_entity.


    TRY.

*-- Read entity from cache
        r_result =  me->instance_tab[ obj_key = i_entity_id ]-obj_ref.

      CATCH cx_sy_itab_line_not_found.

        r_result = entity_factory->get_crm_entity( i_entity_type ).

        r_result->load_as( i_entity_id ).

        me->add_entity2cache( r_result ).

* me->entity_factory->get_transaction(  )->add(  r_result->get_transaction_context(  )  ).


        "r_result = me->read_entity(  i_entity_id = i_entity_id ).

    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_entity_mgr~save.

  ENDMETHOD.
ENDCLASS.
