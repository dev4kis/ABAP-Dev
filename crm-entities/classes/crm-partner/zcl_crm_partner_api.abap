CLASS zcl_crm_partner_api DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_crm_entity_api .


  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        i_entity_factory TYPE REF TO zcl_crm_api_entity_factory.


    "! Read SAP CRM Business Partner using provided <strong>Partner_Id</strong>.
    "! You may use Partner Number or Partner GUID as input.
    METHODS read
      IMPORTING
                i_partner_id    TYPE data
      RETURNING VALUE(r_result) TYPE REF TO zif_crm_partner.
    METHODS create_person
      IMPORTING
                i_create_attr   TYPE zif_crm_partner=>ty_create_person_attr
      RETURNING VALUE(r_result) TYPE REF TO zif_crm_partner.


    METHODS create_organization
      IMPORTING
                i_create_attr   TYPE zif_crm_partner=>ty_create_organization_attr
      RETURNING VALUE(r_result) TYPE REF TO zif_crm_partner  .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA partner_manager TYPE REF TO zif_crm_entity_mgr .
    DATA entity_factory TYPE REF TO zcl_crm_api_entity_factory .
ENDCLASS.



CLASS zcl_crm_partner_api IMPLEMENTATION.


  METHOD constructor.

    me->entity_factory  = i_entity_factory.
    me->partner_manager = i_entity_factory->get_entity_manager( ).

  ENDMETHOD.


  METHOD read.

    DATA: v_entity_guid TYPE crmt_object_guid.

    TRY.

*-- Convert BP Number to BP GUID if required

        DATA(v_element) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( i_partner_id ) ) .

        IF  v_element->applies_to_data( v_entity_guid ).
          v_entity_guid = i_partner_id.
        ELSE.
          SELECT SINGLE partner_guid
              FROM  but000 INTO v_entity_guid
                WHERE  partner  EQ i_partner_id.

        ENDIF.

*-- User Partner manager to retrieve the Business Entity
        DATA(v_entity) = me->partner_manager->read_entity(  i_entity_type = zif_crm_entity_mgr=>ty_entity_type-partner
                                                            i_entity_id   = v_entity_guid ).

        r_result = CAST zif_crm_partner( v_entity ).

      CATCH zcx_crm_entity_api INTO DATA(v_api).

        RAISE EXCEPTION v_api.

    ENDTRY.

  ENDMETHOD.

  METHOD create_person.

    DATA v_create_attr TYPE crmst_bp_create_buil.

    MOVE-CORRESPONDING i_create_attr TO v_create_attr.

    v_create_attr-bp_category = 1. "Person
    v_create_attr-bp_group    = space.

    TRY.

        me->partner_manager->create_entity(
           i_entity_type   =  zif_crm_entity_mgr=>ty_entity_type-partner
           i_create_attr   = v_create_attr

        ).

      CATCH cx_root INTO DATA(v_err).
    ENDTRY.

  ENDMETHOD.

  METHOD create_organization.

    DATA v_create_attr TYPE crmst_bp_create_buil.

    MOVE-CORRESPONDING i_create_attr TO v_create_attr.

    v_create_attr-bp_category = 2. "Organization

    v_create_attr-bp_group    = space.

    TRY.

        me->partner_manager->create_entity(
           i_entity_type   =  zif_crm_entity_mgr=>ty_entity_type-partner
           i_create_attr   = v_create_attr

        ).

      CATCH cx_root INTO DATA(v_err).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
