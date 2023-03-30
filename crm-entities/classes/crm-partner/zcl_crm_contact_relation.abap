class ZCL_CRM_CONTACT_RELATION definition
  public
  final
  create public .

public section.

  interfaces ZIF_CRM_CONTACT_RELATION .

  methods CONSTRUCTOR
    importing
      !I_FROM_PARTNER type ref to ZIF_CRM_PARTNER
      !I_TO_PARTNER type BU_PARTNER
      !I_ADDRESS type ref to ZIF_CRM_ADDRESS
      !I_CONTACT_PERSON_ENTITY type ref to CL_CRM_BOL_ENTITY optional .
  methods GET_CONTACT_ENTITY
    returning
      value(R_CONTACT_ENTITY) type ref to CL_CRM_BOL_ENTITY .
  PROTECTED SECTION.

private section.

  data CONTACT_ENTITY type ref to CL_CRM_BOL_ENTITY .
ENDCLASS.



CLASS ZCL_CRM_CONTACT_RELATION IMPLEMENTATION.


  METHOD constructor.

    me->zif_crm_contact_relation~from_partner   = i_from_partner.
    me->zif_crm_contact_relation~to_partner     = i_to_partner.
    me->zif_crm_contact_relation~address        = i_address.
    me->CONTACT_ENTITY                          = I_CONTACT_PERSON_ENTITY.

  ENDMETHOD.


  method GET_CONTACT_ENTITY.
    r_contact_entity = me->contact_entity.
  endmethod.


  method ZIF_CRM_CONTACT_RELATION~GET_STANDARD_ADDRESS.

    r_address ?= new zcl_crm_cont_address( me ).

  endmethod.
ENDCLASS.
