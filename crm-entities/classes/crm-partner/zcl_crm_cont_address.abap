CLASS zcl_crm_cont_address DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_crm_cont_address .

    METHODS constructor
      IMPORTING
        !i_contact_person TYPE REF TO zif_crm_contact_relation .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA contact_person TYPE REF TO zcl_crm_contact_relation .
ENDCLASS.



CLASS ZCL_CRM_CONT_ADDRESS IMPLEMENTATION.


  METHOD constructor.
    me->contact_person ?= i_contact_person.
  ENDMETHOD.


  METHOD zif_crm_cont_address~get_main_address_data.

  ENDMETHOD.


  METHOD zif_crm_cont_address~get_standard_phone.

    DATA: lr_standard_address_entity TYPE REF TO cl_crm_bol_entity.
    DATA: lr_standard_phone_entity TYPE REF TO cl_crm_bol_entity.

    DATA(contact_person) = me->contact_person->get_contact_entity( ).

    DATA(lr_address_iter) = contact_person->get_related_entities( iv_relation_name = 'BuilContactPersonAddressRel' )->get_iterator( ).
    lr_address_iter->filter_by_property( iv_attr_name = 'STANDARDADDRESS' iv_value = 'X' ).

    lr_standard_address_entity = lr_address_iter->get_first( ).

    DATA(lr_phone_iter) =  lr_standard_address_entity->get_related_entities( iv_relation_name = 'BuilContactPersonAddrTelRel' )->get_iterator( ).
    lr_phone_iter->filter_by_property( iv_attr_name = 'STD_NO' iv_value = 'X' ).

    lr_standard_phone_entity = lr_phone_iter->get_first( ).
  ENDMETHOD.


  METHOD zif_crm_cont_address~set_standard_phone.
    DATA(v_contact_person) = contact_person->get_contact_entity( ).

    DATA(v_address_iter) = v_contact_person->get_related_entities( iv_relation_name = 'BuilContactPersonAddressRel' )->get_iterator( ).

    v_address_iter->filter_by_property( iv_attr_name = 'STANDARDADDRESS'
                                        iv_value     = 'X' ).

    DATA(v_standard_address_entity) = CAST cl_crm_bol_entity( v_address_iter->get_first( ) ).

    v_standard_address_entity->switch_to_change_mode( ).

    v_standard_address_entity->set_property( iv_attr_name = 'COUNTRYTEL'
                                             iv_value     = i_country ).
    v_standard_address_entity->set_property( iv_attr_name = 'STD_NOTEL '
                                             iv_value     = abap_true ).
    v_standard_address_entity->set_property( iv_attr_name = 'TELEPHONETEL'
                                             iv_value     = i_telephone ).
    v_standard_address_entity->set_property( iv_attr_name = 'EXTENSIONTEL'
                                             iv_value     = i_extension ).

    cl_crm_bol_core=>get_instance( )->modify( ).

    IF i_save = abap_true.
      DATA(v_transaction) = v_standard_address_entity->get_transaction( ).

      IF v_transaction->save( ) = abap_false.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_crm_cont_address~set_standard_function.
    DATA(v_contact_person) = contact_person->get_contact_entity( ).

    DATA(v_address_iter) = v_contact_person->get_related_entities( iv_relation_name = 'BuilContactPersonAddressRel' )->get_iterator( ).

    v_address_iter->filter_by_property( iv_attr_name = 'STANDARDADDRESS'
                                        iv_value     = 'X' ).

    DATA(v_standard_address_entity) = CAST cl_crm_bol_entity( v_address_iter->get_first( ) ).

    v_standard_address_entity->switch_to_change_mode( ).

    v_standard_address_entity->set_property( iv_attr_name = 'FUNCTION'
                                             iv_value     = i_function ).

    cl_crm_bol_core=>get_instance( )->modify( ).

    IF i_save = abap_true.
      DATA(v_transaction) = v_standard_address_entity->get_transaction( ).

      IF v_transaction->save( ) = abap_false.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
