CLASS zcl_crm_partner_bol DEFINITION
  PUBLIC
  INHERITING FROM zcl_crm_base_partner
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_crm_api_entity_factory .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .

    METHODS zif_crm_entity~get_transaction_context
        REDEFINITION .
    METHODS zif_crm_partner~create_extension
        REDEFINITION .
    METHODS zif_crm_partner~delete_extension_record
        REDEFINITION .
    METHODS zif_crm_partner~get_contact_relation
        REDEFINITION .
    METHODS zif_crm_partner~get_extension_table
        REDEFINITION .
    METHODS zif_crm_partner~get_general_data
        REDEFINITION .
    METHODS zif_crm_partner~get_relations_list
        REDEFINITION .
    METHODS zif_crm_partner~get_relation_by_category
        REDEFINITION .
    METHODS zif_crm_partner~get_standard_address
        REDEFINITION .
    METHODS zif_crm_partner~update_extension_record
        REDEFINITION .
    METHODS zif_crm_partner~update_id_numbers
        REDEFINITION .
    METHODS zif_crm_partner~update_std_address
        REDEFINITION .
    METHODS zif_crm_partner~update_header_data
        REDEFINITION .
  PROTECTED SECTION.

    METHODS create_partner
        REDEFINITION .
    METHODS load_partner
        REDEFINITION .
  PRIVATE SECTION.

    CLASS-DATA bol_core TYPE REF TO cl_crm_bol_core .
    DATA entity_root TYPE REF TO cl_crm_bol_entity .
*          bp type REF TO cl_crm_bol_entity,
    METHODS parse_relations_result
      IMPORTING
        i_entity_collection TYPE REF TO if_bol_entity_col
      RETURNING
        VALUE(r_result)     TYPE bapibus1006_relationstab.

    METHODS get_extension_structure
      IMPORTING
        i_extension_name TYPE csequence
      RETURNING
        VALUE(r_result)  TYPE string.
    METHODS switch_to_change_mode.
    CONSTANTS c_btx_bol_component TYPE crmt_genil_appl VALUE 'BP_APPL' ##NO_TEXT.
    CONSTANTS c_bol_root_element  TYPE crmt_ext_obj_name VALUE 'BuilHeader' ##NO_TEXT.

ENDCLASS.



CLASS ZCL_CRM_PARTNER_BOL IMPLEMENTATION.


  METHOD class_constructor.


    zcl_crm_partner_bol=>bol_core = cl_crm_bol_core=>get_instance( ).

    IF zcl_crm_partner_bol=>bol_core->is_started EQ abap_false.
      zcl_crm_partner_bol=>bol_core->start_up( iv_appl_name = zcl_crm_partner_bol=>c_btx_bol_component iv_display_mode_support = abap_true ).
    ENDIF.

    "zcl_crm_partner_bol=>bol_core->load_component( CONV crmt_component_name( zcl_crm_partner_bol=>c_btx_bol_component ) ).
    zcl_crm_partner_bol=>bol_core->load_component_set( zcl_crm_partner_bol=>c_btx_bol_component ).

  ENDMETHOD.


  METHOD create_partner.
    DATA v_create_params_tab TYPE crmt_name_value_pair_tab.

    DATA(v_entity_factory) = zcl_crm_partner_bol=>bol_core->get_entity_factory( c_bol_root_element  ).

    v_create_params_tab = VALUE #( ( name = 'BP_CATEGORY' value = i_create_attr-bp_category ) ( name = 'BP_GROUP'    value = i_create_attr-bp_group  ) ).

    me->entity_root = v_entity_factory->create( v_create_params_tab ).

  ENDMETHOD.


  METHOD get_extension_structure.

  ENDMETHOD.


  METHOD load_partner.

    r_found = abap_false.

    TRY.

        me->entity_root =  zcl_crm_partner_bol=>bol_core->get_root_entity(
                                                      iv_object_name = zcl_crm_partner_bol=>c_bol_root_element
                                                      iv_object_guid = i_entity_id
                                                   ).
        IF me->entity_root IS BOUND.
          r_found = abap_true.
        ENDIF.

      CATCH  cx_crm_genil_model_error INTO DATA(v_genil_err).
        zcx_crm_entity_api=>raise_internal_error(  i_text = v_genil_err->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD parse_relations_result.

    DATA: v_relation TYPE crmt_bupa_il_relation.

    IF i_entity_collection IS INITIAL. RETURN. ENDIF.

    DATA(v_entity) = i_entity_collection->get_first( ).

    DO.

      IF v_entity  IS NOT BOUND. EXIT. ENDIF.

      v_entity->get_properties( IMPORTING es_attributes  = v_relation ).

      APPEND VALUE #(
        partner1               = v_relation-partner1
        partner2               = v_relation-partner2
        validfromdate          = v_relation-validfromdate
        validuntildate         = v_relation-validuntildate
        relationshipcategory   = v_relation-relationshipcategory
        relationshiptype       = v_relation-relationshiptype
        defaultrelationship    = v_relation-defaultrelationship

      ) TO r_result.

      v_entity = i_entity_collection->get_next( ).

    ENDDO.

  ENDMETHOD.


  METHOD switch_to_change_mode.

    IF  me->entity_root->is_locked( ) = abap_false.

      IF me->entity_root->switch_to_change_mode( ) <> abap_true.
        zcx_crm_app=>raise_foreign_lock( i_text = '' ).
      ENDIF.

      me->entity_root->lock( ).

      IF me->entity_root->is_locked( ) <> 'X'.
        zcx_crm_app=>raise_foreign_lock( i_text = '' ).
      ENDIF.

      IF me->entity_root->is_changeable( ) <> 'X'.
        zcx_crm_app=>raise_foreign_lock( i_text = '' ).
      ENDIF.


      DATA v_transaction TYPE REF TO    zcl_crm_base_transaction.
      DATA v_context TYPE zcrm_transaction_data.

      TRY.
          v_transaction ?= zcl_crm_transaction_bol=>get_instance(  ).
          v_context-object = me->entity_root->get_name(  ).
          v_context-objectkey = me->zif_crm_entity~get_entity_id(  ).
          v_transaction->add( v_context ).
        CATCH cx_root.
          zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
      ENDTRY.


    ENDIF.


  ENDMETHOD.


  METHOD zif_crm_entity~get_transaction_context.
    r_result-object = me->entity_root->get_name(  ).
    r_result-objectkey = me->zif_crm_entity~get_entity_id(  ).
  ENDMETHOD.


  METHOD zif_crm_partner~create_extension.

    DATA v_struct TYPE REF TO data.
    FIELD-SYMBOLS <fs_properties> TYPE any.

    cl_crm_bupa_axt=>initialize( ).
    cl_crm_bupa_axt=>get_extension_tables( IMPORTING    et_objects_il = DATA(v_object_tab)
                                                        et_model_il =   DATA(v_modeltab)

                                          ).
    TRY.

        DATA(v_object_name) = v_modeltab[  relation_name = i_extension_name ]-object_b.

        DATA(v_extension_struct) = v_object_tab[ object_name = v_object_name  ]-attr_struct.

      CATCH cx_sy_itab_line_not_found.
        zcx_crm_entity_api=>raise_not_customized( |Extension Table { i_extension_name }| ).
    ENDTRY.

    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_exception).
        RAISE EXCEPTION v_lock_exception.
    ENDTRY.

    CREATE DATA v_struct TYPE (v_extension_struct).

    ASSIGN v_struct->* TO <fs_properties>.
    <fs_properties> = CORRESPONDING #( i_values ).

    TRY.
        IF me->entity_root->is_locked( ) = abap_true.

          DATA(v_created_entity) =   me->entity_root->create_related_entity(
               EXPORTING
                 iv_relation_name           =     CONV #( i_extension_name ) ).

          v_created_entity->set_properties( is_attributes = <fs_properties> ).
          cl_crm_bol_core=>get_instance( )->modify( ).

*---ERROR Handling-----------------------------------------------------------------*
          DATA(v_message_container) = v_created_entity->get_message_container(  ).
          DATA v_err_messages TYPE crmt_genil_message_tab.
          v_message_container->get_messages( EXPORTING iv_message_type = 'E' IMPORTING et_messages = v_err_messages ).
          IF line_exists( v_err_messages[ 1 ] ).
            DATA(v_message) = v_err_messages[ 1 ].
            zcx_crm_entity_api=>raise_inconsistent( i_text = |{ v_message-message }| ).
          ENDIF.

*---ERROR Handling-----------------------------------------------------------------*


          CALL METHOD cl_crm_genil_container_tools=>get_key_from_object_id
            EXPORTING
              iv_object_name = v_created_entity->get_name( )
              iv_object_id   = v_created_entity->get_key( )
            IMPORTING
              es_key         = r_key_struc.



        ENDIF.
      CATCH cx_crm_genil_duplicate_rel.    "
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
      CATCH cx_crm_genil_model_error.    "
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_partner~delete_extension_record.
*CALL METHOD SUPER->ZIF_CRM_PARTNER~DELETE_ENTENSION_RECORD
*  EXPORTING
*    I_EXTENSION   =
*    I_RECORD_GUID =
*    .

    DATA v_struct TYPE REF TO data.
    DATA v_struct1 TYPE REF TO data.
    FIELD-SYMBOLS <fs_properties> TYPE any.
    FIELD-SYMBOLS <fs_properties_old> TYPE any.

    CASE i_extension.
      WHEN zif_const_btx_exas=>co_cv_components-education.
        DATA(v_extension_name) = |ZAEXT_BOL_RELAT000064|.

      WHEN zif_const_btx_exas=>co_cv_components-employement.
        v_extension_name = |ZAEXT_BOL_RELAT000068|.

      WHEN zif_const_btx_exas=>co_cv_components-others.
        v_extension_name = |ZAEXT_BOL_RELAT00006B|.

      WHEN OTHERS.
    ENDCASE.
    DATA(v_bpath) = |./{ v_extension_name }/*$|.
*    DATA(v_bpath) = |./{ v_extension_name }[@RECORD_ID="{ i_record_guid }"]/*$|.


    cl_crm_bupa_axt=>initialize( ).
    cl_crm_bupa_axt=>get_extension_tables( IMPORTING    et_objects_il = DATA(v_object_tab)
                                                        et_model_il =   DATA(v_modeltab)

                                          ).
    TRY.

        DATA(v_object_name) = v_modeltab[  relation_name = v_extension_name ]-object_b.

        DATA(v_extension_struct) = v_object_tab[ object_name = v_object_name  ]-attr_struct.

      CATCH cx_sy_itab_line_not_found.
        zcx_crm_entity_api=>raise_not_customized( |Extension Table { v_extension_name }| ).
    ENDTRY.
    CREATE DATA v_struct TYPE (v_extension_struct).
    CREATE DATA v_struct1 TYPE (v_extension_struct).

    ASSIGN v_struct->* TO <fs_properties>.

    ASSIGN v_struct1->* TO <fs_properties_old>.
    <fs_properties_old> = i_old_data.


*    zcl_api_util=>get_and_create_entity_by_bpath(
*      EXPORTING
*        i_entity = me->entity_root
*        i_bpath  = v_bpath
*        i_create = abap_false
*      RECEIVING
*        r_entity = DATA(v_record)
*    ).
    DATA(v_extension_col) = me->entity_root->get_related_entities_by_bpath(
      EXPORTING
        iv_bpath_statement = v_bpath ).

    DATA(v_col_itr) = v_extension_col->get_iterator( ).

    DATA(v_record) = v_col_itr->get_first( ).

    WHILE v_record IS BOUND.
      v_record->get_properties(
        IMPORTING
          es_attributes = <fs_properties>
      ).
      IF <fs_properties> = <fs_properties_old>.
        DATA(v_record_2_delete) = v_record. " Bearing in mind that no records will have the same values: this will work in Prod, but what if....???
        EXIT.

      ENDIF.
      v_record = v_col_itr->get_next( ).
    ENDWHILE.

    IF v_record_2_delete IS BOUND.


      TRY.
          me->switch_to_change_mode( ).
        CATCH zcx_crm_app INTO DATA(v_lock_exception).
          RAISE EXCEPTION v_lock_exception.
      ENDTRY.

      CASE i_del.
        WHEN abap_true.
*      v_record_2_delete->delete( ).
*       v_extension_col->remove( iv_entity = v_record_2_delete ).
        WHEN abap_false.
          v_record_2_delete->set_properties( is_attributes = i_data  ).

        WHEN OTHERS.
      ENDCASE.
      cl_crm_bol_core=>get_instance( )->modify( ).
      RETURN.
    ENDIF.

    zcx_crm_entity_api=>raise_existing(
        i_text = |{ i_record_guid }|
    ).

  ENDMETHOD.


  METHOD zif_crm_partner~get_contact_relation.


*-- Use BOL core to load a contact person relation

    DATA v_contact_attr        TYPE crmst_contact_person_obj_buil.
    DATA v_address_info        TYPE bapibus1006_address.
    DATA v_contact_key         TYPE crmst_header_conp_obj_buil.


    DATA : v_instance   TYPE crmt_genil_obj_instance,
           lt_instances TYPE crmt_genil_obj_instance_tab.


    v_contact_key-bp_number     = me->zif_crm_partner~get_number( ).
    v_contact_key-conp_number   = i_with_partner.

    v_instance-object_name     = cl_crm_buil=>contact_person.
    v_instance-object_id       = cl_crm_genil_container_tools=>build_object_id( v_contact_key  ).

    APPEND v_instance TO lt_instances.

    DATA(lv_coll) =  zcl_crm_partner_bol=>bol_core->get_access_entities( it_instances  = lt_instances ).
    DATA(v_contact_person) = lv_coll->get_first( ).

    IF v_contact_person IS INITIAL.
      zcx_crm_entity_api=>raise_not_qualified( ).
    ENDIF.

    v_contact_person->get_properties( IMPORTING es_attributes = v_contact_attr ).

    DATA(v_address) = NEW zcl_crm_address( v_address_info  ).

    r_result = NEW zcl_crm_contact_relation(  i_from_partner          = me
                                              i_to_partner            = i_with_partner
                                              i_address               = v_address
                                              i_contact_person_entity = v_contact_person
                                            ).

  ENDMETHOD.


  METHOD zif_crm_partner~get_extension_table.
    DATA v_struct TYPE REF TO data.
    FIELD-SYMBOLS <fs_properties> TYPE any.

    cl_crm_bupa_axt=>initialize( ).
    cl_crm_bupa_axt=>get_extension_tables( IMPORTING    et_objects_il = DATA(v_object_tab)
                                                        et_model_il =   DATA(v_modeltab)
                                          ).
    TRY.

        DATA(v_object_name) = v_modeltab[  relation_name = i_extension_name ]-object_b.

        DATA(v_extension_struct) = v_object_tab[ object_name = v_object_name  ]-attr_struct.

      CATCH cx_sy_itab_line_not_found.
        zcx_crm_entity_api=>raise_not_customized( |Extension Table { i_extension_name }| ).
    ENDTRY.

    CREATE DATA v_struct TYPE (v_extension_struct).

    ASSIGN v_struct->* TO <fs_properties>.


    DATA(v_related_entities) = me->entity_root->get_related_entities( iv_relation_name = CONV #( i_extension_name )  ).
    DATA(v_iterator)         = v_related_entities->get_iterator( ).
    DATA(v_entity)           = v_iterator->get_first( ).

    DO.

      IF v_entity IS INITIAL. EXIT. ENDIF.


      IF i_record_guid IS NOT INITIAL.

*      data(v_key) = v_entity->get_key(  ).
        DATA v_key_struc TYPE axts_bol_key_struc.

        CALL METHOD cl_crm_genil_container_tools=>get_key_from_object_id
          EXPORTING
            iv_object_name = v_entity->get_name( )
            iv_object_id   = v_entity->get_key( )
          IMPORTING
            es_key         = v_key_struc.



        IF v_key_struc-record_id <> i_record_guid.
          v_entity =  v_iterator->get_next( ).
          CONTINUE.
        ENDIF.
      ENDIF.
      v_entity->get_properties( IMPORTING es_attributes = <fs_properties> ).

      INSERT <fs_properties> INTO TABLE e_extension.

      v_entity =  v_iterator->get_next( ).

    ENDDO.


  ENDMETHOD.


  METHOD zif_crm_partner~get_general_data.

    me->entity_root->get_properties( IMPORTING es_attributes = r_result ).

  ENDMETHOD.


  METHOD zif_crm_partner~get_relations_list.


    DATA(v_related_entity_list) = me->entity_root->get_related_entities( iv_relation_name = 'BuilRelationshipRel' iv_owned_only = abap_true ).

    r_result = parse_relations_result( v_related_entity_list ).

  ENDMETHOD.


  METHOD zif_crm_partner~get_relation_by_category.


    DATA(v_related_entity_list) = me->entity_root->get_related_entities_by_bpath(    iv_bpath_statement  = |./BuilRelationshipRel[@RELATIONSHIPCATEGORY="{ i_category }"]/*$| ).

    r_result = parse_relations_result(  v_related_entity_list ).

  ENDMETHOD.


  METHOD zif_crm_partner~get_standard_address.

    DATA v_address_data TYPE crmt_bupa_il_address.
    DATA(v_address_bpath) = |./BuilAddressRel[@STANDARDADDRESS="X"]|.
    DATA(v_emal_bpath) = |./BuilAddressRel[@STANDARDADDRESS="X"]/BuilAddressEMailRel/@E_MAIL|.

    DATA(v_std_addrses) =  me->entity_root->get_related_entities_by_bpath(
       EXPORTING
         iv_bpath_statement = v_address_bpath
     )->get_first( ).

    v_std_addrses->get_properties(
      IMPORTING
        es_attributes = v_address_data

    ).
**********************************************************************
* changed, because make no sense...not sure who added the email to this method, don't like it, change later ( if I have time :-)
* refactoring necessary, but for now, i will bring it to work
**********************************************************************

    DATA(v_smptadr) =  me->entity_root->get_properties_by_bpath(  iv_bpath_statement = v_emal_bpath ).
    IF v_smptadr IS BOUND.
      ASSIGN v_smptadr->* TO FIELD-SYMBOL(<smtp>).
      IF <smtp> IS ASSIGNED.
        e_email = <smtp>.
      ENDIF.
    ENDIF.
    DATA(v_crm_address) = NEW zcl_crm_address( i_general_data = CORRESPONDING #( v_address_data ) ).

    r_crm_address = v_crm_address.

  ENDMETHOD.


  METHOD zif_crm_partner~update_extension_record.


    DATA v_struct TYPE REF TO data.
    DATA v_key_struc TYPE axts_bol_key_struc.
    FIELD-SYMBOLS <fs_properties> TYPE any.
    FIELD-SYMBOLS <fs_properties_old> TYPE any.

*    CASE i_extension.
*      WHEN zif_const_btx_exas=>co_cv_components-education.
*        DATA(v_extension_name) = |ZAEXT_BOL_RELAT000064|.
*
*      WHEN zif_const_btx_exas=>co_cv_components-employement.
*        v_extension_name = |ZAEXT_BOL_RELAT000068|.
*
*      WHEN zif_const_btx_exas=>co_cv_components-others.
*        v_extension_name = |ZAEXT_BOL_RELAT00006B|.
*
*      WHEN OTHERS.
    DATA(v_extension_name) = i_extension.
*    ENDCASE.
    DATA(v_bpath) = |./{ v_extension_name }/*$|.



    cl_crm_bupa_axt=>initialize( ).
    cl_crm_bupa_axt=>get_extension_tables( IMPORTING    et_objects_il = DATA(v_object_tab)
                                                        et_model_il =   DATA(v_modeltab)

                                          ).
    TRY.

        DATA(v_object_name) = v_modeltab[  relation_name = v_extension_name ]-object_b.

        DATA(v_extension_struct) = v_object_tab[ object_name = v_object_name  ]-attr_struct.

      CATCH cx_sy_itab_line_not_found.
        zcx_crm_entity_api=>raise_not_customized( |Extension Table { v_extension_name }| ).
    ENDTRY.
    CREATE DATA v_struct TYPE (v_extension_struct).
*    CREATE DATA v_struct1 TYPE (v_extension_struct).

    ASSIGN v_struct->* TO <fs_properties>.


    DATA(v_extension_col) = me->entity_root->get_related_entities_by_bpath(
      EXPORTING
        iv_bpath_statement = v_bpath ).

    DATA(v_col_itr) = v_extension_col->get_iterator( ).

    DATA(v_record) = v_col_itr->get_first( ).

    WHILE v_record IS BOUND.

      CALL METHOD cl_crm_genil_container_tools=>get_key_from_object_id
        EXPORTING
          iv_object_name = v_record->get_name( )
          iv_object_id   = v_record->get_key( )
        IMPORTING
          es_key         = v_key_struc.


      IF v_key_struc-record_id = i_record_guid.
        TRY.
            me->switch_to_change_mode( ).
          CATCH zcx_crm_app INTO DATA(v_lock_exception).
            RAISE EXCEPTION v_lock_exception.
        ENDTRY.
        v_record->set_properties( is_attributes = i_data  ).
        cl_crm_bol_core=>get_instance( )->modify( ).
        RETURN.
      ENDIF.
      v_record = v_col_itr->get_next( ).
    ENDWHILE.





    zcx_crm_entity_api=>raise_not_found(
        i_text = |{ i_record_guid }|
    ).

  ENDMETHOD.


  METHOD zif_crm_partner~update_header_data.
*CALL METHOD SUPER->ZIF_CRM_PARTNER~UPDATE_HEADER_DATA
*  EXPORTING
*    I_DATA =
*    .
    DATA v_partner_header_attr TYPE crmst_header_object_buil.
    me->entity_root->get_properties(
      IMPORTING
        es_attributes = v_partner_header_attr
    ).

    v_partner_header_attr-birthdate = i_data-birthdate.
    v_partner_header_attr-birthplace = i_data-birthplace.
    v_partner_header_attr-nationality = i_data-nationality.

    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_exception).
        RAISE EXCEPTION v_lock_exception.
    ENDTRY.

    me->entity_root->set_properties( is_attributes = v_partner_header_attr ).

    cl_crm_bol_core=>get_instance( )->modify( ).

  ENDMETHOD.


  METHOD zif_crm_partner~update_id_numbers.
*CALL METHOD SUPER->ZIF_CRM_PARTNER~UPDATE_ID_NUMBERS
*  EXPORTING
*    I_DATA =
*    .
    TRY.
        me->switch_to_change_mode( ).
      CATCH zcx_crm_app INTO DATA(v_lock_exception).
        RAISE EXCEPTION v_lock_exception.
    ENDTRY.
    TRY.

        DATA(v_builheader_id_rel_col) = me->entity_root->get_related_entities_by_bpath(
                                        iv_bpath_statement  = |BuilIdentificationRel[@IDENTIFICATIONTYPE="ZNATID"]| ).


        IF v_builheader_id_rel_col->size( ) > 0.


          DATA(v_builheader_id) = v_builheader_id_rel_col->get_first( ).
          v_builheader_id->set_properties( is_attributes = i_data  ).
        ELSE.
          v_builheader_id = me->entity_root->create_related_entity( iv_relation_name = |BuilIdentificationRel| ).
          v_builheader_id->set_properties( is_attributes = i_data  ).
        ENDIF.

      CATCH cx_crm_genil_duplicate_key
            cx_crm_genil_model_error.
        "
        zcx_crm_app=>raise_failed(
*               EXPORTING
*                 i_text       =
*                 i_empty_text =
        ).
    ENDTRY.
    cl_crm_bol_core=>get_instance( )->modify( ).


  ENDMETHOD.


  METHOD zif_crm_partner~update_std_address.
*CALL METHOD SUPER->ZIF_CRM_PARTNER~UPDATE_STD_ADDRESS
*  EXPORTING
*    I_DATA =
*    .
    DATA(v_address_bpath) = |./BuilAddressRel[@STANDARDADDRESS="X"]|.

    DATA(v_std_addrses) =  me->entity_root->get_related_entities_by_bpath(
       EXPORTING
         iv_bpath_statement = v_address_bpath
     ).
    DATA(v_new_addr) = CORRESPONDING crmt_bupa_il_address( i_data ).



    IF v_std_addrses->size( ) > 0.
      DATA(v_std_addr) = v_std_addrses->get_first( ).
      v_std_addr->get_property_as_value(
        EXPORTING
          iv_attr_name =   |ADDRESS_GUID|  " Component Name|
        IMPORTING
          ev_result    = v_new_addr-address_guid
      ).

      v_std_addr->get_property_as_value(
       EXPORTING
         iv_attr_name =   |ADDRESS_NUMBER|  " Component Name|
       IMPORTING
         ev_result    = v_new_addr-address_number
     ).


      v_std_addr->get_property_as_value(
      EXPORTING
        iv_attr_name =   |STANDARDADDRESS|  " Component Name|
      IMPORTING
        ev_result    = v_new_addr-standardaddress
    ).

* Telefone
      DATA(v_tel_info) = CORRESPONDING crmst_telephone_buil( i_data MAPPING telephone = telephonetel
                                                                            extension = extensiontel
                                                                            countryiso = countryiso
                                                                            country    = country  ).
      v_tel_info-std_no = 'X'.
      v_tel_info-valid_from = sy-datum.
      v_tel_info-valid_to    = '99991231'.

* Email
      DATA(v_email) = CORRESPONDING crmst_email_buil( i_data MAPPING e_mail = e_mailsmt ).
      v_email-std_no = 'X'.
      v_email-validfromdate = sy-datum.
      v_email-valid_to    = '99991231'.


*BuilAddressPhone
      TRY.
          me->switch_to_change_mode( ).
        CATCH zcx_crm_app INTO DATA(v_lock_exception).
          RAISE EXCEPTION v_lock_exception.
      ENDTRY.

      TRY.
*        Set Address
          v_std_addr->set_properties( is_attributes = v_new_addr ).


*      Phone Relation
          DATA(v_phone_rel_col) = v_std_addr->get_related_entities_by_bpath(
                        iv_bpath_statement      = |BuilAddressPhoneRel[@STD_NO="X"]|
*
                    ).

          IF v_phone_rel_col->size( ) = 0.

            DATA(v_phone_rel) = v_std_addr->create_related_entity( iv_relation_name = |BuilAddressPhoneRel| ).
            v_phone_rel->set_properties( is_attributes = v_tel_info ).

          ELSE.
            v_phone_rel = v_phone_rel_col->get_first( ).
            v_phone_rel->get_property_as_value(
              EXPORTING
                iv_attr_name = |CONSNUMBER|    " Component Name
              IMPORTING
                ev_result    = v_tel_info-consnumber
            ).
            v_phone_rel->set_properties( is_attributes = v_tel_info ).

          ENDIF.

*      Email Relation:
          DATA(v_email_rel_col) = v_std_addr->get_related_entities_by_bpath(
                                          iv_bpath_statement    = |BuilAddressEMailRel[@STD_NO="X"]| ).

          IF v_email_rel_col->size( ) = 0.
            DATA(v_email_rel) = v_std_addr->create_related_entity( iv_relation_name =  |BuilAddressEMailRel| ).
            v_email_rel->set_properties( is_attributes = v_email ).
          ELSE.
            v_email_rel = v_email_rel_col->get_first( ).
            v_email_rel->get_property_as_value(
              EXPORTING
                iv_attr_name = |CONSNUMBER|    " Component Name
              IMPORTING
                ev_result    = v_email-consnumber ).

            v_email_rel->set_properties( is_attributes = v_email ).
          ENDIF.


*
*

          cl_crm_bol_core=>get_instance( )->modify( ).


        CATCH cx_crm_genil_model_error.  "

        CATCH cx_crm_genil_duplicate_rel.    "
          zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
