CLASS zcl_crm_base_order DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_crm_order .


  PROTECTED SECTION.
    METHODS check_required_fields
      IMPORTING
        !i_create_attr TYPE crmst_order_create_btil .
    METHODS check_process_type
      IMPORTING
        !i_create_attr_process_type TYPE crmt_process_type .



  PRIVATE SECTION.



ENDCLASS.



CLASS ZCL_CRM_BASE_ORDER IMPLEMENTATION.


  METHOD check_process_type.
*-- TODO find a better way to handle test

*-- skip validation for test process type

    IF i_create_attr_process_type  EQ 'ZTST'.

      RETURN.

    ENDIF.

*-- Check if process type exists
    SELECT COUNT(*)
       FROM crmc_proc_type
        WHERE process_type EQ i_create_attr_process_type.
    IF sy-subrc IS NOT INITIAL.
      zcx_crm_entity_api=>raise_invalid_value(
                        i_input_name  = 'Procces Type'
                        i_input_value = i_create_attr_process_type
                       ).
    ENDIF.

  ENDMETHOD.


  METHOD check_required_fields.

    IF i_create_attr-process_type IS INITIAL.
      zcx_crm_entity_api=>raise_missing_input( 'Process Type' ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_crm_entity~create.
    FIELD-SYMBOLS <create_attr> TYPE crmst_order_create_btil.
    ASSIGN i_create_attr TO <create_attr>.

    me->check_process_type( i_create_attr_process_type = <create_attr>-process_type ).

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


  METHOD zif_crm_entity~refresh.

  ENDMETHOD.


  METHOD zif_crm_entity~set_attribute_value.

  ENDMETHOD.


  METHOD zif_crm_order~add_appointment.

  ENDMETHOD.


  METHOD zif_crm_order~add_extension_list_entry.

  ENDMETHOD.


  METHOD zif_crm_order~add_item.

  ENDMETHOD.


  METHOD zif_crm_order~add_partner.

  ENDMETHOD.


  METHOD zif_crm_order~add_predecessor.

  ENDMETHOD.


  METHOD zif_crm_order~add_reference_object.

  ENDMETHOD.


  METHOD zif_crm_order~add_text.

  ENDMETHOD.


  METHOD zif_crm_order~delete_appointment.

  ENDMETHOD.


  METHOD zif_crm_order~delete_extension_list_entry.

  ENDMETHOD.


  METHOD zif_crm_order~delete_item.

  ENDMETHOD.


  METHOD zif_crm_order~delete_partner.

  ENDMETHOD.


  METHOD zif_crm_order~delete_predecessor.

  ENDMETHOD.


  METHOD zif_crm_order~delete_reference_object.

  ENDMETHOD.


  METHOD zif_crm_order~delete_text.

  ENDMETHOD.


  METHOD zif_crm_order~description.

  ENDMETHOD.


  METHOD zif_crm_order~get_appointment_list.

  ENDMETHOD.


  METHOD zif_crm_order~get_change_history_list.


    DATA: ls_status_changedocument TYPE zbtx_status_changedocument_s.
    DATA: lt_jcds                  TYPE TABLE OF crm_jcds.
    DATA: ls_jcds                  TYPE crm_jcds.
    DATA: ls_jcds_old              TYPE crm_jcds.
    DATA: lv_tabix                 TYPE sy-tabix.
    DATA: lv_proc_type             TYPE crmt_process_type.
    DATA: ls_proc_type             TYPE crmc_proc_type.
    DATA: lv_status_descr          TYPE j_txt30.

    FIELD-SYMBOLS: <fs_status_changedocument> TYPE zbtx_status_changedocument_s.


    DATA(v_order_guid) = me->zif_crm_entity~get_entity_id( ).

    SELECT SINGLE process_type
      FROM crmd_orderadm_h
      INTO lv_proc_type
      WHERE guid = v_order_guid.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'CRM_ORDER_PROC_TYPE_SELECT_CB'
      EXPORTING
        iv_process_type      = lv_proc_type
      IMPORTING
        es_proc_type         = ls_proc_type
      EXCEPTIONS
        entry_not_found      = 1
        text_entry_not_found = 2
        OTHERS               = 3.

    CHECK sy-subrc = 0.

    SELECT *
      FROM crm_jcds
      INTO TABLE lt_jcds
      WHERE objnr EQ v_order_guid.

    CHECK sy-subrc = 0.

    DELETE lt_jcds WHERE stat(1) <> 'E'.

**********************************************************************
* Reduce amount of data by combining changes referring to each other
**********************************************************************

    CLEAR ls_jcds_old.

    SORT lt_jcds BY udate ASCENDING utime ASCENDING inact DESCENDING.
    LOOP AT lt_jcds INTO ls_jcds_old WHERE NOT inact IS INITIAL.
      lv_tabix = sy-tabix.
*   Get status which replaced old one
      ADD 1 TO lv_tabix.
      READ TABLE lt_jcds INTO ls_jcds INDEX lv_tabix.
      IF sy-subrc = 0.
*     Adjust protocol for active status
        CLEAR ls_status_changedocument.
        MOVE-CORRESPONDING ls_jcds TO ls_status_changedocument.
        ls_status_changedocument-stat_old = ls_jcds_old-stat.
        APPEND ls_status_changedocument TO r_result.
        DELETE lt_jcds INDEX lv_tabix.
*     Protocol for inactive status not needed any more:
        DELETE lt_jcds.
      ENDIF.
    ENDLOOP.

**********************************************************************
* Collect remaining data:
**********************************************************************

    LOOP AT lt_jcds INTO ls_jcds.
      CLEAR ls_status_changedocument.
      MOVE-CORRESPONDING ls_jcds TO ls_status_changedocument.
      CLEAR ls_status_changedocument-stat_old.
      APPEND ls_status_changedocument TO r_result.
    ENDLOOP.
    REFRESH lt_jcds.

**********************************************************************
* Get Descriptions
**********************************************************************

    LOOP AT r_result ASSIGNING <fs_status_changedocument>.

      lv_status_descr   = cl_generic_act_methods=>get_status_description(
                                            status_profile = ls_proc_type-user_stat_proc
                                            status_key = <fs_status_changedocument>-stat ).

      MOVE lv_status_descr TO <fs_status_changedocument>-f_new.

      lv_status_descr   = cl_generic_act_methods=>get_status_description(
                                            status_profile = ls_proc_type-user_stat_proc
                                            status_key = <fs_status_changedocument>-stat_old ).

      MOVE lv_status_descr TO <fs_status_changedocument>-f_old.

    ENDLOOP.


*-- GET user full name based ON sys user name
    SELECT bname, name_textc FROM user_addr AS user_data
    INTO TABLE @DATA(v_user_ino_tab)
     FOR ALL ENTRIES IN @r_result
      WHERE user_data~bname = @r_result-usnam.

    LOOP AT r_result ASSIGNING FIELD-SYMBOL(<fs_status>).

      TRY.
          <fs_status>-user_fname = v_user_ino_tab[ bname = <fs_status>-usnam ]-name_textc.

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_crm_order~get_customer_h_extension_field.

  ENDMETHOD.


  METHOD zif_crm_order~get_customer_h_extension_struc.

  ENDMETHOD.


  METHOD zif_crm_order~get_docflow_list.

  ENDMETHOD.


  METHOD zif_crm_order~get_dynamic_boolean.

  ENDMETHOD.


  METHOD zif_crm_order~get_extension_list.

  ENDMETHOD.


  METHOD zif_crm_order~get_extension_struc.

  ENDMETHOD.


  METHOD zif_crm_order~get_extension_value.

  ENDMETHOD.


  METHOD zif_crm_order~get_item_by_item_no.
  ENDMETHOD.


  METHOD zif_crm_order~get_item_list.

  ENDMETHOD.


  METHOD zif_crm_order~get_orderadm_h_extension_field.

  ENDMETHOD.


  METHOD zif_crm_order~get_orderadm_h_extension_struc.

  ENDMETHOD.


  METHOD zif_crm_order~get_partner_address.

  ENDMETHOD.


  METHOD zif_crm_order~get_partner_list.

  ENDMETHOD.


  METHOD zif_crm_order~get_predecessor_list.

  ENDMETHOD.


  METHOD zif_crm_order~get_reference_object_list.

  ENDMETHOD.


  METHOD zif_crm_order~get_text_list.

  ENDMETHOD.


  METHOD zif_crm_order~modify_appointment.

  ENDMETHOD.


  METHOD zif_crm_order~modify_extension_list_entry.

  ENDMETHOD.


  METHOD zif_crm_order~modify_extension_struc.

  ENDMETHOD.


  METHOD zif_crm_order~modify_item.
*    me->zif_crm_entity~

  ENDMETHOD.


  METHOD zif_crm_order~modify_partner.

  ENDMETHOD.


  METHOD zif_crm_order~modify_predecessor.

  ENDMETHOD.


  METHOD zif_crm_order~modify_reference_object.

  ENDMETHOD.


  METHOD zif_crm_order~modify_text.

  ENDMETHOD.


  METHOD zif_crm_order~object_guid.

  ENDMETHOD.


  METHOD zif_crm_order~object_id.

  ENDMETHOD.


  METHOD zif_crm_order~set_customer_h_extension_field.

  ENDMETHOD.


  METHOD zif_crm_order~set_dynamic_boolean.

  ENDMETHOD.


  METHOD zif_crm_order~set_extension_struc.

  ENDMETHOD.


  METHOD zif_crm_order~set_extension_struc_2.

  ENDMETHOD.


  METHOD zif_crm_order~set_extension_value.

  ENDMETHOD.


  METHOD zif_crm_order~set_orderadm_h_extension_field.

  ENDMETHOD.


  METHOD zif_crm_order~set_partner_address.

  ENDMETHOD.


  METHOD zif_crm_order~status.

  ENDMETHOD.
ENDCLASS.
