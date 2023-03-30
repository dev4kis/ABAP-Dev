class ZCL_CRM_TRANSACTION_BOL definition
  public
  inheriting from ZCL_CRM_BASE_TRANSACTION
  final
  create private .

public section.

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCE) type ref to ZIF_CRM_TRANSACTION .
  methods CONSTRUCTOR .

  methods ZIF_CRM_TRANSACTION~COMMIT
    redefinition .
  methods ZIF_CRM_TRANSACTION~GET_TRANSACTION
    redefinition .
  methods ZIF_CRM_TRANSACTION~SAVE
    redefinition .
  methods ZIF_CRM_TRANSACTION~ROLLBACK
    redefinition .
protected section.
  PRIVATE SECTION.
    DATA bol_transaction TYPE REF TO cl_crm_bol_custom_tx_ctxt.
    CLASS-DATA: v_instance TYPE REF TO zif_crm_transaction.

ENDCLASS.



CLASS ZCL_CRM_TRANSACTION_BOL IMPLEMENTATION.


  METHOD constructor.
*  **********************************************************************
*  * create transaction if we are in BOL context
*  **********************************************************************
    super->constructor(  ).

  ENDMETHOD.


  METHOD get_instance.
    IF v_instance IS INITIAL.
      v_instance = NEW zcl_crm_transaction_bol(  ).
    ENDIF.
    r_instance = v_instance.
  ENDMETHOD.


  METHOD zif_crm_transaction~commit.

    bol_transaction->if_bol_transaction_context~commit(  ).
  ENDMETHOD.


  METHOD zif_crm_transaction~get_transaction.
    r_result = me->bol_transaction.
  ENDMETHOD.


  METHOD zif_crm_transaction~rollback.
    bol_transaction->if_bol_transaction_context~rollback( ).
  ENDMETHOD.


  METHOD zif_crm_transaction~save.
    DATA: object_guid TYPE crmt_object_guid.

    me->bol_transaction = NEW cl_crm_bol_custom_tx_ctxt(  ).

    LOOP AT me->transaction_data ASSIGNING FIELD-SYMBOL(<transaction_data>).

      DATA(entity_root) = cl_crm_bol_core=>get_instance( )->get_root_entity( iv_object_name = <transaction_data>-object
                                                                             iv_object_guid = <transaction_data>-objectkey ).

      cl_crm_genil_container_tools=>get_key_from_object_id( EXPORTING iv_object_id = entity_root->get_transaction(  )->get_id(  )
                                                                      iv_object_name = entity_root->get_transaction(  )->get_name(  )
                                                            IMPORTING es_key = object_guid ).

      IF entity_root->get_transaction(  )->check_save_needed(  ) = abap_true.

        DATA(v_checker) =  zcl_crm_checks_factory=>factory( i_object_id      = object_guid
                                                            i_root_object    = entity_root
                                                            i_object         = <transaction_data>-object
                                                            i_access_method  = '1' ).

        IF v_checker IS NOT INITIAL.
          IF v_checker->consistency_check(  ) EQ abap_false.
            DATA(v_messages_tab) = v_checker->get_messages(  ).
            IF line_exists(  v_messages_tab[ 1 ] ).
              IF v_messages_tab[ 1 ]-kind_of_message = zcx_crm_app=>be_api_error_kind-business_error.
                zcx_crm_app=>raise_inconsistent( i_text = v_messages_tab[ 1 ]-message ).
              ELSE.
                zcx_crm_app=>raise_failed( i_text = v_messages_tab[ 1 ]-message i_empty_text = abap_true ).
              ENDIF.
            ELSE.
              zcx_crm_app=>raise_failed( i_text = 'General Error' ).
            ENDIF.
          ENDIF.
        ENDIF.

        me->bol_transaction->add_tx_context( entity_root->get_transaction(  ) ).



      ENDIF.


      "Skip validation for business partner
    ENDLOOP.



    IF  me->bol_transaction->if_bol_transaction_context~check_save_needed( ) EQ abap_false.
      RETURN.
    ENDIF.


    IF me->bol_transaction->if_bol_transaction_context~check_save_possible( ) EQ abap_false.
      DATA(o_gloal) = cl_crm_bol_core=>get_instance( )->get_global_message_cont( ).
      o_gloal->if_genil_message_container~get_messages(
        EXPORTING
          iv_message_type = 'E'                 " Messages, Message Type
*          iv_for_display  = abap_false       " Logical Variable
        IMPORTING
          et_messages     = DATA(t_mes)
      ).
      cl_crm_bol_core=>get_instance( )->get_message_cont_manager( )->get_all_message_containers(
        IMPORTING
          et_result = DATA(t_re)                 " Table of Message Containers
      ).
      " Error must be raised
      "TO-DO read global Message Container to provide more detail of error to Caller
      zcx_crm_app=>raise_failed(
        EXPORTING
          i_text       = TEXT-001
      ).
      RETURN. ""
    ENDIF.

    IF bol_transaction->if_bol_transaction_context~save( ) <> 'X'.
      bol_transaction->if_bol_transaction_context~rollback( ).
    ENDIF.


  ENDMETHOD.
ENDCLASS.
