CLASS zcl_my_odata_app DEFINITION
  PUBLIC
  INHERITING FROM zcl_default_odata_app
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS co_service_name TYPE string VALUE 'ZCRM_TUR_SRV' ##NO_TEXT.
    CONSTANTS co_service_version TYPE numc4 VALUE '0001' ##NO_TEXT.

    METHODS: constructor,
      zif_odata_application~is_anonymous_access_allowed REDEFINITION,
      zif_odata_application~register_odata_module REDEFINITION
      ,zif_odata_application~finish_process REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA transaction TYPE REF TO zif_crm_transaction .
ENDCLASS.



CLASS ZCL_MY_ODATA_APP IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    me->base_class = zcl_tur_base_odata_entity=>co_base_entity_classname.


*-- Create transaction context in order to handle save manually
    me->transaction = zcl_crm_transaction_bol=>get_instance(  ).

  ENDMETHOD.


  METHOD zif_odata_application~finish_process.
    TRY.
        me->transaction->save( ).
      CATCH zcx_crm_app INTO DATA(v_exception).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid        = zcx_cua_btx_general=>error
            error_msg     = v_exception->get_text( )
            callstack     = zcl_cua_bxt_tools=>get_callstack( )
            kind_of_error = v_exception->kind_of_error.
    ENDTRY..
*  UHD Incident: 20950774: for requests resulting from a split, prints can only be generated after save
*    zcl_ibase_tur_api=>generate_print_for_splits( ).
  ENDMETHOD.


  METHOD zif_odata_application~is_anonymous_access_allowed.
    r_result = abap_false.
  ENDMETHOD.


  METHOD zif_odata_application~register_odata_module.
*
*    CLEAR c_registered_module.
  ENDMETHOD.
ENDCLASS.
