CLASS zcl_default_odata_app DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_odata_application .


protected section.

  data ATTRIBUTE_TAB type CRMTT_KEY_VALUE .
  data CURRENT_USER type ref to ZIF_EXPO_USER .
  data CURRENT_TUTORIAL_ORIGIN_USER type ref to ZIF_EXPO_USER .
  data SERVICE_DETAILS type ZIF_ODATA_APPLICATION=>TY_ODATA_SERVICE_TECH .
  data SERVICE_CONTEXT type ref to /IWBEP/IF_MGW_CONTEXT .
  data BASE_CLASS type SEOCLSNAME .
  PRIVATE SECTION.
    DATA model TYPE REF TO /iwbep/if_mgw_odata_re_model.

ENDCLASS.



CLASS ZCL_DEFAULT_ODATA_APP IMPLEMENTATION.


  METHOD zif_odata_application~finish_process.

  ENDMETHOD.


  METHOD zif_odata_application~get_context_attribute.

    TRY.

        r_result = me->attribute_tab[ ddlbkey = i_attribute ]-ddlbvalue.

      CATCH cx_sy_itab_line_not_found.
    ENDTRY.


  ENDMETHOD.


  method ZIF_ODATA_APPLICATION~GET_CURRENT_TUT_ORIGIN_USER.
     r_result = me->current_tutorial_origin_user.
  endmethod.


  METHOD zif_odata_application~get_current_user.

    r_result = me->current_user.

  ENDMETHOD.


  METHOD zif_odata_application~get_entity_handler.

    r_result = zcl_odata_entity_factory=>get_entity_impl(  i_entity_name = i_entity_name
                                                                i_base_class = me->base_class ).

    IF r_result IS INITIAL.

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc MESSAGE e001(zodata_cdpc).

    ENDIF.

  ENDMETHOD.


  METHOD zif_odata_application~get_model.
    r_result = me->model.
  ENDMETHOD.


  METHOD zif_odata_application~get_service_details.

    r_result = me->service_details.

  ENDMETHOD.


  METHOD zif_odata_application~init.


    me->service_details = VALUE #( service_name = i_service_name
                                  service_version = i_service_version ).


    me->service_context =  i_odata_context.


  ENDMETHOD.


  METHOD zif_odata_application~is_anonymous_access_allowed.

    r_result = abap_false.

  ENDMETHOD.


  METHOD zif_odata_application~register_odata_module.

  ENDMETHOD.


  METHOD zif_odata_application~set_context_attribute.

    READ TABLE me->attribute_tab WITH KEY ddlbkey = to_upper( i_attribute )
     ASSIGNING FIELD-SYMBOL(<fs_attribute>).
    IF <fs_attribute> IS NOT ASSIGNED.
      APPEND INITIAL LINE TO me->attribute_tab ASSIGNING <fs_attribute>.
    ENDIF.

    <fs_attribute>-ddlbkey      = to_upper( i_attribute ).
    <fs_attribute>-ddlbvalue    = i_value.

  ENDMETHOD.


  METHOD zif_odata_application~set_current_tut_origin_user.
    me->current_tutorial_origin_user = i_user.
  ENDMETHOD.


  METHOD zif_odata_application~set_current_user.

    me->current_user = i_user.

  ENDMETHOD.


  METHOD zif_odata_application~set_model.

    me->model = i_model.

  ENDMETHOD.
ENDCLASS.
