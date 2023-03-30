CLASS zcl_crm_checks_factory DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_crm_checks .

    CLASS-METHODS factory
      IMPORTING
        !i_object_id     TYPE crmt_object_guid
        !i_object        TYPE crmt_ext_obj_name
        !i_access_method TYPE zcrm_api_access_method
        !i_root_object   type  REF TO CL_CRM_BOL_ENTITY
      RETURNING
        VALUE(r_result)  TYPE REF TO zif_crm_checks .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_crm_checks_factory IMPLEMENTATION.


  METHOD factory.

    IF i_object = 'BTOrder'.

      CASE i_root_object->get_related_entity('BTOrderHeader')->get_property_as_string( 'PROCESS_TYPE' ).
        WHEN zif_const_btx_proc_type=>gc_proc_type_exas_req.

          r_result = NEW zcl_check_admission_request( i_object_id = i_object_id i_access_method = i_access_method ).
        WHEN OTHERS.

      ENDCASE..
    ENDIF.
  ENDMETHOD.
ENDCLASS.
