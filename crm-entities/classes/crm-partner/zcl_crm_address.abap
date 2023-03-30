CLASS zcl_crm_address DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_crm_address.
    METHODS constructor
      IMPORTING
        i_general_data TYPE zif_crm_address=>ty_general_data.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA  general_data TYPE zif_crm_address=>ty_general_data.


ENDCLASS.



CLASS zcl_crm_address IMPLEMENTATION.

  METHOD constructor.

    me->general_data = i_general_data.

  ENDMETHOD.

  METHOD zif_crm_address~get_general_data.

    r_result = me->general_data.

  ENDMETHOD.

ENDCLASS.
