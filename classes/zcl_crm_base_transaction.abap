CLASS zcl_crm_base_transaction DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED
  GLOBAL FRIENDS zcl_crm_api_entity_factory.

  PUBLIC SECTION.
    INTERFACES zif_crm_transaction.

    METHODS add
      IMPORTING i_transaction_data type ZCRM_TRANSACTION_DATA.


    DATA entities TYPE TABLE OF REF TO zif_crm_entity.

  PROTECTED SECTION.
    DATA transaction_data type ZCRM_TRANSACTION_DATA_T.

  PRIVATE SECTION.
    CLASS-DATA self TYPE REF TO zif_crm_transaction.

ENDCLASS.



CLASS ZCL_CRM_BASE_TRANSACTION IMPLEMENTATION.


  METHOD add.
      try.
          data(exists) = transaction_data[ object = i_transaction_data-object objectkey = i_transaction_data-objectkey ].
      catch cx_sy_itab_line_not_found.
        APPEND i_transaction_data to transaction_data.
      endtry.
  ENDMETHOD.


  METHOD zif_crm_transaction~commit.

  ENDMETHOD.


  METHOD zif_crm_transaction~rollback.
  ENDMETHOD.


  METHOD zif_crm_transaction~save.

  ENDMETHOD.
ENDCLASS.
