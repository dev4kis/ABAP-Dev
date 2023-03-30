CLASS zcl_base_odata_entity DEFINITION
  PUBLIC ABSTRACT

  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_odata_entity.
  PROTECTED SECTION.

    DATA odata_application TYPE REF TO zif_odata_application.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_base_odata_entity IMPLEMENTATION.

  METHOD zif_odata_entity~count.

    zcx_crm_app=>raise_not_implemented( ).

  ENDMETHOD.

  METHOD zif_odata_entity~create.
    zcx_crm_app=>raise_not_implemented( ).
  ENDMETHOD.

  METHOD zif_odata_entity~delete.
    zcx_crm_app=>raise_not_implemented( ).
  ENDMETHOD.

  METHOD zif_odata_entity~get_stream.

    zcx_crm_app=>raise_not_implemented( ).
  ENDMETHOD.

  METHOD zif_odata_entity~handle_action.
    zcx_crm_app=>raise_not_implemented( ).
  ENDMETHOD.

  METHOD zif_odata_entity~init.

    me->odata_application = i_application.

  ENDMETHOD.

  METHOD zif_odata_entity~query.
    zcx_crm_app=>raise_not_implemented( ).
  ENDMETHOD.

  METHOD zif_odata_entity~read.
    zcx_crm_app=>raise_not_implemented( ).
  ENDMETHOD.

  METHOD zif_odata_entity~update.
    zcx_crm_app=>raise_not_implemented( ).
  ENDMETHOD.

  METHOD zif_odata_entity~query_expand.
    zcx_crm_app=>raise_not_implemented( ).
  ENDMETHOD.

ENDCLASS.
