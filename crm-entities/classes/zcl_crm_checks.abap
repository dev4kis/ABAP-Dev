CLASS zcl_crm_checks DEFINITION
  INHERITING FROM zcl_crm_checks_factory
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

*    Types: begin of ty_messages,
*                type_of_message type char1, " E Error, W Warning....
*                kind_of_message type char1, " B Business T Technical
*                message type string,
*           end of ty_messages.
*
*    Types: ty_messages_tab TYPE STANDARD TABLE OF ty_messages WITH DEFAULT KEY.
*
*    Methods get_messages
*        RETURNING VALUE(r_message_tab) TYPE  ty_messages_tab.
*
    Methods zif_crm_checks~get_messages
        REDEFINITION.

    Methods zif_crm_checks~consistency_check
        REDEFINITION.
  PROTECTED SECTION.
    Methods set_message
        Importing i_message_line TYPE ZIF_CRM_CHECKS=>ty_messages.
  PRIVATE SECTION.

    DATA: v_message_tab type ZIF_CRM_CHECKS=>ty_messages_tab.

ENDCLASS.



CLASS zcl_crm_checks IMPLEMENTATION.
  METHOD zif_crm_checks~consistency_check.

  ENDMETHOD.

  METHOD set_message.
    Append i_message_line to me->v_message_tab.
  ENDMETHOD.

  METHOD zif_crm_checks~get_messages.
    r_message_tab = me->v_message_tab.
  ENDMETHOD.

ENDCLASS.
