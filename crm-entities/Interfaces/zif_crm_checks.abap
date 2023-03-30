interface ZIF_CRM_CHECKS
  public .

    CONSTANTS: c_business_error TYPE char1 VALUE 'B'.
    CONSTANTS: c_technical_error TYPE char1 VALUE 'T'.
    CONSTANTS: c_error TYPE char1 VALUE 'E'.
    CONSTANTS: c_warning TYPE char1 VALUE 'W'.
    CONSTANTS: c_information TYPE char1 VALUE 'I'.

    Types: begin of ty_messages,
                type_of_message type char1, " E Error, W Warning....
                kind_of_message type char1, " B Business T Technical
                message type string,
           end of ty_messages.

    Types: ty_messages_tab TYPE STANDARD TABLE OF ty_messages WITH DEFAULT KEY.


    methods consistency_check
       RETURNING VALUE(r_result) type crmt_boolean.
    methods check_order_partner_exists
       RETURNING VALUE(r_result) type crmt_boolean.
    Methods get_messages
        RETURNING VALUE(r_message_tab) TYPE  ty_messages_tab.
endinterface.


