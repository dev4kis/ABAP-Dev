CLASS zcx_gateway_technic_exception DEFINITION
  PUBLIC
  INHERITING FROM zcx_gateway_exception
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous  LIKE previous OPTIONAL
        !error_msg TYPE string OPTIONAL
        !callstack TYPE abap_callstack OPTIONAL .

    METHODS throw_gateway_exception
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_gateway_technic_exception IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous  = previous
        error_msg = error_msg
        callstack = callstack.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD throw_gateway_exception.
    DATA: lr_msg_container TYPE REF TO  /iwbep/if_message_container.
    READ TABLE me->callstack INDEX 2 ASSIGNING FIELD-SYMBOL(<fs_callstack>).

    lr_msg_container ?= /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

    lr_msg_container->add_message( EXPORTING iv_msg_type            = 'E'
                                             iv_msg_id             = me->error-msgid
                                             iv_msg_number         = 001
                                             iv_is_leading_message = abap_true
*                                             iv_msg_text         = 'Lange Message'
                                             iv_msg_v1           = CONV #( me->error_msg ) ).

    IF <fs_callstack> IS ASSIGNED.
      lr_msg_container->add_message( EXPORTING iv_msg_type            = 'E'
                                               iv_msg_id             = me->error-msgid
                                               iv_msg_number         = 002
*                                             iv_error_category     = /iwbep/if_message_container=>gcs_error_category-processing  " processing error
                                               iv_is_leading_message = abap_false
*                                              iv_msg_text         = 'Lange Message'
                                               iv_msg_v1           = CONV #( <fs_callstack>-mainprogram )
                                               iv_msg_v2           = CONV #( <fs_callstack>-line )
                                               iv_msg_v3           = CONV #( <fs_callstack>-blockname ) ).
    ENDIF.




    RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
      EXPORTING
        textid            = zcx_cua_btx_general=>error
        message_container = lr_msg_container.

  ENDMETHOD.
ENDCLASS.
