CLASS zcx_gateway_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF c_kind_of_error,
        business_error  TYPE char1 VALUE 'B',
        technical_error TYPE char1 VALUE 'T',
      END OF c_kind_of_error .

    CONSTANTS:
      BEGIN OF zcx_cua_btx_general,
        msgid TYPE symsgid VALUE 'ZBF_CUA',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_cua_btx_general .
    CONSTANTS:
      BEGIN OF error,
        msgid TYPE symsgid VALUE 'ZBF_CUA',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'ERROR_MSG',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error .
    DATA error_msg TYPE string .
    DATA callstack TYPE abap_callstack .
    DATA kind_of_error TYPE char1 .

    METHODS constructor
      IMPORTING
        !textid        LIKE if_t100_message=>t100key OPTIONAL
        !previous      LIKE previous OPTIONAL
        !error_msg     TYPE string OPTIONAL
        !callstack     TYPE abap_callstack OPTIONAL
        !kind_of_error TYPE char1 OPTIONAL.

    METHODS throw_gateway_exception
      RAISING
        /iwbep/cx_mgw_busi_exception
        /iwbep/cx_mgw_tech_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_gateway_exception IMPLEMENTATION.




  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->error_msg = error_msg .
    me->callstack = callstack .
    me->kind_of_error = kind_of_error.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_cua_btx_general .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD throw_gateway_exception.

    CASE me->kind_of_error.
      WHEN c_kind_of_error-business_error.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid  = /iwbep/cx_mgw_busi_exception=>business_error
            message = CONV #( me->error_msg ).

      WHEN OTHERS.
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

    ENDCASE.


  ENDMETHOD.

ENDCLASS.
