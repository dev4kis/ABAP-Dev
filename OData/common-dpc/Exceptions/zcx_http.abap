class ZCX_HTTP definition
  public
  inheriting from CX_NO_CHECK
  final
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF zcx_http,
        msgid TYPE symsgid VALUE 'ZHTTP',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_http .
  constants:
    BEGIN OF zcx_http_401,
        msgid TYPE symsgid VALUE 'ZHTTP',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_http_401 .
  constants:
    BEGIN OF zcx_http_500,
        msgid TYPE symsgid VALUE 'ZHTTP',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_http_500 .
  constants:
    BEGIN OF zcx_http_400,
        msgid TYPE symsgid VALUE 'ZHTTP',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_http_400 .
  data MSGV1 type SYMSGV .
  data MSGV2 type SYMSGV .
  data MSGV3 type SYMSGV .
  data MSGV4 type SYMSGV .
  data HTTP_RETURN_CODE type NUM4 .
  constants:
    BEGIN OF http_return_codes,
        unauthorized          TYPE num4 VALUE 401,
        bad_request           TYPE num4 VALUE 400,
        internal_server_error TYPE num4 VALUE 500,
      END OF http_return_codes .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional
      !HTTP_RETURN_CODE type NUM4 optional .
  class-methods RAISE_UNAUTHORIZED
    importing
      !I_TEXT type CSEQUENCE .
  class-methods RAISE_BAD_REQUEST
    importing
      !I_TEXT type CSEQUENCE .
  class-methods RAISE_INTERNAL_SERVER_ERROR
    importing
      !I_TEXT type CSEQUENCE .

  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_HTTP IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->msgv1 = msgv1 .
    me->msgv2 = msgv2 .
    me->msgv3 = msgv3 .
    me->msgv4 = msgv4 .
    me->http_return_code = http_return_code .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_http .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD if_message~get_text.
    result = super->get_text( ).

    IF if_t100_message~t100key-msgid = 'ZHTTP' AND if_t100_message~t100key-msgno = '004'.
      REPLACE ALL OCCURRENCES OF '   .' IN result WITH '.'.
    ENDIF.
  ENDMETHOD.


  METHOD raise_bad_request.
    DATA v_msgv1 TYPE symsgv.
    DATA v_msgv2 TYPE symsgv.
    DATA v_msgv3 TYPE symsgv.
    DATA v_msgv4 TYPE symsgv.

    TRY .

        v_msgv1 = i_text.
        v_msgv2 = i_text+50(50).
        v_msgv3 = i_text+100(50).
        v_msgv4 = i_text+150(50).

      CATCH cx_sy_range_out_of_bounds.
    ENDTRY.

    RAISE EXCEPTION TYPE zcx_http
      EXPORTING
        textid           = zcx_http_400
        http_return_code = http_return_codes-bad_request
        msgv1            = v_msgv1
        msgv2            = v_msgv2
        msgv3            = v_msgv3
        msgv4            = v_msgv4.
  ENDMETHOD.


  METHOD raise_internal_server_error.
    DATA v_msgv1 TYPE symsgv.
    DATA v_msgv2 TYPE symsgv.
    DATA v_msgv3 TYPE symsgv.
    DATA v_msgv4 TYPE symsgv.

    TRY .

        v_msgv1 = i_text.
        v_msgv2 = i_text+50(50).
        v_msgv3 = i_text+100(50).
        v_msgv4 = i_text+150(50).

      CATCH cx_sy_range_out_of_bounds.
    ENDTRY.

    RAISE EXCEPTION TYPE zcx_http
      EXPORTING
        textid           = zcx_http_500
        http_return_code = http_return_codes-internal_server_error
        msgv1            = v_msgv1
        msgv2            = v_msgv2
        msgv3            = v_msgv3
        msgv4            = v_msgv4.
  ENDMETHOD.


  METHOD raise_unauthorized.
    DATA v_msgv1 TYPE symsgv.
    DATA v_msgv2 TYPE symsgv.
    DATA v_msgv3 TYPE symsgv.
    DATA v_msgv4 TYPE symsgv.

    TRY.
        v_msgv1 = i_text.
        v_msgv2 = i_text+50(50).
        v_msgv3 = i_text+100(50).
        v_msgv4 = i_text+150(50).
      CATCH cx_sy_range_out_of_bounds.
    ENDTRY.

    RAISE EXCEPTION TYPE zcx_http
      EXPORTING
        textid           = zcx_http_401
        http_return_code = http_return_codes-unauthorized
        msgv1            = v_msgv1
        msgv2            = v_msgv2
        msgv3            = v_msgv3
        msgv4            = v_msgv4.
  ENDMETHOD.
ENDCLASS.
