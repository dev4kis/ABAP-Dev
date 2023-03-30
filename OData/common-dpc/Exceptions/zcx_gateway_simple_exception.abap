class ZCX_GATEWAY_SIMPLE_EXCEPTION definition
  public
  inheriting from CX_NO_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  data M_ERROR_KIND type CHAR1 .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !ERROR_KIND type CHAR1 optional .
  methods THROW_GATEWAY_EXCEPTION
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCX_GATEWAY_SIMPLE_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->M_ERROR_KIND = M_ERROR_KIND .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
m_error_kind = error_kind.
  endmethod.


  METHOD throw_gateway_exception.
    DATA(o_message_container) = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

    CASE m_error_kind.
      WHEN 'T'.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            message_container = o_message_container.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = o_message_container.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
