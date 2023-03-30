class ZCX_REST_API_EXCEPTIONS definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .
  interfaces IF_T100_DYN_MSG .

  constants:
    BEGIN OF be_api_error_kind,
        business_error  TYPE char1 VALUE 'B',
        technical_error TYPE char1 VALUE 'T',
      END OF be_api_error_kind .
  data MSGV1 type SYST_MSGV .
  data MSGV2 type SYST_MSGV .
  data MSGV3 type SYST_MSGV .
  data MSGV4 type SYST_MSGV .
  data APP_ERROR type INT4 .
  data KIND_OF_ERROR type CHAR1 .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYST_MSGV optional
      !MSGV2 type SYST_MSGV optional
      !MSGV3 type SYST_MSGV optional
      !MSGV4 type SYST_MSGV optional
      !APP_ERROR type INT4 optional
      !KIND_OF_ERROR type CHAR1 optional .
  class-methods RAISE_ERROR
    importing
      !I_TEXT type STRING
      !I_TEXT_ID type SCX_T100KEY optional
      !I_APP_ERROR type INT4
      !I_ERROR_TYPE type CHAR1
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional
    raising
      ZCX_GRAPH_API_EXCEPTION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_REST_API_EXCEPTIONS IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
me->APP_ERROR = APP_ERROR .
me->KIND_OF_ERROR = KIND_OF_ERROR .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD RAISE_ERROR.
    DATA v_msgv1 TYPE symsgv.
    DATA v_msgv2 TYPE symsgv.
    DATA v_msgv3 TYPE symsgv.
    DATA v_msgv4 TYPE symsgv.
    DATA v_ignore TYPE abap_bool.

    TRY .

        v_msgv1 = i_text.
        v_msgv2 = i_text+50.
        v_msgv3 = i_text+100.
        v_msgv4 = i_text+150.

      CATCH cx_sy_range_out_of_bounds.
        v_ignore = abap_true.
    ENDTRY.


    RAISE EXCEPTION TYPE zcx_graph_api_exception
      EXPORTING
        textid        = zcx_crm_app=>default
        msgv1         = v_msgv1
        msgv2         = v_msgv2
        msgv3         = v_msgv3
        msgv4         = v_msgv4
        app_error     = i_app_error
        kind_of_error = i_error_type.
  ENDMETHOD.
ENDCLASS.
