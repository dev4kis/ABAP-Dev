CLASS zcx_graph_api_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .
    CONSTANTS:
      BEGIN OF be_api_error_kind,
        business_error  TYPE char1 VALUE 'B',
        technical_error TYPE char1 VALUE 'T',
      END OF be_api_error_kind .
    DATA msgv1 TYPE syst_msgv .
    DATA msgv2 TYPE syst_msgv .
    DATA msgv3 TYPE syst_msgv .
    DATA msgv4 TYPE syst_msgv .
    DATA app_error TYPE int4 .
    DATA kind_of_error TYPE char1 .
    METHODS constructor
      IMPORTING
        !textid        LIKE if_t100_message=>t100key OPTIONAL
        !previous      LIKE previous OPTIONAL
        !msgv1         TYPE syst_msgv OPTIONAL
        !msgv2         TYPE syst_msgv OPTIONAL
        !msgv3         TYPE syst_msgv OPTIONAL
        !msgv4         TYPE syst_msgv OPTIONAL
        !app_error     TYPE int4 OPTIONAL
        !kind_of_error TYPE char1 OPTIONAL .
    CLASS-METHODS raise_error
      IMPORTING
        !i_text       TYPE string
        !i_text_id    TYPE scx_t100key OPTIONAL
        !i_app_error  TYPE int4
        !i_error_type TYPE char1
        !i_empty_text TYPE crmt_boolean OPTIONAL
        RAISING zcx_graph_api_exception.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_GRAPH_API_EXCEPTION IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->msgv1 = msgv1 .
    me->msgv2 = msgv2 .
    me->msgv3 = msgv3 .
    me->msgv4 = msgv4 .
    me->app_error = app_error .
    me->kind_of_error = kind_of_error.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD raise_error.
    DATA v_msgv1 TYPE symsgv.
    DATA v_msgv2 TYPE symsgv.
    DATA v_msgv3 TYPE symsgv.
    DATA v_msgv4 TYPE symsgv.

    TRY .

        v_msgv1 = i_text.
        v_msgv2 = i_text+50.
        v_msgv3 = i_text+100.
        v_msgv4 = i_text+150.

      CATCH cx_sy_range_out_of_bounds.
        DATA(v_ignore) = abap_true.
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
