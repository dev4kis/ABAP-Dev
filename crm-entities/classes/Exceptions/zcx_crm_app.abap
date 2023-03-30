class ZCX_CRM_APP definition
  public
  inheriting from CX_NO_CHECK
  create private .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF default,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF default .
  constants:
    BEGIN OF zcx_crm_api_input,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_crm_api_input .
  constants:
    BEGIN OF not_supported,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF not_supported .
  constants:
    BEGIN OF existing,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF existing .
  constants:
    BEGIN OF failed,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF failed .
  constants:
    BEGIN OF inconsistent,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF inconsistent .
  constants:
    BEGIN OF invalid,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF invalid .
  constants:
    BEGIN OF internal_error,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF internal_error .
  constants:
    BEGIN OF foreign_lock,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF foreign_lock .
  constants:
    BEGIN OF not_authorized,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF not_authorized .
  constants:
    BEGIN OF not_customized,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF not_customized .
  constants:
    BEGIN OF not_found,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF not_found .
  constants:
    BEGIN OF not_qualified,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF not_qualified .
  constants:
    BEGIN OF system_error,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF system_error .
  constants:
    BEGIN OF not_implemented,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF not_implemented .
  constants:
    BEGIN OF execution_failed,
        msgid TYPE symsgid VALUE 'ZCRM_BE_API',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF execution_failed .
  constants:
    BEGIN OF be_api_error_type,
        not_supported    TYPE int4 VALUE 1,
        existing         TYPE int4 VALUE 2,
        failed 	         TYPE int4 VALUE 3,
        inconsistent     TYPE int4 VALUE 4,
        invalid          TYPE int4 VALUE 5,
        internal_error   TYPE int4 VALUE 6,
        foreign_lock     TYPE int4 VALUE 7,
        not_authorized   TYPE int4 VALUE 8,
        not_customized   TYPE int4 VALUE 9,
        not_found        TYPE int4 VALUE 10,
        not_qualified    TYPE int4 VALUE 11,
        system_error     TYPE int4 VALUE 12,
        not_implemented  TYPE int4 VALUE 17,
        execution_failed TYPE int4 VALUE 18,
      END OF be_api_error_type .
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
    "! Unexpected system error. & & & &.
  class-methods RAISE_SYSTEM_ERROR
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
    "! The combination of input parameters is insufficient to run the method.
  class-methods RAISE_NOT_QUALIFIED
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
    "! Unable to find the requested object.
  class-methods RAISE_NOT_FOUND
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
    "! The object requested is not correctly customized.
  class-methods RAISE_NOT_CUSTOMIZED
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
    "!The user does not have the required authorization.
  class-methods RAISE_NOT_AUTHORIZED
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
    "!The new object that you want to create already exists. & & & &
  class-methods RAISE_EXISTING
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
    "!The method could not be executed. & & & & failed.
  class-methods RAISE_FAILED
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
    "!The requested & is not supported. & & &
  class-methods RAISE_NOT_SUPPORTED
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
  class-methods RAISE_EXEC_FAILED
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
    "!Method is not implemented. & & & &
  class-methods RAISE_NOT_IMPLEMENTED
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
    "!The object data is inconsistent. & & & &
  class-methods RAISE_INCONSISTENT
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
    "!The object data entered is incorrect. & & & & &
  class-methods RAISE_INVALID
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
    "!Internal error occurs & & & &.
  class-methods RAISE_INTERNAL_ERROR
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
    "! The data is locked by another user. & & & &
  class-methods RAISE_FOREIGN_LOCK
    importing
      !I_TEXT type CSEQUENCE optional
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
  class-methods RAISE_MISSING_INPUT
    importing
      !I_INPUT_NAME type CSEQUENCE
      !I_EMPTY_TEXT type CRMT_BOOLEAN optional .
  class-methods RAISE_INVALID_VALUE
    importing
      !I_INPUT_NAME type CSEQUENCE
      !I_INPUT_VALUE type DATA .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS raise_error
      IMPORTING
        !i_text       TYPE string
        !i_text_id    TYPE scx_t100key OPTIONAL
        !i_app_error  TYPE int4
        !i_error_type TYPE char1
        !i_empty_text TYPE crmt_boolean OPTIONAL.



ENDCLASS.



CLASS ZCX_CRM_APP IMPLEMENTATION.


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
    ENDTRY.

    IF i_empty_text = abap_true.
      RAISE EXCEPTION TYPE zcx_crm_app
        EXPORTING
          textid        = zcx_crm_app=>default
          msgv1         = v_msgv1
          msgv2         = v_msgv2
          msgv3         = v_msgv3
          msgv4         = v_msgv4
          app_error     = i_app_error
          kind_of_error = i_error_type.
    ELSE.
      RAISE EXCEPTION TYPE zcx_crm_app
        EXPORTING
          textid        = i_text_id
          msgv1         = v_msgv1
          msgv2         = v_msgv2
          msgv3         = v_msgv3
          msgv4         = v_msgv4
          app_error     = i_app_error
          kind_of_error = i_error_type.
    ENDIF.
  ENDMETHOD.


  METHOD raise_exec_failed.

    zcx_crm_app=>raise_error( i_text    = i_text
                              i_text_id = zcx_crm_app=>execution_failed
                              i_app_error = zcx_crm_app=>be_api_error_type-execution_failed
                              i_error_type = zcx_crm_app=>be_api_error_kind-business_error
                              i_empty_text = i_empty_text
                            ).

  ENDMETHOD.


  METHOD raise_existing.

    zcx_crm_app=>raise_error(  i_text       = i_text
                               i_text_id    = zcx_crm_app=>existing
                               i_app_error  = zcx_crm_app=>be_api_error_type-existing
                               i_error_type = zcx_crm_app=>be_api_error_kind-technical_error
                               i_empty_text = i_empty_text
                             ).

  ENDMETHOD.


  METHOD raise_failed.


    zcx_crm_app=>raise_error( i_text    = i_text
                              i_text_id = zcx_crm_app=>failed
                              i_app_error = zcx_crm_app=>be_api_error_type-failed
                              i_error_type = zcx_crm_app=>be_api_error_kind-technical_error
                              i_empty_text = i_empty_text
                            ).

  ENDMETHOD.


  METHOD raise_foreign_lock.
    zcx_crm_app=>raise_error( i_text    = i_text
                              i_text_id = zcx_crm_app=>foreign_lock
                              i_app_error = zcx_crm_app=>be_api_error_type-foreign_lock
                              i_error_type = zcx_crm_app=>be_api_error_kind-technical_error
                              i_empty_text = i_empty_text
                            ).
  ENDMETHOD.


  METHOD raise_inconsistent.
    zcx_crm_app=>raise_error(      i_text    = i_text
                                   i_text_id = zcx_crm_app=>inconsistent
                                   i_app_error = zcx_crm_app=>be_api_error_type-inconsistent
                                   i_error_type = zcx_crm_app=>be_api_error_kind-business_error
                                   i_empty_text = i_empty_text
                               ).


  ENDMETHOD.


  METHOD raise_internal_error.

    zcx_crm_app=>raise_error( i_text     = i_text
                              i_text_id   = zcx_crm_app=>default
                              i_app_error = zcx_crm_app=>be_api_error_type-internal_error
                              i_error_type = zcx_crm_app=>be_api_error_kind-technical_error
                              i_empty_text = i_empty_text
                            ).

  ENDMETHOD.


  METHOD raise_invalid.

    zcx_crm_app=>raise_error( i_text    = i_text
                              i_text_id = zcx_crm_app=>invalid
                              i_app_error = zcx_crm_app=>be_api_error_type-invalid
                              i_error_type = zcx_crm_app=>be_api_error_kind-business_error
                              i_empty_text = i_empty_text
                            ).

  ENDMETHOD.


  METHOD raise_invalid_value.

    RAISE EXCEPTION TYPE
      zcx_crm_app
      EXPORTING
        textid = zcx_crm_app=>zcx_crm_api_input
        msgv1  = i_input_name
        msgv2  = i_input_value.

  ENDMETHOD.


  METHOD raise_missing_input.

    RAISE EXCEPTION TYPE
      zcx_crm_app
      EXPORTING
        textid = zcx_crm_app=>zcx_crm_api_input
        msgv1  = |{ i_input_name }|.
  ENDMETHOD.


  METHOD raise_not_authorized.

    zcx_crm_app=>raise_error(  i_text    = i_text
                               i_text_id = zcx_crm_app=>not_authorized
                               i_app_error = zcx_crm_app=>be_api_error_type-not_authorized
                               i_error_type = zcx_crm_app=>be_api_error_kind-technical_error
                               i_empty_text = i_empty_text
                                   ).

  ENDMETHOD.


  METHOD raise_not_customized.


    zcx_crm_app=>raise_error( i_text    = i_text
                              i_text_id = zcx_crm_app=>not_customized
                              i_app_error = zcx_crm_app=>be_api_error_type-not_customized
                              i_error_type = zcx_crm_app=>be_api_error_kind-technical_error
                              i_empty_text = i_empty_text

                           ).


  ENDMETHOD.


  METHOD raise_not_found.


    zcx_crm_app=>raise_error(  i_text    = i_text
                               i_text_id = zcx_crm_app=>not_found
                               i_app_error = zcx_crm_app=>be_api_error_type-not_found
                               i_error_type = zcx_crm_app=>be_api_error_kind-technical_error
                               i_empty_text = i_empty_text
                            ).

  ENDMETHOD.


  METHOD raise_not_implemented.

    zcx_crm_app=>raise_error( i_text        = i_text
                             i_text_id      = zcx_crm_app=>not_implemented
                             i_app_error    = zcx_crm_app=>be_api_error_type-not_implemented
                             i_error_type   = zcx_crm_app=>be_api_error_kind-technical_error
                             i_empty_text   = i_empty_text
                           ).

  ENDMETHOD.


  METHOD raise_not_qualified.
    zcx_crm_app=>raise_error(   i_text    = i_text
                                i_text_id = zcx_crm_app=>not_qualified
                                i_app_error = zcx_crm_app=>be_api_error_type-not_qualified
                                i_error_type = zcx_crm_app=>be_api_error_kind-technical_error
                                i_empty_text = i_empty_text
                             ).
  ENDMETHOD.


  METHOD raise_not_supported.

    zcx_crm_app=>raise_error(        i_text    = i_text
                                     i_text_id = zcx_crm_app=>not_supported
                                     i_app_error = zcx_crm_app=>be_api_error_type-not_supported
                                     i_error_type = zcx_crm_app=>be_api_error_kind-technical_error
                                     i_empty_text = i_empty_text
                            ).



  ENDMETHOD.


  METHOD raise_system_error.

    zcx_crm_app=>raise_error( i_text    = i_text
                               i_text_id = zcx_crm_app=>system_error
                               i_app_error = zcx_crm_app=>be_api_error_type-system_error
                               i_error_type = zcx_crm_app=>be_api_error_kind-technical_error
                               i_empty_text = i_empty_text
                             ).

  ENDMETHOD.
ENDCLASS.
