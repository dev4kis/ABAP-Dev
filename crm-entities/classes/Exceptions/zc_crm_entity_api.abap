class ZCX_CRM_ENTITY_API definition
  public
  inheriting from CX_NO_CHECK
  create private .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

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
    BEGIN OF be_api_error_type,
                  not_supported  TYPE int4 VALUE 1,
                  existing       TYPE int4 VALUE 2,
                  failed 	       TYPE int4 VALUE 3,
                  inconsistent   TYPE int4 VALUE 4,
                  invalid        TYPE int4 VALUE 5,
                  internal_error TYPE int4 VALUE 6,
                  foreign_lock   TYPE int4 VALUE 7,
                  not_authorized TYPE int4 VALUE 8,
                  not_customized TYPE int4 VALUE 9,
                  not_found      TYPE int4 VALUE 10,
                  not_qualified  TYPE int4 VALUE 11,
                  system_error   TYPE int4 VALUE 12,
                END OF be_api_error_type .
  data MSGV1 type SYST_MSGV .
  data MSGV2 type SYST_MSGV .
  data MSGV3 type SYST_MSGV .
  data MSGV4 type SYST_MSGV .
  data APP_ERROR type INT4 .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYST_MSGV optional
      !MSGV2 type SYST_MSGV optional
      !MSGV3 type SYST_MSGV optional
      !MSGV4 type SYST_MSGV optional
      !APP_ERROR type INT4 optional .
    "! Unexpected system error. & & & &.
  class-methods RAISE_SYSTEM_ERROR
    importing
      !I_TEXT type CSEQUENCE optional .
    "! The combination of input parameters is insufficient to run the method.
  class-methods RAISE_NOT_QUALIFIED
    importing
      !I_TEXT type CSEQUENCE optional .
    "! Unable to find the requested object.
  class-methods raise_not_found
    importing
      !I_TEXT type CSEQUENCE optional .
    "! The object requested is not correctly customized.
  class-methods RAISE_NOT_CUSTOMIZED
    importing
      !I_TEXT type CSEQUENCE optional .
    "!The user does not have the required authorization.
  class-methods RAISE_NOT_AUTHORIZED
    importing
      !I_TEXT type CSEQUENCE optional .
    "!The new object that you want to create already exists. & & & &
  class-methods RAISE_EXISTING
    importing
      !I_TEXT type CSEQUENCE optional .
    "!The method could not be executed. & & & & failed.
  class-methods RAISE_FAILED
    importing
      !I_TEXT type CSEQUENCE optional .
    "!The requested & is not supported. & & &
  class-methods RAISE_NOT_SUPPORTED
    importing
      !I_TEXT type CSEQUENCE optional .
    "!The object data is inconsistent. & & & &
  class-methods RAISE_INCONSISTENT
    importing
      !I_TEXT type CSEQUENCE optional .
    "!The object data entered is incorrect. & & & & &
  class-methods RAISE_INVALID
    importing
      !I_TEXT type CSEQUENCE optional .
    "!Internal error occurs & & & &.
  class-methods RAISE_INTERNAL_ERROR
    importing
      !I_TEXT type CSEQUENCE optional .
    "! The data is locked by another user. & & & &
  class-methods RAISE_FOREIGN_LOCK
    importing
      !I_TEXT type CSEQUENCE optional .
  class-methods RAISE_MISSING_INPUT
    importing
      !I_INPUT_NAME type CSEQUENCE .
  class-methods RAISE_INVALID_VALUE
    importing
      !I_INPUT_NAME type CSEQUENCE
      !I_INPUT_VALUE type DATA .
protected section.
private section.

  class-methods RAISE_ERROR
    importing
      !I_TEXT type STRING
      !I_TEXT_ID type SCX_T100KEY
      !I_APP_ERROR type INT4 .
ENDCLASS.



CLASS ZCX_CRM_ENTITY_API IMPLEMENTATION.


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
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD raise_error.

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

    RAISE EXCEPTION TYPE zcx_crm_entity_api
      EXPORTING
        textid    = i_text_id
        msgv1     = v_msgv1
        msgv2     = v_msgv2
        msgv3     = v_msgv3
        msgv4     = v_msgv4
        app_error = i_app_error.

  ENDMETHOD.


  METHOD raise_existing.

    zcx_crm_entity_api=>raise_error( i_text    = i_text
                                     i_text_id = zcx_crm_entity_api=>existing
                                     i_app_error = zcx_crm_entity_api=>be_api_error_type-existing

                                 ).

  ENDMETHOD.


  METHOD raise_failed.


    zcx_crm_entity_api=>raise_error( i_text    = i_text
                                     i_text_id = zcx_crm_entity_api=>failed
                                     i_app_error = zcx_crm_entity_api=>be_api_error_type-failed
                                 ).

  ENDMETHOD.


  METHOD raise_foreign_lock.
    zcx_crm_entity_api=>raise_error( i_text    = i_text
                                     i_text_id = zcx_crm_entity_api=>foreign_lock
                                     i_app_error = zcx_crm_entity_api=>be_api_error_type-foreign_lock
                                   ).
  ENDMETHOD.


  METHOD raise_inconsistent.

    zcx_crm_entity_api=>raise_error( i_text    = i_text
                                   i_text_id = zcx_crm_entity_api=>inconsistent
                                   i_app_error = zcx_crm_entity_api=>be_api_error_type-inconsistent
                               ).


  ENDMETHOD.


  METHOD raise_internal_error.

    zcx_crm_entity_api=>raise_error( i_text     = i_text
                                    i_text_id   = zcx_crm_entity_api=>internal_error
                                    i_app_error = zcx_crm_entity_api=>be_api_error_type-internal_error
                                ).

  ENDMETHOD.


  METHOD raise_invalid.

    zcx_crm_entity_api=>raise_error( i_text    = i_text
                                    i_text_id = zcx_crm_entity_api=>invalid
                                    i_app_error = zcx_crm_entity_api=>be_api_error_type-invalid
                                ).

  ENDMETHOD.


  METHOD raise_invalid_value.

    RAISE EXCEPTION TYPE
      zcx_crm_entity_api
      EXPORTING
        textid = zcx_crm_entity_api=>zcx_crm_api_input
        msgv1  = i_input_name
        msgv2  = i_input_value.

  ENDMETHOD.


  METHOD raise_missing_input.

    RAISE EXCEPTION TYPE
      zcx_crm_entity_api
      EXPORTING
        textid = zcx_crm_entity_api=>zcx_crm_api_input
        msgv1  = |{ i_input_name }|.
  ENDMETHOD.


  METHOD raise_not_authorized.

    zcx_crm_entity_api=>raise_error( i_text    = i_text
                                     i_text_id = zcx_crm_entity_api=>not_authorized
                                     i_app_error = zcx_crm_entity_api=>be_api_error_type-not_authorized

                                   ).

  ENDMETHOD.


  METHOD raise_not_customized.


    zcx_crm_entity_api=>raise_error( i_text    = i_text
                                     i_text_id = zcx_crm_entity_api=>not_customized
                                     i_app_error = zcx_crm_entity_api=>be_api_error_type-not_customized
                                   ).


  ENDMETHOD.


  METHOD raise_not_found.


    zcx_crm_entity_api=>raise_error( i_text    = i_text
                                     i_text_id = zcx_crm_entity_api=>not_found
                                     i_app_error = zcx_crm_entity_api=>be_api_error_type-not_found
                                   ).

  ENDMETHOD.


  METHOD raise_not_qualified.
    zcx_crm_entity_api=>raise_error( i_text    = i_text
                                 i_text_id = zcx_crm_entity_api=>not_qualified
                                 i_app_error = zcx_crm_entity_api=>be_api_error_type-not_qualified
                               ).
  ENDMETHOD.


  METHOD raise_not_supported.

    zcx_crm_entity_api=>raise_error( i_text    = i_text
                                     i_text_id = zcx_crm_entity_api=>not_supported
                                     i_app_error = zcx_crm_entity_api=>be_api_error_type-not_supported

                                    ).



  ENDMETHOD.


  METHOD raise_system_error.

    zcx_crm_entity_api=>raise_error( i_text    = i_text
                                     i_text_id = zcx_crm_entity_api=>system_error
                                     i_app_error = zcx_crm_entity_api=>be_api_error_type-system_error
                             ).

  ENDMETHOD.
ENDCLASS.
