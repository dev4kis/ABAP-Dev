CLASS zcx_ldap_access DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF zcx_ldap_access,
        msgid TYPE symsgid VALUE 'ZBF_CUA',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ldap_access,

      BEGIN OF zcx_ldap_violation,
        msgid TYPE symsgid VALUE 'ZBF_CUA',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ldap_violation,

      BEGIN OF entry_already_exists,
        msgid TYPE symsgid VALUE 'ZMCL_LDAP',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF entry_already_exists,

      BEGIN OF duplicate_attribute_value,
        msgid TYPE symsgid VALUE 'ZMCL_LDAP',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF duplicate_attribute_value,

      BEGIN OF entry_not_found,
        msgid TYPE symsgid VALUE 'ZMCL_LDAP',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF entry_not_found,

      BEGIN OF attribute_not_defined,
        msgid TYPE symsgid VALUE 'ZMCL_LDAP',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF attribute_not_defined,

      BEGIN OF attribute_value_not_assigned,
        msgid TYPE symsgid VALUE 'ZMCL_LDAP',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF attribute_value_not_assigned,

      BEGIN OF not_authorized,
        msgid TYPE symsgid VALUE 'ZMCL_LDAP',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF not_authorized,


      BEGIN OF conn_outdated,
        msgid TYPE symsgid VALUE 'ZMCL_LDAP',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF conn_outdated,

      BEGIN OF ldap_failure,
        msgid TYPE symsgid VALUE 'ZMCL_LDAP',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF ldap_failure,

      BEGIN OF invalid_credentials,
        msgid TYPE symsgid VALUE 'ZMCL_LDAP',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_credentials
      .
    CONSTANTS:
      BEGIN OF be_api_error_type,
        not_supported    TYPE int4 VALUE 1,
        existing         TYPE int4 VALUE 2,
        failed           TYPE int4 VALUE 3,
        inconsistent     TYPE int4 VALUE 4,
        invalid          TYPE int4 VALUE 5,
        internal_error   TYPE int4 VALUE 6,
        not_authorized   TYPE int4 VALUE 8,
        not_customized   TYPE int4 VALUE 9,
        not_found        TYPE int4 VALUE 10,
        system_error     TYPE int4 VALUE 12,
        not_implemented  TYPE int4 VALUE 17,
        execution_failed TYPE int4 VALUE 18,
      END OF be_api_error_type .
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
    CLASS-DATA error_msg TYPE string .

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
    CLASS-METHODS:
      raise_not_authorized
        IMPORTING
                  !i_text       TYPE csequence OPTIONAL
                  !i_empty_text TYPE crmt_boolean OPTIONAL
        RAISING   zcx_ldap_access,
      raise_invalid_credentials
        IMPORTING
                  !i_text       TYPE csequence OPTIONAL
                  !i_empty_text TYPE crmt_boolean OPTIONAL
        RAISING   zcx_ldap_access,
      "! User not authorized for LDAP connection
      raise_conn_outdated
        IMPORTING
                  !i_text       TYPE csequence OPTIONAL
                  !i_empty_text TYPE crmt_boolean OPTIONAL
        RAISING   zcx_ldap_access,
      raise_duplicate_entry
        IMPORTING
                  !i_text       TYPE csequence OPTIONAL
                  !i_empty_text TYPE crmt_boolean OPTIONAL
        RAISING   zcx_ldap_access,
      raise_entry_not_found
        IMPORTING
                  !i_text       TYPE csequence OPTIONAL
                  !i_empty_text TYPE crmt_boolean OPTIONAL
        RAISING   zcx_ldap_access,

      raise_ldap_violation
        IMPORTING
                  !i_text       TYPE csequence OPTIONAL
                  !i_empty_text TYPE crmt_boolean OPTIONAL
        RAISING   zcx_ldap_access,
      raise_dupl_attr_val_pair

        IMPORTING
                  !i_text       TYPE csequence OPTIONAL
                  !i_empty_text TYPE crmt_boolean OPTIONAL
        RAISING   zcx_ldap_access,
      raise_undefined_attribute
        IMPORTING
                  !i_text       TYPE csequence OPTIONAL
                  !i_empty_text TYPE crmt_boolean OPTIONAL
        RAISING   zcx_ldap_access,
      raise_unassigned_attribute
        IMPORTING
                  !i_text       TYPE csequence OPTIONAL
                  !i_empty_text TYPE crmt_boolean OPTIONAL
        RAISING   zcx_ldap_access,

      raise_ldap_failure
        IMPORTING
                  !i_text       TYPE csequence OPTIONAL
                  !i_empty_text TYPE crmt_boolean OPTIONAL
        RAISING   zcx_ldap_access
        .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS: raise_error
      IMPORTING
                i_text       TYPE csequence
                i_text_id    TYPE scx_t100key OPTIONAL
                i_app_error  TYPE i
                i_error_type TYPE char1
                i_empty_text TYPE crmt_boolean OPTIONAL
      RAISING   zcx_ldap_access
      .
ENDCLASS.



CLASS ZCX_LDAP_ACCESS IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->error_msg = error_msg .
    me->msgv1 = msgv1 .
    me->msgv2 = msgv2 .
    me->msgv3 = msgv3 .
    me->msgv4 = msgv4 .

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_ldap_access .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
*    IF error_msg IS INITIAL.
*      me->error_msg = me->get_text( ).
*    ENDIF.
  ENDMETHOD.


  METHOD raise_conn_outdated.
    zcx_ldap_access=>raise_error( i_text     = i_text
                                       i_text_id    = zcx_ldap_access=>conn_outdated
                                       i_app_error  = zcx_ldap_access=>be_api_error_type-system_error
                                       i_error_type = zcx_crm_app=>be_api_error_kind-technical_error
                                       i_empty_text = i_empty_text
                                      ).
  ENDMETHOD.


  METHOD raise_duplicate_entry.
    zcx_ldap_access=>raise_error( i_text           = i_text
                                         i_text_id        = zcx_ldap_access=>entry_already_exists
                                         i_app_error      = zcx_ldap_access=>be_api_error_type-existing
                                         i_error_type     = zcx_crm_app=>be_api_error_kind-business_error
                                         i_empty_text     = i_empty_text
                                          ).
  ENDMETHOD.


  METHOD raise_dupl_attr_val_pair.
    zcx_ldap_access=>raise_error( i_text         = i_text
                                         i_text_id      = zcx_ldap_access=>duplicate_attribute_value
                                         i_app_error    = zcx_ldap_access=>be_api_error_type-existing
                                         i_error_type   = zcx_crm_app=>be_api_error_kind-business_error
                                         i_empty_text   = i_empty_text
                                        ).
  ENDMETHOD.


  METHOD raise_entry_not_found.
    zcx_ldap_access=>raise_error( i_text         = i_text
                                         i_text_id      = zcx_ldap_access=>entry_not_found
                                         i_app_error    = zcx_ldap_access=>be_api_error_type-not_found
                                         i_error_type   = zcx_crm_app=>be_api_error_kind-business_error
                                         i_empty_text   = i_empty_text
                                             ).
  ENDMETHOD.


  METHOD raise_error.
    DATA v_msgv1 TYPE symsgv.
    DATA v_msgv2 TYPE symsgv.
    DATA v_msgv3 TYPE symsgv.
    DATA v_msgv4 TYPE symsgv.
    error_msg = i_text.
    TRY .

        v_msgv1 = i_text.
        v_msgv2 = i_text+50.
        v_msgv3 = i_text+100.
        v_msgv4 = i_text+150.

      CATCH cx_sy_range_out_of_bounds.
        DATA(v_no_impact) = abap_true.
    ENDTRY.

    IF i_empty_text = abap_true.
      RAISE EXCEPTION TYPE zcx_ldap_access
        EXPORTING
          textid        = zcx_crm_app=>default
          msgv1         = v_msgv1
          msgv2         = v_msgv2
          msgv3         = v_msgv3
          msgv4         = v_msgv4
          app_error     = i_app_error
          kind_of_error = i_error_type.
    ELSE.
      RAISE EXCEPTION TYPE zcx_ldap_access
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


  METHOD raise_invalid_credentials.
    zcx_ldap_access=>raise_error( i_text    = i_text
                                  i_text_id = zcx_ldap_access=>invalid_credentials
                                  i_app_error = zcx_ldap_access=>be_api_error_type-system_error
                                  i_error_type = zcx_crm_app=>be_api_error_kind-technical_error
                                  i_empty_text = i_empty_text
                                ).
  ENDMETHOD.


  METHOD raise_ldap_failure.
    zcx_ldap_access=>raise_error( i_text         = i_text
                                         i_text_id      = zcx_ldap_access=>ldap_failure
                                         i_app_error    = zcx_ldap_access=>be_api_error_type-failed
                                         i_error_type   = zcx_crm_app=>be_api_error_kind-technical_error
                                         i_empty_text   = i_empty_text
                                               ).
  ENDMETHOD.


  METHOD raise_ldap_violation.
    zcx_ldap_access=>raise_error( i_text         = i_text
                                         i_text_id      = zcx_ldap_access=>zcx_ldap_violation
                                         i_app_error    = zcx_ldap_access=>be_api_error_type-not_found
                                         i_error_type   = zcx_crm_app=>be_api_error_kind-business_error
                                         i_empty_text   = i_empty_text
                                             ).
  ENDMETHOD.


  METHOD raise_not_authorized.
    zcx_ldap_access=>raise_error( i_text    = i_text
                                  i_text_id = zcx_ldap_access=>not_authorized
                                  i_app_error = zcx_ldap_access=>be_api_error_type-system_error
                                  i_error_type = zcx_crm_app=>be_api_error_kind-technical_error
                                  i_empty_text = i_empty_text
                                ).
  ENDMETHOD.


  METHOD raise_unassigned_attribute.
    zcx_ldap_access=>raise_error( i_text         = i_text
                                           i_text_id      = zcx_ldap_access=>attribute_value_not_assigned
                                           i_app_error    = zcx_ldap_access=>be_api_error_type-not_found
                                           i_error_type   = zcx_crm_app=>be_api_error_kind-business_error
                                           i_empty_text   = i_empty_text
                                                 ).
  ENDMETHOD.


  METHOD raise_undefined_attribute.
    zcx_ldap_access=>raise_error( i_text         = i_text
                                         i_text_id      = zcx_ldap_access=>attribute_not_defined
                                         i_app_error    = zcx_ldap_access=>be_api_error_type-not_found
                                         i_error_type   = zcx_crm_app=>be_api_error_kind-business_error
                                         i_empty_text   = i_empty_text
                                               ).
  ENDMETHOD.
ENDCLASS.
