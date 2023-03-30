CLASS zcl_ad_ldap_access DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ldap_access_api .

    DATA basedn TYPE ldap_dn .

*                                   i_logon_user TYPE ldap_usr OPTIONAL
*                                   i_logon_pwd  TYPE ldap_pwd OPTIONAL
    METHODS constructor
      IMPORTING
        !i_server_id TYPE ldap_serv .
  PROTECTED SECTION.
    METHODS: validate_credentials IMPORTING
                                            i_user           TYPE ldap_usr
                                            i_pwd            TYPE ldap_pwd
                                  RETURNING VALUE(pwd_valid) TYPE abap_bool
                                  RAISING   zcx_ldap_access.
  PRIVATE SECTION.
    DATA: gv_server_id TYPE ldap_serv
*          gv_user      TYPE ldap_usr,
*          gv_pwd       TYPE ldap_pwd

          .
    METHODS: connect  IMPORTING i_logon_user TYPE ldap_usr OPTIONAL
                                i_logon_pwd  TYPE ldap_pwd OPTIONAL
                      RETURNING VALUE(rc)    TYPE int4
                      RAISING   zcx_ldap_access,
      disconnect
        RAISING zcx_ldap_access,

      check_and_throw_exc
        IMPORTING
                  i_ldaprc         TYPE ldap_rc
                  i_obj_ldaprc_exc TYPE REF TO cx_ldap_client_open_server_err OPTIONAL
        RAISING   zcx_ldap_access.
ENDCLASS.



CLASS ZCL_AD_LDAP_ACCESS IMPLEMENTATION.


  METHOD check_and_throw_exc.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          zcx_ldap_access=>raise_not_authorized( ).

        WHEN OTHERS.
          zcx_ldap_access=>raise_ldap_failure( ).

      ENDCASE.

    ELSEIF i_obj_ldaprc_exc IS NOT INITIAL.

      CASE i_ldaprc. " Standard LDAP RCs
        WHEN 16.
          zcx_ldap_access=>raise_unassigned_attribute( i_text = i_obj_ldaprc_exc->get_text( ) ).

        WHEN 17.
          zcx_ldap_access=>raise_undefined_attribute( i_text = i_obj_ldaprc_exc->get_text( ) ).

        WHEN 32.
          zcx_ldap_access=>raise_entry_not_found( i_text = i_obj_ldaprc_exc->get_text( ) ).

        WHEN 19 OR 21 OR 34.
          zcx_ldap_access=>raise_ldap_violation( i_text = i_obj_ldaprc_exc->get_text( ) ).

        WHEN 49.
          zcx_ldap_access=>raise_invalid_credentials( i_text = i_obj_ldaprc_exc->get_text( ) ).

        WHEN 68.
          zcx_ldap_access=>raise_duplicate_entry( i_text = i_obj_ldaprc_exc->get_text( ) ).


        WHEN OTHERS.

          zcx_ldap_access=>raise_ldap_failure( i_text = i_obj_ldaprc_exc->get_text( ) ).
      ENDCASE.


    ELSEIF i_ldaprc <> 0.

      CASE i_ldaprc. " Standard LDAP RCs
        WHEN 16.
          zcx_ldap_access=>raise_unassigned_attribute( i_text = || ).

        WHEN 17.
          zcx_ldap_access=>raise_undefined_attribute( i_text = || ).

        WHEN 19 OR 21 OR 34.
          zcx_ldap_access=>raise_ldap_violation( i_text = || ).
        WHEN 32.
          zcx_ldap_access=>raise_entry_not_found( i_text = || ).
        WHEN 49.
          zcx_ldap_access=>raise_not_authorized( i_text = || ).
        WHEN 68.
          zcx_ldap_access=>raise_duplicate_entry( i_text = || ).
        WHEN OTHERS.
          zcx_ldap_access=>raise_ldap_failure( i_text = || ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD connect.

    DATA: v_ldaprc         TYPE ldap_rc,
          v_ldap_dn        TYPE ldap_dn,
          v_basedn_string  TYPE  ldap_dns,
          v_ldap_connector TYPE  ldap_gate,
          v_hold           TYPE ldap_hold,
          obj_ldaprc_exc   TYPE REF TO
               cx_ldap_client_open_server_err.
    IF i_logon_pwd IS NOT INITIAL AND i_logon_user IS NOT INITIAL.

      CALL FUNCTION 'LDAP_SIMPLEBIND'
        EXPORTING
          serverid       = gv_server_id
          usr            = i_logon_user
          pwd            = i_logon_pwd
*         manage_dsa_it  = manage_dsa_it
        IMPORTING
          ldaprc         = v_ldaprc
          ldap_connector = v_ldap_connector
          ldaprc_exc     = obj_ldaprc_exc
        EXCEPTIONS
          no_authoriz    = 1
          config_error   = 2
          nomore_conns   = 3
          ldap_failure   = 4
          not_alive      = 5
          other_error    = 6
          OTHERS         = 7 ##fm_subrc_ok.
    ELSE.
      CALL FUNCTION 'LDAP_SYSTEMBIND'
        EXPORTING
          serverid       = gv_server_id               " LDAP Server: Symbolic Name
          writeread      = 'W'
          wait_time      = 0
          manage_dsa_it  = abap_false
        IMPORTING
          ldaprc         = v_ldaprc
          basedn         = basedn
          basedn_string  = v_basedn_string                " Distinguished Name (String)
          ldap_connector = v_ldap_connector
          ldaprc_exc     = obj_ldaprc_exc
        CHANGING
          holdsess       = v_hold              " Hold connection (seconds)
        EXCEPTIONS
          no_authoriz    = 1                " No Authorization
          config_error   = 2
          nomore_conns   = 3
          ldap_failure   = 4
          not_alive      = 5
          other_error    = 6
          OTHERS         = 7.
    ENDIF.

    CHECK sy-subrc <> 0 OR v_ldaprc <> 0.

    check_and_throw_exc(
      i_ldaprc         = v_ldaprc
      i_obj_ldaprc_exc = obj_ldaprc_exc ).

    rc = 0.

  ENDMETHOD.


  METHOD constructor.
    gv_server_id = i_server_id.
*    gv_user      = i_logon_user.
*    gv_pwd       = i_logon_pwd.

*    me->connect_to_ldap_server( ).
  ENDMETHOD.


  METHOD disconnect.
    DATA: v_ldaprc TYPE ldap_rc
          .

    CALL FUNCTION 'LDAP_UNBIND'
      IMPORTING
        ldaprc       = v_ldaprc
      EXCEPTIONS
        conn_outdate = 1                " Old connection
        ldap_failure = 2
        not_alive    = 3
        other_error  = 4
        OTHERS       = 5.



    CHECK sy-subrc <> 0 OR v_ldaprc <> 0.
*    check_and_throw_exc( i_ldaprc = v_ldaprc ).

  ENDMETHOD.


  METHOD validate_credentials.
    DATA(v_rc) = me->connect(  i_logon_user = i_user
                  i_logon_pwd  = i_pwd
    ).
    CHECK v_rc = 0.
    pwd_valid = abap_true.
    me->disconnect( ).
  ENDMETHOD.


  METHOD zif_ldap_access_api~add_attribute.
  ENDMETHOD.


  METHOD zif_ldap_access_api~create_user.
    me->connect( ).

    DATA: v_ldaprc       TYPE ldap_rc,
*          v_ldap_dn        TYPE ldap_dn,
*          v_basedn_string  TYPE  ldap_dns,
*          v_ldap_connector TYPE  ldap_gate,
*          v_hold           TYPE ldap_hold,
          obj_ldaprc_exc TYPE REF TO
               cx_ldap_client_open_server_err.

    DATA v_ldap_data TYPE ldape.


    CALL FUNCTION 'LDAP_CREATE'
      EXPORTING
        entry        = i_ldap_data
*       LDAP_UPDATE  = ABAP_FALSE
      IMPORTING
        ldaprc       = v_ldaprc
        ldaprc_exc   = obj_ldaprc_exc
      EXCEPTIONS
        no_authoriz  = 1
        conn_outdate = 2
        param_error  = 3
        ldap_failure = 4
        hexval_error = 5
        not_alive    = 6
        other_error  = 7
        OTHERS       = 8.


    IF sy-subrc <> 0 OR v_ldaprc <> 0.
      me->disconnect( ).
      check_and_throw_exc(
        EXPORTING
          i_ldaprc         = v_ldaprc
          i_obj_ldaprc_exc = obj_ldaprc_exc
      ).
    ENDIF.
    me->disconnect( ).

  ENDMETHOD.


  METHOD zif_ldap_access_api~delete_user.

    me->connect(  ).
    DATA: v_ldaprc       TYPE ldap_rc,
          obj_ldaprc_exc TYPE REF TO
                  cx_ldap_client_open_server_err.

    CALL FUNCTION 'LDAP_DELETE'
      EXPORTING
        dn           = i_distinguished_name
*       DN_STRING    =
*       SUBTREE      = ' '
      IMPORTING
        ldaprc       = v_ldaprc
        ldaprc_exc   = obj_ldaprc_exc
      EXCEPTIONS
        no_authoriz  = 1
        conn_outdate = 2
        ldap_failure = 3
        not_alive    = 4
        other_error  = 5
        OTHERS       = 6.

    IF sy-subrc <> 0 OR v_ldaprc <> 0.
      me->disconnect( ).
      check_and_throw_exc(
        EXPORTING
          i_ldaprc         = v_ldaprc
    i_obj_ldaprc_exc = obj_ldaprc_exc
      ).
    ENDIF.
    me->disconnect( ).

  ENDMETHOD.


  METHOD zif_ldap_access_api~read_user_attributes.
    DATA: v_rc TYPE ldap_rc
          .
    me->connect( ).
    CALL FUNCTION 'LDAP_READ'
      EXPORTING
*       base          = basedn
**        base_string   =  'ou=users,ou=KIRReal,o=DEUTSCHEBOERSE' "conv ldap_dns( basedn )
        attributes    = i_attributes
*       scope         = i_scope
*       FILTER        = i_filter_string
        filter_string = i_filter_string
*       TIMEOUT       =
*
      IMPORTING
        ldaprc        = v_rc
        entries       = r_attributes
      EXCEPTIONS
        no_authoriz   = 1
        conn_outdate  = 2
        ldap_failure  = 3
        not_alive     = 4
        other_error   = 5
        OTHERS        = 6.
    IF sy-subrc <> 0 OR v_rc <> 0.
      me->disconnect( ).
      check_and_throw_exc(
        EXPORTING
          i_ldaprc         = v_rc
*    i_obj_ldaprc_exc =
      ).
    ENDIF.
    me->disconnect( ).
*CATCH zcx_ldap_access. " general LDAP Excpetion
*    CHECK r_attributes IS INITIAL.
*    zcx_ldap_access=>raise_entry_not_found(  EXPORTING i_text  = |Account not found in LDAP - search filter { i_filter_string }|  ).

  ENDMETHOD.


  METHOD zif_ldap_access_api~update_attributes.
    DATA v_rc TYPE ldap_rc.
    me->connect( ).
    CALL FUNCTION 'LDAP_UPDATE'
      EXPORTING
        entry        = i_ldap_data2change
      IMPORTING
        ldaprc       = v_rc
      EXCEPTIONS
        no_authoriz  = 1
        conn_outdate = 2
        param_error  = 3
        ldap_failure = 4
        hexval_error = 5
        not_alive    = 6
        other_error  = 7
        OTHERS       = 8.

    IF sy-subrc <> 0 OR v_rc <> 0.
       me->disconnect( ).
      check_and_throw_exc( i_ldaprc = v_rc ).
    ENDIF.
    me->disconnect( ).
  ENDMETHOD.
ENDCLASS.
