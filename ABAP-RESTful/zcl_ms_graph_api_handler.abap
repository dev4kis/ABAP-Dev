CLASS zcl_ms_graph_api_handler DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ms_graph_api .

    TYPES:
      BEGIN OF ty_gr_api_details,
        code    TYPE string,
        message TYPE string,
      END OF ty_gr_api_details .
    TYPES:
      BEGIN OF ty_graph_api_error,
        error TYPE ty_gr_api_details,
      END OF ty_graph_api_error .
  PROTECTED SECTION.
    DATA: rest_api       TYPE REF TO zcl_abap_rest_framework,
          g_access_token TYPE string
          .
    DATA: BEGIN OF b2c_tenant_variables,
            aad_http_dest   TYPE rfc_dest,
            graph_http_dest TYPE rfc_dest,
            aad_ext_prefix  TYPE string,
            client_secret   TYPE string,
            client_id       TYPE string,
            tenant   TYPE string,
          END OF b2c_tenant_variables.
    METHODS:
      init_customizing_attributes
      .
    METHODS close.
  PRIVATE SECTION.
    METHODS set_auth_header.
    METHODS set_content_type_hdr
      IMPORTING i_content_type TYPE string.
    METHODS raise_exception
      RAISING
        zcx_graph_api_exception.
ENDCLASS.



CLASS ZCL_MS_GRAPH_API_HANDLER IMPLEMENTATION.


  METHOD close.
    TRY.
        me->rest_api->close( ).
      CATCH zcx_rest_api_exceptions INTO DATA(v_exception).
        zcx_graph_api_exception=>raise_error(
            EXPORTING
              i_text       = v_exception->get_text( )

              i_app_error  = 4
              i_error_type = zcx_graph_api_exception=>be_api_error_kind-technical_error

          ).
    ENDTRY.
  ENDMETHOD.


  METHOD init_customizing_attributes.

  ENDMETHOD.


  METHOD raise_exception.

    DATA  v_error    TYPE ty_graph_api_error.
    me->rest_api->get_error_data( IMPORTING e_error_data = v_error ) .
    zcx_graph_api_exception=>raise_error(
      EXPORTING
        i_text       = v_error-error-message
*              i_text_id    =
        i_app_error  = 4
        i_error_type = zcx_graph_api_exception=>be_api_error_kind-technical_error
*              i_empty_text =
    ).
*    RAISE EXCEPTION TYPE zcx_graph_api_exception.

  ENDMETHOD.


  METHOD set_auth_header.

    me->rest_api->set_request_header(
            EXPORTING
              iv_name  =     |Authorization|             " HTTP Framework (iHTTP) HTTP Name
              iv_value = |Bearer { g_access_token } | "v_token                 " HTTP Framework (iHTTP) HTTP Value
          ).

  ENDMETHOD.


  METHOD set_content_type_hdr.

    me->rest_api->set_request_header(
          EXPORTING
            iv_name  =  'Content-Type'                " HTTP Framework (iHTTP) HTTP Name
            iv_value =  i_content_type                " HTTP Framework (iHTTP) HTTP Value
        ).

  ENDMETHOD.


  METHOD zif_ms_graph_api~add_user_to_group.
    TYPES: BEGIN OF ty_az_dir_obj,
             id TYPE string,
           END OF ty_az_dir_obj.
    DATA: v_attempts TYPE i,
          v_response TYPE zrest_api_response
          .
    DATA(v_max_tries) = 1.
    CHECK i_user_id IS NOT INITIAL
    AND i_grp IS NOT INITIAL.

    set_auth_header( ).
    set_content_type_hdr( i_content_type = if_rest_media_type=>gc_appl_json  ).

    me->rest_api->set_uri( i_uri = |/groups/{ i_grp }/members/$ref| ). " move to and read from customizing

    DATA(v_user_obj_json_str) = |\{ | &&
                       |"@odata.id": "https://graph.microsoft.com/v1.0/directoryObjects/{ i_user_id }"| &&
                  | \}|.
*    DATA(v_user_obj) = VALUE ty_az_dir_obj( id = |https://graph.microsoft.com/v1.0/directoryObjects/{ i_user_id }| ).


*    DATA(v_data_json) = /ui2/cl_json=>serialize( data = v_ compress = abap_true
*                                                 pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
    me->rest_api->set_binary_body( body = cl_abap_codepage=>convert_to( v_user_obj_json_str ) ).
*    me->rest_api->set_string_body( body = v_data_json ).
    WHILE v_response-status_code <> cl_rest_status_code=>gc_success_no_content.
      TRY .

          v_response =  me->rest_api->execute( method = if_rest_message=>gc_method_post ).
        CATCH zcx_rest_api_exceptions INTO DATA(v_exception).
          zcx_graph_api_exception=>raise_error(
              EXPORTING
                i_text       = v_exception->get_text( )

                i_app_error  = 4
                i_error_type = zcx_graph_api_exception=>be_api_error_kind-technical_error

            ).
      ENDTRY.
      IF v_response-status_code = cl_rest_status_code=>gc_success_ok
                OR v_response-status_code = cl_rest_status_code=>gc_success_no_content.
        me->rest_api->get_response_data( ).
      ELSE.
        IF v_attempts = v_max_tries.
          raise_exception( ).
        ENDIF.

      ENDIF.
      v_attempts = v_attempts + 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD zif_ms_graph_api~create_user.


    set_auth_header( ).

    set_content_type_hdr( i_content_type = if_rest_media_type=>gc_appl_json  ).

    me->rest_api->set_uri( i_uri = |/users| ). " move to and read from customizing

    CHECK i_user_details IS NOT INITIAL
    OR i_user_details_json IS NOT INITIAL
    OR i_user_details_x IS NOT INITIAL.

    IF i_user_details_x IS NOT INITIAL.
      me->rest_api->set_binary_body( body = i_user_details_x ).
    ELSE.
      IF i_user_details IS NOT INITIAL.
        DATA(v_data_json) = /ui2/cl_json=>serialize( data = i_user_details compress = abap_true
                                                     pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
      ELSEIF i_user_details_json IS NOT INITIAL.
        v_data_json = i_user_details_json.
      ENDIF.
      me->rest_api->set_string_body( body = v_data_json ).
    ENDIF.
    TRY.
        DATA(v_response)  = me->rest_api->execute( method = if_rest_message=>gc_method_post ).
      CATCH zcx_rest_api_exceptions INTO DATA(v_exception).
        zcx_graph_api_exception=>raise_error(
            EXPORTING
              i_text       = v_exception->get_text( )

              i_app_error  = 4
              i_error_type = zcx_graph_api_exception=>be_api_error_kind-technical_error

          ).
    ENDTRY.
    IF v_response-status_code = cl_rest_status_code=>gc_success_created.
      me->rest_api->get_response_data( IMPORTING e_response_data = r_aad_user ).
    ELSE.
      raise_exception( ).
    ENDIF.



  ENDMETHOD.


  METHOD zif_ms_graph_api~delete_user.

    set_auth_header( ).

    me->rest_api->set_uri( i_uri = |/users/{ i_user_id }| ). " move to and read from customizing
    TRY.
        DATA(v_response) = me->rest_api->execute( method     = if_rest_message=>gc_method_delete ).
      CATCH zcx_rest_api_exceptions INTO DATA(v_exception).
        zcx_graph_api_exception=>raise_error(
            EXPORTING
              i_text       = v_exception->get_text( )

              i_app_error  = 4
              i_error_type = zcx_graph_api_exception=>be_api_error_kind-technical_error

          ).
    ENDTRY.
    IF v_response-status_code = cl_rest_status_code=>gc_success_no_content.
      me->rest_api->get_response_data( ).
    ELSE.
      raise_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_ms_graph_api~get_user.
    DATA azure_b2c_user TYPE zif_ms_graph_api=>ty_az_graph_api_user.
    set_auth_header( ).
*$filter=(identities/any(i:i/issuer eq 'b2ctestpnrad.onmicrosoft.com' and i/issuerAssignedId eq 'peter456.testuser@dbkfr.de'))
    DATA(v_uri) = |{ zif_ms_graph_api=>co_graph_uri_prefix-user }|.
    IF  i_user_id IS NOT INITIAL.
      v_uri = |{ v_uri }?$filter=(identities/any(i:i/issuer eq '{ b2c_tenant_variables-tenant }' and i/issuerAssignedId eq '{ i_user_id }'))|.
*      v_uri = |{ v_uri }/{ i_user_id }|.
    ENDIF.
    IF  i_requested_attributes IS NOT INITIAL."$select=displayName,givenName,postalCode,identities
      IF i_user_id IS INITIAL.
        v_uri = |{ v_uri }?$select=|.
      ELSE.
        v_uri = |{ v_uri }&$select=|.
      ENDIF.

*      v_uri = |{ v_uri }?$select=|.
      DATA(v_idx) = 0.
      LOOP AT i_requested_attributes ASSIGNING FIELD-SYMBOL(<attr>) .
        v_idx = v_idx + 1.
        IF v_idx = 1.
          v_uri = |{ v_uri }{ <attr> }|.
        ELSE.
          v_uri = |{ v_uri },{ <attr> }|.
        ENDIF.
      ENDLOOP.

    ENDIF.
*
    me->rest_api->set_uri( i_uri = v_uri ). " move to and read from customizing
    TRY.
        DATA(v_response) = me->rest_api->execute( ).
      CATCH zcx_rest_api_exceptions INTO DATA(v_exception).
        zcx_graph_api_exception=>raise_error(
            EXPORTING
              i_text       = v_exception->get_text( )

              i_app_error  = 4
              i_error_type = zcx_graph_api_exception=>be_api_error_kind-technical_error

          ).
    ENDTRY.
    IF v_response-status_code = cl_rest_status_code=>gc_success_ok.
      e_json_data = v_response-response->get_string_data( ).
      me->rest_api->get_response_data( IMPORTING e_response_data = azure_b2c_user ).
      IF lines( azure_b2c_user-value ) = 1 AND i_user_id IS NOT INITIAL.
        r_az_b2c_user = CORRESPONDING #( azure_b2c_user-value[ 1 ] ).

      ENDIF.
      IF e_user_attributes_ext IS NOT INITIAL.
        me->rest_api->get_response_data( IMPORTING e_response_data = e_user_attributes_ext ).
      ENDIF.
    ELSEIF v_response-status_code <> cl_rest_status_code=>gc_client_error_not_found.
      raise_exception( ).
    ENDIF.

*

  ENDMETHOD.


  METHOD zif_ms_graph_api~get_user_memership.
    DATA azure_b2c_groups TYPE zif_ms_graph_api=>ty_az_graph_api_grp.
*
    set_auth_header( ).


    me->rest_api->set_uri( i_uri = |/users/{ i_user_id }/memberOf| ). " move to and read from customizing
    TRY.
        DATA(v_response) = me->rest_api->execute( ).
      CATCH zcx_rest_api_exceptions INTO DATA(v_exception).
        zcx_graph_api_exception=>raise_error(
            EXPORTING
              i_text       = v_exception->get_text( )

              i_app_error  = 4
              i_error_type = zcx_graph_api_exception=>be_api_error_kind-technical_error

          ).
    ENDTRY.
*    me->rest_api->close( ).

    IF v_response-status_code = cl_rest_status_code=>gc_success_ok.
      me->rest_api->get_response_data( IMPORTING e_response_data = azure_b2c_groups ).
      rt_membership = CORRESPONDING #( azure_b2c_groups-value ).
    ELSE.
      raise_exception( ).
    ENDIF.



  ENDMETHOD.


  METHOD zif_ms_graph_api~remove_user_from_group.

*
    set_auth_header( ).

    me->rest_api->set_uri( i_uri = |/groups/{ i_grp }/members/{ i_user_id }/$ref| ). " move to and read from customizing
TRY.
    DATA(v_response) = me->rest_api->execute( method = if_rest_message=>gc_method_delete  ).
 CATCH zcx_rest_api_exceptions INTO DATA(v_exception).
        zcx_graph_api_exception=>raise_error(
            EXPORTING
              i_text       = v_exception->get_text( )

              i_app_error  = 4
              i_error_type = zcx_graph_api_exception=>be_api_error_kind-technical_error

          ).
    ENDTRY.

    IF v_response-status_code = cl_rest_status_code=>gc_success_no_content.
      me->rest_api->get_response_data( ).
    ELSE.
      raise_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_ms_graph_api~update_user.

    set_auth_header( ).
    set_content_type_hdr( i_content_type = if_rest_media_type=>gc_appl_json  ).
    me->rest_api->set_uri( i_uri = |/users/{ i_user_id }| ). " move to and read from customizing

    CHECK i_user_attributes IS NOT INITIAL.
    DATA(v_data_json) = /ui2/cl_json=>serialize( data = i_user_attributes compress = abap_true
                                                 pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
*    me->rest_api->set_string_body( body = v_data_json ).
    me->rest_api->set_binary_body( body = i_user_attributes ).
try.
    DATA(v_response) = me->rest_api->execute( method = if_rest_message=>gc_method_patch ).
 CATCH zcx_rest_api_exceptions INTO DATA(v_exception).
        zcx_graph_api_exception=>raise_error(
            EXPORTING
              i_text       = v_exception->get_text( )

              i_app_error  = 4
              i_error_type = zcx_graph_api_exception=>be_api_error_kind-technical_error

          ).
    ENDTRY.

    IF v_response-status_code = cl_rest_status_code=>gc_success_no_content.
      me->rest_api->get_response_data( ).
    ELSE.
      raise_exception( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
