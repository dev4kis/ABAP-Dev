CLASS zcl_adf_ad_service DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_cua_b2c_graph_access .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_aad_token_response,
             access_token TYPE string,
           END OF ty_aad_token_response.
    METHODS constructor
      IMPORTING
        !i_rfc_destination TYPE rfc_dest .
  PROTECTED SECTION.

    METHODS get_aad_token
      IMPORTING
        !iv_client_id            TYPE string
        !iv_resource             TYPE string OPTIONAL
        !iv_scope                TYPE string OPTIONAL
        !iv_client_secret        TYPE string
      RETURNING
        VALUE(r_aa_access_token) TYPE string .
  PRIVATE SECTION.
    DATA rest_api TYPE REF TO zcl_abap_rest_framework.
ENDCLASS.



CLASS zcl_adf_ad_service IMPLEMENTATION.


  METHOD constructor.
    rest_api = NEW #( i_rfc_destination ).
  ENDMETHOD.


  METHOD get_aad_token.
    DATA: form_data_helper     TYPE REF TO cl_rest_form_data,
          it_params            TYPE tihttpnvp,
          lo_request           TYPE REF TO if_rest_entity,
          v_aad_token_response TYPE ty_aad_token_response.
    CHECK rest_api IS BOUND.
*    me->rest_api->set_request_header(
*       EXPORTING
*         iv_name  =     |Authorization|             " HTTP Framework (iHTTP) HTTP Name
*         iv_value = |Bearer | "v_token                 " HTTP Framework (iHTTP) HTTP Value
*     ).
    CREATE OBJECT form_data_helper
      EXPORTING
        io_entity = lo_request.
    it_params = VALUE #( BASE it_params ( name = 'resource' value = iv_resource ) ).
    it_params = VALUE #( BASE it_params ( name = 'client_id' value = iv_client_id ) ).
    it_params = VALUE #( BASE it_params ( name = 'client_secret' value = iv_client_secret ) ).

    it_params = VALUE #( BASE it_params ( name = 'grant_type' value = 'client_credentials' ) ).

    rest_api->set_request_header( iv_name = 'Content-Type'  iv_value = if_rest_media_type=>gc_appl_www_form_url_encoded ).
    rest_api->set_string_body( cl_http_utility=>fields_to_string( it_params ) ).


    me->rest_api->execute(
      EXPORTING
        method     = if_rest_message=>gc_method_get " Methods for HTTP Versions
        io_entity  =        lo_request                      " REST representation
        async      = abap_false
        is_retry   = abap_false
      RECEIVING
        r_response = DATA(v_response)                               " REST representation
    ).

*    me->rest_api->close( ).
*
    IF v_response-status_code = cl_rest_status_code=>gc_success_ok.
      me->rest_api->get_response_data( IMPORTING e_response_data = v_aad_token_response ).
      r_aa_access_token = v_aad_token_response-access_token.
    ELSE.
*      DATA v_error TYPE ty_graph_api_error.
*      me->rest_api->get_error_data( IMPORTING e_error_data = v_error ) .
    ENDIF.

    me->rest_api->close( ).



  ENDMETHOD.
ENDCLASS.
