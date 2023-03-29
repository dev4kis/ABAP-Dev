class ZCL_ABAP_REST_FRAMEWORK definition
  public
  final
  create public .

public section.

  interfaces ZIF_REST_FRAMEWORK .
  interfaces IF_SERIALIZABLE_OBJECT .

  aliases EXECUTE
    for ZIF_REST_FRAMEWORK~EXECUTE .
  aliases SET_BINARY_BODY
    for ZIF_REST_FRAMEWORK~SET_BINARY_BODY .
  aliases SET_REQUEST_HEADER
    for ZIF_REST_FRAMEWORK~SET_REQUEST_HEADER .
  aliases SET_REQUEST_HEADERS
    for ZIF_REST_FRAMEWORK~SET_REQUEST_HEADERS .
  aliases SET_STRING_BODY
    for ZIF_REST_FRAMEWORK~SET_STRING_BODY .
  aliases SET_URI
    for ZIF_REST_FRAMEWORK~SET_URI .

*      IMPORTING
**        !interface_name        TYPE zinterface_id
**        !business_identifier   TYPE zbusinessid OPTIONAL
**        !method                TYPE zinterface_method
**        !logdata_in_updatetask TYPE char1 OPTIONAL
*      RAISING
*        zcx_interace_config_missing
*        zcx_http_client_failed
  methods CONSTRUCTOR
    importing
      !I_RFC_DESTINATION type RFC_DEST
    raising
      CX_STATIC_CHECK .
  methods GET_STARTTIME
    returning
      value(RV_STARTTIME) type SY-UZEIT .
  methods GET_STARTDATE
    returning
      value(RV_STARTDATE) type SY-DATUM .
  methods GET_SUBMITTIME
    returning
      value(RV_SUBMITTIME) type SY-UZEIT .
  methods GET_SUBMITDATE
    returning
      value(RV_SUBMITDATE) type SY-DATUM .
  methods GET_ENDTIME
    returning
      value(RV_ENDTIME) type SY-UZEIT .
  methods GET_ENDATE
    returning
      value(RV_ENDDATE) type DATUM .
  methods GET_RESPONSE_HEADER
    importing
      !IV_NAME type STRING
    returning
      value(RV_VALUE) type STRING .
  methods GET_RESPONSE_HEADERS
    returning
      value(RT_HEADER_FIELDS) type TIHTTPNVP .
  methods CREATE_MESSAGE_ID
    returning
      value(E_MESSAGE_ID) type GUID_16 .
  methods GET_USER
    returning
      value(RV_USER) type SY-UNAME .
  methods GET_STATUS
    returning
      value(RV_STATUS) type I .
  methods GET_HTTP_CLIENT
    returning
      value(RESULT) type ref to IF_HTTP_CLIENT .
  methods GET_REST_CLIENT
    returning
      value(RESULT) type ref to CL_REST_HTTP_CLIENT .
  methods IS_RETRY
    returning
      value(E_IS_RETRY) type ABAP_BOOL .
  methods GET_REQUEST
    returning
      value(RESULT) type ref to IF_REST_ENTITY .
  methods GET_GUID
    returning
      value(E_MESSAGE_ID) type GUID_16 .
  methods GET_URI
    returning
      value(RESULT) type DAS_URI .
  methods GET_RETRYNUM
    returning
      value(RESULT) type I .
  methods GET_METHOD
    returning
      value(RESULT) type CHAR20 .
  methods GET_DURATION
    returning
      value(R_SECS) type TZNTSTMPL .
  methods GET_PROGRAM_HEADERS
    returning
      value(RESULT) type TIHTTPNVP .
  methods CLOSE
    raising
      ZCX_REST_API_EXCEPTIONS .
  methods GET_ERROR_DATA
    exporting
      !E_ERROR_DATA type ANY
      !E_STATUS_REASON type STRING .
  methods GET_RESPONSE_DATA
    importing
      value(I_RESPONSE_DATA) type ref to IF_REST_ENTITY optional
    exporting
      !E_RESPONSE_DATA type ANY .
  PROTECTED SECTION.
private section.

*"* private components of class ZCL_ABAP_REST_FRAMEWORK
*"* do not include other source files here!!!
  data REST_CLIENT type ref to CL_REST_HTTP_CLIENT .
  data HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  data SUBMIT_DATE type SY-DATUM .
  data SUBMIT_TIME type SY-UZEIT .
  data START_TIME type SY-UZEIT .
  data START_DATE type SY-DATUM .
  data END_TIME type SY-UZEIT .
  data END_DATE type SY-DATUM .
  data MESSAGE_ID type GUID_16 .
  data RETRY type ABAP_BOOL .
  data USER type UNAME .
  data REQUEST type ref to IF_REST_ENTITY .
  data RESPONSE type ref to IF_REST_ENTITY .
  class-data URI_FINAL type DAS_URI .
  class-data RETRY_CNT type I .
  class-data METHOD_CALL type HTTPMETHOD .
  class-data PRE_TIMESTAMP type TIMESTAMPL .
  class-data PRO_TIMESTAMP type TIMESTAMPL .
  class-data DURATION type TZNTSTMPL .
  class-data LOG_UPDATE_TASK type CHAR1 .
  class-data PROGRAM_HEADERS type TIHTTPNVP .
  data HEADERS type TIHTTPNVP .
  data URI_PARAMS type CRMT_ATTR_NAME_VALUE_T .
  data SERVICE_ENDPOINT type STRING .
  data DESTINATION type RFC_DEST .
  data JSON_UTIL type ref to /UI2/CL_JSON .

  methods PARSE_RESPONSE
    importing
      value(I_RESPONSE_DATA) type ref to IF_REST_ENTITY optional
    exporting
      !E_DATA type ANY .
  methods SET_USER .
  methods SET_ENDDATE
    importing
      !IV_ENDATE type DATUM .
  methods SET_ENDTIME
    importing
      !IV_ENDTIME type SY-UZEIT .
  methods SET_STARTDATE .
  methods SET_SUBMITDATE .
  methods SET_SUBMITTIME .
  methods SET_STARTTIME .
*    raising
*      ZCX_INTERACE_CONFIG_MISSING
*      ZCX_HTTP_CLIENT_FAILED
  methods CREATE_HTTP_CLIENT
    returning
      value(HTTP_CLIENT) type ref to IF_HTTP_CLIENT
    raising
      ZCX_HTTP_CLIENT .
  methods SAVE_LOG .
  methods CREATE_REST_CLIENT
    importing
      !IV_HTTP_CLIENT type ref to IF_HTTP_CLIENT
    returning
      value(RV_REST_CLIENT) type ref to CL_REST_HTTP_CLIENT
    raising
      ZCX_HTTP_CLIENT .
  methods CREATE_REQUEST_ENTITY
    importing
      !IV_MULTIPART type ABAP_BOOL default ABAP_FALSE
    returning
      value(RO_ENTITY) type ref to IF_REST_ENTITY .
  methods GET_RESPONSE_ENTITY
    returning
      value(RO_RESPONSE_ENTITY) type ref to IF_REST_ENTITY .
ENDCLASS.



CLASS ZCL_ABAP_REST_FRAMEWORK IMPLEMENTATION.


  METHOD  close.

    DATA: rest_exception TYPE REF TO cx_rest_client_exception.

    TRY.
        rest_client->if_rest_client~close( ).
      CATCH cx_rest_client_exception INTO rest_exception.
        RAISE EXCEPTION TYPE zcx_rest_api_exceptions
        EXPORTING previous = rest_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.
**   Create the unique guid . This is used across all the tables for storing
**   and retrieving the information related to REST calls
*    message_id = me->set_guid( ).
*   Set the calling interface.This needs to be provided by while creating instance
*    DATA: cx_interface_missing  TYPE REF TO   zcx_interace_config_missing,
*          cx_http_client_failed TYPE REF TO zcx_http_client_failed.

*    CREATE OBJECT: cx_interface_missing,
*                   cx_http_client_failed.

*    interface = interface_name.
*    businessid = business_identifier.
*    method_call = method.
*    log_update_task = logdata_in_updatetask .
*   Create the HTTP client and raise exception if interface is not found in the config
*   Propagate the error to the calling program
    me->destination = i_rfc_destination.
    TRY .

        http_client =  create_http_client(
*            interface   = interface_name
*            method      = method
             ).

      CATCH cx_root "zcx_interace_config_missing
        INTO DATA(cx_interface_missing).
        RAISE EXCEPTION cx_interface_missing.

    ENDTRY.
*   Set the protocol method as HTTP 1.1--Hard code this..Not gonna change
    http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
*   Set the request
    create_request_entity( ).
  ENDMETHOD.


  METHOD  create_http_client.
*


*   Create the http client by destination
    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = me->destination
      IMPORTING
        client                   = http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6 ).
    CASE sy-subrc.
      WHEN 1 OR 2 OR 3 OR 4 OR 5 OR 6.
*        RAISE EXCEPTION TYPE zcx_http_client_failed.
    ENDCASE.
*   Go ahead with processign and create the http client
    http_client->propertytype_logon_popup = http_client->co_disabled.
    rest_client = create_rest_client( iv_http_client =  http_client ).
  ENDMETHOD.


  METHOD create_message_id.

    TRY.
        e_message_id = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
        e_message_id = sy-datum && sy-uzeit.
    ENDTRY.

  ENDMETHOD.


  METHOD  create_request_entity.
**To allow GET and DELETE request entity in REST call
    IF method_call EQ if_rest_message=>gc_method_get OR method_call EQ if_rest_message=>gc_method_delete.
    ELSE.
      request = rest_client->if_rest_client~create_request_entity( iv_multipart ).
    ENDIF.
  ENDMETHOD.


  METHOD  create_rest_client.
    TRY .
        rv_rest_client = NEW #( io_http_client = iv_http_client ).
*
      CATCH cx_root INTO DATA(v_exc).
        RAISE EXCEPTION TYPE zcx_http_client
          EXPORTING
            previous = v_exc.
    ENDTRY.
  ENDMETHOD.


  METHOD get_duration.
    r_secs = duration.
    r_secs = r_secs * -1000. "microseconds
  ENDMETHOD.


  METHOD  get_endate.
    rv_enddate =   end_date.
  ENDMETHOD.


  METHOD  get_endtime.
    rv_endtime =  end_time.
  ENDMETHOD.


  METHOD get_error_data.
    CHECK me->response IS BOUND.
    DATA(v_content_lenght) = me->response->get_content_length( ).

    CHECK v_content_lenght > 0.
*    DATA(v_data) = me->response->get_string_data( ).
*
*    CHECK v_data IS NOT INITIAL.

    IF me->get_status( ) <>  cl_rest_status_code=>gc_success_ok.
      me->parse_response( IMPORTING e_data = e_error_data  ).
      e_status_reason = response->get_header_field( iv_name = |~status_reason| ).
*
*      IF v_status_code = cl_rest_status_code=>gc_client_error_unauthorized.
*        zcx_http=>raise_unauthorized( i_text = |{ v_status_reason } - { v_error-message }|  ).
*      ENDIF.
*      zcx_http=>raise_internal_server_error( i_text = |{ v_status_reason } - { v_error-message }|  ).
    ENDIF.
  ENDMETHOD.


  METHOD get_guid.
    e_message_id = me->message_id.
  ENDMETHOD.


  METHOD get_http_client.
    result = http_client.

  ENDMETHOD.


  METHOD get_method.
    result = method_call.
  ENDMETHOD.


  METHOD get_program_headers.
    result = program_headers.
    REFRESH program_headers. "v-javeda | MS2K948978
  ENDMETHOD.


  METHOD get_request.
    result = request.
  ENDMETHOD.


  METHOD get_response_data.
    CHECK me->response IS BOUND.
    DATA(v_content_lenght) = me->response->get_content_length( ).

    CHECK v_content_lenght > 0.
*    DATA(v_data) = me->response->get_string_data( ).
*
*    CHECK v_data IS NOT INITIAL.

    IF me->get_status( ) =  cl_rest_status_code=>gc_success_ok OR me->get_status( ) =  cl_rest_status_code=>gc_success_created.
      me->parse_response( IMPORTING e_data = e_response_data  ).
*      DATA(v_status_reason) = response->get_header_field( iv_name = |~status_reason| ).
*
*      IF v_status_code = cl_rest_status_code=>gc_client_error_unauthorized.
*        zcx_http=>raise_unauthorized( i_text = |{ v_status_reason } - { v_error-message }|  ).
*      ENDIF.
*      zcx_http=>raise_internal_server_error( i_text = |{ v_status_reason } - { v_error-message }|  ).
    ENDIF.
  ENDMETHOD.


  METHOD  get_response_entity.
    ro_response_entity = me->rest_client->if_rest_client~get_response_entity( ).
    response  = ro_response_entity.
  ENDMETHOD.


  METHOD  get_response_header.
    rv_value = rest_client->if_rest_client~get_response_header(
        iv_name  = iv_name ).
  ENDMETHOD.


  METHOD  get_response_headers.
    rt_header_fields = me->rest_client->if_rest_client~get_response_headers(  ).
  ENDMETHOD.


  METHOD get_rest_client.
    result = rest_client.
  ENDMETHOD.


  METHOD get_retrynum.
    result = retry_cnt.
  ENDMETHOD.


  METHOD  get_startdate.
    rv_startdate =  start_date.
  ENDMETHOD.


  METHOD  get_starttime.
    rv_starttime =  start_time.
  ENDMETHOD.


  METHOD  get_status.
    IF  rest_client IS BOUND.
      rv_status =  rest_client->if_rest_client~get_status( ).
    ENDIF.
  ENDMETHOD.


  METHOD  get_submitdate.
    rv_submitdate =  submit_date.
  ENDMETHOD.


  METHOD  get_submittime.
    rv_submittime =  submit_time.
  ENDMETHOD.


  METHOD get_uri.
    result = uri_final.
  ENDMETHOD.


  METHOD  get_user.
    rv_user =  user.
  ENDMETHOD.


  METHOD is_retry.
    e_is_retry = me->retry.
  ENDMETHOD.


  METHOD parse_response.
    DATA: lo_type_def   TYPE REF TO cl_abap_typedescr,
          lo_pos_struct TYPE REF TO cl_abap_structdescr,
          lr_attr_ref   TYPE REF TO data.
    FIELD-SYMBOLS: <data>       TYPE data,
                   <l_msg_data> TYPE any,
                   <attr_struc> TYPE any.

    lo_type_def  = cl_abap_tabledescr=>describe_by_data( p_data = e_data ).
    DATA(v_name) = lo_type_def->absolute_name.

    CREATE DATA lr_attr_ref TYPE (v_name).
    ASSIGN lr_attr_ref->* TO <data>.


    CHECK me->response IS BOUND.

*    DATA(t_components) = build_components( CONV #( v_name ) ).
    DATA(v_data) = me->response->get_string_data( ).
*    DATA(x_data) = me->response->get_binary_data( ).


    json_util = NEW #(  ).
    TRY.
        json_util->deserialize_int(
          EXPORTING
            json  =   v_data              " JSON string
*            jsonx =   x_data         " JSON XString
          CHANGING
            data  =   <data>               " Data to serialize
        ).
      CATCH cx_sy_move_cast_error.
        zcx_crm_app=>raise_internal_error(
          EXPORTING
            i_text       = TEXT-001
*            i_empty_text =
        ).
    ENDTRY.


*
*    DATA: lv_json TYPE /ui2/cl_json=>json,
*          lr_data TYPE REF TO data,
*          lv_val  TYPE string.
*
*    lr_data = /ui2/cl_json=>generate( json = v_data ).
*    ASSIGN lr_data->* TO <l_msg_data>.
*
*    lo_pos_struct ?= cl_abap_typedescr=>describe_by_data( <l_msg_data> ).
*
*    IF lr_data IS BOUND.
*      LOOP AT lo_pos_struct->components ASSIGNING FIELD-SYMBOL(<l_posref>).
*        IF <l_posref>-name = 'ACCESS_TOKEN'.
*          /ui2/cl_data_access=>create( ir_data = lr_data iv_component = `access_token`)->value( IMPORTING ev_data = e_data ).
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*
*    IF e_data IS INITIAL.
*      /ui2/cl_json=>deserialize( EXPORTING
*                               jsonx = x_data
*                               pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*                             CHANGING
*                               data = <data> ).
*      e_data = <data>.
*    ENDIF.


    e_data = <data>.
  ENDMETHOD.


  METHOD  save_log.
*    CALL FUNCTION 'ZSAVE_REST_LOG'
*      EXPORTING
*        framework_class = me
*        update_task     = log_update_task.
  ENDMETHOD.


  METHOD  set_enddate.
    end_date = iv_endate  .
  ENDMETHOD.


  METHOD  set_endtime.
    end_time = iv_endtime.
  ENDMETHOD.


  METHOD  set_startdate.
    start_date = sy-datum.
  ENDMETHOD.


  METHOD  set_starttime.
    start_time = sy-uzeit.
  ENDMETHOD.


  METHOD  set_submitdate.
    submit_date = sy-datum.
  ENDMETHOD.


  METHOD  set_submittime.
    start_date = sy-datum.
  ENDMETHOD.


  METHOD  set_user.
    user = sy-uname.
  ENDMETHOD.


  METHOD  zif_rest_framework~execute.

    DATA:
          rest_exception  TYPE REF TO cx_rest_client_exception.
    CREATE OBJECT rest_exception.
    GET TIME STAMP FIELD pre_timestamp.

*   Is this a retry ?
    me->retry = is_retry.

*   Change only for the retry sceario.
    IF is_retry( ) = abap_true.
*      me->message_id = messageid.
*      retry_cnt = retry_count + 1.
    ELSE.
*   Create the unique guid . This is used across all the tables for storing
*   and retrieving the information related to REST calls
      me->message_id = me->create_message_id( ).
    ENDIF.

*   This method will execute GET,POST,PUT ...ased on the configuration set for the inteface. If Async is set , Request will
*   in the wating status till the async program flushes this out. Apart from executing the calls , this method will hold
*   the metrics of data
*   Get the configuration data for the inteface.

*    lwa_config_data = zcl_rest_utility_class=>get_config_data(
*        interface_id = interface
*        method       = method_call ).

*   Get the static headers from the configuration
*    rest_client->if_rest_client~set_request_headers( VALUE #( ( ) ) )."zcl_rest_utility_class=>get_static_headers( interface_id =  interface ) ).
*   Set the corelation id
    DATA lv_value TYPE string.
    lv_value = message_id.
    rest_client->if_rest_client~set_request_header( iv_name = 'id-sap-restfrmwrk' iv_value = lv_value ).
*   Set the basic log parameters
*    log_start_params( ).
*    gwa_log-method = method.
*   If it's Async , record and exit
    IF async EQ abap_false.
      TRY .
          CASE method.
            WHEN  if_rest_message=>gc_method_head.
              rest_client->if_rest_resource~head( ).
            WHEN if_rest_message=>gc_method_get.
              rest_client->if_rest_resource~get( ).
            WHEN  if_rest_message=>gc_method_delete.
              rest_client->if_rest_resource~delete( ).
            WHEN if_rest_message=>gc_method_options.
              rest_client->if_rest_resource~options( ).
            WHEN  if_rest_message=>gc_method_post.
              rest_client->if_rest_resource~post(  request ).
            WHEN  if_rest_message=>gc_method_put.
              rest_client->if_rest_resource~put(   request ).
            WHEN  if_rest_message=>gc_method_patch.
              http_client->request->set_method(
                  method = if_rest_message=>gc_method_patch
              ).

              http_client->send( ).

              http_client->receive(
                EXCEPTIONS
                  http_communication_failure = 1
                  http_invalid_state         = 2
                  http_processing_failed     = 3
                  OTHERS                     = 4 ).
              IF sy-subrc <> 0.
                RAISE EXCEPTION TYPE zcx_rest_api_exceptions.
              ENDIF.
*
          ENDCASE.

*          log_end_params( ).
        CATCH cx_rest_client_exception INTO rest_exception.
             RAISE EXCEPTION TYPE zcx_rest_api_exceptions.
      ENDTRY.
*Read the respomse and set the appropriate reason
      me->response =  rest_client->if_rest_client~get_response_entity( ).
      IF response IS BOUND.
        r_response-response = me->response.
        r_response-status_code = me->get_status( ).
      ENDIF.
    ELSE.
    ENDIF.
*   duration = duration * -1000. " Convert to microseconds
    GET TIME STAMP FIELD pro_timestamp.
    duration = cl_abap_tstmp=>subtract(
        tstmp1 = pre_timestamp
        tstmp2 = pro_timestamp ).

*   Save the log to database for further reporing
    save_log( ).
  ENDMETHOD.


  METHOD zif_rest_framework~set_binary_body.
    IF body IS NOT INITIAL.
      request->set_binary_data( body ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_rest_framework~set_request_header.
    DATA lv_tihttpnvp TYPE LINE OF tihttpnvp.
    lv_tihttpnvp-name = iv_name.
    lv_tihttpnvp-value = iv_value.
    APPEND lv_tihttpnvp TO program_headers.
    CLEAR lv_tihttpnvp.
    http_client->request->set_header_field( name = iv_name value = iv_value ).
*    http_client->request->set_header_fields( fields = program_headers ).
  ENDMETHOD.


  METHOD zif_rest_framework~set_request_headers.
    DATA: lv_tihttpnvp        TYPE LINE OF tihttpnvp,
          wa_modified_headers TYPE ihttpnvp.
    LOOP AT it_header_fields INTO wa_modified_headers.
      lv_tihttpnvp-name  = wa_modified_headers-name.
      lv_tihttpnvp-value = wa_modified_headers-value.
      APPEND lv_tihttpnvp TO program_headers.
      CLEAR lv_tihttpnvp.
    ENDLOOP.
    http_client->request->set_header_fields( it_header_fields  ).
  ENDMETHOD.


  METHOD  zif_rest_framework~set_string_body.
    IF body IS NOT INITIAL.
      http_client->request->set_cdata( body ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_rest_framework~set_uri.
    CHECK i_uri IS NOT INITIAL.
    cl_http_utility=>set_request_uri(
        request = http_client->request    " HTTP Framework (iHTTP) HTTP Request
        uri     = i_uri ).                  " URI String (in the Form of /path?query-string)

    uri_final = i_uri.
  ENDMETHOD.
ENDCLASS.
