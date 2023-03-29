interface ZIF_REST_FRAMEWORK
  public .


  methods EXECUTE
    importing
      !METHOD type STRING default IF_REST_MESSAGE=>GC_METHOD_GET
      !IO_ENTITY type ref to IF_REST_ENTITY optional
      !ASYNC type ABAP_BOOL default ABAP_FALSE
      !IS_RETRY type ABAP_BOOL default ABAP_FALSE
    returning
      value(R_RESPONSE) type ZREST_API_RESPONSE
    raising
      ZCX_REST_API_EXCEPTIONS .
  methods SET_STRING_BODY
    importing
      !BODY type STRING .
  methods SET_BINARY_BODY
    importing
      !BODY type XSTRING .
  methods SET_REQUEST_HEADER
    importing
      !IV_NAME type IHTTPNAM
      !IV_VALUE type IHTTPVAL .
  methods SET_REQUEST_HEADERS
    importing
      !IT_HEADER_FIELDS type TIHTTPNVP .
  methods SET_URI
    importing
      !I_URI type STRING .
endinterface.
