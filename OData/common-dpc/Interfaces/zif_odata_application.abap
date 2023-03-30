interface ZIF_ODATA_APPLICATION
  public .


  types:
    BEGIN OF ty_odata_service_tech,
      service_name    TYPE string,
      service_version TYPE numc4,
    END OF ty_odata_service_tech .

  "!Validate if the application allows Anonymous Access. By Default anonymous are not allowed.
  "!
  "! @parameter r_result | <p class="shorttext synchronized" lang="en"></p>
  methods IS_ANONYMOUS_ACCESS_ALLOWED
    returning
      value(R_RESULT) type ABAP_BOOL .
  "! Set application context attribute
  "!
  "! @parameter i_attribute | <p class="shorttext synchronized" lang="en">Attribute name</p>
  "! @parameter i_value | <p class="shorttext synchronized" lang="en">Attribute value</p>
  methods SET_CONTEXT_ATTRIBUTE
    importing
      !I_ATTRIBUTE type CSEQUENCE
      !I_VALUE type CSEQUENCE .
  methods SET_MODEL
    importing
      !I_MODEL type ref to /IWBEP/IF_MGW_ODATA_RE_MODEL .
  methods GET_MODEL
    returning
      value(R_RESULT) type ref to /IWBEP/IF_MGW_ODATA_RE_MODEL .
  methods GET_CONTEXT_ATTRIBUTE
    importing
      !I_ATTRIBUTE type CSEQUENCE
    returning
      value(R_RESULT) type STRING .
  methods SET_CURRENT_TUT_ORIGIN_USER
    importing
      !I_USER type ref to ZIF_EXPO_USER .
  methods SET_CURRENT_USER
    importing
      !I_USER type ref to ZIF_EXPO_USER .
  methods GET_ENTITY_HANDLER
    importing
      !I_ENTITY_NAME type CSEQUENCE
    returning
      value(R_RESULT) type ref to ZIF_ODATA_ENTITY
    raising
      /IWBEP/CX_MGW_NOT_IMPL_EXC .
  methods GET_CURRENT_TUT_ORIGIN_USER
    returning
      value(R_RESULT) type ref to ZIF_EXPO_USER .
  methods GET_CURRENT_USER
    returning
      value(R_RESULT) type ref to ZIF_EXPO_USER .
  "! Initialize OData application instance
  "!
  methods FINISH_PROCESS
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods INIT
    importing
      !I_SERVICE_NAME type CSEQUENCE
      !I_SERVICE_VERSION type NUMC4
      !I_ODATA_CONTEXT type ref to /IWBEP/IF_MGW_CONTEXT optional .
  "! Returns OData service technical details
  "!
  "! @parameter r_result | service technical name and service Version
  methods GET_SERVICE_DETAILS
    returning
      value(R_RESULT) type ZIF_ODATA_APPLICATION=>TY_ODATA_SERVICE_TECH .
  "! Return a list of modules defined for the current application
  "!
  methods REGISTER_ODATA_MODULE
    changing
      !C_REGISTERED_MODULE type ZIF_ODATA_MODULE=>TY_MODULE_REGISTRATION .
endinterface.
