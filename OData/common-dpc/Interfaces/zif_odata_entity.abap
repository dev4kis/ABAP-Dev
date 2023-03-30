interface ZIF_ODATA_ENTITY
  public .


  constants CO_ODATA_ENTITY_INTF type SEOCLSNAME value 'ZIF_ODATA_ENTITY' ##NO_TEXT.
  constants CO_VALIDATE_METHOD_CREATE type STRING value 'CREATE' ##NO_TEXT.
  constants CO_VALIDATE_METHOD_UPDATE type STRING value 'UPDATE' ##NO_TEXT.
  constants CO_VALIDATE_METHOD_DELETE type STRING value 'DELETE' ##NO_TEXT.
  constants CO_VALIDATE_METHOD_READ type STRING value 'READ' ##NO_TEXT.
  constants CO_VALIDATE_METHOD_QUERY type STRING value 'QUERY' ##NO_TEXT.
  constants CO_VALIDATE_METHOD_GET_STREAM type STRING value 'GET_STREAM' ##NO_TEXT.
  constants CO_VALIDATE_METHOD_H_ACTION type STRING value 'HANDLE_ACTION' ##NO_TEXT.

  methods INIT
    importing
      !I_APPLICATION type ref to ZIF_ODATA_APPLICATION
    raising
      /IWBEP/CX_MGW_NOT_IMPL_EXC .
  methods READ
    importing
      !I_KEY type /IWBEP/T_MGW_TECH_PAIRS
      !I_NAVIGATION_TAB type /IWBEP/T_MGW_NAVIGATION_PATH optional
      !I_SELECT_COLUMN_TAB type STRING_TABLE optional
      !I_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !I_SOURCE_KEY_TAB type /IWBEP/T_MGW_TECH_PAIRS
      !I_ENTITY_NAME type /IWBEP/MGW_TECH_NAME
    exporting
      !E_ENTITY type ANY
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION
      CX_STATIC_CHECK .
  methods UPDATE
    importing
      !I_KEY type /IWBEP/T_MGW_TECH_PAIRS
      !I_NAVIGATION_TAB type /IWBEP/T_MGW_NAVIGATION_PATH optional
      !I_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !I_UPDATE_ENTITY type DATA
      !I_ENTITY_NAME type /IWBEP/MGW_TECH_NAME
    exporting
      !E_ENTITY type ANY
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION
      CX_STATIC_CHECK .
  methods CREATE
    importing
      !I_NEW_ENTITY type DATA
      !I_ENTITY_NAME type /IWBEP/MGW_TECH_NAME
      !I_CONTRACT_GUID type CRMT_OBJECT_GUID optional
      !I_TUTORIAL_MODE type CRMT_BOOLEAN optional
    exporting
      !E_ENTITY type DATA
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION
      CX_STATIC_CHECK .
  methods DELETE
    importing
      !I_KEY type /IWBEP/T_MGW_TECH_PAIRS
      !I_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !I_ENTITY_NAME type /IWBEP/MGW_TECH_NAME
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION
      CX_STATIC_CHECK .
  methods QUERY
    importing
      !I_SELECT_OPTION_TAB type /IWBEP/T_MGW_SELECT_OPTION optional
      !I_SELECT_COLUMN_TAB type STRING_TABLE optional
      !I_OSQL_WHERE type CSEQUENCE optional
      !I_TOP type I default 100
      !I_SKIP type I optional
      !I_SORT_ELEMENT_TAB type IF_SALV_IDA_TYPES_INT=>YT_DB_SORT_RULE optional
      !I_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH optional
      !I_ENTITY_NAME type /IWBEP/MGW_TECH_NAME
    changing
      !C_QUERY_RESULT type ANY TABLE
    raising
      CX_STATIC_CHECK .
  methods QUERY_EXPAND
    importing
      !I_SELECT_OPTION_TAB type /IWBEP/T_MGW_SELECT_OPTION optional
      !I_SELECT_COLUMN_TAB type STRING_TABLE optional
      !I_OSQL_WHERE type CSEQUENCE optional
      !I_TOP type I default 100
      !I_SKIP type I optional
      !I_SORT_ELEMENT_TAB type IF_SALV_IDA_TYPES_INT=>YT_DB_SORT_RULE optional
      !I_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH optional
      !I_ENTITY_NAME type /IWBEP/MGW_TECH_NAME
    exporting
      !ET_EXPANDED_CLAUSES type STRING_TABLE
      !ET_EXPANDED_TECH_CLAUSES type STRING_TABLE
    changing
      !C_QUERY_RESULT type ANY TABLE .
  methods COUNT
    importing
      !I_SELECT_OPTION_TAB type /IWBEP/T_MGW_SELECT_OPTION optional
      !I_OSQL_WHERE type CSEQUENCE optional
      !I_ENTITY_NAME type /IWBEP/MGW_TECH_NAME
    returning
      value(R_RESULT) type INT4 .
  methods GET_STREAM
    importing
      !I_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(R_RESULT) type ZODATA_MEDIA_RESOURCE .
  methods CREATE_STREAM
    importing
      !I_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !I_NEW_STREAM type ZODATA_MEDIA_RESOURCE
    exporting
      !E_STREAM type DATA .
  methods HANDLE_ACTION
    importing
      !I_ACTION type STRING
      !I_PARAMETERS type /IWFND/T_MGW_NAME_VALUE_PAIR
      !I_ENTITY_NAME type /IWBEP/MGW_TECH_NAME
    exporting
      !ER_ENTITY type DATA
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      CX_STATIC_CHECK .
endinterface.
