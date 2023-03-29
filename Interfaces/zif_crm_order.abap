interface ZIF_CRM_ORDER
  public .


  interfaces ZIF_CRM_ENTITY .

  methods GET_CHANGE_HISTORY_LIST
    returning
      value(R_RESULT) type ZBTX_STATUS_CHANGEDOCUMENT_TT .
  methods GET_DOCFLOW_LIST
    returning
      value(R_RESULT) type ZBTX_DOCFLOW_BTIL_TT .
  "! <p class="shorttext synchronized" lang="en">returns based on filter all available partner</p>
  "!
  "! @parameter i_partner_fct | <p class="shorttext synchronized" lang="en">filter for partner function</p>
  "! @parameter r_result | <p class="shorttext synchronized" lang="en">return all corresponding partner</p>
  methods GET_PARTNER_LIST
    importing
      !I_PARTNER_FCT type CRMT_PARTNER_FCT optional
    returning
      value(R_RESULT) type ZCRM_API_ORDER_PARTNER_T .
  methods SET_PARTNER_ADDRESS
    importing
      !I_PARTNER_FCT type CRMT_PARTNER_FCT
      !I_ADDRESS type CRMST_PARTNERADDRESS_BTIL .
  methods GET_PARTNER_ADDRESS
    importing
      !I_PARTNER_FCT type CRMT_PARTNER_FCT
    returning
      value(R_RESULT) type CRMST_PARTNERADDRESS_BTIL .
  "! <p class="shorttext synchronized" lang="en">Function to create, modify partner</p>
  "!
  "! @parameter i_partner_fct | <p class="shorttext synchronized" lang="en">paramater will be used to set the specific partner function</p>
  "! @parameter i_partner | <p class="shorttext synchronized" lang="en">use partner number, guid or object (zif_partner)</p>
  "! @parameter i_main | <p class="shorttext synchronized" lang="en">identify main partner for given partner function</p>
  methods MODIFY_PARTNER
    importing
      !I_PARTNER_FCT type CRMT_PARTNER_FCT
      !I_PARTNER type ANY
      !I_MAIN type CRMT_BOOLEAN default 'X'
      !I_ADD_PARTNER type CRMT_BOOLEAN default 'X' .
  methods ADD_PARTNER
    importing
      !I_PARTNER_FCT type CRMT_PARTNER_FCT
      !I_PARTNER type ANY
      !I_MAIN type CRMT_BOOLEAN default 'X' .
  methods DELETE_PARTNER
    importing
      !I_PARTNER_FCT type CRMT_PARTNER_FCT
      !I_PARTNER type ANY
      !I_MAIN type CRMT_BOOLEAN default 'X' .
**********************************************************************
  "! <p class="shorttext synchronized" lang="en">get or set the status of an order</p>
  "!
  "! @parameter i_status | <p class="shorttext synchronized" lang="en">if status set, than the sataus will be set to order</p>
  "! @parameter r_status | <p class="shorttext synchronized" lang="en">if no importing status found, value contains current status</p>
  methods STATUS
    importing
      !I_NEW_STATUS type CRM_J_STATUS optional
      !I_CURRENT_STATUS type ZCRM_ORDER_STATUS optional
    returning
      value(R_STATUS) type ZCRM_ORDER_STATUS .
  methods DESCRIPTION .
  methods OBJECT_ID .
  methods OBJECT_GUID .
**********************************************************************
  "! <p class="shorttext synchronized" lang="en">get a boolean from the dynamic structure</p>
  "!
  "! @parameter i_dynamic_bool_key | <p class="shorttext synchronized" lang="en">key of the boolean</p>
  "! @parameter r_value | <p class="shorttext synchronized" lang="en">returns the value of the boolean</p>
  methods GET_DYNAMIC_BOOLEAN
    importing
      !I_DYNAMIC_BOOL_KEY type ZBTX_FLAG_KEY
    returning
      value(R_VALUE) type ZBTX_GENERIC_FLAG .
  "! <p class="shorttext synchronized" lang="en">set the value of a dynamic boolean</p>
  "!
  "! @parameter i_dynamic_bool_key | <p class="shorttext synchronized" lang="en">key of the boolean</p>
  "! @parameter i_value | <p class="shorttext synchronized" lang="en">value of the boolean</p>
  methods SET_DYNAMIC_BOOLEAN
    importing
      !I_DYNAMIC_BOOL_KEY type ZBTX_FLAG_KEY
      !I_VALUE type ZBTX_EXAS_REQUEST_3STATE_BOOL .
**********************************************************************
  methods GET_ORDERADM_H_EXTENSION_STRUC
    returning
      value(R_STRUC) type CRMST_ADMINH_BTIL .
  methods GET_ORDERADM_H_EXTENSION_FIELD
    importing
      !I_FIELD_NAME type NAME_KOMP
    exporting
      value(E_RESULT) type ANY .
  methods SET_ORDERADM_H_EXTENSION_FIELD
    importing
      !I_FIELD_NAME type NAME_KOMP
      !I_VALUE type ANY .
**********************************************************************
  methods GET_CUSTOMER_H_EXTENSION_STRUC
    returning
      value(R_STRUC) type CRMST_CUSTOMERH_BTIL .
  methods GET_CUSTOMER_H_EXTENSION_FIELD
    importing
      !I_FIELD_NAME type NAME_KOMP
    exporting
      value(E_RESULT) type ANY .
  methods SET_CUSTOMER_H_EXTENSION_FIELD
    importing
      !I_FIELD_NAME type NAME_KOMP
      !I_VALUE type ANY .
**********************************************************************
  methods MODIFY_PREDECESSOR .
  methods GET_PREDECESSOR_LIST .
  methods ADD_PREDECESSOR
    importing
      !I_PARENT_GUID type CRMT_OBJECT_GUID
      !I_PARENT_OBJECT_TYPE type SWO_OBJTYP
      !I_RELTYPE type BINRELTYP default 'VONA'
      !I_VONA_KIND type CRMT_VONA_KIND optional .
  methods DELETE_PREDECESSOR .
**********************************************************************
  methods MODIFY_EXTENSION_STRUC .
**********************************************************************
  methods SET_EXTENSION_VALUE
    importing
      !I_EXTENSION_NAME type STRING
      !I_EXTENSION_FIELD type NAME_KOMP
      !I_VALUE type ANY .
  methods SET_EXTENSION_STRUC_2
    importing
      !I_EXTENSION_TAB type ZCRM_GET_EXTENSION_T
      !I_EXTENSION_VALUE type ANY .
  methods SET_EXTENSION_STRUC
    importing
      !I_EXTENSION_NAME type STRING
      !I_EXTENSION_VALUE type ANY .
  methods GET_EXTENSION_VALUE
    importing
      !I_EXTENSION_NAME type STRING
      !I_EXTENSION_FIELD type NAME_KOMP
    exporting
      !E_VALUE type ANY .
  methods GET_EXTENSION_STRUC
    importing
      !I_EXTENSION_NAME type STRING
    exporting
      !R_STRUC type ANY .
**********************************************************************
  methods MODIFY_TEXT
    importing
      !I_TEXT_OBJECT type TDOBJECT
      !I_TDID type TDID
      !I_TEXT type CRMDT_CONC_TEXTLINES
      !I_LANGU type LANGU
      !I_FORMATTED type FLAG optional
      !I_APPEND type FLAG default 'X' .
  methods GET_TEXT_LIST
    importing
      !I_FILTER_TDID type TDID optional
      !I_FILTER_LANGU type LANGU optional
    returning
      value(R_RESULT) type ZCRMST_TEXT_BTIL_T .
  methods ADD_TEXT
    importing
      !I_TEXT_OBJECT type TDOBJECT
      !I_TDID type TDID
      !I_TEXT type CRMDT_CONC_TEXTLINES
      !I_LANGU type LANGU
      !I_FORMATTED type FLAG optional
      !I_APPEND type FLAG default 'X' .
  methods DELETE_TEXT
    importing
      !I_TDID type TDID
      !I_LANGU type SPRAS .
**********************************************************************
  methods GET_APPOINTMENT_LIST
    importing
      !I_TYPE type CRMT_APPTYPE
    returning
      value(R_RESULT) type ZCRM_API_ORDER_APPOINTMENT_T .
  methods MODIFY_APPOINTMENT
    importing
      !I_APPOINTMENT type CRMT_APPTYPE
      !I_FROM_TS type CRMT_DATE_TIMESTAMP_FROM
    returning
      value(R_RESULT) type ref to ZIF_CRM_APPOINTMENT .
  methods ADD_APPOINTMENT .
  methods DELETE_APPOINTMENT .
**********************************************************************
  methods GET_ITEM_LIST
    returning
      value(R_ITEMS) type ZCRM_ORDER_ITEMS_TT .
  methods GET_ITEM_BY_ITEM_NO
    importing
      !I_KEY type ANY
    returning
      value(R_RESULT) type ref to ZIF_CRM_ORDER_ITEM .
  methods MODIFY_ITEM
    importing
      !I_ITEM_PRODUCT type ANY
      !I_ITEM_PROPERTIES type ANY
    returning
      value(R_RESULT) type ref to ZIF_CRM_ORDER_ITEM .
  methods ADD_ITEM
    importing
      !I_ITEM_PROPERTIES type CRMST_ADMINI_BTIL
    returning
      value(R_RESULT) type ref to ZIF_CRM_ORDER_ITEM .
  methods DELETE_ITEM
    importing
      !I_ITEM_GUID type CRMT_OBJECT_GUID optional
      !I_ITEM_NUMBER type CRMT_ITEM_NO_EXT
    returning
      value(R_SUCCESS) type ABAP_BOOL .
**********************************************************************
**********************************************************************
  methods GET_EXTENSION_LIST
    importing
      !I_EXTENSION_NAME type STRING
    exporting
      !E_EXTENSION_LIST type ANY TABLE .
  methods MODIFY_EXTENSION_LIST_ENTRY .
  methods ADD_EXTENSION_LIST_ENTRY
    importing
      !I_EXTENSION_NAME type STRING
      !I_EXTENSION_VALUE type ANY
    returning
      value(R_RECORD_GUID) type CRMT_OBJECT_GUID .
  methods DELETE_EXTENSION_LIST_ENTRY
    importing
      !I_EXTENSION_NAME type STRING
      !I_RECORD_GUID type CRMT_OBJECT_GUID
    returning
      value(R_SUCCESS) type ABAP_BOOL .
**********************************************************************
  methods GET_REFERENCE_OBJECT_LIST
    returning
      value(R_RESULT) type ZCRM_API_ORDER_PRODUCT_T .
  methods MODIFY_REFERENCE_OBJECT
    importing
      !I_REFERENCE_OBJECT type ANY .
  methods ADD_REFERENCE_OBJECT
    importing
      !I_REFERENCE_OBJECT type ANY .
  methods DELETE_REFERENCE_OBJECT
    importing
      !I_REFERENCE_OBJECT type ANY .
endinterface.
