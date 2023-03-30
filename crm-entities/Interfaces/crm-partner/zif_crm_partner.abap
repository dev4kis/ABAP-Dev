interface ZIF_CRM_PARTNER
  public .


  interfaces ZIF_CRM_ENTITY .

  types:
    BEGIN OF ty_create_person_attr,
      first_name TYPE char40,
      last_name  TYPE char40,

    END OF ty_create_person_attr .
  types:
    BEGIN OF ty_create_organization_attr,
      name TYPE c LENGTH 160,
    END OF ty_create_organization_attr .
  types TY_GENERAL_DATA type CRMST_HEADER_OBJECT_BUIL .
  types TY_RELATIONS_TAB type BAPIBUS1006_RELATIONSTAB .
  types:
    ty_contact_relation_tab TYPE STANDARD TABLE OF REF TO zif_crm_contact_relation WITH DEFAULT KEY .

  "! <p class="shorttext synchronized" lang="en">Get partner number</p>
  "!
  "! @parameter r_partner | <p class="shorttext synchronized" lang="en">Business Partner Number</p>
  methods GET_NUMBER
    returning
      value(R_PARTNER) type BU_PARTNER .
  methods GET_GUID
    returning
      value(R_PARTNER_GUID) type BU_PARTNER_GUID .
  methods GET_ADDRESS_LIST
    exporting
      !E_ADDRESS_LIST type ANY TABLE .
  "! <p class="shorttext synchronized" lang="en">Get all partner relations</p>
  "!
  "! @parameter r_result | <p class="shorttext synchronized" lang="en"></p>
  methods GET_RELATIONS_LIST
    returning
      value(R_RESULT) type TY_RELATIONS_TAB .
  "! <p class="shorttext synchronized" lang="en">Get partner relations filtered by given Category</p>
  "!
  "! @parameter i_category | <p class="shorttext synchronized" lang="en">Relationship Category</p>
  "! @parameter r_result | <p class="shorttext synchronized" lang="en"></p>
  methods GET_RELATION_BY_CATEGORY
    importing
      !I_CATEGORY type BU_RELTYP
    returning
      value(R_RESULT) type TY_RELATIONS_TAB .
  methods GET_CONTACT_RELATION
    importing
      !I_WITH_PARTNER type BU_PARTNER
    returning
      value(R_RESULT) type ref to ZIF_CRM_CONTACT_RELATION .
  "! <p class="shorttext synchronized" lang="en">Get custom business partner extensions</p>
  "!
  "! @parameter i_extension_name | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter e_extension | <p class="shorttext synchronized" lang="en"></p>
  methods GET_EXTENSION_TABLE
    importing
      !I_EXTENSION_NAME type CRMT_RELATION_NAME
      !I_RECORD_GUID type AXT_RECORD_ID optional
    exporting
      !E_EXTENSION type ANY TABLE .
  "! <p class="shorttext synchronized" lang="en">Get business partner general information</p>
  "!
  "! @parameter r_result | <p class="shorttext synchronized" lang="en">BUT000 data</p>
  methods GET_GENERAL_DATA
    returning
      value(R_RESULT) type TY_GENERAL_DATA .
  methods CREATE_EXTENSION
    importing
      !I_EXTENSION_NAME type CSEQUENCE
      !I_VALUES type ANY
    returning
      value(R_KEY_STRUC) type AXTS_BOL_KEY_STRUC .
  methods DELETE_EXTENSION_RECORD
    importing
      !I_EXTENSION type STRING
      !I_RECORD_GUID type GUID
      !I_OLD_DATA type ANY optional
      !I_DATA type ANY optional
      !I_DEL type ABAP_BOOL .
  methods UPDATE_EXTENSION_RECORD
    importing
      !I_EXTENSION type CRMT_RELATION_NAME
      !I_RECORD_GUID type GUID
      !I_DATA type ANY optional .
  methods UPDATE_STD_ADDRESS
    importing
      !I_DATA type CRMST_ADDRESS_BUIL .
  methods GET_STANDARD_ADDRESS
    importing
      !I_BP_NO type BU_PARTNER optional
    exporting
      !E_EMAIL type AD_SMTPADR
    returning
      value(R_CRM_ADDRESS) type ref to ZIF_CRM_ADDRESS .
  methods UPDATE_HEADER_DATA
    importing
      !I_DATA type CRMST_HEADER_OBJECT_BUIL .
  methods UPDATE_ID_NUMBERS
    importing
      !I_DATA type CRMT_BUPA_IL_IDENTIFICATION .
endinterface.
