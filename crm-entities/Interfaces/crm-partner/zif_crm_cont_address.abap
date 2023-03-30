INTERFACE zif_crm_cont_address
  PUBLIC .


  METHODS get_main_address_data .
  METHODS get_standard_phone .
  METHODS set_standard_phone
    IMPORTING
      !i_country    TYPE ad_comctry
      !i_countryiso TYPE intca OPTIONAL
      !i_telephone  TYPE ad_tlnmbr
      !i_extension  TYPE ad_tlxtns
      !i_save       TYPE crmt_boolean DEFAULT abap_false .
  METHODS set_standard_function
    IMPORTING
      !i_function TYPE bu_fnctn
      !i_save     TYPE crmt_boolean DEFAULT abap_false .
ENDINTERFACE.
