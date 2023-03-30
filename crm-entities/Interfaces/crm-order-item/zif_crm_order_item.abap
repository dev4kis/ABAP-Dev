INTERFACE zif_crm_order_item
  PUBLIC .


  METHODS add_reference_object
    IMPORTING
              !i_reference_object TYPE any
              !i_field_name       TYPE name_komp
    RETURNING VALUE(r_ref_guid)   TYPE guid .
  METHODS remove_reference_object
    IMPORTING
      !i_refobj_val TYPE any
      !i_field_name TYPE name_komp  .
  METHODS status_i
    IMPORTING
      !i_new_status     TYPE crm_j_status OPTIONAL
      !i_current_status TYPE zcrm_order_status OPTIONAL
    RETURNING
      VALUE(r_status)   TYPE zcrm_order_status .
  METHODS modify_reference_object
    IMPORTING
      !i_old_refobj_val   TYPE crmt_object_guid
      !i_new_refobj_value TYPE crmst_refobj_btil
      !i_field_name       TYPE name_komp .
  METHODS add_extension_structure
    IMPORTING
      !i_extension_name  TYPE string
      !i_extension_value TYPE any .
  METHODS add_product_i
    IMPORTING
      !i_product_item TYPE crmst_producti_btil .
  METHODS modify_product_i
    IMPORTING
      !i_product_item TYPE crmst_producti_btil .
  METHODS get_orderadm_i_struct
    RETURNING
      VALUE(r_order_i_struct) TYPE crmst_admini_btil .
  METHODS get_extension_list
    IMPORTING
      !i_extension_name TYPE string
    EXPORTING
      !e_extension_list TYPE ANY TABLE .
  METHODS modify_extension_structure
    IMPORTING
      !i_extension_name  TYPE string
      !i_extension_value TYPE any .


  METHODS remove_extension_structure
    IMPORTING
      !i_extension_name  TYPE string
      !i_extension_value TYPE any .
ENDINTERFACE.
