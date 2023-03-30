INTERFACE zif_crm_order_docs
  PUBLIC .


  INTERFACES zif_crm_entity .

  TYPES:
    BEGIN OF ty_doc_attr_s,
      guid           TYPE crmt_object_guid,
      file_id        TYPE sdok_docid,
      file_name      TYPE string,
      account        TYPE zbp_acc,
      author         TYPE zbp_upl_by,
      description    TYPE string,
      file_size      TYPE i,
      mime_type      TYPE string,
      file_content   TYPE xstring,
      file_date      TYPE crmt_cm_date,
      language       TYPE spras,
      is_order_print TYPE crmt_boolean,
    END OF ty_doc_attr_s .
  TYPES:
    ty_doc_attr_tt TYPE STANDARD TABLE OF ty_doc_attr_s WITH DEFAULT KEY .

  "! <p class="shorttext synchronized" lang="en">returns based on filter all available partner</p>
  "!
  "! @parameter i_partner_fct | <p class="shorttext synchronized" lang="en">filter for partner function</p>
  "! @parameter r_result | <p class="shorttext synchronized" lang="en">return all corresponding partner</p>
  METHODS upload_attachment
    IMPORTING
      !i_doc           TYPE zif_crm_order_attachm=>ty_doc_attr_s
    RETURNING
      VALUE(r_doc_key) TYPE sdok_docid .
  METHODS download_attachment
    IMPORTING
      !i_phio_id      TYPE sdok_docid
    RETURNING
      VALUE(r_result) TYPE ty_doc_attr_s .
  METHODS download_attachments
    RETURNING
      VALUE(r_result) TYPE ty_doc_attr_tt .
  METHODS get_file_list
    RETURNING
      VALUE(r_result) TYPE ty_doc_attr_tt .
  METHODS remove_attachment
    IMPORTING i_phio_id    TYPE sdok_docid
              i_order_guid TYPE crmt_object_guid.
ENDINTERFACE.
