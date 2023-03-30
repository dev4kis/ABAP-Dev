CLASS zcl_crm_partner_relation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    data from_partner type ref to zif_crm_partner READ-ONLY.
    data to_partner   type ref to zif_crm_partner READ-ONLY.

    data category type crmt_bupa_il_relation READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_crm_partner_relation IMPLEMENTATION.



ENDCLASS.
