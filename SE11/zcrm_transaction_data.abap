@EndUserText.label : 'CRM Transaction KKEy Data'
@AbapCatalog.enhancementCategory : #NOT_CLASSIFIED
define type zcrm_transaction_data {
  object    : crmt_ext_obj_name;
  objectkey : crmt_object_guid;
  saved     : crmt_boolean;

}
