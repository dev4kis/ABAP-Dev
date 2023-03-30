CLASS zcl_crm_order_attachm_bol DEFINITION
  PUBLIC
  INHERITING FROM zcl_crm_base_order_attachm
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_crm_api_entity_factory .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .

    METHODS zif_crm_entity~create
        REDEFINITION .
    METHODS zif_crm_entity~get_transaction_context
        REDEFINITION .
    METHODS zif_crm_entity~load_as
        REDEFINITION .
    METHODS zif_crm_order_attachm~download_attachment
        REDEFINITION .
    METHODS zif_crm_order_attachm~upload_attachment
        REDEFINITION .
    METHODS zif_crm_entity~get_entity_id
        REDEFINITION .
*
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA bol_core TYPE REF TO cl_crm_bol_core .

    CONSTANTS c_btx_bol_component TYPE crmt_genil_appl VALUE 'CM'.
    CONSTANTS c_bol_root_element TYPE crmt_ext_obj_name VALUE 'CMBO'.

    DATA entity_root TYPE REF TO cl_crm_bol_entity.

    METHODS switch_to_change_mode.
    METHODS save.
    METHODS prep_properties
      IMPORTING
        i_doc           TYPE zif_crm_order_attachm=>ty_doc_attr_s
      RETURNING
        VALUE(r_result) TYPE crmt_cmic_doc_attr.
    METHODS get_by_key
      IMPORTING
        i_object_key TYPE crmt_object_guid.
ENDCLASS.



CLASS ZCL_CRM_ORDER_ATTACHM_BOL IMPLEMENTATION.


  METHOD class_constructor.
    zcl_crm_order_attachm_bol=>bol_core = cl_crm_bol_core=>get_instance( ).

    IF zcl_crm_order_attachm_bol=>bol_core->is_started EQ abap_false.

      zcl_crm_order_attachm_bol=>bol_core->start_up( iv_appl_name = 'CM'
                                                     iv_display_mode_support = abap_true ).

    ENDIF.

    zcl_crm_order_attachm_bol=>bol_core->load_component( CONV crmt_component_name( zcl_crm_order_attachm_bol=>c_btx_bol_component ) ).
*    DATA v_genil_model type ref to if_genil_obj_model.
*    v_genil_model ?= CL_CRM_GENIL_MODEL_SERVICE=>get_runtime_model( ) .
*    data(v_lodaed_components) = v_genil_model->get_components_loaded( ).
  ENDMETHOD.


  METHOD get_by_key.
    DATA: object_guid TYPE crmt_object_guid.

    object_guid = i_object_key.
*    object_guid = '42010A7F004F1EDA9E83A74271897173'.
    TRY.
        me->entity_root = zcl_crm_order_attachm_bol=>bol_core->get_root_entity( iv_object_name = me->c_bol_root_element
                                                                                iv_object_guid = object_guid ).
      CATCH cx_crm_genil_model_error.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD prep_properties.
    DATA: lv_filename      TYPE string,
          lv_file_ext      TYPE string,
          lv_mime_type_fn  TYPE w3conttype,
          lv_file_fullname TYPE string,
          lv_content_type  TYPE w3conttype.


    CALL FUNCTION 'CRM_KW_SPLIT_FILENAME'
      EXPORTING
        iv_path      = i_doc-file_name
      IMPORTING
        ev_filename  = lv_filename
        ev_extension = lv_file_ext
        ev_mimetype  = lv_mime_type_fn.


    REPLACE ALL OCCURRENCES OF ';' IN lv_filename WITH '' IN CHARACTER MODE.
    CONCATENATE lv_filename '.' lv_file_ext INTO lv_file_fullname.
    IF i_doc-mime_type IS INITIAL.
      lv_content_type = lv_mime_type_fn.
    ELSE.
      lv_content_type = i_doc-mime_type.
    ENDIF.
    r_result = VALUE #( filename        = lv_file_fullname
                        content_type    = lv_content_type
                        content         = i_doc-file_content
                        kw_relative_url = i_doc-file_name
                        file_size       = i_doc-file_size
                        doc_date        = i_doc-file_date
                        language        = i_doc-language
*                        zdoc_owner      = i_doc-account
                        bp_org          = i_doc-account
                        bp_author       = i_doc-author
                        zis_order_print = i_doc-is_order_print
                         ) .

  ENDMETHOD.


  METHOD save.
    DATA(v_trans) = zcl_crm_order_attachm_bol=>bol_core->get_transaction(
                          iv_entity = me->entity_root
                      ).
    IF v_trans->check_save_needed( ) AND v_trans->check_save_possible( ).
      IF   v_trans->save( ) = 'X'.
        v_trans->commit(  ).
      ELSE.
        v_trans->rollback( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD switch_to_change_mode.
    IF  me->entity_root->is_locked( ) = abap_false.

      IF me->entity_root->switch_to_change_mode( ) <> abap_true.
        zcx_crm_app=>raise_foreign_lock( i_text = '' ).
      ENDIF.

      me->entity_root->lock( ).

      IF me->entity_root->is_locked( ) <> 'X'.
        zcx_crm_app=>raise_foreign_lock( i_text = '' ).
      ENDIF.

      IF me->entity_root->is_changeable( ) <> 'X'.
        zcx_crm_app=>raise_foreign_lock( i_text = '' ).
      ENDIF.


      DATA v_transaction TYPE REF TO    zcl_crm_base_transaction.
      DATA v_context TYPE zcrm_transaction_data.

      TRY.
          v_transaction ?= zcl_crm_transaction_bol=>get_instance(  ).
          v_context-object = me->entity_root->get_name(  ).
*          v_context-objectkey = me->entity_root->get_property_as_string( iv_attr_name =   )
          v_context-objectkey = me->zif_crm_entity~get_entity_id(  ).
          v_transaction->add( v_context ).
        CATCH cx_root.
          zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
      ENDTRY.


    ENDIF.
  ENDMETHOD.


  METHOD zif_crm_entity~create.
    FIELD-SYMBOLS <create_attr> TYPE crmt_cmic_rfolder_attr.   "crmt_cmic_rfolder_create.
    ASSIGN i_create_attr TO <create_attr>.

    DATA: v_params_t    TYPE crmt_name_value_pair_tab,
*
*
          v_doc_factory TYPE REF TO cl_crm_bol_entity_factory.
*    .
*
    v_params_t  = VALUE #( ( name = 'INSTID'        value = <create_attr>-instid )  "GuID of Crm Object
                           ( name = 'TYPEID'        value = <create_attr>-typeid ) " Business Object type:BUS2000116
                           ( name = 'CATID'         value = <create_attr>-catid ) "'BO'
                           ( name = 'NAME'          value = |Attachments Order No.: | )
                           ( name = 'DESCRIPTION'   value = |Attachments for Request No.: | )
                         ).

    TRY.
        super->zif_crm_entity~create( i_create_attr ).

        v_doc_factory = zcl_crm_order_attachm_bol=>bol_core->get_entity_factory( me->c_bol_root_element ).
        me->entity_root = v_doc_factory->create( v_params_t ).
        me->entity_root->set_property_as_string( iv_attr_name =  'DESCRIPTION'                " Component Name
                                                 iv_value     =  |{ <create_attr>-description }|
                                               ).
        me->entity_root->set_property_as_string( iv_attr_name =  'NAME'                " Component Name
                                                 iv_value     =  |{ <create_attr>-name }|
                                               ).
        me->bol_core->modify( ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_entity~get_entity_id.
    TRY.
        me->entity_root->get_property_as_value( EXPORTING iv_attr_name = 'INSTID' IMPORTING ev_result = r_result  ).
*      r_result = me->entity_root->get_key( ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_entity~get_transaction_context.
    TRY.
        r_result-object = me->entity_root->get_name(  ).
        r_result-objectkey = me->zif_crm_entity~get_entity_id(  ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_entity~load_as.
    TRY.
        super->zif_crm_entity~load_as( i_entity_id = i_entity_id ).

        get_by_key( i_object_key = i_entity_id ).
      CATCH cx_root.
        zcx_crm_app=>raise_failed( EXPORTING i_text = cl_bsp_get_text_by_alias=>get_text( EXPORTING alias = 'ZEXAS_COMMON/EXCEPTION_GENERAL_ERROR' language = sy-langu ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_crm_order_attachm~download_attachment.
    TRY.
        DATA(v_docs) = me->entity_root->get_related_entities( iv_relation_name = |CMBODocumentRefRel| ).

      CATCH cx_crm_genil_model_error. " Error when Accessing Object Model
    ENDTRY.
  ENDMETHOD.


  METHOD zif_crm_order_attachm~upload_attachment.
    DATA: v_entity      TYPE REF TO cl_crm_bol_entity,
          bpath         TYPE string,
          v_entity_prop TYPE crmt_cmic_doc_attr.

    TRY.
        bpath = |./CMBODocumentRefRel[@KW_RELATIVE_URL="{ i_doc-file_name }"]/*$|.

        v_entity = zcl_api_util=>get_and_create_entity_by_bpath(  i_entity = me->entity_root i_bpath = bpath  i_create = abap_true  ).

        CHECK v_entity IS BOUND.
        "remember to lock:
        switch_to_change_mode( ).
        DATA(entity_attr) =  prep_properties( i_doc ) .
*        DATA(v_doc_owner) = entity_attr-zdoc_owner.
*        CLEAR entity_attr-zdoc_owner.
        v_entity->set_properties( is_attributes =  entity_attr  ).

        zcl_crm_order_attachm_bol=>bol_core->modify( ).

        r_doc_key = v_entity->get_property_as_string( iv_attr_name = 'PHIO_OBJID' ).
*        v_entity->set_property_as_string( iv_attr_name = 'ZDOC_OWNER'
*                                          iv_value     = CONV #( v_doc_owner )
*                                         ).
*        zcl_crm_order_attachm_bol=>bol_core->modify( ).
        v_entity->get_properties( IMPORTING es_attributes =  v_entity_prop
        ).
        r_doc_key = v_entity_prop-phio_objid.
        DATA(v_doc_owner) = v_entity->get_property_as_string( iv_attr_name = 'BP_ORG' ).

      CATCH  cx_bol_exception INTO DATA(v_bol_exception).
        DATA(v_exc_text) = v_bol_exception->get_text( ).
        zcx_crm_app=>raise_failed( i_text = v_exc_text ).

      CATCH cx_sy_conversion_error INTO DATA(v_conv_exc). " System Exception Involving Conversion Errors
        v_exc_text = v_conv_exc->get_longtext(  ).
        zcx_crm_app=>raise_failed( i_text = v_exc_text ).

      CATCH cx_crm_genil_model_error INTO DATA(v_genil_model_exc).
        v_exc_text = v_genil_model_exc->get_longtext( ).
        zcx_crm_app=>raise_failed( i_text = v_exc_text ).

      CATCH cx_crm_genil_duplicate_rel INTO DATA(v_genil_dupl_exc).
        v_exc_text = v_genil_dupl_exc->get_longtext( ).
        zcx_crm_app=>raise_failed( i_text = v_exc_text ).

      CATCH cx_root INTO DATA(v_root_exc).
        v_exc_text = v_root_exc->get_longtext(  ).
        zcx_crm_app=>raise_failed( i_text = v_exc_text ).

    ENDTRY.

*    me->save( ).
  ENDMETHOD.
ENDCLASS.
