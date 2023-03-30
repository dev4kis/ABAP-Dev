CLASS zcl_crm_order_attachm_fuba DEFINITION
  PUBLIC
  INHERITING FROM zcl_crm_base_order_attachm
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_crm_entity~load_as
        REDEFINITION .
    METHODS zif_crm_order_attachm~download_attachment
        REDEFINITION .
    METHODS zif_crm_order_attachm~download_attachments
        REDEFINITION .
    METHODS zif_crm_order_attachm~get_file_list
        REDEFINITION .
    METHODS zif_crm_order_attachm~remove_attachment
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: order_guid       TYPE crmt_object_guid,
          doc_objects_info TYPE skwf_ios,
          file_properties  TYPE crm_kw_propst.

    METHODS: get_by_key
      IMPORTING
        i_object_key TYPE any
        i_catid      TYPE sibfcatid DEFAULT 'BO'
*        i_typeid     TYPE sibftypeid
      ,
      extract_document
        IMPORTING
                  i_doc_objects_info TYPE skwf_ios
*                  i_properties_only  TYPE abap_bool
        RETURNING VALUE(r_doc_tab)
                    TYPE zif_crm_order_attachm=>ty_doc_attr_tt,
      get_file
        IMPORTING
          i_doc_info        TYPE skwf_io
          i_properties_only TYPE abap_bool
        RETURNING
          VALUE(r_file)     TYPE zif_crm_order_attachm=>ty_doc_attr_s,
*      get_files_properties
*        IMPORTING
*          i_doc_info    TYPE skwf_io
*        RETURNING
*          VALUE(r_file) TYPE zif_crm_order_attachm=>ty_doc_attr_s,
      convert_to_xstring
        IMPORTING
          i_content       TYPE sdokcntbins
          i_file_size     TYPE i
        RETURNING
          VALUE(r_result) TYPE xstring,
      delete_document
        IMPORTING
          i_order_guid TYPE crmt_object_guid
          i_file_obj   TYPE skwf_io.

ENDCLASS.



CLASS ZCL_CRM_ORDER_ATTACHM_FUBA IMPLEMENTATION.


  METHOD convert_to_xstring.
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = i_file_size
*       FIRST_LINE   =
*       LAST_LINE    = 0
      IMPORTING
        buffer       = r_result
      TABLES
        binary_tab   = i_content
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.


    IF sy-subrc <> 0.
        zcx_crm_app=>raise_failed( i_text = 'Conversion of Binary to String' ).
    ENDIF.
  ENDMETHOD.


  METHOD delete_document.
    cl_crm_documents=>delete(
      EXPORTING
        business_object =   VALUE #( objtype = VALUE #( catid = 'BO' typeid = 'BUS2000116' ) instid = i_order_guid )               " Local Persistent Object Reference - BOR-Compatible
        ios             =           VALUE #( ( i_file_obj ) )       " PHIOs/LOIOs to be Deleted
*    delete_children =                  " Relevant Only if IOs are Transferred: Delete Dependent Doc.
*    delete_folders  = space            " Only Relevant if Only BO Transferred: Delete Folder as Well
  IMPORTING
    bad_ios         =           DATA(v_bad_ios)       " KW Framework: Error Info for an Info Object
    error           =           DATA(v_error)       " KW Framework: Error Object
    ).

    CHECK v_error IS NOT INITIAL.
    " handle Error
  ENDMETHOD.


  METHOD extract_document.
    LOOP AT i_doc_objects_info ASSIGNING FIELD-SYMBOL(<doc_info>).
      DATA(v_file) =  get_file( i_doc_info = <doc_info> i_properties_only = abap_false ).

      APPEND v_file TO r_doc_tab.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_by_key.
    SELECT SINGLE object_type FROM crmd_orderadm_h INTO @DATA(v_type) WHERE guid = @i_object_key.
    IF sy-subrc <> 0.
      SELECT SINGLE @abap_true INTO @DATA(is_product) FROM comm_product WHERE product_guid = @i_object_key.
      IF sy-subrc = 0 and is_product = abap_true.
        v_type = 'BUS1178'. " Move to IF
      ENDIF.
    ENDIF.

    cl_crm_documents=>get_info(
          EXPORTING
            business_object       = VALUE #( catid = i_catid typeid = v_type instid = i_object_key )            " Local Persistent Object Reference - BOR-Compatible
          IMPORTING
            phios                 = doc_objects_info              " Table with PHIOs
            ios_properties_result = file_properties
        ).


  ENDMETHOD.


  METHOD get_file.
    DATA: tz  TYPE ttzz-tzone.
    cl_crm_documents=>get_document(
      EXPORTING
            io                = i_doc_info                      " KW Framework: Object Key
      IMPORTING
            properties        = DATA(v_doc_properties)          " Document Properties
            properties_attr   = DATA(v_doc_prop_attr)
            file_access_info  = DATA(v_file_access_info)        " SDOK:  Details on Document Contents in Internal Tables
            content_ascii     = DATA(v_content_ascii)           " Table with Lines from Text Document Contents for Web Server
            content_bin       = DATA(v_content_bin)             " SDOK: Table of Binary Document Contents for Web Server
            business_objects  = DATA(v_bos)                     " Local Persistent Object Reference - BOR-Compatible
            io_does_not_exist =   DATA(v_not_exist)             " IO Does not Exist/has been Deleted
*    properties_ext    =                                        " Document properties for properties extending threshold value
*    flag              =                                        " Boolean Variable (X=True, -=False, Space=Unknown)
    ).
*cl_http_utility=>unescape_url( escaped = CONV #( v_doc_properties[ name = 'FILE_NAME' ]-value ) )
    r_file = VALUE #( guid              = me->order_guid
                      file_id           = i_doc_info-objid
                      file_name         = v_doc_properties[ name = 'FILE_NAME' ]-value
                      account           = VALUE #( v_doc_prop_attr[ field_name = 'BP_ORG' ]-field_value OPTIONAL )
                      author            = VALUE #( v_doc_prop_attr[ field_name = 'BP_AUTHOR' ]-field_value OPTIONAL )
                      is_order_print    = VALUE #( v_doc_prop_attr[ field_name = 'ZIS_ORDER_PRINT' ]-field_value OPTIONAL )
                      file_size         = v_doc_properties[ name = 'FILE_SIZE' ]-value
                      mime_type         = VALUE #( v_doc_properties[ name = 'MIME_TYPE' ]-value OPTIONAL )
                      language          = v_doc_properties[ name = 'LANGUAGE' ]-value ).
    IF r_file-mime_type IS INITIAL.
      r_file-mime_type = VALUE #( v_file_access_info[ file_name = r_file-file_name ]-mimetype OPTIONAL ).
    ENDIF.
    CONVERT TIME STAMP CONV timestamp( v_doc_properties[ name = 'CREATED_AT' ]-value ) TIME ZONE tz
        INTO DATE r_file-file_date TIME DATA(tim)
        DAYLIGHT SAVING TIME DATA(dst).
    IF i_properties_only = abap_false
      .
      r_file-file_content = convert_to_xstring( i_content = v_content_bin i_file_size = r_file-file_size ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_crm_entity~load_as.
    me->order_guid = i_entity_id.
    super->zif_crm_entity~load_as( i_entity_id = order_guid ).
    me->get_by_key( i_object_key = order_guid ).
  ENDMETHOD.


  METHOD zif_crm_order_attachm~download_attachment.

    CHECK doc_objects_info IS NOT INITIAL.
    IF line_exists( doc_objects_info[ objid = i_phio_id ] ).

      DATA(v_files_tab) = extract_document( i_doc_objects_info = VALUE #( ( doc_objects_info[ objid = i_phio_id ] ) ) ).

      CHECK v_files_tab IS NOT INITIAL.

      r_result = v_files_tab[ 1 ].
    ENDIF.
  ENDMETHOD.


  METHOD zif_crm_order_attachm~download_attachments.

    CHECK doc_objects_info IS NOT INITIAL.

    DATA(v_files_tab) = extract_document( i_doc_objects_info = doc_objects_info ).

    CHECK v_files_tab IS NOT INITIAL.

    r_result = v_files_tab.

  ENDMETHOD.


  METHOD zif_crm_order_attachm~get_file_list.

    CHECK me->doc_objects_info IS NOT INITIAL.
    LOOP AT me->doc_objects_info ASSIGNING FIELD-SYMBOL(<doc_obj>).
      DATA(v_file_props) = me->get_file(
                             i_doc_info        = <doc_obj>
                             i_properties_only = abap_true
                           ).
      APPEND v_file_props  TO r_result.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_crm_order_attachm~remove_attachment.
    TRY.
        DATA(v_file2del) = me->doc_objects_info[ objid = i_phio_id ].

        delete_document( i_order_guid = i_order_guid i_file_obj = v_file2del ).
      CATCH cx_sy_itab_line_not_found INTO DATA(v_file_not_found).
       zcx_crm_app=>raise_not_found(  i_text  = v_file_not_found->get_longtext( ) ).

    ENDTRY.
  ENDMETHOD.
ENDCLASS.
