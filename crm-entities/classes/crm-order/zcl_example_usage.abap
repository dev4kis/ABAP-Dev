CLASS zcl_ibase_tur_api DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_order_ov_flags,
        request_guid                TYPE crmt_object_guid,
        show_tech_contact           TYPE abap_bool,
        edit_tech_contact           TYPE abap_bool,
        show_3rd_party_tech_contact TYPE abap_bool,
        edit_3rd_party_tech_contact TYPE abap_bool,
        appr_rej_buttons            TYPE abap_bool,
      END OF ty_order_ov_flags .
    TYPES:
      BEGIN OF ty_next_action_by_s,
        request_guid   TYPE crmt_object_guid,
        request_status TYPE crm_j_status,
        next_action_by TYPE char255,
      END OF ty_next_action_by_s,
           BEGIN OF ty_line_serv_comb,
        product_id      TYPE comt_product_id,
        service_id      TYPE z_service_id_new,
        mic_line_id     TYPE zib_mul_logical_id,
        installation_id TYPE ib_ibase,
      END OF ty_line_serv_comb .
    TYPES:
      ty_line_serv_comb_tt TYPE TABLE OF ty_line_serv_comb .
       .
    TYPES:
      ty_next_action_tt TYPE STANDARD TABLE OF ty_next_action_by_s WITH DEFAULT KEY .
    TYPES:
      ty_requests_tt    TYPE STANDARD TABLE OF zib_pt_orders_v WITH DEFAULT KEY .
    TYPES:
      ty_open_requests_tt  TYPE STANDARD TABLE OF zbtx_tur_open_requests WITH DEFAULT KEY .
    TYPES:
      ty_missing_approvals   TYPE STANDARD TABLE OF zv_order_appr_st WITH DEFAULT KEY .
    TYPES:
      ty_order_installations TYPE STANDARD TABLE OF zib_tur_odata_installation_s WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_splitted_requests,
        id        TYPE crmt_object_id,
        guid      TYPE crmt_object_guid,
        order_api TYPE REF TO zcl_ib_tur_order,
        status    TYPE j_estat,
      END OF ty_splitted_requests .
    TYPES:
      ty_splitted_requests_tt TYPE STANDARD TABLE OF ty_splitted_requests WITH DEFAULT KEY .

    METHODS get_dynamic_print
      IMPORTING
        !i_request_guid    TYPE crmt_object_guid
        !i_company_partner TYPE bu_partner
      RETURNING
        VALUE(r_pdf)       TYPE ztur_order_attachment .
    CLASS-METHODS get_instance
      IMPORTING
        !i_access_method  TYPE char1 DEFAULT '1'
      RETURNING
        VALUE(r_instance) TYPE REF TO zcl_ibase_tur_api .

        METHODS generate_print_for_splits
      IMPORTING
        !i_guid     TYPE crmt_object_guid
        !i_req_type TYPE zpt_order_req_type .
    METHODS get_connection_details
      IMPORTING
        !i_product_guid       TYPE comt_product_guid
        !i_service_id         TYPE comt_product_id
      RETURNING
        VALUE(r_conn_details) TYPE zib_odata_connection_s .
    METHODS get_mic_connection_details
      IMPORTING
        !i_product_guid     TYPE comt_product_guid OPTIONAL
        !i_mic_id           TYPE zib_mul_logical_id
      EXPORTING
        !e_mic_conn_details TYPE ANY TABLE .
    METHODS create_request
      CHANGING
        !c_order         TYPE zbtx_tur_request_s
      RETURNING
        VALUE(r_request) TYPE REF TO zif_ib_tur_order .
    METHODS process_approval_step
      CHANGING
        !c_approval_step TYPE zbtx_tur_order_approval_steps
      RETURNING
        VALUE(result)    TYPE abap_bool .
    METHODS format_session_installation
      IMPORTING
        !i_request_guid       TYPE crmt_object_guid
        !i_company_partner    TYPE bu_partner
        !i_service_id         TYPE z_service_id_new
      CHANGING
        !c_order_installation TYPE ty_order_installations .
    METHODS add_partner
      IMPORTING
        !i_partner      TYPE zbtx_tur_partner_s
        !i_request_guid TYPE crmt_object_guid .
    METHODS get_request
      IMPORTING
        !i_key           TYPE any
      RETURNING
        VALUE(r_request) TYPE REF TO zif_ib_tur_order .
    METHODS save
      IMPORTING
        !i_commit TYPE abap_bool .
    METHODS configure_entitlements
      IMPORTING
        !i_entitlements  TYPE any
      RETURNING
        VALUE(r_request) TYPE REF TO zif_ib_tur_order .
    METHODS add_location_details
      IMPORTING
        !i_request_guid TYPE any OPTIONAL
        !i_loc_details  TYPE zbtx_tur_order_loc_det_s .
    METHODS ordered_services
      IMPORTING
        !i_order_api           TYPE REF TO zif_ib_tur_order OPTIONAL
        !i_services            TYPE any
        !i_request_guid        TYPE crmt_object_guid OPTIONAL
      RETURNING
        VALUE(r_order_service) TYPE REF TO zif_crm_order_item .
    METHODS add_items_2_order
      IMPORTING
        !i_item    TYPE any
        !i_request TYPE REF TO zif_ib_tur_order .
    METHODS add_attachment
      IMPORTING
        !i_request_guid    TYPE crmt_object_guid
        !i_attachment      TYPE ztur_order_attachment
        !i_account         TYPE bu_partner OPTIONAL
      RETURNING
        VALUE(r_file_info) TYPE ztur_order_attachment .
    METHODS delete_attachment
      IMPORTING
        !i_request_guid TYPE crmt_object_guid
        !i_file_id      TYPE sdok_docid .
    METHODS get_attachm_as_media
      IMPORTING
        !i_guid       TYPE crmt_object_guid
        !i_file_id    TYPE sdok_docid OPTIONAL
      RETURNING
        VALUE(r_file) TYPE zib_media_resource_tt .
    METHODS get_attachments
      IMPORTING
        !i_guid        TYPE crmt_object_guid
        !i_order_print TYPE abap_bool DEFAULT abap_false
        !i_file_id     TYPE sdok_docid OPTIONAL
      RETURNING
        VALUE(r_files) TYPE zib_media_resource_tt .
    METHODS determine_overview_flags
      IMPORTING
        !i_company_partner TYPE bu_partner
        !i_request_guid    TYPE crmt_object_guid
      RETURNING
        VALUE(r_ov_flags)  TYPE ty_order_ov_flags .
    METHODS get_request_status
      IMPORTING
        !irequest_guid  TYPE crmt_object_guid
      RETURNING
        VALUE(r_status) TYPE crm_j_status .
    METHODS get_partner_fct
      IMPORTING
        !i_request_guid      TYPE crmt_object_guid
        !i_partner           TYPE bu_partner
      RETURNING
        VALUE(r_partner_fct) TYPE crmt_partner_fct .
    METHODS get_open_approvals
      IMPORTING
        !i_object_guid               TYPE crmt_object_guid
      RETURNING
        VALUE(r_missing_approvals_t) TYPE ty_missing_approvals .
    METHODS get_requests_splits
      IMPORTING
        !i_object_guid            TYPE crmt_object_guid
      RETURNING
        VALUE(r_related_requests) TYPE crmt_object_guid_tab .
    METHODS get_related_request
      IMPORTING
        !i_request_guid                TYPE crmt_object_guid
      RETURNING
        VALUE(r_related_request_guids) TYPE crmt_object_guid_tab .
    METHODS determine_next_action_by
      IMPORTING
        !i_company_partner TYPE bu_partner
      CHANGING
        !c_nex_action_by   TYPE ty_next_action_tt .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO zcl_ibase_tur_api.
    CLASS-DATA access_method TYPE zcrm_api_access_method .
    DATA crm_entity TYPE REF TO zcl_crm_entity_api .
    DATA order_api TYPE REF TO zif_crm_order .
    DATA document_api TYPE REF TO zif_crm_order_attachm .

    METHODS constructor
      IMPORTING
        !i_access_method TYPE zcrm_api_access_method DEFAULT '1' .
    METHODS check_requires_legal_apprvl
      IMPORTING
        !i_object_guid  TYPE crmt_object_guid
        !i_partner      TYPE bu_partner
      RETURNING
        VALUE(r_result) TYPE abap_bool .
    METHODS check_split_needed
      IMPORTING
        !i_object_guid  TYPE crmt_object_guid
      RETURNING
        VALUE(r_result) TYPE abap_bool .
    METHODS do_split
      IMPORTING
        !i_original_request_guid TYPE crmt_object_guid
      RETURNING
        VALUE(r_splits)          TYPE zcl_ibase_tur_api=>ty_splitted_requests_tt .
    METHODS create_split_request
      IMPORTING
        i_origin_guid    TYPE crmt_object_guid
        i_services       TYPE ANY TABLE
      RETURNING
        VALUE(r_request) TYPE ty_splitted_requests .
    METHODS get_order_print
      IMPORTING i_order_guid         TYPE crmt_object_guid
      RETURNING
                VALUE(r_order_print) TYPE ztur_order_attachment.
    METHODS clean_up_text
      CHANGING
        c_notes TYPE zbtx_tur_order_notes.




ENDCLASS.



CLASS ZCL_IBASE_TUR_API IMPLEMENTATION.


  METHOD add_attachment.

    DATA(v_request) = CAST zcl_ib_tur_order( me->get_request( i_key = i_request_guid ) ).

    DATA(v_request_h) = v_request->order_api->get_orderadm_h_extension_struc( ).

    DATA(v_name) = |Attachments Order No.: { v_request_h-object_id }|.

    DATA(v_description) = |Attachments for Request No.: { v_request_h-object_id } - { v_request_h-description }|.

    me->document_api =  me->crm_entity->document->create_document( i_instid      = |{ v_request_h-guid }|
                                                                   i_typeid      = |{ v_request_h-object_type }|
                                                                   i_catid       = 'BO'
                                                                   i_name        = v_name
                                                                   i_description = v_description
                 ).
    DATA(v_doc) = CORRESPONDING zif_crm_order_attachm=>ty_doc_attr_s( i_attachment ).
    v_doc-guid        = i_request_guid.
    v_doc-language    = sy-langu.
    v_doc-description = v_description.
*    v_doc-account     = i_account .
*    v_doc-author      = i
    v_doc-file_date   = sy-datum.
    v_doc-file_size = xstrlen( v_doc-file_content ).

    v_doc-file_id = me->document_api->upload_attachment( i_doc = v_doc  ).
    v_doc-file_content = ''.
    r_file_info = CORRESPONDING #( v_doc ).
*    r_file_info-file_name = cl_http_utility=>unescape_url( escaped = conv #( r_file_info-file_name ) ).

  ENDMETHOD.


  METHOD add_items_2_order.
*Create product for the item: - Not necessary:
*    DATA(v_product_api) = me->crm_entity->product->create_product( i_product_type = '02' i_description = 'NEW LINE' i_root_hierarchy_id = 'CRM_HIER_1' i_root_category_id = 'PORTAL_ORDER_NEW' ).
*    DATA(v_product_guid) = v_product_api->get_product_guid( ).

*    i_request->item( i_product = v_product_guid i_item_cat = 'ZPI1' ).

*    i_request->product_item( i_item ).
*    r_request->services(  ).

  ENDMETHOD.


  METHOD add_location_details.
    DATA(v_order) =  me->get_request( i_key =  i_request_guid ).
    v_order->location_details(  i_loc_details = i_loc_details ).
  ENDMETHOD.


  METHOD add_partner.

*    DATA(request) = cast zcl_ib_tur_order( me->get_request( i_key =  i_request_guid  ) ).
*
*   request->add_partner(
*     EXPORTING
*       i_partner     = i_partner
*       i_partner_fct = ''
*   ).
  ENDMETHOD.


  METHOD check_requires_legal_apprvl.
    DATA v_contracts_t TYPE zbtx_mds_contracts_tt .
    r_result = abap_false.

    SELECT SINGLE os~* FROM zib_tur_order_services_cds AS os
     JOIN zpt_serv_setup9 AS ss9 ON os~service_id = ss9~service_id
    WHERE  os~request_guid = @i_object_guid AND os~request_type IN ( 'PORT001','PORT004','PORT005' )
     INTO @DATA(v_ordered_services).

    CHECK sy-subrc = 0. " A service that requires Dissemination Agreement was ordered
    " Check if requesting partner has active Data Agreement
    CALL FUNCTION 'ZBTX_MDS_GET_BP_CONTRACTS'
      EXPORTING
        it_partner       = VALUE bu_partner_t( ( partner = i_partner ) )
        it_contract_type = VALUE zbtx_mds_contract_tt( ( contract = zif_const_btx_proc_type=>gc_proc_type_mds_contract contract_type = '115' ) )
        iv_status        = zif_const_btx_mds=>gc_status_active-active                " User Status
      IMPORTING
        et_contracts     = v_contracts_t.

    CHECK v_contracts_t IS INITIAL.
    r_result = abap_true.
  ENDMETHOD.


  METHOD check_split_needed.
    r_result = abap_false.

    SELECT SINGLE @abap_true  FROM zib_tur_order_services_cds
      WHERE service_id NOT IN ( SELECT service_id FROM zpt_serv_setup9 )
        AND request_guid = @i_object_guid
*{ BEGIN INSERT EXTREEH; CCDV-892; Shared services should not be considered in the decision if to split or not
        AND bandwidth NE 'SHARED'
        and bandwidth NE '0 Kbit/s'
        and bandwidth NE ''

*} END EXTREEH
    INTO @DATA(v_split_needed).

    CHECK sy-subrc = 0 AND v_split_needed = abap_true. "totology
    r_result = v_split_needed.
  ENDMETHOD.


  METHOD clean_up_text.
    SPLIT c_notes-text AT ':' INTO DATA(v_prefix)
                                           DATA(v_created_by_comment).

    DATA(v_formatted_prefix) = replace( val = v_prefix
                                               sub = `*`
                                               with = ``
                                               occ = 0 ).
    DATA(v_condensed_comment) = condense( val = v_created_by_comment
                                           del = ` ` ).

    DATA(v_formatted_comment) = replace( val = v_condensed_comment
                                         sub = ` = `
                                         with = ``
                                         occ = 0 ).


    SPLIT v_formatted_comment AT '*' INTO DATA(v_created_by)
                                           DATA(v_comment).

    v_created_by = shift_left( v_created_by ).
    v_comment = shift_left( v_comment ).
*    CONCATENATE v_formatted_comment
    c_notes-text = |{ v_formatted_prefix }:| && |{ v_created_by } | && |{ v_comment }|.
  ENDMETHOD.


  METHOD configure_entitlements.

    DATA(v_order) = me->get_request( i_key =  '' ).

*    v_order->services( EXPORTING i_service = i_entitlements i_company_account = ''  ).

    r_request = v_order.
  ENDMETHOD.


  METHOD constructor.

    me->access_method = i_access_method.
    me->crm_entity = NEW zcl_crm_entity_api( i_access_method = i_access_method ).
*

  ENDMETHOD.


  METHOD create_request.
    DATA(v_order_h) = CORRESPONDING zbtx_tur_request_s( c_order ).
    me->order_api =  me->crm_entity->order->create_order( i_process_type = zif_const_btx_proc_type=>gc_proc_type_portal_order ). "zif_const_btx_proc_type=>gc_proc_type_exas_req

    DATA(v_request) = NEW zcl_ib_tur_order( i_order_api = me->order_api ).

    v_order_h-description = v_request->description( i_req_type = v_order_h-request_type ).

    r_request = v_request.
    r_request->company_partner( i_partner = v_order_h-company_partner ).

    r_request->author(  i_partner = v_order_h-author ).

    GET TIME STAMP FIELD DATA(v_timestamp).
    v_order_h-created_on = v_timestamp. "sy-datum
    v_order_h-status = v_request->get_status( )-status.
    DATA(v_req_guid) = v_request->get_request_guid( ).
*    v_order_h-
    c_order = v_order_h.

  ENDMETHOD.


  METHOD create_split_request.
    DATA v_services TYPE zib_tur_odata_service_tt.
    "create header
    SELECT SINGLE * FROM zbtx_tur_order_b INTO @DATA(v_header) WHERE order_guid = @i_origin_guid.
    CHECK sy-subrc = 0.
    DATA(v_order_h) = CORRESPONDING zbtx_tur_request_s( v_header ).

    v_services = CORRESPONDING #( i_services ).

    r_request-order_api = CAST #( me->create_request(
                                CHANGING
                                  c_order = v_order_h
                              ) ).
    r_request-guid = r_request-order_api->get_request_guid( ).
    r_request-id = r_request-order_api->get_request_id( ).
    CHECK r_request-order_api IS BOUND.

    SELECT * FROM zib_tur_order_installations INTO TABLE @DATA(v_order_installations) FOR ALL ENTRIES IN @v_services WHERE  request_guid = @v_services-request_guid
    AND line_item_no = @v_services-line_item_no.

*    "Fetch virtual ibs:
    SELECT * FROM ztur_virt_ib_tmp INTO TABLE @DATA(v_order_instas_virt) FOR ALL ENTRIES IN @v_services WHERE  request_guid = @v_services-request_guid
    AND line_item_no = @v_services-line_item_no.

    LOOP AT v_services ASSIGNING FIELD-SYMBOL(<serv>).
      DATA(v_line_item_old) = <serv>-line_item_no.
      DATA(v_request_guid_old) = <serv>-request_guid.
      DATA(v_request_id_old) = <serv>-request_id.

      CLEAR: <serv>-line_item_no.
      <serv>-request_guid = r_request-guid.
      <serv>-request_id = r_request-id.

      r_request-order_api->zif_ib_tur_order~services(
        EXPORTING
          i_service         = CORRESPONDING #( <serv> )
          i_company_account = v_order_h-company_partner
*        i_product         =
      RECEIVING
        r_order_service   = DATA(v_order_item_api)
      ).
      CHECK v_order_item_api IS BOUND.
      DATA(v_line_item_new) = v_order_item_api->get_orderadm_i_struct( )-number_int.

      DATA(v_real_ib) = VALUE zib_tur_order_installations( v_order_installations[ request_guid = v_request_guid_old line_item_no = v_line_item_old  ] OPTIONAL ).
      IF v_real_ib IS NOT INITIAL.
        v_real_ib-line_item_no = v_line_item_new.
        DATA(v_ib) = CORRESPONDING  zbtx_tur_order_inst_s(  v_real_ib MAPPING line_item_no = line_item_no  ) .
        CASE v_real_ib-ibtyp.
          WHEN 'Z3'.
            v_ib-installation_sub = v_real_ib-ibase.
          WHEN 'Z2'.
            v_ib-installation_base = v_real_ib-ibase.
        ENDCASE.
        r_request-order_api->zif_ib_tur_order~installations(
          EXPORTING
            i_installations  = v_ib
            i_item_no        = v_line_item_new
            i_order_item_api = v_order_item_api
*          RECEIVING
*            r_record_guid    =
        ).
      ELSE.
        DATA(v_virt_ib) = VALUE #( v_order_instas_virt[ request_guid = v_request_guid_old line_item_no = v_line_item_old  ] OPTIONAL ).
        IF v_virt_ib IS NOT INITIAL.
          v_virt_ib-request_guid = r_request-order_api->get_request_guid( ).
          v_virt_ib-line_item_no = v_line_item_new.
          INSERT INTO ztur_virt_ib_tmp VALUES v_virt_ib.
        ENDIF.
      ENDIF.
    ENDLOOP.
    r_request-order_api->bind_related_request( i_parent_guid = i_origin_guid ).
  ENDMETHOD.


  METHOD delete_attachment.
    DATA(v_guid) = i_request_guid.
    me->crm_entity->document->get_document( i_key = v_guid )->remove_attachment(
      EXPORTING
        i_phio_id    = i_file_id
        i_order_guid = v_guid
    ).
  ENDMETHOD.


  METHOD determine_next_action_by.
*************Notes:  SPCRC-2375
* - In Status Draft, only requester sees request and only next action by contains only requester
* - In Status Wait Approval, Request sees everything contained in field next action by
* - Provider if any sees only their names if they haven't approved yet, otherwise missing coop exchange approvals if any, otherwise empty
* - Coop Exchange sees only their names in approval from them is still missing, otherwise empty
    CHECK c_nex_action_by IS NOT INITIAL.
    SELECT request_guid,
               company_partner,
               name_org1,
               function,
               approval_step,step_status
        FROM zib_tur_order_appr_steps AS _as
        JOIN but000 AS b
            ON _as~company_partner = b~partner
            FOR ALL ENTRIES IN @c_nex_action_by
             WHERE request_guid = @c_nex_action_by-request_guid
*               AND step_status = ''
        INTO TABLE @DATA(v_open_approvals) .
    CHECK sy-subrc = 0.
    SORT v_open_approvals BY request_guid company_partner approval_step.
    DELETE ADJACENT DUPLICATES FROM v_open_approvals COMPARING request_guid company_partner approval_step.


    DATA v_next_action TYPE zib_tur_odata_request_s-next_action_by.
    DATA v_partner_role TYPE crmt_partner_fct.

    LOOP AT c_nex_action_by ASSIGNING FIELD-SYMBOL(<v_>).
      TRY.
          v_partner_role = v_open_approvals[ company_partner = i_company_partner request_guid = <v_>-request_guid ]-function.

*                                  ).
          IF <v_>-request_status = zif_ib_tur_const=>co_tur_order_status-draft.

            CHECK v_partner_role = zif_ib_tur_const=>c_partner_functions-company_partner.
            <v_>-next_action_by = v_open_approvals[ company_partner = i_company_partner function = zif_ib_tur_const=>c_partner_functions-company_partner  ]-name_org1.

          ELSEIF <v_>-request_status = zif_ib_tur_const=>co_tur_order_status-waiting_approval.
            CASE v_partner_role.
              WHEN zif_ib_tur_const=>c_partner_functions-company_partner.
                LOOP AT v_open_approvals ASSIGNING FIELD-SYMBOL(<o>) WHERE request_guid = <v_>-request_guid AND step_status = ''.
                  IF v_next_action IS INITIAL.
                    v_next_action = |{ <o>-name_org1 }| .
                  ELSE.
                    v_next_action = |{ v_next_action }, { <o>-name_org1 }|.
                  ENDIF.
                ENDLOOP.
                <v_>-next_action_by = v_next_action.
                CLEAR v_next_action.
              WHEN zif_ib_tur_const=>c_partner_functions-provider.
                <v_>-next_action_by = VALUE #( v_open_approvals[ company_partner = i_company_partner
                                                                 function = zif_ib_tur_const=>c_partner_functions-provider  step_status = '' ]-name_org1 OPTIONAL ).
                IF <v_>-next_action_by IS INITIAL.
                  LOOP AT v_open_approvals ASSIGNING <o> WHERE request_guid = <v_>-request_guid AND step_status = ''.
                    IF v_next_action IS INITIAL.
                      v_next_action = |{ <o>-name_org1 }| .
                    ELSE.
                      v_next_action = |{ v_next_action }, { <o>-name_org1 }|.
                    ENDIF.
                  ENDLOOP.
                  <v_>-next_action_by = v_next_action.
                  CLEAR v_next_action.
                ENDIF.
              WHEN zif_ib_tur_const=>c_partner_functions-partner_exch.
                <v_>-next_action_by = VALUE #( v_open_approvals[ company_partner = i_company_partner
                                                                function = zif_ib_tur_const=>c_partner_functions-partner_exch  step_status = '' ]-name_org1 OPTIONAL ).
            ENDCASE.
          ELSEIF <v_>-request_status = zif_ib_tur_const=>co_tur_order_status-wait_legal_appr.
            <v_>-next_action_by = 'Deutsche Börse AG'.
          ENDIF.
        CATCH cx_sy_itab_line_not_found INTO DATA(v_line_nf_exc).
          DATA(v_tex) = v_line_nf_exc->get_longtext( ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD determine_overview_flags.

    DATA(v_guid) = i_request_guid.
    DATA(v_company) = i_company_partner.

    SELECT SINGLE @abap_true INTO @DATA(v_exist) FROM zbtx_tur_order_b WHERE order_guid = @v_guid.
    IF v_exist = abap_true.

      SELECT SINGLE request_status, step_status, function INTO @DATA(v_) FROM zib_tur_order_appr_steps WHERE request_guid = @v_guid AND company_partner = @v_company.

      CHECK sy-subrc = 0.
      r_ov_flags-request_guid = v_guid.

      CASE v_-step_status.

        WHEN ''.    " Approval processing is still pending
          r_ov_flags-appr_rej_buttons = abap_true.

          IF v_-function = zif_ib_tur_const=>c_partner_functions-company_partner.
            " Requester has not yet approved:
            r_ov_flags-show_tech_contact = abap_true.
            r_ov_flags-edit_tech_contact = abap_true.

          ELSE. " the partner viewing the request is not the Requester itself, then Requester has already approved

            r_ov_flags-show_3rd_party_tech_contact = abap_true.
            r_ov_flags-edit_3rd_party_tech_contact = abap_true.

          ENDIF.

        WHEN OTHERS. " Approval Step has been processed

          r_ov_flags-appr_rej_buttons = abap_false.

          IF v_-function = zif_ib_tur_const=>c_partner_functions-company_partner.

            r_ov_flags-show_tech_contact = abap_true.
            r_ov_flags-edit_tech_contact = abap_false.

            r_ov_flags-show_3rd_party_tech_contact = abap_true.
            r_ov_flags-edit_3rd_party_tech_contact = abap_false.

          ELSE. " the partner viewing the request is not the Requester himself, then Requester has already approved
            r_ov_flags-show_tech_contact = abap_true.

            r_ov_flags-show_3rd_party_tech_contact = abap_true.
            r_ov_flags-edit_3rd_party_tech_contact = abap_false.

          ENDIF.
      ENDCASE.


    ENDIF.
  ENDMETHOD.


  METHOD do_split.
    DATA: v_order_srvs_4_apprv     TYPE STANDARD TABLE OF zib_tur_odata_service_s,
          v_order_srvs_4_leg_aprvl TYPE STANDARD TABLE OF zib_tur_odata_service_s.

    SELECT service_id FROM zpt_serv_setup9 INTO TABLE @DATA(t_da_check_srvs).
    CHECK sy-subrc = 0.

    "Fetch all Items:
    SELECT * FROM zib_tur_order_services_cds INTO TABLE @DATA(v_items) WHERE request_guid = @i_original_request_guid.

    CHECK sy-subrc = 0.



    LOOP AT v_items ASSIGNING FIELD-SYMBOL(<items>).
**********************************************************************
*{ BEGIN INSERT EXTREEH; CCDV-892; Shared services should be handled like the relevant master Service
**********************************************************************
* ZPT_CONN_CHECK geht notfalls, falls das nicht über die Order zu ermitteln ist
**********************************************************************

      IF <items>-bandwidth = 'SHARED'.
*get master (better would be if we could get the master somehow via the request itself
        SELECT needed_service_1 FROM zpt_conn_check
          INTO TABLE @DATA(lt_master)
          WHERE checked_service = @<items>-service_id.

        SORT  lt_master.
        DELETE ADJACENT DUPLICATES FROM lt_master.

* add MICs
        LOOP AT lt_master INTO DATA(ls_master).

          SELECT prim_service FROM zib_mic_config
            APPENDING TABLE @DATA(lt_mics)
            WHERE second_service = @ls_master-needed_service_1.

        ENDLOOP.
        SORT lt_mics.
        DELETE ADJACENT DUPLICATES FROM lt_mics.
        APPEND LINES OF lt_mics TO lt_master.

*find master
        LOOP AT lt_master INTO ls_master.


          READ TABLE v_items INTO DATA(master_item) WITH KEY service_id = ls_master-needed_service_1.
          IF sy-subrc = 0.
            EXIT.
          ENDIF.
        ENDLOOP.

*treat the same way the master is treated.
        IF line_exists( t_da_check_srvs[ service_id = master_item-service_id ] ).
          APPEND CORRESPONDING #( <items> ) TO v_order_srvs_4_leg_aprvl.
        ELSE.
          APPEND CORRESPONDING #( <items> ) TO v_order_srvs_4_apprv.
        ENDIF.
        CLEAR: lt_master, ls_master, master_item.

*entitlements
      ELSEIF <items>-bandwidth = ''
          OR <items>-bandwidth = '0 Kbit/s'.

        SELECT primary_service FROM zib_entitlements
          INTO TABLE @DATA(lt_master_ent)
          WHERE non_primary_service = @<items>-service_id.

*find master
        LOOP AT lt_master_ent INTO DATA(ls_master_ent).


          READ TABLE v_items INTO master_item WITH KEY service_id = ls_master_ent-primary_service.
          IF sy-subrc = 0.
            EXIT.
          ENDIF.
        ENDLOOP.

*treat the same way the master is treated.
        IF line_exists( t_da_check_srvs[ service_id = master_item-service_id ] ).
          APPEND CORRESPONDING #( <items> ) TO v_order_srvs_4_leg_aprvl.
        ELSE.
          APPEND CORRESPONDING #( <items> ) TO v_order_srvs_4_apprv.
        ENDIF.
        CLEAR: lt_master_ent, ls_master_ent, master_item.


      ELSE.
*} END EXTREEH
**********************************************************************

        IF line_exists( t_da_check_srvs[ service_id = <items>-service_id ] ).
          APPEND CORRESPONDING #( <items> ) TO v_order_srvs_4_leg_aprvl.
        ELSE.
          APPEND CORRESPONDING #( <items> ) TO v_order_srvs_4_apprv.
        ENDIF.


      ENDIF.
    ENDLOOP.

*    v_order_srvs_4_leg_aprvl = VALUE #( FOR l IN v_items WHERE (  ) ( CORRESPONDING #( l ) ) ).
*
*    v_order_srvs_4_apprv = VALUE #( FOR l IN v_items WHERE ( service_id <> 'S1350' AND service_id <> 'S1352' AND service_id <> 'S1353' ) ( CORRESPONDING #( l ) ) ).
    IF v_order_srvs_4_leg_aprvl IS NOT INITIAL.
      DATA(v_request_4_wait_legal) = create_split_request( i_origin_guid = i_original_request_guid
                                                           i_services    = v_order_srvs_4_leg_aprvl ).

      v_request_4_wait_legal-status = zif_ib_tur_const=>co_tur_order_status-wait_legal_appr.
      APPEND v_request_4_wait_legal TO r_splits.
    ENDIF.
    CLEAR v_request_4_wait_legal.

    DATA(v_request_4_apprv) = create_split_request( i_origin_guid = i_original_request_guid
                                                   i_services    = v_order_srvs_4_apprv ).


    v_request_4_apprv-status = zif_ib_tur_const=>co_tur_order_status-approved.

    APPEND v_request_4_apprv TO r_splits.

    DATA(v_origin_request) = CAST zcl_ib_tur_order( me->get_request( i_key =  i_original_request_guid ) ).

    DATA(v_partners) = v_origin_request->get_partners( ).
    DATA(v_locations_origin) = v_origin_request->zif_ib_tur_order~location_details( ).
    DATA(v_notes_origin) = v_origin_request->zif_ib_tur_order~notes( ).

    DATA(v_attachments_origin) = zcl_ibase_tur_api=>get_instance( i_access_method = '2' )->get_attachm_as_media(  i_guid    = i_original_request_guid   ).


    LOOP AT r_splits ASSIGNING FIELD-SYMBOL(<request>).
      LOOP AT v_partners ASSIGNING FIELD-SYMBOL(<partner>) WHERE function <> zif_ib_tur_const=>c_partner_functions-company_partner
                                                             AND function <> zif_ib_tur_const=>c_partner_functions-partner_exch.
        " Ideally this should only add Tech Contact, Line Responsible1 and Line Responsible 2( if any)
        <request>-order_api->add_partner(
         EXPORTING
           i_partner        = <partner>-partner
           i_partner_fct    = <partner>-function
*        i_with_appr_step = abap_true
*        i_service_id     =
       ).
*
      ENDLOOP.
*      <request>-order_api->zif_ib_tur_order~company_partner( i_partner = v_partners[ function = zif_ib_tur_const=>c_partner_functions-company_partner ]-partner ).
*
*      <request>-order_api->zif_ib_tur_order~author( i_partner = v_partners[ function = zif_ib_tur_const=>c_partner_functions-author ]-partner ).

      LOOP AT v_notes_origin ASSIGNING FIELD-SYMBOL(<notes>).
        CLEAR <notes>-text_id.
        clean_up_text( CHANGING c_notes = <notes> ).
        <request>-order_api->zif_ib_tur_order~notes(   i_text   = CORRESPONDING #( <notes>  )
            i_author = v_items[ 1 ]-company_partner
*        RECEIVING
*          r_notes  =
        ).
      ENDLOOP.
      LOOP AT v_attachments_origin ASSIGNING FIELD-SYMBOL(<attachm>).
        me->add_attachment(
          EXPORTING
            i_request_guid = <request>-order_api->get_request_guid( )
            i_attachment   = CORRESPONDING #( <attachm> )
*       RECEIVING
*         r_file_info    =                  " Tickets and Requests OData - Order Attachments
        ).
      ENDLOOP.

      LOOP AT v_locations_origin ASSIGNING FIELD-SYMBOL(<loc>).
        <request>-order_api->zif_ib_tur_order~location_details(   i_loc_details = CORRESPONDING #( <loc> )
                                                                  i_option      = zif_ib_tur_const=>c_odata_cud_operations-create
*        RECEIVING
*          r_locations   =
        ).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD format_session_installation.

    TYPES: BEGIN OF lcl_ty_ibase_id,
             ibase_id TYPE char18,
           END OF lcl_ty_ibase_id,
           lcl_ty_ibase_id_tt TYPE STANDARD TABLE OF lcl_ty_ibase_id WITH DEFAULT KEY. ",

    DATA: v_installation_desc TYPE zrac_ibase_descr_long,
          v_prov_bus_id       TYPE zcl_service_toolbox=>ty_provider_id-business_id,
          v_inst_owner        TYPE bu_partner.

    CHECK c_order_installation IS NOT INITIAL.
    DATA(v_order_insts) = VALUE lcl_ty_ibase_id_tt( FOR l IN c_order_installation ( ibase_id = CONV #( |{ l-installation_sub ALPHA = OUT }| ) ) ).


*    SELECT *
*      FROM zib_fast_select5
*      INTO TABLE @DATA(v_installations)
*      FOR ALL ENTRIES IN @i_installations
*        WHERE ibase = @i_installations-table_line.
*
*    SORT v_installations BY ibase.
*    DELETE ADJACENT DUPLICATES FROM v_installations.

*    SELECT SINGLE * FROM zib_map_lsm_sess INTO @DATA(v_sess_map) WHERE session_service_id = @i_service_id.
    SELECT * FROM zib_map_lsm_sess INTO TABLE @DATA(v_sess_map_t) WHERE session_service_id = @i_service_id. "SPCRC-6878 - Session ID can have multiple valid Connection Services

    CHECK sy-subrc = 0 AND v_sess_map_t IS NOT INITIAL."v_sess_map-line_service_id IS NOT INITIAL.
    DATA(v_service_id_filter) = VALUE /iwbep/t_cod_select_options( FOR line IN v_sess_map_t ( sign = 'I' option = 'EQ' low = line-line_service_id  ) ) .

    SELECT DISTINCT conns~product_id,
                    service_id,
                    bandwidth,
                    location2_desc,
                    inst~installation_id,
                    inst~installation AS installation_name,
                    inst~company_id AS installation_owner
      FROM zib_connections_ov_all_cds AS conns
      JOIN zib_installations_cds AS inst ON conns~product_guid = inst~product_guid
      FOR ALL ENTRIES IN @v_order_insts
      WHERE installation_id = @v_order_insts-ibase_id
        AND service_id IN @v_service_id_filter "= @v_sess_map-line_service_id "SPCRC-6878 - Session ID can have multiple valid Connection Services
      INTO TABLE @DATA(v_conns).

    CHECK sy-subrc = 0.


*--- Get all Providers:
    DATA(v_provider_id_tab) = VALUE crmt_bu_partner_t( FOR l1 IN v_conns WHERE ( installation_owner NE i_company_partner ) ( l1-installation_owner ) ).
    DATA(v_partner_fct) = get_partner_fct(
                            i_request_guid = i_request_guid
                            i_partner      = i_company_partner
                          ).

    SELECT SINGLE company_partner FROM zbtx_tur_order_b INTO @DATA(v_request_customer)
    WHERE order_guid = @i_request_guid.
    CHECK sy-subrc = 0.

    IF v_provider_id_tab IS NOT INITIAL.

      zcl_service_toolbox=>get_provider_id( EXPORTING it_partner      = v_provider_id_tab
                                                      iv_service_id   = i_service_id
                                            IMPORTING et_provider_ids = DATA(v_provider_bus_ids) ).

    ENDIF.

    LOOP AT c_order_installation ASSIGNING FIELD-SYMBOL(<inst>).

      DATA(inst_id) = CONV char18( |{ <inst>-installation_sub ALPHA = OUT }| ).

      IF line_exists( v_conns[ installation_id = inst_id ] ).
        DATA(v_location_name) = VALUE #( v_conns[ installation_id = inst_id ]-location2_desc ).
        DATA(v_bandwidth) = VALUE #( v_conns[ installation_id = inst_id ]-bandwidth ).

        v_inst_owner = v_conns[ installation_id = inst_id ]-installation_owner.


        v_prov_bus_id = VALUE #( v_provider_bus_ids[ account = v_inst_owner ]-business_id OPTIONAL ).
        IF v_prov_bus_id IS NOT INITIAL.
*            v_sess_installations-provider = v_prov_bus_id.
          CONCATENATE v_prov_bus_id '-' INTO v_installation_desc SEPARATED BY space.
        ENDIF.

        CONCATENATE v_installation_desc <inst>-install_sub_desc
             INTO v_installation_desc SEPARATED BY space.
        IF v_location_name IS NOT INITIAL.
          CONCATENATE v_installation_desc  '|' v_location_name
          INTO v_installation_desc  SEPARATED BY space.
        ENDIF.

        IF v_bandwidth IS NOT INITIAL.
          CONCATENATE v_installation_desc  '|' v_bandwidth
          INTO v_installation_desc  SEPARATED BY space.
        ENDIF.

        IF v_partner_fct = zif_ib_tur_const=>c_partner_functions-provider.
*{ BEGIN CHANGE EXTREEH; 19.11.21; CCDV-1283; All coops should see all installations, so I take this change back.
*          OR v_partner_fct = zif_ib_tur_const=>c_partner_functions-partner_exch. "EXTLUEDEMANN: 02.07.2021 - SPCRC-6936 Coop Exchange Partner should not see other Installations

          SELECT SINGLE mandt FROM zpt_pex_c
            INTO @DATA(ls_pex)
            WHERE partner_exchange = @i_company_partner
              AND portal_full_visibility = 'X'.

          IF sy-subrc <> 0.

            IF  v_inst_owner <> i_company_partner." AND v_inst_owner <> v_request_customer. " I'm Provider, Installation is not mine and not that of request customer
              v_installation_desc = 'Assigned'.

            ENDIF.
          ENDIF.
*} END EXTREEH
        ENDIF.
        <inst>-install_sub_desc = v_installation_desc.

        CLEAR v_installation_desc.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD generate_print_for_splits.
    DATA(v_existing_prints) = get_order_print( i_order_guid = i_guid ) .

    CHECK v_existing_prints IS INITIAL.

    zcl_pt_rac_order_prints=>create_pdf(
      EXPORTING
        i_order_guid      = i_guid
        i_append_2_order  = abap_true
        i_request_type    = i_req_type
    ).
  ENDMETHOD.


  METHOD get_attachments.
    DATA(v_docs) = me->crm_entity->document->get_document( i_key = i_guid )->get_file_list( ).
    IF i_order_print = abap_true.
      DELETE v_docs WHERE is_order_print <> abap_true.
    ELSE.
      DELETE v_docs WHERE is_order_print = abap_true.
    ENDIF.
    r_files = CORRESPONDING #( v_docs ).
  ENDMETHOD.


  METHOD get_attachm_as_media.
*    DATA(v_request) = CAST zcl_ib_tur_order( me->get_request( i_key = i_request_guid ) ).
*    DATA(v_) = me->document_api->download_attachment( ).
*    me->document_api = me->crm_entity->document->get_document( i_key = i_request_guid ).
*    DATA(v_1) = me->document_api->download_attachment( ).
    IF i_file_id IS INITIAL.
      DATA(v_docs) = me->crm_entity->document->get_document( i_key = i_guid )->download_attachments( ).
      r_file = CORRESPONDING #( v_docs ).
    ELSE.
      DATA(v_doc) = me->crm_entity->document->get_document( i_key = i_guid )->download_attachment( i_phio_id  = i_file_id ).
      IF v_doc-file_id IS NOT INITIAL.
        r_file = VALUE #( ( CORRESPONDING #( v_doc ) ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_connection_details.

    DATA(v_tur_connections) = NEW zcl_ibase_connections( i_product_guid = i_product_guid ).
*    IF sy-uname = 'EXTNZONJIESO'.
*    ELSE.

    SELECT SINGLE * FROM zib_connections_ov_all_cds
    INTO @DATA(line_details)
    WHERE product_guid = @i_product_guid
    AND service_id = @i_service_id
    AND ( valid_to >= @sy-datum OR valid_to = '00000000' )
    .

    CHECK sy-subrc = 0.



*--- Get LSM data:
*    DATA(v_lsm_data) = v_tur_connections->get_lsm_data( i_product_guid = i_product_guid  "<- not needed
*                                                        i_service_id   = i_service_id ).
*    TRY.
*        DATA(v_line_data) = v_lsm_data[ product_guid = i_product_guid service_id = i_service_id ].
*      CATCH cx_sy_itab_line_not_found.
*    ENDTRY."<- not needed

*--- Get Location data:
    DATA(v_location_data) = v_tur_connections->get_location_data( ). "<- stays as is

*--- Get Installation data:
    DATA(v_installation_data) = v_tur_connections->get_installations( ). " <- may stays as is

*--- Get additional properties:
    DATA(v_properties) = v_tur_connections->get_other_properties( i_ibase = v_installation_data-installation1 i_is_mic = line_details-is_mic ).


*-- Fix for "is_VPN Flag for MICs / normal Lines:
    IF line_details-is_mic EQ 'X'. "MIC CASE
      DATA(is_vpn_flag) = v_properties-is_vpn. "We need to read flag from MIC Children in other properties method

      "Adjustment for Cancel Installation -> Installation of MIC should always be the Base
      SELECT SINGLE installation_id AS installation1,
                    installation AS installation1_desc
        FROM zib_installations_cds
        INTO @DATA(v_installations)
        WHERE product_guid EQ @i_product_guid.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      v_installation_data = VALUE #( installation1      = |{ v_installations-installation1 ALPHA = OUT }|
                                     installation1_desc = v_installations-installation1_desc ).
    ELSE.
      is_vpn_flag = line_details-is_vpn. "We can take the flag value from overview call.
    ENDIF.

*--- Fill return structure:
    r_conn_details = VALUE #( product_guid        = line_details-product_guid  "<--
                              product_id          = line_details-product_id
                              business_id         = line_details-business_id   "<--
                              service_id          = line_details-service_id    "<--
                              service_desc        = line_details-service_desc "<--
                              bandwidth           = line_details-bandwidth     "<--
                              mic_line_id         = line_details-mic_line_id   "<--
                              description         = line_details-description  "<--


                              location2           = v_location_data-location2 "<--
                              location2_desc      = v_location_data-location2_desc    "<--


                              room_id_2           = v_location_data-room2
                              room_2_desc         = v_location_data-room2_desc


                              floor_id_2          = v_location_data-floor2
                              floor_desc_2        = v_location_data-floor2_desc

                              rack_b              = v_location_data-rack_b

                              position_b          = v_location_data-position_b

                              shelf_b             = v_location_data-shelf_b
                              circuit             = v_location_data-circuit_id_desc
                              interface           = v_location_data-interface_b_desc
                              installation        = v_installation_data-installation1 "<--
                              installation_desc   = v_installation_data-installation1_desc "<--
                              access_point_desc   = line_details-access_point_des
                              access_point_id     = line_details-access_point_id
                              access_point_interface = v_properties-access_point_interface
                              ap_ip_address       = v_properties-access_point_ip
                              ip_address          = v_properties-ip_address
                              router_id           = v_properties-router_id
                              router_desc         = v_properties-router_desc
                              billing_id          = line_details-billing_id "<--
                              provider_id         = v_properties-provider_id
                              provider_desc       = v_properties-provider_desc
                              is_mic              = line_details-is_mic "<--
                              is_vpn              = is_vpn_flag
                              pr_est_del          = v_properties-pr_est_del
                              pr_inst_due_dte     = v_properties-pr_inst_due_dte
                              pr_install          = v_properties-pr_install
                              dbs_act_date        = v_properties-dbs_act_date
                              media               = v_properties-media


                              building_b          = v_location_data-building_b
                              ).


  ENDMETHOD.


  METHOD get_dynamic_print.
    DATA: v_request_api TYPE REF TO zcl_ib_tur_order,
          v_file        TYPE ztur_order_attachment,
          v_pdf         TYPE zib_media_resource_s.

    SELECT SINGLE company_partner, status, request_type
      FROM zbtx_tur_order_b
      INTO @DATA(v_order)
      WHERE order_guid EQ @i_request_guid.
    CHECK sy-subrc = 0.
    IF v_order-company_partner NE i_company_partner.
      DATA(v_company_id) = i_company_partner.
    ELSE.
      v_company_id = v_order-company_partner.
    ENDIF.
    "get existing print=>generated when the request was approved
    v_file = me->get_order_print( i_request_guid ).

    IF v_order-status = zif_ib_tur_const=>co_tur_order_status-approved
      OR v_order-status = zif_ib_tur_const=>co_tur_order_status-wait_legal_appr
    OR v_order-status = zif_ib_tur_const=>co_tur_order_status-in_progress
    OR v_order-status = zif_ib_tur_const=>co_tur_order_status-rejected
      OR v_order-status = zif_ib_tur_const=>co_tur_order_status-replaced
      OR v_order-status = zif_ib_tur_const=>co_tur_order_status-completed.

      IF v_file IS INITIAL.
        zcl_pt_rac_order_prints=>create_pdf( EXPORTING i_order_guid      = i_request_guid
                                                       i_request_type    = v_order-request_type
                                                       i_append_2_order  = abap_true
                                                       i_company_partner = v_company_id
                                             RECEIVING r_order_pdf       = v_pdf ).
        v_file = CORRESPONDING #( v_pdf ).
      ENDIF.
    ELSE.
      IF v_file-file_id IS NOT INITIAL.
        delete_attachment( EXPORTING i_request_guid = i_request_guid
                                     i_file_id      = v_file-file_id ).
      ENDIF.

      zcl_pt_rac_order_prints=>create_pdf( EXPORTING i_order_guid      = i_request_guid
                                                     i_request_type    = v_order-request_type
                                                     i_company_partner = v_company_id
                                           RECEIVING r_order_pdf       = v_pdf ).

      CHECK v_pdf IS NOT INITIAL.

      v_file = CORRESPONDING #( v_pdf ).
    ENDIF.

    r_pdf = CORRESPONDING #( v_file ).
  ENDMETHOD.


  METHOD get_instance.

*    r_instance = NEW zcl_ibase_tur_api( i_access_method = CONV #( i_access_method ) ).
    IF instance IS NOT BOUND OR access_method <> i_access_method.
      instance = NEW zcl_ibase_tur_api( i_access_method ).
    ENDIF.
    r_instance = instance.
  ENDMETHOD.


  METHOD get_mic_connection_details.

    DATA v_mic_details TYPE STANDARD TABLE OF zib_mic_detail_v. "zib_mic_lines_v.

    SELECT * FROM zib_conn_mic_lines_cds INTO CORRESPONDING FIELDS OF TABLE @v_mic_details WHERE mic_line_id = @i_mic_id.

    CHECK sy-subrc = 0.
    e_mic_conn_details = v_mic_details.

  ENDMETHOD.


  METHOD get_open_approvals.
    SELECT * INTO TABLE @DATA(v_open_approvals) FROM zib_tur_order_appr_steps
    WHERE request_guid = @i_object_guid AND step_status = ''.
    CHECK sy-subrc = 0.
    r_missing_approvals_t = CORRESPONDING #( v_open_approvals ).
  ENDMETHOD.


  METHOD get_order_print.
    DATA(v_attachments) =  zcl_ibase_tur_api=>get_instance(
                               i_access_method = '2'
                           )->get_attachments( i_guid = i_order_guid i_order_print = abap_true ).
    CHECK v_attachments IS NOT INITIAL.
    SORT v_attachments BY file_date DESCENDING.
    TRY.
        DATA(v_order_print) = CORRESPONDING ztur_order_attachment( v_attachments[ 1 ] ).
        DATA(v_media_files) = me->get_attachm_as_media(
                          i_guid    = v_order_print-order_guid
                          i_file_id = v_order_print-file_id
                        ).
        CHECK v_media_files IS NOT INITIAL.
        r_order_print = CORRESPONDING #( v_media_files[ 1 ] ).
      CATCH cx_sy_itab_line_not_found INTO DATA(v_line_nf_exc).
        DATA(v_exc_text) = v_line_nf_exc->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_partner_fct.
    DATA(v_request) = CAST zcl_ib_tur_order( me->get_request( i_key =  i_request_guid ) ).
    DATA(v_partners) = v_request->get_partners(
*                         i_company_account = i_partner
                       ).
    CHECK v_partners IS NOT INITIAL.
    IF line_exists( v_partners[ partner = i_partner ] ).
      r_partner_fct = v_partners[ partner = i_partner ]-function.
    ENDIF.
  ENDMETHOD.


  METHOD get_related_request.

    SELECT others~request_guid
      FROM zib_tur_order_services_cds AS main
      INNER JOIN zib_tur_order_services_cds AS others
      ON main~related_product = others~related_product
      INTO TABLE @r_related_request_guids
      WHERE main~request_guid = @i_request_guid
      AND main~related_product NE ''
      AND others~request_guid NE @i_request_guid.

  ENDMETHOD.


  METHOD get_request.
    IF i_key IS NOT INITIAL.
      me->order_api ?=  me->crm_entity->order->get_order( i_key = i_key ).
      r_request = NEW zcl_ib_tur_order( i_order_api = me->order_api ).
    ENDIF.

  ENDMETHOD.


  METHOD get_requests_splits.
    DATA(v_request) = CAST zcl_ib_tur_order( me->get_request( i_key =  i_object_guid ) ).
    DATA(v_related_requests) = v_request->get_related_requests( ).

    LOOP AT v_related_requests ASSIGNING FIELD-SYMBOL(<rel>).
      IF <rel>-objkey_a = i_object_guid.
        APPEND <rel>-objkey_b TO r_related_requests.
      ELSEIF <rel>-objkey_b = i_object_guid.
        APPEND <rel>-objkey_a TO r_related_requests.
      ENDIF.
    ENDLOOP.

*    r_related_requests = VALUE #( FOR l IN v_request->get_related_requests( ) ( CONV #( l-objkey_b ) ) ).

  ENDMETHOD.


  METHOD get_request_status.
    SELECT SINGLE status FROM zib_tur_orders_cds INTO @DATA(v_status) WHERE  request_guid = @irequest_guid.
    CHECK sy-subrc  = 0.
    r_status = v_status.
  ENDMETHOD.


  METHOD ordered_services.
    DATA _test TYPE ty_order_serv.

    IF  i_order_api IS INITIAL AND i_request_guid IS INITIAL.

      zcx_crm_app=>raise_missing_input(
        EXPORTING
          i_input_name = 'Request GUID'

      ).
    ENDIF.

    DATA(v_service) = CORRESPONDING zbtx_tur_order_serv_s( i_services ).

    IF i_order_api IS INITIAL.
      DATA(v_order) = me->get_request( i_key =  i_request_guid )..
    ELSE.
      v_order = i_order_api.
    ENDIF.
*    r_order_service = v_order->services( i_service = v_service i_product = v_service-item ).

  ENDMETHOD.


  METHOD process_approval_step.
    DATA v_splitted_requests TYPE ty_splitted_requests_tt.
    DATA  v_appr_status  TYPE zif_ib_tur_order=>ty_approval_status  .
    DATA(v_approval_step) = c_approval_step.


    DATA(v_request_guid) = c_approval_step-request_guid.
    DATA(v_request) = CAST zcl_ib_tur_order( me->get_request( i_key = v_request_guid  ) ).
    DATA(v_request_type) = v_request->get_request_type( ).

    SELECT SINGLE function FROM zib_tur_order_appr_steps INTO @DATA(v_function) WHERE request_guid = @c_approval_step-request_guid AND company_partner = @c_approval_step-partner
    AND step_status = ''.
    CHECK sy-subrc = 0.

    TRY.
        IF v_function = zif_ib_tur_const=>c_partner_functions-company_partner AND
         check_requires_legal_apprvl( i_object_guid = v_request_guid i_partner = c_approval_step-partner ) = abap_true.
          IF check_split_needed( v_request_guid ) = abap_true.

            v_splitted_requests = do_split( v_request_guid ).
            IF v_splitted_requests IS NOT INITIAL.
              LOOP AT v_splitted_requests ASSIGNING FIELD-SYMBOL(<splits>).
                c_approval_step-request_guid = <splits>-guid.
                c_approval_step-request_id = <splits>-id.

                CASE <splits>-status.
                  WHEN zif_ib_tur_const=>co_tur_order_status-approved.

                    c_approval_step-status = zif_ib_tur_const=>c_approval_step_status-approved.
                  WHEN zif_ib_tur_const=>co_tur_order_status-wait_legal_appr.
                    c_approval_step-status = zif_ib_tur_const=>c_approval_step_status-waitlegal.
                ENDCASE.

                v_appr_status = <splits>-order_api->zif_ib_tur_order~approve_order(
                  CHANGING
                    c_approval_step = c_approval_step
                ).
                IF v_appr_status-print_generated = abap_false.
                  zcl_pt_rac_order_prints=>create_pdf(
                   EXPORTING
                     i_order_guid      = <splits>-guid
                     i_append_2_order  = abap_true
                     i_request_type    = v_request_type ).
                ENDIF.
                CLEAR v_appr_status.
              ENDLOOP.
              c_approval_step-status = zif_ib_tur_const=>c_approval_step_status-replace.

              v_appr_status = v_request->zif_ib_tur_order~approve_order(
                 CHANGING
                   c_approval_step = c_approval_step
               ).
              IF v_appr_status-print_generated = abap_false.
                zcl_pt_rac_order_prints=>create_pdf(
                 EXPORTING
                   i_order_guid      = v_request_guid
                   i_append_2_order  = abap_true
                   i_request_type    = v_request_type ).
              ENDIF.
              result = abap_true.
            ENDIF.
          ELSE.
            c_approval_step-status = zif_ib_tur_const=>c_approval_step_status-waitlegal.
            v_request->zif_ib_tur_order~approve_order(
               CHANGING
                 c_approval_step = c_approval_step
             ).
            IF v_appr_status-print_generated = abap_false.
              zcl_pt_rac_order_prints=>create_pdf(
               EXPORTING
                 i_order_guid      = v_request_guid
                 i_append_2_order  = abap_true
                 i_request_type    = v_request_type ).
            ENDIF.
          ENDIF.
          "Set

        ELSE.


          v_appr_status = v_request->zif_ib_tur_order~approve_order( CHANGING c_approval_step =  c_approval_step    ).
          IF v_appr_status-print_generated = abap_false.
            zcl_pt_rac_order_prints=>create_pdf(
             EXPORTING
               i_order_guid      = v_request_guid
               i_append_2_order  = abap_true
               i_request_type    = v_request_type ).
          ENDIF.
        ENDIF.
      CATCH zcx_crm_app INTO DATA(v_exec_exc).
        zcx_crm_app=>raise_exec_failed( i_text = v_exec_exc->get_longtext( ) ).
    ENDTRY.
*    CHECK zcl_pt_rac_order_prints=>pdf_content IS NOT INITIAL.
*    me->add_attachment(
*      EXPORTING
*        i_request_guid = c_approval_step-request_guid
*     i_attachment   = VALUE #( file_content = zcl_pt_rac_order_prints=>pdf_content mime_type = zcl_pt_rac_order_prints=>c_mime_type file_name = |Request { v_request->get_request_id( ) }| is_order_print = abap_true )
**     *    i_account      =
**  RECEIVING
**    r_file_info    =                  " Tickets and Requests OData - Order Attachments
*    ).

*    IF v_appr_status-print_generated = abap_false.
*      zcl_pt_rac_order_prints=>create_pdf(
*       EXPORTING
*         i_order_guid      = v_request_guid
*         i_append_2_order  = abap_true
*         i_request_type    = v_request->get_request_type( )
**    i_company_partner =
**  RECEIVING
**    r_order_pdf       =
*     ).
*    ENDIF.
  ENDMETHOD.


  METHOD save.

    "Consider to implemet Checks
    me->crm_entity->save(  ).

    IF i_commit = abap_true.
      me->crm_entity->commit(  ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
