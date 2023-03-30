
CLASS zcl_ib_tur_order DEFINITION
  PUBLIC
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_ibase_tur_api .

  PUBLIC SECTION.

    INTERFACES zif_ib_tur_order .

    TYPES:
      ty_tur_order_appr_step    TYPE zib_tur_order_appr_steps,
      ty_order_partners_tt      TYPE STANDARD TABLE OF zbtx_tur_partner_s WITH DEFAULT KEY,
      ty_axt_recoord_id_tt      TYPE STANDARD TABLE OF axt_record_id,
      ty_tur_order_appr_step_tt TYPE STANDARD TABLE OF ty_tur_order_appr_step WITH DEFAULT KEY.

    METHODS: constructor
      IMPORTING
        i_order_api TYPE REF TO zif_crm_order,
      get_status
        RETURNING
          VALUE(r_status) TYPE zcrm_order_status,
      get_service_items
        RETURNING VALUE(r_service_items_tab) TYPE zbtx_tur_order_serv_tt,
      description
        IMPORTING
          i_req_type           TYPE zpt_order_req_type OPTIONAL
        RETURNING
          VALUE(r_description) TYPE crmt_process_description,

      get_request_guid
        RETURNING
          VALUE(r_request_guid) TYPE crmt_object_guid,
      get_request_type
        RETURNING
          VALUE(r_request_type) TYPE zpt_order_req_type,
      get_request_id
        RETURNING
          VALUE(r_request_id) TYPE crmt_object_id,
      bind_related_request IMPORTING i_parent_guid TYPE crmt_object_guid,
      get_related_requests RETURNING VALUE(r_related_requests) TYPE zbtx_docflow_btil_tt ,
      add_partner
        IMPORTING
          i_partner        TYPE bu_partner
          i_partner_fct    TYPE crmt_partner_fct
          i_with_appr_step TYPE abap_bool DEFAULT abap_true
          i_service_id     TYPE z_service_id_new OPTIONAL,

      delete_note
        IMPORTING i_text_id TYPE tdid
                  i_langu   TYPE spras,

      update_installation
        IMPORTING i_new_inst TYPE zbtx_tur_order_inst_s,

      delete_installation
        IMPORTING i_record_guid  TYPE guid
                  i_line_item_no TYPE crmt_item_no
                  i_is_real_ib   TYPE abap_bool,
      remove_partner
        IMPORTING
          i_partner     TYPE bu_partner
          i_partner_fct TYPE crmt_partner_fct,
      get_partners
        IMPORTING i_partner_fct           TYPE  crmt_partner_fct OPTIONAL
        RETURNING
                  VALUE(r_order_partners) TYPE ty_order_partners_tt,
      delete_service_from_request
        IMPORTING
          i_item_number TYPE crmt_item_no_ext,
*      !I_request_guid
      remove_approval_step IMPORTING i_item_number TYPE  crmt_item_no_ext OPTIONAL
                                     i_record_ids  TYPE ty_axt_recoord_id_tt OPTIONAL,
      get_approval_steps
        IMPORTING
          i_status        TYPE char2
        RETURNING
          VALUE(r_result) TYPE ty_tur_order_appr_step_tt.


  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: c_field_name_desc            TYPE  name_komp VALUE 'DESCRIPTION',
               c_cust_h_field_name_req_type TYPE name_komp VALUE 'ZZ_PT_REQ_TYPE'.
    DATA: order_api       TYPE REF TO zif_crm_order,
          order_header    TYPE crmst_adminh_btil,
          order_item_api  TYPE REF TO zif_crm_order_item,
          company_partner TYPE bu_partner.

    METHODS: create_request
      IMPORTING
        i_request_type TYPE string ,

      map_service_fields
        IMPORTING
          i_input         TYPE any
        CHANGING
          product_i_props TYPE crmst_producti_btil OPTIONAL

          extension1      TYPE zastruc0002wc OPTIONAL
          extension2      TYPE zbtx_aet_tur_ldt_bol_attr OPTIONAL,
      installations
        IMPORTING
          i_service TYPE zbtx_tur_order_inst_s,
      add_required_approval
        IMPORTING i_approval_step TYPE zbtx_tur_order_approval_steps,

      check_coop_exch_x_provider
        IMPORTING
                  i_service_id         TYPE z_service_id_new
                  i_provider_bus_id    TYPE zib_business_id
                  i_req_customer       TYPE bu_partner
                  i_portal_product     TYPE comt_product_id
        RETURNING VALUE(r_patner_exch) TYPE bu_partner,
      remove_all_approval_steps,
      convert_2_tstmp
        IMPORTING
          i_date          TYPE syst_datum
          i_time          TYPE syst_uzeit
        RETURNING
          VALUE(r_result) TYPE timestamp,
      detemine_text_id
        IMPORTING
                  i_author      TYPE bu_partner
        RETURNING VALUE(r_tdid) TYPE tdid,
      exec_on_approval
        IMPORTING
          i_request_guid  TYPE crmt_object_guid
        RETURNING
          VALUE(r_result) TYPE abap_bool,
      process_approval
        IMPORTING
                  i_approver              TYPE bu_partner
                  i_3rd_party_appr_needed TYPE abap_bool
                  i_cust_pft              TYPE crmt_partner_fct
                  i_request_guid          TYPE crmt_object_guid
                  i_request_type          TYPE zpt_order_req_type
                  i_status                TYPE crm_j_status OPTIONAL
        CHANGING  c_results               TYPE zif_ib_tur_order=>ty_approval_status
        RETURNING VALUE(r_sess_passwords) TYPE zpt_rac_odata_sess_pwrds_tt
        ,
      check_update_appr_step
        IMPORTING
          i_item_no    TYPE crmt_item_no
          i_provider   TYPE zib_business_id
          i_service_id TYPE z_service_id_new,
      check_b4_appr
        IMPORTING
          i_partner_fct      TYPE crmt_partner_fct
          i_company_partner  TYPE bu_partner
        RETURNING
          VALUE(return_code) TYPE char2,
      generate_print
        IMPORTING
          i_request_type TYPE zpt_order_req_type
          i_order_guid   TYPE crmt_object_guid,
      format_text
        IMPORTING
          i_text_object           TYPE tdobject
          i_tdid                  TYPE tdid
          i_text                  TYPE crmdt_conc_textlines
          i_langu                 TYPE langu
        RETURNING
          VALUE(r_formatted_text) TYPE string,
      check_cancel_sessions
        IMPORTING
          i_3rd_party_appr_needed TYPE abap_bool
          i_request_type          TYPE zpt_order_req_type
          i_approval_step         TYPE zbtx_tur_order_approval_steps
        CHANGING
          c_results               TYPE zif_ib_tur_order=>ty_approval_status.
    .


    .


ENDCLASS.



CLASS ZCL_IB_TUR_ORDER IMPLEMENTATION.


  METHOD add_partner.
    DATA v_add_appr_step TYPE abap_bool.


    me->order_api->modify_partner(
      EXPORTING
        i_partner_fct =  i_partner_fct
        i_partner     =    i_partner " use partner number, guid or object (zif_partner)
        i_main        = SWITCH #( i_partner_fct WHEN zif_ib_tur_const=>c_partner_functions-partner_exch THEN abap_false ELSE abap_true )   " identify main partner for given partner function
    ).
    v_add_appr_step = abap_false.
*    IF i_with_appr_step = abap_true.
    CASE i_partner_fct.
      WHEN zif_ib_tur_const=>c_partner_functions-partner_exch.
        IF i_with_appr_step = abap_true.
          v_add_appr_step = abap_true.
          DATA(v_appr_step) = zif_ib_tur_const=>c_order_approval_steps-coop_exch_appr.
        ENDIF.
      WHEN    zif_ib_tur_const=>c_partner_functions-provider.
        " OR zif_ib_tur_const=>c_partner_functions-participant. because apparently participant does not need to approve:SPCRC-2379

        v_add_appr_step = abap_true.
        v_appr_step = zif_ib_tur_const=>c_order_approval_steps-_3rd_party_appr.

      WHEN    zif_ib_tur_const=>c_partner_functions-company_partner.
        v_add_appr_step = abap_true.
        v_appr_step = zif_ib_tur_const=>c_order_approval_steps-customer_appr.
      WHEN    OTHERS.
    ENDCASE.

    IF v_add_appr_step = abap_true.
      me->add_required_approval( i_approval_step    = VALUE #(     service_id = i_service_id
                                                                   partner = i_partner
                                                                   approval_step = v_appr_step
                                                                   partner_as = i_partner_fct  )
                                ).

    ENDIF.
  ENDMETHOD.


  METHOD add_required_approval.
    DATA v_existing_approvals TYPE STANDARD TABLE OF zbtx_aet_tur_as_bol_attr.

    DATA(v_approval_step) = VALUE zbtx_aet_tur_as_bol_attr(  zzapproval_step = i_approval_step-approval_step
                                                    zzpartner       = i_approval_step-partner
                                                    zzpartner_as    = i_approval_step-partner_as
                                                    zzservice_id    = i_approval_step-service_id
                                                                                   ).

    me->order_api->get_extension_list(
      EXPORTING
        i_extension_name = zif_ib_tur_const=>c_aet_extensions-approval_steps
      IMPORTING
        e_extension_list = v_existing_approvals
    ).
    IF v_existing_approvals IS NOT INITIAL.
      IF line_exists( v_existing_approvals[ zzpartner = v_approval_step-zzpartner zzapproval_step = v_approval_step-zzapproval_step zzservice_id = v_approval_step-zzservice_id ] ).
        RETURN.
      ENDIF.

      " if provider Approval exist for partner who is also Coop Exchange, only provider approval required
      IF v_approval_step-zzpartner_as = zif_ib_tur_const=>c_partner_functions-partner_exch AND
       line_exists(  v_existing_approvals[ zzpartner = v_approval_step-zzpartner zzapproval_step = zif_ib_tur_const=>c_order_approval_steps-_3rd_party_appr ] ).
        RETURN.
      ENDIF.
    ENDIF.

    IF v_approval_step-zzpartner_as = zif_ib_tur_const=>c_partner_functions-provider
        AND  line_exists(  v_existing_approvals[ zzpartner = v_approval_step-zzpartner zzapproval_step = zif_ib_tur_const=>c_order_approval_steps-coop_exch_appr ] ).
    ENDIF.

    DATA(v_as_record_guid) =  me->order_api->add_extension_list_entry(
       EXPORTING
         i_extension_name = zif_ib_tur_const=>c_aet_extensions-approval_steps
         i_extension_value = v_approval_step
*
     ).
    IF v_as_record_guid IS INITIAL.
      zcx_crm_app=>raise_failed(
      i_text = ''
      ).
    ENDIF.
  ENDMETHOD.


  METHOD bind_related_request.
    me->order_api->add_predecessor(
      EXPORTING
        i_parent_guid        = i_parent_guid
        i_parent_object_type = 'BUS2000116'
        i_reltype            = 'SRCE'
    i_vona_kind              = ''
    ).
  ENDMETHOD.


  METHOD check_b4_appr.
    CHECK me->order_api IS BOUND.
*   1). Check that approving partner has provided technical Contact:
    "Technical Contact are only relevant for Provider(3rd Party and Customer who created the request)
    CASE i_partner_fct.
      WHEN zif_ib_tur_const=>c_partner_functions-provider.
        DATA(v_tech_cont_fct) = zif_ib_tur_const=>c_partner_functions-_3rdparty_tech_contact.
      WHEN zif_ib_tur_const=>c_partner_functions-company_partner.
        v_tech_cont_fct = zif_ib_tur_const=>c_partner_functions-cust_tech_contact.
    ENDCASE..
    IF v_tech_cont_fct IS NOT INITIAL.
      DATA(v_partner_list) = me->order_api->get_partner_list(
              i_partner_fct =  v_tech_cont_fct  ).

      IF v_partner_list IS INITIAL.
        return_code = 'E1'. " Technical Contact Missing
        RETURN.
      ENDIF.
    ENDIF.
*    2. Terms and Conditions have to be accepted
    " Relevant for Customer approval only

*    CHECK i_partner_fct = zif_ib_tur_const=>c_partner_functions-company_partner.
*    DATA v_tac_accepted TYPE abap_bool.
*    order_api->get_customer_h_extension_field(
*      EXPORTING
*        i_field_name = ''
*      IMPORTING
*        e_result     = v_tac_accepted
*    ).
*    IF v_tac_accepted = abap_false.
*      return_code = 'E2'. " Please accept Terms and conditions:
*      RETURN.
*    ENDIF.
  ENDMETHOD.


  METHOD check_cancel_sessions.

    IF i_request_type = zif_ib_tur_const=>c_pt_req_types-new_eti OR
                 i_request_type = zif_ib_tur_const=>c_pt_req_types-new_fix OR
                 i_request_type = zif_ib_tur_const=>c_pt_req_types-new_fix_lf OR
                 i_request_type = zif_ib_tur_const=>c_pt_req_types-new_rrh OR
                 i_request_type = zif_ib_tur_const=>c_pt_req_types-new_eti_on_behalf OR
                 i_request_type = zif_ib_tur_const=>c_pt_req_types-new_fix_on_behalf OR
                 i_request_type = zif_ib_tur_const=>c_pt_req_types-new_fix_lf_on_behalf
    .
      TRY.

          process_approval( EXPORTING i_request_guid           = i_approval_step-request_guid
                                                              i_request_type          = i_request_type
                                                              i_cust_pft              = i_approval_step-partner_as
                                                              i_3rd_party_appr_needed = i_3rd_party_appr_needed
                                                              i_approver              = i_approval_step-processed_by
                                                              i_status                = zif_ib_tur_const=>co_tur_order_status-rejected

                                                      CHANGING c_results               = c_results
                                               ).
        CATCH zcx_pt_btx_apprv_exec INTO DATA(v_appr_exception).
          zcx_crm_app=>raise_exec_failed(
            EXPORTING
              i_text       = v_appr_exception->get_longtext( )
          ).
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD check_coop_exch_x_provider.
    DATA: v_provider_added TYPE abap_bool.
    IF i_provider_bus_id IS NOT INITIAL.
      SELECT SINGLE partner FROM but0id INTO @DATA(v_provider) WHERE idnumber = @i_provider_bus_id.
      IF sy-subrc = 0 AND v_provider <> i_req_customer. " If A provider makes an on-Behalf request, Provider Pfct not needed
        me->add_partner(
          EXPORTING
            i_partner     = v_provider
            i_service_id  = i_service_id
            i_partner_fct = zif_ib_tur_const=>c_partner_functions-provider
            i_with_appr_step = COND #( WHEN r_patner_exch = v_provider THEN abap_false ELSE abap_true )
            ).
        v_provider_added = abap_true.
      ENDIF.
    ENDIF.

    IF
*    i_portal_product = 'PTO610'
*    OR i_portal_product = 'PTO620'
*    OR i_portal_product = 'PTO630'
*    OR i_portal_product = 'PTO640'
*    OR
    i_portal_product = 'PTO313'
    OR i_portal_product = 'PTO323'
    OR i_portal_product = 'PTO333'
    OR i_portal_product = 'PTO393'
    .
*        Coop Exchange will not be included for (--MM Relation) and Reset Session PW Request types
      RETURN.
    ENDIF.


    SELECT  partner_exchange FROM zpt_pex_serv_c
      INTO TABLE @DATA(t_partner_exch)
      WHERE service = @i_service_id
*--------------------------------------------------------------------*
*      28.05.2021 - EXTREEH - SPCRC-6850
*--------------------------------------------------------------------*
        AND no_appr_step NE 'X'.
*--------------------------------------------------------------------*

    IF sy-subrc = 0." AND v_partner_exch IS NOT INITIAL.
      LOOP AT t_partner_exch ASSIGNING FIELD-SYMBOL(<pex>).
        IF <pex>-partner_exchange = i_req_customer." Requester will not be included as Coop Exchange in Request
          CONTINUE.
        ENDIF.

        IF v_provider_added = abap_true AND v_provider = <pex>-partner_exchange. " Because we do not include approval Step for Coop Exchange, if partner is provider as well
          DATA(v_as_4_coop_need) = abap_false.
        ELSE.
          v_as_4_coop_need = abap_true.
        ENDIF.

        me->add_partner(
          EXPORTING
            i_partner     = <pex>-partner_exchange
            i_service_id  = i_service_id
            i_with_appr_step = v_as_4_coop_need
            i_partner_fct = zif_ib_tur_const=>c_partner_functions-partner_exch
        ).
      ENDLOOP.

      " add Partner Function
      " and add approval Step

**        Add approval Step -> will now be done in add Partner Method
*      me->add_required_approval( i_approval_step = VALUE #( service_id = i_service_id
*                                                            partner = v_partner_exch
*                                                            approval_step = zif_ib_tur_const=>c_order_approval_steps-coop_exch_appr
*                                                            partner_as = zif_ib_tur_const=>c_partner_functions-partner_exch  ) ).

*      r_patner_exch = v_partner_exch.

    ENDIF.

  ENDMETHOD.


  METHOD check_update_appr_step.
    DATA t_axt_record_id TYPE ty_axt_recoord_id_tt.
    DATA: v_3rd_party_appr_exists TYPE abap_bool.
    " check if request has a 3rd Party Approval already:

    SELECT  * INTO TABLE @DATA(v_apprv_step_exists)
     FROM zib_tur_order_appr_steps
      WHERE request_guid = @me->order_header-guid
       AND service_id = @i_service_id.
*      AND approval_step = @zif_ib_tur_const=>c_order_approval_steps-_3rd_party_appr.
*        DATA(t_3rd_appr_steps) = v_apprv_step_exists[ approval_step = zif_ib_tur_const=>c_order_approval_steps-_3rd_party_appr ].

    CHECK sy-subrc = 0.
    IF line_exists( v_apprv_step_exists[ approval_step = zif_ib_tur_const=>c_order_approval_steps-_3rd_party_appr ] ).
      v_3rd_party_appr_exists = abap_true.
    ENDIF.
    IF i_provider IS NOT INITIAL.
      SELECT SINGLE partner INTO @DATA(bp) FROM but0id WHERE idnumber = @i_provider.
      IF sy-subrc = 0 AND bp IS NOT INITIAL.
        DATA(v_partner) = bp.
*        if line_exists( v_apprv_step_exists[ company_partner = v_partner ] ).
*            return.
*        ENDIF.
      ENDIF.
    ENDIF.

    IF v_3rd_party_appr_exists = abap_true. " An approval already exists:
      IF i_provider IS INITIAL. " In Request Edit, Provider was removed=> Approval Step and eventually Partner function will has to be removed:
        me->remove_approval_step( i_record_ids  = VALUE #( FOR line IN v_apprv_step_exists
                                                          WHERE ( approval_step = zif_ib_tur_const=>c_order_approval_steps-_3rd_party_appr )
                                                        ( line-record_id ) )  ).

      ELSE. " Provider might have been changed/ or is the same
        LOOP AT v_apprv_step_exists ASSIGNING FIELD-SYMBOL(<appr_st>)
        WHERE ( approval_step = zif_ib_tur_const=>c_order_approval_steps-_3rd_party_appr ) .
          CHECK v_partner <> <appr_st>-company_partner.  " Provider Changed
          APPEND <appr_st>-record_id TO t_axt_record_id.
          DELETE v_apprv_step_exists.
        ENDLOOP.
        IF t_axt_record_id IS NOT INITIAL.
          me->remove_approval_step(
*                      EXPORTING
*                        i_item_number =
              i_record_ids  = t_axt_record_id
          ).
          DATA(v_upd_appr_step) = abap_true.
        ENDIF.
      ENDIF.
    ELSE. " Provider Approval does not exist yet:
      IF i_provider IS NOT INITIAL "" But a new Provider was selected in Request Edit: We neet to add approval Step& Partner function
      AND NOT line_exists( v_apprv_step_exists[ company_partner = v_partner  function = zif_ib_tur_const=>c_partner_functions-company_partner ] ). "  partner is not customer
        v_upd_appr_step = abap_true.
        IF i_provider IS NOT INITIAL
              AND line_exists( v_apprv_step_exists[ company_partner = v_partner  function = zif_ib_tur_const=>c_partner_functions-partner_exch ] ) . " Partner already incl. as Coop Exh
          "Remove Coop Exchange Approval
          LOOP AT v_apprv_step_exists ASSIGNING <appr_st>
         WHERE ( function = zif_ib_tur_const=>c_partner_functions-partner_exch AND company_partner = v_partner ) .
*            CHECK v_partner <> <appr_st>-company_partner.
            APPEND <appr_st>-record_id TO t_axt_record_id.
            DELETE v_apprv_step_exists.
          ENDLOOP.
          IF t_axt_record_id IS NOT INITIAL.
            me->remove_approval_step(
*                      EXPORTING
*                        i_item_number =
                i_record_ids  = t_axt_record_id
            ).
          ENDIF.
        ENDIF.
        v_upd_appr_step = abap_true.
      ENDIF.
    ENDIF.
    CHECK v_upd_appr_step = abap_true.
    me->add_partner(
      EXPORTING
        i_partner        = v_partner
        i_partner_fct    = zif_ib_tur_const=>c_partner_functions-provider
*    i_with_appr_step = abap_true
    i_service_id     = i_service_id
    ).
  ENDMETHOD.


  METHOD constructor.

    me->order_api = i_order_api.
    me->order_header = me->order_api->get_orderadm_h_extension_struc( ).
  ENDMETHOD.


  METHOD convert_2_tstmp.
    CONVERT DATE i_date TIME i_time INTO TIME STAMP r_result TIME ZONE sy-zonlo.
  ENDMETHOD.


  METHOD create_request.

  ENDMETHOD.


  METHOD delete_installation.

    CASE i_is_real_ib.
      WHEN abap_true.

        me->order_item_api = me->order_api->get_item_by_item_no( i_key = i_line_item_no ).

        CHECK me->order_item_api IS BOUND.

        me->order_item_api->remove_reference_object(
          EXPORTING
            i_refobj_val = i_record_guid
            i_field_name       = |IB_COMP_REF_GUID|
        ).

      WHEN abap_false.

        DATA(v_order_processor) = NEW zcl_tur_order_processing( i_request_guid = me->order_header-guid ).
        v_order_processor->delete_virtual_installation( i_request_guid = me->order_header-guid
                                                        i_line_item_id = i_line_item_no ).

    ENDCASE.
  ENDMETHOD.


  METHOD delete_note.
    me->order_api->delete_text(
      EXPORTING
        i_tdid  = i_text_id
        i_langu = i_langu
    ).
  ENDMETHOD.


  METHOD delete_service_from_request.
    DATA t_appr_steps_ids TYPE ty_axt_recoord_id_tt.
*/*  NOTES */*

* When a service is deleted, all corresponding approval steps relating to the service.
    SELECT SINGLE service_id INTO @DATA(v_service_id) FROM zib_tur_order_services_cds WHERE line_item_no = @i_item_number AND request_guid = @me->order_header-guid.
    CHECK sy-subrc = 0.
    "Delete Service: Item
    DATA(v_deleted) = me->order_api->delete_item( EXPORTING i_item_number = i_item_number ).

    SELECT record_id INTO TABLE @t_appr_steps_ids FROM zib_tur_order_appr_steps WHERE service_id = @v_service_id AND request_guid = @me->order_header-guid.
    CHECK sy-subrc = 0.
    "Remove Approval Step
    IF v_deleted = abap_true.
      me->remove_approval_step( i_item_number = i_item_number i_record_ids = t_appr_steps_ids ).
    ENDIF.

  ENDMETHOD.


  METHOD description.



    IF i_req_type IS INITIAL.
      me->order_api->get_orderadm_h_extension_field(
        EXPORTING
          i_field_name = c_field_name_desc
        IMPORTING
          e_result     = r_description
      ).
    ELSE.
*--- Set Request Type
      me->order_api->set_customer_h_extension_field(
        EXPORTING
          i_field_name = c_cust_h_field_name_req_type
          i_value      = i_req_type
      ).
*--- Set Description
      SELECT SINGLE description
     FROM zpt_req_type_c
     INTO @r_description
     WHERE request_type = @i_req_type.
      CHECK sy-subrc = 0.

      me->order_api->set_orderadm_h_extension_field(
        EXPORTING
          i_field_name = c_field_name_desc

          i_value      = r_description
      ).
    ENDIF.

  ENDMETHOD.


  METHOD detemine_text_id.

    DATA v_author_is_customer TYPE abap_bool.

    DATA(v_pfs) = me->order_api->get_partner_list(  ).
    DATA(author_pfs) = VALUE zcrm_api_order_partner_t( FOR l IN v_pfs WHERE ( partner = i_author   ) ( l ) ).

*    SELECT SINGLE @abap_true INTO @v_author_is_customer FROM zbtx_tur_order_b WHERE order_guid = @me->order_header-guid
*        AND company_partner = @i_author.

    IF line_exists( author_pfs[ partner_fct = zif_ib_tur_const=>c_partner_functions-company_partner ] ).
      r_tdid = zif_ib_tur_const=>c_text_ids-customer_notes.
    ELSEIF line_exists( author_pfs[ partner_fct = zif_ib_tur_const=>c_partner_functions-provider ] ).
      r_tdid = zif_ib_tur_const=>c_text_ids-provider_notes.
    ELSEIF line_exists( author_pfs[ partner_fct = zif_ib_tur_const=>c_partner_functions-partner_exch ] ).
      SELECT SINGLE text_id INTO @r_tdid FROM zpt_pex_c WHERE partner_exchange = @i_author.
      CHECK sy-subrc = 0.
    ENDIF.

*    IF sy-subrc <> 0. "request is new, therefore Author of note ist customer mqking the request
*
*
*
*    ELSE.
*
*      CASE v_author_is_customer.
*
*        WHEN abap_true.
*          r_tdid = zif_ib_tur_const=>c_text_ids-customer_notes.
*        WHEN OTHERS.
*          SELECT SINGLE function INTO @DATA(v_pf) FROM zib_tur_order_appr_steps WHERE request_guid = @me->order_header-guid AND company_partner = @i_author.
*          CHECK sy-subrc = 0.
*          IF v_pf = zif_ib_tur_const=>c_partner_functions-provider.
*
*          ELSEIF v_pf = zif_ib_tur_const=>c_partner_functions-partner_exch.
*
*          ENDIF.
*      ENDCASE.
*    ENDIF.

  ENDMETHOD.


  METHOD exec_on_approval.

  ENDMETHOD.


  METHOD format_text.
    DATA v_converter TYPE REF TO if_crm_text_format_conversion.
    DATA: lr_obj                    TYPE REF TO object.
    DATA: v_text_head               TYPE thead.
    DATA: lt_return TYPE bapiret2_t .
    DATA: v_text TYPE string.

    IF i_text CP '*<*>*'.
      DATA(v_class) =     cl_gstext_tools=>get_converter_class_name( iv_text_object = i_text_object
                                                                     iv_text_type   = i_tdid ).


      IF v_class IS INITIAL..
        v_class = 'CL_CRM_TEXT_FORMAT_CONVERSION'.
      ENDIF.

      CREATE OBJECT lr_obj TYPE (v_class).
      v_converter  ?= lr_obj.


      v_text_head-tdobject  = i_text_object.
      v_text_head-tdid  =   i_tdid.
      v_text_head-tdspras = i_langu.
      v_text_head-tdform  = 'SYSTEM'.
      v_text_head-tdstyle = 'SYSTEM'.

      v_converter->initialize( is_thead = v_text_head ).



      CALL METHOD v_converter->convert_html_to_itf
        EXPORTING
          iv_html_text = i_text
        IMPORTING
          ev_itf_text  = r_formatted_text
          et_return    = lt_return.
    ELSE.
      r_formatted_text = i_text.
    ENDIF.
  ENDMETHOD.


  METHOD generate_print.

    zcl_pt_rac_order_prints=>create_pdf(
                EXPORTING
                  i_order_guid      = i_order_guid
                  i_append_2_order  = abap_true
                  i_request_type    = i_request_type
*              i_company_partner =
*            RECEIVING
*              r_order_pdf       =
              ).

  ENDMETHOD.


  METHOD get_approval_steps.
    DATA  t_approval_steps        TYPE STANDARD TABLE OF zbtx_aet_tur_as_bol_attr.
    me->order_api->get_extension_list(
           EXPORTING
             i_extension_name = zif_ib_tur_const=>c_aet_extensions-approval_steps
           IMPORTING
             e_extension_list = t_approval_steps
         ).

    CASE i_status.
      WHEN '00'.
        r_result =  VALUE #( FOR ap IN t_approval_steps WHERE ( zzstatus = '' ) ( record_id    = ap-record_id
                                                                                  request_guid = ap-object_id
                                                                                  request_id   = ap-zzreq_id
                                                                                  service_id   = ap-zzservice_id
                                                                                  approval_step = ap-zzapproval_step
                                                                                  company_partner = ap-zzpartner
                                                                                  function = ap-zzpartner_as
                                                                                  step_status = ap-zzstatus
                                                                                  processed_by = ap-zzprocessed_by
                                                                                  processed_on = ap-zzprocessed_on
                                                                                             ) ).
      WHEN '01'.
        r_result =  VALUE #( FOR ap IN t_approval_steps WHERE ( zzstatus NE '' ) ( record_id = ap-record_id
                                                                                            request_guid = ap-object_id
                                                                                            request_id  = ap-zzreq_id
                                                                                            service_id = ap-zzservice_id
                                                                                            approval_step   = ap-zzapproval_step
                                                                                             company_partner = ap-zzpartner
                                                                                             function = ap-zzpartner_as
                                                                                             step_status = ap-zzstatus
                                                                                              processed_by = ap-zzprocessed_by
                                                                                              processed_on = ap-zzprocessed_on
                                                                                             ) ).
      WHEN OTHERS.
        r_result =  VALUE #( FOR ap IN t_approval_steps  ( record_id = ap-record_id
                                                                                            request_guid = ap-object_id
                                                                                            request_id  = ap-zzreq_id
                                                                                            service_id = ap-zzservice_id
                                                                                            approval_step   = ap-zzapproval_step
                                                                                             company_partner = ap-zzpartner
                                                                                             function = ap-zzpartner_as
                                                                                             step_status = ap-zzstatus
                                                                                              processed_by = ap-zzprocessed_by
                                                                                              processed_on = ap-zzprocessed_on
                                                                                             ) ).
    ENDCASE.
  ENDMETHOD.


  METHOD get_partners.

    DATA v_order_partners TYPE STANDARD TABLE OF zbtx_tur_partner_s.

    DATA(v_partners) = me->order_api->get_partner_list( ).

*    DATA(v_partner_ids) = VALUE rsdsselopt_t( FOR line IN v_partners
*                                                          ( sign = 'I' option = 'EQ' low = CONV bu_partner(  |{ line-partner ALPHA = IN }| ) ) ).

*    SELECT c~contact_partner_id,
*           c~first_name,
*            c~last_name,
*            c~mail,
*            ph~country_id,
*            ph~tel_number,
*            ph~tel_ext,
*            ph~phone_number,
*            ph~sequence_number,
*            ph~phone_number_type
*
*             FROM zcds_ms_contact AS c
*             LEFT OUTER JOIN
*             zcds_ms_contact_phone AS ph
*             ON c~contact_partner_id = ph~contact_partner_id AND c~customer_id = ph~customer_id
*              INTO TABLE @DATA(v_contact_details)
*              FOR ALL ENTRIES IN @v_partners WHERE c~contact_partner_id = @v_partners-partner
*                AND c~customer_id = @i_company_account.
*    CHECK sy-subrc = 0.
*    SORT v_contact_details BY contact_partner_id sequence_number phone_number_type.
*    DELETE ADJACENT DUPLICATES FROM v_contact_details COMPARING contact_partner_id.
**                AND
**                   ph~sequence_number = '001'.
*
*    LOOP AT v_contact_details ASSIGNING FIELD-SYMBOL(<cont_detail>)
*        WHERE country_id IS NOT INITIAL
*        AND tel_number IS NOT INITIAL
*        AND tel_ext IS NOT INITIAL.
*      CALL FUNCTION 'TELNUMBER_FORMAT'
*        EXPORTING
*          country          = <cont_detail>-country_id             " Country code
*          telnumber        = <cont_detail>-tel_number
*          extension_in     = <cont_detail>-tel_ext
**         wildcards        = space
*        IMPORTING
*          number_canonical = <cont_detail>-phone_number
**         country_code     =                  " Country code
**         area_code        =
**         subscriber       =                  " Connection
**         extension_out    =
**         worst_error      =
**         number_canonical_long =
**      TABLES
**         messages         =                  " Error Messages
*        .
*    ENDLOOP.

    v_order_partners = VALUE #( FOR p IN v_partners "WHERE ( partner_fct = zif_ib_tur_const=>c_partner_functions-line_resp_loc1
                                                     "OR partner_fct = zif_ib_tur_const=>c_partner_functions-line_resp_loc2  )
                                       ( partner        = p-partner
*                                         last_name      = VALUE #( v_contact_details[ contact_partner_id = |{ p-partner ALPHA = IN }| ]-last_name OPTIONAL )
*                                         first_name     = VALUE #( v_contact_details[ contact_partner_id = |{ p-partner ALPHA = IN }| ]-first_name OPTIONAL )
                                         function       = p-partner_fct
*                                         phone          = VALUE #( v_contact_details[ contact_partner_id = |{ p-partner ALPHA = IN }| ]-phone_number OPTIONAL )
*                                         email          = VALUE #( v_contact_details[ contact_partner_id = |{ p-partner ALPHA = IN }| ]-mail OPTIONAL )
)
                                         ).



    r_order_partners = CORRESPONDING #( v_order_partners ).
    IF i_partner_fct IS NOT INITIAL.
      DELETE r_order_partners WHERE function NE i_partner_fct. "last_name NE i_partner_fct.
    ENDIF.

  ENDMETHOD.


  METHOD get_related_requests.
    DATA(v_docflow_list) = me->order_api->get_docflow_list( ).
    r_related_requests = VALUE #( FOR l IN v_docflow_list WHERE ( reltype = 'SRCE' ) ( l ) ). "SRCE is BinRelType used in Splitted Requests

  ENDMETHOD.


  METHOD get_request_guid.
    IF me->order_header IS INITIAL.
      me->order_header = me->order_api->get_orderadm_h_extension_struc( ).
    ENDIF.
    r_request_guid = me->order_header-guid.
  ENDMETHOD.


  METHOD get_request_id.
    IF me->order_header IS INITIAL.
      me->order_header = me->order_api->get_orderadm_h_extension_struc( ).
    ENDIF.
    r_request_id = me->order_header-object_id.
  ENDMETHOD.


  METHOD get_request_type.
    me->order_api->get_customer_h_extension_field(
      EXPORTING
        i_field_name =  c_cust_h_field_name_req_type
      IMPORTING
        e_result     = r_request_type

    ).
  ENDMETHOD.


  METHOD get_service_items.
    DATA: t_zastruc0002wc     TYPE STANDARD TABLE OF zastruc0002wc,
          t_zastruc0002wc_tmp TYPE STANDARD TABLE OF zastruc0002wc,
          service_item        TYPE zbtx_tur_order_serv_s.

    DATA(v_items) = me->order_api->get_item_list( ).

    LOOP AT v_items ASSIGNING FIELD-SYMBOL(<items>).

      <items>-item_api->get_extension_list( EXPORTING i_extension_name = zif_ib_tur_const=>c_aet_extensions-service_items
                                         IMPORTING e_extension_list = t_zastruc0002wc_tmp ).
      APPEND LINES OF t_zastruc0002wc_tmp TO t_zastruc0002wc.
      LOOP AT t_zastruc0002wc_tmp ASSIGNING FIELD-SYMBOL(<i>).
        service_item = CORRESPONDING #( <i> MAPPING   service_id = zz_service_id
                                                      environment = zz_environm_no
                                                      bearer_service_id = zz_bearer_serv
                                                      action =  zz_action
                                                      related_rel_nr =  zzbu_rel_nr
                                                      business_id  = zz_business_id ).
        service_item-line_item_no = <items>-item_properties-number_int.
        APPEND service_item TO r_service_items_tab.

      ENDLOOP.

    ENDLOOP.
    .
*    CHECK t_zastruc0002wc IS NOT INITIAL.

*    r_service_items_tab = VALUE #( FOR l IN t_zastruc0002wc ( CORRESPONDING #( l MAPPING   service_id = zz_service_id
*                                                                                           environment = zz_environm_no
*                                                                                           bearer_service_id = zz_bearer_serv
*                                                                                           action =  zz_action
*                                                                                           related_rel_nr =  zzbu_rel_nr
*                                                                                           business_id  = zz_business_id
*                                                                          ) ) ).
  ENDMETHOD.


  METHOD get_status.
    r_status = me->order_api->status( ).
  ENDMETHOD.


  METHOD installations.
    DATA v_ib_id TYPE ib_ibase.

    v_ib_id = COND #( WHEN i_service-installation_sub IS NOT INITIAL THEN i_service-installation_sub
                      WHEN i_service-installation_base IS NOT INITIAL THEN i_service-installation_base ).

    SELECT SINGLE ib_guid_16 FROM ibib INTO @DATA(v_ib_guid) WHERE ibase = @v_ib_id.
    IF sy-subrc = 0.
*      me->order_item_api = me->order_api->get_ite
      me->order_item_api->add_reference_object(
        EXPORTING
          i_reference_object = v_ib_guid
          i_field_name       = 'IB_COMP_REF_GUID'
      ).
    ENDIF.

  ENDMETHOD.


  METHOD map_service_fields.

    DATA v_input_data TYPE ty_generic_structure.
    v_input_data-service_details = CORRESPONDING #( i_input ).
    v_input_data-location_details = CORRESPONDING #( i_input ).

    product_i_props = CORRESPONDING #( v_input_data-service_details
                                                   MAPPING zzbandwidth_i        = bandwidth
                                                           zzbusiness_id_i      = business_id
                                                           zz_conn_type         = sess_conn_type
                                                           zzprovider_id        = provider_id
                                                           zzbill_id            = billing_id
                                                           zzsess_rout_type     = routing_type
                                                           zzbus_unit_type      = bu_type
                                                           zz_fix_version       = fix_version
                                                           zzenvironment_i      = environment  "Either here or in ZBTX_AET_TUR_I below
                                                           zzmax_ord_rq_to      =  max_ord_req_timeout
                                                           zzdrop_copy_po       = drop_copy_order
                                                           zz_eti_sess_name     = session_name
                                                           zz_eti_sess_id       = session_number
                                                           zzrpt_trad_allow     = allow_rep_trades
                                                           zzrpt_quot_allow     = allow_rep_quotes


                                                        ) .
    IF v_input_data-service_details-product_description IS NOT INITIAL.
      product_i_props-zzafld0000aw = v_input_data-service_details-product_description.

    ELSEIF v_input_data-service_details-related_product IS NOT INITIAL .
      DATA(v_product_id) = CONV comt_product_id( |{ v_input_data-service_details-related_product ALPHA = IN }| ).
      SELECT SINGLE shtext_large FROM zib_fast_select1 INTO @DATA(v_prd_desc) WHERE product_id = @v_product_id.
      IF sy-subrc = 0.
        product_i_props-zzafld0000aw = v_prd_desc.
      ENDIF.
    ENDIF.

    extension1 = CORRESPONDING #(  v_input_data-service_details MAPPING zz_service_id  = service_id
                                                                        zz_environm_no = environment
                                                                        zz_bearer_serv = bearer_service_id
                                                                        zz_action      = action
                                                                        zzbu_rel_nr    = related_rel_nr
                                                                        zz_business_id = business_id
                                                   ).

    extension2 = CORRESPONDING #( v_input_data-location_details
                                          MAPPING zz_location_id = location_id
                                                  zzfloor_id     = floor_id
                                                  zz_room_id     = room_id
                                                  zz_rack_no     = rack
                                                  zzrack_unit    = rack_unit
                                                  zzmedia        = media
                                                  zzint_type_text = interface_text
                                                  zzinterface    =  interface
                                                  zzcable_entry   = cable_entry_address
                                                  zzout_of_hours = out_of_business_hour
                                                  zzpublic_ip    = public_ip
                                                  zz_is_old_loc  = is_old
                                                  zzloc_order    = position_in_order
                                                  ).

  ENDMETHOD.


  METHOD process_approval.

    DATA(v_pt_order_appr_handler) = zcl_pt_order_appr_api=>get_instance( i_request_type )->get_approval_handler( i_request_guid ).
*    DATA(v_results) = c_results.
    IF v_pt_order_appr_handler IS BOUND.

      IF ( i_request_type = 'PORT009' OR i_request_type = 'PORT010' OR i_request_type = 'PORT012' OR i_request_type = 'PORT014' OR i_request_type = 'PORT016'
      OR i_request_type = 'PORT017'   OR i_request_type = 'PORT034' OR i_request_type = 'PORT035' OR i_request_type = 'PORT038' OR i_request_type = 'PORT039'
      OR i_request_type = 'PORT042'   OR i_request_type = 'PORT043' OR i_request_type = 'PORT046' OR i_request_type = 'PORT047' OR i_request_type = 'PORT003'
      OR i_request_type = 'PORT007')
       AND i_3rd_party_appr_needed = abap_false."

        "GENERATE and append PDF before framework.

        generate_print(
          EXPORTING
            i_request_type = i_request_type
            i_order_guid   = i_request_guid
        ).

        DATA(v_pdf_generated) = abap_true.
        c_results-print_generated = v_pdf_generated.
      ENDIF.

      "Call order Approval and execution framework..
      v_pt_order_appr_handler->execute( EXPORTING i_approver                = i_approver
                                                  i_3rdparty_appr_missing   = i_3rd_party_appr_needed
                                                  i_cust_pft                = i_cust_pft
                                                  i_status                  = i_status

                                        IMPORTING e_appr_exec_results = DATA(v_apprv_exec_res)

                                       ).


*      IF v_apprv_exec_res-order_status IS NOT INITIAL AND v_apprv_exec_res-order_status <> c_results-new_overall_status.

      IF v_pt_order_appr_handler->further_exec_required = abap_true. " For Cancel Connection, Cancel Installation; further processing is required by RF

        IF v_apprv_exec_res-order_status = zif_ib_tur_const=>co_tur_order_status-completed.

          c_results-new_overall_status = zif_ib_tur_const=>co_tur_order_status-approved.

        ELSE.

          c_results-new_overall_status = v_apprv_exec_res-order_status.

        ENDIF.

        me->order_api->status( i_new_status =  c_results-new_overall_status ).

      ELSE.
        c_results-new_overall_status = v_apprv_exec_res-order_status.
        me->order_api->status( i_new_status = v_apprv_exec_res-order_status ). "SPCRC-6161 // SPCRC-2375: Status of Request should move directly to completed when Session Framework was successful.

      ENDIF.

*      ENDIF.

    ELSE.

      me->order_api->status( i_new_status =  c_results-new_overall_status ).

    ENDIF..

    IF v_pdf_generated = abap_false AND ( c_results-new_overall_status = zif_ib_tur_const=>co_tur_order_status-approved
                                        OR c_results-new_overall_status = zif_ib_tur_const=>co_tur_order_status-completed
      ).
      "generate and append pdf
*      generate_print(
*          EXPORTING
*            i_request_type = i_request_type
*            i_order_guid   = i_request_guid
*        ).
*      zcl_pt_rac_order_prints=>create_pdf(
*        EXPORTING
*          i_order_guid      = i_request_guid
*          i_append_2_order  = abap_true
*          i_request_type    = i_request_type
**          i_company_partner =
**        RECEIVING
**          r_order_pdf       =
*      ).
    ENDIF.

*    IF i_request_type = zif_ib_tur_const=>c_pt_req_types-new_eti OR
*       i_request_type = zif_ib_tur_const=>c_pt_req_types-new_fix OR
*       i_request_type = zif_ib_tur_const=>c_pt_req_types-new_fix_lf OR
*       i_request_type = zif_ib_tur_const=>c_pt_req_types-new_rrh OR
*       i_request_type = zif_ib_tur_const=>c_pt_req_types-new_eti_on_behalf OR
*       i_request_type = zif_ib_tur_const=>c_pt_req_types-new_fix_on_behalf OR
*       i_request_type = zif_ib_tur_const=>c_pt_req_types-new_fix_lf_on_behalf
*    .

    LOOP AT v_apprv_exec_res-order_items ASSIGNING FIELD-SYMBOL(<exec_items>) WHERE related_product IS NOT INITIAL.
      me->zif_ib_tur_order~services(
        EXPORTING
          i_service         = <exec_items>
          i_company_account = <exec_items>-company_partner
*    i_product         =
*  RECEIVING
*    r_order_service   =
      ).


    ENDLOOP.
*    ENDIF.
*    if Error occured in Session Framework: throw exception
    IF v_apprv_exec_res-exception IS NOT INITIAL.
      DATA(v_msg) = v_apprv_exec_res-exception->get_text( ).
      zcx_crm_app=>raise_exec_failed(
        EXPORTING
          i_text       = v_msg "'The creation of the requested session(s) failed. Please contact your Technical Key Account Manager'
*            i_empty_text =
      ).
    ENDIF.
    CHECK v_apprv_exec_res-session_passwords IS NOT INITIAL.
    LOOP AT v_apprv_exec_res-session_passwords ASSIGNING FIELD-SYMBOL(<pwd>).
      CLEAR <pwd>-password_hash.
    ENDLOOP.
    r_sess_passwords = CORRESPONDING #( v_apprv_exec_res-session_passwords MAPPING product_description =  shtext_large
                                                            session_id          = session_id
                                                             ).
  ENDMETHOD.


  METHOD remove_all_approval_steps.
***    CAUTION """""""
*DO NOT CALL THIS METHOD AT PLACES: it is designed to be called only when a Customer/Requester cancells his request
    DATA v_approval_steps_list TYPE STANDARD TABLE OF zbtx_aet_tur_as_bol_attr.
    DATA(v_order_status) = me->order_api->status( ).

    IF v_order_status-status = zif_ib_tur_const=>co_tur_order_status-cancelled.
      me->order_api->get_extension_list( EXPORTING i_extension_name = zif_ib_tur_const=>c_aet_extensions-approval_steps IMPORTING e_extension_list = v_approval_steps_list  ).
      CHECK v_approval_steps_list IS NOT INITIAL.

      LOOP AT v_approval_steps_list ASSIGNING FIELD-SYMBOL(<as>).
        IF NOT <as>-zzstatus = zif_ib_tur_const=>c_approval_step_status-cancelled.
          me->order_api->delete_extension_list_entry(
            EXPORTING
              i_extension_name = zif_ib_tur_const=>c_aet_extensions-approval_steps
              i_record_guid    = <as>-record_id
          ).
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD remove_approval_step.

*  */* Local type definition:
    TYPES: BEGIN OF ty_partner_pfct,
             partner TYPE bu_partner,
             pfct    TYPE crmt_partner_fct,
           END OF ty_partner_pfct,
           ty_partner_pfct_tt TYPE STANDARD TABLE OF ty_partner_pfct WITH DEFAULT KEY.
*           * */* End Local Type Definiton *****

    CHECK i_item_number IS NOT INITIAL OR i_record_ids IS NOT INITIAL.

    DATA(v_order_guid) = me->order_header-guid.
* * Fetch all Approval Steps for request:
    SELECT * FROM zib_tur_order_appr_steps INTO TABLE @DATA(v_all_as) WHERE request_guid = @v_order_guid.
    IF sy-subrc = 0.
      IF i_record_ids IS INITIAL.
* Determine Approval Step(s) ro be deleted, as per deleted Service
        SELECT *

         FROM zib_tur_order_appr_steps AS _as
         JOIN zib_tur_order_services_cds AS v ON _as~request_guid = v~request_guid

                 AND _as~service_id = v~service_id

         WHERE v~line_item_no = @i_item_number
         AND v~request_guid = @v_order_guid
         INTO TABLE @DATA(v_as_del).

      ELSE.
        SELECT * FROM zib_tur_order_appr_steps AS _as
*        JOIN z_tur_services_v AS v ON _as~request_guid = v~request_guid
*
*                 AND _as~service_id = v~service_id
              FOR ALL ENTRIES IN @i_record_ids WHERE record_id = @i_record_ids-table_line
                INTO TABLE @DATA(v_as_del_temp).
        CHECK sy-subrc = 0.
        v_as_del = VALUE #( FOR l IN v_as_del_temp ( _as = CORRESPONDING #( l ) ) ).
      ENDIF.
      DELETE v_as_del WHERE _as-function = zif_ib_tur_const=>c_partner_functions-company_partner.

      DATA v_partners_pfct TYPE ty_partner_pfct_tt.

      v_partners_pfct = VALUE #( FOR line IN v_as_del WHERE ( _as-function <> zif_ib_tur_const=>c_partner_functions-company_partner )
                                           ( partner = line-_as-company_partner
                                           pfct      = line-_as-function )
                                            ).
      CHECK v_as_del IS NOT INITIAL.
      LOOP AT v_as_del ASSIGNING FIELD-SYMBOL(<v>).
        me->order_api->delete_extension_list_entry(
            EXPORTING
              i_extension_name = zif_ib_tur_const=>c_aet_extensions-approval_steps
              i_record_guid    = <v>-_as-record_id
          ).
*      APPEND <v>-company_partner TO v_partners where.

        DELETE v_all_as WHERE record_id = <v>-_as-record_id.
      ENDLOOP.
* After deleting approval Step, we need to see if Approval Partner is involved for other Services still in request;
*Otherwise we need to remove the Partner from Partner Functions as well:
      IF v_all_as IS NOT INITIAL.
        LOOP AT v_partners_pfct ASSIGNING FIELD-SYMBOL(<p>).
          IF line_exists( v_all_as[ company_partner = <p>-partner ] ).
            DELETE v_partners_pfct.
          ENDIF.
        ENDLOOP.
        LOOP AT v_partners_pfct ASSIGNING <p>.
          me->remove_partner(
            EXPORTING
              i_partner     = <p>-partner
              i_partner_fct = <p>-pfct
          ).
        ENDLOOP.

      ENDIF.
    ELSE. " nothing to delete

    ENDIF.
  ENDMETHOD.


  METHOD remove_partner.
    me->order_api->delete_partner(
      EXPORTING
        i_partner_fct = i_partner_fct
        i_partner     = i_partner
*    i_main        = 'X'
    ).
  ENDMETHOD.


  METHOD update_installation.
    DATA v_ib_id TYPE ib_ibase.
    DATA v_service_extensions TYPE STANDARD TABLE OF zastruc0002wc.

    me->order_item_api = me->order_api->get_item_by_item_no( i_key = i_new_inst-line_item_no ).

    IF me->order_item_api IS NOT BOUND. " try to get the right item from Service ID and request ID:

*      IF sy-subrc = 0.

      DATA(v_items) = me->order_api->get_item_list( ).

      LOOP AT v_items ASSIGNING FIELD-SYMBOL(<items>).

        DATA(temp_item_api) = <items>-item_api.

        temp_item_api->get_extension_list( EXPORTING i_extension_name = zif_ib_tur_const=>c_aet_extensions-service_items
                                           IMPORTING e_extension_list = v_service_extensions ).
        TRY.
            DATA(v_guid) = v_service_extensions[ zzrequest_id = me->order_api->get_orderadm_h_extension_struc( )-object_id zz_service_id = i_new_inst-service_id ]-parent_id.
          CATCH cx_sy_itab_line_not_found INTO DATA(v_exc).
        ENDTRY.
        IF v_guid IS NOT INITIAL.
          me->order_item_api = temp_item_api.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CHECK me->order_item_api IS BOUND.

    v_ib_id = COND #( WHEN i_new_inst-installation_sub IS NOT INITIAL THEN i_new_inst-installation_sub
                      WHEN i_new_inst-installation_base IS NOT INITIAL THEN i_new_inst-installation_base ).

    SELECT SINGLE ib_guid_16 FROM ibib INTO @DATA(v_ib_guid) WHERE ibase = @v_ib_id.
    IF sy-subrc = 0.

      me->order_item_api->modify_reference_object(
        EXPORTING
          i_old_refobj_val      = i_new_inst-record_guid
          i_new_refobj_value = VALUE #( ib_comp_ref_guid = v_ib_guid )
          i_field_name       = |IB_COMP_REF_GUID|
      ).


    ENDIF.
  ENDMETHOD.


  METHOD zif_ib_tur_order~approve_order.
    DATA: v_approval_entries      TYPE zbtx_tur_order_apprval_step_tt,
          v_3rd_party_appr_needed TYPE abap_bool,
          t_approval_steps        TYPE STANDARD TABLE OF zbtx_aet_tur_as_bol_attr,
          v_missing_approvals     TYPE STANDARD TABLE OF zib_tur_order_appr_steps.

    IF i_do_check = abap_true.
      DATA(v_check_result) = check_b4_appr( i_company_partner = c_approval_step-partner i_partner_fct = c_approval_step-partner_as ).
    ENDIF.
    IF  v_check_result IS INITIAL.

      DATA(approved_by) = c_approval_step-processed_by.

      DATA(v_request_type) = get_request_type( ).

**** Notes
* 1.) Table zbtx_aet_tur_as can contain an approval step for same Partner multiple times for different Services: but approval only required once:
* 2.) Table zbtx_aet_tur_as can contain only one approval step for same Partner for same Service even if partner carries different roles e.g partner is Provider and Coop Exchange: Only Coop Exchange Approval will be relevant
* 3.) But if Partner has different roles for different Services e.g Partner Exchange for S1 and Provider for S2: Needs to clarify if both Approvals are requiredor which has hiher priority: Current Implementation assumes that both are required


* Get all Open approvals"
*      SELECT * INTO TABLE @DATA(v_missing_approvals) FROM zib_tur_order_appr_steps WHERE request_guid = @c_approval_step-request_guid AND step_status = ''.
*      IF sy-subrc <> 0.
      me->order_api->get_extension_list(
        EXPORTING
          i_extension_name = zif_ib_tur_const=>c_aet_extensions-approval_steps
        IMPORTING
          e_extension_list = t_approval_steps
      ).

      v_missing_approvals = get_approval_steps( i_status = '00' ).
*      v_missing_approvals = VALUE #( FOR ap IN t_approval_steps WHERE ( zzstatus = '' ) ( record_id = ap-record_id
*                                                                                          request_guid = ap-object_id
*                                                                                          request_id  = ap-zzreq_id
*                                                                                          service_id = ap-zzservice_id
*                                                                                          approval_step   = ap-zzapproval_step
*                                                                                           company_partner = ap-zzpartner
*                                                                                           function = ap-zzpartner_as
*                                                                                           step_status = ap-zzstatus
*                                                                                           ) ).
*      ENDIF.

      DATA v_my_missing_approvals TYPE STANDARD TABLE OF zib_tur_order_appr_steps.
*  Check if corresponding Approval Step is open for the corresponding Partner

      LOOP AT v_missing_approvals ASSIGNING FIELD-SYMBOL(<missing>) WHERE company_partner = c_approval_step-partner.

        DATA(v_my_missing_approval) = <missing>.
        APPEND v_my_missing_approval TO v_my_missing_approvals.
        DELETE v_missing_approvals. " We delete our missing approval entry from all missing approvals so we can check later if open approvals from other partners exist

      ENDLOOP.


      CHECK v_my_missing_approvals IS NOT INITIAL. " Open Approval(s) for corresponding Approval Step found
      c_approval_step-partner_as = v_my_missing_approvals[ 1 ]-function.
      DATA(v_req_guit_tab) = VALUE crmt_object_guid_tab( FOR line IN v_my_missing_approvals ( line-request_guid ) ).

      DATA(v_processing_status) = c_approval_step-status.

      CASE v_processing_status.

        WHEN zif_ib_tur_const=>c_approval_step_status-rejected.

          me->order_api->status( i_new_status = zif_ib_tur_const=>co_tur_order_status-rejected  ).

          v_results = VALUE #( request_guid = c_approval_step-request_guid
                               approval_complete = abap_true
                               new_overall_status = zif_ib_tur_const=>co_tur_order_status-rejected ).
          IF v_missing_approvals IS NOT INITIAL.
            v_3rd_party_appr_needed = abap_true.
          ENDIF.
*          If a New Session Request which customer had already approved is rejected by Provider;
*            The Sessions that were created need to be cancelled
*          Ticket 20972222: should not be executed if the request is still in draft mode (i.e. the requester cancels it)

          SELECT SINGLE mandt FROM crm_jest
            INTO @DATA(mandt)
            WHERE objnr = @c_approval_step-request_guid
              AND inact = ' '
              AND stat = @zif_ib_tur_const=>co_tur_order_status-draft.

          IF sy-subrc NE 0.
            check_cancel_sessions( EXPORTING i_3rd_party_appr_needed = v_3rd_party_appr_needed
                                             i_request_type          = v_request_type
                                             i_approval_step         = c_approval_step
                                  CHANGING  c_results = v_results ).
          ENDIF.
        WHEN zif_ib_tur_const=>c_approval_step_status-replace.

          me->order_api->status( i_new_status = zif_ib_tur_const=>co_tur_order_status-replaced  ).

*          generate_print( i_request_type  = v_request_type i_order_guid = c_approval_step-request_guid ).

          c_approval_step-status = zif_ib_tur_const=>c_approval_step_status-approved.

          v_results = VALUE #( request_guid = c_approval_step-request_guid
                               approval_complete = abap_true
                               new_overall_status = zif_ib_tur_const=>co_tur_order_status-replaced ).

        WHEN zif_ib_tur_const=>c_approval_step_status-waitlegal.

          me->order_api->status( i_new_status = zif_ib_tur_const=>co_tur_order_status-wait_legal_appr ).

          c_approval_step-status = zif_ib_tur_const=>c_approval_step_status-approved.

          v_results = VALUE #( request_guid = c_approval_step-request_guid
                               approval_complete = abap_true
                               new_overall_status = zif_ib_tur_const=>co_tur_order_status-wait_legal_appr ).

        WHEN zif_ib_tur_const=>c_approval_step_status-approved.
          " check if any other approval step from other partner is missing:
*
          IF v_missing_approvals IS NOT INITIAL.
            v_3rd_party_appr_needed = abap_true.
*          DATA(v_new_status) = zif_ib_tur_const=>co_tur_order_status-wait_3rd_party_appr.

            "Will nor longer be necessary once approval/Excution Framework is enabled
*          me->order_api->status( i_new_status = zif_ib_tur_const=>co_tur_order_status-waiting_approval ).

            v_results = VALUE #( request_guid = c_approval_step-request_guid
                                 approval_complete = abap_false
                                 new_overall_status = zif_ib_tur_const=>co_tur_order_status-waiting_approval ).

          ELSE. " Only current approval steps were missing

            "SPCRC-2375 // SPCRC-6161 - Open Internet and Session Requests should go directly to status "Completed" (instead of approved) when everything is approved
            "When no error occured in session framework (for Session Requests).

            IF v_request_type EQ zif_ib_tur_const=>c_pt_req_types-new_open_internet.
              DATA(v_new_status) = zif_ib_tur_const=>co_tur_order_status-completed.
            ELSE.
              v_new_status = zif_ib_tur_const=>co_tur_order_status-approved.
            ENDIF.

            v_3rd_party_appr_needed = abap_false.
            v_results = VALUE #( request_guid = c_approval_step-request_guid
                                 approval_complete = abap_true
                                 new_overall_status = v_new_status ).

*          DATA(v_pt_order_appr_handler) = zcl_pt_order_appr_api=>get_instance( v_req_type )->get_approval_handler( i_request_guid =  c_approval_step-request_guid  ).

            ""Will nor longer be necessary once approval/Excution Framework is enabled
*          me->order_api->status( i_new_status =  zif_ib_tur_const=>co_tur_order_status-approved ).

          ENDIF.

*++++++++++++++++++++ TO ENABLE APPROVAL Execution Framework: uncomment following lines
          e_sess_pwds = process_approval( EXPORTING i_request_guid           = c_approval_step-request_guid
                                                    i_request_type          = v_request_type
                                                    i_cust_pft              = c_approval_step-partner_as
                                                    i_3rd_party_appr_needed = v_3rd_party_appr_needed
*                                        i_pt_order_appr_handler = v_pt_order_appr_handler
                                                    i_approver              = c_approval_step-processed_by

                                            CHANGING c_results               = v_results
                                     ).

        WHEN zif_ib_tur_const=>c_approval_step_status-cancelled. " cancelled can only be done from customer who creates the request
          " if recancelled by Customer, all approval steps are no longer required and have to be removed
          me->order_api->status( i_new_status =  zif_ib_tur_const=>co_tur_order_status-cancelled ).
          v_results = VALUE #( request_guid = c_approval_step-request_guid
                              approval_complete = abap_true
                              new_overall_status = zif_ib_tur_const=>co_tur_order_status-rejected ).
*If a New Session Request which customer had already approved is not approved by provider or coop exchange after;
*            The Sessions that were created need to be cancelled

*          Ticket 20972222: should not be executed if the request is still in draft mode (i.e. the requester cancels it)

          SELECT SINGLE mandt FROM crm_jest
            INTO mandt
            WHERE objnr = c_approval_step-request_guid
              AND inact = ' '
              AND stat = zif_ib_tur_const=>co_tur_order_status-draft.

          IF sy-subrc NE 0.


            check_cancel_sessions( EXPORTING i_3rd_party_appr_needed = v_3rd_party_appr_needed
                                             i_request_type          = v_request_type
                                             i_approval_step         = c_approval_step
                                  CHANGING  c_results = v_results ).
          ENDIF.

          me->remove_all_approval_steps( ). "

        WHEN OTHERS.

      ENDCASE.

*    Approval Steps Entity will be updated now to avoid lock conflicts between approval framwework and odata call

      GET TIME STAMP FIELD DATA(v_timestamp).
      IF v_results-new_overall_status <> zif_ib_tur_const=>co_tur_order_status-draft.
        LOOP AT v_my_missing_approvals ASSIGNING FIELD-SYMBOL(<as>).
          <as>-step_status = c_approval_step-status.
          <as>-processed_by = c_approval_step-processed_by.

          <as>-processed_on = v_timestamp.

          DATA(v_extension_path) = |{ zif_ib_tur_const=>c_aet_extensions-approval_steps }[((@ZZPARTNER="{ <as>-company_partner }")&(@ZZSTATUS=""))]|.

          DATA(v_approval_step) = CORRESPONDING zbtx_aet_tur_as_bol_attr( t_approval_steps[ record_id = <as>-record_id ] ).

          v_approval_step-zzprocessed_by = <as>-processed_by.
          v_approval_step-zzstatus = <as>-step_status.
          v_approval_step-zzprocessed_on = <as>-processed_on.

          me->order_api->set_extension_struc(
            EXPORTING
              i_extension_name  = v_extension_path
              i_extension_value = v_approval_step
          ).

        ENDLOOP.
        IF c_approval_step-partner_as = zif_ib_tur_const=>c_partner_functions-company_partner AND (  c_approval_step-status = zif_ib_tur_const=>c_approval_step_status-approved
                                                                                                      OR c_approval_step-status = zif_ib_tur_const=>c_approval_step_status-waitlegal ).
*        If Customer approves, add corresponding partner Function
          me->add_partner(
            EXPORTING
              i_partner        = approved_by
              i_partner_fct    = zif_ib_tur_const=>c_partner_functions-approver
              i_with_appr_step = abap_false
*            i_service_id     =
          ).
        ENDIF.
      ENDIF.
* If request was approved or completed( in case of sessions and Open Internet) set approved date:
      IF v_results-new_overall_status = zif_ib_tur_const=>co_tur_order_status-approved
        OR v_results-new_overall_status = zif_ib_tur_const=>co_tur_order_status-completed.

        me->order_api->modify_appointment( i_appointment = 'ZAPPROVED' i_from_ts = v_timestamp ).

      ENDIF.

      c_approval_step = CORRESPONDING #( v_approval_step MAPPING request_guid = object_id
                                                                 approval_step = zzapproval_step
                                                                 partner       = zzpartner
                                                                 status        = zzstatus
                                                                 processed_by  = zzprocessed_by
                                                                 processed_on  = zzprocessed_on
                                                                  ).
    ELSE.
      CASE v_check_result.
        WHEN 'E1'.
          v_results-message = TEXT-001.
        WHEN 'E2'.
          v_results-message = TEXT-002.
      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD zif_ib_tur_order~attachments.

  ENDMETHOD.


  METHOD zif_ib_tur_order~author.
    me->order_api->modify_partner(

         EXPORTING
           i_partner_fct = zif_ib_tur_const=>c_partner_functions-author
           i_partner     = i_partner
           i_main        = 'X'
       ).

  ENDMETHOD.


  METHOD zif_ib_tur_order~company_partner.

    me->order_api->modify_partner(

      EXPORTING
        i_partner_fct = zif_ib_tur_const=>c_partner_functions-company_partner
        i_partner     = i_partner
        i_main        = 'X'
    ).

  ENDMETHOD.


  METHOD zif_ib_tur_order~installations.

    DATA v_ib_id TYPE ib_ibase.
    DATA v_service_extensions TYPE STANDARD TABLE OF zastruc0002wc.

    IF i_order_item_api IS BOUND.

      me->order_item_api = i_order_item_api.

    ELSEIF i_item_no IS NOT INITIAL.

      me->order_item_api = me->order_api->get_item_by_item_no( i_key = i_item_no ).

    ELSE. " try to get the right item from Service ID and request ID:

*      IF sy-subrc = 0.

      DATA(v_items) = me->order_api->get_item_list( ).

      LOOP AT v_items ASSIGNING FIELD-SYMBOL(<items>).

        DATA(temp_item_api) = <items>-item_api.

        temp_item_api->get_extension_list( EXPORTING i_extension_name = zif_ib_tur_const=>c_aet_extensions-service_items
                                           IMPORTING e_extension_list = v_service_extensions ).
        TRY.
            DATA(v_guid) = v_service_extensions[ zzrequest_id = me->order_api->get_orderadm_h_extension_struc( )-object_id zz_service_id = i_installations-service_id ]-parent_id.
          CATCH cx_sy_itab_line_not_found INTO DATA(v_exc).
        ENDTRY.
        IF v_guid IS NOT INITIAL.
          me->order_item_api = temp_item_api.
          EXIT.
        ENDIF.
      ENDLOOP.


*      ENDIF.
    ENDIF.

    CHECK me->order_item_api IS BOUND.

    v_ib_id = COND #( WHEN i_installations-installation_sub IS NOT INITIAL THEN i_installations-installation_sub
                      WHEN i_installations-installation_base IS NOT INITIAL THEN i_installations-installation_base ).

    SELECT SINGLE ib_guid_16 FROM ibib INTO @DATA(v_ib_guid) WHERE ibase = @v_ib_id.
    IF sy-subrc = 0.

      r_record_guid = me->order_item_api->add_reference_object(
               EXPORTING
                 i_reference_object = v_ib_guid
                 i_field_name       = 'IB_COMP_REF_GUID'
             ).



    ENDIF.

  ENDMETHOD.


  METHOD zif_ib_tur_order~line_responsible.
    me->order_api->add_partner(
      EXPORTING
        i_partner_fct = ''
        i_partner     = ''
        i_main        = 'X'
    ).
  ENDMETHOD.


  METHOD zif_ib_tur_order~location_details.

    DATA v_extension2    TYPE zbtx_aet_tur_ldt_bol_attr.
    DATA v_location_list TYPE TABLE OF zbtx_aet_tur_ldt_bol_attr.

    IF i_loc_details IS NOT INITIAL.


      DATA(v_order_items) = me->order_api->get_item_list(  ).

      LOOP AT v_order_items ASSIGNING FIELD-SYMBOL(<item>).

        me->map_service_fields( EXPORTING i_input         = CORRESPONDING zbtx_tur_order_loc_det_s(  i_loc_details )
                                CHANGING  extension2      = v_extension2 ).

        v_extension2-zz_request_id = me->order_api->get_orderadm_h_extension_struc( )-object_id.

        IF  <item>-item_properties-ordered_prod = zif_ib_tur_const=>c_portal_products-new_entitlements. " Entitlements have no location data
          CONTINUE.
        ENDIF.

        me->order_item_api = <item>-item_api.
        CHECK me->order_item_api IS BOUND.


        CASE i_option.
          WHEN zif_ib_tur_const=>c_odata_cud_operations-create. "Create
            me->order_item_api->add_extension_structure(
              EXPORTING
                i_extension_name  = |{ zif_ib_tur_const=>c_aet_extensions-location_dets }[@ZZLOC_ORDER={ v_extension2-zzloc_order }]|
                i_extension_value = v_extension2
            ).

          WHEN zif_ib_tur_const=>c_odata_cud_operations-update. "Update
            me->order_item_api->modify_extension_structure(
              EXPORTING
                i_extension_name  = |{ zif_ib_tur_const=>c_aet_extensions-location_dets }[@ZZLOC_ORDER={ v_extension2-zzloc_order }]|
                i_extension_value = v_extension2
            ).

          WHEN zif_ib_tur_const=>c_odata_cud_operations-delete. "Delete

            CHECK v_extension2-zzloc_order = 2. " only 2nd Location should be deletable

            me->order_item_api->remove_extension_structure(
             EXPORTING
               i_extension_name  = |{ zif_ib_tur_const=>c_aet_extensions-location_dets }[@ZZLOC_ORDER={ v_extension2-zzloc_order }]|
               i_extension_value = v_extension2 ).

            DATA(v_request_guid) = me->get_request_guid( ).
            DATA(lr2) = me->order_api->get_partner_list( i_partner_fct = zif_ib_tur_const=>c_partner_functions-line_resp_loc2 ).
            IF lr2 IS NOT INITIAL.
              DATA(v_partner) = lr2[ 1 ]-partner.


              CALL FUNCTION 'ZTUR_ORDER_PROCESSING' "*--- Delete Partner (Line Responsible) for second Location which was deleted (asynchron)
                STARTING NEW TASK 'DELETE_PARTNER'
                DESTINATION 'NONE'
                EXPORTING
                  iv_action           = zif_ib_tur_const=>c_order_processing_actions-delete_partner
                  iv_guid             = v_request_guid
                  iv_partner_function = zif_ib_tur_const=>c_partner_functions-line_resp_loc2
                  iv_partner          = v_partner
                  iv_synchronous_call = ''.
            ENDIF.
        ENDCASE.

      ENDLOOP.

    ELSE. "Location Details are initial

      DATA(v_order_id) = me->order_api->get_orderadm_h_extension_struc( )-object_id.

      SELECT * FROM zbtx_aet_tur_ldt
        INTO CORRESPONDING FIELDS OF TABLE @v_location_list
        WHERE zz_request_id = @v_order_id.
      IF sy-subrc <> 0.
        DATA(items) = me->order_api->get_item_list( ).

        LOOP AT items ASSIGNING <item>.
          <item>-item_api->get_extension_list(
            EXPORTING
              i_extension_name = zif_ib_tur_const=>c_aet_extensions-location_dets
            IMPORTING
              e_extension_list = v_location_list
          ).
          IF v_location_list IS NOT INITIAL.
            EXIT.
          ENDIF.
        ENDLOOP.

      ENDIF.
*      CHECK sy-subrc = 0.
      SORT v_location_list BY zz_location_id zz_room_id zzloc_order.
      DELETE ADJACENT DUPLICATES FROM v_location_list COMPARING zz_location_id zz_room_id zzloc_order.

      LOOP AT v_location_list ASSIGNING FIELD-SYMBOL(<loc>).

        <loc>-zz_location_id = |{ <loc>-zz_location_id ALPHA = OUT }| .
        DATA(v_loc) = CORRESPONDING zbtx_tur_order_loc_det_s( <loc>
                                                               MAPPING location_id          = zz_location_id
                                                                       room_id              = zz_room_id
                                                                       floor_id             = zzfloor_id
                                                                       rack                 = zz_rack_no
                                                                       rack_unit            = zzrack_unit
                                                                       media                = zzmedia
                                                                       interface            = zzinterface
                                                                       interface_text       = zzint_type_text
                                                                       cable_entry_address  = zzcable_entry
                                                                       out_of_business_hour = zzout_of_hours
                                                                       public_ip            = zzpublic_ip
                                                                       position_in_order    = zzloc_order
                                                                       is_old               = zz_is_old_loc
                                                              ).

        APPEND v_loc TO r_locations.
        CLEAR v_loc.

      ENDLOOP. "Loop at v_locations

      IF r_locations IS NOT INITIAL.
        LOOP AT r_locations ASSIGNING FIELD-SYMBOL(<l>).
          <l>-location_id = |{ <l>-location_id ALPHA = IN }| .
        ENDLOOP.

        SELECT account,
               location_id,
               description AS location_name,
               street,
               house_no,
               city,
               zip,
               country_code,
               country,npa_nxx
          FROM zib_locations_f4_cds
          INTO TABLE @DATA(loc_details)
          FOR ALL ENTRIES IN @r_locations
          WHERE location_id =  @r_locations-location_id.

        IF sy-subrc = 0.


          DATA(_locations) = VALUE zbtx_tur_order_loc_det_tt( FOR line IN r_locations (
                                                           location_id   = line-location_id
                                                           location_name = VALUE #( loc_details[ location_id = line-location_id ]-location_name OPTIONAL )
                                                           street        = VALUE #( loc_details[ location_id = line-location_id ]-street OPTIONAL )
                                                           house_no      = VALUE #( loc_details[ location_id = line-location_id ]-house_no OPTIONAL )
                                                           city          = VALUE #( loc_details[ location_id = line-location_id ]-city OPTIONAL )
                                                           zip           = VALUE #( loc_details[ location_id = line-location_id ]-zip OPTIONAL )
                                                           country_code  = VALUE #( loc_details[ location_id = line-location_id ]-country_code OPTIONAL )
                                                           country       = VALUE #( loc_details[ location_id = line-location_id ]-country OPTIONAL )
                                                           npa_nxx       = VALUE #( loc_details[ location_id = line-location_id ]-npa_nxx OPTIONAL )
                                                           room_id              = line-room_id
                                                           floor_id             = line-floor_id
                                                           rack                 = line-rack
                                                           rack_unit            = line-rack_unit
                                                           media                = line-media
                                                           interface            = line-interface
                                                           interface_text       = line-interface_text
                                                           cable_entry_address  = line-cable_entry_address
                                                           out_of_business_hour = line-out_of_business_hour

                                                           public_ip            = line-public_ip
                                                           position_in_order    = line-position_in_order
                                                           is_old               = line-is_old
                                         ) ) .

          r_locations = _locations.
        ENDIF.
      ENDIF. "r_locations is not initial.

    ENDIF. "If Location Details are not initial

  ENDMETHOD.


  METHOD zif_ib_tur_order~notes.
    DATA: v_note               TYPE crmst_text_btil,
          v_notes              TYPE zcrmst_text_btil_t,
          v_author_is_customer TYPE abap_bool,
          v_text_id            TYPE tdid.


    IF i_text-text_id IS NOT INITIAL. "update existing note

      v_text_id = i_text-text_id.
      me->order_api->modify_text( i_text_object = 'CRM_ORDERH'
                                  i_tdid = v_text_id
                                  i_text    = i_text-text
                                  i_langu = i_text-language
                                  i_formatted = abap_true
                                  i_append = abap_false
                                ).

    ELSEIF i_text IS NOT INITIAL. " create new Note

      v_text_id = detemine_text_id( i_author ).
      CHECK v_text_id IS NOT INITIAL.
      me->order_api->add_text(  i_text_object = 'CRM_ORDERH'
                                i_tdid        = v_text_id " external Text -> transfer to constant
                                i_text        = i_text-text
                                i_langu       = 'E'
                                i_formatted   = abap_true
                                i_append      = abap_false
                                        ).

      v_notes = VALUE #( ( tdid = v_text_id conc_formatted_lines = i_text-text tdspras = 'E' tdfdate = sy-datum tdftime = sy-uzeit ) ).

    ELSE.

      v_notes =  me->order_api->get_text_list( ).
    ENDIF.

    CHECK v_notes IS NOT INITIAL.
    DATA v_tur_order_note TYPE zbtx_tur_order_notes .
    LOOP AT v_notes ASSIGNING FIELD-SYMBOL(<notes>).
      DATA(v_formated_text) = <notes>-conc_formatted_lines.
      DATA(v_non_formated)  = <notes>-conc_lines.

      IF NOT v_formated_text CP   '*>html>*'.
        DATA(v_text) = v_formated_text.
      ELSEIF v_non_formated CP '<html>*'.
        v_text = v_non_formated.
      ENDIF.

      IF v_text IS NOT INITIAL.
        v_tur_order_note-text_id = <notes>-tdid.
        v_tur_order_note-language = <notes>-tdspras.
        v_tur_order_note-text = v_text.
        v_tur_order_note-created_at = convert_2_tstmp( i_date = <notes>-tdfdate i_time = <notes>-tdftime ).
        v_tur_order_note-request_guid = me->order_header-guid.

        APPEND v_tur_order_note TO r_notes.
      ENDIF.
    ENDLOOP.
*
*    r_notes = VALUE #( FOR line IN v_notes ( request_guid = me->order_header-guid
*                                             text_id      = line-tdid
*                                             language     = line-tdspras
*                                             text         = line-conc_formatted_lines
*                                             created_at   = convert_2_tstmp( i_date = line-tdfdate i_time = line-tdftime )
*                                              )  ).
*
  ENDMETHOD.


  METHOD zif_ib_tur_order~services.

    DATA: v_product_i_props TYPE  crmst_producti_btil,
          v_extension1      TYPE zastruc0002wc,
          v_extension2      TYPE zbtx_aet_tur_ldt_bol_attr.

    DATA(v_service) = CORRESPONDING zbtx_tur_order_serv_s( i_service ) .


    me->map_service_fields( EXPORTING i_input = v_service CHANGING product_i_props = v_product_i_props extension1 = v_extension1 ).

    v_product_i_props-process_qty_unit     = 'ST'.

    v_extension1-zzrequest_id = me->order_header-object_id.


    IF v_service-line_item_no IS INITIAL. " Add Service
      SELECT SINGLE product_guid
          FROM comm_product
          INTO @DATA(v_product_guid)
          WHERE product_id = @v_service-item.
      CHECK sy-subrc = 0.
      order_item_api = CAST #( me->order_api->add_item(  i_item_properties = VALUE #( product = v_product_guid ) ) ).
      CHECK order_item_api IS NOT INITIAL.

      order_item_api->add_product_i( i_product_item = v_product_i_props ).

*  ZBTX_AET_TUR_I

      order_item_api->add_extension_structure(
        EXPORTING
          i_extension_name  = zif_ib_tur_const=>c_aet_extensions-service_items
          i_extension_value = v_extension1
      ).

      IF v_service-related_product IS NOT INITIAL.
        order_item_api->add_reference_object(
            EXPORTING
              i_reference_object = v_service-related_product
              i_field_name       = 'PRODUCT_ID'
          ).
      ENDIF.
*for Added Service; required Customer Approval Steps:
      me->add_required_approval( i_approval_step = VALUE #( service_id    = v_service-service_id
                                                            partner       = i_company_account
                                                            partner_as    = zif_ib_tur_const=>c_partner_functions-company_partner
                                                            approval_step = zif_ib_tur_const=>c_order_approval_steps-customer_appr ) ).

* Check and add Coop Exchange + Provider as Partner Function and Approval Step if required:
      check_coop_exch_x_provider( EXPORTING i_service_id        = v_service-service_id
                                            i_provider_bus_id   = v_product_i_props-zzprovider_id
                                            i_portal_product    = v_service-item
                                            i_req_customer      = i_company_account

  ).

    ELSE. " update Service
*      order_item_api = CAST #( me->order_api->add_item(  i_item_properties = VALUE #( product = v_product_guid ) ) ).

      order_item_api = CAST #( me->order_api->get_item_by_item_no( i_key = v_service-line_item_no ) ).
      CHECK order_item_api IS NOT INITIAL.
      order_item_api->modify_product_i( i_product_item = v_product_i_props ).
      check_update_appr_step( i_service_id = v_service-service_id i_item_no = v_service-line_item_no i_provider = v_product_i_props-zzprovider_id ).

      IF v_service-item_status IS NOT INITIAL.

        order_item_api->status_i(
          EXPORTING
            i_new_status     = v_service-item_status
*            i_current_status =
*          receiving
*            r_status         =
        ).
      ENDIF.
*  ZBTX_AET_TUR_I

      order_item_api->modify_extension_structure(
        EXPORTING
          i_extension_name  = zif_ib_tur_const=>c_aet_extensions-service_items
          i_extension_value = v_extension1
      ).

      IF v_service-related_product IS NOT INITIAL.
        order_item_api->add_reference_object(
          EXPORTING
            i_reference_object = v_service-related_product
            i_field_name       = 'PRODUCT_ID'
        ).
      ENDIF.
    ENDIF.

    r_order_service = order_item_api.

  ENDMETHOD.


  METHOD zif_ib_tur_order~set_status.
    me->order_api->status( i_new_status = i_new_status ).
  ENDMETHOD.
ENDCLASS.
