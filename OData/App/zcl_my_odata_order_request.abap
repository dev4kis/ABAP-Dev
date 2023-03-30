class ZCL_TUR_ODATA_ORDER_REQUEST definition
  public
  inheriting from ZCL_TUR_BASE_ODATA_ENTITY
  final
  create private

  global friends ZCL_ODATA_ENTITY_FACTORY .

public section.

  methods CONSTRUCTOR .
protected section.

  methods COUNT
    redefinition .
  methods CREATE
    redefinition .
  methods QUERY
    redefinition .
  methods READ
    redefinition .
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_next_action_by_s,
             request_guid   TYPE crmt_object_guid,
             request_status TYPE crm_j_status,
             next_action_by TYPE char255,
           END OF ty_next_action_by_s,
           ty_next_action_tt TYPE STANDARD TABLE OF ty_next_action_by_s WITH DEFAULT KEY,
           ty_requests_tt    TYPE STANDARD TABLE OF zib_pt_orders_v WITH DEFAULT KEY.
    METHODS: get_request
      IMPORTING
        i_filter_tab   TYPE /iwbep/t_mgw_select_option
        i_sort_tab     TYPE if_salv_ida_types_int=>yt_db_sort_rule OPTIONAL
      CHANGING
        c_query_result TYPE ANY TABLE,
      request_4rm_other_partners IMPORTING i_filter_tab TYPE /iwbep/t_mgw_select_option
                                 CHANGING  c_others     TYPE  ty_requests_tt
                                 ,
      determine_next_action_by IMPORTING i_status         TYPE crm_j_status OPTIONAL
                               CHANGING  c_next_action_by TYPE ty_next_action_tt,
      init_query_engine,
      get_desc_4rm_req_type
        IMPORTING
          i_request_type  TYPE zbtx_tur_request_s-request_type
        RETURNING
          VALUE(r_result) TYPE crmt_process_description,
      get_db_props
        CHANGING
          c_order_h TYPE zib_tur_odata_request_s.

    DATA: v_query_engine TYPE REF TO zcl_ida_query_engine.

ENDCLASS.



CLASS ZCL_TUR_ODATA_ORDER_REQUEST IMPLEMENTATION.


  METHOD constructor.


    super->constructor( ).


  ENDMETHOD.


  METHOD count.


  ENDMETHOD.


  METHOD create.

    DATA: v_order_h     TYPE zbtx_tur_request_s,
          v_request     TYPE REF TO zcl_ib_tur_order,
          v_new_order_h TYPE zib_tur_odata_request_s.

    v_order_h = CORRESPONDING #( i_new_entiy ).
    DATA(v_tur_api) = zcl_ibase_tur_api=>get_instance( ).
    v_order_h-description = get_desc_4rm_req_type( v_order_h-request_type ) .

    v_order_h-company_partner = me->company_account.
    v_order_h-author = me->session_user.

    v_request ?= v_tur_api->create_request( CHANGING c_order = v_order_h
                                             ).
    DATA(v_guid)      = v_request->get_request_guid( ).
    DATA(v_req_id)    = v_request->get_request_id( ).


    v_new_order_h = CORRESPONDING #( v_order_h ).
    v_new_order_h-request_guid = v_guid.
    v_new_order_h-request_id   = v_req_id.

    get_db_props( CHANGING c_order_h = v_new_order_h ).
    e_entity = v_new_order_h.

*    me->read( EXPORTING i_key = VALUE #( ( name = 'REQUEST_GUID' value = v_guid )
*                                       )
*              IMPORTING e_entity = e_entity ).

  ENDMETHOD.


  METHOD determine_next_action_by.
*************Notes:  SPCRC-2375
* - In Status Draft, only requester sees request and only next action by contains only requester
* - In Status Wait Approval, Request sees everything contained in field next action by
* - Provider if any sees only their names if they haven't approved yet, otherwise missing coop exchange approvals if any, otherwise empty
* - Coop Exchange sees only their names in approval from them is still missing, otherwise empty
    CHECK c_next_action_by IS NOT INITIAL.
*    tur_api->determine_next_action_by(
*      EXPORTING
*        i_company_partner = me->company_account
*      CHANGING
*        c_nex_action_by   = c_next_action_by
*    ).
    SELECT request_guid,
               company_partner,
               name_org1,
               function,
               approval_step,step_status
        FROM zib_tur_order_appr_steps AS _as
        JOIN but000 AS b
            ON _as~company_partner = b~partner
            FOR ALL ENTRIES IN @c_next_action_by
             WHERE request_guid = @c_next_action_by-request_guid
*               AND step_status = ''
        INTO TABLE @DATA(v_open_approvals) .
    CHECK sy-subrc = 0.
    SORT v_open_approvals BY request_guid company_partner approval_step.
    DELETE ADJACENT DUPLICATES FROM v_open_approvals COMPARING request_guid company_partner approval_step.


    DATA v_next_action TYPE zib_tur_odata_request_s-next_action_by.
    DATA v_partner_role TYPE crmt_partner_fct.

    LOOP AT c_next_action_by ASSIGNING FIELD-SYMBOL(<v_>).
      TRY.
          v_partner_role = v_open_approvals[ company_partner = me->company_account request_guid = <v_>-request_guid ]-function.

*      v_partner_role = me->tur_api->get_partner_fct(
*                                    i_request_guid = <v_>-request_guid
*                                    i_partner      = me->company_account
*                                  ).
          IF <v_>-request_status = zif_ib_tur_const=>co_tur_order_status-draft.

            CHECK v_partner_role = zif_ib_tur_const=>c_partner_functions-company_partner.
            <v_>-next_action_by = v_open_approvals[ company_partner = me->company_account function = zif_ib_tur_const=>c_partner_functions-company_partner  ]-name_org1.

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
                <v_>-next_action_by = VALUE #( v_open_approvals[ company_partner = me->company_account
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
                <v_>-next_action_by = VALUE #( v_open_approvals[ company_partner = me->company_account
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


  METHOD get_db_props.
    SELECT SINGLE txt30 INTO @DATA(v_status_text) FROM tj30t WHERE stsma = 'ZPT1' AND spras = @sy-langu AND estat = @c_order_h-status.
    IF sy-subrc = 0.
      c_order_h-status_text = v_status_text.
    ENDIF.

    SELECT SINGLE concat( name_first, name_last ) FROM but000 WHERE partner = @c_order_h-author INTO @DATA(v_author_name) .
    IF sy-subrc = 0.
      c_order_h-author_name = v_author_name.
    ENDIF.
  ENDMETHOD.


  METHOD get_desc_4rm_req_type.
    SELECT SINGLE description
    FROM zpt_req_type_c
    INTO @r_result
    WHERE request_type = @i_request_type.
  ENDMETHOD.


  METHOD get_request.

    DATA v_tur_orders TYPE STANDARD TABLE OF zib_pt_orders_v.

    init_query_engine( ).

    v_query_engine->zif_exas_query~add_filter_list( i_filter_tab = i_filter_tab ).

    v_query_engine->zif_exas_query~set_sort_fields( i_sort_element_tab = i_sort_tab ).
    v_query_engine->zif_exas_query~exec(  CHANGING c_query_result = v_tur_orders  ).

*    SORT v_tur_orders BY created_on.
    c_query_result = CORRESPONDING #( v_tur_orders ).

  ENDMETHOD.


  METHOD init_query_engine.
    CLEAR v_query_engine.
    me->v_query_engine = NEW #(
                i_cds_view_parameter_tab = VALUE #( )
                i_query_entity           = 'ZIB_TUR_ORDERS_CDS'
*    i_query_entity_type      =
            ).
  ENDMETHOD.


  METHOD query.

    DATA: v_orders TYPE ty_requests_tt,
          v_       TYPE zcl_zcrm_tur_mpc_ext=>tt_orderrequest. "ZIB_TUR_ODATA_REQUEST_S
    DATA(v_filter_tab) = i_select_option_tab.

    DATA(v_nav_prop) = VALUE #(  i_nav_path[ 1 ]-nav_prop OPTIONAL ) .
    IF v_nav_prop IS NOT INITIAL.
      CASE v_nav_prop.

        WHEN 'toSplittedRequests'.
          TRY.
              DATA(v_origin_guid) = v_filter_tab[ property = 'REQUEST_GUID' ]-select_options[ 1 ]-low.
            CATCH cx_sy_itab_line_not_found INTO DATA(v_missing_key_exc).
              zcx_crm_app=>raise_missing_input(   i_input_name = 'Principal Request Key' ).
          ENDTRY.
          DATA(v_requests_splits) = me->tur_api->get_requests_splits( i_object_guid = CONV #( v_origin_guid )  ).
          CHECK v_requests_splits IS NOT INITIAL.
          DELETE v_filter_tab WHERE property = 'REQUEST_GUID'.
          DATA(v_guids_filter) = VALUE  /iwbep/s_mgw_select_option( property       = 'REQUEST_GUID'
                                                                    select_options = VALUE #( FOR l IN v_requests_splits ( sign = 'I' option = 'EQ' low = l ) ) ).
          APPEND v_guids_filter TO v_filter_tab.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    DATA(v_company_partner) = VALUE  /iwbep/s_mgw_select_option( property = 'COMPANY_PARTNER' select_options = VALUE #( ( sign = 'I' option = 'EQ' low = me->company_account ) ) ).
    APPEND v_company_partner TO v_filter_tab.

    DATA(v_sort_by) = i_sort_element_tab.


    get_request( EXPORTING i_sort_tab    = v_sort_by
                           i_filter_tab  = v_filter_tab

                 CHANGING c_query_result = v_orders ).


    IF v_nav_prop IS INITIAL.
      me->request_4rm_other_partners(  EXPORTING i_filter_tab = v_filter_tab CHANGING c_others = v_orders ).
    ENDIF.
    CHECK v_orders IS NOT INITIAL.

*    IF v_created_on_filter IS NOT INITIAL.
*      DATA(v_lb) = v_created_on_filter[ 1 ]-low.
*      DATA(v_hb) = v_created_on_filter[ 1 ]-high.
*      DELETE v_orders WHERE created_on NOT BETWEEN v_lb AND v_hb.
*    ENDIF.
    DATA(v_next_action_by_tab) = VALUE ty_next_action_tt( FOR l1 IN v_orders WHERE ( status = zif_ib_tur_const=>co_tur_order_status-draft
                                                                                    OR status = zif_ib_tur_const=>co_tur_order_status-waiting_approval  )
                                                         ( request_guid = l1-request_guid request_status = l1-status ) ).
    v_  = CORRESPONDING #( v_orders ).

    """"""""Need to determin next Action by:

    me->determine_next_action_by(  CHANGING c_next_action_by = v_next_action_by_tab ).

*    IF v_next_action_by_tab IS NOT INITIAL.
    LOOP AT v_ ASSIGNING FIELD-SYMBOL(<v_>).
      CASE <v_>-status.
        WHEN zif_ib_tur_const=>co_tur_order_status-wait_legal_appr.
          <v_>-next_action_by = 'Deutsche Börse AG'.
        WHEN OTHERS.

          <v_>-next_action_by =  VALUE #( v_next_action_by_tab[ request_guid = <v_>-request_guid ]-next_action_by OPTIONAL ).
      ENDCASE.
    ENDLOOP.
*    ENDIF.
    DATA(v_no_sort_fields) = lines( v_sort_by ).
    SORT v_ BY request_id DESCENDING  created_on DESCENDING.
    c_query_result = CORRESPONDING #( v_ ).
  ENDMETHOD.


  METHOD read.
    DATA v_ TYPE zcl_zcrm_tur_mpc_ext=>ts_orderrequest.
    DATA v_order TYPE zib_pt_orders_v.
    DATA v_orders TYPE STANDARD TABLE OF zib_pt_orders_v.
    DATA v_request TYPE REF TO zcl_ib_tur_order.
    TRY.
        DATA(v_guid) =  i_key[ name = 'REQUEST_GUID' ]-value.
      CATCH cx_sy_itab_line_not_found.
        zcx_crm_app=>raise_missing_input( i_input_name =  'Order Key' ).
    ENDTRY.
*---- First try to get Data from Shadow,if Request is old:
    me->get_request(
      EXPORTING

        i_filter_tab   = VALUE #( ( property = 'REQUEST_GUID' select_options = VALUE #( ( sign = 'I' option = 'EQ' low = v_guid ) ) ) )
      CHANGING
        c_query_result = v_orders
    ).

    IF v_orders IS NOT INITIAL.
      v_order = v_orders[ 1 ].
    ELSE. " If Request is just being created, get from Interface
      DATA(v_tur_api) = zcl_ibase_tur_api=>get_instance( ).

      v_request ?= v_tur_api->get_request( v_guid ).

      v_order = VALUE #(        description = v_request->description( )
                                request_guid = v_request->get_request_guid( )
                                status       = v_request->get_status( )-status
                                request_id   = v_request->get_request_id( )
                                ).
    ENDIF.
    v_ = CORRESPONDING #( v_order ).
    DATA(v_next_action_by_tab) = VALUE ty_next_action_tt( ( request_guid = v_-request_guid request_status = v_-status ) ).
    me->determine_next_action_by(
      CHANGING
        c_next_action_by = v_next_action_by_tab
    ).
    v_-next_action_by = v_next_action_by_tab[ request_guid = v_-request_guid ]-next_action_by.
    e_entity = CORRESPONDING #( v_ ).

  ENDMETHOD.


  METHOD request_4rm_other_partners.

*************************************
*** Here we fetch requests from other Partners in which current Company is involved
*** for instance als Provider/3rd Party
****************************************
    DATA(v_filter_tab) = i_filter_tab.
    DELETE v_filter_tab WHERE property = 'COMPANY_PARTNER'.
*---> Get all other requests in which Org is invloved: but not as Requester(Org)
    SELECT DISTINCT object_id AS request_guid FROM zbtx_aet_tur_as AS o INTO TABLE @DATA(t_other_involved_reqs)
      WHERE zzpartner = @me->company_account AND zzpartner_as <> @zif_ib_tur_const=>c_partner_functions-company_partner .
    CHECK sy-subrc = 0.

    SELECT p~object_id AS request_guid,
           p~zzpartner AS partner_org,
           p~zzpartner_as AS function,
           p~zzstatus AS approval_status,
           b~status
       FROM zbtx_aet_tur_as AS o
      JOIN zbtx_aet_tur_as AS p
    ON o~object_id = p~object_id
    JOIN zbtx_tur_order_b AS b ON p~object_id = b~order_guid
    INTO TABLE @DATA(v_others_requests)
      FOR ALL ENTRIES IN @t_other_involved_reqs WHERE p~object_id = @t_other_involved_reqs-request_guid
   AND

*      o~zzapproval_step = @zif_ib_tur_const=>c_order_approval_steps-customer_appr
*     AND o~zzstatus = @zif_ib_tur_const=>c_approval_step_status-approved
*   AND
*    p~zzpartner = @me->company_account AND p~zzpartner_as NE @zif_ib_tur_const=>c_partner_functions-company_partner
*
*    AND
     b~status IN ( @zif_ib_tur_const=>co_tur_order_status-waiting_approval,
                      @zif_ib_tur_const=>co_tur_order_status-rejected,
                      @zif_ib_tur_const=>co_tur_order_status-approved,
                      @zif_ib_tur_const=>co_tur_order_status-in_progress,
                      @zif_ib_tur_const=>co_tur_order_status-completed ).

    CHECK sy-subrc = 0.
    LOOP AT v_others_requests ASSIGNING FIELD-SYMBOL(<other>) WHERE function = zif_ib_tur_const=>c_partner_functions-partner_exch
                                                                AND partner_org = me->company_account.
      " In Provider is also include in this request and hasn's approved, then Coop Exchange should not see the request yes and should be able to approve
      IF line_exists( v_others_requests[ request_guid = <other>-request_guid function = zif_ib_tur_const=>c_partner_functions-provider  approval_status = '' ] ) .
*        DELETE v_others_requests WHERE request_guid = <other>-request_guid.
      ENDIF.

    ENDLOOP.

    DATA(v_mine) = c_others.

    SORT v_others_requests BY request_guid.
    DELETE ADJACENT DUPLICATES FROM v_others_requests COMPARING request_guid function.
    DATA(_filter_tab) = VALUE  /iwbep/s_mgw_select_option( property = 'REQUEST_GUID'
                                                           select_options = VALUE #( FOR line IN v_others_requests ( sign = 'I' option = 'EQ' low = line-request_guid ) ) ).


    APPEND _filter_tab TO v_filter_tab.
*    APPEND LINES OF i_filter_tab TO _filter_tab.
*    DELETE _filter_tab WHERE property = 'COMPANY_PARTNER'.
    init_query_engine( ).

    me->v_query_engine->zif_exas_query~add_filter_list( i_filter_tab = v_filter_tab  ).

    me->v_query_engine->zif_exas_query~exec(
*      EXPORTING
*        i_max_rows     =     " Maximum number of rows
      CHANGING
        c_query_result = c_others
    ).

    APPEND LINES OF v_mine TO c_others.
    SORT c_others.
    DELETE ADJACENT DUPLICATES FROM c_others.

  ENDMETHOD.
ENDCLASS.
