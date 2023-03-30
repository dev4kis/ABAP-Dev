CLASS zcl_my_base_odata_entity DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_sess_installations,
             product_guid       TYPE comt_product_guid,
             installation_id1   TYPE ib_ibase,
             provider           TYPE zib_business_id,
             installation_desc1 TYPE char120,
             bandwidth1         TYPE zib_bandwidth,
             installation_id2   TYPE ib_ibase,
             installation_desc2 TYPE char120,
             bandwidth2         TYPE zib_bandwidth,
             installation_id3   TYPE ib_ibase,
             bandwidth3         TYPE zib_bandwidth,
             installation_desc3 TYPE char120,
             installation_id4   TYPE ib_ibase,
             installation_desc4 TYPE char120,
             bandwidth4         TYPE zib_bandwidth,
           END OF ty_sess_installations,
           ty_sess_installations_tt TYPE STANDARD TABLE OF ty_sess_installations WITH DEFAULT KEY,

           BEGIN OF ty_bu_rel_s,
             relnr       TYPE bu_relnr,
             rel_partner TYPE bu_partner,
           END OF ty_bu_rel_s,
           ty_bu_rel_tt TYPE STANDARD TABLE OF ty_bu_rel_s WITH DEFAULT KEY,

           BEGIN OF ty_bupa_bus_id,
             account      TYPE bu_partner,
             account_name TYPE bu_descrip,
             business_id  TYPE zib_business_id,
           END OF ty_bupa_bus_id,
           ty_bupa_bus_id_tt TYPE STANDARD TABLE OF ty_bupa_bus_id WITH DEFAULT KEY..


    INTERFACES zif_odata_entity .

    DATA: company_account    TYPE bu_partner,
          session_user       TYPE bu_partner,
          user_contract_guid TYPE crmt_object_guid,
          tur_api            TYPE REF TO zcl_ibase_tur_api.


    CONSTANTS co_base_entity_classname TYPE seoclsname VALUE 'ZCL_TUR_BASE_ODATA_ENTITY' ##NO_TEXT.
protected section.

  data ODATA_APP type ref to ZIF_ODATA_APPLICATION .
  data OPEN_REQUESTS type ZCL_IBASE_TUR_API=>TY_OPEN_REQUESTS_TT .

  methods READ
    importing
      !I_KEY type /IWBEP/T_MGW_TECH_PAIRS
      !I_NAVIGATION_TAB type /IWBEP/T_MGW_NAVIGATION_PATH optional
      !I_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !I_SELECT_COLUMN_TAB type STRING_TABLE optional
      !I_SOURCE_KEY type /IWBEP/T_MGW_TECH_PAIRS optional
    exporting
      !E_ENTITY type ANY
    raising
      /IWBEP/CX_MGW_NOT_IMPL_EXC .
  methods UPDATE
    importing
      !I_KEY type /IWBEP/T_MGW_TECH_PAIRS
      !I_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !I_SOURCE_KEY type /IWBEP/T_MGW_TECH_PAIRS optional
      !I_UPDATE_ENTITY type DATA
    exporting
      !E_ENTITY type ANY
    raising
      /IWBEP/CX_MGW_NOT_IMPL_EXC .
  methods CREATE
    importing
      !I_NEW_ENTIY type DATA
    exporting
      !E_ENTITY type DATA
    raising
      /IWBEP/CX_MGW_NOT_IMPL_EXC .
  methods QUERY
    importing
      !I_SELECT_OPTION_TAB type /IWBEP/T_MGW_SELECT_OPTION optional
      !I_SELECT_COLUMN_TAB type STRING_TABLE optional
      !I_NAV_PATH type /IWBEP/T_MGW_NAVIGATION_PATH optional
      !I_OSQL_WHERE type CSEQUENCE optional
      !I_TOP type I optional
      !I_SKIP type I optional
      !I_SORT_ELEMENT_TAB type IF_SALV_IDA_TYPES_INT=>YT_DB_SORT_RULE optional
      !I_ENTITY_NAME type /IWBEP/MGW_TECH_NAME optional
    changing
      !C_QUERY_RESULT type ANY TABLE
    raising
      /IWBEP/CX_MGW_NOT_IMPL_EXC .
  methods DELETE
    importing
      !I_KEY type /IWBEP/T_MGW_TECH_PAIRS
      !I_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
    raising
      /IWBEP/CX_MGW_NOT_IMPL_EXC .
  methods INIT .
  methods COUNT
    importing
      !I_SELECT_OPTION_TAB type /IWBEP/T_MGW_SELECT_OPTION optional
      !I_OSQL_WHERE type CSEQUENCE optional
    returning
      value(R_RESULT) type INT4
    raising
      /IWBEP/CX_MGW_NOT_IMPL_EXC .
  methods GET_STREAM
    importing
      !I_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(R_RESULT) type ZODATA_MEDIA_RESOURCE
    raising
      /IWBEP/CX_MGW_NOT_IMPL_EXC .
  methods HANDLE_ACTION
    importing
      !I_ACTION type STRING
      !I_PARAMETERS type /IWFND/T_MGW_NAME_VALUE_PAIR
    exporting
      !ER_ENTITY type DATA
    raising
      /IWBEP/CX_MGW_NOT_IMPL_EXC .
  methods VALIDATE
    importing
      !I_METHOD type STRING
      !I_KEY type /IWBEP/T_MGW_TECH_PAIRS optional
      !I_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !I_SELECT_OPTION_TAB type /IWBEP/T_MGW_SELECT_OPTION optional
      !I_PARAMETERS type /IWFND/T_MGW_NAME_VALUE_PAIR optional
      !I_ENTITY type DATA optional
      !I_ENTITY_NAME type /IWBEP/MGW_TECH_NAME optional
      !I_ACTION type STRING optional
      !I_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH optional
      !I_CONTRACT_GUID type CRMT_OBJECT_GUID optional
      !I_TUTORIAL_MODE type CRMT_BOOLEAN optional .
  methods CREATE_STREAM
    importing
      !I_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !I_CONTENT type ZTUR_ORDER_ATTACHMENT
    exporting
      !E_FILE_INFO type ANY .
  methods GET_OPEN_REQUESTS
    importing
      !I_PROD_ID_TT type COMT_PRODUCT_ID_TAB optional
      !I_RELNR_TAB type TY_BU_REL_TT optional .
  methods IS_VALID_IB
    importing
      !I_IB_ID type IB_IBASE
      !I_IB_TYPE type IB_IBTYP
    returning
      value(R_IS_VALID) type ABAP_BOOL .
  methods FORMAT_SESSION_INSTALLATIONS
    importing
      !I_PRODUCT_GUID_TAB type COMT_PRODUCT_GUID_TAB
    returning
      value(R_SESS_INSTALLATIONS) type TY_SESS_INSTALLATIONS_TT .
  methods GET_BP_NAME
    changing
      !C_PARTNER_NAMES type ZBTX_PARTNER_NAMES_TT .
  methods VALIDATE_BUSINESS_IDS
    importing
      !I_RESULTS type TY_BUPA_BUS_ID_TT
    changing
      !C_RESULT type TY_BUPA_BUS_ID_TT .
  methods VALIDATE_PERMISSION
    importing
      !I_ACTION type STRING
    returning
      value(R_HAS_PERMISSION) type ABAP_BOOL .
  methods GET_INVOLVED_CUSTOMER
    importing
      !I_PRODUCT_ID type COMT_PRODUCT_ID optional
      !I_SERVICE_ID type Z_SERVICE_ID_NEW optional
      !I_REQUEST_GUID type CRMT_OBJECT_GUID optional
    returning
      value(R_INVOLVED_CUSTOMER_TAB) type CRMT_BU_PARTNER_T .
  methods GET_COOP_EXCHANGE_PARTNERS
    importing
      !I_SERVICE_ID type Z_SERVICE_ID_NEW optional
    returning
      value(R_INVOLVED_CUSTOMER_TAB) type CRMT_BU_PARTNER_T .
  methods GET_INSTALLATION_OWNER
    importing
      !I_INSTALLATION_ID type IB_IBASE
    returning
      value(R_INSTALLATION_OWNER) type CRMT_BU_PARTNER_T .
  methods IS_READ_REQUEST_ALLOWED
    importing
      !IV_GUID type CRMT_OBJECT_GUID
    returning
      value(READ_ALLOWED) type ABAP_BOOL .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MY_BASE_ODATA_ENTITY IMPLEMENTATION.


  METHOD count.

*-- should be redefined by subclass IF needed
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc.

  ENDMETHOD.


  METHOD create.
    e_entity = ''.
*-- should be redefined by subclass IF need
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc.

  ENDMETHOD.


  METHOD create_stream.

*-- should be redefined by subclass IF need
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc.

  ENDMETHOD.


  METHOD delete.

*-- should be redefined by subclass IF need
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc.

  ENDMETHOD.


  METHOD format_session_installations.
    DATA v_sess_installations TYPE ty_sess_installations.

    DATA(v_guid_tab) = i_product_guid_tab.

    SELECT DISTINCT lsm_product_id,
                    session_product_id AS product_id,
                    session_product_guid AS product_guid,
                    installation,
                    installation_desc,
                    lsm_bandwidth AS bandwidth,
                    session_service_id,
                    location_id,
                    location_name,
                    installation_owner,
                    is_provided,
                    customer            AS participant
           FROM zib_conn_sessions "zv_ib_conn_sess
            INTO TABLE @DATA(v_installations)
              FOR ALL ENTRIES IN @v_guid_tab
           WHERE session_product_guid = @v_guid_tab-table_line.

    SORT v_installations BY product_id location_id installation.
    DELETE ADJACENT DUPLICATES FROM v_installations COMPARING product_id
                                                              location_id
                                                              installation. " On Same Installation

********* Exceptional Case: Some Installations might have been poorly cleaned up
********So that some Lines are cancelled without cancelling corresponding Sessions. These Sessions, will not show in Query above
* ******** and need to determined separately since we still need their locations
    LOOP AT v_guid_tab ASSIGNING FIELD-SYMBOL(<guid>).
      IF line_exists( v_installations[ product_guid = <guid> ] ).
        DELETE v_guid_tab.
      ENDIF.
    ENDLOOP.

    IF v_guid_tab IS NOT INITIAL.

      SELECT si~product_guid        AS product_guid,
             si~product_id          AS product_id,
             si~service_id          AS session_service_id,
             si~installation_id     AS installation,
             si~installation        AS installation_desc,
             si~installation_owner  AS installation_owner,
             si~is_provided         AS is_provided
       INTO TABLE @DATA(v_other_instas)
        FROM zib_tur_prov_sess_cds AS si
      FOR ALL ENTRIES IN @v_guid_tab
        WHERE si~product_guid = @v_guid_tab-table_line.

      IF v_other_instas IS NOT INITIAL.
        MOVE-CORRESPONDING v_other_instas TO v_installations KEEPING TARGET LINES.
      ENDIF.
    ENDIF.

    DATA(v_provider_id_tab) = VALUE crmt_bu_partner_t( FOR l IN v_installations WHERE ( is_provided = abap_true ) ( l-installation_owner ) ).

    IF v_provider_id_tab IS NOT INITIAL.
      SORT v_provider_id_tab.
      DELETE ADJACENT DUPLICATES FROM v_provider_id_tab.
*      DATA(v_provider_bus_ids) = NEW zcl_ib_tur_sessions(  )->get_business_ids_by_bp(  i_bp_id_tab = v_provider_id_tab
*                                                                                       i_proc_type = 'ZC10'
*                                                                                       i_service_id = v_installations[ 1 ]-session_service_id ).

      IF v_provider_id_tab IS NOT INITIAL.

        zcl_service_toolbox=>get_provider_id( EXPORTING it_partner      = v_provider_id_tab
                                                        iv_service_id   = v_installations[ 1 ]-session_service_id
                                              IMPORTING et_provider_ids = DATA(v_provider_bus_ids) ).

      ENDIF.

    ENDIF.
    SORT v_installations BY installation location_name DESCENDING.


*{ BEGIN CHANGE EXTREEH; 10.12.21; CCDV-1283; All coops should see all installations

    SELECT SINGLE mandt FROM zpt_pex_c
      INTO @DATA(ls_pex)
      WHERE partner_exchange = @me->company_account
        AND portal_full_visibility = 'X'.

    IF sy-subrc = 0.
      DATA(lv_no_masking) = 'X'.
    ENDIF.
*} END EXTREEH



    DATA(v_tabix) = 0.
    LOOP AT i_product_guid_tab ASSIGNING FIELD-SYMBOL(<id>).

      IF line_exists( v_installations[ product_guid = <id> ] ).
        LOOP AT v_installations ASSIGNING FIELD-SYMBOL(<inst>) WHERE product_guid = <id>.
          <inst>-installation = |{ <inst>-installation ALPHA = IN }|.

          v_sess_installations-product_guid = <id>.
*          v_sess_installations-
          v_tabix = v_tabix + 1.
          CASE v_tabix.

            WHEN 1.

              v_sess_installations-installation_id1 = <inst>-installation.
********************************************************************************************************************
*--- "SPCRC-5940 // SPCRC-1614: Installation Description should be "Assigned" if Installation does not belong to me.
              IF <inst>-participant IS NOT INITIAL.
                IF <inst>-participant NE me->company_account.

                  IF <inst>-installation_owner NE me->company_account AND lv_no_masking NE 'X'.
                    v_sess_installations-installation_desc1 = 'Assigned'.
                    CONTINUE.
                  ENDIF.

                ENDIF.
              ENDIF.
********************************************************************************************************************

              IF <inst>-is_provided = abap_true.

                DATA(v_prov_bus_id) = VALUE #( v_provider_bus_ids[ account = <inst>-installation_owner ]-business_id OPTIONAL ).
                IF v_prov_bus_id IS NOT INITIAL.
                  v_sess_installations-provider = v_prov_bus_id.
                  CONCATENATE v_prov_bus_id '-' INTO v_sess_installations-installation_desc1 SEPARATED BY space.
                ENDIF.

              ENDIF.

              CONCATENATE v_sess_installations-installation_desc1 <inst>-installation_desc
                INTO v_sess_installations-installation_desc1 SEPARATED BY space.
              IF <inst>-location_name IS NOT INITIAL.
                CONCATENATE v_sess_installations-installation_desc1 '|' <inst>-location_name
                INTO v_sess_installations-installation_desc1 SEPARATED BY space.
              ENDIF.
              v_sess_installations-installation_id1 = <inst>-installation.
              v_sess_installations-bandwidth1 = <inst>-bandwidth.

            WHEN 2.
              v_sess_installations-installation_id2 = <inst>-installation.
********************************************************************************************************************
*--- "SPCRC-5940 // SPCRC-1614: Installation Description should be "Assigned" if Installation does not belong to me.
              IF <inst>-participant IS NOT INITIAL.
                IF <inst>-participant NE me->company_account.
                  IF lv_no_masking NE 'X'.

                    IF <inst>-installation_owner NE me->company_account.
****      SPCRC-6776 - in case the installation is masked we need to take the splitlocation into account as well
                      IF <inst>-installation EQ v_sess_installations-installation_id1. "Splitlocation
*         if we have a split location, we just skip the entry and reset the counter
                        v_tabix = v_tabix - 1.

                      ELSE.
                        v_sess_installations-installation_desc2 = 'Assigned'.
                      ENDIF.
                      CONTINUE.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
********************************************************************************************************************

              IF <inst>-installation EQ v_sess_installations-installation_id1. "Splitlocation
                IF <inst>-location_name IS NOT INITIAL.
                  CONCATENATE v_sess_installations-installation_desc1 '/'
                  <inst>-location_name
                  INTO v_sess_installations-installation_desc1 SEPARATED BY space.
                ENDIF.
                v_sess_installations-bandwidth1 = <inst>-bandwidth.
                v_tabix = v_tabix - 1.
                CLEAR  v_sess_installations-installation_id2.
              ELSE.
                IF <inst>-is_provided = abap_true.
                  v_prov_bus_id = VALUE #( v_provider_bus_ids[ account = <inst>-installation_owner ]-business_id OPTIONAL ).
                  IF v_prov_bus_id IS NOT INITIAL.
                    CONCATENATE v_prov_bus_id '-' INTO v_sess_installations-installation_desc2 SEPARATED BY space.
                  ENDIF.

                ENDIF.
                CONCATENATE v_sess_installations-installation_desc2 <inst>-installation_desc
               INTO v_sess_installations-installation_desc2 SEPARATED BY space.
                IF <inst>-location_name IS NOT INITIAL.
                  CONCATENATE v_sess_installations-installation_desc2 '|' <inst>-location_name
                  INTO v_sess_installations-installation_desc2 SEPARATED BY space.
                ENDIF.
                v_sess_installations-bandwidth2 = <inst>-bandwidth.
                v_sess_installations-installation_id2 = <inst>-installation.
              ENDIF.

            WHEN 3.
              v_sess_installations-installation_id3 = <inst>-installation.
********************************************************************************************************************
*--- "SPCRC-5940 // SPCRC-1614: Installation Description should be "Assigned" if Installation does not belong to me.
              IF <inst>-participant IS NOT INITIAL.
                IF <inst>-participant NE me->company_account.

                  IF lv_no_masking NE 'X'.

                    IF <inst>-installation_owner NE me->company_account.
****      SPCRC-6776 - in case the installation is masked we need to take the splitlocation into account as well
                      IF <inst>-installation EQ v_sess_installations-installation_id2. "Splitlocation
*         if we have a split location, we just skip the entry and reset the counter
                        v_tabix = v_tabix - 1.
                      ELSE.
                        v_sess_installations-installation_desc3 = 'Assigned'.
                      ENDIF.
                      CONTINUE.
                    ENDIF.
                  ENDIF.

                ENDIF.
              ENDIF.
********************************************************************************************************************

              IF <inst>-installation EQ v_sess_installations-installation_id2. "Splitlocation
                IF <inst>-location_name IS NOT INITIAL.
                  CONCATENATE v_sess_installations-installation_desc2 '/'
                  <inst>-location_name
                  INTO v_sess_installations-installation_desc2 SEPARATED BY space.
                ENDIF.
                v_tabix = v_tabix - 1.
                CLEAR v_sess_installations-installation_id3.
              ELSE.
                IF <inst>-is_provided = abap_true.
                  v_prov_bus_id = VALUE #( v_provider_bus_ids[ account = <inst>-installation_owner ]-business_id OPTIONAL ).
                  IF v_prov_bus_id IS NOT INITIAL.
                    CONCATENATE v_prov_bus_id '-' INTO v_sess_installations-installation_desc3 SEPARATED BY space.
                  ENDIF.

                ENDIF.
                CONCATENATE v_sess_installations-installation_desc3 <inst>-installation_desc
                INTO v_sess_installations-installation_desc3 SEPARATED BY space.
                IF <inst>-location_name IS NOT INITIAL.
                  CONCATENATE v_sess_installations-installation_desc3 '|' <inst>-location_name
                  INTO v_sess_installations-installation_desc3 SEPARATED BY space.
                ENDIF.
                v_sess_installations-bandwidth3 = <inst>-bandwidth.
                v_sess_installations-installation_id3 = <inst>-installation.
              ENDIF.

            WHEN 4.
              v_sess_installations-installation_id4 = <inst>-installation.
********************************************************************************************************************
*--- "SPCRC-5940 // SPCRC-1614: Installation Description should be "Assigned" if Installation does not belong to me.
              IF <inst>-participant IS NOT INITIAL.
                IF <inst>-participant NE me->company_account.
                  IF lv_no_masking NE 'X'.

                    IF <inst>-installation_owner NE me->company_account. "SPCRC-5940 // SPCRC-1614: Installation Description should be "Assigned" if Installation does not belong to me.
****      SPCRC-6776 - in case the installation is masked we need to take the splitlocation into account as well
                      IF <inst>-installation EQ v_sess_installations-installation_id3. "Splitlocation
*         if we have a split location, we just skip the entry and reset the counter
                        v_tabix = v_tabix - 1.
                      ELSE.
                        v_sess_installations-installation_desc4 = 'Assigned'.
                      ENDIF.
                      CONTINUE.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
********************************************************************************************************************

              IF <inst>-installation EQ v_sess_installations-installation_id3. "Splitlocation
                IF <inst>-location_name IS NOT INITIAL.
                  CONCATENATE v_sess_installations-installation_desc3 '/'
                  <inst>-location_name
                  INTO v_sess_installations-installation_desc3 SEPARATED BY space.
                ENDIF.
                v_tabix = v_tabix - 1.
                CLEAR  v_sess_installations-installation_id4.
              ELSE.

                IF <inst>-is_provided = abap_true.
                  v_prov_bus_id = VALUE #( v_provider_bus_ids[ account = <inst>-installation_owner ]-business_id OPTIONAL ).
                  IF v_prov_bus_id IS NOT INITIAL.
                    CONCATENATE v_prov_bus_id '-' INTO v_sess_installations-installation_desc4 SEPARATED BY space.
                  ENDIF.
                ENDIF.
                CONCATENATE v_sess_installations-installation_desc4 <inst>-installation_desc
               INTO v_sess_installations-installation_desc4 SEPARATED BY space.
                IF <inst>-location_name IS NOT INITIAL.
                  CONCATENATE v_sess_installations-installation_desc4 '|' <inst>-location_name
                  INTO v_sess_installations-installation_desc4 SEPARATED BY space.
                ENDIF.
                v_sess_installations-bandwidth4 = <inst>-bandwidth.
                v_sess_installations-installation_id4 = <inst>-installation.
                EXIT. " ahouldn't be necessary if every Session has not  more than 4 Installation
              ENDIF.
          ENDCASE.

        ENDLOOP.
        CLEAR v_tabix.
      ENDIF.

*--- Copy of ETI Session
      v_sess_installations-installation_id1 = |{ v_sess_installations-installation_id1 ALPHA = OUT }|.
      IF v_sess_installations-installation_id1 IS NOT INITIAL
        AND v_sess_installations-bandwidth1 IS NOT INITIAL.
        CONCATENATE v_sess_installations-installation_desc1 '|' v_sess_installations-bandwidth1
                INTO v_sess_installations-installation_desc1 SEPARATED BY space.
      ENDIF.
      v_sess_installations-installation_id2 = |{ v_sess_installations-installation_id2 ALPHA = OUT }|.
      IF v_sess_installations-installation_id2 IS NOT INITIAL
        AND v_sess_installations-bandwidth2 IS NOT INITIAL.
        CONCATENATE v_sess_installations-installation_desc2 '|' v_sess_installations-bandwidth2
                INTO v_sess_installations-installation_desc2 SEPARATED BY space.
      ENDIF.
      v_sess_installations-installation_id3 = |{ v_sess_installations-installation_id3 ALPHA = OUT }|.
      IF v_sess_installations-installation_id3 IS NOT INITIAL
        AND v_sess_installations-bandwidth3 IS NOT INITIAL.
        CONCATENATE v_sess_installations-installation_desc3 '|' v_sess_installations-bandwidth3
                INTO v_sess_installations-installation_desc3 SEPARATED BY space.
      ENDIF.
      v_sess_installations-installation_id4 = |{ v_sess_installations-installation_id4 ALPHA = OUT }|.
      IF v_sess_installations-installation_id4 IS NOT INITIAL
        AND v_sess_installations-bandwidth4 IS NOT INITIAL.
        CONCATENATE v_sess_installations-installation_desc4 '|' v_sess_installations-bandwidth4
                INTO v_sess_installations-installation_desc4 SEPARATED BY space.
      ENDIF.

      APPEND v_sess_installations TO r_sess_installations.
      CLEAR v_sess_installations.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_bp_name.

    SELECT partner,
           name_org1,
           name_org2,
           name_org3,
           name_org4,
           name_last,
           name_first INTO CORRESPONDING FIELDS OF TABLE @c_partner_names
           FROM but000
           FOR ALL ENTRIES IN @c_partner_names WHERE partner = @c_partner_names-partner.

  ENDMETHOD.


  METHOD get_coop_exchange_partners.

    IF i_service_id IS NOT INITIAL.

      SELECT partner_exchange FROM zpt_pex_serv_c INTO TABLE @DATA(v_coop_partner_t) WHERE service EQ @i_service_id.
      APPEND LINES OF v_coop_partner_t TO r_involved_customer_tab.

    ENDIF.

  ENDMETHOD.


  METHOD get_installation_owner.

    IF i_installation_id IS NOT INITIAL.

      DATA(v_installation_id) = |{ i_installation_id ALPHA = OUT }|.
      SELECT company_id FROM zib_installations_cds INTO TABLE @DATA(v_customer_ids) WHERE installation_id EQ @v_installation_id.

      IF v_customer_ids IS NOT INITIAL.
        SORT v_customer_ids BY company_id.
        DELETE ADJACENT DUPLICATES FROM v_customer_ids COMPARING company_id.
        APPEND LINES OF v_customer_ids TO r_installation_owner.
      ENDIF.
    ENDIF.
    " From Cancel Installation Request Overview; Other Parties might be involved: fetch from Request
    SELECT o~request_guid FROM zib_tur_orders_cds AS o
    JOIN zib_tur_order_installations AS inst ON o~request_guid = inst~request_guid
    AND o~request_type = 'PORT032'
     INTO TABLE @DATA(t_involved_reqs) WHERE ibase = @i_installation_id.
    CHECK sy-subrc = 0.
    SORT t_involved_reqs BY request_guid.
    DELETE ADJACENT DUPLICATES FROM t_involved_reqs COMPARING request_guid.
    LOOP AT t_involved_reqs ASSIGNING FIELD-SYMBOL(<req>).
      IF me->is_read_request_allowed( iv_guid = <req>-request_guid ) = abap_true
      AND NOT line_exists( r_installation_owner[ table_line = me->company_account ] ).
        APPEND me->company_account TO r_installation_owner.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_involved_customer.

    DATA: lt_business_id TYPE zib_business_id_tt.
    DATA: lt_usage       TYPE zib_ibase_product_use_tt.
    DATA: v_product_id TYPE comt_product_id.

    v_product_id     = |{ i_product_id ALPHA = IN }|.

*get involved parties via the product
    IF v_product_id IS NOT INITIAL.

* 1. get account from fast select1
      SELECT SINGLE account FROM zib_fast_select1
         INTO @DATA(v_customer_id)
        WHERE product_id EQ @v_product_id.

      IF v_customer_id IS NOT INITIAL.
        APPEND v_customer_id TO r_involved_customer_tab.
      ENDIF.

* 2. get account via BusinessID (if available)
      SELECT lsm~business_id FROM zimlsmapplctdata AS lsm
        INNER JOIN zib_fast_select1 AS fs1
          ON fs1~product_guid = lsm~product_guid
        INTO TABLE lt_business_id
        WHERE fs1~product_id = v_product_id.

      IF sy-subrc = 0.
        zcl_service_toolbox=>get_bp_by_memberid(
          EXPORTING
            it_memberid       = lt_business_id                 " Business ID Table Type
            iv_with_adm       = 'X'              " Select all Admissions
            iv_with_but0ids   = 'X'                  " Select all ButoIDs (Admissions)
          IMPORTING
            et_bps            = DATA(lt_bps)                  " Table type für result for map admissions to ibase services
        ).

        LOOP AT lt_bps INTO DATA(ls_bps).
          APPEND ls_bps-bp TO r_involved_customer_tab.
        ENDLOOP.
      ENDIF.

* 3. get accounts via Ibases where the product is installed

      CALL FUNCTION 'ZIB_PRODUCT_IBASE_GET'
        EXPORTING
*         IV_PRODUCT_GUID       =
          iv_product_id = v_product_id
          iv_usage      = '2'  "customer only
        IMPORTING
          et_part_use   = lt_usage.

      LOOP AT lt_usage INTO DATA(ls_usage).
        APPEND ls_usage-account_partner TO r_involved_customer_tab.

      ENDLOOP.
    ENDIF.

* get involved parties from the request.
    IF i_request_guid IS NOT INITIAL.

      SELECT SINGLE company_partner FROM zib_tur_orders_cds INTO @v_customer_id WHERE request_guid EQ @i_request_guid.
      APPEND v_customer_id TO r_involved_customer_tab.

    ENDIF.

* Added for change Session Requests, we need to check if a new Provider is added to the Request: SPCRC-7038
    IF i_product_id IS NOT INITIAL.

      SELECT SINGLE request_guid
        FROM zib_tur_order_services_cds AS serv
        INNER JOIN zbtx_tur_order_b AS order
          ON serv~request_guid = order~order_guid
          WHERE serv~related_product EQ @v_product_id
            AND ( order~status EQ @zif_ib_tur_const=>co_tur_order_status-waiting_approval
               OR order~status EQ @zif_ib_tur_const=>co_tur_order_status-completed
                )
        INTO @DATA(v_request_guid) .

      IF v_request_guid IS NOT INITIAL.
        IF is_read_request_allowed( v_request_guid ) EQ abap_true.
          APPEND me->company_account TO r_involved_customer_tab.
        ENDIF.
      ENDIF.
    ENDIF.


*convert to be sure - leading '0' should be there
    LOOP AT r_involved_customer_tab ASSIGNING FIELD-SYMBOL(<fs_customer>).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_customer>
        IMPORTING
          output = <fs_customer>.

    ENDLOOP.

    SORT r_involved_customer_tab.
    DELETE ADJACENT DUPLICATES FROM r_involved_customer_tab.



  ENDMETHOD.


  METHOD get_open_requests.

    IF i_prod_id_tt IS NOT INITIAL.

      SELECT product_id AS object_id,
                request_guid,
                request_id,
                request_type,
                service_id,
                channel_service
*           @abap_true AS has_open_req
            INTO CORRESPONDING FIELDS OF TABLE @me->open_requests
            FROM zib_tur_open_requests_cds
            FOR ALL ENTRIES IN @i_prod_id_tt
            WHERE product_id = @i_prod_id_tt-table_line.

      SORT open_requests BY object_id service_id.
      DELETE ADJACENT DUPLICATES FROM open_requests COMPARING object_id service_id.

    ELSEIF i_relnr_tab IS NOT INITIAL.
*SELECT SINGLE partner1 from but050 where relnr = @i_relnr_tab
      SELECT related_rel_nr AS object_id,
             service_id,
             request_guid,
             request_id
        INTO CORRESPONDING FIELDS OF TABLE @me->open_requests "TABLE @DATA(v_mm_rel_open_req_t)
        FROM zib_tur_mm_rel_openreq_cds
        FOR ALL ENTRIES IN @i_relnr_tab
          WHERE related_rel_nr = @i_relnr_tab-relnr
          AND mm_rel_partner = @i_relnr_tab-rel_partner.


    ENDIF.

  ENDMETHOD.


  METHOD get_stream.

*-- should be redefined by subclass IF need
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc.


  ENDMETHOD.


  METHOD handle_action.
    er_entity = ''.
*-- should be redefined by subclass IF need
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc.


  ENDMETHOD.


  METHOD init.

    tur_api = zcl_ibase_tur_api=>get_instance(  ).
**    CONV crmt_object_guid( me->odata_app->get_context_attribute( i_attribute = 'USER-CONTRACT' ) ).
*
*    DATA(v_change_org_flag) = me->odata_app->get_context_attribute( i_attribute = 'CHANGE-ORG' ).
*
**    SELECT SINGLE zz_partner_z001 AS company_partner,
**                  zz_partner_z002  AS user_account
**      INTO @DATA(v_accounts)
**      FROM    zbtx_index_srq
**      WHERE   guid = @v_contract_guid.
*
**--- Change Org adjustment:
*    IF v_change_org_flag EQ 'true'.
*
*      DATA(v_change_org) = CONV bu_partner( me->odata_app->get_context_attribute( i_attribute = 'CHANGE-ORG-NUMBER' ) ).
*      DATA(v_change_org_no) = |{ v_change_org ALPHA = IN }|.
*
*      v_accounts-company_partner = v_change_org_no.
*    ENDIF.
*
*    me->company_account = v_accounts-company_partner.
*    me->session_user = v_accounts-user_account.

  ENDMETHOD.


  METHOD is_read_request_allowed.
    read_allowed = abap_true. " by default
    me->tur_api->get_partner_fct(
      EXPORTING
        i_request_guid = iv_guid
        i_partner      = me->company_account
      RECEIVING
        r_partner_fct  = DATA(v_patner_fct)
    ).

    IF v_patner_fct IS INITIAL.
      read_allowed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD is_valid_ib.

    DATA v_parts                TYPE zttib_if_parts.

    r_is_valid = abap_false.

    CALL FUNCTION 'ZIB_IF_PARTS_GET_FAST'
      EXPORTING
        iv_installation = i_ib_id
      IMPORTING
        et_parts        = v_parts
      EXCEPTIONS
        no_installation = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

**********************************************************************
*Bug: SPCRC-2503
* if Installation contains parts other than Root Objects, then it is empty
* Root Pools have Parent = 0

    IF line_exists( v_parts[ category_id = 'HISTORY_LINEPOOL' ] ).
      DATA(v_old_idx) = VALUE #( v_parts[ category_id = 'HISTORY_LINEPOOL' ]-indx OPTIONAL ).
    ENDIF.

    DELETE v_parts WHERE ( parent = 0 AND category_id <> 'PIPE' ). "OR parent NE v_old_idx. "delete all root objects

    IF v_parts IS INITIAL. " IF no parts are left, then Installation had only Root objects => Not Valid = Status Empty
      r_is_valid = abap_false.
      RETURN.

    ELSE. " If more objects exist, delete those that could be from Old Line Pool

      IF v_old_idx IS NOT INITIAL.
        DELETE v_parts WHERE parent = v_old_idx ."and category_id <> 'LINEPOOL'.
      ENDIF.

      IF v_parts IS NOT INITIAL. " If any Parts are left after deleting objects that belong to old Pool, then Installation is Valid: Not OLD, NOT EMPTY
        r_is_valid = abap_true.
        RETURN.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD query.

*-- should be redefined by subclass IF need
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc.

  ENDMETHOD.


  METHOD read.

*-- should be redefined by subclass IF need
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc.

  ENDMETHOD.


  METHOD update.

*-- should be redefined by subclass IF need
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc.

  ENDMETHOD.


  METHOD validate.
  ENDMETHOD.


  METHOD validate_business_ids.

    SELECT partner, idnumber FROM but0id
       FOR ALL ENTRIES IN @i_results WHERE partner = @i_results-account AND idnumber = @i_results-business_id
       AND ( valid_date_to EQ @( || ) OR valid_date_to >= @sy-datum ) "valid_date_to EQ @( || ) OR valid_date_to >= @sy-datum
       INTO TABLE @DATA(v_valid_business_id).
    IF sy-subrc = 0.
      LOOP AT c_result ASSIGNING FIELD-SYMBOL(<res>).
        IF NOT line_exists( v_valid_business_id[ idnumber = <res>-business_id partner = <res>-account ] ).
          DELETE c_result.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD validate_permission.

    DATA: v_view_parameter_tab        TYPE if_salv_ida_types_int=>yt_parameter,
          v_aktiv_user_permission_tab TYPE zif_expo_types=>ty_view-portal_user_permission_tab.

    r_has_permission = abap_false.
* Get granted Permissions:
    DATA(v_user_permission_query)    = NEW zcl_ida_query_engine( i_query_entity           = CONV #( zif_cds_query=>co_ext_portal-aktiv_user_permission )
                                                                   i_cds_view_parameter_tab = v_view_parameter_tab ).

    v_user_permission_query->zif_exas_query~set_range(  i_column    = 'CONTACT_PARTNER'
                                                        i_range_tab = VALUE /iwbep/t_cod_select_options( ( sign = 'I' option = 'EQ'  low = me->session_user ) )
                                                            ).
* *-- GET Portal User by Contact Id )
    v_user_permission_query->zif_exas_query~exec( CHANGING c_query_result = v_aktiv_user_permission_tab ).
    CHECK v_aktiv_user_permission_tab IS NOT INITIAL.
    CASE   i_action.
      WHEN zif_ib_tur_const=>c_approval_step_status-approved OR zif_ib_tur_const=>c_approval_step_status-rejected.
        IF line_exists( v_aktiv_user_permission_tab[ permission_id = zif_ib_tur_const=>c_tur_portal_user_perm-approve_request ] ) OR
            line_exists( v_aktiv_user_permission_tab[ permission_id = zif_ib_tur_const=>c_tur_portal_user_perm-change_org_ap ] ).
          r_has_permission = abap_true.
        ENDIF.
      WHEN zif_ib_tur_const=>c_approval_step_status-cancelled.
        IF line_exists( v_aktiv_user_permission_tab[ permission_id = zif_ib_tur_const=>c_tur_portal_user_perm-maintain_data ] )
        OR line_exists( v_aktiv_user_permission_tab[ permission_id = zif_ib_tur_const=>c_tur_portal_user_perm-change_org ] ).
          r_has_permission = abap_true.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_odata_entity~count.
    TRY.
        r_result = me->count(  EXPORTING  i_select_option_tab = i_select_option_tab
                                          i_osql_where = i_osql_where ).
      CATCH /iwbep/cx_mgw_not_impl_exc INTO DATA(v_not_impl_err).
*    zcx_gateway_business_exception=>raise_business_exception(  ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_odata_entity~create.
    validate( i_method        = zif_odata_entity=>co_validate_method_create
              i_entity        = i_new_entity
              i_contract_guid = i_contract_guid
              i_entity_name   = i_entity_name
              i_tutorial_mode = i_tutorial_mode ).

    TRY.
        me->create(
            EXPORTING
             i_new_entiy = i_new_entity
            IMPORTING
             e_entity = e_entity
             ).
      CATCH zcx_crm_app INTO DATA(v_app_exc).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid        = zcx_cua_btx_general=>error
            error_msg     = v_app_exc->get_text( )
            callstack     = zcl_cua_bxt_tools=>get_callstack( )
            kind_of_error = v_app_exc->kind_of_error.
      CATCH zcx_http INTO DATA(v_http_exception).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid    = zcx_cua_btx_general=>error
            error_msg = v_http_exception->get_text( )
            callstack = zcl_cua_bxt_tools=>get_callstack( ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_odata_entity~create_stream.

*    DATA(v_decoded_filename) = cl_http_utility=>unescape_url( escaped = i_new_stream-file_name  ).
    DATA(v_attachment) = VALUE ztur_order_attachment( file_content = i_new_stream-content
                                                      file_name    = i_new_stream-file_name
                                                      account      = me->company_account
                                                      author       = me->session_user
                                                      mime_type    = i_new_stream-mime_type ).

    me->create_stream( EXPORTING i_key_tab = i_key_tab
                                 i_content = v_attachment
                      IMPORTING
                              e_file_info = e_stream
                                    ).


  ENDMETHOD.


  METHOD zif_odata_entity~delete.
    validate( i_key     = i_key
              i_key_tab = i_key_tab
              i_method  = zif_odata_entity=>co_validate_method_delete ).

    TRY.
        me->delete( EXPORTING i_key = i_key i_key_tab = i_key_tab ).
      CATCH /iwbep/cx_mgw_not_impl_exc INTO DATA(v_not_impl_err).
        RAISE EXCEPTION TYPE zcx_gateway_technic_exception.
      CATCH zcx_crm_app INTO DATA(v_app_exc).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid        = zcx_cua_btx_general=>error
            error_msg     = v_app_exc->get_text( )
            callstack     = zcl_cua_bxt_tools=>get_callstack( )
            kind_of_error = v_app_exc->kind_of_error.
      CATCH zcx_http INTO DATA(v_http_exception).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid    = zcx_cua_btx_general=>error
            error_msg = v_http_exception->get_text( )
            callstack = zcl_cua_bxt_tools=>get_callstack( ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_odata_entity~get_stream.
    IF line_exists( i_key_tab[ name = 'RequestGUID' ] ).
      DATA(v_guid) =  i_key_tab[ name = 'RequestGUID' ]-value.
      IF is_read_request_allowed( CONV #( v_guid ) ) <> abap_true.
        zcx_http=>raise_unauthorized( |No Permission to read/access requested Data| ).
      ENDIF.
    ENDIF.

    validate( i_key    = VALUE #( ( name = 'REQUEST_GUID' value = v_guid ) )
              i_method = zif_odata_entity=>co_validate_method_get_stream ).

    TRY.

        r_result = me->get_stream(  i_key_tab = i_key_tab     ).
*        r_result-file_name = cl_http_utility=>escape_url( unescaped = r_result-file_name  ).

      CATCH /iwbep/cx_mgw_not_impl_exc INTO DATA(v_not_impl_err).
        RAISE EXCEPTION TYPE zcx_gateway_technic_exception.
      CATCH zcx_crm_app INTO DATA(v_app_exc).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid        = zcx_cua_btx_general=>error
            error_msg     = v_app_exc->get_text( )
            callstack     = zcl_cua_bxt_tools=>get_callstack( )
            kind_of_error = v_app_exc->kind_of_error.
      CATCH zcx_http INTO DATA(v_http_exception).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid    = zcx_cua_btx_general=>error
            error_msg = v_http_exception->get_text( )
            callstack = zcl_cua_bxt_tools=>get_callstack( ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_odata_entity~handle_action.
    validate( i_method      = zif_odata_entity=>co_validate_method_h_action
              i_parameters  = i_parameters
              i_entity_name = i_entity_name
              i_action      = i_action ).

    TRY.
        me->handle_action(
          EXPORTING
            i_action     = i_action
            i_parameters = i_parameters
          IMPORTING
            er_entity    = er_entity
        ).
      CATCH /iwbep/cx_mgw_not_impl_exc INTO DATA(v_not_impl_err).
        RAISE EXCEPTION TYPE zcx_gateway_technic_exception.
      CATCH zcx_crm_app INTO DATA(v_app_exc).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid        = zcx_cua_btx_general=>error
            error_msg     = v_app_exc->get_text( )
            callstack     = zcl_cua_bxt_tools=>get_callstack( )
            kind_of_error = v_app_exc->kind_of_error.
      CATCH zcx_http INTO DATA(v_http_exception).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid    = zcx_cua_btx_general=>error
            error_msg = v_http_exception->get_text( )
            callstack = zcl_cua_bxt_tools=>get_callstack( ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_odata_entity~init.

    me->odata_app = i_application.

    DATA(v_portal_user_data) = me->odata_app->get_current_user( )->get_general_data( ).

    me->user_contract_guid = v_portal_user_data-contract_guid.
    me->session_user = v_portal_user_data-contact_partner.

    DATA(v_is_change_org) = me->odata_app->get_context_attribute( i_attribute = 'CHANGE-ORG' ).

    IF v_is_change_org EQ 'true'.
      DATA(v_change_org) = CONV bu_partner( me->odata_app->get_context_attribute( i_attribute = 'CHANGE-ORG-NUMBER' ) ).
      DATA(v_change_org_no) = |{ v_change_org ALPHA = IN }|.
      me->company_account = v_change_org_no.
    ELSE.
      me->company_account = v_portal_user_data-company_partner.
    ENDIF.

    me->init( ).

  ENDMETHOD.


  METHOD zif_odata_entity~query.

    IF line_exists( i_select_option_tab[ property = 'ORDER_GUID' ] )
    OR line_exists( i_select_option_tab[ property = 'REQUEST_GUID' ] ).
      DATA(v_guid) =  VALUE #( i_select_option_tab[ property = 'ORDER_GUID' ]-select_options[ 1 ]-low OPTIONAL ).
      IF v_guid IS INITIAL.
        v_guid = VALUE #( i_select_option_tab[ property = 'REQUEST_GUID' ]-select_options[ 1 ]-low OPTIONAL ).
      ENDIF.
      IF is_read_request_allowed( CONV #( v_guid ) ) <> abap_true.
        zcx_http=>raise_unauthorized( |No Permission to read/access requested Data| ).
      ENDIF.
    ENDIF.

    validate( i_select_option_tab = i_select_option_tab
              i_method            = zif_odata_entity=>co_validate_method_query
              i_entity_name       = i_entity_name
              i_navigation_path   = i_navigation_path ).

    TRY.
        me->query(
             EXPORTING
                i_select_option_tab = i_select_option_tab
                i_nav_path          = i_navigation_path
                i_select_column_tab = i_select_column_tab
                i_osql_where        = i_osql_where
                i_skip              = i_skip
                i_top               = i_top
                i_sort_element_tab  = i_sort_element_tab
                i_entity_name       = i_entity_name
              CHANGING
                c_query_result = c_query_result
        ).
      CATCH /iwbep/cx_mgw_not_impl_exc INTO DATA(v_not_impl_err).
        RAISE EXCEPTION TYPE zcx_gateway_technic_exception.
      CATCH zcx_crm_app INTO DATA(v_app_exc).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid        = zcx_cua_btx_general=>error
            error_msg     = v_app_exc->get_text( )
            callstack     = zcl_cua_bxt_tools=>get_callstack( )
            kind_of_error = v_app_exc->kind_of_error.
      CATCH zcx_http INTO DATA(v_http_exception).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid    = zcx_cua_btx_general=>error
            error_msg = v_http_exception->get_text( )
            callstack = zcl_cua_bxt_tools=>get_callstack( ).
    ENDTRY.

*     if i_skip <> 0.
*    DELETE TABLE c_query_result FROM  .
*    ENDIF.
*
*    if i_top is NOT INITIAL.
*    DATA(max) = i_top + 1.
*    DELETE c_query_result FROM max to lines( c_query_result ).
*    ENDIF.
  ENDMETHOD.


  METHOD zif_odata_entity~query_expand.

  ENDMETHOD.


  METHOD zif_odata_entity~read.

    IF line_exists( i_key[ name = 'REQUEST_GUID' ] ).
      DATA(v_guid) =  i_key[ name = 'REQUEST_GUID' ]-value.
      IF is_read_request_allowed( CONV #( v_guid ) ) <> abap_true.
        zcx_http=>raise_unauthorized( |No Permission to read/access requested Data| ).
        RETURN.
      ENDIF.

    ENDIF.

    validate( i_key             = COND #( WHEN i_key IS NOT INITIAL
                                          THEN i_key
                                          ELSE i_source_key_tab )
              i_method          = zif_odata_entity=>co_validate_method_read
              i_entity_name     = i_entity_name
              i_navigation_path = i_navigation_tab ).

    TRY.
        me->read( EXPORTING
                      i_key                 = i_key
                      i_navigation_tab      = i_navigation_tab
                      i_key_tab             = i_key_tab
                      i_select_column_tab   = i_select_column_tab
                      i_source_key          = i_source_key_tab
                  IMPORTING
                    e_entity = e_entity
                 ).
      CATCH /iwbep/cx_mgw_not_impl_exc INTO DATA(v_not_impl_err).
        RAISE EXCEPTION TYPE zcx_gateway_technic_exception.
      CATCH zcx_crm_app INTO DATA(v_app_exc).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid        = zcx_cua_btx_general=>error
            error_msg     = v_app_exc->get_text( )
            callstack     = zcl_cua_bxt_tools=>get_callstack( )
            kind_of_error = v_app_exc->kind_of_error.
      CATCH zcx_http INTO DATA(v_http_exception).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid    = zcx_cua_btx_general=>error
            error_msg = v_http_exception->get_text( )
            callstack = zcl_cua_bxt_tools=>get_callstack( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_odata_entity~update.

    TRY.
        validate( i_method      = zif_odata_entity=>co_validate_method_update
                  i_entity      = i_update_entity
                  i_entity_name = i_entity_name ).

        me->update( EXPORTING
                      i_key                 = i_key
*                  i_navigation_tab      = i_navigation_tab
                      i_key_tab             = i_key_tab
                      i_update_entity        = i_update_entity
*                  i_select_column_tab   = i_select_column_tab
*                  i_source_key          = i_source_key_tab
                  IMPORTING
                    e_entity = e_entity
                 ).

      CATCH /iwbep/cx_mgw_not_impl_exc INTO DATA(v_not_impl_err).
        RAISE EXCEPTION TYPE zcx_gateway_technic_exception.
      CATCH zcx_crm_app INTO DATA(v_app_exc).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid        = zcx_cua_btx_general=>error
            error_msg     = v_app_exc->get_text( )
            callstack     = zcl_cua_bxt_tools=>get_callstack( )
            kind_of_error = v_app_exc->kind_of_error.
      CATCH zcx_http INTO DATA(v_http_exception).
        RAISE EXCEPTION TYPE zcx_gateway_exception
          EXPORTING
            textid    = zcx_cua_btx_general=>error
            error_msg = v_http_exception->get_text( )
            callstack = zcl_cua_bxt_tools=>get_callstack( ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
