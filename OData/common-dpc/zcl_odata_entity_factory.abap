CLASS zcl_odata_entity_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_odata_entity_injector .

  PUBLIC SECTION.

    CLASS-METHODS get_external_user
      IMPORTING
        !i_user_id      TYPE crmt_object_guid
      RETURNING
        VALUE(r_result) TYPE REF TO zif_expo_user .
    "! <p class="shorttext synchronized" lang="en">Get OData Entity implementation</p>
    "! Get an OData Entity implementation, using the Technical Service Name, Entity name and key.
    "! @parameter in_entity_id | <p class="shorttext synchronized" lang="en">Entity Key</p>
    "! @parameter in_entity_name | <p class="shorttext synchronized" lang="en">OData Entity name</p>
    "! @parameter in_technical_name | <p class="shorttext synchronized" lang="en">OData Technical service name</p>
    "! @parameter rv_result | <p class="shorttext synchronized" lang="en">OData Entity instance</p>
    CLASS-METHODS get_entity
      IMPORTING
        !in_entity_id         TYPE any
        !in_entity_name       TYPE /iwbep/mgw_tech_name
        !in_tech_service_name TYPE /iwbep/med_grp_technical_name
        !in_tech_service_vers TYPE /iwbep/med_grp_version
      RETURNING
        VALUE(rv_result)      TYPE REF TO zif_odata_entity .
    CLASS-METHODS class_constructor .
    CLASS-METHODS get_application_impl
      IMPORTING
        !i_service_name    TYPE /iwbep/med_grp_technical_name
        !i_service_version TYPE /iwbep/med_grp_version
      RETURNING
        VALUE(r_result)    TYPE REF TO zif_odata_application .
    CLASS-METHODS create_interface_impl
      IMPORTING
        !i_entity_class_name TYPE seoclsname
      RETURNING
        VALUE(rv_result)     TYPE REF TO zif_odata_entity .
    CLASS-METHODS get_entity_impl
      IMPORTING
        !i_entity_name  TYPE csequence
        !i_base_class   TYPE seoclsname OPTIONAL
      RETURNING
        VALUE(r_result) TYPE REF TO zif_odata_entity .

    "! Get an external portal company by Company key. Company <strong>Contract GUID</strong> or <strong>Company Partner number</strong>
    "! can be used as key
    "! @parameter i_company_id | Company Portal Contract GUID or Company Business Partner Number
    "! @parameter r_result |
    CLASS-METHODS get_external_company

      IMPORTING
                i_company_id    TYPE data
      RETURNING VALUE(r_result) TYPE REF TO zif_expo_company.
  PROTECTED SECTION.


  PRIVATE SECTION.

    CLASS-DATA entety_impl_tab TYPE ty_entety_impl_tab .
    CLASS-DATA entity_cache_tab TYPE ty_cache_entity_tab .
    CLASS-DATA service_config_tab TYPE ty_service_config_tab .
    CLASS-DATA application_instance_tab TYPE ty_application_instance_tab .
    CLASS-DATA cust_application_tab TYPE ty_application_config_tab .
    CLASS-DATA external_user_prototype TYPE REF TO zif_expo_user .
    CLASS-DATA external_company_prototype TYPE REF TO zif_expo_company .
    CLASS-DATA external_user_clone TYPE REF TO zif_expo_user .

    CLASS-METHODS get_entity_cls_by_tech_service
      IMPORTING
        !in_tech_service_name TYPE /iwbep/med_grp_technical_name
        !in_tech_service_vers TYPE /iwbep/med_grp_version
        !in_entity_name       TYPE /iwbep/mgw_tech_name
      RETURNING
        VALUE(rv_result)      TYPE seoclsname .
    CLASS-METHODS class_syntax_check
      IMPORTING
        !i_entity_class_name TYPE seoclsname
      RETURNING
        VALUE(r_result)      TYPE abap_bool .
ENDCLASS.



CLASS ZCL_ODATA_ENTITY_FACTORY IMPLEMENTATION.


  METHOD class_constructor.

    "#TODO move this code a customizing table

    APPEND VALUE #( service_name    = zcl_exas_odata_app=>co_service_name
                    service_version = zcl_exas_odata_app=>co_service_version
                    impl_class      =  'ZCL_EXAS_ODATA_APP'
    ) TO zcl_odata_entity_factory=>cust_application_tab.

    APPEND VALUE #( service_name    = 'ZFI_BILLING_PORTAL_ODATA_SRV'
                    service_version = '0001'
                    impl_class      = 'ZCL_BILLP_ODATA_APP'
    ) TO zcl_odata_entity_factory=>cust_application_tab.

    APPEND VALUE #( service_name    = zcl_general_odata_app=>co_service_name
                    service_version = zcl_general_odata_app=>co_service_version
                    impl_class      = 'ZCL_GENERAL_ODATA_APP'
    ) TO zcl_odata_entity_factory=>cust_application_tab.

    APPEND VALUE #( service_name    = zcl_userid_odata_app=>co_service_name
                    service_version = zcl_userid_odata_app=>co_service_version
                    impl_class      = 'ZCL_USERID_ODATA_APP'
    ) TO zcl_odata_entity_factory=>cust_application_tab.


    APPEND VALUE #( service_name    = zcl_cua_odata_app=>co_service_name
                service_version     = zcl_cua_odata_app=>co_service_version
                impl_class          = 'ZCL_CUA_ODATA_APP'
                 ) TO zcl_odata_entity_factory=>cust_application_tab.

    APPEND VALUE #( service_name    = zcl_cua_nav_odata_app=>co_service_name
                service_version     = zcl_cua_nav_odata_app=>co_service_version
                impl_class          = 'ZCL_CUA_NAV_ODATA_APP'
                 ) TO zcl_odata_entity_factory=>cust_application_tab.

    APPEND VALUE #( service_name    = zcl_cua_enav_odata_app=>co_service_name
                service_version     = zcl_cua_enav_odata_app=>co_service_version
                impl_class          = 'ZCL_CUA_ENAV_ODATA_APP'
                 ) TO zcl_odata_entity_factory=>cust_application_tab.

    APPEND VALUE #( service_name    = zcl_tur_odata_app=>co_service_name
                service_version     = zcl_tur_odata_app=>co_service_version
                impl_class          = 'ZCL_TUR_ODATA_APP'
                ) TO zcl_odata_entity_factory=>cust_application_tab.

    APPEND VALUE #( service_name    = zcl_tur_f4_odata_app=>co_service_name
                service_version     = zcl_tur_f4_odata_app=>co_service_version
                impl_class          = 'ZCL_TUR_F4_ODATA_APP'
                ) TO zcl_odata_entity_factory=>cust_application_tab.


    APPEND VALUE #( service_name    = zcl_techu_admin_odata_app=>co_service_name
                service_version     = zcl_techu_admin_odata_app=>co_service_version
                impl_class          = 'ZCL_TECHU_ADMIN_ODATA_APP'
                ) TO zcl_odata_entity_factory=>cust_application_tab.

    APPEND VALUE #( service_name    = zcl_cua_uov_odata_app=>co_service_name
                service_version     = zcl_cua_uov_odata_app=>co_service_version
                impl_class          = 'ZCL_CUA_UOV_ODATA_APP'
                ) TO zcl_odata_entity_factory=>cust_application_tab.

    APPEND VALUE #( service_name    = zcl_cua_mypr_odata_app=>co_service_name
                service_version     = zcl_cua_mypr_odata_app=>co_service_version
                impl_class          = 'ZCL_CUA_MYPR_ODATA_APP'
                ) TO zcl_odata_entity_factory=>cust_application_tab.


    APPEND VALUE #(
*                service_name    = zcl_cua_task_odata_app=>co_service_name
                service_name    = 'ZBF_CUA_OPEN_TASK_SRV'
*                service_version     = zcl_cua_task_odata_app=>co_service_version
                service_version     = '0001'
                impl_class          = 'ZCL_CUA_TASK_ODATA_APP'
                ) TO zcl_odata_entity_factory=>cust_application_tab.


    APPEND VALUE #(
*                service_name    = zcl_cua_cc_odata_app=>co_service_name
                service_name    = 'ZBF_CUA_CENTRAL_COORD_SRV'
*                service_version     = zcl_cua_cc_odata_app=>co_service_version
                service_version     = '0001'
                impl_class          = 'ZCL_CUA_CC_ODATA_APP'
                ) TO zcl_odata_entity_factory=>cust_application_tab.

    APPEND VALUE #(
*                service_name    = zcl_itai_odata_app=>co_service_name
                service_name    = 'ZITAI_ACR_SRV'
*                service_version     = zcl_itai_odata_app=>co_service_version
                service_version     = '0001'
                impl_class          = 'ZCL_ITAI_ODATA_APP'
                ) TO zcl_odata_entity_factory=>cust_application_tab.

    APPEND VALUE #(
                service_name    = 'ZBP_BSB_REG_SRV'
                service_version     = '0001'
                impl_class          = 'ZCL_BSB_REG_ODATA_APP'
                ) TO zcl_odata_entity_factory=>cust_application_tab.

      APPEND VALUE #(
                service_name    = 'ZBF_MESSAGE_BOARD_SRV'
                service_version     = '0001'
                impl_class          = 'ZCL_MB_ODATA_APP'
                ) TO zcl_odata_entity_factory=>cust_application_tab.

    zcl_odata_entity_factory=>external_user_prototype    = NEW zcl_external_portal_user( ).
    zcl_odata_entity_factory=>external_company_prototype = NEW zcl_expo_portal_company( ).




  ENDMETHOD.


  METHOD class_syntax_check.
*--------------------------------------------*
* Task: Perform a syntax check for the given *
*       ABAP class. In case of errors expor- *
*       ting parameter EV_ERROR_MESSAGE con- *
*       tains a corresponding error test.    *
*--------------------------------------------*

    DATA: v_includes_tab TYPE TABLE OF char40,
          v_line         TYPE i,
          v_word         TYPE string,
          v_message      TYPE string.


*-- Initialize return parameter
    r_result = abap_true.


*-- Build name of ABAP include that holds the ABAP class implementation
    DATA(v_source_name) = '==============================CP'. "#EC NOTEXT
    SHIFT v_source_name LEFT BY strlen( i_entity_class_name ) PLACES.
    v_source_name = i_entity_class_name && v_source_name.

*-- Check whether ABAP class can be generated
    LOAD REPORT v_source_name PART 'INCL' INTO v_includes_tab[].

    IF sy-subrc NE 0.
*-- Repo load not found, so try and generate - this may be the case in
      "brand new systems, or else load may be out of date - vs 17 Jun 2010
      GENERATE REPORT v_source_name.
      "OK now try and reload...
      CLEAR v_includes_tab[].
      LOAD REPORT v_source_name PART 'INCL' INTO v_includes_tab[].
      IF sy-subrc EQ 4. " Still not found, so ABAP class does not exist.
        r_result = abap_false.
      ENDIF.
    ENDIF.

*-- Perform syntax check on ABAP class
    SYNTAX-CHECK FOR PROGRAM v_source_name MESSAGE v_message LINE v_line WORD v_word.


    IF ( v_message IS NOT INITIAL ).

      r_result = abap_false.
    ENDIF.


  ENDMETHOD.


  METHOD create_interface_impl.

    DATA(v_interface_descr) =  CAST cl_abap_intfdescr( cl_abap_intfdescr=>describe_by_name( 'ZIF_ODATA_ENTITY' ) ).

    IF v_interface_descr->applies_to_class( i_entity_class_name ).
*      AND class_syntax_check( i_entity_class_name ).
      "The syntax check is quite expensive performance wise
      "thus it will be disabled, anyway an executable entity handler is expected.

      CREATE OBJECT rv_result TYPE (i_entity_class_name).

    ELSE.

    ENDIF.

  ENDMETHOD.


  METHOD get_application_impl.

*-- TODO perform class sintax check before creating the application instance

*-- TRY to retrieve the instance from cached table
    READ TABLE zcl_odata_entity_factory=>application_instance_tab
            TRANSPORTING instance INTO DATA(v_cache_app)
                WITH KEY service_name     = i_service_name
                   service_version = i_service_version.

    IF sy-subrc IS INITIAL.
      r_result = v_cache_app-instance.


    ELSE.

      TRY.

*-- Get application class name from Customizing table
          DATA(v_application_impl_class) = zcl_odata_entity_factory=>cust_application_tab[ service_name = i_service_name service_version = i_service_version ]-impl_class.

          CREATE OBJECT r_result TYPE (v_application_impl_class).


        CATCH cx_sy_itab_line_not_found.

*-- Use default application
          r_result = NEW  zcl_default_odata_app( ).
      ENDTRY.
    ENDIF.


*--ADD instance to local cache
    APPEND VALUE #(
        service_name    = i_service_name
        service_version = i_service_version
        instance        = r_result

    ) TO zcl_odata_entity_factory=>application_instance_tab.

  ENDMETHOD.


  METHOD get_entity.


    TRY.

        "Try to read Entity from local cache
        rv_result = zcl_odata_entity_factory=>entity_cache_tab[ obj_key = in_entity_id  ]-obj_ref.

      CATCH cx_sy_itab_line_not_found.

        "No entity found in local cache thus create a new one and add it to cache
        DATA(v_entity_class_name) = get_entity_cls_by_tech_service( in_tech_service_name = to_upper( in_tech_service_name )
                                                                    in_tech_service_vers = to_upper( in_tech_service_vers )
                                                                    in_entity_name       = in_entity_name
                                                                   ).


        "check if the given Class implements the required interface

        "rv_result = create_interface_impl( in_entity_id        = in_entity_id i_entity_class_name = v_entity_class_name ).


    ENDTRY.


  ENDMETHOD.


  METHOD get_entity_cls_by_tech_service.

    "Where to store ??
    IF zcl_odata_entity_factory=>service_config_tab[] IS INITIAL.

    ENDIF.

    TRY.

        rv_result = zcl_odata_entity_factory=>service_config_tab[ service_name = in_tech_service_name
                                                                  service_vers = in_tech_service_vers
                                                                  entity_name  = in_entity_name ]-entity_class.
      CATCH cx_sy_itab_line_not_found.


    ENDTRY.

  ENDMETHOD.


  METHOD get_entity_impl.
    "#TODO - Make the get entity more general, should no be using ZCL_EXAS_BASE_ODATA_ENTIY directly

    DATA: v_subclass_tab     TYPE seor_inheritance_keys.
    DATA: v_base_class_norm TYPE seoclskey,
          v_interf_impl_tab TYPE seor_implementing_keys.


    IF i_base_class IS SUPPLIED.

*-- Normalize Base Class name
      v_base_class_norm-clsname = to_upper( i_base_class  ) .

*-- GET all classes which directly inherit from ZCL_EXAS_BASE_ODATA_ENTIY
      CALL FUNCTION 'SEO_CLASS_GET_ALL_SUBS'
        EXPORTING
          clskey             = v_base_class_norm    " Class/Interface Key
        IMPORTING
          inhkeys            = v_subclass_tab
        EXCEPTIONS
          class_not_existing = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ELSE.

*-- If Entity Base class is not provided, load all  ZIF_ODATA_ENTITY implementation
      v_base_class_norm-clsname = to_upper( zif_odata_entity=>co_odata_entity_intf ).

      CALL FUNCTION 'SEO_INTERFACE_IMPLEM_GET_ALL'
        EXPORTING
          intkey       = v_base_class_norm   " Key structure of a class
        IMPORTING
          impkeys      = v_interf_impl_tab
        EXCEPTIONS
          not_existing = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      APPEND LINES OF v_interf_impl_tab TO v_subclass_tab.

    ENDIF.

    IF v_subclass_tab[] IS INITIAL. RETURN. ENDIF.

    DATA(v_search_token_regex) =  |#{ i_entity_name }$| .


*-- GET class description text in EN
    SELECT clsname,  descript    FROM vseoclass
     INTO TABLE @DATA(v_subclass_text_tab)
     FOR ALL ENTRIES IN @v_subclass_tab
      WHERE  clsname  EQ @v_subclass_tab-clsname
       AND langu      EQ 'E'
       AND version    EQ 1."#EC CI_SUBRC


*-- Convert class description to upper case before searching for Entity Token

    LOOP AT v_subclass_text_tab ASSIGNING FIELD-SYMBOL(<fs_subclass>).

      IF find( val  = <fs_subclass>-descript
                                             regex = v_search_token_regex
                                             case  = abap_false ) NE -1 .
        DATA(v_entity_impl) = <fs_subclass>-clsname.
        EXIT.
      ENDIF.

*      IF to_upper(  <fs_subclass>-descript )  CS v_search_token_regex.
*        DATA(v_entity_impl) = <fs_subclass>-clsname.
*      ENDIF.

    ENDLOOP.

    r_result = zcl_odata_entity_factory=>create_interface_impl( v_entity_impl ).


  ENDMETHOD.


  METHOD get_external_company.

    IF zcl_odata_entity_factory=>external_company_prototype IS BOUND.

      r_result =  zcl_odata_entity_factory=>external_company_prototype->clone(  i_company_id   ).

    ENDIF.

  ENDMETHOD.


  METHOD get_external_user.

***************************************************************************
* in a session context, we don't have to load again the data, becasue the should be loaded already
***************************************************************************

    IF zcl_odata_entity_factory=>external_user_clone IS BOUND.
      r_result = zcl_odata_entity_factory=>external_user_clone.
      RETURN.
    ENDIF.

    IF zcl_odata_entity_factory=>external_user_prototype IS BOUND.

      zcl_odata_entity_factory=>external_user_clone =  zcl_odata_entity_factory=>external_user_prototype->clone( i_user_id ).

      r_result = zcl_odata_entity_factory=>external_user_clone.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
