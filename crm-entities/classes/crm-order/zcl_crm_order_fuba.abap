class ZCL_CRM_ORDER_FUBA definition
  public
  inheriting from ZCL_CRM_BASE_ORDER
  final
  create public .

public section.

  methods ZIF_CRM_ENTITY~LOAD_AS
    redefinition .
  methods ZIF_CRM_ORDER~GET_PARTNER_LIST
    redefinition .
protected section.
private section.

  data PARTNER type CRMT_PARTNER_EXTERNAL_WRKT .
  data ORDERADM type CRMT_ORDERADM_H_WRKT .

  methods GET_BY_KEY
    importing
      !I_OBJECT_KEY type ANY .
  methods MAP_PARTNER_TO_PARTNER_GUID
    importing
      !I_PARTNER type BU_PARTNER
    returning
      value(R_PARTNER_GUID) type BU_PARTNER_GUID .
  methods MAP_PARTNER_GUID_TO_PARTNER
    importing
      !I_PARTNER_GUID type BU_PARTNER_GUID
    returning
      value(R_PARTNER) type BU_PARTNER .
ENDCLASS.



CLASS ZCL_CRM_ORDER_FUBA IMPLEMENTATION.


  method GET_BY_KEY.

    INCLUDE: crm_object_names_con.

    DATA: v_object_guid       TYPE crmt_object_guid,
          v_object_guids      TYPE crmt_object_guid_tab,
          v_requested_objects TYPE crmt_object_name_tab.


    v_object_guid = i_object_key.


    v_requested_objects = VALUE #( ( gc_object_name-orderadm_h )
                                    ( gc_object_name-partner    )
                                  ).


  append v_object_guid to v_object_guids.

  CALL FUNCTION 'CRM_ORDER_READ'
    EXPORTING
      it_header_guid       = v_object_guids
      it_requested_objects = v_requested_objects
    IMPORTING
      et_orderadm_h        = me->orderadm
      et_partner           = me->partner
    EXCEPTIONS
      document_not_found   = 1
      error_occurred       = 2
      document_locked      = 3
      no_change_authority  = 4
      no_display_authority = 5
      no_change_allowed    = 6
      OTHERS               = 7.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.



  endmethod.


  METHOD map_partner_guid_to_partner.
    SELECT SINGLE partner
     INTO r_partner
     FROM   but000
     WHERE partner = i_partner_guid.
  ENDMETHOD.


  METHOD map_partner_to_partner_guid.
    DATA: v_partner TYPE bu_partner.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_partner
      IMPORTING
        output = v_partner.


    SELECT SINGLE partner_guid
     INTO r_partner_guid
     FROM   but000
     WHERE partner = v_partner.
  ENDMETHOD.


  method ZIF_CRM_ENTITY~LOAD_AS.
    super->zif_crm_entity~load_as( i_entity_id = i_entity_id ).
     me->get_by_key( i_object_key = i_entity_id ).
  endmethod.


  method ZIF_CRM_ORDER~GET_PARTNER_LIST.

    LOOP AT me->partner  ASSIGNING FIELD-SYMBOL(<fs_partner>).

          DATA(partner_id) =  <fs_partner>-PARTNER_NO .

          DATA(v_data_type) = cl_abap_typedescr=>describe_by_data( partner_id )->get_relative_name( ).

          DATA(v_partner_guid) = SWITCH bu_partner_guid( v_data_type
                    WHEN 'BU_PARTNER_GUID' THEN partner_id
                    WHEN 'BU_PARTNER' THEN me->map_partner_to_partner_guid( conv bu_partner( partner_id ) )
                    ELSE  partner_id ).

          data(v_partner_id) = SWITCH bu_partner( v_data_type
                    WHEN 'BU_PARTNER_GUID' THEN me->map_partner_guid_to_partner( conv bu_partner_guid( partner_id ) )
                    WHEN 'BU_PARTNER' THEN partner_id
                    ELSE  partner_id ).


          APPEND  VALUE #( partner = v_partner_id
"                           partner_ref  = NEW zcl_crm_entity_api( i_access_method = '2' )->partner->read( i_partner_id = v_partner_guid )
                           partner_fct  = <fs_partner>-PARTNER_FCT
                           partner_guid = v_partner_guid
                           main        = <fs_partner>-MAINPARTNER
                           )        TO r_result.


    ENDLOOP.



  endmethod.
ENDCLASS.
