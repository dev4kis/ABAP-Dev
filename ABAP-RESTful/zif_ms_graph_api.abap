INTERFACE zif_ms_graph_api
  PUBLIC .


  TYPES:
    BEGIN OF ty_az_graph_user,
      id                TYPE string,
      displayname       TYPE string,
      givenname         TYPE string,
      surname           TYPE string,
      mail              TYPE string,
      userprincipalname TYPE string,
      jobtitle          TYPE string,
      mobilephone       TYPE string,
      officelocation    TYPE string,
      preferredlanguage TYPE string,
      businessphones    TYPE string,
    END OF ty_az_graph_user,
    BEGIN OF ty_az_graph_group,
      id          TYPE string,
      displayname TYPE string,
    END OF ty_az_graph_group,

    ty_az_graph_user_tt  TYPE STANDARD TABLE OF ty_az_graph_user WITH KEY id,
    ty_az_graph_group_tt TYPE STANDARD TABLE OF ty_az_graph_group WITH KEY id,
    BEGIN OF ty_az_graph_api_user,
      _odata_context TYPE string,
      value          TYPE ty_az_graph_user_tt,
    END OF ty_az_graph_api_user,
    BEGIN OF ty_az_graph_api_grp,
      _odata_context TYPE string,
      value          TYPE ty_az_graph_group_tt,
    END OF ty_az_graph_api_grp.

  METHODS create_user
    IMPORTING
              !i_user_details      TYPE any OPTIONAL
              !i_user_details_json TYPE /ui2/cl_json=>json OPTIONAL
              !i_user_details_x    TYPE xstring OPTIONAL
                PREFERRED PARAMETER i_user_details_x
    RETURNING VALUE(r_aad_user)    TYPE ty_az_graph_user
    RAISING   zcx_graph_api_exception .
  METHODS get_user
    IMPORTING
              !i_user_id              TYPE any OPTIONAL
              !i_requested_attributes TYPE stringtab OPTIONAL
    EXPORTING
              !e_json_data            TYPE  /ui2/cl_json=>json
              !e_user_attributes_ext  TYPE any
    RETURNING
              VALUE(r_az_b2c_user)    TYPE ty_az_graph_user
    RAISING   zcx_graph_api_exception .
  METHODS delete_user
    IMPORTING
              !i_user_id TYPE any
    RAISING   zcx_graph_api_exception .
  METHODS update_user
    IMPORTING
              !i_user_id         TYPE any
              !i_user_attributes TYPE any
    RAISING   zcx_graph_api_exception .
  METHODS add_user_to_group
    IMPORTING
              !i_user_id TYPE any
              !i_grp     TYPE any
    RAISING   zcx_graph_api_exception .
  METHODS get_user_memership
    IMPORTING
              !i_user_id           TYPE any
    RETURNING
              VALUE(rt_membership) TYPE ty_az_graph_group_tt
    RAISING   zcx_graph_api_exception .
  METHODS remove_user_from_group
    IMPORTING
              !i_user_id TYPE any
              !i_grp     TYPE any
    RAISING   zcx_graph_api_exception .

  CONSTANTS: BEGIN OF co_graph_uri_prefix,
               user TYPE string VALUE '/users',
             END OF co_graph_uri_prefix.

ENDINTERFACE.
