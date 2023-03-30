INTERFACE zif_ldap_access_api
  PUBLIC .

  METHODS read_user_attributes
    IMPORTING
              !i_attributes       TYPE ldapastab OPTIONAL
              !i_scope            TYPE ldap_scop OPTIONAL
*              !i_base_dns         TYPE ldap_dns
              !i_filter_string    TYPE ldap_filts
    RETURNING
              VALUE(r_attributes) TYPE ldapetab
    RAISING   zcx_ldap_access .
  METHODS create_user
    IMPORTING
              !i_ldap_data TYPE ldape
    RETURNING
              VALUE(rc)    TYPE ldap_rc
    RAISING   zcx_ldap_access .
  METHODS delete_user
    IMPORTING
              !i_distinguished_name TYPE ldap_dn
    RETURNING
              VALUE(rc)             TYPE ldap_rc
    RAISING   zcx_ldap_access.
  METHODS update_attributes
    IMPORTING
              !i_ldap_data2change TYPE ldape
    RETURNING
              VALUE(rc)           TYPE ldap_rc
    RAISING   zcx_ldap_access .
  METHODS remove_attribute .
  METHODS add_attribute .
*  methods CONNECT
*    returning
*      value(RC) type LDAP_RC .
  METHODS disconnect
    RETURNING
              VALUE(rc) TYPE ldap_rc
    RAISING   zcx_ldap_access .
ENDINTERFACE.
