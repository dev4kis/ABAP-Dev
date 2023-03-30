INTERFACE zif_crm_address
  PUBLIC.

  TYPES ty_general_data    TYPE bapibus1006_address.

  METHODS get_general_data
    RETURNING VALUE(r_result) TYPE ty_general_data.


ENDINTERFACE.
