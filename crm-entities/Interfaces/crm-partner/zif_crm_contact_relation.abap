interface ZIF_CRM_CONTACT_RELATION
  public .


  data FROM_PARTNER type ref to ZIF_CRM_PARTNER .
  data TO_PARTNER type BU_PARTNER .
  data ADDRESS type ref to ZIF_CRM_ADDRESS .

  methods GET_STANDARD_ADDRESS
    returning
      value(R_ADDRESS) type ref to ZIF_CRM_CONT_ADDRESS .
endinterface.
