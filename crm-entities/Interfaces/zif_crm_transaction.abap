interface ZIF_CRM_TRANSACTION
  public .


  methods SAVE
    exceptions
      ZCX_CRM_APP .
  methods COMMIT .
  methods GET_TRANSACTION
    returning
      value(R_RESULT) type ref to CL_CRM_BOL_CUSTOM_TX_CTXT .
  methods ROLLBACK .
endinterface.
