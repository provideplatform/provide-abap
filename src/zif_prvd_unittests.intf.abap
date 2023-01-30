interface ZIF_PRVD_UNITTESTS
  public .


  "! Ident reachability
  methods IS_IDENT_REACHABLE
    returning
      value(RV_RESULT) type ABAP_BOOL .
  "! Ident authentication
  methods IS_IDENT_AUTH_OK
    returning
      value(RV_RESULT) type ABAP_BOOL .
  "! BPI reachability
  methods IS_BPI_REACHABLE
    returning
      value(RV_RESULT) type ABAP_BOOL .
  "! Sending a protocol message
  methods IS_PROTOCOL_MESSAGE_OK
    returning
      value(RV_RESULT) type ABAP_BOOL .
  "! general http expected check
  methods IS_EXPECTED_HTTP_STATUS
    importing
      !I_EXPECTED_STAT type I
    returning
      value(RV_RESULT) type ABAP_BOOL .
endinterface.
