interface ZIF_PROUBC_NCHAIN_ABI_UPLOAD
  public .


  types:
    BEGIN OF ty_abi_registry,
           nchain_networkid      TYPE zproubc_smartcontract_addr,
           smartcontract_address TYPE zproubc_smartcontract_addr,
           valid_from            TYPE timestampl,
           valid_to              TYPE timestampl,
           abi_location          TYPE zcasesensitive_str,
           created_by            TYPE uname,
           created_on            TYPE timestampl,
           changed_by            TYPE uname,
           changed_on            TYPE timestampl,
           production_mainnet    TYPE char1,
         END OF ty_abi_registry .
  types:
    tty_abi_registry TYPE STANDARD TABLE OF ty_abi_registry .

  methods UPLOAD_ABI_FILE .
  methods VERIFY_ABI_FILE .
  methods REGISTER_ABI_FILE
    exporting
      !EV_SUCCESS type ABAP_BOOL .
  methods UNREGISTER_ABI_FILE
    exporting
      !EV_SUCCESS type ABAP_BOOL .
  methods DELETE_FILE
    exporting
      !EV_SUCCESS type ABAP_BOOL .
  methods DELETE_REGISTRY
    exporting
      !EV_SUCCESS type ABAP_BOOL .
  methods UPDATE_VALIDTO .
  methods UPDATE_VALIDFROM .
  methods VALIDATE_REGISTRY .
endinterface.
