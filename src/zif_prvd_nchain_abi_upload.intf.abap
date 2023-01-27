interface ZIF_PRVD_NCHAIN_ABI_UPLOAD
  public .


  types:
    BEGIN OF ty_abi_registry,
           nchain_networkid      TYPE zprvd_smartcontract_addr,
           smartcontract_address TYPE zprvd_smartcontract_addr,
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

  "! Uploads an ABI file into a AL11 directory
  methods UPLOAD_ABI_FILE .
  "! Creates a table entry to ZPRVDABIREGISTRY, indexing the smart contract/network id to the file
  methods REGISTER_ABI_FILE
    exporting
      !EV_SUCCESS type ABAP_BOOL .
  "! Clears the ABI file directory assignment to ZPRVDABIREGISTRY
  methods UNREGISTER_ABI_FILE
    exporting
      !EV_SUCCESS type ABAP_BOOL .
  "! Deletes the specific ABI file from AL11
  methods DELETE_FILE
    exporting
      !EV_SUCCESS type ABAP_BOOL .
  "! Deletes an entry from ZPRVDABIREGISTRY. Note that AL11 file deletion is handled elsewhere
  methods DELETE_REGISTRY
    exporting
      !EV_SUCCESS type ABAP_BOOL .
  "! Sets a valid to date for the ABI registry table entry (enables timelocked go-live)
  methods UPDATE_VALIDTO .
  "! Sets a valid from date for the ABI registry table entry (enables timelocked go-live)
  methods UPDATE_VALIDFROM .
  "! Validates an incoming or current entry to the ZPRVDABIREGISTRY table
  methods VALIDATE_REGISTRY .
  "! Selects a default ALL path to which ABI files will be stored
  methods SET_AL11_ABI_FILE_PATH .
endinterface.
