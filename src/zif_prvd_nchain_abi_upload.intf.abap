INTERFACE zif_prvd_nchain_abi_upload
  PUBLIC .


  TYPES:
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
  TYPES:
    tty_abi_registry TYPE STANDARD TABLE OF ty_abi_registry .

  "! Uploads an ABI file into a AL11 directory
  METHODS UPLOAD_ABI_FILE .
  "! Creates a table entry to ZPRVDABIREGISTRY, indexing the smart contract/network id to the file
  METHODS register_abi_file
    EXPORTING
      !ev_success TYPE abap_bool .
  "! Clears the ABI file directory assignment to ZPRVDABIREGISTRY
  METHODS unregister_abi_file
    EXPORTING
      !ev_success TYPE abap_bool .
  "! Deletes the specific ABI file from AL11
  METHODS delete_file
    EXPORTING
      !ev_success TYPE abap_bool .
  "! Deletes an entry from ZPRVDABIREGISTRY. Note that AL11 file deletion is handled elsewhere
  METHODS delete_registry
    EXPORTING
      !ev_success TYPE abap_bool .
  "! Sets a valid to date for the ABI registry table entry (enables timelocked go-live)
  METHODS UPDATE_VALIDTO .
  "! Sets a valid from date for the ABI registry table entry (enables timelocked go-live)
  METHODS UPDATE_VALIDFROM .
  "! Validates an incoming or current entry to the ZPRVDABIREGISTRY table
  METHODS VALIDATE_REGISTRY .
  "! Selects a default ALL path to which ABI files will be stored
  METHODS SET_AL11_ABI_FILE_PATH .
ENDINTERFACE.
