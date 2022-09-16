INTERFACE zif_proubc_nchain_abi_upload
  PUBLIC .

  TYPES: BEGIN OF ty_abi_registry,
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
         END OF ty_abi_registry.

  TYPES: tty_abi_registry TYPE STANDARD TABLE OF ty_abi_registry.

  METHODS:  upload_abi_file,
    verify_abi_file,
    register_abi_file EXPORTING ev_success type abap_bool,
    unregister_abi_file EXPORTING ev_success type abap_bool,
    delete_file EXPORTING ev_success type abap_bool,
    delete_registry EXPORTING ev_success type abap_bool,
    update_validto,
    update_validfrom.

ENDINTERFACE.
