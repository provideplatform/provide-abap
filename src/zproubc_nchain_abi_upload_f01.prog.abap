*&---------------------------------------------------------------------*
*& Include zproubc_nchain_abi_upload_f01
*&---------------------------------------------------------------------*

CLASS lcl_proubc_nchain_abi_upload IMPLEMENTATION.
  METHOD get_data.
  ENDMETHOD.
  METHOD refresh_data.
  ENDMETHOD.
  METHOD save_data.
    me->zif_proubc_nchain_abi_upload~upload_abi_file( ).
    me->zif_proubc_nchain_abi_upload~register_abi_file(  ).
  ENDMETHOD.
  METHOD delete_abi.
    me->zif_proubc_nchain_abi_upload~delete_file( ).
    me->zif_proubc_nchain_abi_upload~delete_registry( ).
  ENDMETHOD.
  METHOD preview_abi.
    CASE iv_previewtype.
      WHEN 'D'.
      WHEN 'S'.
    ENDCASE.
  ENDMETHOD.
  METHOD new_abi.
  ENDMETHOD.
  METHOD load_abi.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~upload_abi_file.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~verify_abi_file.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~register_abi_file.
    DATA: ls_existingentry TYPE zprvdabiregistry,
          ls_stagingentry  TYPE zprvdabiregistry.
    "validate entry
    SELECT SINGLE * FROM zprvdabiregistry
     INTO ls_existingentry
     WHERE nchain_networkid = ls_currententry-nchain_networkid
                                         AND smartcontract_address = ls_currententry-smartcontract_address.
    IF sy-subrc = 0.
      ls_stagingentry-nchain_networkid = ls_existingentry-nchain_networkid.
      ls_stagingentry-smartcontract_address = ls_existingentry-smartcontract_address.
      ls_stagingentry-changed_by = sy-uname.
      GET TIME STAMP FIELD ls_stagingentry-changed_on.
    ELSEIF sy-subrc = 4.
      ls_stagingentry-nchain_networkid = ls_currententry-nchain_networkid.
      ls_stagingentry-smartcontract_address = ls_currententry-smartcontract_address.
      ls_stagingentry-created_by = sy-uname.
      GET TIME STAMP FIELD ls_stagingentry-created_on.
    ELSE.
      "todo error handling/logging
    ENDIF.

    ls_stagingentry-valid_from = ls_currententry-valid_from.
    ls_stagingentry-valid_to = ls_currententry-valid_to.
    ls_stagingentry-abi_location = ls_currententry-abi_location.
    ls_stagingentry-production_mainnet = ls_currententry-production_mainnet.

    APPEND ls_stagingentry TO lt_update_staging.

    IF lt_update_staging IS NOT INITIAL.
      MODIFY zprvdabiregistry FROM TABLE lt_update_staging.
      IF sy-subrc = 0.
        ev_success = abap_true.
      ELSE.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~unregister_abi_file.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~delete_file.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~delete_registry.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~update_validto.
  ENDMETHOD.
  METHOD zif_proubc_nchain_abi_upload~update_validfrom.
  ENDMETHOD.
ENDCLASS.

DATA: lo_proubc_nchain_abi_upload TYPE REF TO lcl_proubc_nchain_abi_upload.
