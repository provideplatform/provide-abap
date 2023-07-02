CLASS zcl_prvd_nchain_network DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_prvd_nchain_network .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_prvd_nchain_network IMPLEMENTATION.


  METHOD zif_prvd_nchain_network~create.
    DATA: ls_nchain_network TYPE zprvdnchainntwrk.
    ls_nchain_network-mandt = sy-mandt.
    ls_nchain_network-network_id = is_prvd_nchain_network-network_id.
    ls_nchain_network-network_type = is_prvd_nchain_network-network_type.
    ls_nchain_network-description = is_prvd_nchain_network-description.
    MODIFY zprvdnchainntwrk FROM ls_nchain_network.
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.


  METHOD zif_prvd_nchain_network~delete.
  ENDMETHOD.


  METHOD zif_prvd_nchain_network~query.
  ENDMETHOD.


  METHOD zif_prvd_nchain_network~read.
  ENDMETHOD.


  METHOD zif_prvd_nchain_network~update.
      DATA: ls_nchain_network TYPE zprvdnchainntwrk.
    ls_nchain_network-mandt = sy-mandt.
    ls_nchain_network-network_id = is_prvd_nchain_network-network_id.
    ls_nchain_network-network_type = is_prvd_nchain_network-network_type.
    ls_nchain_network-description = is_prvd_nchain_network-description.
    MODIFY zprvdnchainntwrk FROM ls_nchain_network.
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
