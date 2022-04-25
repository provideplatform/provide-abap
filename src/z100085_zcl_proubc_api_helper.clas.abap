CLASS z100085_zcl_proubc_api_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      map_data_to_tenant IMPORTING iv_data    TYPE REF TO data
                         EXPORTING et_tenants TYPE z100085_prvdorgs.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_api_helper IMPLEMENTATION.
  METHOD map_data_to_tenant.
    DATA: ls_tenant TYPE z100085_prvdorgs.
    "Todo this is dumping the ABAP code, need it fixed to map the deserialized data into the ABAP structure for populating db
    DATA(lo_structdescr) = CAST cl_abap_structdescr(  cl_abap_structdescr=>describe_by_data( p_data = iv_data ) ).
    DATA(components)     = lo_structdescr->get_components( ).
    LOOP AT components ASSIGNING FIELD-SYMBOL(<fs_component>).
    ENDLOOP.

    "loop at iv_data assigning field-symbol(<fs_data>).
    "endloop.
    "assign iv_data->* to et_tenants.
    "assign component '' of structure iv_data to ls_tenant-prvdorgid.
  ENDMETHOD.
ENDCLASS.
