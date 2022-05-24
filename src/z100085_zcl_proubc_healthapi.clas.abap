CLASS z100085_zcl_proubc_healthapi DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: if_rest_resource~get REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z100085_zcl_proubc_healthapi IMPLEMENTATION.
  METHOD if_rest_resource~get.
    mo_response->set_status( cl_rest_status_code=>gc_success_no_content ).
  ENDMETHOD.
ENDCLASS.
