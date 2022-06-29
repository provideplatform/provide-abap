class ZCL_PROUBC_AUTHAPI definition
  public
  inheriting from CL_REST_RESOURCE
  final
  create public .

public section.

  methods IF_REST_RESOURCE~HEAD
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PROUBC_AUTHAPI IMPLEMENTATION.


  METHOD IF_REST_RESOURCE~HEAD.
    mo_response->set_status( cl_rest_status_code=>gc_success_no_content ).
  ENDMETHOD.
ENDCLASS.
