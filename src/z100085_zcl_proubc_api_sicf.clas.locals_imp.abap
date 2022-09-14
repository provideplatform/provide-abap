*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

"todo add error msg handling
CLASS lcl_errormsg DEFINITION.
ENDCLASS.

CLASS lcl_errormsg IMPLEMENTATION.
ENDCLASS.

CLASS lcl_default_handler DEFINITION INHERITING FROM cl_rest_resource.
  PUBLIC SECTION.
    METHODS: if_rest_resource~get REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_default_handler IMPLEMENTATION.
  METHOD if_rest_resource~get.

  ENDMETHOD.
ENDCLASS.
