CLASS zcl_prvd_json DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .
    CLASS cl_abap_tstmp DEFINITION LOAD .
    CLASS cx_sy_conversion_error DEFINITION LOAD .

    TYPES json TYPE string .
    TYPES:
      BEGIN OF name_mapping,
        abap TYPE abap_compname,
        json TYPE string,
      END OF name_mapping .
    TYPES:
      name_mappings TYPE HASHED TABLE OF name_mapping WITH UNIQUE KEY abap .
    TYPES bool TYPE char1 .
    TYPES tribool TYPE char1 .
    TYPES pretty_name_mode TYPE char1 .

    CONSTANTS:
      BEGIN OF pretty_mode,
        none          TYPE char1  VALUE ``,
        low_case      TYPE char1  VALUE `L`,
        camel_case    TYPE char1  VALUE `X`,
        extended      TYPE char1  VALUE `Y`,
        user          TYPE char1  VALUE `U`,
        user_low_case TYPE char1  VALUE `C`,
      END OF  pretty_mode .
    CONSTANTS:
      BEGIN OF c_bool,
        true  TYPE bool  VALUE `X`,
        false TYPE bool  VALUE ``,
      END OF  c_bool .
    CONSTANTS:
      BEGIN OF c_tribool,
        true      TYPE tribool  VALUE c_bool-true,
        false     TYPE tribool  VALUE `-`,
        undefined TYPE tribool  VALUE ``,
      END OF  c_tribool .

    CLASS-METHODS:
      serialize     IMPORTING
                      !data             TYPE data
                      !compress         TYPE bool DEFAULT c_bool-false
                      !name             TYPE string OPTIONAL
                      !pretty_name      TYPE pretty_name_mode DEFAULT pretty_mode-none
                      !type_descr       TYPE REF TO cl_abap_typedescr OPTIONAL
                      !assoc_arrays     TYPE bool DEFAULT c_bool-false
                      !ts_as_iso8601    TYPE bool DEFAULT c_bool-false
                      !expand_includes  TYPE bool DEFAULT c_bool-true
                      !assoc_arrays_opt TYPE bool DEFAULT c_bool-false
                      !numc_as_string   TYPE bool DEFAULT c_bool-false
                      !name_mappings    TYPE name_mappings OPTIONAL
                      !conversion_exits TYPE bool DEFAULT c_bool-false
                    RETURNING
                      VALUE(r_json)     TYPE json ,
      deserialize.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_prvd_json IMPLEMENTATION.
  METHOD serialize.
  ENDMETHOD.
  METHOD deserialize.
  ENDMETHOD.
ENDCLASS.
