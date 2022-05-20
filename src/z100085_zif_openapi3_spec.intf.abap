INTERFACE z100085_zif_openapi3_spec
  PUBLIC .

  "based upon version 3.0.3 https://swagger.io/specification
  "development still in progress

  TYPES: BEGIN OF ty_contact,
           name  TYPE string,
           url   TYPE string,
           email TYPE string,
         END OF ty_contact.

  TYPES: BEGIN OF ty_license,
           name TYPE string,
           url  TYPE string,
         END OF ty_license.

  TYPES: BEGIN OF ty_info,
           title          TYPE string, "!REQUIRED
           description    TYPE string,
           termsOfService TYPE string,
           contact        TYPE ty_contact,
           license        TYPE ty_license,
           version        TYPE string, "!REQUIRED
         END OF ty_info.

  TYPES: BEGIN OF ty_server,
           url         TYPE string,
           description TYPE string,
           variables   TYPE REF TO data,
         END OF ty_server.

  TYPES: tty_servers TYPE TABLE OF ty_server.

  TYPES: BEGIN OF ty_paths_object,
           uri TYPE string,
           "TODO fill in rest of HTTP verbs
         END OF ty_paths_object.

  TYPES: BEGIN OF ty_paths,
           pathobjects TYPE STANDARD TABLE OF ty_paths_object WITH NON-UNIQUE DEFAULT KEY,
         END OF ty_paths.

  "! https://swagger.io/specification/#security-requirement-object
  TYPES: BEGIN OF ty_security_reqobj,
            name type string,
            value type STANDARD TABLE OF string with non-unique DEFAULT KEY,
         END OF ty_security_reqobj.

  TYPES: BEGIN OF ty_path_operation,
           tags         TYPE STANDARD TABLE OF string WITH NON-UNIQUE DEFAULT KEY,
           summary      TYPE string,
           description  TYPE string,
           externalDocs TYPE REF TO data, "TODO create the obj
           operationId  TYPE string,
           parameters   TYPE REF TO data, "TODO create the obj
           requestBody  TYPE REF TO data, "TODO create the obj
           responses    TYPE REF TO data, "TODO create the obj
           callbacks    TYPE REF TO data, "TODO create the obj
           deprecated   TYPE bool,
           security     TYPE STANDARD TABLE OF ty_security_reqobj WITH NON-UNIQUE DEFAULT KEY,
           servers      TYPE standard TABLE OF ty_server with non-unique DEFAULT KEY,
         END OF ty_path_operation.

  TYPES: BEGIN OF ty_path_item,
           ref         TYPE string, "must map to $ref
           summary     TYPE string,
           description TYPE string,
           get         TYPE ty_path_operation,
           put         TYPE ty_path_operation,
           delete      TYPE ty_path_operation,
           options     TYPE ty_path_operation,
         END OF ty_path_item.

*  TYPES: BEGIN OF ty_components,
*         END OF ty_components.

*  TYPES: BEGIN OF ty_security,
*         END OF ty_security.

*  TYPES: BEGIN OF ty_tags,
*         END OF ty_tags.

*  TYPES: BEGIN OF ty_externaldocs,
*         END OF ty_externaldocs.

  TYPES: BEGIN OF ty_fixedfields,
           openapi      TYPE string, "!REQUIRED
           info         TYPE ty_info,  "!REQUIRED
           "servers      TYPE tty_servers,
           paths        TYPE ty_paths, "!REQUIRED
           "components   TYPE ty_components,
           "security     TYPE ty_security,
           "tags         TYPE ty_tags,
           "externalDocs TYPE ty_externaldocs,
         END OF ty_fixedfields.

  TYPES: BEGIN OF ty_map_w_conversion,
           mapkey                TYPE string,
           mapkeyalphaconversion TYPE string,
           mapdata               TYPE REF TO data,
           mapdatatype           TYPE string,
         END OF ty_map_w_conversion.


ENDINTERFACE.
