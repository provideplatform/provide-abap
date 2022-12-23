INTERFACE zif_prvd_nchain_solidity_types PUBLIC .
  TYPES: BEGIN OF ty_soliditymapping,
          soliditytype TYPE string,
          abapddictype TYPE string,
         END OF ty_soliditymapping.

  "!Changes raw solidity data to appropriate ABAP ddic type
  METHODS: convert_solidity_to_abap IMPORTING iv_rawvalue TYPE string
                                             is_mapping TYPE ty_soliditymapping.
ENDINTERFACE.
