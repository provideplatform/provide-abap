INTERFACE zif_prvd_nchain_solidity_types
  PUBLIC .

  "uint      Digits  Max value
*-----------------------------
*uint8       3       255
*uint16      5       65,535
*uint24      8       16,777,215
*uint32      10      4,294,967,295
*uint40      13      1,099,511,627,775
*uint48      15      281,474,976,710,655
*uint56      17      72,057,594,037,927,935
*uint64      20      18,446,744,073,709,551,615
*uint72      22      4,722,366,482,869,645,213,695
*uint80      25      1,208,925,819,614,629,174,706,175
*uint88      27      309,485,009,821,345,068,724,781,055
*uint96      29      79,228,162,514,264,337,593,543,950,335
*...
*uint128     39      340,282,366,920,938,463,463,374,607,431,768,211,455
*...
*uint256     78      115,792,089,237,316,195,423,570,985,008,687,907,853,269,984,665,640,564,039,457,584,007,913,129,639,935

  "! Method change solidity uint8 to a char3 type
  METHODS: uint8_to_char3 IMPORTING iv_uint8_data TYPE REF TO data
                          EXPORTING ev_char3      TYPE char3.
  "! Method change solidity uint16 to a char5 type
  METHODS: uint16_to_char5 IMPORTING iv_uint16_data TYPE REF TO data
                           EXPORTING ev_char5       TYPE char5.
  "! Method change solidity uint24 to a char8 type
  METHODS: uint24_to_char8 IMPORTING iv_uint24_data TYPE REF TO data
                           EXPORTING ev_char8       TYPE char8.
  "! Method change solidity uint32 to a char10 type
  METHODS: uint32_to_char10 IMPORTING iv_uint8_data TYPE REF TO data
                            EXPORTING ev_char10     TYPE char10.
  "! Method change solidity uint40 to a char13 type
  METHODS: uint40_to_char3 IMPORTING iv_uint8_data TYPE REF TO data
                           EXPORTING ev_char13     TYPE char13.
  "! Method change solidity uint40 to a char15 type
  METHODS: uint48_to_char15 IMPORTING iv_uint8_data TYPE REF TO data
                           EXPORTING ev_char15     TYPE char15.
  "! Method change solidity uint56 to a char17 type
  METHODS: uint56_to_char17 IMPORTING iv_uint8_data TYPE REF TO data
                           EXPORTING ev_char17     TYPE char17.
  "! Method change solidity uint64 to a char20 type
  METHODS: uint64_to_char20 IMPORTING iv_uint8_data TYPE REF TO data
                           EXPORTING ev_char20     TYPE char20.
  "! Method change solidity uint72 to a char22 type
  METHODS: uint72_to_char22 IMPORTING iv_uint8_data TYPE REF TO data
                           EXPORTING ev_char22     TYPE char22.
  "! Method change solidity uint80 to a char25 type
  METHODS: uint80_to_char25 IMPORTING iv_uint8_data TYPE REF TO data
                           EXPORTING ev_char25     TYPE char25.
  "! Method change solidity uint88 to a char27 type
  METHODS: uint88_to_char27 IMPORTING iv_uint8_data TYPE REF TO data
                           EXPORTING ev_char27     TYPE char27.
  "! Method change solidity uint96 to a char29 type
  METHODS: uint96_to_char29 IMPORTING iv_uint8_data TYPE REF TO data
                           EXPORTING ev_char29     TYPE char29.
  "! Method change solidity uint128 to a char40 type
  METHODS: uint128_to_char40 IMPORTING iv_uint8_data TYPE REF TO data
                            EXPORTING ev_char40     TYPE char40.
  "! Method change solidity uint256 to a char80 type
  METHODS: uint256_to_char80 IMPORTING iv_uint8_data TYPE REF TO data
                            EXPORTING ev_char80     TYPE char80.





ENDINTERFACE.
