INTERFACE zif_prvd_privacy
  PUBLIC .


  TYPES:
"! Component schema: CreatecircuitRequest, object
    BEGIN OF ty_createcircuitrequest,
           iv_identifier TYPE string,
           iv_proving_scheme TYPE string,
           iv_curve TYPE string,
           iv_provider TYPE string,
           iv_name TYPE string,
         END OF ty_createcircuitrequest .
  TYPES:
"! Component schema: ProveRequest, object
    BEGIN OF ty_proverequest,
          iv_identifier TYPE string,
          iv_proving_scheme TYPE string,
          iv_curve TYPE string,
          iv_provider TYPE string,
          iv_name TYPE string,
         END OF ty_proverequest .
  TYPES:
"! Component schema: Witness, object
    BEGIN OF ty_witness,
           iv_x TYPE string,
           iv_y TYPE string,
         END OF ty_witness .
  TYPES:
"! Component schema: VerifyRequest, object
    BEGIN OF ty_verifyrequest,
           is_witness TYPE ty_witness,
           iv_proof TYPE string,
         END OF ty_verifyrequest .

"! GET - "List circuits"
"! Operation id: Listcircuits
"! Response: 200
  METHODS listcircuits
    RAISING
      cx_static_check .
"! POST - "Create circuit"
"! Operation id: Createcircuit
"! Response: 200
"! Body ref: #/components/schemas/CreatecircuitRequest
  METHODS createcircuit
    IMPORTING
      !is_body TYPE ty_createcircuitrequest
    RAISING
      cx_static_check .
"! POST - "Verify"
"! Operation id: Verify
"! Parameter: circuit_id, required, path
"! Response: 200
"! Body ref: #/components/schemas/VerifyRequest
  METHODS verify
    IMPORTING
      !iv_circuit_id TYPE string
      !is_body TYPE ty_verifyrequest
    RAISING
      cx_static_check .
ENDINTERFACE.
