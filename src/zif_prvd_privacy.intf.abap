interface ZIF_PRVD_PRIVACY
  public .


  types:
"! Component schema: CreatecircuitRequest, object
    BEGIN OF ty_createcircuitrequest,
           iv_identifier TYPE string,
           iv_proving_scheme TYPE string,
           iv_curve TYPE string,
           iv_provider TYPE string,
           iv_name TYPE string,
         END OF ty_createcircuitrequest .
  types:
"! Component schema: ProveRequest, object
    BEGIN OF ty_proverequest,
          iv_identifier TYPE string,
          iv_proving_scheme TYPE string,
          iv_curve TYPE string,
          iv_provider TYPE string,
          iv_name TYPE string,
         END OF ty_proverequest .
  types:
"! Component schema: Witness, object
    BEGIN OF ty_witness,
           iv_x TYPE string,
           iv_y TYPE string,
         END OF ty_witness .
  types:
"! Component schema: VerifyRequest, object
    BEGIN OF ty_verifyrequest,
           is_witness TYPE ty_witness,
           iv_proof TYPE string,
         END OF ty_verifyrequest .

"! GET - "List circuits"
"! Operation id: Listcircuits
"! Response: 200
  methods LISTCIRCUITS
    raising
      CX_STATIC_CHECK .
"! POST - "Create circuit"
"! Operation id: Createcircuit
"! Response: 200
"! Body ref: #/components/schemas/CreatecircuitRequest
  methods CREATECIRCUIT
    importing
      !IS_BODY type TY_CREATECIRCUITREQUEST
    raising
      CX_STATIC_CHECK .
"! POST - "Verify"
"! Operation id: Verify
"! Parameter: circuit_id, required, path
"! Response: 200
"! Body ref: #/components/schemas/VerifyRequest
  methods VERIFY
    importing
      !IV_CIRCUIT_ID type STRING
      !IS_BODY type TY_VERIFYREQUEST
    raising
      CX_STATIC_CHECK .
endinterface.
