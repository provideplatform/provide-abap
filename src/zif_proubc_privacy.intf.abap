INTERFACE zif_proubc_privacy PUBLIC.

* Component schema: CreatecircuitRequest, object
  TYPES: BEGIN OF ty_createcircuitrequest,
           identifier TYPE string,
           proving_scheme TYPE string,
           curve TYPE string,
           provider TYPE string,
           name TYPE string,
         END OF ty_createcircuitrequest.

* Component schema: ProveRequest, object
  TYPES: BEGIN OF ty_proverequest,
           identifier TYPE string,
           proving_scheme TYPE string,
           curve TYPE string,
           provider TYPE string,
           name TYPE string,
         END OF ty_proverequest.

* Component schema: Witness, object
  TYPES: BEGIN OF ty_witness,
           x TYPE string,
           y TYPE string,
         END OF ty_witness.


* Component schema: VerifyRequest, object
  TYPES: BEGIN OF ty_verifyrequest,
           witness TYPE ty_witness,
           proof TYPE string,
         END OF ty_verifyrequest.


* GET - "List circuits"
* Operation id: Listcircuits
* Response: 200
  METHODS listcircuits
    RAISING cx_static_check.

* POST - "Create circuit"
* Operation id: Createcircuit
* Response: 200
* Body ref: #/components/schemas/CreatecircuitRequest
  METHODS createcircuit
    IMPORTING
      body TYPE ty_createcircuitrequest
    RAISING cx_static_check.

* POST - "Verify"
* Operation id: Verify
* Parameter: circuit_id, required, path
* Response: 200
* Body ref: #/components/schemas/VerifyRequest
  METHODS verify
    IMPORTING
      circuit_id TYPE string
      body TYPE ty_verifyrequest
    RAISING cx_static_check.

ENDINTERFACE.
