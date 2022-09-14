INTERFACE z100085_zif_proubc_testobj
  PUBLIC .

  TYPES: BEGIN OF ty_obtrigtest,
           bpi_tenant_id TYPE z100085_prvdorgs-organization_id,
           status        TYPE i, "status 201, 400, etc
           bpi_response_payload type string,
           bpi_endpoint  TYPE string,
           msg_payload   TYPE z100085_zif_proubc_baseline=>protocolmessage_req,
         END OF ty_obtrigtest.


ENDINTERFACE.
