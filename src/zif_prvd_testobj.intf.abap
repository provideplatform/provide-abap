interface ZIF_PRVD_TESTOBJ
  public .


  types:
    BEGIN OF ty_obtrigtest,
      bpi_tenant_id        TYPE zprvdtenants-organization_id,
      status               TYPE i,
      bpi_response_payload TYPE string,
      bpi_endpoint         TYPE string,
      msg_payload          TYPE zif_prvd_baseline=>protocolmessage_req,
    END OF ty_obtrigtest .
endinterface.
