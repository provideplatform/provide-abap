interface ZIF_PRVD_TENANTS
  public .


  types:
    BEGIN OF ty_tenant_wo_token,
      mandt          TYPE mandt,
      organization_id      TYPE zprvdtenants-organization_id,
      subject_account_id TYPE zprvdtenants-subject_account_id,
      workgroup_id   TYPE zprvdtenants-workgroup_id,
      bpi_endpoint   TYPE zprvdtenants-bpi_endpoint,
      ident_endpoint TYPE zprvdtenants-ident_endpoint,
      created_by     TYPE zprvdtenants-changedby,
      created_at     TYPE zprvdtenants-changed_at,
      changed_by     TYPE zprvdtenants-changedby,
      changed_at     TYPE zprvdtenants-changed_at,
      reachable      TYPE boolean,
    END OF ty_tenant_wo_token .
  types:
    tty_tenant_wo_token TYPE TABLE OF ty_tenant_wo_token .

  types:
    BEGIN OF ty_org_refresh_token_req,
      mandt          TYPE mandt,
      organization_id      TYPE zprvdtenants-organization_id,
      refresh_token  type zprvdrefreshtoken,
    end of ty_org_refresh_token_req.
endinterface.
