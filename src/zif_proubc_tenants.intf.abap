interface ZIF_PROUBC_TENANTS
  public .


  types:
    BEGIN OF ty_tenant_wo_token,
           mandt           TYPE mandt,
           organization_id TYPE zprvdtenants-tenant_id,
           bpi_endpoint    TYPE zprvdtenants-bpi_endpoint,
           ident_endpoint  TYPE zprvdtenants-ident_endpoint,
           created_by      TYPE zprvdtenants-changedby,
           created_at      TYPE zprvdtenants-changed_at,
           changed_by      TYPE zprvdtenants-changedby,
           changed_at      TYPE zprvdtenants-changed_at,
           reachable          TYPE boolean, "use BPI endpoint to evaluate
         END OF ty_tenant_wo_token .
  types:
    tty_tenant_wo_token type table of ty_tenant_wo_token .
endinterface.
