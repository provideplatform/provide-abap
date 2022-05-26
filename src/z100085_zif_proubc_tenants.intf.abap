INTERFACE z100085_zif_proubc_tenants
  PUBLIC .

  TYPES: BEGIN OF ty_tenant_wo_token,
           mandt           TYPE mandt,
           organization_id TYPE z100085_prvdorgs-organization_id,
           bpi_endpoint    TYPE z100085_prvdorgs-bpi_endpoint,
           ident_endpoint  TYPE z100085_prvdorgs-ident_endpoint,
           created_by      TYPE z100085_prvdorgs-changedby,
           created_at      TYPE z100085_prvdorgs-changed_at,
           changed_by      TYPE z100085_prvdorgs-changedby,
           changed_at      TYPE z100085_prvdorgs-changed_at,
           reachable          TYPE boolean, "use BPI endpoint to evaluate
         END OF ty_tenant_wo_token.

    types:  tty_tenant_wo_token type table of ty_tenant_wo_token.
ENDINTERFACE.
