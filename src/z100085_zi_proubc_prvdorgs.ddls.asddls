@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Root view PRVD orgs'
define root view entity Z100085_ZI_PROUBC_PRVDORGS as select from z100085_prvdorgs {
  key organization_id,
  bpi_endpoint,
  ident_endpoint,
  refresh_token, //todo merge these 2 columns into one large string
  refresh_tokenext,
  createdby,
  created_at,
  changedby,
  changed_at
}
