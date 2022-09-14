@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Root view PRVD orgs'
define root view entity Z100085_ZI_PROUBC_PRVDORGS as select from z100085_prvdorgs {
  key organization_id,
  bpi_endpoint,
  ident_endpoint,
  createdby,
  created_at,
  changedby,
  changed_at
}
