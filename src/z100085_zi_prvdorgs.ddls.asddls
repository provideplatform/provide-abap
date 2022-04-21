@AbapCatalog.sqlViewName: 'Z100085ZI1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Interface View PRVD Orgs'
@ObjectModel: {
    createEnabled: true,
    updateEnabled: true,
    deleteEnabled: true
}
define view Z100085_ZI_PRVDORGS as select from z100085_prvdorgs {
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
