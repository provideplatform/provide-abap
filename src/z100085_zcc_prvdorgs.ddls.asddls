@AbapCatalog.sqlViewName: 'Z100085ZCC1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Consumption View PRVD Orgs'
define view Z100085_ZCC_PRVDORGS as select from Z100085_ZC_PRVDORGS {
  key organization_id,
  bpi_endpoint,
  ident_endpoint,
  refresh_token, //todo merge these 2 columns into one large string
  refresh_tokenext 
}
