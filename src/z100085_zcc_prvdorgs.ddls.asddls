@AbapCatalog.sqlViewName: 'Z100085ZCC1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Consumption View PRVD Orgs'
@OData.publish: true
@ObjectModel: {
    createEnabled: true,
    updateEnabled: true,
    deleteEnabled: true
}
define view Z100085_ZCC_PRVDORGS as select from Z100085_ZC_PRVDORGS {
  @UI: {
    lineItem: [{ position: 10, importance: #HIGH}],
    identification: [{position: 10, label: 'PRVD Org ID'}]
  }
  key organization_id,
  @UI: {
    lineItem: [{ position: 20, importance: #HIGH}],
    identification: [{position: 20, label: 'BPI Endpoint'}]
  }
  bpi_endpoint,
  @UI: {
    lineItem: [{ position: 30, importance: #HIGH}],
    identification: [{position: 30, label: 'Ident Endpoint'}]
  }
  ident_endpoint
}
