@EndUserText.label: 'proUBC PRVDOrgs Projection View'
@AccessControl.authorizationCheck: #CHECK

@UI: {
 headerInfo: { typeName: 'proUBCOrgs', typeNamePlural: 'proUBCOrgs', title: { type: #STANDARD, value: 'organization_id' } } }
define root view entity Z100085_ZC_PROUBC_PRVDORGS as projection on Z100085_ZI_PROUBC_PRVDORGS {
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
  ident_endpoint,
  
  @UI.hidden: true
  refresh_token, //todo merge these 2 columns into one large string
  @UI.hidden: true
  refresh_tokenext,
  
  createdby,
  created_at,
  changedby,
  changed_at
}
