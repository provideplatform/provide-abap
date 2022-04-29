@AbapCatalog.sqlViewName: 'Z100085ZC1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Composite View PRVD Orgs'
@ObjectModel: {
    createEnabled: true,
    updateEnabled: true,
    deleteEnabled: true
}
define view Z100085_ZC_PRVDORGS as select from Z100085_ZI_PRVDORGS {
  key organization_id,
  bpi_endpoint,
  ident_endpoint,
  refresh_token, //todo merge these 2 columns into one large string LCHAR
  refresh_tokenext,
  createdby,
  created_at,
  changedby,
  changed_at
  
  //cast('' as abap.rawstring( 2048 )) as refreshTokenFull
}
