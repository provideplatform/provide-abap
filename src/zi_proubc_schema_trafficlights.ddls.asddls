@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'proUBC Schema Traffic Lights'
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZIPRVDTRAFLIGHT'
define root view ZI_PROUBC_SCHEMA_TRAFFICLIGHTS
  as select from zprvdtraflight
  association[0..1] to edbas as a on $projection.schema_name = a.idoctyp
{
  key schema_datetime,
  key schema_id,
  schema_name,
  schema_type,
  schema_tlight
  //_association_name // Make association public
}
