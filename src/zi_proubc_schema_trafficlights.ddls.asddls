@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'PRVD Schema Traffic Lights'
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZIPRVDTRAFLIGHT'
define root view ZI_PROUBC_SCHEMA_TRAFFICLIGHTS
  as select from zprvdtraflight
  association[0..1] to edbas as _iDocTypeDetails on $projection.schema_name = _iDocTypeDetails.idoctyp
{
  key schema_id,
  schema_name,
  schema_type,
  schema_tlight,
  //_association_name // Make association public
  _iDocTypeDetails
}
