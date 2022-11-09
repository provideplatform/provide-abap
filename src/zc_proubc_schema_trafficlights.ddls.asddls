@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'proUBC Schema Traffic Lights - Cons view'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@AbapCatalog.sqlViewName: 'ZCPRVDTRAFLIGHT'

define view ZC_PROUBC_SCHEMA_TRAFFICLIGHTS
  as select from ZI_PROUBC_SCHEMA_TRAFFICLIGHTS
{
  key schema_id,
      schema_name,
      schema_type,
      schema_tlight,
      _iDocTypeDetails
}
