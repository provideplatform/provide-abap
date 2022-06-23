@AbapCatalog.sqlViewName: 'Z100085ZCBPIOBJ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Composite view of proUBC BPI objects'
@UI.headerInfo: {
    typeName: 'BPIObject',
    typeNamePlural: 'BPIObjects',
    typeImageUrl: '',
    imageUrl: '',
    title: {
        type: #STANDARD,
        label: 'BPI Objects',
        iconUrl: '',
        criticality: '',
        criticalityRepresentation: #WITHOUT_ICON,
        value: '',
        valueQualifier: '',
        targetElement: '',
        url: ''
    },
    description: {
        type: #STANDARD,
        label: 'BPI Objects',
        iconUrl: '',
        criticality: '',
        criticalityRepresentation: #WITHOUT_ICON,
        value: '',
        valueQualifier: '',
        targetElement: '',
        url: ''
    }
}
define view Z100085_ZC_BPIOBJ as select from z100085_bpiobj {
  @UI.lineItem.position: 10
  key object_id,
   @UI.lineItem.position: 20
  baseline_id,
   @UI.lineItem.position: 30  
  schematype,
   @UI.lineItem.position: 40   
  schema_id,     
   @UI.lineItem.position: 40
  status,        
   @UI.lineItem.position: 50
  proof,        
   @UI.lineItem.position: 60
  created_by,   
   @UI.lineItem.position: 70
  created_at,    
   @UI.lineItem.position: 80
  changed_by,   
   @UI.lineItem.position: 90
  changed_at    
}
