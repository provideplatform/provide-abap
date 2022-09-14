@EndUserText.label: 'proUBC PO BPI Objects Projection View'
@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
define view entity Z100085_ZC_BPIOBJ as projection on Z100085_ZI_BPIOBJ as BPIObject {
      key object_id,
      baseline_id,
      schematype, 
      schema_id,     
      status,        
      proof,        
      created_by,   
      created_at,    
      changed_by,   
      changed_at,    
      _BPIPurchaseOrder : redirected to parent Z100085_ZC_PROUBC_PORD
}
