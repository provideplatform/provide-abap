@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Interface view of proUBC BPI objects'
define view entity Z100085_ZI_BPIOBJ as select from z100085_bpiobj
association to parent Z100085_ZI_PROUBC_PORD as _BPIPurchaseOrder
    on $projection.object_id = _BPIPurchaseOrder.PurchaseOrder {
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
      _BPIPurchaseOrder
}
