@AbapCatalog.sqlViewName: 'Z100085POUI5'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'proUBC UI5/odata example PO'
@UI.headerInfo: {
    typeName: 'PurchaseOrder',
    typeNamePlural: 'PurchaseOrders',
    typeImageUrl: '',
    imageUrl: '',
    title: {
        type: #STANDARD,
        label: 'proUBC Purchase Orders',
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
        label: 'proUBC Purchase Orders',
        iconUrl: '',
        criticality: '',
        criticalityRepresentation: #WITHOUT_ICON,
        value: '',
        valueQualifier: '',
        targetElement: '',
        url: ''
    }
}
define view Z100085_ZCC_PURCHORD 
    as select from I_PurchaseOrder
    //association[0..1] to Z100085_ZC_BPIOBJ as _BPIObjects on _BPIObjects.object_id = $projection.PurchaseOrder 
    association[0..1] to I_Supplier_VH as _SupplierVH on _SupplierVH.Supplier = $projection.Supplier
{
    @UI.lineItem.position: 10
    key PurchaseOrder,
    @UI.lineItem.position: 20
          PurchaseOrderType,
      PurchaseOrderSubtype,
      PurchasingDocumentOrigin,
      PurchasingDocumentIsAged,

      //Admin
      @UI.lineItem.position: 30
      CreatedByUser,
 //     @Semantics.businessDate.createdAt
      CreationDate,
       PurchaseOrderDate,
      @Semantics.language: true
      @ObjectModel.foreignKey.association: '_Language'
      Language,

      //Status
      PurchasingDocumentDeletionCode,
      ReleaseIsNotCompleted,
      PurchasingCompletenessStatus,
      PurchasingProcessingStatus,
      PurgReleaseSequenceStatus,
      ReleaseCode,  
      PurchasingReleaseStrategy,    

      //Organization
      @ObjectModel.foreignKey.association: '_CompanyCode'
      CompanyCode,
      @ObjectModel.foreignKey.association: '_PurchasingOrganization'
      PurchasingOrganization,
      @ObjectModel.foreignKey.association: '_PurchasingGroup'
      PurchasingGroup,

      //Supplier
      @ObjectModel.foreignKey.association: '_Supplier'
      @Consumption.valueHelp: '_SupplierVH'
      @UI.lineItem.position: 40
      Supplier,
      @UI.lineItem.position: 50
      _SupplierVH.SupplierName as SupplierName,
      ManualSupplierAddressID,
      SupplierAddressID,
      SupplierRespSalesPersonName,
      SupplierPhoneNumber,
      @ObjectModel.foreignKey.association: '_SupplyingSupplier'
      SupplyingSupplier,
      @ObjectModel.foreignKey.association: '_SupplyingPlant'
      SupplyingPlant,
      @ObjectModel.foreignKey.association: '_InvoicingParty'
      InvoicingParty,
      Customer,

      //References
      CorrespncExternalReference, //Your reference
      CorrespncInternalReference, //Our reference
      
      // Contract
      PurchaseContract,
      
      // Request For Quotation
      RequestForQuotation,      
      
      //Quotation
      SupplierQuotationExternalID,

      //PaymentTerms
      @ObjectModel.foreignKey.association: '_PaymentTerms'
      PaymentTerms,
      CashDiscount1Days,
      CashDiscount2Days,
      NetPaymentDays,
      CashDiscount1Percent,
      CashDiscount2Percent,

      //DownPayment
      DownPaymentType,
      DownPaymentPercentageOfTotAmt,
      DownPaymentAmount,
      DownPaymentDueDate,

      //Incoterms
      @ObjectModel.foreignKey.association: '_IncotermsClassification'
      IncotermsClassification,
      IncotermsTransferLocation,
      @ObjectModel.foreignKey.association: '_IncotermsVersion'
      IncotermsVersion,
      IncotermsLocation1,
      IncotermsLocation2,
      
      //Intratat
      IsIntrastatReportingRelevant,
      IsIntrastatReportingExcluded,

      //Pricing
      PurchasingDocumentCondition,
      PricingProcedure,
      
      @Semantics.currencyCode
      @ObjectModel.foreignKey.association: '_DocumentCurrency'
      DocumentCurrency,

      ValidityStartDate,
      ValidityEndDate,

      @Consumption.hidden: true 
      ExchangeRate,
      ExchangeRateIsFixed,
      
      LastChangeDateTime,
      IsEndOfPurposeBlocked,

      TaxReturnCountry,
      VATRegistrationCountry,
      PurgReasonForDocCancellation,
      PurgReleaseTimeTotalAmount,
      
      //custom associations
      //@UI.connectedFields.valueQualifier: 'BPIObjects'
      //@UI.connectedFields.groupLabel: 'BPIObjects'
      //@UI.connectedFields.type: #STANDARD
      //@UI.connectedFields.name: 'BPIObjects'
      //@UI.facet.parentId: 'PurchaseOrder'
      //@UI.fieldGroup: [{ 
      //                   targetElement: '',
      //                   type: #WITH_NAVIGATION_PATH,
      //                   value: 'PurchaseOrder',
      //                   qualifier: 'BPIObjects',
      //                   groupLabel : 'BPIObjects' }]
      //_BPIObjects,
      _SupplierVH,
      
      //standard Associations
      @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
    _PurchaseOrderItem,
    _PurchaseOrderType,
      _CompanyCode,
      _CreatedByUser,
      _Supplier,
      _SupplyingSupplier,
      _InvoicingParty,
      _PurchasingOrganization,
      _PurchasingGroup,
      _DocumentCurrency,
      _IncotermsClassification,
      _IncotermsVersion,
      _SupplyingPlant,
      _PaymentTerms,
      _SupplierAddress,
      _Language,
      _PurchaseOrderCalcFields
    
    //
   // case _BPIObjects.object_id
   //     when '' then
   //         cast('-' as abap.char( 1 ))
   //     when PurchaseOrder then
   //         cast('X' as abap.char( 1 ))
   //     else
   //         cast('-' as abap.char( 1 ))
   // end as isBaselined
}
