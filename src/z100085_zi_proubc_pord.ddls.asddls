@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'proUBC Purchase Orders Interface View'
define root view entity Z100085_ZI_PROUBC_PORD as select from I_PurchaseOrder
    composition of  Z100085_ZI_BPIOBJ as _BPIObjects
    association[0..1] to I_Supplier_VH as _SupplierVH on _SupplierVH.Supplier = $projection.Supplier
{
    
      key PurchaseOrder,
      PurchaseOrderType,
      PurchaseOrderSubtype,
      PurchasingDocumentOrigin,
      PurchasingDocumentIsAged,

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
      Supplier,
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
      
      _BPIObjects,
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
    
}
