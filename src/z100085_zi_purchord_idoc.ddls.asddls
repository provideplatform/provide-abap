@AbapCatalog.sqlViewName: 'Z100085POIDOC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'proUBC iDoc details for Purchase Order'
define view Z100085_ZI_PURCHORD_IDOC 
    as select from edidc
    inner join cmfp on cmfp.msgv1 = edidc.docnum
    inner join nast on nast.cmfpnr = cmfp.nr
    //inner join edid4 on edid4.docnum = edidc.docnum                    
{
   key edidc.docnum as IDocNum,
   nast.objky as PurchaseOrder
   
   //key edid4.counter,
   //key edid4.segnum
   //cast(1000 as abap.int4) as segmentlength,
   //edid4.sdata
   //todo fix casting error, get belnr from segment
   //cast(edid4.sdata as abap.rawstring( 1000 )) as segmentdata
} where
    edidc.mestyp = 'ORDERS'
    and edidc.idoctp = 'ORDERS05'
    and cmfp.arbgb = 'E0'
    and nast.kappl = 'EF'
    //and edid4.segnam = 'E1EDK01'
    //edidc.segnam = 'E1EDK01'
