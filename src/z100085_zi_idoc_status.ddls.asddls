@AbapCatalog.sqlViewName: 'Z100085IDOCSTAT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Most recent status of idoc'
define view z100085_ZI_IDOC_STATUS
    with parameters p_idocnum:edi_docnum
    as select from edids {
    key edids.docnum as IDocNum,
    max(edids.countr)  as counter
}
where edids.docnum = :p_idocnum

group by
    edids.docnum,
    edids.countr
