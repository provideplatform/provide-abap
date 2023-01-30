INTERFACE zif_prvd_idoc
  PUBLIC .


  TYPES:
    BEGIN OF ty_basictype,
      idoctype          TYPE edbas-idoctyp,
      idoctypedescr     TYPE edi_text60,
      personresponsible TYPE edbas-presp,
      processingperson  TYPE edbas-pwork,
      lastchangeperson  TYPE edbas-plast,
      isreleased        TYPE edbas-closed,
      releaseoftype     TYPE edbas-released,
      predecessor       TYPE edbas-pretyp,
      generatedtype     TYPE edbas-generated,
      createddate       TYPE edbas-credate,
      createdtime       TYPE edbas-cretime,
      changeddate       TYPE edbas-ldate,
      changedtime       TYPE edbas-ltime,
      applrel           TYPE edbas-applrel,
    END OF ty_basictype .
  TYPES:
    tt_basictype TYPE TABLE OF ty_basictype .

  TYPES:
    BEGIN OF ty_schema_trafficlight,
      schema_id       TYPE zprvdtraflight-schema_id,
      schema_name     TYPE zprvdtraflight-schema_name,
      schema_type     TYPE zprvdtraflight-schema_type,
      schema_tlight   TYPE zprvdtraflight-schema_tlight,
    END OF ty_schema_trafficlight.

  TYPES:
    BEGIN OF ty_basictype_w_segments,
      basictype     TYPE ty_basictype,
      idocstruct    TYPE ledid_t_idoc_struct,
      idocsegments  TYPE ledid_t_segment,
      segmentstruct TYPE ledid_t_segment_struct,
    END OF ty_basictype_w_segments .
ENDINTERFACE.
