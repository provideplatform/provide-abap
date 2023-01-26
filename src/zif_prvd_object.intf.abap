interface ZIF_PRVD_OBJECT
  public .


  types:
    BEGIN OF ty_update_status_req,
      status      TYPE string,
      baseline_id TYPE zbpiobj-baseline_id,
    END OF ty_update_status_req .
  types:
    BEGIN OF ty_update_status_res,
      status      TYPE string,
      baseline_id TYPE zbpiobj-baseline_id,
      object_id   TYPE zbpiobj-object_id,
    END OF ty_update_status_res .
  types:
    BEGIN OF ty_update_object_req,
      payload     TYPE REF TO data,
      type        TYPE string,
      baseline_id TYPE zbpiobj-baseline_id,
    END OF ty_update_object_req .
  types:
    BEGIN OF ty_create_object_req,
      payload     TYPE REF TO data,
      type        TYPE string,
      schema_type TYPE string,
      baseline_id TYPE zbpiobj-baseline_id,
    END OF ty_create_object_req .
  types:
    BEGIN OF ty_create_object_req_objid,
      payload     TYPE REF TO data,
      type        TYPE string,
      schema_type TYPE string,
      baseline_id TYPE zbpiobj-baseline_id,
      object_id   TYPE zbpiobj-object_id,
    END OF ty_create_object_req_objid .
  types:
    tty_create_object_req_objid TYPE TABLE OF ty_create_object_req_objid .
endinterface.
