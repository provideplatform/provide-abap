*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

types: begin of ty_prvd_tenant,
        organization_id  type char64,
        bpi_endpoint     type char255,
        ident_endpoint   type char255,
        refresh_token    type Z100085_PRVDREFRESHTOKEN,
       end of ty_prvd_tenant.

class lcl_prvd_tenant definition.
public section.
  Data: organization_id  type char64,
        bpi_endpoint     type char255,
        ident_endpoint   type char255,
        refresh_token    type Z100085_PRVDREFRESHTOKEN.
protected section.
private section.
endclass.

class lcl_prvd_tenant implementation.
endclass.

"todo add error msg handling
class lcl_errormsg definition.
endclass.

class lcl_errormsg implementation.
endclass.
