interface ZIF_PROUBC_IDENT
  public .


  types:
* Generated by abap-openapi-client
* Ident, 1.0
* Component schema: CreateapplicationRequest, object
    BEGIN OF createapplicationrequest,
           name TYPE string,
         END OF createapplicationrequest .
  types:
* Component schema: AssociateusertoapplicationRequest, object
    BEGIN OF associateusertoapplicationrequ,
           user_id TYPE string,
         END OF associateusertoapplicationrequ .
  types:
* Component schema: UpdateapplicationRequest, object
    BEGIN OF updateapplicationrequest,
           name        TYPE string,
           description TYPE string,
           type        TYPE string,
           hidden      TYPE abap_bool,
         END OF updateapplicationrequest .
  types:
* Component schema: CreateorganizationRequest, object
    BEGIN OF createorganizationrequest,
           name        TYPE string,
           description TYPE string,
         END OF createorganizationrequest .
  types:
* Component schema: UpdateorganizationdetailsRequest, object
    BEGIN OF updateorganizationdetailsreque,
           name        TYPE string,
           description TYPE string,
         END OF updateorganizationdetailsreque .
  types:
* Component schema: Authorizelong-termtokenRequest, object
    BEGIN OF authorize_access_refreshtoken,
           scope           TYPE zcasesensitive_str,
           organization_id TYPE zcasesensitive_str,
         END OF authorize_access_refreshtoken .
  TYPES:
* Component schema: Authorizelong-termtokenRequest, object
    BEGIN OF refresh_accesstoken_request,
      organization_id TYPE zcasesensitive_str,
      grant_type      TYPE zcasesensitive_str,
    END OF refresh_accesstoken_request .
  types:
    BEGIN OF authorizelongtermtokenresponse,
           id            TYPE zcasesensitive_str,
           access_token  TYPE zcasesensitive_str,
           refresh_token TYPE zcasesensitive_str,
           expires_in    TYPE int4,
           scope         TYPE zcasesensitive_str,
           permissions   TYPE int4,
         END OF authorizelongtermtokenresponse .
  types:
* Component schema: AuthenticationRequest, object
    BEGIN OF authenticationrequest,
           email    TYPE string,
           password TYPE zcasesensitive_str,
         END OF authenticationrequest .
  types:
* Component schema: CreateuserRequest, object
    BEGIN OF createuserrequest,
           email      TYPE string,
           first_name TYPE string,
           last_name  TYPE string,
         END OF createuserrequest .
  types:
* Component schema: UpdateuserRequest, object
    BEGIN OF updateuserrequest,
           email      TYPE string,
           first_name TYPE string,
           last_name  TYPE string,
           password   TYPE string,
         END OF updateuserrequest .

* POST - "Create application"
* Operation id: Createapplication
* Parameter: name, required, header
* Response: 200
* Body ref: #/components/schemas/CreateapplicationRequest
  methods CREATEAPPLICATION
    importing
      !NAME type STRING
      !BODY type CREATEAPPLICATIONREQUEST
    raising
      CX_STATIC_CHECK .
* GET - "List applications"
* Operation id: Listapplications
* Response: 200
  methods LISTAPPLICATIONS
    raising
      CX_STATIC_CHECK .
* POST - "Associate user to application"
* Operation id: Associateusertoapplication
* Parameter: application_id, required, path
* Response: 200
* Body ref: #/components/schemas/AssociateusertoapplicationRequest
  methods ASSOCIATEUSERTOAPPLICATION
    importing
      !APPLICATION_ID type STRING
      !BODY type ASSOCIATEUSERTOAPPLICATIONREQU
    raising
      CX_STATIC_CHECK .
* GET - "List application users"
* Operation id: Listapplicationusers
* Parameter: content-type, required, header
* Parameter: application_id, required, path
* Response: 200
  methods LISTAPPLICATIONUSERS
    importing
      !CONTENT_TYPE type STRING
      !APPLICATION_ID type STRING
    raising
      CX_STATIC_CHECK .
* GET - "Get application details"
* Operation id: Getapplicationdetails
* Parameter: content-type, required, header
* Parameter: application_id, required, path
* Response: 200
  methods GETAPPLICATIONDETAILS
    importing
      !CONTENT_TYPE type STRING
      !APPLICATION_ID type STRING
    raising
      CX_STATIC_CHECK .
* PUT - "Update application"
* Operation id: Updateapplication
* Parameter: name, required, header
* Parameter: application_id, required, path
* Response: 200
* Body ref: #/components/schemas/UpdateapplicationRequest
  methods UPDATEAPPLICATION
    importing
      !NAME type STRING
      !APPLICATION_ID type STRING
      !BODY type UPDATEAPPLICATIONREQUEST
    raising
      CX_STATIC_CHECK .
* DELETE - "Delete application"
* Operation id: Deleteapplication
* Parameter: content-type, required, header
* Parameter: application_id, required, path
* Response: 200
  methods DELETEAPPLICATION
    importing
      !CONTENT_TYPE type STRING
      !APPLICATION_ID type STRING
    raising
      CX_STATIC_CHECK .
* GET - "List organizations"
* Operation id: Listorganizations
* Parameter: content-type, required, header
* Response: 200
  methods LISTORGANIZATIONS
    importing
      !CONTENT_TYPE type STRING
    raising
      CX_STATIC_CHECK .
* POST - "Create organization"
* Operation id: Createorganization
* Parameter: name, required, header
* Response: 200
* Body ref: #/components/schemas/CreateorganizationRequest
  methods CREATEORGANIZATION
    importing
      !NAME type STRING
      !BODY type CREATEORGANIZATIONREQUEST
    raising
      CX_STATIC_CHECK .
* GET - "Get organization details"
* Operation id: Getorganizationdetails
* Parameter: content-type, required, header
* Parameter: name, required, header
* Parameter: organization_id, required, path
* Response: 200
  methods GETORGANIZATIONDETAILS
    importing
      !CONTENT_TYPE type STRING
      !NAME type STRING
      !ORGANIZATION_ID type STRING
    raising
      CX_STATIC_CHECK .
* PUT - "Update organization details"
* Operation id: Updateorganizationdetails
* Parameter: name, required, header
* Parameter: organization_id, required, path
* Response: 200
* Body ref: #/components/schemas/UpdateorganizationdetailsRequest
  methods UPDATEORGANIZATIONDETAILS
    importing
      !NAME type STRING
      !ORGANIZATION_ID type STRING
      !BODY type UPDATEORGANIZATIONDETAILSREQUE
    raising
      CX_STATIC_CHECK .
* GET - "List tokens"
* Operation id: Listtokens
* Parameter: content-type, required, header
* Parameter: name, required, header
* Response: 200
  methods LISTTOKENS
    importing
      !CONTENT_TYPE type STRING
      !NAME type STRING
    raising
      CX_STATIC_CHECK .
* POST - "Authorize refresh token"
* Operation id: refresh_accesstoken_request
* Response: 201
* Body ref: #/components/schemas/refresh_accesstoken_request
  methods REFRESH_ACCESS_TOKEN
    importing
      !BODY type REFRESH_ACCESSTOKEN_REQUEST
    exporting
      !STATUS type I
      !APIRESPONSE type ref to DATA
    raising
      CX_STATIC_CHECK .
  methods AUTHORIZELONG_TERMTOKEN
    importing
      !BODY type AUTHORIZE_ACCESS_REFRESHTOKEN
    exporting
      !STATUS type I
      !APIRESPONSE type ref to DATA
    raising
      CX_STATIC_CHECK .
* PUT - "Update user"
* Operation id: Updateuser
* Parameter: name, required, header
* Response: 200
* Body ref: #/components/schemas/UpdateuserRequest
  methods UPDATEUSER
    importing
      !NAME type STRING
      !BODY type UPDATEUSERREQUEST
    raising
      CX_STATIC_CHECK .
* DELETE - "Revoke token"
* Operation id: Revoketoken
* Parameter: content-type, required, header
* Response: 200
  methods REVOKETOKEN
    importing
      !CONTENT_TYPE type STRING
    raising
      CX_STATIC_CHECK .
* POST - "Authentication"
* Operation id: Authentication
* Response: 200
* Body ref: #/components/schemas/AuthenticationRequest
  methods AUTHENTICATION
    importing
      !BODY type AUTHENTICATIONREQUEST
    exporting
      !APIRESPONSE type ref to DATA
    raising
      CX_STATIC_CHECK .
* GET - "List users Copy"
* Operation id: ListusersCopy
* Parameter: content-type, required, header
* Response: 200
  methods LISTUSERSCOPY
    importing
      !CONTENT_TYPE type STRING
    raising
      CX_STATIC_CHECK .
* POST - "Create user"
* Operation id: Createuser
* Response: 200
* Body ref: #/components/schemas/CreateuserRequest
  methods CREATEUSER
    importing
      !BODY type CREATEUSERREQUEST
    raising
      CX_STATIC_CHECK .
* GET - "Get user detail"
* Operation id: Getuserdetail
* Parameter: content-type, required, header
* Parameter: name, required, header
* Parameter: user_id, required, path
* Response: 200
  methods GETUSERDETAIL
    importing
      !CONTENT_TYPE type STRING
      !NAME type STRING
      !USER_ID type STRING
    raising
      CX_STATIC_CHECK .
* DELETE - "Delete user"
* Operation id: Deleteuser
* Parameter: content-type, required, header
* Parameter: user_id, required, path
* Response: 200
  methods DELETEUSER
    importing
      !CONTENT_TYPE type STRING
      !USER_ID type STRING
    raising
      CX_STATIC_CHECK .
endinterface.
