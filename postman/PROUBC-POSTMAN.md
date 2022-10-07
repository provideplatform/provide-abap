# proUBC Postman Collection

## Overview
Following the initial install and configuration of proUBC, you can use this API along with PRVD CLI and shuttle to configure and test PRVD tenants as well as PRVD tenant's baseline protocol messages and object status updates

It is assumed you will test this API on SAP system with basic authentication, typically in an on-premise environment

Make sure the SICF node /sap/proubc is active!

## Postman Environment Variables

sapbaseurl - External IP or host used for the SAP system.
sapuser - the SAP user id
sappassword - the SAP user password
xcsrftoken - A temporary authentication token *required* for PUT, POST, and DELETE HTTP calls to SAP. This is passed with the X-CSRF-Token header. This is generated with a HEAD request to sapbaseurl/sap/proubc/auth. 
prvdtenantid - A globally unique identifier for the PRVD user
bpiobjectid - An id that may correspond to SAP sales order, purchase order, etc
prvdrefreshtoken - A JWT token for verified credentials provided by Ident. By default, the JWT shall expire every 30 days or earlier at user discretion.

## SAP Backend Authorization
Additonal information forthcoming.

## Version
Last update Aug 23 2022