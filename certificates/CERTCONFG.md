# proUBC API Certificates

For outbound connectivity to PRVD stack services you will need the relevant client and root certificates maintained in transaction code STRUST. The default proUBC delivery has no dependency to any middleware components - so API calls take place directly from SAP to the PRVD stack. However this does require certificate maintenance to ensure secure connectivity between SAP and PRVD stack. The instructions below refer to the PRVD stack APIs that are operated as subdomains from [provide.services](https://provide.services). 

## Client certificate
When using the provide.services host for Ident, Baseline, Vault, Nchain, and Privacy - download the client certificate from:
- your browser on any provide.services site
- hosted location

Maintain the client certificates in STRUST

## Root certificate
Download the root certificates from

Maintain the root certificates in STRUST

## Alternate configurations
Other deployments of proUBC and PRVD stack may require different hosts using different client / root certs. If you are using an additional middleware system between proUBC/SAP and the PRVD stack, you may need to install certificates in that middleware system as well

## Questions?
Feel free to email the [Provide Services team](mailto:ryan@provide.services) to engage technical support
Use subject line 'proUBC PRVD Stack API Certs'