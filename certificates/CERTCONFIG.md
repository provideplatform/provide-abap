# proUBC API Certificates

For outbound connectivity to PRVD stack services you will need the relevant client and root certificates maintained in transaction code STRUST. The default proUBC delivery has no dependency to any middleware components - so API calls take place directly from SAP to the PRVD stack. However this does require certificate maintenance to ensure secure connectivity between SAP and PRVD stack. The instructions below refer to the PRVD stack APIs that are operated as subdomains from [provide.services](https://provide.services). 

## Client certificate
The provide.services client certificate is provided here (provideservices.cer) for your convenience.

You can check the authenticity of this cert by downloading the client cert used by visiting any API subdomain (e.g. ident.provide.services) at provide.services. Make sure the root CA on the cert matches (sometimes antivirus programs may overwrite the root ca with their own for client certs)

## Root certificate
The Starfield (SFSRootCAG2.cer) and Amazon Root CA (AmazonRootCA1.cer) root certs are provided here for your convenience.

You can check the authenticity of these certs from https://www.amazontrust.com/repository/

## Maintain the client and root certificates in STRUST

In STRUST, upload the certificates using this process
    1. Make sure you are in change mode
    2. Upload the cert click the little up arrow and select the cert file
    3. Click "Add to certificate list"
    4. Click save

![cert steps](/certsteps.PNG)

Complete this process for all 3 certs named here. Reccomended to follow this process in each folder on the left-hand side in STRUST.

## Alternate configurations
Other deployments of proUBC and PRVD stack may require different hosts using different client / root certs. If you are using an additional middleware system between proUBC/SAP and the PRVD stack, you may need to install certificates in that middleware system as well

## Questions?
Feel free to email the [Provide Services team](mailto:ryan@provide.services) to engage technical support
Use subject line 'proUBC PRVD Stack API Certs'