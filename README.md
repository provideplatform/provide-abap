# proUBC

Open-source implementation of the PRVD Stack baseline middleware pattern for SAP, using ABAP

## About proUBC
proUBC enables SAP systems to synchronize data to the Ethereum mainnent, Ethereum Layer 2 scaling solutions (such as Polygon Matic), and other EVM compatible networks. This gives the ERP the capability to leverage web3, DeFi, Data Oracles, and other blockchain-based services and never-before-realized Ethereum-enabled enterprise use cases in finance, supply chain, CRM, manufacturing and more. This is achieved securely and cost-effectively through advanced cryptotography techniques and the APIs of the PRVD stack (aka BRI-1, Baseline Reference Implementation #1). 

Several of the PRVD stack API service proxies for ABAP in proUBC include:
- Ident. Manages, authorizes, and authenticates verified credentials (VCs) used across PRVD stack APIs and beyond. https://docs.provide.services/api/rest-api-v1/ident
- Baseline. Creates zero-knowledge proof based messaging through defined workgroup participants. https://docs.provide.services/api/rest-api-v1/baseline
- Nchain. Nchain enables the configuration of wallets, EVM networks, and broadcasting transactions. https://docs.provide.services/api/rest-api-v1/nchain
- Vault. Vault service offers state-of-the-art key management with a focus on providing advanced privacy and messaging capabilities (i.e., zero-knowledge proofs, SNARK-friendly hash functions, double-ratchet algorithm, etc.) in a single enterprise-grade API. https://docs.provide.services/api/rest-api-v1/vault
- Privacy. API toolkit for creating, configuring, and secure storage of zero knowledge (zk) circuits, rollups and proofs. https://docs.provide.services/api/rest-api-v1/privacy

Enterprise-level security and privacy are built into all aspects of these APIs through an industry leading implementation of the Baseline Protocol. Adherence to the Baseline Protocol standards means no sensitive data from the ERP or elsewhere is ever directly placed on-chain

proUBC is released as open source software under the Apache 2.0 license:  free to use and vendor agnostic for the public good of the SAP, Ethereum, PRVD Oasis and Baseline Protocol developer ecosystems.

proUBC also works hand-in-hand with the PRVD CLI and Shuttle for onboarding users and low-code/no-code workflow & workgroup modeling using the baseline pattern

Beyond proUBC and SAP, PRVD stack can also used across other enterprise applications and services - such as ServiceNow and Excel. Other language specific PRVD stack libraries include support for Rust, .NET, iOS, Go, PHP, Ruby, Python and JavaScript. 

Apart from the advantages of bringing web3 capabilities to the enterprise, the proUBC and PRVD stack can be very helpful in adding new assurances to traditional enterprise middleware systems and business processes. In many multi-party business integrations contexts such as with SAP, it's infeasible to figure out whether your supplier or customer can attest to having the very same precise data as you at any given time past or present. Your customer/supplier may not be able to directly access your system - and you may not be able to access theirs. Further, reconciling acknowledgments in how data changes at a given time (when did the material availability date change? and did the customer or supplier know it before the order cutoff?) can be difficult to capture in an easy to use format. Your idoc status information may tell the story of how the data changed and moved in your system, but not necessarily whether it was used correctly by the customer or supplier who genuinely needs it! REST API logs can help too, but even those can be difficult to share and costly to retain records for over a longer term.

What if you could have an easy to use, secure, always-on, always-fair electronic bulletein board that could address that problem?
This highly desired capability can be provided to you on Ethereum thanks to the PRVD stack and proUBC! The transaction finality, global state, impartiality, and continuity of service Ethereum operates upon coupled with zero-knowledge cryptography innovations PRVD Stack, proUBC and Baseline Protocol can make that capability a reality.


## Configuration Summary

General steps to implement proUBC in your SAP system are as follows
- Clone this repository via abapGit
- Administer proUBC SAP user roles
- Activate proUBC inbound REST service in your SAP system
- Configure PRVD stack API connectivity in your SAP system
- Set up PRVD tenants/users using PRVD CLI, Shuttle, and the proUBC inbound REST service
- Send your first protocol message and verify end-to-end configuration

Additional detailed documentation, including step-by-step implementation guide will be published in a gitbook soon!

## Questions?

Feel free to create an issue in the repo or reach out to Ryan at ryan@provide.services. 

Also check out PRVD Oasis (prvdoasis.org) - the OASIS Open project to govern the PRVD stack, including proUBC. Several noteworthy enterpise and web3 organizations support PRVD Oasis including Bank United, DataBrains, Polygon, Synadia, Chainlink, and Haas Online (and more others to come!). Follow or become a member of PRVD Oasis to learn more about other remarkable projects using proUBC and the PRVD stack.
