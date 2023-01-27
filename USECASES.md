# SAP S/4 HANA x zk+Web3 use cases

Made possible with PRVD Stack, proUBC, and Provide Shuttle!

## Cross-company iDoc state verification
- Use the zprvd_shuttleidoc_exmpl report in this repo to create zero-knowledge proofs of ORDERS or INTERNAL_ORDER idoc types using the PRVD stack
- Allows companies to confirm iDocs arrive to 3rd parties in the exact state desired - without needing to directly access the 3rd party system
- Check out the proUBC docs for implmentation instructions

## Foreign currency, digital asset price synchronization with Chainlink
- Use the Chainlink price feed contracts on Polygon to maintain foreign exchange and digital asset prices in S/4
- Create "Proof of Price" zk-workflow to enable greater coordination between business partners on exchange rates thereby simplifying accounting/finance operations
- Generate report to Filecoin to improve collaboration across business partners and facilitate sharing B2B commerce data back to the Chainlink DON
- Chainlink Fall 2022 Hackathon Top Quality award winner
- git repo link : https://github.com/provideplatform/proubc-chainlink-fall-hackathon-2022

## Cost effective data storage with Filecoin / IPFS
- Development in progress
- Generate an excel file in SAP - and share it to Filecoin/IPFS
- Demonstrate how to retrieve files from Filecoin into SAP
- How to integrate and pay a storage provider - with or without direct use of the $FIL token
- git repo link : https://github.com/fleischr/SAP-Excel-to-IPFS

## Vendor Bank Account Verification
- Prevent fraud, theft, and manipulation of vendor bank info from social engineering and phishing attempts by using zero knowledge as a cryptographically secure channel to verify bank info
- git repo link : https://github.com/fleischr/zkvendorbankverify

## Your use case

Working on something cool? Feel free to open a PR and share it here!





