# SAP S/4 HANA ABAP + web3 + zk + sustainability use cases

Made possible with PRVD Stack, provide-abap, and Provide Shuttle!

## Carbon offsetting with the Carbonmark API
- Implement a RESTful ABAP application
- Offset carbon emissions based on SFLIGHT data
- Carbon retirements purchased and retired from Carbonmark, KlimaDAO in real time using the Provide Payments fiat-crypto bridge, Circle USDC
- Github repo : [https://github.com/fleischr/provide-eco-sflight-demo-abap](https://github.com/fleischr/provide-eco-sflight-demo-abap)

## Carbon emissions verification
- For scope 3 emissions data, generate a proof of emissions that helps your business partners, governments and NGOs verify your scope 3 carbon emissions data - even when offline from your SAP landscape
- Leverages EPM demo data model
- Github repo: [https://github.com/fleischr/eco_emissions_demo](https://github.com/fleischr/eco_emissions_demo)

## Create NFT digital twins of SAP real world assets
- Introduces general framework to mint and interact with NFTs in ABAP for various business processes
- B2C scenarios : NFTs for home appliance warranties, items in PC/mobile/video games
- B2B scenarios : Tokenized invoices, AR/AP balances
- Github rep : [https://github.com/fleischr/abap-nft](https://github.com/fleischr/abap-nft)

## Send ERC-20 Stablecoin and CBDCs
- Demonstrates how to send ERC-20 tokens in ABAP for stablecoin / CBDC based payments
- B2C scenarios : Customer rebates
- B2B scenarios : Cross-border payments
- Github repo : [https://github.com/fleischr/abap-erc20](https://github.com/fleischr/abap-erc-20)

## Cross-company iDoc state verification
- Use the zprvd_shuttleidoc_exmpl report in this repo to create zero-knowledge proofs of ORDERS or INTERNAL_ORDER idoc types using the PRVD stack
- Allows companies to confirm iDocs arrive to 3rd parties in the exact state desired - without needing to directly access the 3rd party system
- Check out the provide-abap docs for implmentation instructions

## Foreign currency, digital asset price synchronization with Chainlink
- Use the Chainlink price feed contracts on Polygon to maintain foreign exchange and digital asset prices in S/4
- Create "Proof of Price" zk-workflow to enable greater coordination between business partners on exchange rates thereby simplifying accounting/finance operations
- Generate report to Filecoin to improve collaboration across business partners and facilitate sharing B2B commerce data back to the Chainlink DON
- Chainlink Fall 2022 Hackathon Top Quality award winner
- Github repo : [https://github.com/provideplatform/prvd-chainlink-fall-hackathon-2022](https://github.com/provideplatform/prvd-chainlink-fall-hackathon-2022)

## Cost effective data storage with Filecoin / IPFS
- Development in progress
- Generate an excel file in SAP - and share it to Filecoin/IPFS
- Demonstrate how to retrieve files from Filecoin into SAP
- How to integrate and pay a storage provider - with or without direct use of the $FIL token
- Github repo : [https://github.com/fleischr/SAP-Excel-to-IPFS](https://github.com/fleischr/SAP-Excel-to-IPFS)

## Vendor Bank Account Verification
- Prevent fraud, theft, and manipulation of vendor bank info from social engineering and phishing attempts by using zero knowledge as a cryptographically secure channel to verify bank info
- Github repo : [https://github.com/fleischr/zkvendorbankverify](https://github.com/fleischr/zkvendorbankverify)

## Your use case

Working on something cool? Feel free to open a PR and share it here!





