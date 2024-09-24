# Real Estate Smart Contract on Stacks Blockchain

## Overview

This project provides a robust and feature-rich smart contract for managing real estate transactions on the Stacks blockchain using Clarity. The contract automates the processes of property listing, buying, selling, and dispute resolution, reducing the need for intermediaries and ensuring transparency and security in real estate transactions.

## Features

1. **Property Listing and Metadata:**
   - Register properties as non-fungible tokens (NFTs) with unique asset IDs.
   - Include property details like cost and listing duration.

2. **Automated Transactions:**
   - Automate the buying and selling of properties.
   - Use an escrow mechanism to hold funds and transfer ownership once conditions are met.

3. **Multi-Signature Approval:**
   - Require multiple approvers to validate high-value transactions.
   - Ensure security and consensus before completing a property sale.

4. **Dynamic Pricing and Listing Expiration:**
   - Allow property owners to update property prices.
   - Automatically cancel property listings after the expiration period.

5. **Dispute Resolution:**
   - Reset transactions in case of disputes or errors, safeguarding both buyers and sellers.

## Prerequisites

- Stacks Blockchain Network: A Stacks node or access to a network like the Stacks Testnet or Mainnet.
- Clarity Smart Contract Development Tools: Install [Clarinet](https://github.com/hirosystems/clarinet) for local development and testing.
- Familiarity with Clarity Language: Basic knowledge of the Clarity language and the Stacks ecosystem.

## Contract Overview

The smart contract consists of the following key components:

1. **Variables:**
   - `property-owner`: Stores the address of the property owner.
   - `potential-buyer`: Optionally stores the address of a potential buyer.
   - `property-cost`: The cost of the property in microSTX.
   - `transaction-complete`: A boolean flag indicating if the property has been sold.
   - `property-asset-id`: The unique ID representing the property NFT.
   - `listing-expiration`: Expiration time of the property listing in blocks.
   - `multi-signature-approvers`: A list of principals who can approve the transaction.
   - `approvers-count`: The number of approvals received for the transaction.

2. **NFT Definition:**
   - `real-estate-nft`: Represents the property as a non-fungible token.

3. **Core Functions:**
   - `register-property`: Registers a property with a cost, asset ID, and expiration duration.
   - `submit-offer`: Allows a buyer to express interest in the property.
   - `add-approver`: Adds a multi-signature approver for the transaction.
   - `approve-transaction`: Approves the transaction from an approver's side.
   - `complete-transaction`: Finalizes the sale, transferring ownership and funds.
   - `withdraw-offer`: Allows the buyer to withdraw their offer.
   - `update-property-cost`: Updates the cost of the property.
   - `cancel-listing-if-expired`: Cancels the property listing if the expiration time has passed.
   - `reset-transaction`: Resets the transaction in case of disputes.

4. **Events:**
   - `property-listed`: Logs details when a property is listed.
   - `transaction-completed`: Logs details when a transaction is successfully completed.

## Setup and Deployment

1. **Install Clarinet:**
   - Follow the instructions on the [Clarinet GitHub page](https://github.com/hirosystems/clarinet) to install the Clarinet tool.

2. **Clone the Repository:**
   - Clone this repository to your local machine:
     ```bash
     git clone https://github.com/your-username/real-estate-smart-contract.git
     cd real-estate-smart-contract
     ```

3. **Compile the Contract:**
   - Use Clarinet to compile the smart contract:
     ```bash
     clarinet check
     ```

4. **Run Tests:**
   - Ensure the contract logic is working correctly by running the tests:
     ```bash
     clarinet test
     ```

5. **Deploy the Contract:**
   - Deploy the contract to the desired network (Testnet or Mainnet):
     ```bash
     clarinet deploy --network testnet
     ```

6. **Interact with the Contract:**
   - Use the Clarinet console or Stacks wallet to interact with the contract functions.

## Usage

### Listing a Property

```clarity
(register-property cost asset-id duration)
```
- **cost**: The cost of the property in microSTX.
- **asset-id**: The unique ID for the property.
- **duration**: The number of blocks for which the listing is valid.

### Submitting an Offer

```clarity
(submit-offer)
```
- The potential buyer submits an offer for the property.

### Completing the Transaction

```clarity
(complete-transaction)
```
- Finalizes the sale, transferring ownership and funds, provided that the transaction has the necessary approvals.

### Updating Property Cost

```clarity
(update-property-cost new-cost)
```
- **new-cost**: The updated cost of the property in microSTX.

### Cancelling Expired Listings

```clarity
(cancel-listing-if-expired)
```
- Cancels the property listing if the listing has expired.

### Resetting the Transaction

```clarity
(reset-transaction)
```
- Resets the transaction state, allowing a fresh start in case of disputes.

## Error Codes

- `u100`: Unauthorized property registration.
- `u101`: Property already sold or inactive.
- `u102`: An existing offer is already submitted.
- `u103`: Property transaction already completed.
- `u104`: No potential buyer available.
- `u105`: Only the potential buyer can call this function.
- `u106`: Property transaction already completed.
- `u107`: Invalid potential buyer.
- `u108`: Only the potential buyer can call this function.
- `u109`: Property transaction already completed.
- `u110`: Listing expired.
- `u111`: Only the property owner can add approvers.
- `u112`: Maximum number of approvers reached.
- `u113`: Caller is not an approver.
- `u114`: Insufficient approvals.
- `u115`: Only the property owner can update cost.
- `u116`: Cannot update cost of a sold property.
- `u117`: Property transaction already completed.
- `u118`: Listing not yet expired.
- `u119`: Only the property owner can reset the transaction.

## Contribution

Contributions to this project are welcome. If you have suggestions for new features, optimizations, or bug fixes, please submit a pull request or open an issue on the GitHub repository.

## Acknowledgements

- [Stacks Blockchain](https://www.stacks.co/) for providing a powerful platform to build decentralized applications.
- [Clarity Language](https://docs.stacks.co/docs/clarity/overview) for enabling predictable and secure smart contracts.
- Community contributors and developers for their valuable feedback and support.