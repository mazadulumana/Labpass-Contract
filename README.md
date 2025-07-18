# 🧪 Labpass - Tokenized Lab Access

> NFTs as time-bound lab/resource permissions

## 📋 Overview

Labpass is a Clarity smart contract that enables tokenized access to laboratory resources through time-bound NFTs. Lab owners can create digital passes that grant users specific access levels to their facilities for predetermined durations.

## ✨ Features

- 🏭 **Lab Management**: Create and manage laboratory resources
- 🎫 **Time-bound Passes**: Mint NFTs with expiration blocks
- 🔐 **Access Control**: Multiple access levels per lab
- 💰 **Marketplace**: Buy/sell lab passes
- 📊 **Access Tracking**: Monitor lab usage and history
- ⏰ **Pass Extension**: Extend existing passes
- 🔄 **Transfer Support**: Standard NFT transfer functionality

## 🚀 Usage

### Creating a Lab

```clarity
(contract-call? .labpass create-lab "Biotech Lab A" u100 u3)
```

### Minting a Lab Pass

```clarity
(contract-call? .labpass mint-labpass u1 u24 u2)
```
*Mints a pass for lab #1, valid for 24 hours, with access level 2*

### Accessing a Lab

```clarity
(contract-call? .labpass access-lab u1 u1)
```
*Use pass #1 to access lab #1*

### Extending a Pass

```clarity
(contract-call? .labpass extend-pass u1 u12)
```
*Extend pass #1 by 12 hours*

## 📖 Read-Only Functions

- `get-pass-info`: Get pass details
- `get-lab-info`: Get lab information  
- `is-pass-valid`: Check if pass is still valid
- `get-user-passes`: Get user's pass history
- `get-balance`: Get user's pass count

## 🏪 Marketplace Functions

- `list-in-ustx`: List pass for sale
- `unlist-in-ustx`: Remove listing
- `buy-in-ustx`: Purchase listed pass

## 🔧 Lab Management

- `set-lab-status`: Enable/disable lab access
- Lab owners receive payments automatically
- Configurable pricing per hour

## 💡 Key Concepts

- **Duration**: Measured in hours, converted to blocks (144 blocks/hour)
- **Access Levels**: Numeric levels (1-max) for different permissions
- **Expiry**: Passes expire at specific block heights
- **Ownership**: Passes are transferable NFTs

## 🛡️ Security Features

- Owner-only lab creation
- Pass ownership verification
- Expiry validation
- Access level restrictions
- Payment validation

## 📊 Data Structures

- **Labs**: Name, owner, pricing, access levels
- **Passes**: Lab ID, owner, expiry, access level
- **Access Log**: Usage tracking and history
- **Marketplace**: Listing prices and commissions

## 🎯 Use Cases

- 🔬 Research facility access
- 🏭 Manufacturing equipment time slots  
- 💻 Shared workspace permissions
- 🎓 Educational lab bookings
- 🧬 Specialized equipment access

---

*Built with Clarity for Stacks blockchain* ⚡

