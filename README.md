# 🌳 Treedent - On-chain Tree Planting Verification System

Treedent is a decentralized application that enables transparent verification of tree planting activities on the Stacks blockchain.

## 🎯 Features

- Register newly planted trees with location and species data
- Verify planted trees through authorized verifiers
- Track planter statistics and reputation
- Maintain a network of trusted verifiers

## 🔧 Smart Contract Functions

### For Tree Planters
- `register-tree`: Register a newly planted tree with coordinates and species
- `get-tree-details`: View details of a specific tree
- `get-planter-stats`: Check your planting statistics
- `get-all-trees-for-planter`: List all trees planted by an address

### For Verifiers
- `verify-tree`: Verify a planted tree and update planter's reputation
- `is-verifier`: Check if an address is an authorized verifier

### For Contract Owner
- `add-verifier`: Add a new authorized verifier
- `remove-verifier`: Remove a verifier's authorization

## 📝 Usage

1. Register a tree:
```clarity
(contract-call? .treedent register-tree 4250000 -7834000 "Quercus alba")
```

2. Verify a tree (verifiers only):
```clarity
(contract-call? .treedent verify-tree u1)
```

3. Check tree details:
```clarity
(contract-call? .treedent get-tree-details u1)
```

## 🌱 Data Structure

Trees are stored with the following properties:
- Planter address
- Latitude/Longitude (multiplied by 100000 for precision)
- Species name
- Planting timestamp
- Verification status
- Verifier address

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
```


