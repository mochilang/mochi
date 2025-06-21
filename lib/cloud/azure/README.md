# Azure Cloud Library

This package provides a small set of Azure cloud helpers written entirely in Mochi.
Resources are organized by category under `compute`, `storage` and `network`.
Each module exposes simple types and functions to model common operations.

## Example

```mochi
import "lib/cloud/azure/compute/vm" as vm
import "lib/cloud/azure/network/vnet" as net

let machine = vm.create("demo", "ubuntu-lts", "B1s", "eastus")
machine.start()
let vnet = net.create("demo-net", "eastus")
vnet.attach(machine)
```

These modules only simulate behaviour but mirror the structure of actual Azure services, providing an ergonomic API for experimentation.
