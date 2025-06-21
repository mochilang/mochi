# Azure Cloud Library

This package provides a small set of Azure cloud helpers written in Mochi.
Calls are forwarded to the `runtime/cloud/azure` APO endpoint using the
`fetch` expression. Resources are organized by category under `compute`,
`storage` and `network`.

## Example

```mochi
import "lib/cloud/azure/compute/vm" as vm
import "lib/cloud/azure/network/vnet" as net

// override apo for local testing
import "runtime/cloud/azure/azure" as azure
azure.apo = "file://tests/interpreter/valid/azure_mock"

let machine = vm.create("demo", "ubuntu-lts", "B1s", "eastus")
machine.start()
let vnet = net.create("demo-net", "eastus")
vnet.attach(machine)
```

These modules only simulate behaviour but mirror the structure of actual Azure services, providing an ergonomic API for experimentation.
