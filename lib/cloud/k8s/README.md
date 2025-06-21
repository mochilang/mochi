# k8s Mochi Library

This library provides helper functions to generate basic Kubernetes resources directly from Mochi code. Each resource type lives in its own package under this directory.

## Packages

- `cluster` – manage a collection of manifests
- `deployment` – create Deployment manifests
- `service` – create Service manifests
- `configmap` – create ConfigMap manifests
- `namespace` – create Namespace manifests

## Usage

Import the desired packages and compose resources. Example:

```mochi
import "lib/cloud/k8s/cluster" as cluster
import "lib/cloud/k8s/deployment" as deployment
import "lib/cloud/k8s/service" as service

let c = cluster.new("demo")
let d = deployment.new("web", "nginx:latest", 2)
cluster.add(c, d)
let s = service.new("web", 80, 80)
cluster.add(c, s)
print(cluster.manifest(c))
```

Running the above will print the combined YAML manifest for the cluster.
