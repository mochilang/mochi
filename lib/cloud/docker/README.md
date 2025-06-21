# Docker Cloud Library for Mochi

This library talks to a small Docker API server provided by `runtime/cloud/docker`.
Every operation sends an HTTP request and prints the command returned by the server.
Set `MOCHI_DOCKER_API` to change the server address (defaults to `http://localhost:23750`).

## Usage

```mochi
import "lib/cloud/docker" as docker

let img = docker.image("my-image", ".")
docker.build(img)
docker.push(img)

let ctr = docker.container("my-container", img, ["-p", "80:80"])
docker.run(ctr)
docker.stop(ctr)
docker.remove_container(ctr)

let net = docker.network("app-net")
docker.create_network(net)
docker.remove_network(net)

let vol = docker.volume("data")
docker.create_volume(vol)
docker.remove_volume(vol)
```

