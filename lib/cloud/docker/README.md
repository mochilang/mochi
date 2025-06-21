# Docker Cloud Library for Mochi

This library provides a minimal toolkit for orchestrating Docker
images and containers directly from Mochi scripts. It exposes simple
APIs for defining images and running containers. Each object type is
defined in its own folder for clarity.

## Usage

```mochi
import "lib/cloud/docker" as docker

let img = docker.image("my-image", ".")
img.build()

let ctr = docker.container("my-container", img, ["-p", "80:80"])
ctr.run()
```

The library only prints the Docker commands, making it safe to use in
environments without the Docker CLI installed.
