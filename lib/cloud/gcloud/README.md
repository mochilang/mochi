# GCloud Mochi Library

This package provides a small wrapper around Google Cloud REST APIs.
Create a `Client` and use subpackages like `storage`, `pubsub`, and `functions`
to interact with services.

````mochi
import "lib/cloud/gcloud" as gcloud
import "lib/cloud/gcloud/storage" as storage

let gc = gcloud.client("my-project", "TOKEN")
let bucket = storage.create(gc, "my-bucket")
````
