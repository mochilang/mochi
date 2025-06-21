# GCloud Mochi Library

This library offers a tiny Google Cloud wrapper using Mochi's `fetch` statement.
It communicates with the API helpers implemented in `runtime/cloud/gcloud`.
Create a `Client` then use the storage, pubsub, and functions submodules.

````mochi
import "lib/cloud/gcloud" as gcloud
import "lib/cloud/gcloud/storage" as storage

let gc = gcloud.client("my-project", "TOKEN")
let bucket = storage.create(gc, "my-bucket")
````
