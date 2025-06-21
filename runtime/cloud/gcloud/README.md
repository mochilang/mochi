# runtime/cloud/gcloud

This package provides minimal helpers for interacting with Google Cloud services using Go. It mirrors the functionality exposed in `lib/cloud/gcloud` so Mochi programs compiled to Go can leverage the same API surface.

The package offers a `Client` type and helpers for Storage, Pub/Sub, and Cloud Functions. All operations are implemented using the public REST endpoints.
