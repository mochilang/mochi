# AWS Cloud Library for Mochi

This library provides simple wrappers around a runtime AWS mock server.
Each package issues HTTP requests using `fetch` for a smooth developer
experience entirely in Mochi code.

## Packages

- `s3` – buckets with basic put/get/list operations
- `lambda` – remote function invocation
- `dynamo` – a key/value table store

Import individual packages as needed:

```mochi
import "lib/cloud/aws/s3" as s3
let bucket = s3.bucket("demo")
```

A base URL can be given to direct requests to a specific server:

```mochi
let bucket = s3.bucket("demo", "http://localhost:1234")
```
