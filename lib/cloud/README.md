# Cloud Library

This is a small set of pure Mochi packages for building cloud style
applications. Each resource is defined in its own subfolder and can be
imported individually.

- `cloud` – basic application container
- `bucket` – in memory object storage
- `queue` – simple FIFO queue
- `topic` – pub/sub topic with subscribers
- `function` – function wrapper for handlers

Example usage:

```mochi
import "./lib/cloud/bucket" as bucket
import "./lib/cloud/queue" as queue

let b = bucket.new("assets")
bucket.put(b, "key.txt", "value")
print(bucket.get(b, "key.txt"))

let q = queue.new<string>()
queue.push(q, "msg")
print(queue.pop(q))
```
