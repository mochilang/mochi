# Cloud Library

This package provides simple building blocks for cloud-style applications.
Resources are accessed over HTTP and use JSON for requests and responses.

Supported primitives:

- **Bucket** – store and retrieve objects by key.
- **Queue** – push and pop messages.

Each resource is created with a base URL pointing at a server that implements
its API. Headers can be provided for authentication.

These modules are implemented entirely in Mochi to keep the syntax concise
and familiar.

## Planned objects

The initial release only includes the Bucket and Queue clients. The following
objects are planned for future versions:

- [ ] Function
- [ ] Topic
- [ ] Table
- [ ] Counter
- [ ] Schedule
- [ ] Secret
- [ ] FileSystem
- [ ] API

These placeholders represent the set of cloud primitives we would like to
support over time. Contributions are welcome.
