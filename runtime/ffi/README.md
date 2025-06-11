# Runtime FFI

This directory contains language specific runtimes used to invoke foreign
functions from Mochi programs. Most runtimes expose a small set of common
interfaces defined in [`ffi.go`](./ffi.go):

```go
// Caller invokes a registered function by name.
type Caller interface {
    Call(name string, args ...any) (any, error)
}

// Registerer registers a function for later invocation.
type Registerer interface {
    Register(name string, fn any) error
}

// Loader loads additional modules that may register functions.
type Loader interface {
    LoadModule(path string) error
}
```

The Go and TypeScript runtimes implement these APIs and provide package level
helpers. The Python runtime offers a simplified `Attr` function for retrieving
or invoking module attributes. The Go runtime can additionally load functions
from plugins exposing an `Exports` map.
