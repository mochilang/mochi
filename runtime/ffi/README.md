# Runtime FFI

This directory contains language specific runtimes used to invoke foreign
functions from Mochi programs. Each runtime implements a small set of common
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

The Go, TypeScript and Python runtimes expose concrete implementations of these
APIs while also providing package level helpers for convenience. The Go runtime
can additionally load functions from modules compiled as plugins exposing an
`Exports` map.
