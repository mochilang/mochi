# Go FFI Runtime

`runtime/ffi/go` exposes a small foreign function interface for calling native Go
functions from Mochi. Functions are registered under a name and invoked by that
name at runtime.

```go
import goffi "mochi/runtime/ffi/go"

func init() {
    goffi.Register("add", func(a, b int) int { return a + b })
}
```

A registered function can later be called dynamically:

```go
res, err := goffi.Call("add", 2, 3)
// res == 5
```

`Call` uses reflection to invoke the function with `[]any` arguments. If the
function's final return value implements `error`, it is returned from `Call`.
Otherwise the first return value (or slice of values) is provided.

This package is intentionally minimal and keeps an in-process registry. Mochi's
interpreter and compilers can translate `ffi("go", name)` expressions into
`goffi.Call` invocations.
