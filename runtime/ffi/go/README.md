# Go FFI Runtime

`runtime/ffi/go` exposes a small foreign function interface for calling native Go
functions from Mochi. Functions are registered under a name and invoked by that
name at runtime. In addition to manual registration, runtime modules can be
loaded from Go plugins that expose an `Exports` map.

```go
import goffi "mochi/runtime/ffi/go"

func init() {
    goffi.Register("add", func(a, b int) int { return a + b })
}
```

Modules built as Go plugins can automatically register functions by exporting an
`Exports` map. Use `LoadModule` to load the plugin:

```go
err := goffi.LoadModule("./math.so")
if err != nil {
    log.Fatal(err)
}
```

The plugin should expose a global variable named `Exports`:

```go
package main

import goffi "mochi/runtime/ffi/go"

var Exports = map[string]any{
    "square": func(n int) int { return n * n },
}

func main() {}
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
