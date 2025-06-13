# TypeScript FFI Runtime (Go)

This package mirrors the old TypeScript implementation of the FFI runtime but is
implemented in Go. It allows Mochi programs to call functions and values
exposed from Go or from TypeScript modules executed via Deno.

Values can be registered directly or loaded from a module. Symbols are invoked
by name at runtime.

```go
import tsffi "mochi/runtime/ffi/ts"

func init() {
    tsffi.Register("add", func(a, b int) int { return a + b })
    tsffi.LoadModule("runtime/ffi/ts/sample-module.mjs")
}

res, _ := tsffi.Call("add", 1, 2)
pi, _ := tsffi.Call("pi")
```

`LoadModule` uses Deno to import the module and register all of its exports so
they can be called later.
