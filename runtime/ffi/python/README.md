# `mochi/runtime/ffi/python`

This package provides a minimal Foreign Function Interface (FFI) for
invoking Python code from Go.  It is intended for Mochi's interpreter and
runtime to execute small Python snippets or call functions in existing
modules without embedding CPython directly.

The implementation shells out to `python3` and exchanges values using
JSON.  Arguments are marshalled into a JSON array which is made
available to the Python process via the `MOCHI_ARGS` environment
variable.  The executed script is expected to output a JSON encoded
result to `stdout`.

Example usage:

```go
import "mochi/runtime/ffi/python"

// Call a function from the standard library.
res, err := python.Call("math", "sqrt", 9)
if err != nil {
        log.Fatal(err)
}
fmt.Println(res) // 3.0
```

The helper `Exec` runs an arbitrary code block and returns its result:

```go
result, _ := python.Exec(`
return args[0] + args[1]
`, 2, 3)
fmt.Println(result) // 5
```

`Attr` can retrieve variables or call functions dynamically:

```go
v, _ := python.Attr("math", "pi")
fmt.Println(v) // 3.141592653589793

res, _ := python.Attr("math", "pow", 2, 3)
fmt.Println(res) // 8
```

This lightweight approach avoids cgo dependencies while making it easy
to leverage Python's vast ecosystem from Mochi programs.
