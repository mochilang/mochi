# libmochi for Go

This package provides small helpers for executing Mochi code from Go. It
expects the `mochi` executable to be available on the system `PATH` or a
custom path may be supplied via `RunOptions`.

## Usage

```go
out, err := libmochi.Run("print(\"hello\")", nil)
fmt.Print(out) // => "hello\n"

res, err := libmochi.Call(
    "fun add(a:int, b:int): int { return a + b }",
    "add", []any{2, 3}, nil,
)
fmt.Println(res) // => 5
```

Both helpers return the captured standard output or decoded result and
return an error if the Mochi process exits with a non-zero status.
