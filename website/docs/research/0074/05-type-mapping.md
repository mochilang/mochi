---
title: "05. Type mapping"
sidebar_position: 6
sidebar_label: "05. Type mapping"
description: "The complete closed translation table from Go types to Mochi types, the refusal cases, the generic monomorphisation rule, the string vs []byte parameter handling, the error desugar, the chan-as-stream and func-as-callback handle patterns."
---

# 05. Type mapping table

This note documents the closed translation from Go types to Mochi types. The table is the same shape MEP-73's §05 documents for Rust, adapted for Go's type system.

## Scalar types

| Go type | Mochi type | Notes |
|---------|-----------|-------|
| `bool` | `bool` | Direct passthrough. |
| `int` | `int` | Mochi `int` is 64-bit on all targets; Go `int` is platform-width. The wrapper inserts a runtime check when Go `int` is 32-bit (rare: only on 32-bit linux / wasm-js). |
| `int8`, `int16`, `int32` | `int` | Sign-extend on the C side. |
| `int64` | `int` | Direct passthrough (both are 64-bit). |
| `uint`, `uint8`, `uint16`, `uint32` | `int` | The wrapper guards positive-range at the boundary; out-of-range values raise a runtime error. |
| `uint64` | `int` | The wrapper rejects values `> int64.MaxValue` at the boundary. |
| `uintptr` | `int` | Same as `uint64`. |
| `float32` | `float` | Widening cast. |
| `float64` | `float` | Direct passthrough. |
| `byte` (= `uint8`) | `int` | (with positive-range guard) |
| `rune` (= `int32`) | `int` | |
| `complex64`, `complex128` | refused | Mochi has no complex type. The user can hand-write an `extern fn` that takes (real, imag) pairs. |

## Strings and byte slices

| Go type | Mochi type | Notes |
|---------|-----------|-------|
| `string` | `string` | At the cgo boundary, Go's `string` is copied to a C `char*` via `C.CString`. The wrapper inserts the matching `C.free` call inside a `defer` block. Free symbol: `mochi_go_<module>_string_free(*const c_char)`. |
| `[]byte` | `bytes` | Owned `(ptr, len, cap)` triple. The wrapper inserts a `_free` symbol that calls `runtime.KeepAlive` and lets Go GC handle reclamation after the Mochi side calls free. |

The `string` round-trip cost is a `C.CString` allocation per call (the Go side cannot pass its internal `string` directly because the underlying buffer is GC-tracked and can move). For hot loops, the user can opt into a `string` builder pattern via the `batched` wrapper variant.

## Collection types

| Go type | Mochi type | Notes |
|---------|-----------|-------|
| `[]T` (slice of in-table T) | `list<T>` | `(ptr, len, cap)` triple. The wrapper provides element-wise getter/setter symbols. |
| `[N]T` (array of in-table T) | `list<T>` | Fixed-size on the Go side; the wrapper allocates a slice and copies. |
| `map[K]V` (K is `string` or integer; V is in-table) | `map<K, V>` | Opaque `*C.MochiMap` handle. Symbols: `_get`, `_set`, `_delete`, `_iter_begin`, `_iter_next`, `_iter_end`, `_free`. |
| `map[K]V` (K is a struct or interface) | refused | Mochi's map keys are `string` or integer; mapping struct keys would require hashing on the C side which the wrapper does not implement. |

Go's `map` is the trickiest case. Go maps cannot be passed across the cgo boundary because the underlying hash table is GC-tracked and the bucket layout is internal. The wrapper exposes a per-handle iterator with `_iter_*` symbols; the Mochi side bridges this to Mochi's `for k, v in m { ... }` loop via the iterator protocol. Iteration order is non-deterministic on the Go side (Go intentionally randomises map iteration); the wrapper documents this.

## Pointer types

| Go type | Mochi type | Notes |
|---------|-----------|-------|
| `*T` (T is a named struct) | `T?` (nullable handle) | Opaque cgo handle. The wrapper exposes constructor / accessor symbols per exported field. |
| `*T` (T is a named primitive like `*int`) | `int?` | Auto-dereferenced on the boundary; nil maps to Mochi `nil`. |
| `*T` (T is unexported) | refused | The Mochi side cannot construct or inspect an unexported type. |
| `**T` (pointer-to-pointer) | refused | Mochi has no pointer arithmetic; double-pointer semantics don't translate. |

## Struct types

A Go struct with all-exported fields, all of which are in-table, becomes a Mochi `record`:

```go
type User struct {
    ID    int64
    Name  string
    Email string
}
```

```mochi
record User {
    ID: int,
    Name: string,
    Email: string,
}
```

A struct with mixed-export fields (some uppercase, some lowercase) projects the exported subset:

```go
type Internal struct {
    Public  string
    private int
}
```

```mochi
record Internal {
    Public: string,
}
```

A `SkipReport` entry records the dropped `private` field. The wrapper exposes only the public accessor.

A struct embedding another struct or interface promotes the embedded type's exported fields and methods. The bridge resolves promotions at ingest time.

## Interface types

A Go interface becomes a Mochi `extern type` with per-method `extern fn` declarations:

```go
type Reader interface {
    Read(p []byte) (n int, err error)
}
```

```mochi
extern type Reader
extern fn (r: Reader) read(p: bytes): Result<int> from go "io.Reader.Read"
```

The Mochi side gets an opaque handle; method calls go through cgo. Mochi cannot itself implement a Go interface (the implementation would have to live on the Go side); a future MEP could add this via callback handles, but MEP-74 v1 does not.

## Channel types

A Go `chan T` (bidirectional) becomes a Mochi `stream<T>`:

```go
func Tick(d time.Duration) <-chan time.Time
```

```mochi
extern fn tick(d: int): stream<int> from go "time.Tick"
```

The wrapper allocates a Go channel of buffer size `[go.goroutine-bridge.default-buffer]`, registers it as a cgo handle, and exposes `_send`, `_recv`, `_close` symbols. The Mochi side consumes the stream via the normal stream-iterator protocol.

Direction-restricted channels (`<-chan T`, `chan<- T`) project the appropriate subset of operations. A receive-only channel maps to a Mochi `stream<T>` with the `send` operation disabled.

## Function types

A Go `func` value (taken as a parameter, returned from a function, or stored in a field) becomes a Mochi callback handle:

```go
func Walk(root string, fn func(path string) error) error
```

```mochi
extern fn walk(root: string, fn: fun(path: string) -> Result<unit>): Result<unit> from go "path/filepath.Walk"
```

The wrapper registers the Mochi callback as a cgo handle, calls into Go, and dispatches each invocation back to Mochi via the `_call` cgo export. The cost is two cgo crossings per callback invocation (Mochi → Go → callback → Mochi → Go → return).

## The `error` interface

Go's `error` is an interface with one method (`Error() string`). MEP-74 treats it as a built-in sum type rather than as a generic interface:

```go
func Open(name string) (*File, error)
```

```mochi
extern fn open(name: string): Result<File> from go "os.Open"
```

The Mochi `Result<T>` desugar uses the same `try` / `catch` lowering MEP-73 introduced for `Result<T, E>` in Rust. The error value's `Error() string` call result is the carried message.

## Tuple returns

A Go function returning multiple values returns a tuple to Mochi:

```go
func Split(s string, sep string) []string
func Cut(s string, sep string) (before, after string, found bool)
```

```mochi
extern fn split(s: string, sep: string): list<string> from go "strings.Split"
extern fn cut(s: string, sep: string): tuple<string, string, bool> from go "strings.Cut"
```

The last-position `error` is special-cased: `(T, error)` becomes `Result<T>` rather than `tuple<T, Result<unit>>`. The `(T1, T2, error)` becomes `Result<tuple<T1, T2>>`. The desugar is documented per-fn in the emitted shim.

## Variadic parameters

A Go `...T` variadic parameter becomes Mochi `varargs<T>`:

```go
func Printf(format string, args ...any)
```

The `any` case is refused by default; with `[go.monomorphise]` declarations, the user can bind `Printf` at concrete types.

## Generic items

Go 1.18+ generics are refused by default; opt-in via `[go.monomorphise]`. The bridge synthesises one wrapper per declared instantiation:

```toml
[go.monomorphise]
items = [
    { item = "encoding/json.Unmarshal", T = "MyStruct" },
    { item = "slices.Sort", T = "int64" },
    { item = "slices.Sort", T = "string" },
]
```

The wrapper emits `mochi_go_<module>_<fn>_<T>` symbols per instantiation. The Mochi shim file declares each as a separate `extern fn`.

## Refusal cases

Items are skipped with a `SkipReport` for the following reasons:

| Reason | Example |
|--------|---------|
| `unexported_in_position` | A function returning an unexported type. |
| `internal_path` | Item lives under `<module>/internal/`. |
| `generic_without_monomorphise` | Generic item not listed in `[go.monomorphise]`. |
| `unsafe_pointer` | Item involves `unsafe.Pointer`. |
| `reflect_value` | Item involves `reflect.Value` or `reflect.Type`. |
| `cgo_handle` | Item involves `cgo.Handle` directly (without going through the bridge's own handle pool). |
| `interface_with_complex_method` | Interface method returns a type outside the table. |
| `chan_of_struct_with_unexported` | Channel element type fails the struct projection. |
| `map_key_not_basic` | Map key is a struct or interface. |
| `requires_cgo_capability` | Module declares `import "C"` and the user has not opted in. |
| `build_tag_excluded` | Item is behind a build tag not in `[go.build-tags]`. |

Each `SkipReport` carries the qualified item name and the reason; the user sees them as warnings during `mochi pkg lock`.

## Type round-trip table

| Boundary direction | Cost | Notes |
|--------------------|------|-------|
| Mochi `int` → Go `int64` | 0 (passthrough) | Both are 64-bit on 64-bit hosts. |
| Mochi `string` → Go `string` | ~50ns + alloc | `C.CString` then `C.GoString`. |
| Mochi `list<int>` → Go `[]int64` | O(n) copy | Slice header passed; backing memory copied. |
| Mochi `bytes` → Go `[]byte` | O(n) copy | Same as list. |
| Mochi callback → Go `func` | ~150ns/invocation | Handle registration + dispatch. |
| Go `chan T` → Mochi `stream<T>` | ~200ns/element | cgo crossing per element. |
| Go `error` → Mochi `Result<T>` | ~100ns on error path | Calls `.Error() string`. |

## Cross-references

- [[02-design-philosophy]] §6 for why the table is closed.
- [[04-go-doc-ast-ingest]] for the ApiSurface JSON shape the typemap consumes.
- [[09-abi-stability]] for the cgo boundary contract.
- [[10-interface-and-method-set]] for the interface encoding deep-dive.
- [[08-goroutine-bridge]] for the channel and callback handle protocols.
- [MEP-74 §1.3](/docs/mep/mep-0074#1-pipeline-overview) for where the typemap sits in the pipeline.
