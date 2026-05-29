---
title: "10. Interfaces and method sets"
sidebar_position: 11
sidebar_label: "10. Interfaces and method sets"
description: "Go's structural interface satisfaction, the value-receiver vs pointer-receiver method-set rules, how the bridge translates `type I interface { M() }` into Mochi extern types, the embedded-interface promotion story, the empty-interface `any` / `interface{}` refusal, the trait-object analogue to Rust's `dyn Trait`."
---

# 10. Interfaces and method sets

This note covers the trickiest part of the type-mapping: Go interfaces and their interaction with method sets. Go's interface semantics are structural (any type with the right method set satisfies the interface) and the satisfaction rules are subtle (value vs pointer receivers). The bridge has to encode all of this through the cgo boundary.

## Go interfaces are structural

A Go interface is a method set. A type satisfies the interface iff its method set includes every method the interface lists. There is no explicit `implements` declaration; satisfaction is checked structurally by the compiler:

```go
type Reader interface {
    Read(p []byte) (n int, err error)
}

type FileReader struct { f *os.File }

func (r *FileReader) Read(p []byte) (n int, err error) {
    return r.f.Read(p)
}

// FileReader satisfies Reader implicitly; no `type FileReader implements Reader` declaration.
```

The bridge's strategy: an interface type becomes a Mochi `extern type` with per-method `extern fn` declarations. The Mochi side holds an opaque handle; method calls dispatch through cgo to the underlying Go value:

```mochi
extern type Reader
extern fn (r: Reader) read(p: bytes): Result<int> from go "io.Reader.Read"
```

The Mochi user cannot construct a `Reader` value directly; they obtain it from a Go function that returns `Reader` (e.g., `os.Open` returns `*os.File` which the wrapper auto-promotes to a `Reader` handle when assigned to a `Reader`-typed variable).

## Method sets: value vs pointer receivers

A Go method can have a value receiver (`func (r T) M()`) or a pointer receiver (`func (r *T) M()`). The method-set rules:

- Method set of type `T`: methods with receiver `T` (only value receivers).
- Method set of type `*T`: methods with receiver `T` OR receiver `*T` (both).

This matters for interface satisfaction: a value of type `T` satisfies an interface only if every interface method has a value-receiver implementation on `T`. A value of type `*T` always satisfies.

The bridge encodes the receiver kind in the emitted `extern fn`:

```mochi
extern type Buffer  // wraps Go *bytes.Buffer
extern fn (b: Buffer) write_string(s: string): Result<int> from go "bytes.Buffer.WriteString" receiver "pointer"
extern fn (b: Buffer) string(): string from go "bytes.Buffer.String" receiver "value"
```

The `receiver "pointer"` / `receiver "value"` clause is honoured by the wrapper synthesiser when it picks the call shape:

- For value-receiver methods, the wrapper takes the handle by value (copy the Go value out of the handle, call the method, copy back if mutated... actually Go's value semantics means the wrapper just dereferences the handle, copies the value, calls the method, ignores the copy because value receivers can't mutate).
- For pointer-receiver methods, the wrapper takes the handle, dereferences to get the pointer, calls the method.

The bridge always exposes the more-complete `*T` method set; Mochi values are always opaque handles to `*T` (pointer to the underlying Go value). This sidesteps the receiver-kind subtlety on the consumer side: every Mochi call through the bridge sees the pointer's method set, which is the superset.

## Empty interface (`any` / `interface{}`)

Go's empty interface, written `interface{}` pre-1.18 or `any` from 1.18 onwards, accepts any value. The bridge refuses items whose signature has an `any` in a position the bridge cannot resolve at ingest time:

- A function `func(args ...any)` is refused unless the user lists explicit `[go.monomorphise]` entries binding each arg type.
- A function returning `any` is refused; the bridge cannot type the return on the Mochi side.
- A struct field of type `any` is refused.
- A `map[string]any` value is refused.

The user can hand-author a `custom` `extern fn` that takes responsibility for the `any` at the FFI boundary (e.g., using runtime type assertions on the Go side via a hand-written wrapper item).

## Embedded interfaces

A Go interface can embed another interface:

```go
type ReadWriter interface {
    Reader
    Writer
}
```

The method set of `ReadWriter` is the union of `Reader`'s and `Writer`'s. The bridge resolves the embedding at ingest time and emits the flattened method set:

```mochi
extern type ReadWriter
extern fn (rw: ReadWriter) read(p: bytes): Result<int> from go "io.ReadWriter.Read"
extern fn (rw: ReadWriter) write(p: bytes): Result<int> from go "io.ReadWriter.Write"
```

A `promoted_from = "<source-interface>"` annotation on each emitted item tracks the embedding (purely informational).

## Type assertions and interface conversion

Go has two-form type assertions:

```go
var r Reader = someExpr()
fr, ok := r.(*FileReader)  // succeed only if r dynamically is *FileReader
```

Mochi does not have this surface directly. The bridge offers a `try_as_<concrete-type>` method on every interface type:

```mochi
let r: Reader = open_file("path")
let maybe_fr: FileReader? = r.try_as_file_reader()
if maybe_fr is some {
    // use the FileReader-specific methods
}
```

The wrapper implements the `try_as_*` method via a Go-side type switch:

```go
//export mochi_go_<module>_Reader_try_as_FileReader
func mochi_go_<module>_Reader_try_as_FileReader(handle uint64) uint64 {
    r := resolveHandle(handle).(Reader)
    if fr, ok := r.(*FileReader); ok {
        return acquireHandle(fr)
    }
    return 0 // 0 sentinel = nil
}
```

The Mochi side sees the result as `FileReader?` (`null` when the assertion fails).

## Common-stdlib interfaces

Several Go interfaces are universally implemented and worth special-casing:

| Interface | Mochi treatment |
|-----------|-----------------|
| `error` | Special: desugared into Mochi `Result<T>` for return positions. |
| `fmt.Stringer` (`String() string`) | Mapped to Mochi's `to_string()` convention. |
| `io.Reader` / `io.Writer` | Mapped to opaque handles with `read` / `write` methods. |
| `sort.Interface` | Refused: Mochi has its own sort surface; cross-language `sort.Sort` is not supported. |
| `context.Context` | Mapped to an opaque `Context` extern type; the bridge provides `context.background()`, `context.with_timeout()`, etc. shims. |

The `fmt.Stringer` special-case is implicit: any wrapped type whose underlying Go type satisfies `fmt.Stringer` automatically gets a Mochi `.to_string()` method.

## Type parameter constraints (generics)

Go 1.18+ generics introduce type parameter constraints, which are interfaces:

```go
type Ordered interface {
    ~int | ~int64 | ~float64 | ~string
}

func Min[T Ordered](a, b T) T
```

The constraint `Ordered` is a special interface (a "type union") that does not have method-set satisfaction; it has type-membership satisfaction. The bridge refuses generic items whose constraint is a type union (the bridge's monomorphisation has to pick specific types from the union).

When `[go.monomorphise]` declares an instantiation, the bridge synthesises one wrapper per instantiation:

```toml
[go.monomorphise]
items = [
    { item = "golang.org/x/exp/constraints.Min", T = "int64" },
    { item = "golang.org/x/exp/constraints.Min", T = "float64" },
]
```

Generates:

```go
//export mochi_go_<module>_Min_int64
func mochi_go_<module>_Min_int64(a, b int64) int64 {
    return constraints.Min[int64](a, b)
}

//export mochi_go_<module>_Min_float64
func mochi_go_<module>_Min_float64(a, b float64) float64 {
    return constraints.Min[float64](a, b)
}
```

The Mochi shim file declares each as a separate `extern fn`; the user calls `constraints.min_int64(...)` or `constraints.min_float64(...)`.

## Interface method dispatch cost

Each Mochi-side method call on an interface handle costs:

- Cgo call into the wrapper: ~200ns.
- Wrapper does `resolveHandle(id).(InterfaceType)`: ~30ns (sync.Map lookup + type-assert).
- Wrapper dispatches to the concrete type's method: Go's interface dispatch via the itab cache: ~5ns.
- Total: ~235ns per call.

This is materially more expensive than a Mochi-native method call (~5ns) but comparable to a typical Go interface dispatch in a benchmark loop.

## Trait-object analogue

Go interfaces play the role Rust's `dyn Trait` plays. MEP-73 §10 documents the lifetime-and-ownership story for `dyn Trait`. The Go story is simpler because Go interfaces are not lifetime-parameterised: every interface value owns its underlying data (via the interface's internal `(type-pointer, data-pointer)` representation). The bridge's opaque handle simply pins the interface value via `cgo.Handle`.

There is no equivalent of Rust's `Box<dyn Trait>` distinction; every Go interface value is already "boxed" in the interface representation.

## Cross-references

- [[05-type-mapping]] for the broader translation table.
- [[09-abi-stability]] for the cgo dispatch cost.
- [[04-go-doc-ast-ingest]] for how method sets are extracted from `*types.Interface`.
- [[12-risks-and-alternatives]] §R9 for the cgo-export symbol collision risk.
- [The Go spec on method sets](https://go.dev/ref/spec#Method_sets) for the canonical rules.
