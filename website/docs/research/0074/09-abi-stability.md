---
title: "09. ABI stability"
sidebar_position: 10
sidebar_label: "09. ABI stability"
description: "The cgo `//export` ABI guarantees, the C-side ownership contract, the runtime.KeepAlive invariant, the string and slice round-trip mechanics, the c-archive vs c-shared decision, the cross-platform header story, and the batched-variant optimisation for hot loops."
---

# 09. ABI stability

This note covers the ABI contract between Mochi and the synthesised wrapper package. The contract is layered: cgo's `//export` ABI underneath, the bridge's symbol-naming and ownership conventions on top.

## The cgo `//export` ABI

A Go function annotated with `//export Foo` is exposed as a C-callable symbol with the C-side declaration:

```c
extern <C-return-type> Foo(<C-argument-types>);
```

The cgo translation:

| Go type | C type |
|---------|--------|
| `int8`, `int16`, `int32`, `int64` | `int8_t`, `int16_t`, `int32_t`, `int64_t` |
| `uint8`, `uint16`, `uint32`, `uint64` | `uint8_t`, `uint16_t`, `uint32_t`, `uint64_t` |
| `float32`, `float64` | `float`, `double` |
| `bool` | `_Bool` (C99) |
| `*C.char` | `char*` |
| `[]byte` via `unsafe.Slice` | not supported as `//export` arg; use `*C.char` + `int` |
| `unsafe.Pointer` | `void*` |
| Multi-return `(T, error)` | struct return: `typedef struct { T r0; char* r1; } Foo_return;` |

The bridge restricts itself to the safe subset: scalar arguments, `*C.char` for strings, `*C.void` (cast to specific struct pointers) for opaque handles, multi-return structs for `Result<T>`-shaped functions. The full `unsafe.Pointer` surface is not used in the synthesised wrapper.

## C-side ownership contract

The bridge documents the ownership of every value crossing the boundary:

| Value direction | Allocator | Releaser | Lifetime |
|------------------|-----------|----------|----------|
| Mochi → Go scalar | n/a | n/a | per-call |
| Mochi → Go `*C.char` (string) | Mochi side via `mochi_make_cstr` | the Go side after `C.GoString` copies | per-call |
| Go → Mochi `*C.char` (string) | Go side via `C.CString` | the Mochi side via `mochi_go_<module>_string_free` | until free |
| Go → Mochi struct handle | Go side via `C.malloc`-equivalent backing the cgo handle | the Mochi side via `mochi_go_<module>_<type>_free` | until free |
| Go → Mochi slice triple | Go side; backing array pinned by `cgo.Handle` | Mochi side via `mochi_go_<module>_<type>_free` | until free |
| Mochi → Go callback | Mochi-side closure registered as handle | Mochi side via `mochi_go_<module>_callback_release` | scope of the outer call |

Every `//export` function the wrapper emits has a matching `_free` symbol (when it returns an owned value). The Mochi side calls the appropriate `_free` from a `defer`-equivalent block when the value goes out of Mochi scope.

## The `runtime.KeepAlive` invariant

A subtle Go-side concern: when the wrapper passes a Go object's internal pointer to the C side, Go's GC can move or reclaim the object if it cannot prove the object is still reachable from Go code. The fix: insert `runtime.KeepAlive(obj)` at the end of every `//export` function, before the return:

```go
//export mochi_go_module_GetData
func mochi_go_module_GetData(handle uint64) *C.char {
    obj := resolveHandle(handle).(*Data)
    cstr := C.CString(obj.Text)
    runtime.KeepAlive(obj) // ensure obj is not GC'd while cstr is in C-side scope
    return cstr
}
```

The bridge's wrapper synthesiser inserts `runtime.KeepAlive` on every variable that participates in a C-side pointer at function return. This is mechanical and correct by construction.

## String round-trip mechanics

Strings are the most expensive scalar to cross the boundary. The round-trip:

- **Mochi → Go**: Mochi side allocates a C string via its own allocator (`mochi_make_cstr` in the Mochi runtime). The C string is passed to the wrapper. The wrapper calls `C.GoString(cstr)` which copies the bytes into a Go-allocated string. The C string can be freed immediately after the call returns.

- **Go → Mochi**: Wrapper side calls `C.CString(goStr)` which mallocs a C buffer and copies the bytes. The C buffer is returned to Mochi. Mochi side eventually calls `mochi_go_<module>_string_free(cstr)` which (on the wrapper side) calls `C.free(unsafe.Pointer(cstr))`.

The cost: two allocations and two copies per round-trip. For a hot loop processing string arguments, this dominates the cgo overhead. The batched variant amortises by passing a `[]string`-as-`(ptr, len, cap)`-triples slice.

## Slice round-trip mechanics

Slices follow a similar pattern. The `[]T` (for in-table T) round-trip:

- **Mochi → Go**: Mochi side passes a `(ptr, len, cap)` triple. The wrapper reconstructs a Go slice header via `unsafe.Slice(ptr, len)`. The slice elements are read by the Go function; modifications are visible to Mochi because the backing array is shared.

- **Go → Mochi**: Wrapper allocates a backing C array via `C.malloc(sizeof(T) * len)`, copies the slice elements into it, returns the `(ptr, len, cap)` triple. The Mochi side eventually calls `mochi_go_<module>_<type>_free(ptr)` which the wrapper resolves via `C.free`.

For `[]byte`, the bridge can sometimes avoid the copy: if the Go function does not retain the slice past return, the wrapper can pass the Mochi-side pointer directly. The bridge does this when the source function's signature is `func(p []byte) ...` with no goroutine-spawn or channel-send that would extend the slice's lifetime.

## c-archive vs c-shared

Go offers two C-ABI build modes:

- **`-buildmode=c-archive`**: emit a static library (`.a`) plus header. Linked into the consumer's binary statically. Whole-program optimisation possible. Single-binary deployment.

- **`-buildmode=c-shared`**: emit a shared library (`.so` / `.dylib` / `.dll`) plus header. Dynamically loaded by the consumer. Symbol resolution at runtime. Multiple consumers can share the same .so.

MEP-74 uses `c-archive` for the consume direction (wrapper packages linked into the Mochi binary) and offers both `c-archive` and `c-shared` for the publish direction (Mochi-as-library consumed by non-Go via `[go.publish.crate-type-equivalent]`).

The c-archive trade-off:

- Each wrapper module is its own `.a`, linked statically into the final binary. A program importing 10 Go modules has 10 c-archives linked in.
- The Go runtime is embedded once per c-archive but the linker deduplicates the runtime symbols across c-archives (the Go team's c-archive design accommodates this).
- The final binary size grows linearly with the number of imported Go modules (~1-3 MB per typical module, dominated by transitive deps).

The c-shared trade-off:

- The consumer dlopens the .so / .dylib at runtime; if not on the user's library path, the load fails.
- Multiple consumers can share the .so; memory savings on multi-process deployments.
- Cross-platform packaging is harder (the .so / .dylib name and path conventions vary).

MEP-74 defaults to c-archive because single-binary deployment matches the rest of MEP-54's Go-target story.

## Cross-platform header

The `_cgo_export.h` header cgo generates contains the C-side declarations for every `//export`. The header is host-arch-specific:

- `int` width differs (`int` is 32-bit on most platforms; the bridge uses `int64_t` / `int32_t` exclusively to avoid the variation).
- Struct alignment differs (the bridge uses `#pragma pack(8)` to force 8-byte alignment).
- Pointer width differs on 32-bit platforms (the bridge guards on `sizeof(void*) == 8`; 32-bit hosts are not supported on the consume side).

The bridge writes the header to `<workdir>/go_wrap/<module>/<goarch>-<goos>.h` and the build driver picks the right one based on the build target.

## Symbol-naming convention

Every `//export` symbol the wrapper emits is named:

```
mochi_go_<module-path-hash>_<item-name-snake-case>
```

Where:

- `<module-path-hash>` is the first 8 hex characters of `SHA-256(canonical-import-path)`. This avoids long module-path names in symbols and makes collisions across different versions of the same module deterministic (different versions produce different hashes only if their canonical-import-path differs, which it does for v2+).
- `<item-name-snake-case>` is the Go-export item name with PascalCase converted to snake_case.

Examples:

- `github.com/spf13/cobra.NewCommand` → `mochi_go_a1b2c3d4_new_command`
- `github.com/spf13/cobra.Command.SetUse` → `mochi_go_a1b2c3d4_command_set_use`

The hash prefix isolates wrapper packages from one another and prevents global-namespace collisions when multiple wrappers are linked into the same binary.

## The batched-variant optimisation

For functions called in hot loops, the bridge synthesises a batched variant that processes a slice in one cgo crossing. The synthesiser identifies hot-path candidates by inspecting the user's Mochi source for `for x in xs { wrapped_call(x) }`-shaped patterns; when one is found and the source function is opted into `[go.batched]`, the bridge offers both the per-element variant and the batched variant. The user selects per-call via `<alias>.<fn>_batched(xs)`.

The cost amortisation:

- Per-element: 200ns × N cgo crossings = 200N ns
- Batched: 200ns + (50ns × N marshalling) = 200ns + 50N ns

For N = 1,000, the per-element cost is 200μs; the batched cost is 50.2μs. ~4x speedup. For N = 1M, the speedup is ~4x. The crossover is around N = 1.

## ABI stability across Go versions

Go's c-archive ABI has been stable since Go 1.5 (August 2015). Specific guarantees:

- `//export` symbol calling convention is platform's standard C calling convention (cdecl on x86, AAPCS64 on arm64, etc.).
- Multi-return structs are passed by value following the platform's struct-return convention.
- `*C.char` is `char*`, pointer-width.
- The Go runtime initialisation runs lazily on first cgo entry; consumers do not need to call an init function explicitly (older versions of Go required this; since Go 1.10 it is automatic).

The bridge pins the wrapper's `go-version-floor = "1.21"` (the same floor MEP-54 uses). Older Go versions are not supported on the consume side; users wanting older-Go support can override via the manifest.

## Cross-references

- [[02-design-philosophy]] §3 for why c-archive over alternatives.
- [[05-type-mapping]] for the type translation that determines the ABI per item.
- [[08-goroutine-bridge]] for the channel and callback ABI extensions.
- [[12-risks-and-alternatives]] §R1 for the cgo cost discussion.
- [The cgo documentation](https://pkg.go.dev/cmd/cgo) for the upstream ABI documentation.
