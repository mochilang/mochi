---
title: "09. ABI stability"
sidebar_position: 10
sidebar_label: "09. ABI stability"
description: "The extern \"C\" guarantees, the repr(C) requirement for direct-pass struct types, opaque handles for non-repr(C) types, the String / Vec / HashMap round-trip encoding, drop semantics across the wrapper boundary, and static-link vs cdylib build-mode trade-offs."
---

# 09. ABI stability

This note documents the FFI boundary the wrapper crate exposes. The boundary is `extern "C"` plus a small set of hand-rolled encoding conventions; it is what Mochi's runtime calls into.

## The extern "C" surface

Each wrapper symbol is declared as:

```rust
#[no_mangle]
pub extern "C" fn mochi_<crate>_<item>(<C-shaped params>) -> <C-shaped result>
```

The `#[no_mangle]` attribute disables Rust name mangling. The `extern "C"` ABI uses the platform C calling convention (SysV AMD64 on Linux / macOS, x64 Windows on Windows, AAPCS64 on Linux ARM64).

The bridge prefixes every symbol with `mochi_<crate>_` to avoid collisions when multiple wrapper crates are linked into a single Mochi binary. The `<crate>` is the snake_case form of the imported crate name with hyphens converted to underscores.

## C-shaped types

The C-shaped subset the wrapper exposes:

| Mochi type | Rust C-shape | Notes |
|------------|--------------|-------|
| `int` | `i64` | Direct pass. |
| `float` | `f64` | Direct pass. |
| `bool` | `i32` (0 / 1) | C bool is not stable on all toolchains; the wrapper uses i32. |
| `string` | `MochiString` (struct ptr + len + cap) | Owned across the boundary; see encoding below. |
| `list<T>` (T scalar) | `MochiSlice<T>` (struct ptr + len + cap) | T must itself be C-shaped. |
| `map<K, V>` | `MochiMap` (opaque handle) | Serialised as a sequence of key-value pairs. |
| record | `repr(C)` struct of C-shaped fields | The wrapper synthesises a repr(C) parallel struct. |
| sum type | `MochiSum` (tag + payload union) | Tag is i32; payload is encoded per variant. |
| `Option<T>` | `MochiOption<T>` (i32 present-flag + T) | Present flag is 0 for none, 1 for some. |

The `MochiString`, `MochiSlice`, `MochiMap`, `MochiSum`, and `MochiOption` types are defined in `mochi-runtime`'s C header and are shared across all wrapper crates.

## String encoding

`MochiString` is:

```c
typedef struct {
    char* ptr;      // UTF-8 bytes, not null-terminated
    size_t len;     // byte length
    size_t cap;     // allocator capacity (used by free)
} MochiString;
```

Round-trip from Rust:

```rust
#[no_mangle]
pub extern "C" fn mochi_example_greet(name: MochiString) -> MochiString {
    let name_str = unsafe {
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(name.ptr as *const u8, name.len))
    };
    let result = format!("Hello, {}!", name_str);
    let mut bytes = result.into_bytes();
    let ptr = bytes.as_mut_ptr() as *mut c_char;
    let len = bytes.len();
    let cap = bytes.capacity();
    std::mem::forget(bytes);
    MochiString { ptr, len, cap }
}

#[no_mangle]
pub extern "C" fn mochi_example_string_free(s: MochiString) {
    unsafe {
        let _ = Vec::from_raw_parts(s.ptr as *mut u8, s.len, s.cap);
    }
}
```

The Mochi runtime owns the string after the wrapper returns and calls `mochi_<crate>_string_free` when the Mochi GC determines the string is unreachable. The wrapper does not retain references to the string after return.

This convention requires the wrapper and the runtime to use the same allocator. Rust's `Vec` uses the global allocator (jemalloc on default builds, system malloc on `--no-default-features`). The runtime calls into `std::alloc::Global` for symmetry. On targets where the runtime uses a different allocator (e.g., a Mochi-side bump allocator for embedded mode), the round-trip is gated.

## Slice and list encoding

```c
typedef struct {
    void* ptr;      // T elements packed contiguously
    size_t len;
    size_t cap;
} MochiSlice;
```

Element types must themselves be C-shaped: `i64`, `f64`, `bool` (as `i32`), or a repr(C) struct. A `list<list<int>>` is encoded as `MochiSlice` whose elements are `MochiSlice` (3 pointer-words each).

A list of strings is `MochiSlice` whose elements are `MochiString` structs. Each inner MochiString owns its bytes; the outer free walks the slice and calls string-free on each element before freeing the outer buffer.

## Map encoding

`map<K, V>` is encoded as an opaque handle: the wrapper returns a pointer to a Rust-owned `HashMap<K, V>` box, and Mochi operates on it via a small extern API:

```rust
#[no_mangle]
pub extern "C" fn mochi_example_map_string_int_new() -> *mut HashMap<String, i64> {
    Box::into_raw(Box::new(HashMap::new()))
}

#[no_mangle]
pub extern "C" fn mochi_example_map_string_int_insert(
    m: *mut HashMap<String, i64>, k: MochiString, v: i64,
) {
    let m = unsafe { &mut *m };
    let key = unsafe { String::from_utf8_unchecked(Vec::from_raw_parts(k.ptr as *mut u8, k.len, k.cap)) };
    m.insert(key, v);
}

#[no_mangle]
pub extern "C" fn mochi_example_map_string_int_get(
    m: *mut HashMap<String, i64>, k: MochiString,
) -> MochiOption<i64> {
    let m = unsafe { &*m };
    let key = unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(k.ptr as *const u8, k.len)) };
    match m.get(key) {
        Some(v) => MochiOption { present: 1, value: *v },
        None => MochiOption { present: 0, value: 0 },
    }
}

#[no_mangle]
pub extern "C" fn mochi_example_map_string_int_free(m: *mut HashMap<String, i64>) {
    let _ = unsafe { Box::from_raw(m) };
}
```

The handle convention scales to nested structures (`map<string, list<string>>`) and to non-scalar values. The Mochi runtime treats the handle as an opaque pointer with a free callback.

## Sum type encoding

A Mochi sum `type E = A(int) | B(string)` translates to:

```c
typedef struct {
    int32_t tag;
    union {
        int64_t variant_a;
        MochiString variant_b;
    } payload;
} MochiSumE;
```

The wrapper builds the discriminator-and-payload union at the boundary. Variants with no payload have tag-only encoding (the union is sized but unused).

For Rust enums with `#[repr(C)]`, the wrapper can pass the enum directly. For enums without repr(C), the wrapper builds a parallel repr(C) shadow type.

## Repr(C) struct opaque handles

For non-repr(C) Rust types (most user structs), the wrapper exposes the type as an opaque handle: a `Box<T>` raw pointer that Mochi treats as opaque. Operations on the type (method calls, field access via accessor functions) take the handle and dispatch internally.

Example: Rust `pub struct Counter { count: i64, label: String }` without repr(C):

```rust
#[no_mangle]
pub extern "C" fn mochi_example_counter_new(count: i64, label: MochiString) -> *mut Counter {
    let label = unsafe { String::from_utf8_unchecked(Vec::from_raw_parts(label.ptr as *mut u8, label.len, label.cap)) };
    Box::into_raw(Box::new(Counter { count, label }))
}

#[no_mangle]
pub extern "C" fn mochi_example_counter_increment(c: *mut Counter, by: i64) -> i64 {
    let c = unsafe { &mut *c };
    c.count += by;
    c.count
}

#[no_mangle]
pub extern "C" fn mochi_example_counter_free(c: *mut Counter) {
    let _ = unsafe { Box::from_raw(c) };
}
```

A struct that is both `repr(C)` AND has all-C-shaped fields can be passed directly without the handle indirection. The bridge prefers the direct path when possible (it avoids a heap allocation and a free callback per round-trip).

## Drop semantics

The wrapper owns Rust-side memory until Mochi calls the matching `_free` function. Mochi's GC tracks each opaque handle and calls free at collection time.

A panic inside the wrapper (e.g., a `unwrap()` on a Rust Result) unwinds through the `extern "C"` boundary. Rust's `panic = "abort"` profile is required to make this safe: the wrapper crate sets `panic = "abort"` in its Cargo.toml, and the runtime catches the abort signal and converts it to a Mochi panic.

```toml
# rust_wrap/<crate>/Cargo.toml
[profile.release]
panic = "abort"

[profile.dev]
panic = "abort"
```

An alternative is `extern "C-unwind"` (stable since Rust 1.71), which lets Rust panics propagate across the boundary. The bridge does not use C-unwind in v1: the runtime side would need a per-target unwind handler, which is non-trivial to ship across Linux / macOS / Windows. Future sub-phase.

## Static link vs cdylib

The wrapper crate can build as `rlib` (Rust archive, statically linked into the consumer) or `cdylib` (C dynamic library, dynamically loaded). The default is `rlib` because:

- A single static binary is the Mochi packaging default.
- No dynamic-loader dependency at runtime (`dlopen` / `LoadLibrary`).
- No symbol-mangling concerns about cross-shared-object visibility.

The user opts into cdylib via:

```toml
[rust.publish]
crate-type = "cdylib"
```

This is required for the Mochi-as-Rust-library publish path (downstream consumers need a cdylib for C FFI). For the consume direction (`import rust`), rlib is always used.

When the wrapper is rlib, the consumer (the Mochi binary) statically links all symbols, including duplicates from different wrapper crates. The linker collapses duplicates by symbol name; the `mochi_<crate>_` prefix ensures collision-free linkage.

## Symbol visibility

The wrapper crate's `Cargo.toml`:

```toml
[lib]
crate-type = ["staticlib", "rlib"]

[profile.release]
panic = "abort"
strip = "symbols"
lto = "fat"
opt-level = 3
codegen-units = 1
```

`strip = "symbols"` removes debug symbols from the release artefact. `lto = "fat"` enables link-time optimisation across the wrapper crate boundary. `codegen-units = 1` lets the inliner see the whole crate.

The `staticlib` crate-type produces a `.a` file that the Mochi linker absorbs. The `rlib` crate-type is also emitted so Cargo's resolver can chain across multiple wrapper crates in the same workspace.

## ABI versioning

Each wrapper exposes its ABI version via a sentinel symbol:

```rust
#[no_mangle]
pub static MOCHI_WRAPPER_ABI_VERSION: u32 = 1;
```

The Mochi runtime reads this at link time and refuses to dlopen / dynamically link a wrapper whose ABI version disagrees with the runtime's expectation. The ABI version bumps when the MochiString / MochiSlice / MochiOption / MochiSum layout changes.

ABI version 1 is the v1 shape. A v2 ABI would, for example, add a generation counter to MochiString for use-after-free detection. The bridge handles ABI drift via a per-wrapper rebuild: when the runtime ABI bumps, the bridge regenerates and rebuilds all wrapper crates.

## Cross-references

- [[05-type-mapping]] for the Rust-side types that drive the C-shape.
- [[08-async-bridge]] for the block_on entry into wrapper functions.
- [[10-lifetimes-and-ownership]] for the borrow-to-clone strategy at the wrapper layer.
- [MEP-73 §6](/docs/mep/mep-0073#6-build-orchestration) for how the wrapper crate fits into the workspace build.
- [Rust Reference: Type Layout](https://doc.rust-lang.org/reference/type-layout.html) for the repr(C) guarantees.
