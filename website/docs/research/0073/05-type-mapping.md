---
title: "05. Type mapping table"
sidebar_position: 6
sidebar_label: "05. Type mapping"
description: "The complete closed Rust-to-Mochi type translation table, the refusal cases (lifetimes, generic bounds beyond Clone, impl Trait, dyn Trait, raw pointers, unsafe fn, Pin / Future / Cow), the explicit `[rust.monomorphise]` declaration rule for generic instantiation, and the `&str` vs `String` parameter handling."
---

# 05. Type mapping table

This note enumerates the closed translation table the bridge uses to map Rust types to Mochi types. Items whose entire signature falls inside the table are translated; items with any out-of-table type are skipped with a `SkipReport`.

## Scalar types

| Rust | Mochi | Notes |
|------|-------|-------|
| `i8`, `i16`, `i32` | `int` | Widened to i64 at the FFI boundary; range-checked in the wrapper. |
| `i64` | `int` | Native fit. |
| `i128` | (refused) | No i128 in Mochi. User must override. |
| `u8`, `u16`, `u32` | `int` | Widened to i64 at the FFI boundary. |
| `u64` | `int` | Translates as i64; values >= 2^63 panic the wrapper. |
| `u128` | (refused) | No u128 in Mochi. |
| `usize`, `isize` | `int` | Translates as i64 on 64-bit hosts. 32-bit hosts panic on values >= 2^31. |
| `f32` | `float` | Widened to f64 at the FFI boundary. |
| `f64` | `float` | Native fit. |
| `bool` | `bool` | Native fit. |
| `char` | `string` | Translated as a 1-codepoint Mochi string (Mochi has no char type). |
| `()` | unit (no Mochi binding emitted) | A function returning `()` becomes a Mochi `fun(...): unit`. |

## String types

| Rust | Mochi | Notes |
|------|-------|-------|
| `String` | `string` | Owned. Round-trip via `*const c_char` + length plus `mochi_<crate>_string_free`. |
| `&str` | `string` (by copy) | Param: copied into a Rust `String` inside the wrapper. Return: refused (the borrow has no Mochi lifetime). |
| `&'static str` | `string` | Param and return both ok (static lifetime erases cleanly). |
| `Cow<str>` | (refused) | Cow's borrow-vs-owned indeterminacy has no Mochi analogue. |
| `OsString`, `&OsStr` | (refused, v1) | Platform-dependent encoding. Future sub-phase. |
| `PathBuf`, `&Path` | (refused, v1) | Same. Use `String` and convert in the wrapper. |
| `CString`, `&CStr` | (refused, v1) | Future. |

## Collection types

| Rust | Mochi | Notes |
|------|-------|-------|
| `Vec<T>` where T in table | `list<T>` | Owned. Round-trip via `(ptr, len, cap)` plus `_free`. |
| `&[T]` where T is i64 / f64 / bool | `list<T>` (by copy) | Param: copied into a Vec inside the wrapper. |
| `&[u8]` | `string` or `list<int>` (param choice via `[rust.bytes]` config) | Default `list<int>`; can opt into `string` via manifest. |
| `&mut [T]` | (refused) | Mutable borrow has no Mochi shape. |
| `[T; N]` | `list<T>` | Fixed-size array. The wrapper bounds-checks at the FFI boundary. |
| `HashMap<K, V>` where K = `String` and V in table | `map<string, V>` | Owned. Round-trip via serialised key-value pairs plus `_free`. |
| `HashMap<K, V>` where K is integer type and V in table | `map<int, V>` | Same. |
| `BTreeMap<K, V>` where K is `String` / integer and V in table | `omap<K, V>` | Sorted iteration order. |
| `HashSet<T>` where T in table | `set<T>` | Owned. |
| `BTreeSet<T>` where T in table | `oset<T>` | Sorted iteration. |
| `VecDeque<T>` | `list<T>` (by copy at the boundary) | The wrapper materialises a Vec. |

## Option, Result, Tuple

| Rust | Mochi | Notes |
|------|-------|-------|
| `Option<T>` where T in table | `T?` (Mochi's optional) | None becomes nil; Some(v) becomes v. |
| `Result<T, E>` where T and E both in table | desugared try-catch | A function returning `Result<T, E>` becomes a Mochi `fun(...): T` that raises a Mochi panic with the `E` value when the Result is `Err`. |
| `(A, B)` where A and B in table | `tuple<A, B>` | Pairs. |
| `(A, B, C)` where all in table | `tuple<A, B, C>` | Triples. Higher arity supported up to 12 (Rust's traits-implemented-up-to ceiling). |

## Struct types

A struct with all-fields-in-table and `Plain` (named-field) variant translates to a Mochi record:

```rust
pub struct Counter {
    pub count: i64,
    pub label: String,
}
```

→

```mochi
record Counter {
    count: int,
    label: string,
}
```

A struct with any out-of-table field type is refused. A tuple struct (`pub struct Wrapper(pub i64)`) is refused; the user can hand-author an `extern type` override.

A struct that derives `#[derive(Clone)]` is required: the wrapper takes structs by value across the FFI boundary, which requires Clone. Structs without Clone are refused.

## Enum types

| Rust | Mochi | Notes |
|------|-------|-------|
| Unit-only enum (`enum E { A, B, C }`) | Mochi sum (`type E = A \| B \| C`) | Compact integer encoding across FFI. |
| Tuple-variant enum (`enum E { A(i64), B(String) }`) | Mochi sum (`type E = A(int) \| B(string)`) | Each variant gets a constructor. |
| Struct-variant enum (`enum E { A { x: i64 } }`) | Mochi sum (`type E = A { x: int }`) | Each variant gets a named-payload constructor. |
| Mixed enum | Same | Combined translation. |
| Enum with `#[repr(C)]` | Same | The wrapper uses the repr-C tag-and-payload encoding. |
| Enum without `#[repr(C)]` | Same | The wrapper builds a tagged encoding manually. |

## Function items

A function whose signature has all-types-in-table and is not behind an unsupported attribute becomes a Mochi `extern fn`:

```rust
pub fn compute(name: &str, factor: i64) -> Result<f64, String>
```

→

```mochi
extern fn compute(name: string, factor: int): float from rust "compute"
```

Function attributes:

- `#[must_use]`: respected via a Mochi `@must_use` annotation.
- `unsafe fn`: refused unless `[rust.capabilities] unsafe = true` is declared.
- `async fn`: lowered via the tokio bridge (see [[08-async-bridge]]).
- `pub(crate)`, `pub(super)`: not visible in rustdoc public surface; skipped.
- `extern "C" fn`: directly translatable (skips the wrapper layer).

Methods (`impl Foo { pub fn bar(&self, ...) }`) become `extern fn` items whose first parameter is the receiver type:

```rust
impl Counter {
    pub fn increment(&mut self, by: i64) -> i64
}
```

→

```mochi
extern fn counter_increment(c: Counter, by: int): int from rust "Counter::increment"
```

The receiver is taken by value across the FFI boundary; the wrapper materialises the `&mut self` reference internally. For methods returning `Self`, the wrapper returns by-value.

## Generic items: `[rust.monomorphise]`

Generic functions (`pub fn collect<T>(...) -> Vec<T>`) are refused at default ingest because the type parameter has no concretisation. The user enables import by enumerating concretisations in `mochi.toml`:

```toml
[rust]
monomorphise = [
    { item = "Vec::with_capacity", T = "i64" },
    { item = "Vec::with_capacity", T = "String" },
    { item = "serde_json::from_str", T = "MyStruct" },
]
```

For each enumeration, the bridge generates a monomorphised extern fn:

```mochi
extern fn vec_with_capacity_int(cap: int): list<int> from rust "Vec::<i64>::with_capacity"
extern fn vec_with_capacity_string(cap: int): list<string> from rust "Vec::<String>::with_capacity"
```

The naming convention is `<base>_<T-suffix>` where the suffix is the lowercase type name with `<>` removed.

Generic items with multiple type parameters (`HashMap::insert<K, V>`) require all parameters to be concretised:

```toml
{ item = "HashMap::insert", K = "String", V = "i64" }
```

Generic bounds beyond `Clone` (e.g., `where T: Serialize + Send + 'static`) are refused: even with explicit monomorphisation, the bridge cannot verify that the concretisation satisfies the bound from rustdoc JSON alone. Users in this case should hand-author the binding.

## Refusal reasons

A `SkipReport` entry records:

```go
type SkipReport struct {
    ItemPath  string  // e.g., "tokio::sync::mpsc::Receiver::poll_recv"
    Reason    SkipReason  // enum
    Detail    string  // free-text explanation
}

type SkipReason int
const (
    SkipLifetime SkipReason = iota  // contains a non-'static lifetime
    SkipGeneric                     // unconcretised generic parameter
    SkipImplTrait                   // impl Trait in return position
    SkipDynTrait                    // dyn Trait
    SkipRawPointer                  // *const T or *mut T
    SkipUnsafe                      // unsafe fn without capability opt-in
    SkipPin                         // Pin<Box<T>> or Pin<&T>
    SkipFuture                      // Box<dyn Future> direct return
    SkipCow                         // Cow<str> or Cow<[T]>
    SkipOsString                    // OsString / OsStr / PathBuf
    SkipNonClone                    // struct without Clone derive
    SkipPubCrate                    // pub(crate) or pub(super)
    SkipTrait                       // Trait definition (v1 has no trait dispatch)
    SkipMacro                       // macro_rules! or proc-macro
    SkipConstant                    // const item (v1 doesn't bind consts)
    SkipExternFnUnsafe              // extern "C" unsafe fn
    SkipCustomAbi                   // extern "stdcall" / extern "fastcall" / etc.
    SkipQualifiedPath               // <T as Trait>::Item
    SkipOpaqueTypeAlias             // type T = impl Trait
)
```

The bridge emits the `SkipReport` list to `<workdir>/rust_wrap/<crate>/SKIPPED.txt`:

```
SKIPPED: tokio::sync::mpsc::Receiver::poll_recv
  Reason: SkipPin
  Detail: parameter `Pin<&mut Self>` cannot be expressed in Mochi
  Override: write `extern fn poll_recv(...) ... custom`

SKIPPED: tokio::spawn
  Reason: SkipGeneric
  Detail: signature `spawn<T: Future>(f: T) -> JoinHandle<T::Output>` has unconcretised generic T
  Override: add `{ item = "tokio::spawn", T = "MyFuture" }` to [rust.monomorphise]
```

The user reads SKIPPED.txt to understand which items they did not get and how to opt them in.

## Cross-references

- [[02-design-philosophy]] §6 for why the table is closed.
- [[04-rustdoc-json-ingest]] for the parsed surface that feeds the table.
- [[09-abi-stability]] for the wrapper-side encoding of each translated type.
- [[10-lifetimes-and-ownership]] for the borrow-to-clone strategy underpinning the table.
- [MEP-73 §3](/docs/mep/mep-0073#3-lockfile-extension-rust-package) for the type-table-related lockfile fields.
