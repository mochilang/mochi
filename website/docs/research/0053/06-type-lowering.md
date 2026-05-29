---
title: "06. Type-system lowering"
sidebar_position: 7
sidebar_label: "06. Type lowering"
description: "Mochi types onto i64 / f64 / String / Vec / HashMap / HashSet / struct / tagged enum / Box<dyn Fn>, and the Rc<RefCell> single-thread choice."
---

# 06. Type-system lowering

This note maps each Mochi type to its Rust lowering and frames the trade-offs in the non-obvious cases.

## Scalar types

| Mochi | Rust | Note |
|-------|------|------|
| `int` | `i64` | Mochi's int is bounded 64-bit signed. |
| `float` | `f64` | IEEE-754 double precision. |
| `bool` | `bool` | Native Rust bool. |
| `string` | `String` | UTF-8, char-aware via `mochi_runtime::strings`. |

`i64` not `isize`: Mochi int is portable, must behave identically on 32-bit and 64-bit hosts. `isize` would vary.

`String` not `&str`: Mochi strings are owned values that can be returned from functions, stored in records, mutated via concatenation. `&str` would force lifetime annotations throughout the emitted code, which the colour pass would have to navigate. Owned `String` is simpler; the colour pass handles clones.

## Collection types

| Mochi | Rust | Note |
|-------|------|------|
| `list<T>` | `Vec<T>` | Native Rust vec. |
| `map<K, V>` | `HashMap<K, V>` | Iteration order unspecified. |
| `omap<K, V>` | `BTreeMap<K, V>` | Sorted iteration order matches vm3's order-preserving iteration. |
| `set<T>` | `HashSet<T>` | Iteration order unspecified. |
| `oset<T>` | `BTreeSet<T>` | Sorted iteration order. |

Mochi distinguishes `map` (unordered) from `omap` (insertion-ordered, but vm3 actually iterates in insertion order via a slice + map combo). The Rust lowering uses BTreeMap for omap, which gives **sorted** order, not insertion order. This is a small semantic deviation: Mochi-on-Rust output for `keys(omap)` is sorted by key, while Mochi-on-vm3 is in insertion order. For most Mochi programs (where insertion order matches sort order by construction), this is invisible; for programs that insert in non-sort order, the outputs diverge.

The deviation is acceptable for MEP-53 because: (1) no existing fixture exercises the insertion-order-non-equals-sort-order case, (2) the C target (MEP-45) has the same deviation, (3) emitting an `indexmap`-backed map would add a dep that the embedded gate would have to drop.

## Record and sum types

`record User { id: int, name: string }` and anonymous `type Pair = { a: int, b: int }`:

```rust
#[derive(Clone, Debug, PartialEq, Default)]
struct User { id: i64, name: String }
```

The four derives are load-bearing (see [[language-surface]]). `Hash` is not auto-derived because Mochi records can contain `f64` which is not `Hash` (NaN != NaN). When a record needs to be a HashMap key, the lower pass emits `#[derive(Hash, Eq)]` additionally and rejects f64 fields at typecheck.

`type Shape = Circle { r: float } | Rect { w: float, h: float } | Empty`:

```rust
#[derive(Clone, Debug, PartialEq)]
enum Shape {
    Circle { r: f64 },
    Rect { w: f64, h: f64 },
    Empty,
}
```

Self-referential variants get Box-wrapped at the recursive position:

```mochi
type List = Cons(int, List) | Nil
```

```rust
enum List {
    Cons(i64, Box<List>),
    Nil,
}
```

The Box is required because Rust enums must be sized at compile time; `enum E { V(E), N }` would have infinite size. The Box adds one heap allocation per recursive node, which is acceptable for the Mochi semantic model.

## Function types

`fun(T) -> U`:

```rust
Box<dyn Fn(T) -> U>
```

When the lower pass detects an `FnMut` requirement (closure body mutates a captured `&mut` binding), the box switches to `Box<dyn FnMut>`. When it detects an `FnOnce` requirement (move-out of a captured value inside the body), the box switches to `Box<dyn FnOnce>`. The detection is conservative: false positives push toward FnMut / FnOnce when Fn would have sufficed, which the user sees as a slightly stricter call-site shape but never as a compile failure.

The box is heap-allocated; calls go through a vtable dispatch. The colour pass mitigates the heap cost for capture by eliding `.clone()` of captured values when they're Copy.

## Concurrency types

| Mochi | Rust | Note |
|-------|------|------|
| `chan<T>` | `mochi_runtime::chan::Chan<T>` | Wraps `Rc<RefCell<VecDeque<T>>>`. |
| `stream<T>` | `mochi_runtime::stream::Stream<T>` | Wraps `Rc<RefCell<Vec<Rc<RefCell<VecDeque<T>>>>>>`. |
| `Sub<T>` | `mochi_runtime::stream::Sub<T>` | Wraps `Rc<RefCell<VecDeque<T>>>`. |
| `agent A { ... }` | `struct A { ... } impl A { fn new() -> Self ... }` | Plain struct. |
| `async fun(): T` | `fun(): T` | Async coloring is typecheck-only; no runtime effect. |
| `await fut` | `fut` | Identity. |

`Rc<RefCell>` not `Arc<Mutex>`: see [[agent-streams]] for the single-thread rationale.

## Error types

`panic` does not have a user-visible type; it's a control-flow effect. `try { ... } catch e { ... }` binds `e: i64` from the panic payload.

There is no Mochi `Result<T, E>` type. Mochi programs that want explicit error returns use sum types (`type Result = Ok(T) | Err(E)`) and pattern-match.

## Reference / pointer types

Mochi has no pointer type, no reference type at the surface level. All values are owned. The Rust target follows: no `&T` or `&mut T` in the emitted public surface, only as implementation details of method signatures (e.g., `&self` in impl methods).

The runtime crate uses `&str` and `&[T]` in some signatures (e.g., `mochi_runtime::strings::index(s: impl AsRef<str>, i: i64)`); this is a Rust-side ergonomics choice that doesn't leak through to the Mochi-visible API.

## Cross-references

- [[language-surface]] for the per-feature lowering shape.
- [[runtime]] for the runtime crate types.
- [[agent-streams]] for the single-thread concurrency model.
- [[codegen-design]] for the colour pass that handles the Clone semantics.
