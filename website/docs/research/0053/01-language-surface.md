---
title: "01. Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "Mochi features mapped onto Rust 1.78+ lowering obligations for MEP-53."
---

# 01. Language surface

This note enumerates the Mochi surface forms the Rust transpiler must accept and the Rust shape each lowers to. The exhaustive table is in [MEP-53 §3](/docs/mep/mep-0053#3-surface-syntax-lowering); this note frames the categories and calls out the cases that drove specific Rust-target decisions.

## Scope

Mochi's surface is roughly: scalars, control flow, records, sum types, closures, lists / maps / sets, query DSL, Datalog, agents, channels, streams, async, try / catch / panic, FFI, fetch, JSON, LLM generate. Each maps to a Rust idiom, but several mappings have non-trivial trade-offs.

## Scalars and control flow

`int` → `i64`, `float` → `f64`, `bool` → `bool`, `string` → `String`. Arithmetic uses native ops; integer division and modulo route through `mochi_runtime::check::div_i64` / `mod_i64` so that vm3's "panic code 5 on zero divisor" semantics are preserved exactly (Rust's built-in `/` and `%` already panic on zero for ints, but the panic message format differs from vm3's; routing through the runtime gives uniform panic codes).

`for i in lo..hi` uses Rust's exclusive `..` range, which matches Mochi's exclusive range exactly. `for x in xs` lowers to `for x in xs.iter().cloned()`, with the clone gated by the [[codegen-design]] colour pass.

## Records and sum types

`record` and anonymous `type X = { ... }` both lower to:

```rust
#[derive(Clone, Debug, PartialEq, Default)]
struct Foo { /* ... */ }
```

The four derives are load-bearing: `Clone` for functional update semantics, `Debug` for `print(foo)` (lowered to `print_str(format!("{:?}", foo))`), `PartialEq` for `==`, `Default` for `spawn AgentType()` zero-value construction.

`type T = A | B` lowers to a tagged enum:

```rust
#[derive(Clone, Debug, PartialEq)]
enum T { A { /* ... */ }, B { /* ... */ } }
```

Self-referential variants get Box-wrapped at the recursive position. The match-to-decision-tree pass (Maranget 2008) is reused from the C target via clower.

## Closures

`Box<dyn Fn(...)>` is the lowering target. Generic `impl Fn` cannot be stored in struct fields, returned from functions with multiple bodies, or kept in homogeneous lists, all of which Mochi's source language permits. `Box<dyn Fn>` accepts a runtime indirection cost for surface compatibility.

Captures lower to explicit `move` clauses with `clone()` calls computed by the closure-conversion pass. Recursive closures use a `Rc<RefCell<Option<Box<dyn Fn>>>>` Y-combinator trampoline.

`FnMut` is detected at lower time and switches the box type to `Box<dyn FnMut>` when the closure body mutates a captured `&mut` binding.

## Collections

`Vec<T>` for lists, `HashMap<K, V>` for maps, `HashSet<T>` for sets. Iteration order of HashMap and HashSet is unspecified by Rust stdlib; Mochi's omap (insertion-ordered map) is lowered via `BTreeMap<K, V>` when the lower pass detects an ordered-iteration requirement (e.g., printing the map, or `keys(m)` consumption).

## Concurrency primitives

Channels (`Rc<RefCell<VecDeque<T>>>`), streams (`Rc<RefCell<Vec<Rc<RefCell<VecDeque<T>>>>>>`), and agents (plain structs) are all single-thread. No `Arc`, no `Mutex`, no `Thread::spawn`. Mochi's `async expr` lowers to `expr` (immediate evaluation); `await fut` lowers to `fut` (identity). The async colouring is a typecheck-time pass with no runtime effect.

Rationale: Mochi's source language does not expose a thread-spawn primitive at user level. Forcing every Mochi program through `Arc<Mutex<...>>` for the channel and stream primitives would pay a synchronisation tax that the source language does not require. Users who need real OS threads can call into `std::thread` via FFI.

## try / catch / panic

`try { ... } catch e { ... }` lowers to `match mochi_runtime::panic::catch(|| { ... }) { Some(code) => ..., None => {} }`. The runtime's catch wraps `panic::catch_unwind` with `panic::AssertUnwindSafe` and downcasts the payload to extract an i64 code. Stdlib panics (out-of-bounds, divide-by-zero) are message-string-mapped to canonical codes (4, 5). User panics via `panic(code)` lower to `panic::panic_any(code)`.

## FFI

`extern fn name(a: T): U from "header.h"` lowers to a Rust `extern "C"` block plus a sidecar `cffi/` directory carrying the user-supplied C source and a `build.rs` that runs cc-rs. String arguments round-trip through `CString::new(s).unwrap().as_ptr()`.

## Fetch + JSON + LLM

`fetch <url>` / `httpGet(url)` → `mochi_runtime::fetch::get(url)` (HTTP/1.1 over `std::net::TcpStream`, no TLS).

`json_decode(s)` → `mochi_runtime::json::decode(s)` (top-level object decoder returning `HashMap<String, String>` with non-string values string-coerced).

`generate <provider> { prompt: p, model: m }` → `mochi_runtime::llm::call(provider, prompt)` (cassette replay over `MOCHI_LLM_CASSETTE_DIR`).

## Cross-references

- [[design-philosophy]] for the rationale behind each idiom choice.
- [[type-lowering]] for the precise primitive mapping.
- [[agent-streams]] for the single-thread runtime model.
- [[codegen-design]] for the rtree IR and colour pass.
- [MEP-53 §3](/docs/mep/mep-0053#3-surface-syntax-lowering) for the exhaustive normative table.
