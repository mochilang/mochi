---
title: "Phase 6. Closures"
sidebar_position: 8
sidebar_label: "Phase 6. Closures"
description: "MEP-53 Phase 6, closures lowered to Box<dyn Fn> with explicit move-capture clauses."
---

# Phase 6. Closures (`Box<dyn Fn>`)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-29 07:35 (GMT+7) |
| Tracking issue | — (umbrella) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | ad43642b16 |

## Gate

`TestPhase6Closures` walks `tests/transpiler3/rust/fixtures/phase06-closures/` (34 fixtures) and asserts byte-equal stdout. Coverage: lambda assigned to var, lambda returned from function, lambda passed as arg, lambda capturing by value, lambda capturing recursive bindings, higher-order ops (map, filter, reduce), nested closures.

## Lowering decisions

`let f = fun(x: int): int => x + n` (where `n` is in scope) lowers to:

```rust
let f: Box<dyn Fn(i64) -> i64> = Box::new({
    let n = n.clone();
    move |x: i64| -> i64 { x + n }
});
```

The capture clause `{ let n = n.clone(); move |...| { ... } }` is computed by `transpiler3/rust/lower/closure.go`. The clone is mandatory for non-Copy types (String, Vec, structs); the colour pass elides it for `Copy` types (i64, f64, bool, &str). Closures escape into the heap via `Box<dyn Fn>` so they can be stored in struct fields, returned, and homogenised in lists.

Higher-order builtins trampoline through `.call`-equivalent invocation:

```rust
let mapped: Vec<i64> = xs.iter().map(|x| f(x.clone())).collect();
```

The `x.clone()` is required because Box<dyn Fn(i64) -> i64> takes `i64` by value; with f returning a non-Copy result, that result is also cloned on print.

Recursive closures use the `Y-combinator` trick: a `Rc<RefCell<Option<Box<dyn Fn>>>>` is allocated, the closure body references it via `f.borrow().as_ref().unwrap()(arg)`, then the body itself is assigned via `*f.borrow_mut() = Some(Box::new(...))`. This is the only place the runtime touches RefCell at the user-visible level; phase 6 fixtures exercise three recursive closures (factorial, fibonacci, ackermann).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/closure.go` | Capture analysis + closure conversion |
| `transpiler3/rust/lower/recursive_closure.go` | Y-combinator trampoline for recursive closures |
| `transpiler3/rust/colour/colour.go` | Wire Copy-vs-clone elision (no-op until phase 6) |
| `transpiler3/rust/build/phase06_test.go` | 34-fixture gate |
| `tests/transpiler3/rust/fixtures/phase06-closures/*.mochi` + `.out` | 34 fixtures |

## Test set

- `TestPhase6Closures/<fixture>` for each `.mochi` in the fixture directory (34 fixtures).

## Closeout notes

The colour pass deserves more lines than it has. It is a forward-borrow-flow analysis that decides, at each value use site, whether `.clone()` is required. The rule: if a value is used after the current site AND the consuming function takes it by value AND the type is not Copy, then clone. The pass is rerun until fixpoint because elision at one site can shift others. Without the colour pass, the emitted code typechecks but clones aggressively (every closure capture clones every captured binding); with it, closures over Copy types are clone-free.
