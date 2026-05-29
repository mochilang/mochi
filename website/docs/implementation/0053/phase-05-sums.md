---
title: "Phase 5. Sum types"
sidebar_position: 7
sidebar_label: "Phase 5. Sums"
description: "MEP-53 Phase 5, sum types lowered to Rust tagged enums with pattern matching."
---

# Phase 5. Sum types (tagged enums)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-28 (GMT+7) |
| Landed         | 2026-05-29 07:35 (GMT+7) |
| Tracking issue | — (umbrella) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | 26217cbcec |

## Gate

`TestPhase5Sums` walks `tests/transpiler3/rust/fixtures/phase05-sums/` (27 fixtures) and asserts byte-equal stdout. Coverage: tag-only variants, variants with positional fields, variants with named fields, recursive types (Tree, List), match with wildcards, match with literal patterns, match with binding patterns, match with guards.

## Lowering decisions

```mochi
type Shape = Circle { r: float } | Rect { w: float, h: float } | Empty
```

lowers to:

```rust
#[derive(Clone, Debug, PartialEq)]
enum Shape {
    Circle { r: f64 },
    Rect { w: f64, h: f64 },
    Empty,
}
```

Variant constructors are emitted as `Shape::Circle { r: 2.0 }` (struct-like) or `Shape::Cons(x, xs)` (tuple-like). Match is direct:

```rust
match s {
    Shape::Circle { r } => /* arm */,
    Shape::Rect { w, h } => /* arm */,
    Shape::Empty => /* arm */,
}
```

Recursive variants (`Cons(int, List)`) require indirection — `Box<List>` is wrapped around the recursive position by the lower pass when the type is self-referential.

Match decision trees are pre-built by the clower pass (Maranget 2008 algorithm) so the emitted `match` is exhaustive without redundant arms. The Rust compiler's own exhaustiveness check is the secondary gate.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/sum.go` | Sum type and match lowering |
| `transpiler3/rust/lower/recursive.go` | Box-wrapping for self-referential variants |
| `transpiler3/rust/build/phase05_test.go` | 27-fixture gate |
| `tests/transpiler3/rust/fixtures/phase05-sums/*.mochi` + `.out` | 27 fixtures |

## Test set

- `TestPhase5Sums/<fixture>` for each `.mochi` in the fixture directory (27 fixtures).

## Closeout notes

Pattern matching against `&` references vs owned values is the subtle piece. When matching on `xs.iter().next()` (which is `Option<&T>`), the binding inside the variant is `&Inner`, but downstream code wants `Inner`. Phase 5 always matches on owned values (using `.cloned()` before match) so the binding is uniform; the colour pass in phase 6 can later promote some match scrutinees back to `&` form to avoid the clone.
