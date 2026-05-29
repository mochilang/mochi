---
title: "Phase 4. Records (Data.define)"
sidebar_position: 8
sidebar_label: "Phase 4. Records (Data.define)"
description: "MEP-56 Phase 4, record declarations to Ruby Data.define, field access, structural equality."
---

# Phase 4. Records (Data.define)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | 4738082cde |

## Gate

`TestPhase4Records` in `transpiler3/ruby/build/phase04_test.go`: two subtests, `record_decl_access` and `record_equality`. Each subtest compiles a Mochi source via `Driver.Build` to a `.rb`, executes the script under the resolved Ruby toolchain with `-I mochi-runtime/lib`, and diffs stdout against the recorded expectation. `record_decl_access` additionally asserts that the rendered `.rb` contains the substrings `User = Data.define(:id, :name)`, `User.new(id: 7, name: "Mochi")`, `u.id`, and `u.name`, so the lowering shape is locked, not just the runtime output.

## Lowering decisions

Each `aotir.RecordDecl` lowers to a single `rtree.DataDecl{Name, Fields}` rendered as `Name = Data.define(:f1, :f2, ...)` (lower.go lines 63 to 70). `Data.define` is the canonical Ruby 3.2+ immutable value class: it auto-generates a constructor accepting positional or keyword arguments, `Equals`/`Hash` based on every field, `inspect` printing `#<data Name f1=v1, f2=v2>`, and `#deconstruct`/`#deconstruct_keys` for `case/in` pattern matching. All four of those align with Mochi record semantics with no glue code.

Record declarations are appended to the outer module's `progDecls` slice before the nested `Main` module, so the emitted file has the shape `module Hello; User = Data.define(:id, :name); module Main; ...; end; end`. Putting the records inside the outer wrapper (not at top level) keeps them namespaced so two Mochi modules can declare the same record name without collision once multi-file builds land.

`aotir.RecordLit` lowers to `TypeName.new(field1: value1, field2: value2)` via a `MethodCall` with `UseParens: true` and `RawExpr` field args of the form `name: value` (lower.go lines 671 to 685). Keyword-arg construction is preferred over positional because Mochi record literals are written `User { id: 7, name: "Mochi" }`, which already commits to named fields, and keyword args make the rendered Ruby self-documenting. `aotir.FieldAccess` lowers to a no-arg `MethodCall{Receiver: recv, Method: e.FieldName}`, which renders as `receiver.fieldname`. Ruby's `Data` auto-defines a reader method per field, so `u.id` and `u.name` resolve without a writer (records are immutable).

Structural equality falls out for free: `BinEqRec` lowers via `rubyBinOp` to `==`, and `Data` instances compare equal iff their class and all field values compare equal. The `record_equality` subtest depends on this: two `Pair { a: 1, b: 2 }` literals produce two distinct `Pair` instances that `==` returns `true` for, without the lowerer emitting any `Equals` override.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | Record declarations appended as `rtree.DataDecl` before the `Main` module; `RecordLit` → `TypeName.new(field: value, ...)`; `FieldAccess` → `receiver.field`; `BinEqRec`/`BinNeRec` reuse `==`/`!=` via `rubyBinOp` |
| `transpiler3/ruby/rtree/` | `DataDecl` node rendering `Name = Data.define(:f1, :f2, ...)` |
| `transpiler3/ruby/build/phase04_test.go` | `TestPhase4Records` with 2 subtests + render-shape assertions |

## Test set

- `TestPhase4Records/record_decl_access`, `record_equality`.

## Closeout notes

Phase 4 landed on CRuby 3.4 with both subtests green. The render-shape assertion in `record_decl_access` (`wantInRb` substrings) was deliberate: a future refactor that moved record literals to positional construction would still produce correct output but would silently break match patterns and reader-method generation. Locking the keyword form in the test stops that drift. Key implementation insight: `Data.define` did not exist before Ruby 3.2, which is why §1 of MEP-56 sets the floor there; pre-3.2 would have forced `Struct.new` (mutable, not what Mochi wants) plus a manual `==` override.
