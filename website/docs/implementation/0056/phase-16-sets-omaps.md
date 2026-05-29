---
title: "Phase 16. Sets and ordered maps"
sidebar_position: 20
sidebar_label: "Phase 16. Sets and ordered maps"
description: "MEP-56 Phase 16, set<T> lowered to Ruby Set, omap<K,V> lowered to insertion-ordered Hash."
---

# Phase 16. Sets and ordered maps

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | none |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | a39456b067 |

## Gate

`TestPhase16SetsOMaps` in `transpiler3/ruby/build/phase16_test.go`: six subtests (`set_add_has`, `set_len`, `set_forin`, `omap_literal`, `omap_set`, `omap_has_len`). Each subtest compiles a Mochi program using either `set<T>` literals + `add`/`has`/`len`/`for in` or `omap<K,V>` literals + index get/set + `has`/`len`, then runs the emitted `.rb` under the resolved Ruby toolchain with `-I mochi-runtime/lib` and diffs stdout (e.g. `omap_set` expects `10\n20\n` after mutating an `omap{"x": 10}` with `m["y"] = 20`).

## Lowering decisions

`set<T>` lowers to Ruby's `Set` class (auto-loaded by CRuby 3.2+ without an explicit `require 'set'`) and `omap<K,V>` lowers to plain Ruby `Hash`, which has preserved insertion order since Ruby 1.9 (`transpiler3/ruby/lower/lower.go` lines 939 to 1021):

- `aotir.SetLiteralExpr` to `RawExpr` rendering `Set.new([e1, e2, ...])` (lines 939 to 948). `Set.new(Array)` is the canonical constructor; `Set[e1, e2, ...]` would also work but the array form factors better when the lowerer has already produced a comma-joined element list.
- `aotir.SetAddExpr` to `RawExpr` rendering `(recv | Set[elem])` (lines 949 to 959). The `|` operator returns a fresh union, keeping `add` functional (matches Mochi's value-typed `set<T>`). Mutating `recv.add(elem)` would have been one fewer allocation but would silently alias the caller's set.
- `aotir.SetHasExpr` to `MethodCall{Method: "include?", UseParens: true}` (lines 960 to 969).
- `aotir.SetLenExpr` to `MethodCall{Method: "size"}` (lines 970 to 975). `size` is preferred over `length` because Ruby's `Set` only exposes `size`.
- `aotir.SetToListExpr` to `MethodCall{Method: "to_a"}` (lines 976 to 981); supports the `for x in s` iteration form via array conversion.
- `aotir.OMapLiteralExpr` to `RawExpr` rendering `{k1 => v1, k2 => v2, ...}` (lines 982 to 995). Ruby `Hash` literals are insertion-ordered since 1.9, so no special "ordered" wrapper is needed; `omap` and `map` happen to share the same Ruby runtime representation.
- `aotir.OMapGetExpr` to `RawExpr` rendering `recv.fetch(key)` (lines 996 to 1005). `fetch` is preferred over `[]` because missing-key access raises `KeyError` (matching Mochi panic semantics) instead of silently returning `nil`.
- `aotir.OMapHasExpr` to `MethodCall{Method: "key?", UseParens: true}` (lines 1006 to 1015).
- `aotir.OMapLenExpr` to `MethodCall{Method: "size"}` (lines 1016 to 1021).

Map element assignment (`m["y"] = 20`) is handled by the generic index-assignment `RawStmt` path in `lowerStmt` (line 285), rendered as `m["y"] = 20`, so `omap_set` falls out for free without a dedicated IR node.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | Five `SetExpr` cases and four `OMapExpr` cases at lines 939 to 1021; element assignment lowers via the generic index-set RawStmt at line 285 |
| `transpiler3/ruby/build/phase16_test.go` | `TestPhase16SetsOMaps` with 6 subtests |

## Test set

- `TestPhase16SetsOMaps/set_add_has`, `set_len`, `set_forin`, `omap_literal`, `omap_set`, `omap_has_len`.

## Closeout notes

Phase 16 landed on CRuby 4.0 (Homebrew). Two design picks worth pinning: (1) `Set` is autoloaded since Ruby 3.2 so no `require 'set'` lives in the emitted source, keeping the prelude to just `require "mochi/runtime"`; this would have to be revisited if Phase 27 (mruby) supports `Set`. (2) `omap` and `map` collapse to the same `Hash` shape because Ruby has guaranteed insertion order for over a decade. The distinction is purely a Mochi type-system signal that ordering is observable; no extra runtime cost.
