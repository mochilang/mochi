---
title: "Phase 2. Scalars and arithmetic"
sidebar_position: 4
sidebar_label: "Phase 2. Scalars and arithmetic"
description: "MEP-54 Phase 2, int64/float64/bool/string arithmetic, comparison, modulo, var/let, if/else, while, for-range."
---

# Phase 2. Scalars and arithmetic

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-27 (GMT+7) |
| Landed         | 2026-05-27 (GMT+7) |
| Tracking PR    | [#22485](https://github.com/mochilang/mochi/pull/22485) |
| Commit         | 20727c019e |

## Gate

Twelve new fixtures cover int64/float64/bool/string arithmetic and comparison, modulo, var/let, if/else, while, and for-range: `var_let`, `int_arith`, `float_arith`, `bool_ops`, `string_concat`, `mod_basic`, `cmp_int`, `cmp_float`, `if_else_basic`, `if_else_nested`, `while_loop`, `for_range`. All 17 fixtures (Phase 1 + Phase 2) pass byte-equal under `go test ./transpiler3/go/build/... -run TestPhase1Hello`.

## Lowering decisions

`letTypeText` is added so `LetStmt` consults the bound expression's aotir type when emitting the Go annotation (`var x int64 = 1`, `var name string = "mochi"`). The int pin is always `int64` and the float pin is `float64`, never bare `int` or `float32`, per MEP-54 §6 "Type lowering". `BinaryExpr` dispatches each `aotir.BinOp` to the matching Go operator: `BinAddI`, `BinSubI`, `BinMulI`, `BinDivI`, `BinModI` for int math; the `F` variants for float; `BinEqI/NeI/LtI/...` for comparisons; `BinAnd`, `BinOr` for bool ops. String concatenation lowers `BinAddS` to Go's `+` on strings (the C runtime materialises the join, Go's runtime does the same).

`gotree.IfStmt` is taught to render `} else {` on one line via a new `BlockStmt.writeInlineNoNewline` helper so `if/else` is gofmt-stable. `UnaryExpr` parenthesises its operand when it would otherwise form a `--`, `++`, or `<-<-` token run (e.g., `-(-x)`, `-(<-c)`) so the printed source stays unambiguous. `ForRangeStmt` lowers `for i in lo..hi` to `for i := int64(lo); i < int64(hi); i++` to preserve Mochi's half-open range semantics on int64. `WhileStmt` lowers to Go's `for cond { ... }`.

`var` vs `let` both lower to `var` in Go (Mochi semantics are reassignable-vs-final but Go can express the difference only with comments at the variable level, and the upstream type-checker already enforces the rule). Reassignment (`x = x + 1`) lowers to a plain assignment statement; the upstream checker has already validated the binding is mutable.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/types.go` | `lowerType(aotir.Type)` -> Go scalar type strings (`int64`, `float64`, `bool`, `string`) |
| `transpiler3/go/lower/stmt.go` | `LetStmt` -> `var x T = expr`; `AssignStmt` -> `x = expr`; `WhileStmt` -> `for cond {...}`; `ForRangeStmt` -> `for i := int64(lo); i < int64(hi); i++` |
| `transpiler3/go/lower/expr.go` | `BinaryExpr` op dispatch; `UnaryExpr` with operand parenthesisation |
| `transpiler3/go/gotree/stmt.go` | `BlockStmt.writeInlineNoNewline` for inline `} else {` |
| `tests/transpiler3/go/fixtures/` | 12 new fixtures (`var_let`, `int_arith`, `float_arith`, `bool_ops`, `string_concat`, `mod_basic`, `cmp_int`, `cmp_float`, `if_else_basic`, `if_else_nested`, `while_loop`, `for_range`) |

## Test set

- `TestPhase1Hello/var_let`, `int_arith`, `float_arith`, `bool_ops`, `string_concat`, `mod_basic`, `cmp_int`, `cmp_float`, `if_else_basic`, `if_else_nested`, `while_loop`, `for_range`.

## Closeout notes

Choosing `int64` over Go's machine-width `int` is a deliberate soundness pin: a Mochi program compiled on a 32-bit Go target would otherwise silently truncate values that fit on 64-bit hosts. The `<-c` parenthesisation in `UnaryExpr` was a defensive add against Phase 9.1 channels even though Phase 2 has no channels yet, because the alternative would have been a Phase-9.1-time patch to a stable printer. `for-range` reuses Go's idiomatic counted-for over a literal int64 range to avoid materialising a slice of indices.
