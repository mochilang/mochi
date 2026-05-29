---
title: "05. Codegen design"
sidebar_position: 6
sidebar_label: "05. Codegen"
description: "Rust source via the structural rtree IR (not syn/quote), aotir reuse, the colour pass for borrow-and-clone elision, 4-space indent, stable item ordering."
---

# 05. Codegen design

This note describes how MEP-53 emits Rust source code: the IR, the rendering, the passes that run between aotir and Rust, and why the design rejects `syn` / `quote`.

## Pipeline

```
.mochi
  -> parse (mochi/parser)
  -> typecheck (mochi/types)
  -> clower.Lower (shared with MEP-45/46/47/48/56)
  -> aotir.Program
  -> transpiler3/rust/lower.Lower
  -> rtree.SourceFile
  -> transpiler3/rust/colour.Colour
  -> rtree.SourceFile (annotated)
  -> transpiler3/rust/emit.Emit
  -> workdir/{Cargo.toml, src/main.rs, ...}
  -> cargo build
  -> binary or .wasm
```

The fork from the shared aotir IR happens at `transpiler3/rust/lower.Lower`. All passes before that (parse, typecheck, clower) are shared with other targets.

## The rtree AST

`transpiler3/rust/rtree/` exposes a structural Rust AST:

- Top-level: `SourceFile`, `UseDecl`, `ModDecl`, `StructDecl`, `EnumDecl`, `FnDecl`, `ImplDecl`, `TraitDecl`, `ExternBlock`, `ConstDecl`.
- Statements: `LetStmt`, `IfStmt`, `MatchStmt`, `ForStmt`, `WhileStmt`, `LoopStmt`, `Return`, `BreakStmt`, `ContinueStmt`, `ExprStmt`.
- Expressions: `BinaryOp`, `UnaryOp`, `Call`, `MethodCall`, `Ident`, `Path`, `StringLit`, `IntLit`, `FloatLit`, `BoolLit`, `TupleLit`, `ArrayLit`, `StructLit`, `EnumLit`, `Closure`, `Block`, `MatchExpr`, `IfExpr`.
- Escape hatch: `RawDecl`, `RawStmt`, `RawExpr` for cases where the lower pass needs to emit non-structural Rust (e.g., `cargo:rustc-link-lib=` lines).

Each node exposes a `RustString(indent int) string` or `RustExprString() string` renderer using 4-space indent. The `SourceFile.RustSource()` entry point prepends `#![allow(unused, non_snake_case, non_camel_case_types)]`, then `extern crate mochi_runtime;` and any other `extern crate` directives, then `use` statements (sorted, deduplicated), then declarations in deterministic order.

## Why not syn / quote

The alternative is to depend on the Rust `syn` crate, which provides a procedural-macro IR with TokenStream-based rendering. Using `syn` from Go means either:

1. Calling `syn` from a Rust-side helper binary invoked as a subprocess. Adds toolchain build complexity (the helper has to be vendored and rebuilt), latency (subprocess fork per emit), and a circular-dep risk (the Rust helper needs cargo, which is what we're trying to drive).
2. Reimplementing `syn`-style token emission in Go via `quote!`-equivalent macros. No win over direct string emission via rtree, and the syn / quote APIs change faster than Mochi's needs.

The structural rtree is reused across targets (Ruby uses an analogous structure at `transpiler3/ruby/rtree/`), which amortises the design cost. The Go-side renderer is ~600 LOC.

## The colour pass

`transpiler3/rust/colour/Colour(sf)` runs after lower and before emit. Its job is to decide where `.clone()` is required:

- For each `Ident` use site, determine if the value is used again after this point in any path.
- If so, and the consuming context takes by value, and the type is not `Copy`, mark the use as `RequiresClone`.
- Otherwise, leave bare (which moves the value, consuming it).

The pass is a forward dataflow on the rtree: walk the `Block` body in order, maintain a "live-after-this-stmt" set, and mark uses based on whether the var is in that set when the use occurs.

The pass runs until fixpoint because elision at one use site can shift others. For example, in:

```rust
let xs = vec![1, 2, 3];
let a = sum(xs.clone());  // marked RequiresClone initially
let b = len(xs);          // marked Bare initially
```

After the first pass: `a` clones, `b` moves. But `a` could move too if `b` were rewritten. The current implementation does not actually optimise across these cases (it's a simple per-use analysis), but the structural rtree supports a fixed-point algorithm if a future phase needs it.

The pass also detects `FnMut` requirements: if a closure body mutates a captured `&mut` binding, the box type switches from `Box<dyn Fn>` to `Box<dyn FnMut>`. This is detected by walking the closure body and looking for assignment statements with a captured-var LHS.

## Stable item ordering

`SourceFile.RustSource()` emits items in deterministic order: `extern crate` declarations first (sorted), `use` declarations next (sorted), then top-level declarations grouped by kind (consts, structs, enums, traits, impls, fns) in source-order within each group. This determinism is load-bearing for phase 16 (reproducible builds): without it, two builds of the same source would produce different `main.rs` files because the lower pass walks the aotir program in a non-deterministic order.

## Emit details

`transpiler3/rust/emit.Emit(sf, workDir)`:

1. Renders `sf.RustSource()` to `<workDir>/src/main.rs`.
2. Generates `<workDir>/Cargo.toml` via `generateCargoToml` (which includes `license`, `description`, `repository` from phase 15 onwards).
3. Writes `<workDir>/Cargo.lock` if `Driver.Deterministic` (with a pinned itertools version).
4. Copies `cffi/` from the user-supplied source if any `extern fn` directive was lowered (phase 12).
5. Writes `<workDir>/build.rs` for the cc-rs invocation if cffi/ exists (phase 12).
6. Does not invoke cargo (the build target handles that).

## Files

| File | Lines (approx) | Purpose |
|------|----------------|---------|
| `transpiler3/rust/rtree/rtree.go` | ~600 | Node types + renderer |
| `transpiler3/rust/lower/lower.go` | ~400 | Top-level orchestration |
| `transpiler3/rust/lower/{struct,sum,closure,query,datalog,agent,stream,chan,try,extern,fetch,json,llm}.go` | ~50-200 each | Per-feature lowerings |
| `transpiler3/rust/colour/colour.go` | ~250 | Forward-dataflow colour pass |
| `transpiler3/rust/emit/emit.go` | ~150 | Disk write + Cargo.toml gen |
| `transpiler3/rust/build/build.go` | ~600 | Driver + cargo invocation |

## Cross-references

- [[language-surface]] for the per-feature lowering shape.
- [[type-lowering]] for the Rust type targets.
- [[runtime]] for the runtime crate the emit references.
- [[testing-gates]] for the byte-equal stdout gate that validates the emitted code.
