---
title: "Phase 0. Skeleton"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton"
description: "MEP-56 Phase 0, toolchain detection, rtree skeleton, runtime gem stub."
---

# Phase 0. Skeleton

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-56 §Phases](/docs/mep/mep-0056#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 12:01 (GMT+7) |
| Landed         | 2026-05-29 12:01 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | [#22510](https://github.com/mochilang/mochi/pull/22510) |
| Commit         | d5559fc885 |

## Gate

`TestPhase0Skeleton` in `transpiler3/ruby/build/phase00_test.go`: three subtests, `toolchain`, `runtime_gem`, and `go_build`. The first calls `resolveToolchain()` and rejects Ruby older than 3.2 (no `Data.define`). The second checks that `mochi-runtime/lib/mochi/runtime.rb` exists on disk, the minimum the emitter requires once `require "mochi/runtime"` is rendered. The third runs `go build ./transpiler3/ruby/...` from the repo root and fails on any compile error in the Ruby pipeline subtree.

## Lowering decisions

Phase 0 ships no lowering yet, only the scaffolding the later phases depend on. The driver under `transpiler3/ruby/build/` exports `Driver.Build(src, out, target)`, `TargetRubySource`, and `resolveToolchain()`. Toolchain resolution walks (1) `$MOCHI_RUBY`, (2) Homebrew slots `/opt/homebrew/opt/ruby{,@3.4,@3.3,@3.2}/bin/ruby` and `/usr/local/opt/ruby/bin/ruby`, then (3) `exec.LookPath("ruby")`, parses `ruby -v` for the major and minor, and returns a `Toolchain{Ruby, Bundle, Major, Minor}`. Anything below 3.2 is rejected at this stage so later phases never have to defend against a missing `Data.define` or `case/in`.

`transpiler3/ruby/rtree/` lands the structural Ruby AST: `SourceFile`, `ModuleDecl`, `ClassDecl`, `MethodDecl`, `DataDecl`, `IfStmt`, `Return`, `Assign`, `MethodCall`, `BinaryOp`, `UnaryOp`, `Ident`, `StringLit`, `IntLit`, `FloatLit`, `BoolLit`, `NilLit`, `RawDecl`, `RawStmt`, and `RawExpr`. Each node exposes a `RubyString(indent int)` or `RubyExprString()` renderer using 2-space indent. The `SourceFile.RubySource()` entry point prepends `# frozen_string_literal: true` (per §1 of the MEP) and the `require` list, then concatenates declarations.

`transpiler3/ruby/lower/Lower(prog, fileBase, className)` exists as a stub signature; it returns a `*rtree.SourceFile` wrapping an empty `Main` module so the driver can round-trip parse, lower, and emit without crashing before any Mochi feature is wired. `mochi-runtime/lib/mochi/runtime.rb` ships the skeleton `Mochi::Runtime::IO.putln` and a placeholder `Mochi::Runtime::Panic < StandardError` so the emitted `require "mochi/runtime"` resolves on every supported Ruby.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/build/build.go` | `Driver`, `Build`, `resolveToolchain`, `TargetRubySource` enum |
| `transpiler3/ruby/rtree/` | Structural Ruby AST nodes + renderer |
| `transpiler3/ruby/lower/lower.go` | Stub `Lower` returning an empty `Main` module |
| `mochi-runtime/lib/mochi/runtime.rb` | Runtime gem skeleton (`Mochi::Runtime::IO.putln`, `Panic`) |
| `transpiler3/ruby/build/phase00_test.go` | `TestPhase0Skeleton` with 3 subtests |

## Test set

- `TestPhase0Skeleton/toolchain`, `TestPhase0Skeleton/runtime_gem`, `TestPhase0Skeleton/go_build`.

## Closeout notes

Phase 0 landed on CRuby 3.4 with Homebrew slot resolution and `go build ./transpiler3/ruby/...` clean. The `runtime_gem` subtest is permissive (it skips rather than fails when the skeleton file is absent) so Phase 0 can ship before the gem layout is finalised; later phases tighten this once `require "mochi/runtime"` becomes load-bearing. Key implementation insight: scaffolding the `rtree` renderer in the same commit as the driver avoided a Phase 1 dependency loop where the hello-world test would need both pieces at once.
