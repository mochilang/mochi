---
title: "Phase 1. Hello world"
sidebar_position: 3
sidebar_label: "Phase 1. Hello world"
description: "MEP-56 Phase 1, end-to-end hello.mochi from parse to Ruby stdout."
---

# Phase 1. Hello world

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

`TestPhase1HelloEnd2End` in `transpiler3/ruby/build/phase01_test.go`: a single subtest that builds `examples/v0.1/hello.mochi` via `Driver.Build(src, outDir, TargetRubySource)`, asserts `outDir/hello.rb` exists, runs it under the resolved Ruby with `-I mochi-runtime/lib`, and asserts trimmed stdout equals `Hello, world`. The diff is string equality, not byte-equal against vm3 (that gate arrives in later phases when the fixture corpus grows).

## Lowering decisions

Phase 1 wires the first real lowering path: a Mochi `print("hi")` reaches the Ruby pipeline. The C lowerer rewrites `print(stringLit)` into a `CallStmt{Func: "mochi_print_str", Args: [...]}`, and `lowerCallStmt` in `transpiler3/ruby/lower/lower.go` recognises `mochi_print_str` (and the parallel `mochi_print_bool`) and emits a bare `puts arg` `MethodCall`. `puts` is preferred over `print` because it appends `\n` automatically, matching the vm3 oracle for top-level print statements.

The full file shape emitted by `Lower` is fixed in this phase and unchanged for the rest of the language phases. The IR program's `Main` function becomes `def self.run(argv); ...; end` inside a nested `module Main`, and the file's PascalCased `className` (computed by `ModuleName("hello.mochi")` → `"Hello"`) wraps that as `module Hello; module Main; ...; end; end`. A `RawDecl` then appends an `if __FILE__ == $PROGRAM_NAME` guard that invokes `Hello::Main.run(ARGV)` so the script is runnable directly with `ruby hello.rb` while remaining requirable from a host program.

`SourceFile` carries `FrozenStringLiteral: true` and `Requires: ["mochi/runtime"]`, so the rendered header is `# frozen_string_literal: true` followed by `require "mochi/runtime"`. The hello fixture does not yet exercise the runtime gem (a `puts` literal needs nothing from `Mochi::Runtime`) but the require lands now so subsequent phases pay no extra cost when they start to depend on it.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/ruby/lower/lower.go` | `Lower` body shape: `module ClassName / module Main / def self.run(argv)`, `__FILE__ == $PROGRAM_NAME` entry guard, `mochi_print_str` and `mochi_print_bool` → `puts arg` |
| `transpiler3/ruby/lower/lower.go` | `ModuleName(src)` mapping `hello.mochi` → `Hello` and `hello_world.mochi` → `HelloWorld` |
| `transpiler3/ruby/build/phase01_test.go` | `TestPhase1HelloEnd2End` |

## Test set

- `TestPhase1HelloEnd2End` (single end-to-end fixture, no subtests).

## Closeout notes

Phase 1 landed on CRuby 3.4. The emitted `hello.rb` is roughly fifteen lines including `# frozen_string_literal: true`, the runtime require, the nested modules, the `puts "Hello, world"` body, and the `__FILE__ == $PROGRAM_NAME` guard. Key insight: locking the module nesting (`module ClassName / module Main`) in Phase 1 meant every later phase could just append declarations into the outer module without revisiting the file shape. The `argv` parameter on `run` is unused for now but reserved for the CLI surface phases.
