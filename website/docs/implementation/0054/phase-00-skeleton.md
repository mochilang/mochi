---
title: "Phase 0. Skeleton"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton"
description: "MEP-54 Phase 0, gotree shadow AST package, lower/emit/build skeletons, runtime sub-packages, CI workflow."
---

# Phase 0. Skeleton

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 (Mochi-to-Go transpiler under `transpiler3/go/`) |
| Status         | LANDED |
| Started        | 2026-05-26 (GMT+7) |
| Landed         | 2026-05-26 (GMT+7) |
| Tracking issue | none (combined with Phase 1) |
| Tracking PR    | [#22485](https://github.com/mochilang/mochi/pull/22485) |
| Commit         | b836c348fb |

## Gate

`TestPhase0Skeleton` in `transpiler3/go/build/phase00_test.go` shells out to `go build ./transpiler3/go/...` and `go vet ./transpiler3/go/...` against the repo root. `TestPhase0DriverRejectsEmptyPaths` exercises `Driver.Build("", "out", "", "")` and `Driver.Build("src.mochi", "", "", "")` and demands a descriptive error rather than a panic. `TestPhase0LowerEmpty` constructs an empty `aotir.Program` whose only function is `main` with an empty body, asks `glower.Lower` for a `gotree.File`, and checks the rendered output contains `package main`. `TestPhase0EmitWritesFile` calls `gemit.Emit(&gotree.File{PackageName: "main"}, tmp, "main.go")` and inspects the on-disk file for `package main`.

## Lowering decisions

Phase 0 ships no Mochi-level lowering yet; it scaffolds the packages every later phase depends on. `transpiler3/go/gotree` introduces the Go shadow AST (`File`, `Decl`, `FuncDecl`, `Stmt`, `Expr`, `FuncType`, `Field`, `BlockStmt`, ...). Every node exposes a `Render` method that emits unformatted Go text; `File.Render` pipes its output through `go/format.Source` so callers always receive gofmt-clean bytes. `transpiler3/go/emit/emit.go` writes `File.Render()` to `outDir/fileName`, creating the directory on demand. `transpiler3/go/lower/lower.go` exports `Lower(prog *aotir.Program) (*gotree.File, error)` returning a minimum-viable file with an empty `main` function. `transpiler3/go/build` ships `Driver{CacheDir, GoBin, ModulePath, KeepWorkDir, WorkDirPath}` and `Build(src, out, target, profile)`. The driver runs `parser.Parse` -> `types.Check` -> `clower.Lower` (shared aotir) -> `glower.Lower` -> `gemit.Emit` -> `writeGoMod` -> `goBuild` and then writes the resulting binary to the caller's `out` path. `Target` is an enum declaring the host tuples plus `TargetGoModule`, `TargetGoWasmJS`, `TargetGoWasiP1` so the CLI surface stays stable across phases.

The runtime sub-packages (`transpiler3/go/runtime/{collections, query, datalog, stream, agent, llm, option, result, stringz, timez}`) ship as stubs with `doc.go` files. They are wired so later phases can drop helper functions in without restructuring imports. `transpiler3/go` itself ships a `doc.go` describing the pipeline (`parse + type-check -> aotir -> gotree -> go/format -> go build`) and the directory layout convention so newcomers can read the rest of the work top-down.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/doc.go` | Package overview + pipeline description |
| `transpiler3/go/gotree/` | Go shadow AST node types + `Render` methods (gofmt-clean) |
| `transpiler3/go/emit/emit.go` | `Emit(file, dir, name)` writes `file.Render()` to disk |
| `transpiler3/go/lower/lower.go` | `Lower(prog)` returns minimum `gotree.File` with empty `main` |
| `transpiler3/go/build/{driver.go, gobuild.go, gomod.go}` | `Driver`, `Build`, `Target`, `Profile`, `writeGoMod`, `goBuild` |
| `transpiler3/go/runtime/` | Per-program runtime stubs (collections, query, datalog, ...) |
| `transpiler3/go/build/phase00_test.go` | `TestPhase0*` build / vet / driver / lower / emit gates |
| `.github/workflows/transpiler3-go-test.yml` | CI workflow scope: `go test ./transpiler3/go/...` |

## Test set

- `TestPhase0Skeleton/build_._transpiler3_go_...`
- `TestPhase0Skeleton/vet_._transpiler3_go_...`
- `TestPhase0DriverRejectsEmptyPaths`
- `TestPhase0LowerEmpty`
- `TestPhase0EmitWritesFile`

## Closeout notes

Phase 0 landed alongside Phase 1 in a single PR because the gotree renderer is too coupled to the hello-world fixture for either to ship independently. `go.mod` pins Go 1.26.0 with toolchain 1.26.3 so the produced binaries are always built against the same toolchain the test runner uses. The runtime sub-packages are imported under namespaced paths (`transpiler3/go/runtime/collections`, etc.) so a later phase that adds a `query` helper does not have to coordinate with the unrelated `stream` package.
