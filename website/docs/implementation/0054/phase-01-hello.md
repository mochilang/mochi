---
title: "Phase 1. Hello world"
sidebar_position: 3
sidebar_label: "Phase 1. Hello world"
description: "MEP-54 Phase 1, end-to-end parse + type-check + aotir lower + gotree lower + go build pipeline lighting up print('hello, mochi!')."
---

# Phase 1. Hello world

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 (Mochi-to-Go transpiler under `transpiler3/go/`) |
| Status         | LANDED |
| Started        | 2026-05-26 (GMT+7) |
| Landed         | 2026-05-26 (GMT+7) |
| Tracking issue | none (combined with Phase 0) |
| Tracking PR    | [#22485](https://github.com/mochilang/mochi/pull/22485) |
| Commit         | b836c348fb |

## Gate

`TestPhase1Hello` in `transpiler3/go/build/phase01_test.go` walks every fixture directory under `tests/transpiler3/go/fixtures`, runs the end-to-end pipeline, and diffs the binary's stdout against the fixture's `expect.txt`. Each sub-test gets its own `CacheDir` so a stray file in one fixture's work directory cannot bleed into the next. The runner skips on Windows because the driver hard-codes a POSIX `go` invocation (the cross-tuple sweep moves under Phase 16).

Five fixtures cover Phase 1: `hello`, `hello_int`, `hello_bool`, `hello_float`, `hello_newline`. All pass byte-equal under `go test ./transpiler3/go/build/...`.

## Lowering decisions

Phase 1 wires the smallest end-to-end surface. `parser.Parse(src)` produces a Mochi AST, `types.Check(prog, types.NewEnv(nil))` validates it (errors short-circuit before lowering), `clower.Lower(prog)` returns the shared aotir IR (the same IR consumed by MEP-45/46/47/48/49/50/51/52/53/55/56), `glower.Lower(ir)` returns a `gotree.File`, and `gemit.Emit` writes `main.go`. `writeGoMod(workDir, modulePath)` writes a `go.mod` pinning Go 1.26.0 + toolchain 1.26.3 so the build is reproducible; `goBuild(GoBin, workDir, absOut, nil)` shells out to `go build` and writes the binary to the caller's `out`.

The Go lowerer handles the single Mochi node that hello-world needs: `aotir.PrintStmt` for `print("hello, mochi!")`. Print lowers to `fmt.Println(args...)` with the `fmt` import added on demand via `addImport`. The println call uses Go's variadic forwarding so multi-arg prints (`print("x", 1)`) work without per-arg conversions in Phase 1; later phases lift more value shapes into the format args.

The work directory is created under `CacheDir` (defaulting to `os.TempDir()`) with `MkdirTemp` so concurrent fixture runs cannot collide. `KeepWorkDir` is exposed so failing tests can keep the emitted `main.go` for inspection; the default deletes the directory on `Build` return. `WorkDirPath` is always set on success so callers can inspect the path even when `KeepWorkDir` is true.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/lower.go` | `Lower(prog)` recognises `aotir.PrintStmt` and emits `fmt.Println(args...)` |
| `transpiler3/go/lower/expr.go` | Lowers string / int / bool / float literals for print args |
| `transpiler3/go/build/driver.go` | `Driver.Build` wires `parser` -> `types.Check` -> `clower.Lower` -> `glower.Lower` -> `gemit.Emit` -> `goBuild` |
| `transpiler3/go/build/gobuild.go` | `goBuild(goBin, workDir, out, env)` shelling out to `go build` |
| `transpiler3/go/build/gomod.go` | `writeGoMod(dir, modulePath)` pinning Go 1.26.0 + toolchain 1.26.3 |
| `transpiler3/go/build/phase01_test.go` | `TestPhase1Hello` fixture walker |
| `tests/transpiler3/go/fixtures/hello{,_int,_bool,_float,_newline}/` | Five hello-world fixtures with `expect.txt` |

## Test set

- `TestPhase1Hello/hello`
- `TestPhase1Hello/hello_int`
- `TestPhase1Hello/hello_bool`
- `TestPhase1Hello/hello_float`
- `TestPhase1Hello/hello_newline`

## Closeout notes

Secondary gates verified during Phase 1 closeout: `go vet ./transpiler3/go/...` clean, `gofmt` reaches a fixed point on every emitted `main.go`, `go build` emits the binary in under 2 seconds on the macOS arm64 reference machine. Pinning the toolchain in `go.mod` avoided the "developer has Go 1.27 installed locally, CI runs 1.26" drift that bit MEP-46. The shared aotir layer paid off immediately: the Go lowerer only had to translate `aotir.PrintStmt`, not re-derive call-site type information from the parser AST.
