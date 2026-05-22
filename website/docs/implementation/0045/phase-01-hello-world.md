---
title: "Phase 1. Hello world"
sidebar_position: 3
sidebar_label: "Phase 1. Hello world"
description: "MEP-45 Phase 1 tracking: source-to-binary minimum viable pipeline that prints \"hello, mochi!\" on the host triple."
---

# Phase 1. Hello world

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 1](/docs/mep/mep-0045#phase-1-hello-world) |
| Status         | IN PROGRESS |
| Started        | 2026-05-22 19:26 (GMT+7) |
| Landed         | — |
| Tracking issue | [#22072](https://github.com/mochilang/mochi/issues/22072) |
| Tracking PR    | — |

## Gate

`mochi build --target=c-aot --out=/tmp/hello tests/transpiler3/c/fixtures/hello/hello.mochi && /tmp/hello | diff - tests/transpiler3/c/fixtures/hello/expect.txt` exits 0 on host triple (Phase 1.1 onward; Phase 1.0 ships the in-process `Driver.Build` API only). `--target=c-aot` is the staging target value used until MEP-42's `--target=c` is retired; the eventual one-flag shape is `mochi build --target=c <file> --out=<path>`.

## Goal-alignment audit

The user-facing goal of MEP-45 is "ship a Mochi program as a single native binary". Phase 1 hits that goal at minimum scale: one source file (`print("hello, mochi!")`) becomes a host-triple ELF/Mach-O that prints the expected bytes and exits 0. Every later phase widens the source surface (control flow, ADTs, generics, queries, agents) but the build-driver shape (parse to type-check to lower to emit to cc) is finalised here. The CLI sub-phase (1.1) is what makes the pipeline usable by hand without Go test harnesses; the cache sub-phase (1.2) is what keeps rebuilds cheap once fixtures multiply; the zig fallback (1.3) is what removes the host-cc precondition for first-time contributors and CI agents without a system cc. All three move the goal directly. Aligns.

## Sub-phases

| #   | Scope                                                                                                                                     | Status      | Commit | PR |
|-----|-------------------------------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 1.0 | Source-to-binary minimum: parser reused; lower; emit; build via host `cc` discovery; single integration test passes                       | IN PROGRESS | —      | — |
| 1.1 | `--target=c-aot`, `--out PATH`, `--emit=c\|executable` CLI flags wired through `cmd/mochi/main.go`                                        | IN PROGRESS | —      | — |
| 1.2 | `.mochi/cache/` BLAKE3 content-addressed cache; rebuild on unchanged source is copyFile no-op; `--emit=c` bypasses cache                  | IN PROGRESS | —      | — |
| 1.3 | Vendored `zig cc` fallback under `transpiler3/c/toolchain/zig/install.go`                                                                 | NOT STARTED | —      | — |

## Decisions made

- **`transpiler3/c/runtime/include/doc.go` and `transpiler3/c/runtime/src/doc.go` removed.** Phase 0 placed those so `go vet ./transpiler3/c/...` would walk every subtree, but Phase 1 needed to ship `runtime/include/mochi/print.h` and `runtime/src/print.c` in those directories and Go refuses C siblings without cgo (`C source files not allowed when not using cgo or SWIG`). The parent `transpiler3/c/runtime/` stays a Go package (its `doc.go` + `embed.go` carry the `//go:embed include/mochi/print.h src/print.c` directive that pulls the C tree at build time), which is enough for `go vet` and `go doc` walking. `runtime/include/` and `runtime/src/` are now C-only by design; later runtime phases append `*.h` and `*.c` here without touching Go packaging.
- **Runtime is staged into a tempdir at build time, not co-located with generated C.** `Driver.Build` mkdtemps `mochi-aot-*`, materialises the embedded `include/mochi/*.h` + `src/*.c` tree under that dir, writes `main.c` next to it, and invokes `cc -std=c2x -Wall -Wextra -pedantic -I <tmp>/include -o <out> <tmp>/main.c <tmp>/src/print.c`. Tempdir removal happens on success unless `KeepEmit=true` (Phase 1.1 surface). Reason: keeps the runtime hermetic to a single build, matches the cache-key shape that Phase 1.2 will hash, and avoids polluting source trees with `.c` artefacts.
- **Host-cc discovery: `$CC`, then `cc`, `clang`, `gcc` on PATH, then (Phase 1.3) vendored zig.** No autoconf-style probing. If none resolve, `Driver.Build` returns an error message that names every candidate it tried; Phase 1.3 appends zig as the last-chance fallback so first-time setups on a fresh machine still work.
- **Phase 1.1 introduces `--target=c-aot`, not `--target=c`.** MEP-42 already owns `--target=c` and ships in production. Until MEP-45 reaches feature parity with MEP-42 (planned for the Phase 19 close-out), the two pipelines coexist under distinct target names: `--target=c` keeps routing through `compiler3/build/c`, `--target=c-aot` routes through `transpiler3/c/build`. When MEP-45 retires MEP-42, `c-aot` is renamed to `c` and the prior `--target=c` path is deleted. The spec gate sentence (MEP-45 §Phase 1) names `--target=c-aot` so the gate is reproducible today; the eventual one-flag shape is noted next to it.
- **`--out` is overloaded by target.** For `--target=go|c` it is an output directory (MEP-43/MEP-42 surface, unchanged). For `--target=c-aot` it is the binary path itself; the harness validates it is non-empty and passes it directly to `Driver.Build` as the produced binary's destination. Documented in the cobra help string for `--out`. Reason: matches the MEP-45 spec gate sentence (`-o /tmp/hello`) and avoids a redundant `--out=<dir> --binary=<path>` pair on the AOT path.
- **`--emit=c` in `--target=c-aot` keeps the source as `<out>.c`.** Not as a separate file in a working directory: literally `<out>.c`, so a contributor running `mochi build --target=c-aot --out=/tmp/hello --emit=c file.mochi` finds `/tmp/hello` and `/tmp/hello.c` side by side. `Driver.KeepEmit=true` sets this up; `Driver.EmittedCPath` is the absolute path that the CLI echoes back on the `source ...` line. Reason: keeps inspection cheap during phase development; no temp-dir hunt.
- **Cache root: `$HOME/.mochi/cache`, overridable via `MOCHI_CACHE_DIR`.** Spec sentence "`.mochi/cache/`" is per-user, not per-project, because cached binaries are reproducible for any caller of the same `(transpiler-version, profile, triple, runtime-fingerprint, source)` tuple. Tests set `MOCHI_CACHE_DIR=<tempdir>` to isolate. Fallback chain when `$HOME` is unset: `os.UserCacheDir()/mochi`.
- **Cache key includes a runtime fingerprint, not just source + transpiler version + profile + triple.** Spec text names the first four; we also fold in a BLAKE3 over the embedded `runtime/` FS (sorted by path, length-prefixed) so that runtime header or libmochi C changes invalidate every cache entry automatically. Reason: a contributor editing `runtime/src/print.c` between two builds with the same source would otherwise get the stale binary. Costs one fs walk per process lifetime (memoised via `sync.Once`).
- **`--emit=c` builds bypass the cache entirely.** Cache stores only the binary; a `KeepEmit=true` caller wants the C source too, and serving from cache would never produce it. Easier to skip than to also stash the C tree per entry. Documented in `Driver.Build` behaviour and tested by `TestCacheBypassWithKeepEmit`.
- **Cache writes are atomic via temp-then-rename.** `copyFile` writes to `.tmp-*` next to the destination then `os.Rename`s. Two concurrent first-time builds therefore cannot leave a half-written binary in the cache. Read side uses `os.Stat` to gate; no flocks. Reason: filesystem rename is atomic on every tier-1 fs; locking adds operational pain without solving a real concurrent-corruption case here.

## Test set

- `TestHello` (`transpiler3/c/build/build_test.go`): walks up from cwd to find `go.mod`, locates `tests/transpiler3/c/fixtures/hello/{hello.mochi,expect.txt}`, calls `Driver.Build`, runs the produced binary, byte-diffs stdout vs `expect.txt`. Skipped on Windows (Phase 11 wires the host-cc story there).
- `TestCLIHello/{executable,emit-c,cache-hit}` (`transpiler3/c/build/cli_test.go`): `go build`s `cmd/mochi`, then runs the produced binary with `build --target=c-aot --out=<bin> [--emit=c] tests/.../hello.mochi`. The `executable` sub-case asserts no stray `<bin>.c`; the `emit-c` sub-case asserts `source <bin>.c` is printed and the file exists. Both sub-cases exec the resulting binary and diff stdout vs `expect.txt`. The `cache-hit` sub-case builds twice against the same `MOCHI_CACHE_DIR=<tempdir>` and asserts the second invocation prints `cached <path>` instead of `binary <path>`.
- `TestCacheHit`, `TestCacheInvalidatesOnSourceChange`, `TestCacheBypassWithKeepEmit` (`transpiler3/c/build/cache_test.go`): direct `Driver.Build` API tests that exercise the cache hit path, source-edit invalidation, and the `KeepEmit` bypass. Each test allocates its own `t.TempDir()` for `CacheDir` so runs are hermetic.

## Deferred work

_Cross matrix is Phase 11. Reproducibility of the hello binary across hosts is Phase 17._

## Closeout notes

_Fill in after gate green._
