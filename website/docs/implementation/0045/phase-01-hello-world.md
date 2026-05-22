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

`mochi build tests/transpiler3/c/fixtures/hello/hello.mochi -o /tmp/hello && /tmp/hello | diff - tests/transpiler3/c/fixtures/hello/expect.txt` exits 0 on host triple.

## Goal-alignment audit

The user-facing goal of MEP-45 is "ship a Mochi program as a single native binary". Phase 1 hits that goal at minimum scale: one source file (`print("hello, mochi!")`) becomes a host-triple ELF/Mach-O that prints the expected bytes and exits 0. Every later phase widens the source surface (control flow, ADTs, generics, queries, agents) but the build-driver shape (parse to type-check to lower to emit to cc) is finalised here. The CLI sub-phase (1.1) is what makes the pipeline usable by hand without Go test harnesses; the cache sub-phase (1.2) is what keeps rebuilds cheap once fixtures multiply; the zig fallback (1.3) is what removes the host-cc precondition for first-time contributors and CI agents without a system cc. All three move the goal directly. Aligns.

## Sub-phases

| #   | Scope                                                                                                                                     | Status      | Commit | PR |
|-----|-------------------------------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 1.0 | Source-to-binary minimum: parser reused; lower; emit; build via host `cc` discovery; single integration test passes                       | IN PROGRESS | —      | — |
| 1.1 | `--out PATH` and `--emit=c` CLI flags                                                                                                     | NOT STARTED | —      | — |
| 1.2 | `.mochi/cache/` BLAKE3 content-addressed cache; rebuild on unchanged source is no-op                                                      | NOT STARTED | —      | — |
| 1.3 | Vendored `zig cc` fallback under `transpiler3/c/toolchain/zig/install.go`                                                                 | NOT STARTED | —      | — |

## Decisions made

- **`transpiler3/c/runtime/include/doc.go` and `transpiler3/c/runtime/src/doc.go` removed.** Phase 0 placed those so `go vet ./transpiler3/c/...` would walk every subtree, but Phase 1 needed to ship `runtime/include/mochi/print.h` and `runtime/src/print.c` in those directories and Go refuses C siblings without cgo (`C source files not allowed when not using cgo or SWIG`). The parent `transpiler3/c/runtime/` stays a Go package (its `doc.go` + `embed.go` carry the `//go:embed include/mochi/print.h src/print.c` directive that pulls the C tree at build time), which is enough for `go vet` and `go doc` walking. `runtime/include/` and `runtime/src/` are now C-only by design; later runtime phases append `*.h` and `*.c` here without touching Go packaging.
- **Runtime is staged into a tempdir at build time, not co-located with generated C.** `Driver.Build` mkdtemps `mochi-aot-*`, materialises the embedded `include/mochi/*.h` + `src/*.c` tree under that dir, writes `main.c` next to it, and invokes `cc -std=c2x -Wall -Wextra -pedantic -I <tmp>/include -o <out> <tmp>/main.c <tmp>/src/print.c`. Tempdir removal happens on success unless `KeepEmit=true` (Phase 1.1 surface). Reason: keeps the runtime hermetic to a single build, matches the cache-key shape that Phase 1.2 will hash, and avoids polluting source trees with `.c` artefacts.
- **Host-cc discovery: `$CC`, then `cc`, `clang`, `gcc` on PATH, then (Phase 1.3) vendored zig.** No autoconf-style probing. If none resolve, `Driver.Build` returns an error message that names every candidate it tried; Phase 1.3 appends zig as the last-chance fallback so first-time setups on a fresh machine still work.

## Test set

- `TestHello` (`transpiler3/c/build/build_test.go`): walks up from cwd to find `go.mod`, locates `tests/transpiler3/c/fixtures/hello/{hello.mochi,expect.txt}`, calls `Driver.Build`, runs the produced binary, byte-diffs stdout vs `expect.txt`. Skipped on Windows (Phase 11 wires the host-cc story there).

## Deferred work

_Cross matrix is Phase 11. Reproducibility of the hello binary across hosts is Phase 17._

## Closeout notes

_Fill in after gate green._
