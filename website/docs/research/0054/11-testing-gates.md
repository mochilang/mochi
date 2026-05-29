---
title: "11. Testing gates"
sidebar_position: 12
sidebar_label: "11. Testing gates"
description: "Per-phase Go test gates with vm3 oracle, byte-equal stdout diff, go vet secondary gate, reproducibility SHA-256 gate, publish dry-run via go mod tidy + go install."
---

# 11. Testing gates

This note describes the gates that decide whether a phase has LANDED. The general principle: **a phase is LANDED only when its gate is green on every Go runtime listed in MEP-54 §6 for that phase.**

## Phase-test structure

Each phase ships a `transpiler3/go/build/phaseNN_test.go` file containing a top-level `TestPhaseNN<Topic>` test with one inline subtest per fixture. The subtest:

1. Compiles the fixture's `.mochi` source via `Driver.Build`.
2. Executes the produced binary.
3. Reads the fixture's `expect.txt`.
4. Diffs stdout against `expect.txt`; fails on any difference.

The fixtures live under `tests/transpiler3/go/<topic>/<name>/` with `<name>.mochi` and `expect.txt` files. The `expect.txt` is recorded by running vm3 on the source — vm3 is the recording oracle, not a runtime dependency.

Subtests are inline (not table-driven over a slice) so test failures point at a specific subtest name like `TestPhase09Channels/chan_basic`, which makes test reports readable.

## Per-phase test files

| Phase | Test | File |
|-------|------|------|
| 0 | `TestPhase0Skeleton` | `phase00_test.go` |
| 1 | `TestPhase1Hello` | `phase01_test.go` |
| 2 | `TestPhase2Scalars` | `phase02_test.go` |
| 3.1 | `TestPhase031Lists` | `phase03_1_test.go` |
| 3.2 | `TestPhase032Maps` | `phase03_2_test.go` |
| 3.3 | `TestPhase033Sets` | `phase03_3_test.go` |
| 3.4 | `TestPhase034ListOfRecords` | `phase03_4_test.go` |
| 4 | `TestPhase4Records` | `phase04_test.go` |
| 5 | `TestPhase5Sums` | `phase05_test.go` |
| 6 | `TestPhase6Functions` | `phase06_test.go` |
| 6.1 | `TestPhase61Closures` | `phase06_1_test.go` |
| 7.1-7.12 | `TestPhase07NNxxx` | `phase07_N_test.go` |
| 8 | `TestPhase8Datalog` | `phase08_test.go` |
| 9.1 | `TestPhase091Channels` | `phase09_1_test.go` |
| 9.2 | `TestPhase092Streams` | `phase09_2_test.go` |
| 10 | `TestPhase10Agents` | `phase10_test.go` |
| ... | ... | ... |
| 18 | `TestPhase18Publish` | `phase18_test.go` |

## Gates in order of strictness

### Tier 1: byte-equal stdout vs vm3

The master correctness gate. `Driver.Build` produces a binary; we execute it and `diff -u expect.txt actual.txt`. Any byte difference fails the subtest. This includes trailing newlines, extra whitespace, floating-point formatting (`1.0` vs `1`), and stable-but-implementation-dependent map iteration order.

The `expect.txt` is recorded by vm3 once at fixture-add time and committed alongside the `.mochi` source. Re-recording requires a deliberate action (`MOCHI_RECORD_FIXTURES=1 go test ./...`) so accidental regressions cannot silently overwrite the gold file.

### Tier 2: `go build` clean

Implicit in Tier 1 (if `go build` fails, the test cannot produce stdout). But the test harness logs the `go build` stderr separately so compile failures surface with a more specific error message than "no stdout to compare against".

### Tier 3: `go vet` clean

After `go build` succeeds, the harness runs `go vet ./...` on the emitted workspace. Vet warnings (e.g., "Println call has no arguments", "format string mismatches") fail the subtest. `go vet` catches a class of bugs the lowerer might introduce that compile fine but are wrong (e.g., printing the wrong number of arguments to `fmt.Printf`).

Some vet warnings are spurious on lowered code (e.g., synthesised `_ = m` assignments in agent message handlers trigger "unused variable" if the lowerer also gives the variable a real name). The lowerer goes out of its way to avoid these, but when they happen the harness supports an allowlist via `MOCHI_GO_VET_ALLOW=...` to skip specific warnings while real bugs are still caught.

### Tier 4: cross-build clean

For phases past 16, the gate adds:

- `GOOS=linux GOARCH=arm64 go build ./...` clean.
- `GOOS=darwin GOARCH=amd64 go build ./...` clean.
- `GOOS=windows GOARCH=amd64 go build ./...` clean.
- `GOOS=freebsd GOARCH=amd64 go build ./...` clean.

These are compile-only; the produced binaries are not executed on the CI runner (cross-arch exec is Phase 16.x).

### Tier 5: wasm gate

For Phase 17 and later:

- `GOOS=js GOARCH=wasm go build ./...` clean. Plus a node.js run that loads the wasm via `wasm_exec.js` and asserts the stdout matches `expect.txt`.
- `GOOS=wasip1 GOARCH=wasm go build ./...` clean. Plus a wasmtime run that asserts the stdout matches `expect.txt`.

The wasmtime gate is skipped if `wasmtime` is not on PATH.

### Tier 6: reproducibility gate

For Phase 16 and later: `TestPhase16Hermetic` runs `Driver.Build` twice in independent temp directories with `Deterministic=true`; the resulting binaries are SHA-256 compared. The gate fails if they differ. macOS runs `allow_failure: true` because of the LC_UUID issue.

### Tier 7: publish gate

For Phase 18: `TestPhase18ModTidyIdempotent` runs `go mod tidy` against the emitted module and asserts no changes. `TestPhase18GoInstall` runs `go install ./...` against a published tag and asserts the produced binary matches the locally-built one.

## Fixture growth

Each phase adds a handful of fixtures (typically 3-10). Phase 0 has 0 fixtures (it tests the toolchain). Phase 1 has 1 fixture (hello world). Phase 2 has ~5 (one per scalar arithmetic case). By Phase 18 the corpus is ~400 fixtures.

Fixtures are immutable once added; they constitute the gold-file regression set. Re-recording is reserved for cases where the source-language semantics intentionally change (which requires a tracking MEP, not a Mochi-to-Go-side decision).

## CI matrix

The matrix CI runs on each PR:

```yaml
strategy:
  fail-fast: false
  matrix:
    os: [ubuntu-24.04, macos-15, windows-2025]
    go: ['1.26.0', '1.26.x']
```

Each (os, go) combination runs `go test ./transpiler3/go/build/...`. The full matrix is 6 jobs. Cross-arch builds run only on the `ubuntu-24.04, 1.26.x` job to save CI minutes (we trust cross-compile to be deterministic across Go versions).

`go test -race` is run on the `ubuntu-24.04, 1.26.x` job only. Phase 9.2 / Phase 10 / Phase 11 fixtures use the race detector to catch goroutine misuse.

## Pull-request flow

Each phase ships as one PR:

1. Implementation in `transpiler3/go/lower/`, `transpiler3/go/emit/`, etc.
2. Test fixtures in `tests/transpiler3/go/<topic>/<name>/`.
3. Phase test file in `transpiler3/go/build/phaseNN_test.go`.
4. Implementation tracking page in `website/docs/implementation/0054/phase-NN-<topic>.md`.

A phase PR is auto-mergeable (via `gh pr merge --merge --auto`) when:

- All CI matrix jobs green.
- The phase test passes locally.
- The tracking page documents the gate and the test set.

Sub-phases (e.g., 9.1, 9.2) ship independently when a single phase has multiple targets that need separate work.

## Regression catch

Beyond the per-phase gate, a continuous `TestAllPhases` runs the full corpus on every PR touching `transpiler3/go/`. This catches cases where a phase-N change breaks a phase-(N-k) fixture. The continuous test runs on `ubuntu-24.04, 1.26.x` only.

A nightly job runs the full corpus on every (os, go) combination to catch regressions specific to a runtime tuple.
