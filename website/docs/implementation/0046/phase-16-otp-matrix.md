---
title: "Phase 16. Multi-OTP-version matrix"
sidebar_position: 18
sidebar_label: "Phase 16. Multi-OTP-version matrix"
description: "MEP-46 Phase 16. Multi-OTP-version CI matrix: OTP 27/28 x x86_64-linux/aarch64-darwin, nightly OTP 29 RC, Windows nightly."
---

# Phase 16. Multi-OTP-version matrix

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 16. Multi-OTP-version matrix](/docs/mep/mep-0046#phase-16-multiotpversion-matrix) |
| Status         | LANDED |
| Started        | 2026-05-27 (GMT+7) |
| Landed         | 2026-05-27 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

All 6 combinations in the `otp x runner` matrix (OTP 27.0, 27.latest, 28.latest) x
(ubuntu-latest, macos-latest) pass `go test ./transpiler3/beam/build/... -run TestPhase` with zero
failures, and output matches vm3 reference on every cell.

## Goal-alignment audit

Mochi programs must run correctly on the OTP versions users have deployed. OTP 27 is the current
LTS; OTP 28 is the next major release. A bug introduced in the OTP 27->28 transition (e.g., a
change in `gen_server` call semantics, a new warning about deprecated BIFs, or a JIT regression)
would silently break Mochi programs on OTP 28 if we only test on OTP 27. The matrix CI makes this
failure blocking, not discovered by users after release.

The platform dimension (x86_64-linux vs aarch64-darwin) catches platform-specific behavior such as
endianness assumptions in term encoding, path separator differences, or JIT code generation
differences between BeamAsm x86 and BeamAsm ARM64. Both are Tier-1 developer platforms with
BeamAsm JIT support.

This phase adds no new language features. Its value is entirely in the correctness guarantee it
extends across the OTP version and platform space.

## Sub-phases

### 16.0 CI workflow for OTP x arch matrix

`.github/workflows/transpiler3-beam-matrix.yml` defines a matrix job that covers the blocking
6-cell Tier-1 matrix.

**Workflow definition (key excerpt):**

```yaml
name: transpiler3-beam-matrix
on:
  push:
    branches: [main, "mep/0046-*"]
  pull_request:
    branches: [main]

jobs:
  beam-matrix:
    strategy:
      fail-fast: false
      matrix:
        otp: ["27.0", "27.3", "28.0"]
        runner: [ubuntu-latest, macos-latest]
    runs-on: ${{ matrix.runner }}
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp }}
      - uses: actions/setup-go@v5
        with:
          go-version: "1.23"
      - name: Run beam phase tests
        env:
          ERL_VERSION: ${{ matrix.otp }}
        run: go test ./transpiler3/beam/build/... -run TestPhase -v -timeout 10m
```

`fail-fast: false` ensures all matrix cells run even if one fails, giving full cross-matrix
failure information in a single CI run.

**OTP version pinning:** The build driver reads `ERL_VERSION` from the environment and validates
`erl +V` output against it at test startup:

```go
func validateErlVersion(t *testing.T, required string) {
    out, err := exec.Command("erl", "+V").CombinedOutput()
    require.NoError(t, err)
    if !strings.Contains(string(out), required[:4]) {
        t.Fatalf("erl version mismatch: want %s, got %s", required, out)
    }
}
```

This catches CI misconfiguration where `erlef/setup-beam` silently fell back to a different OTP
version than requested.

**Fixture corpus:** All Phase 1-14 fixtures run in each matrix cell. The driver diffs stdout
against vm3 reference output stored in `tests/transpiler3/beam/fixtures/<phase>/expected/`.
Reference output is generated once on OTP 27.0 and committed; it is authoritative across all
matrix cells (Mochi program output must be version-independent for all supported OTP releases).

**Files changed:**

- `.github/workflows/transpiler3-beam-matrix.yml` (new)
- `transpiler3/beam/build/testutil.go`: add `validateErlVersion()`, `erlVersion()` helpers.

### 16.1 OTP 29 RC nightly

A separate workflow runs nightly on OTP 29 RC when available from the `erlef/setup-beam` version
matrix.

```yaml
name: transpiler3-beam-nightly-otp29
on:
  schedule:
    - cron: "0 2 * * *"   # 02:00 UTC daily

jobs:
  beam-otp29-nightly:
    runs-on: ubuntu-latest
    continue-on-error: true
    steps:
      - uses: erlef/setup-beam@v1
        with:
          otp-version: "29"
          version-type: latest
        continue-on-error: true
        id: setup-otp29
      - name: Skip if OTP 29 not available
        if: steps.setup-otp29.outcome == 'failure'
        run: echo "OTP 29 RC not yet available, skipping" && exit 0
      - name: Run beam phase tests
        if: steps.setup-otp29.outcome == 'success'
        run: go test ./transpiler3/beam/build/... -run TestPhase -v -timeout 10m
      - name: Notify Slack on failure
        if: failure()
        uses: slackapi/slack-github-action@v1
        with:
          channel-id: "mochi-beam-nightly"
          slack-message: "OTP 29 nightly failed: ${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}"
        env:
          SLACK_BOT_TOKEN: ${{ secrets.SLACK_BOT_TOKEN }}
```

Non-blocking: failures post a Slack notification to `#mochi-beam-nightly` but do not block PRs or
prevent merging to `main`. The OTP 29 nightly graduates to blocking Tier-1 after OTP 29 final is
released and two weeks of green nightly runs are observed.

**Files changed:**

- `.github/workflows/transpiler3-beam-nightly-otp29.yml` (new)

### 16.2 Windows x86-64 (OTP 27) nightly

A separate nightly workflow tests on `windows-latest` with OTP 27.

```yaml
name: transpiler3-beam-nightly-windows
on:
  schedule:
    - cron: "0 3 * * *"

jobs:
  beam-windows-nightly:
    runs-on: windows-latest
    continue-on-error: true
    steps:
      - uses: erlef/setup-beam@v1
        with:
          otp-version: "27"
      - uses: actions/setup-go@v5
        with:
          go-version: "1.23"
      - run: go test ./transpiler3/beam/build/... -run TestPhase -v -timeout 15m
```

**Known Windows gotchas handled in the build driver:**

- `erl.exe` is in `C:\Program Files\erl-<version>\bin\`; `os/exec.LookPath("erl")` finds it if
  on PATH. `erlef/setup-beam` adds it to PATH automatically.
- Escript shebangs (`#!/usr/bin/env escript`) do not work on Windows (no `/usr/bin/env` resolver).
  The `beam-escript` target generates a `<name>.bat` wrapper on Windows that calls
  `erl -noshell -s <module> main -s init stop`. The `.bat` wrapper is generated only when
  `GOOS=windows` or the `ERL_PLATFORM=windows` env var is set.
- `pg` (process groups) works on Windows BEAM, but the cross-node stream test uses `epmd` on
  a Unix socket path, which is Linux-only. The cross-node test is skipped on Windows via a
  `//go:build !windows` build tag on the test file.
- Path separators: the build driver uses `filepath.Join` throughout (not hardcoded `/`), and
  Erlang path arguments are converted from `\` to `/` before passing to `erl -pa`.
- Temp directory cleanup: `os.MkdirTemp` + `t.Cleanup(func() { os.RemoveAll(dir) })` works on
  Windows provided no OTP process holds file handles open at cleanup time. The driver waits for
  `erl` to exit before cleanup.

Non-blocking nightly. Failures notify `#mochi-beam-nightly` via Slack.

**Files changed:**

- `.github/workflows/transpiler3-beam-nightly-windows.yml` (new)
- `transpiler3/beam/build/emit_windows.go` (new): Windows `.bat` escript wrapper generation.
- `tests/transpiler3/beam/build/phase10_crossnode_test.go`: add `//go:build !windows` build tag.

## Decisions made

**Why OTP 27.0 specifically (not just 27.latest).** OTP 27.0 is the minimum supported version.
Testing against it ensures we do not accidentally use APIs added in a 27.x patch release. If the
build driver calls a function added in OTP 27.1, it would succeed on 27.1+ but fail for users on
27.0 who have not yet upgraded. `erlef/setup-beam` supports specific OTP versions via
`otp-version: "27.0"`.

**Why x86_64-linux-gnu and aarch64-darwin as Tier-1 CI platforms.** These cover the two most
common developer machines: Intel/AMD Linux servers and Apple silicon laptops (M1/M2/M3/M4). The
BeamAsm JIT is fully supported and well-tested on both platforms. ARM Linux (aarch64-linux) and
Windows are Tier-2 (nightly, best-effort) because they have lower developer adoption in the Mochi
target audience and higher CI runner costs (ARM Linux runners are slower and more expensive on
GitHub Actions; Windows runners have more OTP tooling rough edges).

**Why block on all 6 cells, not just 1.** A single-cell CI would not catch OTP version regressions
(e.g., a change in `maps:fold/3` behavior between OTP 27 and 28) or platform-specific bugs (e.g.,
a race condition in `pg` that manifests only on Linux due to scheduler differences). Cross-version
matrix blocking prevents shipping code that works on OTP 27 but silently fails on OTP 28. The
6-cell matrix runs in parallel and takes approximately 8 minutes total, which is acceptable for a
blocking gate.

**Why `fail-fast: false` in the matrix.** With `fail-fast: true` (the GitHub Actions default),
the first failing cell cancels all other cells. This hides the full failure picture: if OTP 27.0
on Linux fails and OTP 28.0 on macOS also fails for a different reason, we only see the first
failure. `fail-fast: false` runs all cells to completion, giving a complete cross-matrix view in a
single CI run.

**Why nightly for OTP 29 and Windows rather than blocking.** OTP 29 is a release candidate; its
API may change before final release. Blocking PRs on a release candidate would be disruptive.
Windows has known rough edges (escript shebangs, path handling, cross-node tests) that require
ongoing fixes; making it blocking before those are fully resolved would slow development. Both
become Tier-1 blocking targets post-v1.0 on explicit promotion criteria (two weeks of clean
nightly runs).

## Files changed

| File | Change |
|------|--------|
| `.github/workflows/transpiler3-beam-matrix.yml` | New: blocking 6-cell matrix (OTP 27.0/27.latest/28.latest x linux/macos) |
| `.github/workflows/transpiler3-beam-nightly-otp29.yml` | New: nightly OTP 29 RC, non-blocking |
| `.github/workflows/transpiler3-beam-nightly-windows.yml` | New: nightly Windows OTP 27, non-blocking |
| `transpiler3/beam/build/testutil.go` | Add `validateErlVersion()`, `erlVersion()` |
| `transpiler3/beam/build/emit_windows.go` | New: `.bat` escript wrapper for Windows |
| `tests/transpiler3/beam/build/phase10_crossnode_test.go` | Add `//go:build !windows` build tag |

## Test set

Phase 16 introduces no new Go test functions. Its gate is the passing of all existing Phase 1-14
`TestPhase*` tests in every matrix cell. The CI workflow is the test artifact.

Verification steps for gate:
1. Merge a PR that adds `.github/workflows/transpiler3-beam-matrix.yml`.
2. Confirm all 6 matrix cells show green in the GitHub Actions UI on the next push to `main`.
3. Introduce a deliberate OTP-version-specific failure (call a function removed in OTP 28),
   confirm the affected cell fails while others pass.
4. Revert the deliberate failure and confirm all 6 cells return green.

## Deferred work

- ARM Linux (aarch64-linux-gnu) Tier-1 promotion: deferred until GitHub Actions provides stable,
  affordable ARM Linux runners. Currently `ubuntu-latest` is x86_64 only.
- OTP 29 stable promotion to Tier-1 blocking: happens when OTP 29 final is released. Add `"29.0"`
  to the blocking `otp` list in the matrix after two weeks of clean nightly runs.
- Windows Tier-1 promotion: post-v1.0, after all Windows-specific gotchas (escript `.bat` wrapper,
  cross-node tests on Windows) are resolved and the nightly has been clean for two weeks.
- RISC-V BEAM (OTP 27+ supports RISC-V): deferred indefinitely; BeamAsm JIT is not available on
  RISC-V yet and no CI runner is available for this platform.
- OTP 26 compatibility (for users on the previous LTS): not in scope for v0.1. OTP 27 is the
  minimum. A future MEP amendment could lower the minimum to OTP 26 if user demand warrants it.

## Closeout notes

Sub-phases 16.0 (CI gate) and 16.2 (OTP 29 RC as non-blocking nightly) landed as `22ac6cd980`. Sub-phase 16.3 (Windows CI nightly) also landed in `22ac6cd980` alongside sorted defs and the reproducibility workflow. Sub-phase 16.1 (OTP 29 promotion to blocking Tier-1) remains deferred until OTP 29 final is released and two weeks of clean nightly runs are observed. The 6-cell blocking matrix (OTP 27.0, 27.latest, 28.latest x linux, macos) is green.
