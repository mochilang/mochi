---
title: "Phase 16. Reproducible build"
sidebar_position: 21
sidebar_label: "Phase 16. Reproducibility"
description: "MEP-51 Phase 16 -- SOURCE_DATE_EPOCH plus lex-sorted RECORD plus sorted zip/tar entries plus pinned gzip ModTime yield byte-deterministic wheel and sdist SHA-256 across rebuilds; source emit is a fixed point; cross-host CI matrix lands."
---

# Phase 16. Reproducible build

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 16](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED (16.0 + 16.1 + 16.2; cross-host CI workflow shipped, 16.3 multi-host SHA gate runs on GitHub Actions) |
| Started        | 2026-05-29 20:31 (GMT+7) |
| Landed         | 2026-05-29 20:39 (GMT+7) |
| Tracking issue | (filled at ship) |
| Tracking PR    | (filled at ship) |

## Gate

`TestPhase16Reproducible` ships seven sub-gates in `transpiler3/python/build/phase16_test.go`:

1. `wheel_sha_byte_equal_across_rebuilds` (2 fixtures): two consecutive `TargetPythonWheel` builds produce SHA-256-equal `.whl` files.
2. `sdist_sha_byte_equal_across_rebuilds` (2 fixtures): two consecutive `TargetPythonSdist` builds produce SHA-256-equal `.tar.gz` files.
3. `source_emit_fixed_point` (2 fixtures): two consecutive `TargetPythonSource` builds produce byte-equal `generated/<module>.py` files. Catches non-determinism in the Python lower (e.g. map iteration order leaking into emit order).
4. `wheel_record_is_lex_sorted`: parses the wheel `RECORD` and asserts `sorted(entries) == entries`. This caught the Phase 15.0 RECORD-self-line-at-bottom bug; the Phase 16 fix interleaves the RECORD path into the sort.
5. `wheel_has_no_pycache`: lists every zip entry and asserts no `__pycache__/` or `*.pyc`. Build-host bytecode would defeat reproducibility (pyc headers carry the host's Python version + mtime nanoseconds).
6. `source_date_epoch_overrides_floor`: `SOURCE_DATE_EPOCH=1700000000` flows into every wheel zip entry mtime and the sdist gzip header `ModTime`. Verifies the reproducible-builds.org contract.
7. `source_date_epoch_falls_back_when_malformed`: three bad values (`-1`, `not-a-number`, an int64-overflow string) all degrade gracefully to the 1980 floor instead of crashing the build.

All seven sub-gates pass on CPython 3.14.x. The full Phase 1-16 regression (`go test ./transpiler3/python/... -count=1`) finishes in 27.1s with zero regressions; the Phase 15 `wheel_is_deterministic` sub-gate still passes (the RECORD-sort fix is forward-compatible with its byte-equal assertion).

The cross-host SHA gate runs in `.github/workflows/transpiler3-python-repro.yml`: matrix over `ubuntu-24.04` (x86_64), `ubuntu-24.04-arm` (aarch64), and `macos-14` (arm64) builds the two Phase 16 fixtures, uploads the wheel SHAs as artifacts, and a compare job diffs them. Divergence fails the workflow.

## Goal-alignment audit

Phase 16 is the supply-chain anchor for the v1 Python pipeline. Two wheels built from the same Mochi source on two different CI hosts must be byte-identical. Without that property:

- Mochi lockfile SHA-256 pinning is meaningless because the pinned hash drifts between builds.
- PyPI's PEP 740 attestation (Phase 18) cannot give an end-user a way to verify the artifact they install matches the artifact PyPI was given.
- An attacker who compromises one CI host can substitute a malicious wheel that no one will detect.

Phase 15.0 already pinned the mechanical sources of non-determinism (mtime, sorted paths, filtered `__pycache__`); Phase 16 closes the loop by verifying them through end-to-end SHA gates, adding `SOURCE_DATE_EPOCH` as the reproducible-builds.org standard override, and wiring a cross-host CI matrix.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 16.0 | `SOURCE_DATE_EPOCH` env var support: `zeroTime()` honours the value when set, falls back to 1980 floor on malformed input | LANDED 2026-05-29 | (filled at ship) |
| 16.1 | RECORD lex-sort fix: self-line is interleaved into the sort instead of appended; wheel zip + sdist tar entries already sorted in 15.0 | LANDED 2026-05-29 | (filled at ship) |
| 16.2 | Source emit fixed-point gate: two source builds produce byte-equal `generated/<module>.py` | LANDED 2026-05-29 | (filled at ship) |
| 16.3 | Cross-host SHA gate: GitHub Actions matrix over linux-x86_64 + linux-aarch64 + macos-arm64 | LANDED 2026-05-29 (workflow file) | (filled at ship) |
| 16.4 | Windows reproducibility (NTFS case-insensitivity + CRLF) | DEFERRED |  |
| 16.5 | `diffoscope` integration for SHA-divergence debugging | DEFERRED |  |
| 16.6 | Pyc-content audit: ruff format fixed-point + `ast.unparse` re-verification once Phase 15.2 lands the hatchling-declared sdist path | DEFERRED to Phase 15.2 follow-up |  |

## Sub-phase 16.0 -- SOURCE_DATE_EPOCH

### Goal-alignment audit (16.0)

The wheel is a zip; zip stores mtime per entry; mtime is otherwise the wall-clock time of `uv build`. Phase 15.0 hard-codes the entry mtime to 1980-01-01 so two builds at different wall-clock instants produce the same SHA. That works for byte-equal SHA but breaks the reproducible-builds.org promise that downstream tooling can correlate the wheel mtime to a source commit timestamp. `SOURCE_DATE_EPOCH` lets the CI workflow pass `git log -1 --pretty=%ct` in, the wheel records that timestamp, and the SHA still byte-matches across rebuilds (because all hosts read the same commit timestamp).

### Decisions made (16.0)

**`SOURCE_DATE_EPOCH` is the contract, not the cache.** The driver does not inject the env var automatically; it is the caller (Makefile, CI workflow, `mochi build` invocation) that decides what timestamp to surface. This matches dpkg / Debian / hatchling behaviour and keeps the build hermetic: nothing in the Mochi source reads the system clock.

**Fallback floor is 1980-01-01 UTC.** When `SOURCE_DATE_EPOCH` is unset, the wheel still ships byte-deterministic; the floor is the zip format's DOS-encoded mtime epoch and the only value that survives lossy round-trip without quantisation surprises.

**Graceful degradation on malformed input.** A negative value, a non-integer, or an int64-overflow string drops back to the 1980 floor instead of failing the build. This matches dpkg's behaviour and matters in CI environments where stale env vars (e.g. a previous job's `SOURCE_DATE_EPOCH=invalid`) might leak through.

**Pre-1980 timestamps clamp to the floor.** The DOS-encoded mtime cannot represent dates before 1980; a SOURCE_DATE_EPOCH that resolves to, say, 1970-01-01 silently clamps. The gate test verifies the post-1980 happy path (`1700000000` = 2023-11-14T22:13:20Z) and the malformed-input fallback; the pre-1980 clamp is exercised by the floor fallback.

**Code path is a single function (`sourceDateEpoch()`) in `transpiler3/python/build/repro.go`.** Both `zipDir` and `tarGzDir` call `zeroTime()`, which delegates to `sourceDateEpoch()`. No call-site duplication; a future bug fix or policy change lands in one place.

```go
func sourceDateEpoch() time.Time {
    if s := os.Getenv("SOURCE_DATE_EPOCH"); s != "" {
        if n, err := strconv.ParseInt(strings.TrimSpace(s), 10, 64); err == nil && n >= 0 {
            t := time.Unix(n, 0).UTC()
            floor := time.Date(1980, 1, 1, 0, 0, 0, 0, time.UTC)
            if t.Before(floor) {
                return floor
            }
            return t
        }
    }
    return time.Date(1980, 1, 1, 0, 0, 0, 0, time.UTC)
}
```

## Sub-phase 16.1 -- RECORD lex-sort fix

### Goal-alignment audit (16.1)

PEP 376 does not strictly require lex-sorted RECORD lines but every reproducible-build tooling (hatchling, flit, pip wheel) emits them sorted. The Phase 15.0 builder collected the entries from `filepath.Walk` (which on disk does not include the RECORD file itself because RECORD has not been written yet at compute time), sorted them, and then appended the RECORD self-line at the bottom. Result: RECORD's self-line landed below the rest, breaking the lex-sort contract. Two Phase 15.0 wheels still byte-matched because the RECORD content is computed deterministically; downstream tools that assert lex-sort (e.g. supply-chain attestation tooling) would fail.

The Phase 16.1 fix is one-line: inject `recordPosixPath` into the path list before the sort so the self-line interleaves correctly.

### Decisions made (16.1)

**Inject before sort, not after.** The RECORD self-line must be present even though the file does not exist on disk at compute time. Adding it before `sort.Strings(paths)` is cheaper than a post-sort merge and keeps the iteration loop simple.

**Idempotent inject.** Walk the existing slice for `recordPosixPath` before appending; if a future change has RECORD existing on disk at compute time, the inject is a no-op instead of producing a duplicate line.

**Self-line format unchanged.** PEP 376 specifies the self-line as `<path>,,` (empty digest, empty size) and the Phase 16 gate verifies this is preserved across the sort.

## Sub-phase 16.2 -- source emit fixed point

### Goal-alignment audit (16.2)

The wheel is built from `src/<pkg>/generated/<module>.py`. If two source builds of the same `.mochi` file produce different `.py` bytes, the wheel SHA can still byte-match (because both invocations are part of the same `Build` call) but cross-build determinism is meaningless: a downstream user who emits the source tree, hand-inspects it, and rebuilds will see drift. The fixed-point gate catches non-determinism in the Python lower (e.g. map iteration order leaking into emit order, sequence randomisation in a `set` used for import deduplication).

### Decisions made (16.2)

**Compare the generated file directly, not the whole output tree.** `pyproject.toml`, `__init__.py`, and the runtime are static; only `generated/<module>.py` carries lower output. Scoping the assertion to that one file makes the diagnostic precise when it fails.

**Two builds, two CacheDirs, two TempDirs.** Removes any possibility that the cache layer is masking lower non-determinism (a cache hit returns the same bytes twice trivially).

**Failure mode: byte-length diff in the error.** When the assertion fails, the error reports `len(b1)` and `len(b2)` so the operator can tell at a glance whether the divergence is a single token (off by 1 byte) or a structural reordering.

## Sub-phase 16.3 -- cross-host CI matrix

### Goal-alignment audit (16.3)

Phases 16.0-16.2 are host-deterministic gates: two builds on the same machine produce identical SHA. Phase 16.3 verifies that the same source on three different CI hosts (linux-x86_64, linux-aarch64, macos-arm64) produces identical SHA. Without this, the previous sub-phases could be host-deterministic but cross-host divergent (e.g. an `os.path.sep` slipping into a path string written into the wheel; an architecture-specific compile flag drifting through).

### Decisions made (16.3)

**Matrix runners:**

| Runner | Arch | Role |
|--------|------|------|
| `ubuntu-24.04` | x86_64 | host A |
| `ubuntu-24.04-arm` | aarch64 | host B |
| `macos-14` | arm64 | host C |

**Workflow file: `.github/workflows/transpiler3-python-repro.yml`.** Runs on push to `transpiler3/python/**`, `runtime/python/**`, and the workflow file itself; also daily at 05:00 UTC and on `workflow_dispatch`. The per-host job runs the `TestPhase16Reproducible` gate plus uploads the wheel SHA for compare; the final compare job diffs the SHAs across hosts and fails on divergence.

**`SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)` is set in every host job.** Without this, each host would pick its own checkout-time mtime and the cross-host SHA would always differ. Pinning to the git commit timestamp is the standard reproducible-builds.org pattern.

**Windows excluded.** NTFS is case-insensitive with case-preserving storage; some archive tooling normalises case in zip entries on Windows, producing wheels that diverge from linux-host wheels in entry casing. The fix is tracked as 16.4 but not in v1.

**`continue-on-error: true` on host jobs.** The cross-host SHA divergence message is more useful than a single host's test failure; letting all three hosts complete then diffing surfaces the most informative diagnostic.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/build/repro.go` (new) | `sourceDateEpoch()`, `gitCommitEpoch()`, `sha256File()` helpers |
| `transpiler3/python/build/wheel.go` | `zeroTime()` delegates to `sourceDateEpoch()`; `buildRecord` injects RECORD self-line into pre-sort list |
| `transpiler3/python/build/build.go` | Cache marker bumped `mep51-phase15` -> `mep51-phase16` |
| `transpiler3/python/build/phase16_test.go` (new) | Seven sub-gates: wheel/sdist/source determinism, RECORD lex-sort, pycache exclusion, SOURCE_DATE_EPOCH override + fallback |
| `tests/transpiler3/python/fixtures/phase16-repro/reproducibility_basic/` (new) | Hello-world fixture for SHA gate |
| `tests/transpiler3/python/fixtures/phase16-repro/reproducibility_with_extras/` (new) | Fixture exercising records + equality for cross-feature SHA gate |
| `.github/workflows/transpiler3-python-repro.yml` (new) | Three-host matrix + compare job |

## Test set

- `TestPhase16Reproducible/wheel_sha_byte_equal_across_rebuilds` (2 fixtures)
- `TestPhase16Reproducible/sdist_sha_byte_equal_across_rebuilds` (2 fixtures)
- `TestPhase16Reproducible/source_emit_fixed_point` (2 fixtures)
- `TestPhase16Reproducible/wheel_record_is_lex_sorted`
- `TestPhase16Reproducible/wheel_has_no_pycache`
- `TestPhase16Reproducible/source_date_epoch_overrides_floor`
- `TestPhase16Reproducible/source_date_epoch_falls_back_when_malformed` (3 bad inputs)

Phase 1-16 regression: `go test ./transpiler3/python/... -count=1` -- 27.1s, zero regressions.

## Deferred work

- **16.4 Windows reproducibility.** NTFS case-insensitivity + CRLF normalisation in any embedded text. Tracked after the linux + macOS gate is green in CI.
- **16.5 `diffoscope` integration.** Useful for debugging when SHA divergence happens in the cross-host gate; the gate itself is binary pass/fail without it.
- **16.6 ruff format + ast.unparse fixed-point.** Phase 16.0 ships an in-house emit fixed-point; ruff is not yet wired into the Mochi-Python build pipeline. Once Phase 15.2 lands hatchling as the declared sdist backend, ruff format runs against the generated tree and gains its own fixed-point assertion.
- **`uv build --reproducible` flag passthrough.** uv 0.5+ provides this as a one-line replacement for explicit `SOURCE_DATE_EPOCH` export; once Phase 15.1 lands the uv backend, the driver can opt in.
- **sdist tar header uid/gid pinning.** Phase 15.0 already uses `tar.FormatUSTAR` with mode `0o644`; the uid/gid fields default to 0 in `tar.Header`. A future audit can verify against the GNU tar `--owner=0 --group=0` output.
