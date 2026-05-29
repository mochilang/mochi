---
title: "Phase 16. Reproducible build"
sidebar_position: 17
sidebar_label: "Phase 16. Reproducible build"
description: "MEP-52 Phase 16, byte-identical .tgz SHA256 across same-host rebuilds via npm 11's hardcoded mtime invariant plus a Deterministic-mode forward-compat hedge that sets SOURCE_DATE_EPOCH + TZ=UTC; sorted `files` whitelist; no host leakage in emit."
---

# Phase 16. Reproducible build

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 16](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (same-host gate; 16.4 two-host CI matrix deferred) |
| Started        | 2026-05-30 01:33 (GMT+7) |
| Landed         | 2026-05-30 01:39 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |

## Gate

`TestPhase16ReproSameHost`: two independent builds of the same Mochi fixture (different temp dirs, different working dirs, both with `Driver.Deterministic = true`) produce byte-identical `.tgz` SHA256 across the curated 3-fixture corpus. Plus three supporting gates:

- `TestPhase16ReproWithoutDeterministic` documents the empirical finding: npm 11 (verified against 11.12.1) produces a reproducible tarball **by default**. The tar header `mtime` is hardcoded to 1985-10-26 in npm's pack pipeline (a long-standing tradition inherited from `pacote` and `npm-packlist`).
- `TestPhase16NoHostLeak` greps every emit for `/Users/`, `/home/`, `/private/`, `C:\`, `__filename`, `__dirname`, `import.meta.url`.
- `TestPhase16FilesSorted` asserts `package.json` `files` is in lex order so npm's pack order is host-independent of `readdir`'s filesystem-specific order.

## Goal-alignment audit

The MEP-52 §Phase 16 spec proposed a single gate: build the same Mochi source on two distinct CI hosts (linux-x64 GitHub runner and aarch64-darwin self-hosted runner) with `SOURCE_DATE_EPOCH=<commit unix time>`, then assert the `.tgz` SHA256 is byte-identical. Before starting Phase 16 I audited that gate against the user-facing goal.

Findings:

- The user-facing goal is "Mochi's npm `.tgz` is reproducibly built; a downstream consumer can verify the tarball matches the source". The smallest gate proving that is "build the same source twice with the same Mochi version, identical SHA256".
- npm 11.12.1 already produces a reproducible tarball **without** `SOURCE_DATE_EPOCH`. Empirical check: `npm pack` writes every tar entry with `mtime = 1985-10-26 00:00 UTC` regardless of the env. So the only non-determinism left in the pipeline lives in the Mochi emit + the file enumeration order, both of which we control.
- The two-host gate is the only way to catch host-leakage bugs (different temp paths, different umask, different timezone). I have one host. The same-host gate covers every non-determinism inside our pipeline; the cross-host check lands with 16.4 once the CI matrix is wired.
- The MEP-52 §Phase 16 spec proposed `SOURCE_DATE_EPOCH` plumbing. npm 11 ignores the variable, so the plumbing is dead-code-but-correct today. I shipped it anyway as a forward-compat hedge: if a future npm release replaces the hardcoded 1985 with a fresh timestamp, the `Driver.Deterministic` opt-in already feeds it a stable value.

Conclusion: the user-facing Phase 16 goal (reproducible .tgz) is satisfied by npm's built-in determinism plus a sorted `files` list and a no-host-leakage emit. The remaining surface (cross-host CI matrix) lands as 16.4.

## Lowering

The TypeScript driver gains two opt-in fields:

```go
type Driver struct {
    // ... Phase 1 to 15 fields
    Deterministic   bool   // Phase 16 mode toggle
    SourceDateEpoch int64  // Unix timestamp; defaults to 0 (the epoch)
}
```

When `Deterministic` is true and the target is `TargetNpmPackage`, the `npm pack` subprocess runs with:

```
SOURCE_DATE_EPOCH=<SourceDateEpoch>
TZ=UTC
```

appended to `os.Environ()`. npm 11 ignores both; the plumbing is a forward-compat hedge.

### `files` whitelist sort

The Phase 15 emit shipped `files: ["dist/", "README.md", "LICENSE"]`. Phase 16.1 changes this to lex order: `["LICENSE", "README.md", "dist/"]` (because `L`(0x4C) < `R`(0x52) < `d`(0x64)). npm 9.5+ packs entries in the listed order; the sort makes the tarball's top-level entry order host-independent of `readdir`'s filesystem-specific order.

### No host leakage

Phase 16.3 forbids the following tokens from ever appearing in the emit:

- Absolute path literals: `/Users/`, `/home/`, `/private/`, `C:\`.
- Node-specific runtime probes: `__filename`, `__dirname`, `import.meta.url`. Mochi has no source-level analogue, so any occurrence is an emitter bug.

The check runs over the Phase 15 curated corpus's emit (one per major lowering category) and is wired as a regression gate; future phases that introduce new emit paths get the same scan automatically.

## Sub-phases

| #    | Scope                                                                                                            | Status   | Commit |
|------|-------------------------------------------------------------------------------------------------------------------|----------|--------|
| 16.0 | `SOURCE_DATE_EPOCH` + `TZ=UTC` plumbing; sorted `files`; no-host-leakage gate; same-host SHA256 invariant         | LANDED   | (this PR) |
| 16.1 | (folded into 16.0) sorted `files` whitelist                                                                       | LANDED   | (this PR) |
| 16.2 | (folded into 16.0) normalised tar headers (uid=0, gid=0, perms 0644/0755); npm 11 already does this               | LANDED   | (this PR) |
| 16.3 | (folded into 16.0) no `__filename`, `__dirname`, `import.meta.url`, absolute path leakage in emit                 | LANDED   | (this PR) |
| 16.4 | Two-host CI matrix gate (linux-x64 + aarch64-darwin); upload `sha.txt`, downstream job diffs                      | DEFERRED | n/a    |

## Files

| File | Purpose |
|------|---------|
| `transpiler3/typescript/build/repro.go` | `reproBuildEnv(epoch)` returns env slice with `SOURCE_DATE_EPOCH` + `TZ=UTC` |
| `transpiler3/typescript/build/build.go` | `Driver.Deterministic` + `SourceDateEpoch` fields; npm-pack env override |
| `transpiler3/typescript/build/npm_pack.go` | `runNpmPack(pkgDir, env)` accepts env override |
| `transpiler3/typescript/emit/npmpkg.go` | `files` whitelist in lex order |
| `transpiler3/typescript/build/phase16_test.go` | `TestPhase16ReproSameHost`, `TestPhase16ReproWithoutDeterministic`, `TestPhase16NoHostLeak`, `TestPhase16FilesSorted` |

## Test set

- `TestPhase16ReproSameHost`, 3 curated fixtures (hello, scalars, closures), two builds each, byte-identical SHA256.
- `TestPhase16ReproWithoutDeterministic`, documents npm 11's default determinism; gate fails if a future npm release introduces fresh timestamps.
- `TestPhase16NoHostLeak`, scans 6 curated fixtures' emits for 7 forbidden tokens.
- `TestPhase16FilesSorted`, asserts `package.json` `files` is `sort.StringsAreSorted` true.

## Empirical: npm 11 reproducibility surface

Direct check against the npm CLI:

```
$ SOURCE_DATE_EPOCH=1700000000 TZ=UTC npm pack
$ SOURCE_DATE_EPOCH=1800000000 TZ=UTC npm pack
$ shasum -a 256 *.tgz
d5da5c8ccc...  t1.tgz
d5da5c8ccc...  t2.tgz
$ gzcat t1.tgz | tar -tvf - | head -1
-rw-r--r--  0 0      0          19 Oct 26  1985 package/index.js
```

Both epochs produce the same SHA, both tar headers carry `mtime = 1985-10-26`. The npm CLI's tar pipeline is already host- and time-independent. The spec's `SOURCE_DATE_EPOCH` proposal is therefore architecturally redundant against current npm; the plumbing remains as a forward-compat hedge.

## Deferred work

- Two-host CI matrix gate (16.4). Linux-x64 + aarch64-darwin runners; both upload `sha.txt`; downstream job `diff`s. Lands once a self-hosted Apple Silicon runner is in CI.
- Reproducible `.tar.zst` (npm uses gzip; the registry does not yet accept zstd). Out of scope for v1.
- Cross-platform Windows reproducibility (CRLF/LF and case-folding pitfalls). Windows is a consumer surface only; Phase 17 verifies, Phase 16 gate is Unix-only.
- JSR reproducibility (`deno publish` against `jsr.io` with `SOURCE_DATE_EPOCH`). Phase 17 verifies, Phase 16 gate is npm-only.
- Sigstore + provenance attestations. Phase 18 wires `npm publish --provenance` against the byte-identical tarball Phase 16 produces.
