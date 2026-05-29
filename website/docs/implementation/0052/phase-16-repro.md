---
title: "Phase 16. Reproducible build"
sidebar_position: 17
sidebar_label: "Phase 16. Reproducible build"
description: "MEP-52 Phase 16, byte-identical .tgz SHA256 across two CI hosts via SOURCE_DATE_EPOCH plus sorted tarball entries; npm 9.5+ honours SOURCE_DATE_EPOCH natively; same applies to JSR via deno publish."
---

# Phase 16. Reproducible build

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 16](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase16Repro`: build the same Mochi source on two distinct CI hosts (linux-x64 GitHub runner and aarch64-darwin self-hosted runner) with `SOURCE_DATE_EPOCH=<commit unix time>`, then assert that the `<pkg>-<ver>.tgz` SHA256 is byte-identical. Secondary gates: tarball entries sorted lexicographically; mtime, uid, gid normalised; no `__filename`, `__dirname`, or `import.meta.url` literal leaks the build host's filesystem layout into emitted source.

## Goal-alignment audit

Reproducible builds are how a downstream consumer (or a security auditor) verifies that the `.tgz` they downloaded from npm corresponds to the source in the linked Git commit. Without reproducibility, the only thing `npm publish --provenance` (Phase 18) can attest is "this tarball was built by this CI job", not "this tarball is the deterministic output of this source". Phase 16 is the prerequisite for Phase 18's provenance statement to be auditable end-to-end. The two hosts in the gate (Intel Linux runner and Apple Silicon Darwin runner) are chosen because they have different filesystem layouts, different default `umask`, different temp-dir paths, all places where non-reproducibility tends to leak in.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 16.0 | `SOURCE_DATE_EPOCH` plumbing into `npm pack`; verify npm 9.5+ honours it for all tar header timestamps | NOT STARTED | n/a |
| 16.1 | Sorted tarball entries; pre-pack step rewrites `package.json` `files` glob expansion to a sorted list | NOT STARTED | n/a |
| 16.2 | Normalise `uid`, `gid`, permission bits; tar header uniformity | NOT STARTED | n/a |
| 16.3 | Strip build-host leakage from emitted TS source: no `__filename`, no `__dirname`, no absolute path literals from the build machine | NOT STARTED | n/a |
| 16.4 | Two-host gate harness; CI matrix runs the same build on linux-x64 and aarch64-darwin then SHA256 diffs | NOT STARTED | n/a |

## Sub-phase 16.0, SOURCE_DATE_EPOCH

### Decisions made (16.0)

**Source**: the commit's Unix timestamp (`git log -1 --format=%ct`). The transpiler reads it from the environment; if `SOURCE_DATE_EPOCH` is unset, it falls back to `0` (Unix epoch). This matches the Reproducible Builds spec.

**Plumbing**: `transpiler3/typescript/build/repro.go` exports `BuildEnv()` which returns the environment slice for the `npm pack` subprocess. `SOURCE_DATE_EPOCH=<n>` is unconditionally set.

**npm 9.5+**: honours `SOURCE_DATE_EPOCH` for tar header `mtime`. Verified empirically against npm 10.9 (shipped with Node 22.11.0).

```go
// transpiler3/typescript/build/repro.go
func BuildEnv(commitUnixTime int64) []string {
    return append(
        os.Environ(),
        fmt.Sprintf("SOURCE_DATE_EPOCH=%d", commitUnixTime),
        "TZ=UTC",
    )
}
```

**`TZ=UTC`**: prevents tar mtime from being interpreted in the host's local timezone in any sub-tool.

## Sub-phase 16.1, Sorted tarball entries

### Decisions made (16.1)

**Why a pre-pack step**: npm `pack` walks `files` in `package.json`. Glob expansion order depends on `readdir`, which on some filesystems is insertion order (ext4) and on others is inode order (xfs). The pre-pack rewrites the glob result to an explicit sorted file list:

```json
{
  "files": [
    "dist/browser/index.d.ts",
    "dist/browser/index.js",
    "dist/browser/index.js.map",
    "dist/bun/index.d.ts",
    "dist/bun/index.js",
    "dist/bun/index.js.map",
    "dist/deno/index.d.ts",
    "dist/deno/index.js",
    "dist/deno/index.js.map",
    "dist/node/index.d.ts",
    "dist/node/index.js",
    "dist/node/index.js.map",
    "LICENSE",
    "README.md"
  ]
}
```

The transpiler emits the `files` list in lexicographic order. npm 9.5+ packs in the listed order; readdir is bypassed.

## Sub-phase 16.2, Normalised tar headers

### Decisions made (16.2)

**`uid=0`, `gid=0`**: npm packs tar with `--owner=0 --group=0` since 7.0; the entries are owned by root in the tarball. No host-uid leak.

**Permission bits**: npm normalises to `0644` for files and `0755` for directories. Already canonical.

**No symlinks, no devices**: the Mochi dist tree never contains symlinks; the emitter rejects any source path that resolves through a symlink.

## Sub-phase 16.3, No build-host leakage

### Decisions made (16.3)

**Emitter rules**:

- No `__filename` literal in emitted source. Mochi has no equivalent; if a user references `current_file()` (a Mochi reflection intrinsic, deferred to v2), the transpiler errors.
- No `__dirname` literal. Same rationale.
- No `import.meta.url`. The emitter never uses this; per MEP-52 §6 `import.meta.url` is reserved for the host runtime, not embedded in emitted code.
- No absolute path literals (`/Users/...`, `/home/...`, `C:\...`) from the build machine. The path-walking passes carry relative paths only; source-map paths are relativised against the package root.

**Source-map path normalisation**: `dist/node/index.js.map` carries `../../src/main.ts` (relative to the dist file), not `/home/runner/work/.../src/main.ts`. The transpiler's source-map writer normalises before writing.

## Sub-phase 16.4, Two-host gate

### Decisions made (16.4)

**Hosts**: linux-x64 (GitHub-hosted `ubuntu-22.04` runner) and aarch64-darwin (self-hosted Apple Silicon runner). Both use the same Node 22.11.0, npm 10.9.0.

**Gate harness**:

```bash
export SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)
mochi build --target=npm-package -o /tmp/out
sha256sum /tmp/out/*.tgz > /tmp/sha.txt
```

CI uploads `sha.txt` from each host; the gate job downloads both and `diff`s. Any mismatch fails Phase 16.

**Why not a single host**: a single host can't catch host-leakage bugs by definition. The two-host gate is the only way to verify that the output is genuinely independent of the build environment.

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/build/repro.go` | `SOURCE_DATE_EPOCH` env, sorted `files` list, source-map path normalisation |
| `transpiler3/typescript/build/pack.go` | `npm pack` subprocess with reproducible env |
| `transpiler3/typescript/build/phase16_test.go` | `TestPhase16Repro`, two-host SHA256 diff |
| `.github/workflows/phase16-repro.yml` | Matrix: linux-x64 + aarch64-darwin; upload-artifact `sha.txt`; downstream job diffs |

## Test set

- `TestPhase16Repro`, two-host SHA256 diff over the full Phase 1-15 corpus.
- `TestPhase16NoHostLeak`, greps emitted source for `/home/`, `/Users/`, `__filename`, `__dirname`, `import.meta.url`.
- `TestPhase16FilesSorted`, asserts `package.json` `files` is in lexicographic order.
- `TestPhase16SourceMapRelative`, asserts every `.js.map` `sources[]` entry is relative.

## Deferred work

- Reproducible `.tar.zst` (npm uses gzip; Sigstore supports zstd but npm registry does not yet accept). Phase 16 ships gzip only.
- Cross-platform-Windows reproducibility (CRLF/LF and case-folding tarball pitfalls). Windows builds are not part of the Phase 16 gate; Windows is a Phase 17 consumer test only.
- JSR reproducibility (`deno publish` against `jsr.io` with `SOURCE_DATE_EPOCH`). Phase 17 verifies, Phase 16 gate is npm-only.
