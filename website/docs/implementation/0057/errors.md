---
title: "MEP-57 error code registry"
sidebar_position: 0
sidebar_label: "Error registry"
description: "Single source of truth for every M057_* error code used by the Mochi package system phases. Cross-references the owning phase, trigger, and recovery hint."
---

# MEP-57 error code registry

Every error returned by `cmd/mochi/pkg*` or `pkg/pkg*` packages must be sourced
from a sentinel listed here. Phase docs link to a row rather than redeclaring
a local table, so `errors.Is(err, pkgerr.LockE001)` works across phase
boundaries.

Conventions:

- All codes are namespaced `M057_<CATEGORY>_E<NNN>`.
- Phase docs introduce sentinels under `pkg/pkgerr/`; each sentinel is
  exported as `ErrCATEGORYExxx`.
- Codes are dense within a category (`E001`, `E002`, ... no gaps).
- A code is owned by exactly one phase. Other phases wrap with `%w` rather
  than re-declaring.
- User-facing rows include a recovery hint. Internal markers (cold-cache,
  retry-from-sentinel) are tagged `internal`.

## MANIFEST (Phase 1)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_MANIFEST_E001 | 1.1 | TOML lex/parse failure | "fix the syntax at line N" |
| M057_MANIFEST_E002 | 1.2 | Unknown top-level key | "remove or rename `<key>`" |
| M057_MANIFEST_E003 | 1.2 | Missing required `[package]` field | "add `<field>` to `[package]`" |
| M057_MANIFEST_E004 | 1.2 | Invalid version range syntax | "see semver caret/tilde docs" |
| M057_MANIFEST_E005 | 1.2 | Invalid SPDX 3.x license expression | "use a valid SPDX expression" |
| M057_MANIFEST_E006 | 1.2 | Conflicting feature flag enables | "remove the duplicate" |
| M057_MANIFEST_E007 | 1.2 | Invalid edition string | "use a supported edition" |
| M057_MANIFEST_E008 | 1.5 | Workspace inheritance value not resolvable | "set value or remove `workspace = true`" |
| M057_MANIFEST_E009 | 1.5 | Inheritance cycle | "break the cycle in workspace.toml" |
| M057_MANIFEST_E010 | 1.2 | Capability not in the closed cap set | "see Phase 10 §10.1 for the cap vocabulary" |
| M057_MANIFEST_E011 | 1.2 | Target name not in the supported set | "see Phase 14 for supported targets" |
| M057_MANIFEST_E012 | 1.2 | Override declares unknown target | "remove or correct target name" |

## RESOLVE (Phase 2)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_RESOLVE_E001 | 2.0 | Ambiguous import spec | "qualify with `@scope/name`" |
| M057_RESOLVE_E002 | 2.0 | No manifest at import path | "add `mochi.toml` to the package root" |
| M057_RESOLVE_E003 | 2.4 | `@req` source pin disagrees with locked version | "re-run `mochi pkg lock`" |
| M057_RESOLVE_E004 | 2.0 | Package not found in any registry | "check scope/name spelling" |
| M057_RESOLVE_E005 | 2.0 | Locked version yanked, no successor | "re-run `mochi pkg lock --upgrade <pkg>`" |
| M057_RESOLVE_E006 | 2.0 | Locked package missing from cache (offline) | "see Phase 18: vendor or refresh cache" |
| M057_RESOLVE_E007 | 2.2 | Cold-cache sentinel (internal) | internal |
| M057_RESOLVE_E008 | 2.3 | Cache lock contention exceeded timeout | "another mochi process holds the lock" |
| M057_RESOLVE_E009 | 2.3 | Cache integrity check failed | "run `mochi pkg cache verify`" |
| M057_RESOLVE_E010 | 2.0 | Both `mochi.toml` and `mochi.workspace.toml` in the same directory | "rename or delete one" |

## WORKSPACE (Phase 3)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_WORKSPACE_E001 | 3.0 | Duplicate member name | "rename one of the members" |
| M057_WORKSPACE_E002 | 3.0 | Member matches multiple globs | "narrow `members` or use `exclude`" |
| M057_WORKSPACE_E003 | 3.7 | Workspace dependency cycle | "break the cycle in member manifests" |
| M057_WORKSPACE_E004 | 3.0 | Member glob escapes workspace root | "remove `..` from the glob" |
| M057_WORKSPACE_E005 | 3.0 | Nested workspace (member is itself a workspace root) | "demote the nested manifest" |

## LOCK (Phase 4)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_LOCK_E001 | 4.4 | Manifest changed since lock | "re-run `mochi pkg lock`" |
| M057_LOCK_E002 | 4.4 | Resolution drifted (mirror state change) | "re-run `mochi pkg lock --upgrade`" |
| M057_LOCK_E003 | 4.0 | Lockfile format version newer than mochi | "upgrade mochi to read this lock" |
| M057_LOCK_E004 | 4.0 | Lockfile TOML parse failure | "do not hand-edit `mochi.lock`" |
| M057_LOCK_E005 | 4.0 | Missing required lock field | "regenerate the lockfile" |
| M057_LOCK_E006 | 4.7 | New capability appeared without `--accept-capabilities` | "audit the new cap then accept" |

## SOLVER (Phase 5)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_SOLVER_E001 | 5.11 | Watchdog: exceeded 60s wall or 1M iterations | "narrow ranges or split workspaces" |
| M057_SOLVER_E002 | 5.5 | Unsatisfiable constraint set (root in conflict) | "Phase 6 explanation in the error body" |
| M057_SOLVER_E003 | 5.10 | Capability denied: candidate requires caps consumer pin disallows | "extend pin or pick another version" |
| M057_SOLVER_E004 | 5.10 | Target unsupported: candidate misses a required target | "drop target or pick another version" |
| M057_SOLVER_E005 | 5.10 | Compiler incompat: candidate's mochi range excludes current | "upgrade mochi or pick another version" |

## INDEX (Phase 7 local registry, Phase 8 sparse index)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_INDEX_E001 | 7.6 | Local index FS read failed | "check filesystem permissions" |
| M057_INDEX_E002 | 8.3 | Sparse index JSONL parse failure | "the registry served malformed data" |
| M057_INDEX_E003 | 8.9 | Index format version exceeds client's max known | "upgrade mochi" |
| M057_INDEX_E004 | 8.5 | Fetch failed after retries | "check network/mirror health" |
| M057_INDEX_E005 | 8.5 | 429 rate-limit not respected by backoff | "wait and retry; consider mirror" |
| M057_INDEX_E006 | 11.2 | Mirror returned content that diverges from primary | "see `mochi pkg audit mirror`" |
| M057_INDEX_E007 | 8.7 | All failover entries exhausted | "configure additional mirror" |
| M057_INDEX_E008 | 7.6 | Package not found in local index | "check name; refresh index" |

## BLOB (Phase 9 content store, Phase 11 mirror, Phase 18 vendor)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_BLOB_E001 | 9.4 | BLAKE3 of fetched bytes does not match the URL hex | "treat the mirror as untrusted" |
| M057_BLOB_E002 | 9.0 | Blob fetch partial / connection reset | "retried automatically; see logs" |
| M057_BLOB_E003 | 9.5 | Tar entry path-escape or absolute path | "the publisher tarball is malformed" |
| M057_BLOB_E004 | 9.5 | Tar entry exceeds per-entry size cap | "see Phase 9 hard limit" |
| M057_BLOB_E005 | 9.5 | Tar decompressed size exceeds bomb cap | "report to the registry; do not extract" |
| M057_BLOB_E006 | 18.6 | Vendor verify: extracted hash does not match lock | "the vendor tree was edited" |
| M057_BLOB_E007 | 7.3 | Local registry blob 404 | "check the local registry root" |

`M057_BLOB_E001` was previously raised by Phases 7, 9, 11, 18 with conflicting
semantics. The single owner is Phase 9.4 (BLAKE3 mismatch on extract). Phase
7's 404 is now `BLOB_E007`. Phase 11's mirror divergence is `INDEX_E006`.
Phase 18's verify mismatch is `BLOB_E006`.

## CAP (Phase 10)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_CAP_E001 | 10.3 | Consumer pin denies a required capability | "extend pin or pick another version" |
| M057_CAP_E002 | 10.5 | Monotonicity violation: cap added between versions without minor bump | "publish a minor bump" |
| M057_CAP_E003 | 10.7 | Capability vocabulary unknown to this mochi | "upgrade mochi" |
| M057_CAP_E004 | 10.10 | `.caps.json` sidecar does not match resolved tree | "re-run `mochi pkg lock`" |
| M057_CAP_E005 | 10.5 | Tarball-time capability re-check disagrees with lock | "the package was tampered with" |

## PUB (Phase 12)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_PUB_E001 | 12.0 | Pre-flight: missing `[package].repository` | "add repository URL" |
| M057_PUB_E002 | 12.0 | Pre-flight: working tree dirty | "commit or stash before publish" |
| M057_PUB_E003 | 12.0 | Pre-flight: version already exists | "bump version" |
| M057_PUB_E004 | 12.5 | Tarball size exceeds 50MB hard limit | "split package" |
| M057_PUB_E005 | 12.6 | Registry returned 5xx after retries | "check registry status" |
| M057_PUB_E006 | 12.6 | Registry returned 401 (auth fail) | "refresh `MOCHI_TOKEN`" |
| M057_PUB_E007 | 12.0 | Pre-flight: license string missing or unrecognised | "use a valid SPDX expression" |
| M057_PUB_E008 | 12.0 | Pre-flight: workspace member cannot be published | "publish the workspace root or split" |

## SIG (Phase 13)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_SIG_E001 | 13.0 | OIDC token unavailable in CI | "set `permissions: id-token: write`" |
| M057_SIG_E002 | 13.3 | Fulcio rejected CSR | "see Fulcio error in body" |
| M057_SIG_E003 | 13.4 | Rekor submission failed after timeout | "Rekor may be degraded" |
| M057_SIG_E004 | 13.6 | Sigstore bundle signature does not verify | "treat as untrusted" |
| M057_SIG_E005 | 13.6 | Rekor inclusion proof invalid | "treat as untrusted" |
| M057_SIG_E006 | 13.6 | OIDC subject does not match publisher binding | "verify expected publisher" |
| M057_SIG_E007 | 13.9 | Publisher binding API rejected request | "check registry binding policy" |
| M057_SIG_E008 | 13.10 | TUF root expired or signature invalid | "refresh TUF root from pinned source" |
| M057_SIG_E009 | 13.10 | TUF metadata rollback detected | "abort; report to registry operator" |

## TUF (Phase 13 metadata client)

`SIG_E008` and `SIG_E009` cover root + rollback at the trust-root layer.
`TUF_E*` codes cover the targets-metadata client used per fetch (separate
sentinels so callers can distinguish "trust root broken" from "this
specific target failed").

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_TUF_E001 | 13.10 | TUF targets metadata expired or signature invalid | "refresh metadata; check clock skew" |
| M057_TUF_E002 | 13.10 | TUF target hash/length disagrees with metadata | "treat as untrusted; report to operator" |

## FAN (Phase 14 polyglot fan-out)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_FAN_E001 | 14.0 | Driver unavailable: tool not on PATH | "install `<tool>`" |
| M057_FAN_E002 | 14.0 | Target name not in supported set | "see Phase 14 supported targets" |
| M057_FAN_E003 | 14.10 | Strict mode: per-target failure aborts all | "fix the failing target or drop `--strict`" |
| M057_FAN_E004 | 14.3-14.8 | Target-specific schema rejection | "see target driver logs" |
| M057_FAN_E005 | 14.10 | Rollback: published target cannot be un-published | "operator intervention required" |

## SBOM (Phase 15)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_SBOM_E001 | 15.6 | CycloneDX schema validation failed | "see emitted message for field" |
| M057_SBOM_E002 | 15.6 | SPDX schema validation failed | "see emitted message for field" |
| M057_SBOM_E003 | 15.3 | in-toto Statement does not match resolved tree | "re-run publish from a clean tree" |
| M057_SBOM_E004 | 15.4 | PURL malformed (invalid scope, version, or qualifier) | "fix the manifest field surfaced in the message" |

## ADV (Phase 16 advisory)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_ADV_E001 | 16.5 | Audit found vulnerabilities at or above `--fail-on` threshold | "see hit list; consider `ignore`" |
| M057_ADV_E002 | 16.2 | Advisory feed refresh failed | "retry with `--refresh`" |
| M057_ADV_E003 | 16.0 | Advisory YAML schema invalid | "advisory author / registry bug" |
| M057_ADV_E004 | 16.6 | `[ignore-advisories]` references unknown ID | "remove or update the ignore" |

## REPRO (Phase 17)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_REPRO_E001 | 17.2 | Tarball contains non-deterministic metadata (xattr, PAX record) | "drop the xattr; rebuild" |
| M057_REPRO_E002 | 17.5 | Twice-built tarball hashes differ | "see [phase 17](./phase-17-repro) repro harness" |
| M057_REPRO_E003 | 17.6 | Consumer rebuild does not match registry hash | "report to publisher; treat as untrusted" |
| M057_REPRO_E004 | 17.7 | Source repository tag missing or moved | "publisher must re-tag commit" |
| M057_REPRO_E005 | 17.0 | `SOURCE_DATE_EPOCH` not a valid integer | "unset or set to seconds since epoch" |

## OFFLINE (Phase 18)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_OFFLINE_E001 | 18.3 | Cache miss in offline mode | "vendor the dep or drop `--offline`" |
| M057_OFFLINE_E002 | 18.5 | Frozen mode: lockfile drift | "re-run `mochi pkg lock` first" |
| M057_OFFLINE_E003 | 18.7 | Hard offline: configuration change refused | "unset `MOCHI_OFFLINE=hard`" |

## CACHE (Phase 19)

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_CACHE_E001 | 19.6 | GC raced a concurrent build | "the build will retry transparently" |
| M057_CACHE_E002 | 19.0 | Hardlink installer hit per-inode limit | "fallback to copy is automatic" |
| M057_CACHE_E003 | 19.6 | `mochi pkg cache verify` found tampering | "run `mochi pkg cache prune`" |

## PERF (Phase 19)

`CACHE_*` covers correctness of the on-disk cache. `PERF_*` covers
bench-gate and operational failures of GC/install paths, which are
recoverable but user-visible.

| Code | Owner | Trigger | Recovery hint |
|------|-------|---------|---------------|
| M057_PERF_E001 | 19.5 | Bench result exceeds rolling baseline beyond threshold | "see `bench/baseline.json`; investigate regression" |
| M057_PERF_E002 | 19.6 | Cache GC failed (filesystem permissions, disk full) | "free disk space; check `$MOCHI_HOME` permissions" |
| M057_PERF_E003 | 19.0 | Hardlink failed across filesystems; fell back to copy | "internal; logged at debug, not surfaced to user" |

## Localization policy

The English error sentence is the source of truth for snapshot tests. Renderers
may translate the body but must preserve the code (`M057_*`) verbatim so users
can grep dashboards. Phase 6 explanations are localizable; Phase 1-5 messages
are not.

## Naming convention enforcement

A unit test in `pkg/pkgerr/registry_test.go` walks every exported sentinel,
parses the code, and fails if:

- the code is not of shape `M057_<CATEGORY>_E<NNN>`,
- two sentinels share the same code,
- a code is gapped (`E001`, `E003` without `E002`),
- the owning phase number conflicts with this registry.

The test ships in Phase 0.4 so collisions are caught at PR time.
