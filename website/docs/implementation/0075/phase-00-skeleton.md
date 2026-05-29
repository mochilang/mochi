---
title: "Phase 0. Skeleton"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton"
description: "MEP-75 Phase 0 lands package3/php/ skeleton: Driver / Workspace types, errors package with SkipReport and 16 PHP-specific SkipReason variants, stub packages for all 12 bridge sub-packages."
---

# Phase 0. Skeleton

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-75 §Phases](/docs/mep/mep-0075#phases) |
| Status         | IN PROGRESS |
| Started        | 2026-05-29 23:11 (GMT+7) |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |
| Commit         | — |

## Gate

`TestPhase0Skeleton` in `package3/php/build/phase00_test.go`: subtests `end_to_end`, `package_layout`, `workspace_subdirs`.

- `end_to_end`: Driver allocates a `mochi-php-` prefixed temp work-dir, PrepareWorkspace succeeds, Cleanup removes the work-dir and resets WorkDir to empty.
- `package_layout`: verifies all 13 expected files exist on disk under `package3/php/` (README.md + 12 Go package stubs).
- `workspace_subdirs`: EnsureSubDirs creates `shims/`, `glue/`, and `vendor/` under the workspace root with the correct suffix.

In addition, the package-level test suite covers:

- `package3/php/errors/`: 16 SkipReason variants string-encode round-trip, exhaustive sweep catches missing switch cases, SkipReport renders with and without an override line, BridgeError formats with and without a package, errors.Is unwraps BridgeError.Cause.
- `package3/php/build/`: NewDriver defaults (CacheDir contains "php-deps", PHPBin defaults to "php"), NoCache returns empty CacheDir, Verbose + Deterministic flags, PrepareWorkspace creates dir with mochi-php- prefix, PrepareWorkspace idempotence, explicit WorkDir, Cleanup removes dir, Cleanup without Prepare is a no-op, XDG_CACHE_HOME overrides defaultCacheDir, EnsureSubDirs creates shims/glue/vendor, EnsureSubDirs idempotent.

## Lowering decisions

Phase 0 ships no PHP reflection or type mapping yet, only the scaffolding the later phases depend on. The `Driver` exports `NewDriver(Options) -> *Driver`, `PrepareWorkspace() -> (*Workspace, error)`, `Cleanup() -> error`. `Workspace` exports `EnsureSubDirs() -> error`.

The `SkipReason` enum lands all 16 refusal classifications from research note 05. The reasons are PHP-specific: `SkipMixed`, `SkipObject`, `SkipUntypedArray`, `SkipSelfStatic`, `SkipCallable`, `SkipResource`, `SkipIntersection`, `SkipNever`, `SkipVararg`, `SkipPrivate`, `SkipAbstractNoImpl`, `SkipMagicMethod`, `SkipAnonymousClass`, `SkipNoReflection`, `SkipExtension`. Each constant's `String()` produces a stable token used in the `SKIPPED.txt` golden fixtures.

The `BridgeError` type carries `(Phase, Package, Cause)` and formats as `phase[package]: cause` (with the bracketed segment omitted when package is empty). It implements `Unwrap` so `errors.Is` and `errors.As` traverse through it.

The cache-dir resolution honours `$XDG_CACHE_HOME` first, then falls back to `~/.cache/mochi/php-deps/`, then to `$TMPDIR/mochi-php-cache`. This matches the resolution strategy in the MEP-75 spec §8.

## Files changed

| File | Purpose |
|------|---------|
| `package3/php/build/driver.go` | `Driver`, `Options`, `Workspace`, `NewDriver`, `PrepareWorkspace`, `Cleanup`, `defaultCacheDir`, `EnsureSubDirs` |
| `package3/php/build/driver_test.go` | Driver + Workspace unit tests |
| `package3/php/build/phase00_test.go` | `TestPhase0Skeleton` sentinel with 3 subtests |
| `package3/php/errors/errors.go` | `SkipReason` (16 variants), `SkipReport`, `BridgeError`, `Wrap` |
| `package3/php/errors/errors_test.go` | Errors unit tests |
| `package3/php/packagist/packagist.go` | Package stub (phase 1) |
| `package3/php/cache/cache.go` | Package stub (phase 2) |
| `package3/php/reflect/reflect.go` | Package stub (phase 3) |
| `package3/php/typemap/typemap.go` | Package stub (phase 4) |
| `package3/php/externemit/externemit.go` | Package stub (phase 5) |
| `package3/php/glue/glue.go` | Package stub (phase 6) |
| `package3/php/autoload/autoload.go` | Package stub (phase 7) |
| `package3/php/lock/lock.go` | Package stub (phase 8) |
| `package3/php/library/library.go` | Package stub (phase 9) |
| `package3/php/publish/publish.go` | Package stub (phase 10) |
| `package3/php/README.md` | Bridge overview, pipeline diagram, type table |
| `website/docs/implementation/0075/index.md` | MEP-75 implementation tracking index |
| `website/docs/implementation/0075/phase-00-skeleton.md` | This file |

## Test set

- `TestPhase0Skeleton/end_to_end`
- `TestPhase0Skeleton/package_layout`
- `TestPhase0Skeleton/workspace_subdirs`
- All `package3/php/build/...` and `package3/php/errors/...` unit tests.

## Closeout notes

Phase 0 is the smallest viable skeleton: enough to give later phases a stable package tree and a CI-green starting point. The driver's WorkDir is allocated with a `mochi-php-` prefix so Cleanup can safely refuse to remove a user-provided directory (a user-specified WorkDir survives Cleanup, per the explicit-vs-allocated distinction). The PHPBin field defaults to `"php"` and is overrideable for test environments that install PHP at a non-standard path.

No external runtime dependencies are introduced. The build adds two active Go packages to the repo (`package3/php/build/` and `package3/php/errors/`) and ten stub packages, all pure-Go with stdlib-only imports.
