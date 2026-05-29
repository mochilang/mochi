---
title: "Phase 0. Skeleton"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton"
description: "MEP-71 Phase 0 lands package3/python/ skeleton: Driver / Venv types, errors package with SkipReport, deterministic pyproject.toml venv-root renderer."
---

# Phase 0. Skeleton

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-71 §Phases](/docs/mep/mep-0071#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 22:20 (GMT+7) |
| Landed         | 2026-05-29 22:40 (GMT+7) |
| Tracking issue | (filled by automation) |
| Tracking PR    | (filled by automation) |
| Commit         | `8e1ef75f` |

## Gate

`TestPhase0Skeleton` in `package3/python/build/phase00_test.go`: subtests `end_to_end`, `package_layout`, `default_venv_invariants`. The first allocates a Driver, prepares a Venv, adds members and shared deps, writes the venv root pyproject.toml to a scratch directory, re-reads it, and asserts the expected substrings appear. The second verifies the on-disk layout of `package3/python/` (the documented Go packages exist). The third checks that `DefaultVenv()` round-trips through `Validate()` without error and renders a manifest containing the auto-generated header, the `[build-system]`, `[project]`, and `[python]` tables.

In addition, the package-level test suite covers:

- `package3/python/errors/`: 20 SkipReason variants string-encode round-trip, exhaustive sweep catches additions that forget to update the switch, out-of-range values render as `SkipUnknown`, SkipReport renders with and without an override line, SkipReport preserves empty Detail, BridgeError formats with and without a package, errors.Is unwraps BridgeError.Cause, errors.As recovers `Phase` + `Package`, nested BridgeError chain unwraps to the original cause.
- `package3/python/build/`: DefaultVenv defaults, AddMember sort-and-deduplicate, AddSharedDep replace semantics, RenderPyprojectToml content + determinism over 10 iterations, free-threaded / stubgen toggles, wrapper-vs-user member split in the rendered `[python.wrappers]` / `[python.members]` tables, Validate rejects unsupported implementation / runtime-mode / async-mode / duplicate paths / empty member fields, Driver cache-dir defaults including `XDG_CACHE_HOME` and `HOME` fallbacks, PrepareVenv idempotence, Cleanup idempotence + user-vs-allocated work-dir distinction, Cleanup no-op before PrepareVenv.

## Lowering decisions

Phase 0 ships no PyPI ingest yet, only the scaffolding the later phases depend on. The `Driver` exports `NewDriver(Options) -> *Driver`, `PrepareVenv() -> (*Venv, error)`, `WriteVenvRoot(*Venv) -> (string, error)`, and `Cleanup() -> error`. `Venv` exports `DefaultVenv()`, `AddMember`, `AddSharedDep`, `RenderPyprojectToml`, and `Validate`.

The pyproject.toml venv-root renderer is a small hand-rolled TOML writer. It uses no external library because (1) the schema is fixed and small, (2) the output must be byte-stable for the venv-cache key (planned for phase 8), and (3) avoiding pelletier/go-toml or burntsushi/toml keeps the package self-contained.

The renderer emits, in order: a comment header identifying the bridge, the `[build-system]` table pinning `setuptools>=68` and the `setuptools.build_meta` backend, the `[project]` table with `name`, `version`, `requires-python`, and the alphabetised `dependencies` list, the bridge-private `[python]` table with `implementation`, `runtime-mode`, `async-mode`, `free-threaded`, `stubgen-fallback`, and `sidecar-glob`, the `[python.wrappers]` table listing only the wrapper-kind members, and finally `[python.members]` enumerating every member with its kind tag.

The default `Venv` is CPython `>=3.12,<3.15` with `runtime-mode = "embedded"`, `async-mode = "per-call"`, `free-threaded = false`, `stubgen-fallback = true`, and `sidecar-glob = "*_externs.py"`. These match the MEP-71 spec §3 (consume path) defaults and align with the MEP-51 Phase 12 sidecar convention.

The `SkipReason` enum lands all 20 refusal classifications from research note 05 §"The refusal table" plus the runtime / import-time refusals from note 10 §"Import-time side effects": SkipNoComplexType, SkipOpenUnion, SkipParamSpec, SkipTypeVarTuple, SkipForwardRef, SkipUnsupportedTypingConstruct, SkipCFunctionWithoutStubs, SkipOverloadAmbiguity, SkipUntypedPackage, SkipImportTimeNetwork, SkipImportTimeError, SkipPrivateName, SkipDunder, SkipDescriptor, SkipMetaclass, SkipDynamicAttribute, SkipIncompatibleAsyncRuntime, SkipBytearrayMutable, SkipPyodideUnavailable. Each constant's `String()` produces a stable token used in the `SKIPPED.txt` golden fixtures.

The `BridgeError` type carries `(Phase, Package, Cause)` and formats as `phase[package]: cause` (with the bracketed segment omitted when package is empty, matching the MEP-73 BridgeError contract but with `Package` substituted for `Crate`). It implements `Unwrap` so `errors.Is` and `errors.As` traverse through it.

## Files changed

| File | Purpose |
|------|---------|
| `package3/python/build/driver.go` | `Driver`, `Options`, `NewDriver`, `PrepareVenv`, `WriteVenvRoot`, `Cleanup`, `defaultCacheDir` |
| `package3/python/build/venv.go` | `Venv`, `VenvMember`, `VenvMemberKind`, `DefaultVenv`, `AddMember`, `AddSharedDep`, `RenderPyprojectToml`, `Validate` |
| `package3/python/build/driver_test.go` | Driver unit tests (cache-dir resolution, PrepareVenv idempotence, Cleanup semantics) |
| `package3/python/build/venv_test.go` | Venv unit tests (rendering, determinism, validation, free-threaded toggle, wrapper-vs-user split) |
| `package3/python/build/phase00_test.go` | `TestPhase0Skeleton` sentinel with 3 subtests |
| `package3/python/errors/errors.go` | `SkipReason` (20 variants), `SkipReport`, `BridgeError`, `Wrap` |
| `package3/python/errors/errors_test.go` | Errors unit tests (SkipReason exhaustiveness + out-of-range, SkipReport format, BridgeError unwrap + As + chain) |

## Test set

- `TestPhase0Skeleton/end_to_end`
- `TestPhase0Skeleton/package_layout`
- `TestPhase0Skeleton/default_venv_invariants`
- All `package3/python/build/...` and `package3/python/errors/...` unit tests.

## Closeout notes

Phase 0 is the smallest viable skeleton: enough to render the venv root pyproject.toml that later phases will populate with synthesised wrapper modules plus the user's emitted top-level module. The driver's `WorkDir` is allocated with a `mochi-python-` prefix so `Cleanup` can safely refuse to remove a user-provided directory.

The cache-dir resolution honours `$XDG_CACHE_HOME` first, then falls back to `~/.cache/mochi/python-deps/`, then to `$TMPDIR/mochi-cache/python-deps`. This matches the MEP-73 Rust bridge cache and the broader Mochi cache convention.

The Venv's `[python.wrappers]` table is bridge-private (not part of any PEP). It is the surface phase 8 reads to locate the per-wrapper PEP 517 backend metadata and the libpython link manifest. Distinguishing it from `[python.members]` (which lists every member regardless of kind) means a downstream tool can read `[python.wrappers]` directly to enumerate the wrapped PyPI distributions without filtering.

No external runtime dependencies are introduced. The build adds two Go packages to the repo (`package3/python/build/` and `package3/python/errors/`), both pure-Go with stdlib-only imports. The unit tests are pure Go and require no network or CPython runtime.
