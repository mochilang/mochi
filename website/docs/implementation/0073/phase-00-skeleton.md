---
title: "Phase 0. Skeleton"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton"
description: "MEP-73 Phase 0 lands package3/rust/ skeleton: Driver / Workspace types, errors package with SkipReport, deterministic Cargo.toml workspace-root renderer."
---

# Phase 0. Skeleton

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-73 §Phases](/docs/mep/mep-0073#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 20:30 (GMT+7) |
| Landed         | 2026-05-29 20:46 (GMT+7) |
| Tracking issue | [#22736](https://github.com/mochilang/mochi/issues/22736) |
| Tracking PR    | [#22737](https://github.com/mochilang/mochi/pull/22737) |
| Commit         | [2dc3b34f](https://github.com/mochilang/mochi/commit/2dc3b34f) |

## Gate

`TestPhase0Skeleton` in `package3/rust/build/phase00_test.go`: subtests `end_to_end`, `package_layout`, `default_workspace_invariants`. The first allocates a Driver, prepares a workspace, adds members and shared deps, writes the workspace root Cargo.toml to a scratch directory, re-reads it, and asserts the expected substrings appear. The second verifies the on-disk layout of `package3/rust/` (the documented Go packages exist). The third checks that `DefaultWorkspace()` round-trips through `Validate()` without error and renders a manifest containing the auto-generated header and both release + dev profile sections.

In addition, the package-level test suite covers:

- `package3/rust/errors/`: 20 SkipReason variants string-encode round-trip, SkipReport renders with and without an override line, BridgeError formats with and without a crate, errors.Is unwraps BridgeError.Cause.
- `package3/rust/build/`: DefaultWorkspace defaults, AddMember sort-and-deduplicate, AddSharedDep replace semantics, RenderRootCargoToml content + determinism over 10 iterations, lto / opt-level variant rendering, Validate rejects unsupported resolver / duplicate paths / empty member fields / built-in profile inheritance, Driver cache-dir defaults including XDG_CACHE_HOME and HOME fallbacks, PrepareWorkspace idempotence, Cleanup idempotence + user-vs-allocated work-dir distinction.

## Lowering decisions

Phase 0 ships no rustdoc ingest yet, only the scaffolding the later phases depend on. The `Driver` exports `NewDriver(Options) -> *Driver`, `PrepareWorkspace() -> (*Workspace, error)`, `WriteWorkspaceRoot(*Workspace) -> (string, error)`, and `Cleanup() -> error`. `Workspace` exports `DefaultWorkspace()`, `AddMember`, `AddSharedDep`, `RenderRootCargoToml`, and `Validate`.

The Cargo.toml workspace-root renderer is a small hand-rolled TOML writer. It uses no external library because (1) the schema is fixed and small, (2) the output must be byte-stable for the workspace-cache key (planned for phase 7), and (3) avoiding burntsushi/toml or pelletier/go-toml keeps the package self-contained.

The renderer emits, in order: a comment header identifying the bridge, the `[workspace]` section with `resolver = "2"` and the sorted `members = [...]` list, the `[workspace.package]` section with edition + rust-version, the `[workspace.dependencies]` section with alphabetised crate names, and one `[profile.<name>]` section per declared profile. The lto field has three valid forms (`"off"` renders as `lto = false`, `"fat"` as `lto = true`, `"thin"` as a quoted string) per cargo's accepted syntax. The opt-level field renders as a bare integer for `"0"`-`"3"` and as a quoted string for `"z"` / `"s"`.

The default profiles are release (`opt-level=3`, `lto="fat"`, `panic="abort"`, `strip="symbols"`, `codegen-units=1`, `debug=false`) and dev (`opt-level=0`, `panic="abort"`, `debug=true`). These match the wrapper-crate recommendations in research note 09 §"Symbol visibility".

The `SkipReason` enum lands all 20 refusal classifications from research note 05. Each constant's `String()` produces a stable token used in the `SKIPPED.txt` golden fixtures. An exhaustive sub-test sweeps every declared constant and asserts a non-`SkipUnknown` result, so a future addition to the enum that forgets to update the switch is caught immediately.

The `BridgeError` type carries `(Phase, Crate, Cause)` and formats as `phase[crate]: cause` (with the bracketed segment omitted when crate is empty). It implements `Unwrap` so `errors.Is` and `errors.As` traverse through it.

## Files changed

| File | Purpose |
|------|---------|
| `package3/rust/build/driver.go` | `Driver`, `Options`, `NewDriver`, `PrepareWorkspace`, `WriteWorkspaceRoot`, `Cleanup`, `defaultCacheDir` |
| `package3/rust/build/workspace.go` | `Workspace`, `WorkspaceMember`, `WorkspaceMemberKind`, `WorkspaceProfile`, `DefaultWorkspace`, `AddMember`, `AddSharedDep`, `RenderRootCargoToml`, `Validate` |
| `package3/rust/build/driver_test.go` | Driver unit tests (cache-dir resolution, PrepareWorkspace idempotence, Cleanup semantics) |
| `package3/rust/build/workspace_test.go` | Workspace unit tests (rendering, determinism, validation, lto / opt-level variants) |
| `package3/rust/build/phase00_test.go` | `TestPhase0Skeleton` sentinel with 3 subtests |
| `package3/rust/errors/errors.go` | `SkipReason` (20 variants), `SkipReport`, `BridgeError`, `Wrap` |
| `package3/rust/errors/errors_test.go` | Errors unit tests (SkipReason exhaustiveness, SkipReport format, BridgeError unwrap) |

## Test set

- `TestPhase0Skeleton/end_to_end`
- `TestPhase0Skeleton/package_layout`
- `TestPhase0Skeleton/default_workspace_invariants`
- All `package3/rust/build/...` and `package3/rust/errors/...` unit tests.

## Closeout notes

Phase 0 is the smallest viable skeleton: enough to render the workspace root Cargo.toml that later phases will populate with synthesised wrapper crates plus the user's emitted top-level crate. The driver's `WorkDir` is allocated with a `mochi-rust-` prefix so `Cleanup` can safely refuse to remove a user-provided directory.

The cache-dir resolution honours `$XDG_CACHE_HOME` first, then falls back to `~/.cache/mochi/rust-deps/`, then to `$TMPDIR/mochi-cache/rust-deps`. This matches the behaviour of other Mochi caches.

No external runtime dependencies are introduced. The build adds two Go packages to the repo (`package3/rust/build/` and `package3/rust/errors/`), both pure-Go with stdlib-only imports.
