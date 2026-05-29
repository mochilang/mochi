---
title: "Phase 0. Skeleton"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton"
description: "MEP-74 Phase 0 lands package3/go/ skeleton: Driver / Workspace types modelling the synthesised go.work, errors package with SkipReport, deterministic go.work renderer."
---

# Phase 0. Skeleton

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 20:55 (GMT+7) |
| Landed         | 2026-05-29 21:07 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase0Skeleton` in `package3/go/build/phase00_test.go`: subtests `end_to_end`, `package_layout`, `default_workspace_invariants`. The first allocates a Driver, prepares a workspace, adds modules and a replace directive, writes the workspace root go.work to a scratch directory, re-reads it, and asserts the expected substrings appear. The second verifies the on-disk layout of `package3/go/` (the documented Go packages exist). The third checks that `DefaultWorkspace()` round-trips through `Validate()` without error and renders a manifest containing the auto-generated header, the `go 1.21` directive, and reports `c-archive` as the default cgo build mode.

In addition, the package-level test suite covers:

- `package3/go/errors/`: 20 SkipReason variants string-encode round-trip (one constant per refusal case from research note 05 plus four bridge-internal ones for deprecated items, replace-driven publishes, var/const items, and unsupported double pointers), SkipReport renders with and without an override line, BridgeError formats with and without a module, errors.Is unwraps BridgeError.Cause, the unknown-value fallback returns "SkipUnknown".
- `package3/go/build/`: DefaultWorkspace defaults (go-version floor 1.21, CgoArchive build mode, trimpath enabled, ldflags `-s -w`), AddModule sort-and-deduplicate, AddReplace replace semantics with nil-map autovivification, RenderGoWork content + determinism over 10 iterations, replace-directive alphabetisation, empty-workspace short-circuit, Validate rejects malformed go-version / duplicate paths / empty module fields / absolute paths / parent-traversal paths, Driver cache-dir defaults including XDG_CACHE_HOME and HOME fallbacks plus the TMPDIR fallback path, PrepareWorkspace idempotence, Cleanup idempotence and the user-vs-allocated work-dir distinction.

## Lowering decisions

Phase 0 ships no go/packages ingest yet, only the scaffolding the later phases depend on. The `Driver` exports `NewDriver(Options) -> *Driver`, `PrepareWorkspace() -> (*Workspace, error)`, `WriteWorkspaceRoot(*Workspace) -> (string, error)`, and `Cleanup() -> error`. `Workspace` exports `DefaultWorkspace()`, `AddModule`, `AddReplace`, `RenderGoWork`, and `Validate`.

The go.work renderer is a small hand-rolled writer. It uses no external library because (1) the schema is fixed and small, (2) the output must be byte-stable for the workspace-cache key (planned for phase 9), and (3) avoiding `golang.org/x/mod/modfile` keeps the package self-contained at phase 0. Phase 9 may switch to modfile once it joins the bridge's dependency set.

The renderer emits, in order: a comment header identifying the bridge, the `go <version>` directive, the `use (...)` block with paths sorted alphabetically, and one `replace <module> => <target>` line per declared directive (also alphabetised). The trailing block separator between `use` and `replace` is suppressed when one or the other is empty.

The default workspace pins `go 1.21` as the toolchain floor (matching MEP-54's own floor) and selects `CgoArchive` as the build mode, `-trimpath` as the reproducibility flag, and `-s -w` as the default ldflags. The TinyGo embedded path (phase 16) and the wasm path (phase 17) will switch to `CgoNone` and select alternative compile paths.

The `SkipReason` enum lands all 11 refusal classifications from research note 05's table plus 9 internal ones (the closed-form table includes complex64, pointer-to-pointer, func-returning-func, complex variadics, const items, var items, deprecated markers, replace-driven publishes, and a generic SkipUnknown zero value). Each constant's `String()` produces a stable token used in the `SKIPPED.txt` golden fixtures. An exhaustive sub-test sweeps every declared constant and asserts a non-`SkipUnknown` result, so a future addition to the enum that forgets to update the switch is caught immediately.

The `BridgeError` type carries `(Phase, Module, Cause)` and formats as `phase[module]: cause` (with the bracketed segment omitted when module is empty). It implements `Unwrap` so `errors.Is` and `errors.As` traverse through it. The `Wrap(phase, module, cause)` helper returns nil for a nil cause so callers can use it unconditionally from a happy path.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/build/driver.go` | `Driver`, `Options`, `NewDriver`, `PrepareWorkspace`, `WriteWorkspaceRoot`, `Cleanup`, `defaultCacheDir` |
| `package3/go/build/workspace.go` | `Workspace`, `WorkspaceModule`, `WorkspaceModuleKind`, `CgoBuildMode`, `DefaultWorkspace`, `AddModule`, `AddReplace`, `RenderGoWork`, `Validate` |
| `package3/go/build/driver_test.go` | Driver unit tests (cache-dir resolution, PrepareWorkspace idempotence, Cleanup semantics) |
| `package3/go/build/workspace_test.go` | Workspace unit tests (rendering, determinism, validation, CgoBuildMode mapping) |
| `package3/go/build/phase00_test.go` | `TestPhase0Skeleton` sentinel with 3 subtests |
| `package3/go/errors/errors.go` | `SkipReason` (20 variants), `SkipReport`, `BridgeError`, `Wrap` |
| `package3/go/errors/errors_test.go` | Errors unit tests (SkipReason exhaustiveness, SkipReport format, BridgeError unwrap) |

## Test set

- `TestPhase0Skeleton/end_to_end`
- `TestPhase0Skeleton/package_layout`
- `TestPhase0Skeleton/default_workspace_invariants`
- All `package3/go/build/...` and `package3/go/errors/...` unit tests.

Local run on darwin-arm64:

```
$ go test ./package3/go/...
ok  	mochi/package3/go/build	0.802s
ok  	mochi/package3/go/errors	0.483s
```

## Closeout notes

Phase 0 is the smallest viable skeleton: enough to render the workspace root go.work that later phases will populate with synthesised wrapper modules plus the user's emitted top-level module. The driver's `WorkDir` is allocated with a `mochi-go-` prefix so `Cleanup` can safely refuse to remove a user-provided directory.

The cache-dir resolution honours `$XDG_CACHE_HOME` first, then falls back to `~/.cache/mochi/go-deps/`, then to `$TMPDIR/mochi-cache/go-deps`. This matches the behaviour of `package3/rust/`'s phase 0 (the parallel MEP-73 phase 0 chose `~/.cache/mochi/rust-deps/`).

No external runtime dependencies are introduced. The phase adds two Go packages to the repo (`package3/go/build/` and `package3/go/errors/`), both pure-Go with stdlib-only imports. The `golang.org/x/mod/modfile` dependency is deferred to phase 9 when the wrapper build actually needs to parse / amend go.mod files.

The Go-side workspace model is simpler than MEP-73's cargo workspace: `go.work` is a flat list of `use` directives plus `replace` overrides, whereas cargo workspace carries shared dependencies, profile sections, and resolver version. The simplicity propagates downstream: the bridge does not need to manage workspace-level dependency inheritance because Go modules already carry their own `go.mod` requires.
