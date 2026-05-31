---
title: "Phase 0. Skeleton"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton"
description: "MEP-72 Phase 0 lands package3/typescript/ skeleton: Driver / Workspace types modelling the synthesised package.json + tsconfig.json + import-map, errors package with SkipReport, deterministic import-map renderer, semver helper matching node-semver."
---

# Phase 0. Skeleton

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Started        | (pending) |
| Landed         | (pending) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase0Skeleton` in `package3/typescript/build/phase00_test.go`: subtests `end_to_end`, `package_layout`, `default_workspace_invariants`. The first allocates a Driver, prepares a workspace, adds npm + JSR packages and an import-map override, writes the workspace root `package.json` + `import-map.json` + `tsconfig.json` to a scratch directory, re-reads them, and asserts the expected substrings appear. The second verifies the on-disk layout of `package3/typescript/` (the documented Go packages exist). The third checks that `DefaultWorkspace()` round-trips through `Validate()` without error and renders manifests containing the auto-generated header comment, the `"type": "module"` declaration, the `target = "es2024"` in tsconfig, and reports `node22` as the default runtime.

In addition, the package-level test suite covers:

- `package3/typescript/errors/`: 18 SkipReason variants string-encode round-trip (one constant per refusal case from research note 05 plus internal ones for `any` returns, unresolved conditional types, CJS-only packages on browser target, dual-package hazard divergence, deprecated `@deprecated` JSDoc markers, namespace-only declarations, ambient module wildcards, and a generic SkipUnknown fallback), SkipReport renders with and without an override line, BridgeError formats with and without a package, errors.Is unwraps BridgeError.Cause, the unknown-value fallback returns "SkipUnknown".
- `package3/typescript/build/`: DefaultWorkspace defaults (TS target floor `es2024`, `module = "esnext"`, runtime `node22`, bundler `bun` with esbuild fallback, sourcemaps `true`), AddNpmPackage and AddJsrPackage sort-and-deduplicate, AddImportMapEntry override semantics with nil-map autovivification, RenderPackageJSON + RenderImportMap + RenderTsconfig content + determinism over 10 iterations, dependency-list alphabetisation, empty-workspace short-circuit, Validate rejects malformed semver / duplicate aliases / empty package names / absolute paths in import-map / parent-traversal paths, Driver cache-dir defaults including `$XDG_CACHE_HOME` and `$HOME` fallbacks plus the `$TMPDIR` fallback path, PrepareWorkspace idempotence, Cleanup idempotence and the user-vs-allocated work-dir distinction.
- `package3/typescript/semver/`: node-semver range parsing for `^1.2.3`, `~1.2.3`, `>=1.2.3 <2.0.0`, `1.x`, `*`, `latest`; range satisfaction; SemVer ordering with prerelease segments; the "highest matching version" picker.

## Lowering decisions

Phase 0 ships no `.d.ts` ingest yet, only the scaffolding the later phases depend on. The `Driver` exports `NewDriver(Options) -> *Driver`, `PrepareWorkspace() -> (*Workspace, error)`, `WriteWorkspaceRoot(*Workspace) -> (string, error)`, and `Cleanup() -> error`. `Workspace` exports `DefaultWorkspace()`, `AddNpmPackage`, `AddJsrPackage`, `AddImportMapEntry`, `RenderPackageJSON`, `RenderImportMap`, `RenderTsconfig`, and `Validate`.

The `package.json` renderer emits, in this order: the `"name"` (the synthesised workspace name), `"version"` (always `"0.0.0"` for the synthesised workspace; user-published packages carry their own version via TargetNpmLibrary), `"private": true` (always, the workspace is not publishable), `"type": "module"` (ESM by default), the `"dependencies"` map alphabetised by name, the `"devDependencies"` map (with `typescript` pinned to the version the ingest helper was built against), and the `"engines"` field declaring the runtime target. The renderer is hand-rolled (not via `encoding/json`'s map marshaling) so the output is byte-stable for the workspace-cache key. Phase 8 may switch to a JSON canonicaliser once the wrapper build needs to amend `package.json` in place.

The `import-map.json` renderer emits the `"imports"` map with entries alphabetised by import-specifier; bare specifiers (`"zod"`) come before scoped specifiers (`"@std/encoding"`); JSR specifiers (`"jsr:@std/encoding"`) come after npm specifiers (`"npm:zod"`). The `"scopes"` map is only emitted when at least one scoped override is declared; scope keys are alphabetised within the map; each scope's inner map is alphabetised by specifier. The default workspace declares no scopes (no overrides yet).

The `tsconfig.json` renderer emits a minimal pinned tsconfig: `target = "es2024"`, `module = "esnext"`, `moduleResolution = "bundler"`, `strict = true`, `isolatedModules = true`, `verbatimModuleSyntax = true`, `noEmit = true` (the synthesis-only tsconfig; the library-emit tsconfig under TargetNpmLibrary flips this), and `lib = ["es2024", "DOM"]` (DOM included for the browser-target workspace, omitted for node-only).

The default workspace pins `target = "es2024"` (matching MEP-52's own floor) and selects `runtime = "node22"` as the bridge default (MEP-72 §2 nomenclature). The bundler defaults to `bun build` (fastest of the three; MEP-52 Phase 17 already uses it) with `esbuild` as the fallback when Bun is unavailable. The `sourcemaps = true` default matches MEP-52 Phase 16's reproducible-build gate.

The `SkipReason` enum lands all 11 refusal classifications from research note 05's table plus 7 internal ones (the closed-form table includes `any`-typed returns, unresolved generic parameters, conditional types unresolvable at bind site, mapped types deeper than 2 levels, CJS-only on browser target, dual-package hazard divergence, deprecated declarations, namespace-only declarations, ambient-module wildcards, and a generic `SkipUnknown` zero value). Each constant's `String()` produces a stable token used in the `SKIPPED.txt` golden fixtures. An exhaustive sub-test sweeps every declared constant and asserts a non-`SkipUnknown` result, so a future addition to the enum that forgets to update the switch is caught immediately.

The `BridgeError` type carries `(Phase, Package, Cause)` and formats as `phase[pkg]: cause` (with the bracketed segment omitted when package is empty). It implements `Unwrap` so `errors.Is` and `errors.As` traverse through it. The `Wrap(phase, pkg, cause)` helper returns nil for a nil cause so callers can use it unconditionally from a happy path.

The `semver/` package implements the node-semver range grammar (caret, tilde, hyphen range, `||` union, `x`/`X`/`*` wildcards) plus SemVer ordering with prerelease-segment lexical ordering per SemVer 2.0.0. It does NOT use `golang.org/x/mod/semver` because that package implements Go's module-version semver dialect (which lacks prerelease union ranges). The JSR + npm registries both speak node-semver; the bridge needs the node dialect.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/build/driver.go` | `Driver`, `Options`, `NewDriver`, `PrepareWorkspace`, `WriteWorkspaceRoot`, `Cleanup`, `defaultCacheDir` |
| `package3/typescript/build/workspace.go` | `Workspace`, `NpmDependency`, `JsrDependency`, `ImportMapEntry`, `RuntimeTarget`, `DefaultWorkspace`, `AddNpmPackage`, `AddJsrPackage`, `AddImportMapEntry`, `RenderPackageJSON`, `RenderImportMap`, `RenderTsconfig`, `Validate` |
| `package3/typescript/build/driver_test.go` | Driver unit tests (cache-dir resolution, PrepareWorkspace idempotence, Cleanup semantics) |
| `package3/typescript/build/workspace_test.go` | Workspace unit tests (rendering, determinism, validation, RuntimeTarget mapping) |
| `package3/typescript/build/phase00_test.go` | `TestPhase0Skeleton` sentinel with 3 subtests |
| `package3/typescript/errors/errors.go` | `SkipReason` (18 variants), `SkipReport`, `BridgeError`, `Wrap` |
| `package3/typescript/errors/errors_test.go` | Errors unit tests (SkipReason exhaustiveness, SkipReport format, BridgeError unwrap) |
| `package3/typescript/semver/semver.go` | `Range`, `Version`, `ParseRange`, `ParseVersion`, `Satisfies`, `HighestMatching` |
| `package3/typescript/semver/semver_test.go` | Semver unit tests (range parsing, prerelease ordering, highest-matching picker) |

## Test set

- `TestPhase0Skeleton/end_to_end`
- `TestPhase0Skeleton/package_layout`
- `TestPhase0Skeleton/default_workspace_invariants`
- All `package3/typescript/build/...`, `package3/typescript/errors/...`, `package3/typescript/semver/...` unit tests.

Expected local run on darwin-arm64:

```
$ go test ./package3/typescript/...
ok  	mochi/package3/typescript/build	0.802s
ok  	mochi/package3/typescript/errors	0.483s
ok  	mochi/package3/typescript/semver	0.612s
```

## Closeout notes

Phase 0 is the smallest viable skeleton: enough to render the synthesised workspace root (`package.json` + `import-map.json` + `tsconfig.json`) that later phases populate with consumed packages. The driver's `WorkDir` is allocated with a `mochi-ts-` prefix so `Cleanup` can safely refuse to remove a user-provided directory.

The cache-dir resolution honours `$XDG_CACHE_HOME` first, then falls back to `~/.cache/mochi/ts-deps/`, then to `$TMPDIR/mochi-cache/ts-deps`. This matches the behaviour of `package3/rust/` and `package3/go/`'s phase 0 (which chose `~/.cache/mochi/rust-deps/` and `~/.cache/mochi/go-deps/` respectively).

No external runtime dependencies are introduced. The phase adds three Go packages to the repo (`package3/typescript/build/`, `package3/typescript/errors/`, `package3/typescript/semver/`), all pure-Go with stdlib-only imports. The `golang.org/x/mod/modfile`-equivalent for TypeScript would be a canonical-JSON library; that dependency is deferred until phase 8 actually needs to amend `package.json` in place.

The TypeScript workspace model is structurally simpler than MEP-73's cargo workspace and MEP-74's go workspace: `package.json`'s `dependencies` map is a flat map of name to semver-range; `import-map.json` provides per-specifier overrides; `tsconfig.json` is the type-check configuration. There is no equivalent of cargo's profile sections or go.work's replace directives (the `"resolutions"` field on `package.json` is the closest analog and only npm + Yarn honour it; the bridge avoids it for cross-runtime compatibility).

## Sub-phase decomposition

None at phase 0. The phase is single-shot scaffolding.

## Cross-references

- [MEP-72 spec §1 Pipeline overview](/docs/mep/mep-0072#1-pipeline-overview) — the bridge stages this skeleton scaffolds.
- [Research note 01 §3 Manifest tables](/docs/research/0072/01-language-surface#3-the-manifest-tables) — the manifest the workspace renderer mirrors.
- [Research note 02 §1 No wrapper package](/docs/research/0072/02-design-philosophy#1-no-wrapper-package-on-consume-side) — why the skeleton omits a `wrapper/` package.
- [MEP-74 phase 0 skeleton](/docs/implementation/0074/phase-00-skeleton) — the sister go.work-renderer phase.
- [MEP-73 phase 0 skeleton](/docs/implementation/0073/phase-00-skeleton) — the sister cargo-workspace-renderer phase.
