---
title: "Phase 9. Build orchestration"
sidebar_position: 11
sidebar_label: "Phase 9. Build orchestration"
description: "MEP-74 Phase 9 wires the cgo wrapper synthesiser (phase 6) into a deterministic `go build -buildmode=c-archive` pipeline. Driver.Build assembles a multi-module workspace under the bridge work-dir, writes every wrapper module + synthesised go.mod, renders the go.work, and either runs `go build` per wrapper or short-circuits via a content-addressed artefact cache keyed on every file hash + target tuple. The end-to-end sentinel runs an actual c-archive build on the host (skipped on platforms with no cgo) and exercises the cache hit path by mutating the cached archive and re-running."
---

# Phase 9. Build orchestration

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 23:34 (GMT+7) |
| Landed         | 2026-05-29 23:47 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase9BuildOrchestration` in `package3/go/build/phase09_test.go`
exercises the full pipeline against a handwritten minimal cgo
wrapper (`package main` + one `//export` function returning a
`C.int64_t` sum):

- assembles the workspace topology from the wrapper.Result tree,
- writes the wrapper Go source + synthesised `go.mod`,
- renders `go.work`,
- runs `go build -buildmode=c-archive -trimpath -buildvcs=false
  -ldflags=-s -w` and confirms the `.a` + `.h` pair land on disk,
- tampers with the cached archive bytes and re-runs the orchestrator
  with a fresh work-dir to prove the second run hydrates from cache
  instead of re-invoking `go build`.

The sentinel is skipped on platforms where `c-archive` is not
supported (`js`, `ios`), when `CGO_ENABLED=0`, or when the host
C toolchain is missing from `go env CC`. The skip path is
covered by `cgoArchiveSupported`.

Plus 30+ unit tests under `package3/go/build/build_test.go`:

- plan validation (`TestBuildRejectsEmptyPlan`,
  `TestBuildRejectsWrapperWithoutFiles`, `TestBuildRejectsWrapperWithEmptyName`),
- wrapper module write-out (`TestBuildWritesWrapperFilesAndGoMod`,
  `TestBuildAssemblesWorkspaceGoWork`),
- file-path safety (`TestBuildRejectsEscapingFileName`,
  `TestBuildRejectsAbsoluteFileName`),
- cache-key determinism + sensitivity
  (`TestComputeCacheKeyDeterministic`, `TestComputeCacheKeyChangesOnFileEdit`,
  `TestComputeCacheKeyChangesOnTarget`,
  `TestComputeCacheKeyStableAcrossSlicePermutation`),
- target / env handling
  (`TestTargetStringExplicit`, `TestBuildEnvCarriesTargetTuple`,
  `TestBuildEnvCgoDisabled`, `TestBuildEnvDeterministicAddsFlags`,
  `TestBuildEnvExtraEnvOverrides`),
- cache hydration / population
  (`TestBuildCacheHitShortCircuits`, `TestBuildCacheMissPopulatesOnSkipBuild`,
  `TestCacheArtefactDirShardsByPrefix`),
- helpers (`TestCopyFileWritesContents`, `TestSetEnvOverridesExisting`,
  `TestSynthesisedImportPath`, `TestRenderWrapperGoMod`,
  `TestSortedWrappersStable`).

## Lowering decisions

Phase 9 is the *first* phase that takes the synthesised wrapper
source from phase 6 and produces a binary artefact. Two design
decisions are load-bearing:

**Workspace per build, not per project.** Each `Driver.Build`
call writes a fresh `go.work` under `<work-dir>/go_workspace/` and
materialises every wrapper module under
`go_wrap/<flat-module>/`. This gives the bridge a private
toolchain state per build invocation: a wrapper module's
`go.mod` cannot interfere with the user's top-level `go.mod`, and
two concurrent Mochi builds do not race on the workspace. The
work-dir is allocated under `$TMPDIR/mochi-go-<random>/` by
`PrepareWorkspace` and is removed on `Driver.Cleanup`. The cache
directory lives separately under `$XDG_CACHE_HOME/mochi/go-deps/`
and is preserved.

**Content-addressed artefact cache keyed on inputs.** The cache
key is `sha256(sorted wrapper files + workspace go.work + target
tuple + cgo flag)`. The key is computed *before* the `go build`
invocation, so the cache lookup happens on the structural input
rather than on a post-build digest. A hit copies the cached `.a` +
`.h` directly into the work-dir without invoking `go build`. The
key folds in the rendered `go.work` (not just the modules slice)
so a workspace topology change (a new `replace` directive, a
different GoVersion floor) invalidates the cache deterministically.

The cache is sharded by the first 2 hex chars of the key
(`artefacts/ab/abcdef.../`) to keep any single directory from
overflowing the OS file count limit on the worst-case 10k+
project tree.

**`SkipBuild` is for unit-test scaffolding.** Setting
`BuildPlan.SkipBuild=true` exercises the workspace + cache-key path
without invoking the cgo toolchain. The unit tests in `build_test.go`
use it to keep the suite fast and not depend on the host toolchain.
Production callers always leave `SkipBuild=false`; the sentinel
`TestPhase9BuildOrchestration` does the same and consequently is
the only test that actually invokes `go build`.

**Deterministic flag set.** The bridge always passes `-trimpath`
(strips the build-tree absolute prefix from debug info) and
`-buildvcs=false` (refuses to embed git revision metadata). The
`Deterministic` driver option layers in `GOFLAGS=-trimpath
-buildvcs=false` and `SOURCE_DATE_EPOCH=0` as defence-in-depth so
even toolchain extensions that consult those env vars produce
byte-identical archives across runs. `-ldflags=-s -w` strips the
debug symbol table; the wrapper module is not user-visible so
losing it is acceptable.

**Synthesised import path prefix.** Wrapper modules use the
`mochilang.local/` prefix in their `go.mod` module line. This is
*not* a real proxy.golang.org-reachable path; it is a workspace-
internal name that does not collide with anything the user could
declare in their own `go.mod`. The prefix is hard-coded in
`synthesisedImportPath`. Phase 11's `TargetGoLibrary` emit picks
its own prefix policy for *publish-direction* modules.

**Build-direction does not run `go mod download` separately.**
The wrapper modules' `go.mod` only requires the synthesised local
modules (no external deps), and the workspace `use (...)` block
pulls them in directly. The cgo wrapper is self-contained per phase
6's baseline lowering (no transitive Go imports beyond stdlib + the
source module's exports). When phase 14's goroutine bridge lands,
the wrapper will gain a `runtime/cgo` import which is part of the
Go stdlib and similarly does not need a fetch step.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/build/build.go` | `Driver.Build`, `BuildPlan`, `BuildResult`, `BuildArtefact`, `Target`, cache-key + cache-hit + cache-populate helpers. |
| `package3/go/build/build_test.go` | 30 unit tests over plan validation, wrapper writes, workspace assembly, cache key stability, env handling. |
| `package3/go/build/phase09_test.go` | `TestPhase9BuildOrchestration` end-to-end sentinel (skipped on no-cgo platforms). |
| `website/docs/implementation/0074/phase-09-build.md` | (this page) |

## Test set

- `TestPhase9BuildOrchestration` (host cgo end-to-end)
- 18 unit tests in `build_test.go` (build path)
- 15 existing unit tests in `driver_test.go` + `workspace_test.go` (skeleton, unchanged)

Local run on darwin-arm64:

```
$ go test ./package3/go/build/...
ok      mochi/package3/go/build 8.5s
$ go vet ./package3/go/build/...
(no output)
$ go test ./package3/go/...
ok      mochi/package3/go/apisurface    (cached)
ok      mochi/package3/go/build (cached)
ok      mochi/package3/go/cmd/go-ingest (cached)
ok      mochi/package3/go/emit  (cached)
ok      mochi/package3/go/errors        (cached)
ok      mochi/package3/go/moduleproxy   (cached)
ok      mochi/package3/go/semver        (cached)
ok      mochi/package3/go/sumdb (cached)
ok      mochi/package3/go/typemap       (cached)
ok      mochi/package3/go/wrapper       (cached)
```

## Closeout notes

Phase 9 lands the baseline build pipeline; sub-phases for
cross-compilation matrix coverage (9.1 linux-amd64 / linux-arm64,
9.2 windows-amd64, 9.3 wasm-wasip1 without cgo, 9.4 tinygo
embedded) are deferred. Each sub-phase adds a target column to
the matrix and a CI runner that exercises
`TestPhase9BuildOrchestration` under the cross-toolchain. The
build code itself does not change; the env-passthrough surface is
already plumbed via `BuildPlan.Target` and `BuildPlan.ExtraEnv`.

The `BuildResult.CacheKey` is intentionally exposed so phase 10
can fold it into `mochi.lock`'s `[[go-package]]` table, giving the
lockfile a single source of truth for "this wrapper build
input set hashes to this artefact". Phase 10's `--check` mode
will recompute the key from the lockfile's wrapper SHA-256 + h1:
hash and refuse to proceed on drift.

The phase-9 cache is keyed on input bytes, not on the wrapper's
emitted SHA-256 column from the lockfile, because the cache lives
under the user's home and may legitimately contain entries for
multiple lockfile generations. Phase 10's check is independent of
the phase-9 cache.

Sub-phases reserved:

- **9.1** linux-amd64 + linux-arm64 (cross-cgo via `zig cc` or
  cross-Clang sysroot).
- **9.2** windows-amd64 (cross-cgo via `mingw-w64`).
- **9.3** wasm-wasip1 (cgo off; alternate `-buildmode=archive`
  path).
- **9.4** tinygo embedded subset (separate driver path
  `package3/go/tinygo/` lands in phase 16).
