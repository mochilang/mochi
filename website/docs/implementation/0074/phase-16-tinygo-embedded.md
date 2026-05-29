---
title: "Phase 16. TinyGo embedded subset"
sidebar_position: 18
sidebar_label: "Phase 16. TinyGo embedded subset"
description: "MEP-74 Phase 16 lands the `profile = \"embedded\"` opt-in: a closed banned-import + banned-type set that gates which Go surfaces TinyGo can compile, plus a `//go:linkname` wrapper renderer that binds wrapper symbols to source-module functions without cgo for the wasm-js / wasi-libc / baremetal targets where the regular `//export` c-archive path is unavailable."
---

# Phase 16. TinyGo embedded subset

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED (baseline) |
| Started        | 2026-05-30 00:43 (GMT+7) |
| Landed         | 2026-05-30 00:55 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase16TinygoSentinel` in `package3/go/tinygo/phase16_test.go` renders a `//go:linkname`-decorated wrapper file against a synthetic source module (two functions: `Double(int64) int64` and `Touch(int64)`) and asserts `go build ./...` accepts the file. The regular Go toolchain is used as the strictest available linkname-syntax proxy on CI (TinyGo is not assumed installed).

`TestPhase16NoLinknameBuildTagFlip` exercises the opt-out path: under `-tags=mochi_no_linkname` the rendered file disappears (the `//go:build !mochi_no_linkname` header strips it), so a downstream `go vet` over the wrapper tree that doesn't want the directive still builds cleanly.

`TestPhase16RenderFileDeterministic` hashes the rendered output 10 times and asserts SHA-256 constancy, load-bearing for the phase 10 wrapper-sha256 pin.

`TestPhase16EmbeddedSubsetCompatibleSurface` walks a hand-curated "good" surface (only allowed imports, only allowed types) and asserts the embedded profile reports zero violations.

`TestPhase16EmbeddedSubsetRejectsBadSurface` walks a hand-curated "bad" surface (one banned import + one banned-type result) and asserts both violations surface with the right kinds.

Plus 21 unit tests in `tinygo_test.go`:

- profile descriptor (`TestProfileIsValid`),
- subset walker (`TestCheckPackageStandardIsNoop`, `TestCheckPackageDetectsBannedImports`, `TestCheckPackageDetectsBannedTypeInParam`, `TestCheckPackageDetectsBannedTypeInResult`, `TestCheckPackageStripWrappersHandlesSliceMapPointer`, `TestCheckPackageMethodViolation`, `TestCheckPackageInterfaceMethodViolation`, `TestCheckPackageSortStable`),
- compatibility predicate (`TestIsCompatible`),
- spec validation (`TestLinknameSpecValidate`),
- single-spec renderer (`TestRenderLinknameContainsCanonicalParts`, `TestRenderLinknameZeroResult`, `TestRenderLinknameMultiResult`, `TestRenderLinknameVariadic`, `TestRenderLinknameUnnamedParam`, `TestRenderLinknameRejectsInvalidSpec`),
- file renderer (`TestRenderFileContainsBuildTagAndImport`, `TestRenderFileRejectsEmptyPkgName`, `TestRenderFileBubblesSpecError`),
- banned-list invariants (`TestBannedImportListIsSorted`).

## Lowering decisions

The tinygo package is a leaf module: it imports `package3/go/apisurface` for the surface walk and otherwise depends only on the Go stdlib (`errors`, `fmt`, `sort`, `strings`). It splits into three concerns: a profile descriptor + banned-set tables, a `CheckPackage` walker that produces deterministic `Violation` records, and a renderer that emits one `//go:linkname` wrapper per spec.

**The embedded subset is a banned-set, not an allow-list.** The TinyGo team's compatibility matrix (as of TinyGo 0.30, 2026-Q1) names a closed set of stdlib packages that don't compile under any TinyGo target: reflection, runtime/debug, plugin, net/http, net/rpc, the debug/* family, encoding/gob, go/ast and friends, runtime/cgo, runtime/pprof, runtime/trace, syscall/js, text/template, unsafe. Phase 16 encodes that list as `BannedImports`. The default is allow: any stdlib (or third-party) import not in the banned set is considered embedded-compatible. The reason for banned-set rather than allow-list: TinyGo's compatible surface is large (most of the stdlib that doesn't touch reflection or the network is fine), so an allow-list would require keeping a much-longer mirror of the upstream stdlib.

**The banned-type set is checked under wrapper-peeling.** A function param `[]reflect.Value` or `map[string]*runtime/cgo.Handle` is just as poisonous as the bare `reflect.Value`. `stripWrappers` peels `*`, `[]`, `[N]`, `...`, and `map[K]V` (the value half is the interesting part; map keys can't be reflect-backed in practice). The check is intentionally textual: the apisurface stores types as strings, and a deeper structural walk via `apisurface.ParseType` would require teaching the parser about every Go type form the embedded subset cares about, which is out of scope for the gate.

**Violations are deterministically sorted.** `CheckPackage` sorts violations by `(Kind, Where)` via `sort.SliceStable` so the wrapper-synthesiser can fold them into the SkipReport with byte-stable output (which the phase 10 `wrapper-sha256` lockfile pin requires). The "import" violations sort lexicographically before "param-type" and "result-type" (the kinds are themselves chosen to alphabetise meaningfully).

**The renderer uses `//go:linkname`, not `//export`.** TinyGo on wasm-js, wasi-libc, and baremetal targets has no working cgo; the regular phase 6 `//export` c-archive path requires cgo's runtime. `//go:linkname` is a compiler directive that aliases a local Go symbol to an arbitrary external symbol at link time — the wrapper has no body, the linker resolves both names to the same address. It works under both stock Go (with the `unsafe` import for the directive permission) and TinyGo. The trade-off: linkname'd symbols are not subject to the regular cgo-export contract, so the caller is responsible for matching the source signature exactly. The renderer's job is to make that signature byte-stable.

**The rendered file is `//go:build !mochi_no_linkname`-gated.** A downstream `go vet` run that doesn't want to follow the linkname directive (linkname-chasing has been a frequent source of vet false positives in recent Go releases) can pass `-tags=mochi_no_linkname` to strip the file. The build tag is negative-form so the default (no tags) keeps the file visible.

**The wrapper has no body.** A `//go:linkname` directive on a Go function declaration without a body is a hard contract: the linker MUST resolve the name to the target, or the build fails. This is intentional: a phantom body would create a real Go function at the local name and the linkname directive would attempt to overlay both definitions, producing a redeclaration error. Body-less linkname is the canonical TinyGo recipe.

**`import _ "unsafe"` is the price of admission.** Go enforces that `//go:linkname` only works in a file that imports `unsafe` (even if the rest of the file doesn't use unsafe). The renderer hardcodes the import; callers that consume the rendered file do not need to add their own.

**No `//go:wasmexport` emission yet.** The wasm-js target additionally needs a `//go:wasmexport <symbol>` directive to make the wrapper visible to the host JavaScript environment. That's phase 16.1: it requires per-target switching in the renderer (the standard linkname directive is universal, the wasmexport directive is wasm-js-specific). Phase 16 ships the universal baseline so the in-process gate stays target-agnostic.

**The check is opt-in via profile, not always-on.** `CheckPackage(ProfileStandard, _)` returns nil. The wrapper-synthesiser only invokes the check when the user's `mochi.toml` declares `profile = "embedded"` for a given import. The default cgo path stays the canonical baseline.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/tinygo/tinygo.go` | `ErrTinygo`, `Profile` + `IsValid`, `BannedImports`, `BannedTypePrefixes`, `Violation` + `String`, `CheckPackage`, `IsCompatible`, `LinknameSpec` + `Validate`, `RenderLinkname`, `RenderFile`. |
| `package3/go/tinygo/tinygo_test.go` | 21 unit tests covering profile descriptor, walker, predicates, validator, single-spec renderer, file renderer, and banned-list invariants. |
| `package3/go/tinygo/phase16_test.go` | `TestPhase16TinygoSentinel` (rendered file compiles via `go build` against a synthetic source module), `TestPhase16NoLinknameBuildTagFlip` (opt-out tag strips file cleanly), `TestPhase16RenderFileDeterministic` (SHA-256 stability across 10 renders), plus `TestPhase16EmbeddedSubsetCompatibleSurface` / `TestPhase16EmbeddedSubsetRejectsBadSurface` covering both directions of the gate. |
| `website/docs/implementation/0074/phase-16-tinygo-embedded.md` | (this page) |

## Test set

- `TestPhase16TinygoSentinel`
- `TestPhase16EmbeddedSubsetCompatibleSurface`
- `TestPhase16EmbeddedSubsetRejectsBadSurface`
- `TestPhase16RenderFileDeterministic`
- `TestPhase16NoLinknameBuildTagFlip`
- 21 unit tests in `tinygo_test.go`

Local run on darwin-arm64:

```
$ go test ./package3/go/tinygo/...
ok      mochi/package3/go/tinygo        0.521s
$ go test ./package3/go/...
ok      mochi/package3/go/apisurface    (cached)
ok      mochi/package3/go/build (cached)
ok      mochi/package3/go/cmd/go-ingest (cached)
ok      mochi/package3/go/cosign        (cached)
ok      mochi/package3/go/emit  (cached)
ok      mochi/package3/go/errors        (cached)
ok      mochi/package3/go/goroutine     (cached)
ok      mochi/package3/go/library       (cached)
ok      mochi/package3/go/lockfile      (cached)
ok      mochi/package3/go/moduleproxy   (cached)
ok      mochi/package3/go/monomorphise  (cached)
ok      mochi/package3/go/publish       (cached)
ok      mochi/package3/go/semver        (cached)
ok      mochi/package3/go/sumdb (cached)
ok      mochi/package3/go/tinygo        0.257s
ok      mochi/package3/go/typemap       (cached)
ok      mochi/package3/go/wrapper       (cached)
```

## Closeout notes

Phase 16 lands the TinyGo embedded-subset gate plus the `//go:linkname` wrapper renderer as a leaf module. Wrapper-synthesiser integration (calling `CheckPackage` + `RenderFile` once per embedded-profile import into the per-module wrapper output) is wired into phase 6's deferred sub-phases 6.1+.

Future phase 16.x reservations:

- **16.1** Per-target directive switching: the wasm-js target additionally needs `//go:wasmexport` next to `//go:linkname` to make the symbol visible to the host JavaScript runtime. Sub-phase 16.1 adds a `Target` field to `LinknameSpec` and emits the right directive set per target.
- **16.2** Live TinyGo gate: replace the `go build` proxy in the sentinel with a real `tinygo build -target=wasm` invocation when TinyGo is installed; skip the gate on CI runners without TinyGo rather than fall back to the proxy.
- **16.3** Per-symbol opt-in (`mochi.toml` `[go.linkname.<symbol>]` table) for cases where the user wants the linkname path on a per-export basis (e.g., one fast-path symbol in an otherwise cgo wrapper).
- **16.4** Embedded-subset profile for third-party imports: the current banned set is stdlib-only; phase 16.4 lets users declare per-module bans for third-party packages that pull in disallowed stdlib transitively.
- **16.5** Phase 6.1 wrapper-synth integration that ties `CheckPackage` + `RenderFile` into the per-module wrapper output and updates the lockfile.

Phase 17 (vanity-import + WASI publish) is the final remaining phase; it consumes the same surface walk machinery and adds the publish-direction gate for wasm-wasip1 / wasm-js targets.
