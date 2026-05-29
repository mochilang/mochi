---
title: "Phase 15. Monomorphisation"
sidebar_position: 17
sidebar_label: "Phase 15. Monomorphisation"
description: "MEP-74 Phase 15 lands the bridge-side monomorphiser: a parser for the `[go.monomorphise]` mochi.toml table, a resolver that matches each spec against an exported generic Func or generic Type method in an ApiSurface, and a renderer that emits one non-generic Go wrapper per instantiation so the rest of the bridge (cgo wrapper synth, Mochi-side extern emit, lockfile pin) can treat each instantiation as a plain export."
---

# Phase 15. Monomorphisation

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED (baseline) |
| Started        | 2026-05-30 00:30 (GMT+7) |
| Landed         | 2026-05-30 00:50 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase15MonomorphiseSentinel` in `package3/go/monomorphise/phase15_test.go` writes a tiny synthetic source module (two generic funcs: `Sort[T any]` and `Pair[K, V any]`) plus a wrapper module that renders one monomorphised instance per spec, and asserts `go build ./...` against the rendered wrapper compiles cleanly. The wrapper module uses a relative `replace` directive so the test stays hermetic (no module-proxy fetch).

`TestPhase15RenderInstanceDeterministic` hashes the output of `RenderInstance` 10 times and asserts the SHA-256 is constant, which is load-bearing for the phase 10 wrapper-sha256 lockfile pin.

Plus 28 unit tests in `monomorphise_test.go`:

- spec parser (`TestParseSpecsHappyPath`, `TestParseSpecsMultipleTypeArgs`, `TestParseSpecsIgnoresBlankAndComment`, `TestParseSpecsRejectsMalformedLine`, `TestParseSpecsRejectsUnknownKey`, `TestParseSpecsRejectsMissingDot`, `TestParseSpecsRejectsEmptyT`),
- spec accessors and validation (`TestSpecAccessors`, `TestSpecAccessorsNoDot`, `TestValidateEmptyOK`, `TestValidateRejectsEmptyTypeArg`),
- resolver (`TestResolveHappyPath`, `TestResolveReportsMissing`, `TestResolveReportsArityMismatch`, `TestResolveSkipsNonGenericFunc`, `TestResolveMatchesGenericTypeMethod`, `TestResolveDeterministicOrder`, `TestResolveNilSetReturnsEmpty`),
- symbol naming (`TestSanitiseSuffix`),
- renderer (`TestRenderInstanceHappyPath`, `TestRenderInstanceSingleResult`, `TestRenderInstanceMultipleResults`, `TestRenderInstanceNoResults`, `TestRenderInstanceUnnamedParam`, `TestRenderInstanceRejectsArityMismatch`, `TestRenderInstanceRejectsMissingModule`),
- type substitution (`TestConcretiseTypePreservesLongerIdents`, `TestConcretiseTypeEmpty`, `TestReplaceIdentBoundaryDoesNotEatPartialMatches`).

## Lowering decisions

The monomorphise package is a leaf module: it imports `package3/go/apisurface` for the surface walk and otherwise depends only on the Go stdlib (`errors`, `fmt`, `sort`, `strings`). It splits into three concerns: a parser for `[go.monomorphise]` table fragments, a resolver that produces fully-typed `Instance` records, and a renderer that emits one wrapper function per instance.

**The manifest is opt-in, not auto-detected.** MEP-74 deliberately leaves automatic monomorphisation (from a Mochi-side `slices.Sort([]int{})` call-site) to a future sub-phase. The v1 surface is explicit: every instantiation the wrapper synthesiser must emit is named in `[go.monomorphise]` in the project's `mochi.toml`. Two reasons: explicit instantiations match how Go users already think about generic bindings (one symbol per `[T1, T2, ...]` combination), and the manifest gives the lockfile a stable per-instance entry to pin a wrapper-sha256 against.

**Item format is `<package-import-path>.<Identifier>`.** Each spec's `item` field is the canonical fully-qualified name. The terminal `.<Ident>` is the generic function's name; methods on generic types use the three-part form `<pkg-path>.<TypeName>.<Method>` (for example, `example.com/data.Stack.Push`). The parser does not impose this split itself: it only checks that there's at least one dot. The resolver builds a lookup table keyed by the full path so the parser doesn't need to know about packages.

**Symbol suffix is identifier-sanitised type args.** `sanitiseSuffix` joins `TypeArgs` with `_` after replacing any non-identifier character (dot, slash, bracket, asterisk, comma) with `_`. So `[]string` becomes `__string`, `*foo.Bar` becomes `_foo_Bar`, and `["string", "int64"]` joins to `string_int64`. The rule is intentionally simple: the suffix is just a per-instance differentiator within the `mochi_<module>_<Ident>_<suffix>` symbol namespace, so collision avoidance dominates readability.

**Type-parameter substitution is identifier-bounded, longest-first.** `concretiseType` walks the type-parameter map and applies each `<TypeParam> -> <TypeArg>` replacement using `replaceIdentBoundary`, which respects identifier boundaries (so substituting `T` in `[]T` works but does not touch `TX` or `Truthy`). When multiple type parameters share a prefix, the longer name is applied first; this keeps the substitution stable even if a future test introduces multi-character names like `T` and `T2` side by side. Go generics in practice use single capital letters, but the rule applies in general.

**Deterministic output, twice.** First, the renderer is byte-deterministic: it builds output via `strings.Builder` with no map iteration, no time-of-day, no random IDs. Second, the resolver sorts its output by `Spec.Item` then `SymbolSuffix` via `sort.SliceStable`, so two manifest orderings that resolve to the same set of instances produce the same wrapper output. Both properties are load-bearing for the phase 10 wrapper-sha256 pin.

**Missing specs return as strings, not errors.** A spec that doesn't match a generic identifier (typo, deleted upstream, never existed) and a spec with wrong arity both return as entries in the `missing` slice rather than aborting `Resolve`. The wrapper synthesiser surfaces these as `SkipReport` entries so a single misspelled item doesn't fail the entire `mochi pkg build`. The resolver only returns a hard error for structurally invalid input (missing dot, empty type args, etc.) that the parser already caught.

**Wrappers are non-generic, `//export`-decorated.** Each instance renders to a single Go function `mochi_<moduleFlatName>_<Ident>_<suffix>(p0, p1, ...) (r0, r1, ...) { return alias.Ident[T1, T2, ...](p0, p1, ...) }` with a `//export` directive. The wrapper's parameters and results have their `TypeParam` names rewritten to the concrete `TypeArg`s. The result is a plain non-generic export that the phase 6 cgo wrapper synthesiser and phase 7 Mochi-side extern emitter can consume without any generic-aware codegen.

**Method receivers are not yet rewritten.** A method on a generic type `Stack[T].Push(v T)` resolves and renders to a free function `mochi_<module>_Push_int64(v int64)`. The receiver dispatch (calling `s.Push(v)` rather than `Push(s, v)`) is a phase 15.1 reservation: it requires the wrapper to allocate or accept a receiver value, which intersects with the phase 14 handle pool. Phase 15 ships the free-function path so single-arg `slices.Sort[int64]` and friends land in v1.

**No constraint-checking.** The resolver matches purely on arity (number of `TypeParam`s vs `TypeArg`s). It does not verify that the supplied `int64` satisfies the generic's `comparable` or `cmp.Ordered` constraint. Go's compile step on the rendered wrapper catches the violation; surfacing a clearer pre-compile diagnostic is phase 15.2.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/monomorphise/monomorphise.go` | `ErrMonomorphise`, `Spec` + `SpecSet`, `ParseSpecs`, `Validate`, accessors, `Instance`, `Resolve`, `RenderInstance`, internal helpers (`sanitiseSuffix`, `concretiseType`, `replaceIdentBoundary`). |
| `package3/go/monomorphise/monomorphise_test.go` | 28 unit tests covering parser, validation, resolver, sanitiser, renderer, and substitution edge cases. |
| `package3/go/monomorphise/phase15_test.go` | `TestPhase15MonomorphiseSentinel` (compiles rendered wrappers against a synthetic two-generic source module via a relative replace directive) plus `TestPhase15RenderInstanceDeterministic` (SHA-256 stability across 10 renders). |
| `website/docs/implementation/0074/phase-15-monomorphise.md` | (this page) |

## Test set

- `TestPhase15MonomorphiseSentinel`
- `TestPhase15RenderInstanceDeterministic`
- 28 unit tests in `monomorphise_test.go`

Local run on darwin-arm64:

```
$ go test ./package3/go/monomorphise/...
ok      mochi/package3/go/monomorphise  0.289s
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
ok      mochi/package3/go/monomorphise  0.173s
ok      mochi/package3/go/publish       (cached)
ok      mochi/package3/go/semver        (cached)
ok      mochi/package3/go/sumdb (cached)
ok      mochi/package3/go/typemap       (cached)
ok      mochi/package3/go/wrapper       (cached)
```

## Closeout notes

Phase 15 lands the bridge-side monomorphiser as a leaf module. The integration into the wrapper-synthesiser (calling `Resolve` + `RenderInstance` once per spec into the per-module wrapper output) is wired into phase 6's deferred sub-phases 6.1+. Phase 15 ships standalone so phase 10 (lockfile) can already pin a stable `wrapper-sha256` for any in-test instance that exercises the renderer.

Future phase 15.x reservations:

- **15.1** Method-receiver rewriting: a generic-type method `Stack[T].Push(v T)` currently renders to a free function. Sub-phase 15.1 will pair with the phase 14 handle pool so the wrapper can either resolve a `*Stack[int64]` from a handle (Mochi-owned receiver) or instantiate a fresh receiver from a constructor wrapper.
- **15.2** Pre-compile constraint check: surface a clearer diagnostic when a `TypeArg` does not satisfy the generic's `cmp.Ordered` / `comparable` / interface constraint, rather than relying on Go's compile-step error.
- **15.3** Auto-monomorphisation: walk Mochi call sites and synthesise a spec when a Mochi-side call binds a concrete type to an unconstrained generic. The opt-in `[go.monomorphise]` manifest stays as the explicit override.
- **15.4** Multi-param positional spec form: today `T = "string, int64"` is the only way to encode multiple type args. Sub-phase 15.4 will accept `T = ["string", "int64"]` (TOML inline array) once the wider `mochi.toml` driver supports inline-array values inside the inline-table fragment.
- **15.5** Wrapper-synth integration (phase 6.1) that drops the rendered instances into the per-module wrapper output and updates the lockfile.

Phase 16 (TinyGo embedded subset) builds on the same renderer: a TinyGo wrapper for a generic stdlib function needs the same monomorphised non-generic export the phase 15 renderer produces.
