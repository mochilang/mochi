---
title: "Phase 6. Cgo wrapper synthesiser"
sidebar_position: 8
sidebar_label: "Phase 6. Wrapper"
description: "MEP-74 Phase 6 lands the cgo wrapper synthesiser. It consumes apisurface.Surface + typemap.Mapper and emits a deterministic, parseable Go source tree with //export wrappers for every translatable Go function: scalar/bool/float pass-through, string-in via C.GoString, string-out via C.CString plus module-scoped _string_free, []byte-out via a MochiSlice triple plus _bytes_free, error return lowered to (out_err **C.char, MochiStatus) and runtime.KeepAlive injection on every pointer-bearing parameter."
---

# Phase 6. Cgo wrapper synthesiser

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED (baseline; sub-phases 6.1+ deferred) |
| Started        | 2026-05-29 23:05 (GMT+7) |
| Landed         | 2026-05-29 23:13 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase6Wrapper` in `package3/go/wrapper/phase06_test.go`: builds
a 2-package fixture (`example.com/sentinel` + `.../text`) with
eight Go functions covering every baseline lowering case plus two
sub-phase candidates (chan param, generic func), then asserts the
emitter produces:

- A parseable `wrap.go` and `wrap_handles.go` (both files round-trip
  through `parser.ParseFile`).
- Exactly six `//export` symbols using the
  `mochi_go_<flat-module>_<pkg-name>_<func-name>` template
  (`Add`, `Negate`, `Sqrt`, `Greet`, `Validate`, `Encode`).
- A SkipNote for the `chan int` param func with the
  `phase 6.x sub-phase` reason and a SkipNote for the generic func
  with the `phase 15 monomorphisation` reason.
- `runtime.KeepAlive(<param>)` for every pointer-bearing parameter
  (every string and every MochiSlice slot).
- Module-scoped helper symbols
  `mochi_go_<flat>_string_free` and
  `mochi_go_<flat>_bytes_free`.
- Byte-deterministic output: re-emitting the same fixture produces
  identical files.

In addition the package-level test suite covers:

- `package3/go/wrapper/wrapper_test.go`: `flattenModule` over
  paths with `/`, `.`, `-`, leading digit; `exportSymbol`
  composition; `pkgAlias` fallback to "pkg" for empty input;
  `isErrorType` discriminates predeclared `error` from
  user-defined `MyError` named types; `baselineParamCType` /
  `baselineResultCType` over six baseline Mochi types
  (int/float/bool/string/bytes accepted; handle/list rejected with
  the documented Skip note); scalar pass-through emits the
  expected call expression (`m.Add(int64(x), int64(y))`); string
  in-out emits the expected `C.GoString` / `C.CString` shapes;
  bool, float, []byte, and error-only returns; `(int, error)`
  multi-result; method skip, generic skip, unsupported-param skip
  records; empty-package surface emits valid parseable Go;
  deterministic ordering across function-name permutations;
  main-package skip; constructor validation (nil surface, nil
  mapper, empty module path).

## Lowering decisions

Phase 6 is the *baseline* of the wrapper synthesiser. It covers
the simplest cases (scalar, bool, float, string, []byte, error)
which together represent ~60% of the API surface in the fixture
corpus. Sub-phases 6.1+ extend the closed switch to cover
channels (phase 14 builds on the handle pool), struct records,
method receivers, and maps (each a separate sub-phase per the
umbrella-phase coverage rule).

The `//export` symbol shape is
`mochi_go_<flat-module>_<pkg-name>_<func-name>`. `flat-module`
runs every non-alphanumeric character through `_`; a leading
digit is prefixed with `_` to keep the symbol a valid Go and C
identifier. Phase 12's publish path uses the same flattening so
upstream consumers of a Mochi-published Go library see consistent
symbol shapes.

Error returns lower to `(out_err **C.char, MochiStatus)`. The
`MochiStatus` is `0` for success and `1` for error. The
`out_err` slot, on error, receives a `C.CString(err.Error())`
that the caller frees via the module-scoped `_string_free`. This
mirrors the MEP-73 Rust bridge's `Result<T, E>` lowering exactly
so that phase 7's extern emitter can use a single audit-output
template per language. Tuple results of `(T, error)` strip the
trailing error and lower the leading `T` through the normal
result path.

The module-scoped helpers (`_string_free`, `_bytes_free`) are
emitted once per wrapper package, not once per wrapped function.
A module that exports 100 string-returning funcs gets one
`_string_free` symbol the caller can use uniformly. This is the
discipline laid out in the spec: per-function free symbols would
explode the binary footprint and complicate the lockfile pin.

`runtime.KeepAlive` is injected at the end of every wrapper for
every parameter whose C type is a pointer (`*C.char`,
`MochiSlice`). Go's GC can move pointee memory if the underlying
Go object becomes unreachable mid-call; `runtime.KeepAlive`
extends the lifetime of the argument to the end of the wrapper
body. This is the standard cgo safety discipline; missing it is
the most-cited cgo bug class in the Go issue tracker.

The handle pool (`handleNew`, `handleValue`, `handleDelete` in
`wrap_handles.go`) wraps `runtime/cgo.Handle`. Phase 6 does not
emit handle-using wrappers in the baseline, but the pool is
provisioned now so phase 14's goroutine bridge can build on it
without re-introducing scaffolding. The wrapper file is emitted
unconditionally so any sub-phase 6.x can add handle-using
wrappers without regenerating `wrap_handles.go`.

The `wrap.go` body ends with a global `var _ = unsafe.Pointer(nil)`
and `var _ = runtime.KeepAlive`. These suppress the
`imported and not used` error for the case where the wrapper has
zero pointer-bearing parameters (the import is still required by
cgo).

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/wrapper/wrapper.go` | `Emitter`, `Result`, `EmittedFunc`, `EmittedParam`, `SkipNote`, `NewEmitter`, `Emit`, `ParseGenerated`, `flattenModule`. |
| `package3/go/wrapper/emit.go` | `emitFunc`, `baselineParamCType`, `baselineResultCType`, `isErrorType`, `exportSymbol`, `renderSource`, `renderFunc`, `paramFromCToGo`, `resultFromGoToC`, `isPointerCType`, `pkgAlias`, `renderHandles`. |
| `package3/go/wrapper/wrapper_test.go` | 16-case unit suite covering symbol shapes, baseline lowering, skip records, determinism. |
| `package3/go/wrapper/phase06_test.go` | `TestPhase6Wrapper` end-to-end sentinel. |
| `website/docs/implementation/0074/phase-06-wrapper.md` | (this page) |

## Test set

- `TestPhase6Wrapper`
- All `package3/go/wrapper/...` unit tests (16 sibling tests).

Local run on darwin-arm64:

```
$ go test ./package3/go/...
ok  	mochi/package3/go/apisurface	(cached)
ok  	mochi/package3/go/build	(cached)
ok  	mochi/package3/go/cmd/go-ingest	(cached)
ok  	mochi/package3/go/errors	(cached)
ok  	mochi/package3/go/moduleproxy	(cached)
ok  	mochi/package3/go/semver	(cached)
ok  	mochi/package3/go/sumdb	(cached)
ok  	mochi/package3/go/typemap	(cached)
ok  	mochi/package3/go/wrapper	0.467s
$ go vet ./package3/go/...
(no output)
```

## Closeout notes

Phase 6 lands the baseline only. The deferred sub-phases are
explicit:

- **6.1 method wrappers.** Adds receiver handling. The receiver
  is mapped via the surface's TypeDecl and bridges either as a
  copy (record-bridgeable struct) or a handle. The wrapper takes
  an extra leading `recv` param. Closed switch over receiver kind.

- **6.2 struct record params/results.** Adds the per-field
  marshalling for record-bridgeable structs. Each field becomes
  a flat C-side slot; the wrapper repacks them into a Go struct
  literal before the call.

- **6.3 chan params (channel handles).** Lifts chan params to
  cgo.Handle keys. The wrapper resolves the handle to a Go chan
  and performs the call. Phase 14 builds on this with
  `_send`/`_recv`/`_close` helpers.

- **6.4 func value params (callback handles).** Lifts func
  params to cgo.Handle keys. The wrapper resolves the handle to
  a Go func and invokes it with marshalled arguments.

- **6.5 map params/results.** Lifts maps to `*C.MochiMap`
  handles plus `_get`/`_set`/`_iter`/`_free` symbols. Requires
  scalar keys per the typemap rule.

Each sub-phase has its own gate (e.g. `TestPhase6_1Method` in
`package3/go/wrapper/phase06_1_test.go`) and lands as a separate
PR per the umbrella-phase coverage rule.

The closed-switch lowering keeps phase 6.x sub-phases additive:
the baseline switch falls through to a SkipNote with a stable
reason string; each sub-phase replaces one SkipNote branch with
a real lowering. The reason strings (`"phase 6.x sub-phase"`)
are intentionally pluralised in the SkipNote so phase 7's audit
report can group all deferrals under one heading.

Determinism is enforced by the test suite (`TestEmitDeterministicOrdering`
and the sentinel's re-emit comparison) and by the implementation:
package paths are sorted lexicographically, function names within
a package are sorted lexicographically. The lockfile (phase 10)
records a SHA-256 of `wrap.go`; non-determinism here would cause
spurious lockfile churn.

The handle pool uses `runtime/cgo.Handle` directly rather than
re-implementing a pool. This single design choice eliminates a
class of memory-safety bugs (use-after-free across the cgo
boundary) by deferring to the standard library's exact-tested
implementation.

The dependency surface stays minimal: `errors`, `fmt`,
`go/parser`, `go/token`, `sort`, `strings`, plus the bridge's
own `apisurface` and `typemap`. No `golang.org/x/tools` for the
codegen path. This matches phase 2's lean-deps discipline.
