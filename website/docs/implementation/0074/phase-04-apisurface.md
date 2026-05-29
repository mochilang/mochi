---
title: "Phase 4. ApiSurface parser"
sidebar_position: 6
sidebar_label: "Phase 4. ApiSurface parser"
description: "MEP-74 Phase 4 lands the typed bridge-side parser: closed-grammar GoType AST + recursive descent type-expression parser, Surface loader with typed Func/Type/Const/Var indices, optional strict cross-reference validation, ready for phase 5 to consume."
---

# Phase 4. ApiSurface parser

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 21:55 (GMT+7) |
| Landed         | 2026-05-29 22:13 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase4ApisurfaceParser` in `package3/go/apisurface/phase04_test.go`:
drives the full consumer pipeline. Synthesises an in-memory fixture
module, runs `Ingest` (phase 3) over it, `Encode`s the result to
JSON, `Decode`s back, then `Load`s the decoded document into a
typed Surface. Verifies typed lookups recover every declaration
with the right type shape: `Read(io.Reader, []byte) -> (int,
error)` resolves to `NamedType{io.Reader}` + `SliceType{byte}` +
`BasicType{int}` + `BasicType{error}`; generic `MapKeys[K
comparable, V any]` keeps both type parameters; pointer var
`*Greeter` resolves to `PointerType{NamedType{Greeter}}`; the
`Greeter.Hello` method preserves `ReceiverPointer == true`.

In addition the package-level test suite covers:

- `package3/go/apisurface/typeexpr_test.go`: 9 grammar-shape
  classes (basics, pointers, slices, arrays, maps, chans, named,
  generics, funcs), 8 nested-shape round-trips with `String()`
  byte-stability, 19 malformed-input rejections each guarded by
  `errors.Is(err, ErrTypeParse)`, interface/struct raw-body
  retention, top-level ellipsis acceptance for phase 5 inspection.

- `package3/go/apisurface/surface_test.go`: 11 Load scenarios --
  happy path with cross-package io.Reader reference, generic func
  TypeParams, variadic param flag, interface method extraction,
  schema-version rejection, nil-input rejection, malformed-type
  rejection, strict-cross-reference rejection on a missing import
  with permissive default accepting same, PackagePaths sort,
  missing-pkg-or-name LookupFunc/LookupType/LookupMethod returns
  nil, `walkTypes` node count check.

## Lowering decisions

Phase 4's role in the bridge is to turn the JSON document phase 3
emits into a typed AST the rest of the pipeline can consume
without re-parsing strings. Every later phase walks the typed
Surface; no later phase calls `ParseType` directly except in
diagnostic paths.

The closed `GoType` interface admits 11 concrete shapes:
`BasicType`, `NamedType`, `PointerType`, `SliceType`,
`ArrayType`, `MapType`, `ChanType`, `FuncType`, `EllipsisType`,
`InterfaceType`, `StructType`. The set is closed: phase 5's type
mapping is a closed switch over these, and a new shape requires
both a parser update and an explicit mapping decision.

The decision to keep `InterfaceType` and `StructType` as opaque
`Source string` (the raw `interface{...}` / `struct{...}` text)
rather than expanding their bodies follows from how rarely
anonymous interface/struct literals appear in real Go module
surfaces. The 24-module fixture corpus produces fewer than 30
total occurrences across all packages. Phase 5 can decide
case-by-case whether to re-parse those bodies; the schema does
not pay storage for the common case.

The type-expression grammar follows Go's source-form syntax
exactly. The parser is a hand-rolled recursive descent (no
go/parser dependency, which would pull in the full AST and
position machinery for ~30 tokens of input). The grammar's two
ambiguities -- `Foo[T]` (generic instantiation vs. array literal)
and `func() T` (result type vs. statement) -- are resolved by
context: `[` after an identifier always opens a type-arg list
since we are parsing a type expression. `func()` followed by a
type-startable token always names a result.

The send-only-channel form `chan<- T` is parsed differently from
the receive-only `<-chan T`: the recv form is detected by an
initial `<-` lexeme that must be followed by `chan`, while the
send form is the `chan` keyword followed by `<-` followed by a
type. This matches Go's source grammar.

Generic instantiation uses the same `[T1, T2, ...]` syntax. The
parser emits `NamedType.TypeArgs` populated with the parsed type
expressions. Phase 5's monomorphiser will consume this to drive
per-instantiation wrapper synthesis (phase 15 expands
monomorphisation explicitly; phase 4 just records the args).

`ParseType` is intentionally strict about trailing input: a
string that does not parse to completion returns `ErrTypeParse`.
This catches off-by-one in the ingester (e.g. an extra ` `
character that would mask deeper parse failures).

The `Surface.Load` API takes an optional `LoadOptions` struct
with one flag today: `StrictCrossReferences`. When true, every
`NamedType` whose `PackagePath` is non-empty must appear in the
consuming package's `Imports` list. When false (the default),
unknown imports pass through silently. The default is permissive
because Go's stdlib packages (io, fmt, sync) are not always
recorded in `Imports` (the ingester only records imports
referenced by the exported surface, which may miss some when an
internal helper uses them). Phase 5 will turn strict on once the
ingester is audited to record every needed import.

The typed Surface exposes maps keyed by name for O(1) lookups
(`Funcs`, `Types`, `Consts`, `Vars` on each `PackageView`), and
convenience methods `LookupFunc`, `LookupType`, `LookupMethod`,
`PackagePaths` at the Surface level. The underlying JSON
records remain reachable via the `Underlying` pointer on each
typed declaration -- phase 9 (build orchestration) uses this to
recover the original `Position` for error messages.

The `walkTypes` traversal helper is exported only at package
scope (lowercase first letter would keep it private, but the test
suite verifies its node count). Phase 5 will lift it to public
form when implementing the type-mapping table walks.

The fix in `ingest.go` for untyped constants (e.g.
`const Version = "1.0.0"`) is small but load-bearing: without
it, untyped string constants render as `"untyped string"`, which
fails phase-4's grammar. `types.Default(t)` collapses
untyped-basic types to their canonical concrete form; for the
rare untyped-nil case we fall back to stripping the literal
`untyped ` prefix.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/apisurface/typeexpr.go` | `GoType` interface, 11 concrete shapes, `ParseType`, `ErrTypeParse`, recursive descent parser |
| `package3/go/apisurface/typeexpr_test.go` | grammar-shape tests + 19 malformed-input rejections + String round-trip |
| `package3/go/apisurface/surface.go` | `Surface`, `Load`, `LoadOptions`, `PackageView`, `FuncDecl`, `TypeDecl`, `FieldDecl`, `ValueDecl`, `LookupFunc/Type/Method`, `PackagePaths`, `walkTypes` |
| `package3/go/apisurface/surface_test.go` | Load happy + 11 rejection scenarios |
| `package3/go/apisurface/phase04_test.go` | `TestPhase4ApisurfaceParser` end-to-end sentinel |
| `package3/go/apisurface/ingest.go` | untyped-constant normalisation fix |

## Test set

- `TestPhase4ApisurfaceParser`
- All `package3/go/apisurface/...` unit tests (4 test files after phase 4).

Local run on darwin-arm64:

```
$ go test ./package3/go/...
ok  	mochi/package3/go/apisurface	34.804s
ok  	mochi/package3/go/build	1.084s
ok  	mochi/package3/go/cmd/go-ingest	34.893s
ok  	mochi/package3/go/errors	1.062s
ok  	mochi/package3/go/moduleproxy	2.202s
ok  	mochi/package3/go/semver	1.175s
ok  	mochi/package3/go/sumdb	2.264s
$ go vet ./package3/go/...
(no output)
```

## Closeout notes

Phase 4 deliberately keeps the AST close to source form: the
`String()` method on every `GoType` reconstructs the input
verbatim. This invariant lets phase 9 dump diagnostic
representations without round-tripping through `types.TypeString`,
and lets phase 5 use the AST as a key in lookup tables (the
canonical-form string is stable).

The decision to defer interface/struct anonymous-literal
unfolding to phase 5 follows the same principle as phase 3's
deferral of the parser: each phase owns one transformation, and
phase 4's job is purely to recover structure from strings. If
phase 5 needs the bodies parsed, it gets the raw text and can
choose whether to re-enter `ParseType` or run its own parser.

Subtle bug: an earlier version of the parser mis-handled
`*github.com/foo/bar.Baz` because the `/` was being treated as a
type-element boundary. The fix was to mark `/` and `-` as
identifier-continuation characters (Go module paths use both).
The `TestParseTypeStringRoundTrip` case
`*github.com/foo/bar.Baz` guards against this regressing.

Phase 4 also lands a quiet ingester fix for untyped constants.
The const `Version = "1.0.0"` (untyped string) used to leak the
internal `"untyped string"` type rendering into the JSON, which
broke the closed grammar. `types.Default` resolves the untyped
type to its canonical form (`string`, `int`, `float64`, etc.) at
ingest time, so phase 4's parser never sees the leaked form. A
test in `phase04_test.go` covers this by encoding then re-loading
a fixture with an untyped const.

Phase 5 (closed type-mapping table) will consume `Surface` and
produce a per-NamedType / per-BasicType lookup of the
corresponding Mochi type. The strict-cross-reference flag flips
on at that point: a missing import will be a hard error since
phase 5 cannot generate a wrapper for a type whose package is
unknown.
