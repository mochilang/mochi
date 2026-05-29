---
title: "Phase 7. Mochi extern emitter"
sidebar_position: 9
sidebar_label: "Phase 7. Extern emit"
description: "MEP-74 Phase 7 lands the Mochi extern fn emitter. It consumes wrapper.Result and produces a deterministic Mochi source file with one `extern fun <alias>.<Name>(...) : <T>` declaration per //export wrapper. Error-bearing wrappers wrap their success type in MEP-13's Result<T, string>; unit returns drop the `:` clause; every line carries the matching C symbol as a trailing comment for phase 10's lockfile audit."
---

# Phase 7. Mochi extern emitter

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED (baseline; sub-phases 7.1+ deferred) |
| Started        | 2026-05-29 23:18 (GMT+7) |
| Landed         | 2026-05-29 23:23 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase7ExternEmitter` in `package3/go/emit/phase07_test.go`
drives the full apisurface -> typemap -> wrapper -> emit pipeline
with a 2-package fixture (`example.com/sentinel` +
`example.com/sentinel/text`) containing six baseline funcs (`Add`,
`Negate`, `Sqrt`, `Greet`, `Validate`, `Encode`). It asserts:

- Exactly six `extern fun sentinel.<Name>(...)` lines.
- Scalar/bool/float pass-through (`Add: (int, int) -> int`,
  `Negate: bool -> bool`, `Sqrt: float -> float`).
- String in/out (`Greet: string -> string`).
- Error-only result (`Validate: string -> Result<unit, string>`).
- Bytes return (`Encode: string -> bytes`).
- Every line carries its C `//export` symbol as a trailing
  `// mochi_go_<flat>_<pkg>_<func>` comment for cross-check.
- Byte-deterministic re-emit (second run produces identical source).

In addition, `package3/go/emit/emit_test.go` covers:

- `isIdent` over Mochi identifier rules (letters/digits/underscore;
  no leading digit; no dots, hyphens; non-empty).
- `NewEmitter` constructor validation (nil wrap, empty alias,
  non-identifier alias).
- Empty wrapper produces a parseable banner-only source.
- Scalar / string / unit / error-only / value-plus-error / bytes /
  bool / float baseline lowerings.
- Deterministic ordering across `EmittedFunc` permutations
  (Alpha < Mu < Zeta after the internal sort).
- Multi-result tuple lowering deferred to phase 7.1 (closed-switch
  SkipNote with stable reason).
- Param missing MochiType records a SkipNote rather than panicking.
- `renderResultType` matrix over `(Result nil/value × HasError
  false/true × list/scalar)`.

## Lowering decisions

Phase 7 is the *baseline* of the extern emitter. The wrapper
(phase 6) already short-circuits non-baseline shapes via
`SkipNote`; phase 7 inherits that closure, so the externs file
is always a strict subset of the wrapper's surface. Sub-phases
7.x lift the deferred shapes:

- **7.1 multi-result tuples** -- Adds Mochi tuple result types
  (`(int, string)`) for wrappers that emit `out_param` slots.
  Requires the phase 6.x sub-phase that lowers multi-result Go
  funcs to be live.
- **7.2 handle types** -- Emits `extern type handle<T>` shims for
  the cgo handle pool keys phase 14's goroutine bridge produces.
  Each handle type gets one Mochi-side opaque type and matching
  `_free` extern.
- **7.3 method receivers** -- Emits a Mochi `extern fun <alias>
  .<RecvType>.<Method>(self: handle<RecvType>, ...)` shape
  once phase 6.1 ships the receiver-bearing wrapper.

The error-lowering choice is **Result<T, string>** (MEP-13 sum
type with a string payload). The reasoning:

- Mochi's idiom for fallible operations is the sum type
  `Result<T, E>`, with explicit `match` pattern handling. No
  non-local control flow (`throw`/`catch`) at the FFI boundary.
- `string` keeps the payload trivially bridgeable. Phase 6's
  wrapper already lowers `err.Error()` to a `*C.char`; phase 7's
  consumer treats it as a Mochi `string`. The error chain (cause,
  wraps) is lost; recovering it lands in phase 13's cosign
  integration when error-payload structs become structured.
- This mirrors MEP-73's Rust bridge exactly so phase 11's audit
  output can use one template across both languages.

The **C-symbol trailing comment** on every extern line is *not*
decorative. Phase 10's `mochi.lock --check` hashes the externs
file along with the wrapper. The trailing comment is the
human-readable cross-reference an auditor uses to confirm "yes,
`sentinel.Encode` lowers to `mochi_go_example_com_sentinel_text_Encode`",
without having to grep the wrapper. This is the same approach
the MEP-73 Rust bridge uses, and the same line-shape (single
trailing `//` per declaration) makes a diff trivial to review.

The **alias** is required at construction time. Phase 8's import
grammar passes the user's `as <alias>` token through verbatim;
phase 7 validates it as a Mochi identifier (rejects `yaml.v3`,
`pkg-name`, `123pkg`). The import resolver may suggest a
sanitised alias (`yaml_v3`) when the user's chosen alias is
invalid.

The **sort key** is the C symbol (`f.Symbol`), not the Mochi
name. Sorting by the C symbol means a wrapper that re-shuffles
its symbol order (a future phase 6.x change, for example, adding
a per-package prefix) produces a stable ordering downstream;
sorting by the Mochi alias-name would mask the wrapper's intent
and complicate the lockfile diff.

The **unit return form** drops the trailing `:` clause entirely
(`extern fun sentinel.Log(msg: string)  // <sym>` rather than
`... : unit  // ...`). This matches the existing
`tests/parser/valid/extern_decl.mochi` convention and keeps the
externs file readable when most wrappers return values.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/emit/emit.go` | `Emitter`, `Result`, `EmittedExtern`, `EmittedParam`, `SkipNote`, `NewEmitter`, `Emit`, `emitOne`, `render`, `renderExtern`, `renderResultType`, `isIdent`. |
| `package3/go/emit/emit_test.go` | 13-case unit suite covering ident validation, constructor errors, empty wrappers, all baseline lowering shapes, determinism, multi-result skip, nil-Mochi skip, result-type rendering matrix. |
| `package3/go/emit/phase07_test.go` | `TestPhase7ExternEmitter` end-to-end sentinel over the 2-package fixture. |
| `website/docs/implementation/0074/phase-07-extern-emit.md` | (this page) |

## Test set

- `TestPhase7ExternEmitter`
- All `package3/go/emit/...` unit tests (13 sibling tests).

Local run on darwin-arm64:

```
$ go test ./package3/go/...
ok      mochi/package3/go/apisurface    (cached)
ok      mochi/package3/go/build (cached)
ok      mochi/package3/go/cmd/go-ingest (cached)
ok      mochi/package3/go/emit  0.497s
ok      mochi/package3/go/errors        (cached)
ok      mochi/package3/go/moduleproxy   (cached)
ok      mochi/package3/go/semver        (cached)
ok      mochi/package3/go/sumdb (cached)
ok      mochi/package3/go/typemap       (cached)
ok      mochi/package3/go/wrapper       (cached)
$ go vet ./package3/go/...
(no output)
```

## Closeout notes

Phase 7 is the baseline only. The deferred sub-phases are
explicit (each will be its own PR per the umbrella-phase coverage
rule):

- **7.1 multi-result tuples.** Adds `(T1, T2, ...)` lowering for
  multi-result wrappers. Depends on phase 6.x sub-phase shipping
  the corresponding wrapper.
- **7.2 handle-type declarations.** Adds `extern type handle<T>`
  emission and matching `_free` extern. Depends on phase 6.3
  (chan handles), 6.4 (func value handles), 6.5 (map handles).
- **7.3 method receivers.** Adds receiver-bearing extern shape.
  Depends on phase 6.1 (method wrappers).

The closed-switch lowering keeps phase 7.x additive: the baseline
switch falls through to a SkipNote with a stable reason string
(`"multi-result tuple lowering lands in phase 7.1"`); each
sub-phase replaces one SkipNote branch with a real lowering.

Determinism is enforced by the test suite
(`TestEmitDeterministicOrdering` and the sentinel's re-emit
comparison) and by the implementation: wrappers are sorted by C
symbol before rendering, params keep wrapper order, the banner
text is constant. The lockfile (phase 10) records a SHA-256 of
the externs source; non-determinism here would cause spurious
lockfile churn.

The dependency surface stays minimal: `errors`, `fmt`, `sort`,
`strings`, plus the bridge's own `typemap` and `wrapper`. No
external imports; no parser dependency (the externs file is
validated by the Mochi parser at phase 8's gate, not at emit
time, because the parser lives in `runtime/parser/`).
