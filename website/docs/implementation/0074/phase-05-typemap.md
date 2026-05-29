---
title: "Phase 5. Typemap"
sidebar_position: 7
sidebar_label: "Phase 5. Typemap"
description: "MEP-74 Phase 5 lands the closed Go-type to Mochi-type mapping table: ScalarType, ListType, MochiMap, RecordType, HandleType, FuncType, OptionType, AnyType, paired with a TransferDirection (Copy, View, Handle) that drives the cgo wrapper synthesiser in phase 6."
---

# Phase 5. Typemap

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 22:30 (GMT+7) |
| Landed         | 2026-05-29 23:03 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase5Typemap` in `package3/go/typemap/phase05_test.go`: drives
a fixture surface with one record-bridgeable struct (Point, both
fields int + exported), one handle-only struct (Mutex, has an
unexported field), one alias (Name = string), plus a battery of 21
type expressions that together exercise every variant of the closed
MochiType grammar: ScalarType (int, float, bool, string, bytes),
ListType (`[]string`, `[]Point`, `[4]byte`), MochiMap (with nested
list value), RecordType (Point), HandleType (Mutex, chan int,
io.Reader), FuncType (with multi-result), OptionType (pointer to
scalar and pointer to record), AnyType (`interface{}` and `any`),
plus the ellipsis-at-top-level shortcut. The sentinel asserts each
mapping has the documented TransferDirection (Copy for value types,
Handle for opaque ones) and verifies — via a `seenKinds` set — that
every concrete MochiType variant is reached. A regression in any
single mapping rule fails the sentinel even if the regression does
not affect any other variant.

In addition the package-level test suite covers:

- `package3/go/typemap/mapper_test.go`: every Go basic type widens
  or maps to the documented Mochi scalar (`int`, `float`, `bool`,
  `string`, `error`, with `int8/int16/int32/int64/uint*/byte/rune`
  collapsing to `int` and `float32/float64` collapsing to `float`);
  `[]byte` collapses to Mochi `bytes`; map with non-scalar key is
  rejected with `ErrUnmappable`; pointer-to-scalar yields
  `option<T>`; pointer-to-chan stays a handle (no double-wrap);
  variadic params surface as `list<T>` in the params tuple; `any`
  and `comparable` both fall through to `AnyType` with Handle
  direction; named types map to a handle keyed by qualified name
  when no surface is available, to a `RecordType` when the surface
  shows an all-exported, all-Copy struct, and back to a handle when
  any field is unexported or itself requires Handle; named aliases
  rewrite to their underlying mapping (`type Name = string` ->
  `ScalarType{string}`; `type IntList = []int` -> `ListType`);
  nested composites (`[][]int`, `map[string][]map[string]int`)
  render canonically; pointer-to-record yields
  `OptionType{RecordType}`; cross-package unknown named types
  produce a handle keyed by the qualified name; widening notes are
  attached to narrow Go integer types; the Mapper cache returns the
  same Mapping pointer for repeated lookups of the same type
  expression; `MapFunc` over an `apisurface.FuncDecl` produces a
  `FuncType` with the same arity; `TransferDirection.String` and
  `MochiType.String` round-trip every documented form.

## Lowering decisions

Phase 5 defines the closed grammar of Mochi types that the bridge
will emit. The grammar is intentionally narrow — eight variants —
so phase 6 (cgo wrapper synthesiser) and phase 7 (extern fn
emitter) can switch over it exhaustively. Adding a ninth variant
requires updating every consumer in lock-step, which the closed
interface (`isMochiType()` private marker method) enforces at
compile time.

The `TransferDirection` triple — `Copy`, `View`, `Handle` —
captures the *runtime* boundary-crossing rule, independent of the
Mochi-side type. A `ListType` may be Copy (slice of scalars), View
(slice of records borrowed from Go memory), or Handle (slice of
channels). Phase 6 dispatches on `(MochiType, TransferDirection)`
together, so embedding the direction in `Mapping` rather than in
the type itself keeps the type grammar small while still letting
the wrapper synthesiser make per-call decisions.

Every Go integer width collapses to Mochi `int`. The collapse is
deliberate: Mochi is a single-int language by spec (MEP-39's
`int` is a 64-bit signed scalar). Each narrow Go integer
(`int8/16/32`, `uint*`, `byte`, `rune`, `uintptr`) carries a
`Note` documenting the widening so phase 7's diagnostic output
can warn the user that a Go API taking `int32` will see Mochi
`int` widening on call and narrowing on return. The narrowing
direction (return-value `int -> int32`) is enforced by the
wrapper-emitter in phase 6 with an overflow check.

Pointer types map to `OptionType` when the pointee is itself a
value type (scalar, slice, map, record). The `nil` pointer becomes
`None`; a live pointer becomes `Some(deref)`. Pointers to handle
types (chan, func, interface) stay handles directly — wrapping a
handle in `option` adds no information because the Mochi-side
handle key is already nullable (the zero key is reserved as
"absent"). This single-level unwrap is captured by an early-return
in `mapPointer`.

Named struct types are the load-bearing record-vs-handle decision.
The rule is conservative: a struct is record-bridgeable iff every
exported field is itself Copy-bridgeable. Any unexported field, any
field whose mapping requires Handle (nested chan, func value, or
non-bridgeable named type), or any unmappable field forces the
whole struct to be a handle. The fallback path emits a `Note`
naming the offending field so the user can see why their `Point`-
shaped struct was bridged opaquely. This precise blame attribution
is tested by `TestMapStructWithHandleFieldFallsBack`.

Channels, function values, and non-empty interfaces all map to
`HandleType` with `Handle` direction. Channels carry an
expressive name (`chan<int>`) so the bridge can distinguish handles
to channels of different element types at runtime; this protects
phase 14's channel-bridge against type-confusion mistakes when one
Go API accepts `chan int` and another accepts `chan string`. Empty
`interface{}` and `any` collapse to `AnyType` (still Handle
direction) — the bridge accepts an opaque value, and the Mochi
side gets a `any` it can inspect via runtime reflection.

The `Mapper` is bound to an `apisurface.Surface` at construction
time. When the surface is nil, named types collapse to handles
keyed by qualified name — this is the standalone mode used by
phase 7's documentation generator. When a surface is present,
named types consult `surface.LookupType` for their underlying
shape; this is the cross-reference resolution used by phase 6's
wrapper emitter.

The Mapper cache is keyed by `GoType.String()` and returns the
same `*Mapping` pointer for repeated lookups. Pointer-identity
caching matters because phase 6 will store the Mapping in a struct
field for each function it emits; identical type expressions
should share storage rather than duplicate it.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/typemap/types.go` | `MochiType` interface + 8 concrete variants (`ScalarType`, `ListType`, `MochiMap`, `RecordType`, `HandleType`, `FuncType`, `OptionType`, `AnyType`), `TransferDirection` enum, `Mapping` record. |
| `package3/go/typemap/mapper.go` | `Mapper`, `NewMapper`, `Map`, `MapFunc`, `ErrUnmappable`, plus per-shape handlers (`mapBasic`, `mapPointer`, `mapSlice`, `mapArray`, `mapMochiMap`, `mapChan`, `mapFunc`, `mapEllipsis`, `mapInterface`, `mapStruct`, `mapNamed`, `mapTypeDecl`, `mapStructDecl`). |
| `package3/go/typemap/mapper_test.go` | 32-case unit suite covering every Go type shape, transfer-direction rule, and widening note. |
| `package3/go/typemap/phase05_test.go` | `TestPhase5Typemap` end-to-end sentinel exercising every MochiType variant + every TransferDirection. |
| `website/docs/implementation/0074/phase-05-typemap.md` | (this page) |

## Test set

- `TestPhase5Typemap`
- All `package3/go/typemap/...` unit tests (31 sibling tests).

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
ok  	mochi/package3/go/typemap	0.514s
$ go vet ./package3/go/...
(no output)
```

## Closeout notes

The closed-grammar approach was a deliberate inversion of the
common pattern of letting the wrapper emitter (phase 6) make
ad-hoc type decisions inline. By pushing the table into a dedicated
package consumed by both phase 6 and phase 7, we ensure that the
extern-fn documentation generator and the cgo wrapper synthesiser
agree on what each Go type bridges to. A divergence between them
would mean the docs say `int` and the wrapper takes `int32`, which
is exactly the kind of silent contract drift this MEP set out to
eliminate.

The `Mapping.Notes` slice is the load-bearing feedback channel for
phase 7's `mochi go-bridge audit` command. Every non-obvious
decision — int widening, struct demotion to handle, channel
element-type encoding — leaves a Note so the auditor can produce
a human-readable report explaining the bridge's choices. Phase 7
will sort and dedupe these Notes by Go type expression to keep the
audit output stable across runs.

The `OptionType` variant is intentionally limited to pointer
unwrap. Mochi has a richer option model (option chains, map-get
returning option) but those are concerns of the Mochi compiler,
not the bridge. The bridge's job is to map *one Go type to one
Mochi type*; richer option flows live in user code on the Mochi
side once the value is received.

The widening-note discipline matters for the future
`mochi go-bridge` CLI. A user binding a stdlib API like `os.Pipe`
that returns `(*os.File, *os.File, error)` will see in the audit
report that the `*os.File` results bridge as opaque handles (no
fields are exported), and that any int32 return value (e.g.
`syscall.Wait4`) widens to Mochi int. These two facts together
explain to the user *why* their bridge looks the way it does, with
no need to re-read the spec.

A subtle design decision: the per-field walk in `mapStructDecl`
short-circuits on the first non-Copy field rather than collecting
*all* offending fields. This keeps the Note tight (one offender
named per fallback), and the test suite explicitly verifies the
first offender's name appears in the Note. If phase 7's audit
output ever needs the full list of offenders, the walk can be
generalised in a follow-up without breaking the existing surface.
