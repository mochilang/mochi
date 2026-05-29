---
title: "Phase 4. type mapping table"
sidebar_position: 6
sidebar_label: "Phase 4. type mapping"
description: "MEP-71 Phase 4 lands the closed Python-to-Mochi type translation table, the type expression parser, and the SkipReason-emitting Mapper."
---

# Phase 4. type mapping table

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-71 §Phases](/docs/mep/mep-0071#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 23:26 (GMT+7) |
| Landed         | 2026-05-29 23:34 (GMT+7) |
| Tracking issue | (filled by automation) |
| Tracking PR    | (filled by automation) |
| Commit         | (filled by automation) |

## Gate

`TestPhase4TypeMapping` in `package3/python/typemap/phase04_test.go` with subtests:

- `scalar_table`. The 7 scalar lowerings (`int`, `float`, `bool`, `str`, `bytes`, `None`, `NoneType`).
- `collection_table`. The 13 collection lowerings covering `list` / `List`, `set` / `frozenset`, `dict` / `Dict`, `tuple` heterogeneous + homogeneous (`tuple[T, ...]` -> `list<T>`), `Iterator` / `Iterable`, `AsyncIterator`, `Awaitable`, `Coroutine[Y, S, R]` reduced to `async R`, `collections.abc.Iterable` prefix stripping.
- `union_and_optional`. `Optional[int]` and `int | None` (both orderings) collapse to `int?`. `int | str` becomes `int | string`. `int | str | None` becomes `int | string?`. The legacy `Union[int, str]` form parses identically.
- `callable`. `Callable[[int, str], bool]` lowers to `fun(int, string): bool`.
- `refusal_set`. The 10 refusal cases: `Any` (without `AllowPartial`), `complex`, `object` outside Protocol, `ParamSpec[P]`, `Callable[..., int]`, `Unpack[Ts]`, `Generator[int, None, None]`, `Type[int]`, unresolved forward references, and `Dict[float, int]` (non-scalar map key).
- `class_mappings`. TypedDict -> KindRecord, frozen `@dataclass` -> KindRecord, mutable `@dataclass` -> refusal with override suggestion, Protocol -> KindInterface.
- `partial_allows_any`. With `AllowPartial = true` (PEP 561 partial marker or stubgen output), `Any` maps to `ref<Any>` instead of refusing.
- `nested_round_trip`. `Dict[str, List[Optional[int]]]` -> `map<string, list<int?>>`.

The package-level coverage:

- `package3/python/typemap/mochitype_test.go`. Kind rendering over 16 cases including the unknown / out-of-range sentinel. Render of every supported kind: scalars, list, nested list, map, set, tuple, Optional (idempotent nesting), sum (no None / with None / single branch / all-None collapse), async, stream, fun (with and without params), ref, typevar, record (with field-name sort), interface (with method-name sort and async-method prefix). Structural Equal across lists / records / fields. Panics on Return / FunParams when Kind is not KindFun.
- `package3/python/typemap/pytype_test.go`. Parses bare names, qualified attributes (`typing.List`, `collections.abc.Iterable`), subscripts (single and nested), qualified subscripts (`typing.Dict[str, int]`), unions (2-branch and many-branch), Optional, Callable with bracketed-list parameters, Callable with ellipsis (`Callable[..., int]`), homogeneous tuples (`tuple[int, ...]`), forward-reference string literals, Literal value lists (string / int / bool / None / multi-value), bare `None`, parenthesised unions, trailing input rejection, and 6 explicit error cases (unterminated subscript, leading `|`, unbalanced parens, unterminated string). String round-trip across 6 canonical shapes. `QualifiedName` returns empty for non-identifier kinds.
- `package3/python/typemap/mapper_test.go`. Scalars, list variants (bare lowercase / capitalised / `typing.`-prefixed), dict, dict-rejects-non-scalar-key (float and bool), homogeneous + heterogeneous tuples, set / frozenset, Optional in both syntaxes, Union with and without None, Callable, Callable-ellipsis-refused, Awaitable, Coroutine[Y, S, R] reduced, AsyncIterator -> stream, Iterator -> list, Any refused by default and accepted under AllowPartial, complex refused, ParamSpec / Unpack / Generator refused, forward references resolved against `Classes` and unresolved otherwise, TypeVars resolved against `TypeVars`, generic user-defined classes lowered to `KindRef` with parametric Params, `Literal` collapsing to scalar for single-shape values and refused for mixed-shape, `Final` / `ClassVar` / `Annotated` / `NotRequired` unwrap to the inner type, `Type[X]` refused, deep nesting (`Dict[str, List[Optional[int]]]`), empty-string refusal, `ItemPath` propagation into `SkipReport`, bare typing constructors refused, union-with-Any refused with `SkipOpenUnion` even when `AllowPartial`, `builtins.` and `collections.abc.` prefix stripping, list with wrong arity rejected with informative detail.
- `package3/python/typemap/class_test.go`. TypedDict -> record (2 fields), frozen `@dataclass` -> record, mutable `@dataclass` -> refusal with non-empty Override, whitespace-tolerant frozen detection (`dataclasses.dataclass(  frozen = True  )`), Protocol -> interface with one method, Protocol dunders skipped into `Skipped` with `SkipDunder`, `*args` / `**kwargs` in Protocol method -> `SkipParamSpec`, unannotated Protocol parameter -> `SkipUnsupportedTypingConstruct`, plain class (no Protocol / TypedDict / dataclass) refused entirely, TypedDict field with `complex` -> field skipped into `Skipped` while class still maps, top-level function mapping (sync return, async wraps return in `NewAsync`, missing return defaults to `None`, `*args` refused), `classDecoratedAsFrozen` over 8 decorator shapes.

## Lowering decisions

The type mapping pipeline is three stages:

1. **Parse** the raw type-expression string via `ParsePyType` into a `PyType` AST. The grammar is intentionally narrow: names, qualified attributes, subscripts, unions, ellipsis, literals (including string forward references), and parenthesised groups. The parser is hand-rolled (no Python AST library); it covers the constructs that appear in `.pyi` files without trying to handle arbitrary Python expressions.
2. **Lower** the PyType into a `MochiType` via the `Mapper`. The mapper holds the closed table, the in-scope `TypeVars`, the in-scope `Classes`, and the `AllowPartial` flag. Out-of-table items return a `Decision` carrying a `*errors.SkipReport` with the correct `SkipReason` from MEP-71's research note 05.
3. **Render** the MochiType via `Render` into the canonical Mochi-side string. Record fields and interface methods sort alphabetically so the rendering is deterministic across stub iterations.

The closed scalar table is `int -> int`, `float -> float`, `bool -> bool`, `str -> string`, `bytes -> bytes`, `None / NoneType -> None`. Mochi's `string` (not `str`) is the canonical scalar name.

Container lowerings follow the spec table. The two non-obvious decisions:

- `dict[K, V]` rejects non-scalar keys. The spec restricts K to `str` or an integer type; we further exclude `bool` because Mochi's map key set excludes `bool` by policy.
- `tuple[T, ...]` collapses to `list<T>`. The homogeneous form is structurally a list; preserving "tuple" would suggest a fixed-length type that the source doesn't claim. Phase 5 will not be surprised by this lowering because tuples and lists share the same iteration / indexing surface on the Mochi side.
- `Iterator[T]` materialises to `list<T>` at the wrapper boundary. The Mochi caller does not see Python's lazy iteration; the wrapper consumes the iterator and hands back a fully materialised list. This is the spec's intentional choice (research note 05 §"Iterators at the boundary").

Union and Optional are unified through `NewSum`: a union with any `None` branch becomes an Optional wrapping the non-None branches; a union with no None remains a sum; a single-branch sum collapses to the branch directly. This is the spec's PEP 604 lowering rule.

`Callable[[A, B], R]` becomes a `KindFun` MochiType with Params = `[A, B, R]` (the return type is the final entry). `Callable[..., R]` is refused with `SkipParamSpec`: the bridge does not synthesise opaque variadic dispatch surfaces.

`Awaitable[T]` becomes `async T`; `Coroutine[Y, S, R]` reduces to `async R` (the yield and send types are not part of the Mochi async surface). `AsyncIterator[T]` becomes `stream<T>`.

`Annotated[T, ...]`, `Final[T]`, `ClassVar[T]`, `NotRequired[T]`, and `Required[T]` all unwrap to the inner T. The metadata is preserved by Phase 5 in the wrapper-generation step, not in the type signature.

`Literal[v, ...]` collapses to the scalar that matches all literal arguments. Mixed-kind Literal (e.g. `Literal["a", 1]`) is refused; this is the spec's narrowing-dependent refusal rule.

Class mappings (`MapClass`) dispatch on the Phase 3 ClassDecl flags:

- `IsTypedDict` -> KindRecord. Fields are mapped one by one; a field whose type is out-of-table is appended to `Skipped` while the class still maps successfully (the user can pick up the report and decide whether to write an override or accept the narrower interface).
- `IsDataclass` -> KindRecord only when the decorator list contains `frozen=True` (whitespace-tolerant detection); mutable dataclasses are refused with an `Override` suggestion the user can paste into the sidecar.
- `IsProtocol` -> KindInterface. Methods are mapped; dunders skip with `SkipDunder`; `*args` / `**kwargs` skip with `SkipParamSpec`; unannotated parameters skip with `SkipUnsupportedTypingConstruct`. The `self` / `cls` parameter is dropped silently because Mochi interfaces do not carry it.
- Plain classes (no flag) refuse entirely with `SkipUnsupportedTypingConstruct`: the bridge does not invent a record / interface surface for arbitrary Python classes.

The deliberate "closed table, refuse on miss" decision is documented in `package3/python/typemap/doc.go`. We do not invent dynamic interop types (no MochiPyAny). The user can always opt back in via a hand-written `extern python fun` declaration in the MEP-51 sidecar.

## Files changed

| File | Purpose |
|------|---------|
| `package3/python/typemap/doc.go` | Package doc (closed table, refusal set) |
| `package3/python/typemap/mochitype.go` | `Kind`, `MochiType`, `MochiField`, `MochiMethod`, constructors, `Render`, `Equal` |
| `package3/python/typemap/pytype.go` | `PyKind`, `PyType`, `ParsePyType` (hand-rolled recursive-descent parser) |
| `package3/python/typemap/mapper.go` | `Mapper`, `Decision`, `Map`, `MapParsed`, the closed scalar / container / union / Callable / Literal lowering table |
| `package3/python/typemap/class.go` | `ClassDecision`, `MapClass`, `MapFunction`, field / method / function lowerings |
| `package3/python/typemap/mochitype_test.go` | MochiType render + Equal tests |
| `package3/python/typemap/pytype_test.go` | PyType parser tests |
| `package3/python/typemap/mapper_test.go` | Closed-table behaviour tests + refusal set tests |
| `package3/python/typemap/class_test.go` | Class lowering tests (TypedDict / dataclass / Protocol) + function lowering |
| `package3/python/typemap/phase04_test.go` | `TestPhase4TypeMapping` sentinel with 8 subtests |

## Test set

- `TestPhase4TypeMapping/scalar_table`
- `TestPhase4TypeMapping/collection_table`
- `TestPhase4TypeMapping/union_and_optional`
- `TestPhase4TypeMapping/callable`
- `TestPhase4TypeMapping/refusal_set`
- `TestPhase4TypeMapping/class_mappings`
- `TestPhase4TypeMapping/partial_allows_any`
- `TestPhase4TypeMapping/nested_round_trip`
- All `package3/python/typemap/...` unit tests.

## Closeout notes

Phase 4 is the type-policy chokepoint. Every Python type that crosses into Mochi flows through this table. Adding a new lowering means adding a case to `Mapper.mapName` / `Mapper.mapSubscript` and a test row in `mapper_test.go`; removing one means tightening the refusal set with a fresh `SkipReason` (and a SKIPPED.txt golden update on Phase 5's fixture corpus, when Phase 5 lands).

The Mapper does not know about Phase 3's `ModuleSurface` directly. The caller (Phase 5 / 6) walks the surface, sets `ItemPath` to the dotted Python path of the item being mapped (e.g. `httpx.AsyncClient.send`), invokes `Map` / `MapClass` / `MapFunction`, and collects the SkipReports. This separation means the type policy stays in `typemap/` and the walking + emission stays in `wrapper/` and `emit/` once those phases land.

The `AllowPartial` flag is the bridge into Phase 3's `StubSource.Partial`. When the upstream source did not commit to full types, the mapper lowers `Any` to `ref<Any>` instead of refusing; downstream phases will emit the wrapper symbol with a dynamic boundary call. Phase 5 is responsible for honouring the `--allow-partial` CLI flag and threading it through into the Mapper.

No CPython runtime, no mypy install, and no typeshed checkout is required for any test in this phase. The mapper consumes plain strings; the tests construct stubs.ClassDecl values inline rather than parsing real `.pyi` files (Phase 3's tests already cover that surface).
