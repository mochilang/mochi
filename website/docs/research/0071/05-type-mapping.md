---
title: "05. Type mapping"
sidebar_position: 6
sidebar_label: "05. Type mapping"
description: "The complete closed translation table between Python types and Mochi types, the refusal cases, the generic resolution rule, the `int` arbitrary precision boundary, `Optional` / `Union` desugar, dataclass / TypedDict / NamedTuple handling, Protocol structural matching, callable arrow types, async iterators."
---

# 05. Type mapping

This note defines the closed type-mapping table that drives the wrapper synthesiser. The table is closed by design ([[02-design-philosophy]] §7): types inside the table get first-class Mochi treatment; types outside become opaque `PyObject` handles.

## Scalars

| Python | Mochi | Notes |
|--------|-------|-------|
| `bool` | `bool` | Bidirectional, no boxing. CPython's `True` / `False` are singletons; the wrapper holds borrowed references. |
| `int` | `int` if `\|n\| <= sys.maxsize`, else `bigint` | Python `int` is arbitrary precision; Mochi's `int` is 64-bit on 64-bit hosts. The boundary is checked at the wrapper boundary; values outside the range become `bigint` (Mochi's arbitrary-precision integer type from MEP-2 §3.4). |
| `float` | `float` | IEEE 754 double-precision on both sides. NaN, infinity, and signed-zero round-trip. |
| `complex` | refused | Python's `complex` has no Mochi counterpart. Becomes opaque `PyObject`. `SkipReason::NoComplexType`. |
| `str` | `string` | Both UTF-8 internally (Mochi) / UTF-32 internally (CPython); the wrapper converts at the boundary. The wrapper holds a `PyObject*` and decodes lazily on access. |
| `bytes` | `bytes` | Bidirectional, no decoding. The Mochi `bytes` type is a `[]byte` in the host; the wrapper memcpy's. |
| `bytearray` | `bytes` (with mutability annotation) | Refused for writeback; treated as a `bytes` copy at the boundary. |
| `None` | `null` (in `Optional[T]` context); refused as a standalone type | `None` as a return type becomes Mochi `void`; `None` as a parameter type is rejected (Mochi disallows passing `null` as a non-optional value). |
| `bytes` of fixed size | `bytes` | No fixed-size type in Mochi; size is dynamic. |

The `int` boundary is the only scalar with runtime cost. Most Python `int` values are within `[-2^63, 2^63)` and pass through directly; large ones (cryptography, accounting, bignum math) take the `bigint` path. The wrapper synthesiser does not annotate which call sites might overflow; that information is not in the stubs. Callers that always stay within int64 can opt out of the boxing via `[python].int-mode = "int64-only"` (refuses any return value that would have needed bigint).

## Sequence and mapping collections

| Python | Mochi | Notes |
|--------|-------|-------|
| `list[T]` | `list[T']` where `T' = MapType(T)` | Eager. The wrapper iterates the Python list and constructs a Mochi list with each element converted. Round-trip is symmetric. |
| `tuple[T1, T2, ...]` (fixed) | `(T1', T2', ...)` Mochi tuple | Fixed-arity tuples become Mochi tuples. Element count and types match. |
| `tuple[T, ...]` (variadic) | `list[T']` | Variadic tuples have no Mochi counterpart; they degrade to list. |
| `dict[K, V]` | `dict[K', V']` | Eager. The wrapper iterates `.items()` and constructs a Mochi dict. Insertion order is preserved (both CPython 3.7+ and Mochi guarantee this). |
| `set[T]` | `set[T']` | Eager. The wrapper iterates and constructs. |
| `frozenset[T]` | `set[T']` | Frozenset becomes regular set; mutability discipline is lost. |
| `bytes` | covered above | |
| `range` | `range` (Mochi has it under MEP-2 §5.7) | Iteration semantics match. |

Eager construction is the design choice: every list/dict/tuple is fully materialised at the boundary, never lazily proxied. Reasons:

1. **Lifetime safety.** A lazily proxied list would hold a `PyObject*` borrowed reference; if the Python side mutated the list under the Mochi reference, behaviour would be undefined. Eager copy avoids the question.
2. **GIL discipline.** Lazy access would require GIL reacquisition on every element access from Mochi. Eager copy acquires GIL once at the boundary.
3. **Type confidence.** Eager construction is the only place we can verify that every element actually matches `T`; lazy would defer the check to the first access, leaving stale errors.

The cost is wall-clock: passing a 10M-element list across the boundary costs ~50ms. For hot loops, the convention is to pass an opaque `PyObject` handle and call into Python for the iteration, or use the streaming `Iterator[T]` mapping (below).

## Algebraic types

| Python | Mochi | Notes |
|--------|-------|-------|
| `Optional[T]` (`T \| None`) | `T'?` (Mochi optional) | First-class. The Mochi `?` suffix represents "value or null". |
| `Union[T1, T2]` (closed, T1 ≠ T2) | Mochi sum type if both branches are nominal types | Generated as `enum U { T1(T1'), T2(T2') }` plus a destructuring helper. |
| `Union[T1, T2, ...]` (open or with `Any`) | `any` | Open unions degrade. |
| `Literal["a", "b"]` | Mochi string literal enum | Maps to `enum { A = "a", B = "b" }` if literal is string; integer literal becomes int enum. |
| `Literal[1, 2, 3]` | int enum | Same. |
| `Final[T]` | `T'` (Mochi has no final marker) | Finality is a static-checker concern; the bridge ignores it. |
| `Annotated[T, ...]` | `T'` (annotations stripped) | The metadata payload is ignored unless it's a Mochi-specific marker (reserved for future). |
| `Never` / `NoReturn` | `never` | Mochi's bottom type. |
| `Any` | `any` | Boxed opaque. |

The closed-Union → sum-type translation is the trickiest case: it works only when both branches are themselves nominal Mochi types and they have no overlap. `Union[int, str]` is a closed Union of nominal types; it maps to a Mochi sum. `Union[list[int], list[str]]` is open via subtyping (Mochi's subtype lattice doesn't distinguish list element types at runtime); it degrades to `any`.

## User-defined types

| Python | Mochi | Notes |
|--------|-------|-------|
| `@dataclass class C: x: int; y: str = "..."` | Mochi `struct C { x: int, y: string }` | Field order, defaults, and frozen marker are preserved. `__init__` is auto-generated on both sides. |
| `class C(TypedDict): x: int; y: str` | Mochi `struct C` (same as dataclass) | TypedDict is structural in Python; Mochi treats it nominally for safety. `total=False` becomes optional fields. |
| `class P(NamedTuple): x: int; y: int` | Mochi `(int, int)` tuple, or struct if names matter | If the tuple is unpacked everywhere, it stays a tuple; if field access is observed, becomes a struct. |
| `class E(Enum): A = 1; B = 2` | Mochi `enum E { A, B }` | Values map directly. |
| `class P(Protocol): def f(self, x: int) -> str: ...` | Mochi `interface P { fn f(x: int): string }` | Protocols become Mochi interfaces. Structural matching is preserved. |
| `class C: ...` (regular class) | opaque `PyObject` handle, methods accessed by name | A regular class becomes a handle. The wrapper synthesises `extern fn` declarations for each method. |
| `class C(BaseException): ...` | Mochi `error C` | Exception classes become Mochi error types. |
| `@runtime_checkable Protocol` | Mochi `interface` with `isinstance`-style narrowing | A runtime-checkable Protocol generates a Mochi predicate. |

The dataclass case is the most common (numpy.dtype, pydantic.BaseModel via dataclass-like behaviour, FastAPI request models). The TypedDict case is the second most common (JSON API surfaces). Both map cleanly.

The regular-class fallback is intentional: not every Python class deserves a Mochi struct. A pydantic.BaseModel with validators and computed properties has behaviour that a Mochi struct cannot represent. The opaque-handle path is the safe default; the user can opt in to struct conversion by annotating the class with a Mochi-side hint.

## Callable types

| Python | Mochi | Notes |
|--------|-------|-------|
| `Callable[[T1, T2], R]` | Mochi arrow `fn(T1', T2') -> R'` | Closed types. |
| `Callable[..., R]` | Mochi `any` (callable but untyped) | Variadic, ignored. |
| `Callable[P, R]` with `ParamSpec` | refused | ParamSpec is too dynamic for the closed table. |
| `Coroutine[Y, S, R]` | Mochi `async fn() -> R'` | Y and S are typically ignored (yields and sends are coroutine-internal). |
| `Awaitable[R]` | Mochi `async fn() -> R'` | Same as Coroutine. |
| `Generator[Y, S, R]` | Mochi `Iterator[Y']` (S and R discarded) | The send and return values of a generator are not first-class in Mochi. |
| `AsyncGenerator[Y, S]` | Mochi `AsyncIterator[Y']` | Same. |
| `Iterator[T]` | Mochi `Iterator[T']` | First-class. The wrapper holds a Python iterator and exposes `next()`. |
| `Iterable[T]` | Mochi `Iterable[T']` | Same; `iter()` is called once at the boundary. |
| `AsyncIterator[T]` | Mochi `AsyncIterator[T']` | First-class. |

Callable round-trip: a Mochi function passed to Python becomes a Python callable that releases the GIL and re-enters Mochi. A Python callable returned to Mochi becomes a Mochi arrow that acquires the GIL and re-enters Python. The wrapper handles both directions symmetrically.

## Generic resolution rule

Python generics are erased at runtime. Mochi's are monomorphised. The bridge has to bridge these:

- **Generic alias instantiations**: `list[int]`, `dict[str, int]`, `Optional[User]` are concrete; they map directly using the table.
- **Type variables in function signatures**: `def first(xs: list[T]) -> T:` is a generic Python function. The bridge emits a Mochi generic function `fn first[T](xs: list[T]) -> T` and trusts Python's runtime polymorphism at the call site.
- **Generic classes**: `class Container(Generic[T]): ...` becomes a Mochi `struct Container[T]`. Instantiation happens at the call site.
- **Higher-kinded types**: not in Python's typing system; not in Mochi's; no translation needed.

The generic functions delegate to Python's untyped runtime polymorphism. There is no monomorphisation of Python deps (unlike MEP-73's Rust deps where every generic instantiation gets its own wrapper). Reason: Python generics are pure type hints; the runtime behaviour is the same regardless of T. We can pass `T = User` or `T = int` to the same Python function and get a result back; the wrapper's only job is to translate the boundary types.

## The refusal table

When a Python type cannot map, the bridge emits a `SkipReason` and the item becomes `any` in the Mochi shim. The user can still call the function; the return type and argument types are dynamic.

| Reason | Python construct |
|--------|------------------|
| `SkipReason::NoComplexType` | `complex` |
| `SkipReason::OpenUnion` | `Union[A, B, ...]` where one branch is `Any` or subtype-overlapping |
| `SkipReason::ParamSpec` | `Callable[P, R]` with `ParamSpec` |
| `SkipReason::TypeVarTuple` | `tuple[*Ts]` |
| `SkipReason::ForwardRef` | Unresolvable forward reference |
| `SkipReason::UnsupportedTypingConstruct` | `cast`, `assert_type`, `reveal_type`, etc. (these should not appear in stubs but sometimes do in inline) |
| `SkipReason::CFunctionWithoutStubs` | C extension function with no `.pyi`; signature is `(*args, **kwargs)` |
| `SkipReason::OverloadAmbiguity` | `@overload` set where the closed table cannot pick a single arm |

## The PyObject opaque handle

When no other mapping applies, the value becomes an opaque `PyObject` handle. From Mochi:

```mochi
import python "requests" as requests

fn main() {
    let session = requests.Session()   // returns PyObject handle
    let resp = session.get(url)         // method call on handle
    let body = resp.json()              // dict[str, any] (typed by stubs)
    let close_method = session.close    // PyObject handle (callable)
    close_method()                       // invoke
}
```

The handle is reference-counted: a Mochi variable holds a CPython `PyObject*` with refcount incremented. When the Mochi variable goes out of scope (Mochi's escape analysis or GC), the wrapper decrements. The wrapper acquires the GIL for the decrement.

The handle is opaque: Mochi cannot inspect fields, call methods other than through synthesised `extern fn`, or pattern-match. The handle is movable across function boundaries inside Mochi.

## Cross-references

- [[02-design-philosophy]] §7 for why the table is closed.
- [[04-pep561-stub-ingest]] for how the type information is sourced.
- [[10-gil-and-cextensions]] for the GIL handling on every conversion.
- [MEP-71 §6](/docs/mep/mep-0071) for the normative type table.
- [MEP-2](/docs/mep/mep-0002) for Mochi's scalar and collection types.
- [PEP 484](https://peps.python.org/pep-0484/) and [PEP 526](https://peps.python.org/pep-0526/) for Python typing.
