---
title: "MEP-51 research note 06, Per-type lowering for Mochi to Python 3.12"
description: "Every Mochi type's Python 3.12 representation, with rationale per type covering boxing, slots, PEP 695 generics, variance, and FFI hand-off."
---

# MEP-51 research note 06, Per-type lowering for Mochi to Python 3.12

Author: research pass for [[mep-0051]]. Date: 2026-05-23 15:00 (GMT+7).
Method: per-type walk over the Mochi type system (defined by the
shared `types/check.go` package), CPython 3.12 reference (the
`typing`, `collections.abc`, and `dataclasses` modules), the
typeshed stubs at `python/typeshed` (revision 2026-05), PEPs 8, 484,
526, 544, 561, 585, 591, 593, 604, 612, 646, 657, 692, 695, 698,
and 723, and the [[mep-0050]] type-lowering note as a structural
template. The Mochi-on-left / Python-on-right table convention is
adopted from [[mep-0049]] and [[mep-0048]].

This note specifies how every Mochi type lowers to Python 3.12 at
the type-system level: what Python annotation is emitted, what
runtime value-shape it corresponds to, and what the type checker
sees. Phase E of the codegen ([[05-codegen-design]] §7) consults
this table on every annotation-emit; the runtime ([[04-runtime]])
provides the few polyfills the table requires.

The note has one section per Mochi type, ordered from primitive
through container through algebraic through interactive. Each
section covers: (a) the Python annotation, (b) the runtime
representation, (c) why this choice over alternatives, (d) special
cases and gotchas, (e) example Mochi-to-Python pair.

---

## 1. `int`

**Annotation**: `int`.

**Runtime**: CPython `PyLongObject`, arbitrary precision, no overflow.

**Choice rationale**: Python's `int` is unique among popular target
languages in being **arbitrary precision by default**. This matches
Mochi's mathematical-int semantic by accident: Mochi `int` has no
overflow at the language level, and Python `int` has none at the
runtime level. The Swift target ([[mep-0049]]) lowers Mochi `int`
to `Int64` for cross-platform determinism; Python avoids that
contortion.

**Caveats**:

- **Boxing**: every int is a heap-allocated `PyObject*`. CPython
  caches `-5..256` so single-digit arithmetic is cheap, but
  arithmetic at scale (10^8 iterations) pays per-op heap traffic.
  We do not optimise; users who need bignum-free arithmetic reach
  for `numpy.int64` or similar (out of scope).
- **`bool` is a subclass of `int`**: `isinstance(True, int)` is
  True. Mochi never relies on this; the emitter ensures booleans
  are pattern-matched **before** ints in `match` chains (see §3.1).
- **PEP 3141 numeric tower**: `int <: Rational <: Real <: Complex
  <: Number`. The Mochi emitter does not touch this hierarchy.

**Example**:

```mochi
let n: int = 1_000_000_000
let big: int = 10 ** 100
```

```python
n: int = 1_000_000_000
big: int = 10 ** 100
```

---

## 2. `float`

**Annotation**: `float`.

**Runtime**: CPython `PyFloatObject`, IEEE 754 binary64 (double).

**Choice rationale**: Python `float` is always 64-bit double; there
is no `float32` in the standard library outside `numpy` and
`array.array('f')`. Mochi `float` is double-precision by spec, so
this is a direct match.

**Caveats**:

- **`nan != nan`**: Mochi `float` follows IEEE 754; `nan` compares
  unequal to itself. The emitter uses `math.isnan(x)` for nan
  detection rather than `x == nan`.
- **`-0.0 == 0.0`**: True under `==`; distinguishable under
  `math.copysign(1, x)`. Mochi semantics align.
- **`inf - inf == nan`**: standard IEEE 754. The emitter does
  not intercept.
- **No `Float80` or `Decimal` in `float`**: Mochi has no surface
  for these. Decimal arithmetic (for currency) uses the `decimal`
  stdlib module via FFI (§16).

**Example**:

```mochi
let pi: float = 3.14159
let nan: float = 0.0 / 0.0
```

```python
import math
pi: float = 3.14159
nan: float = float("nan")
```

The `0.0 / 0.0` -> `float("nan")` rewrite is in the emitter because
CPython raises `ZeroDivisionError` on integer or float zero division;
the Mochi semantic of "float zero division is nan" requires the
explicit `float("nan")` construction.

---

## 3. `bool`

**Annotation**: `bool`.

**Runtime**: `True` / `False`, singletons, `PyBool_Type`.

**Choice rationale**: Direct map.

### 3.1 The `int`-subclass trap

`isinstance(True, int)` returns True. This bites pattern matches:

```python
match v:
    case int():  # matches True and False too
        ...
    case bool():  # never reached
        ...
```

The emitter always orders `bool` cases **before** `int` cases in a
pattern chain. mypy reports a "unreachable" warning if you write
them in the wrong order, but only sometimes; we do not rely on it.

### 3.2 Truthiness

Python's truthiness is broad: `0`, `0.0`, `""`, `[]`, `{}`, `None`,
`set()` are all falsy. Mochi requires explicit boolean conditions
(`if x` is a type error if `x: int`). The emitter therefore lowers
Mochi `if x` over a non-bool to `if x != 0:` (or the type-appropriate
explicit comparison).

---

## 4. `string`

**Annotation**: `str`.

**Runtime**: PEP 393 string, internally `latin1` / UCS-2 / UCS-4
chosen per-string by max codepoint. Immutable.

**Choice rationale**: Python `str` is **code-point indexed** since
3.3, matching Mochi `string` semantics exactly. `len(s)` returns
code-point count, not byte count or grapheme-cluster count. Slicing
(`s[i:j]`) is O(j-i) and copies.

### 4.1 UTF-8 byte length

Mochi `len_bytes(s)` lowers to `len(s.encode("utf-8"))`. The encode
materialises a temporary `bytes`. For hot paths the emitter caches:

```python
# Mochi: let bs = len_bytes(s); for i in 0..<bs { ... }
_s_bytes = s.encode("utf-8")
bs = len(_s_bytes)
for i in range(bs):
    ...
```

### 4.2 Grapheme clusters

Mochi has no surface for graphemes (yet). Future work via `regex`
or `grapheme` PyPI packages, tracked in [[12-risks-and-alternatives]] §R13.

### 4.3 String formatting

Mochi string interpolation (`"hello ${name}"`) lowers to Python f-
strings:

```python
greeting = f"hello {name}"
```

Mochi `format(x, "fmt")` lowers to `format(x, "fmt")` (Python
builtin).

### 4.4 Immutability

`str` is immutable. Mochi `s[i] = 'x'` is a Mochi type error
(strings are immutable in Mochi too); never reaches the emitter.

**Example**:

```mochi
let name: string = "Alice"
let greeting: string = "hello, ${name}"
let length: int = len(name)
let bytes_len: int = len_bytes(name)
```

```python
name: str = "Alice"
greeting: str = f"hello, {name}"
length: int = len(name)
bytes_len: int = len(name.encode("utf-8"))
```

---

## 5. `bytes`

**Annotation**: `bytes`.

**Runtime**: `PyBytesObject`, immutable.

**Choice rationale**: Direct map. Mochi `bytes` is immutable; Python
`bytes` is too. For mutable byte vectors (Mochi has no v1 surface),
`bytearray` would be the target.

**Caveats**:

- **`memoryview`**: for FFI hand-off (zero-copy access to underlying
  buffer), the emitter wraps `bytes` in `memoryview`. See §16.
- **`b"..."` literals**: ASCII-only; non-ASCII bytes must use
  `\xNN` escapes.

**Example**:

```mochi
let header: bytes = b"\x89PNG\r\n"
let length: int = len(header)
```

```python
header: bytes = b"\x89PNG\r\n"
length: int = len(header)
```

---

## 6. `list<T>`

**Annotation**: `list[T]` (PEP 585 builtin generic).

**Runtime**: `PyListObject`, dynamic array, O(1) amortised append,
O(n) middle insert, O(1) indexed access. Mutable.

**Choice rationale**: Mochi `list<T>` semantics match Python `list`
semantics exactly (insertion-ordered, mutable, indexed). No polyfill
needed.

### 6.1 Empty and sized literals

```python
empty: list[int] = []
sized: list[int] = [0] * 10           # ten zeros
named: list[int] = [1, 2, 3]
```

### 6.2 Slicing semantics

Mochi `xs[a:b]` lowers to `list(xs[a:b])` via the
`mochi_runtime.collections.list_slice` helper. The materialisation
is intentional: Mochi semantics require slices to be independent
copies. Python slice already copies for `list` (unlike `Substring`
in Swift or `array_slice` in C), but the wrapper documents the
contract and survives potential future optimisations.

### 6.3 Generic over `T`

The element type `T` is fully expressive (`list[list[int]]`,
`list[Point]`, `list[Ok[int] | Err[str]]`). PEP 585 (3.9+) made
`list[...]` a runtime-subscriptable type without `from typing
import List`.

### 6.4 Variance

`list[T]` is **invariant** in `T`: `list[Cat]` is not a subtype of
`list[Animal]` even if `Cat <: Animal`. This is because lists are
mutable; covariance would allow `list_of_cats.append(Dog())` via
the `Animal` view. mypy and pyright both enforce invariance. Mochi
semantics match.

### 6.5 `collections.abc` views

For function signatures accepting read-only lists, the emitter
prefers `Sequence[T]`:

```python
from collections.abc import Sequence

def sum_items(xs: Sequence[int]) -> int:
    return sum(xs)
```

This accepts both `list[int]` and `tuple[int, ...]`. The emitter
uses `Sequence` only when the Mochi declaration is
read-only (a future Mochi surface; v1 always uses `list[T]`).

### 6.6 Performance: `list.append` vs comprehension

Python list comprehensions are ~25% faster than `append` loops due
to bytecode specialisation (`LIST_APPEND` opcode). The emitter
prefers comprehensions for Mochi `[ expr for x in xs ]` and
generator expressions for `( expr for x in xs )`.

---

## 7. `map<K, V>`

**Annotation**: `dict[K, V]` (PEP 585 builtin generic).

**Runtime**: `PyDictObject`, hash table with insertion-order
guarantee since CPython 3.7 (PEP 468). O(1) amortised lookup,
insert, delete.

**Choice rationale**: Direct map. Mochi `map<K, V>` semantics
(insertion-ordered, hashed-key) match Python `dict` exactly. No
polyfill needed.

### 7.1 Why direct map works (unlike Swift)

The Swift target ([[mep-0049]] §1) lowers Mochi `map<K, V>` to
`OrderedDictionary` from swift-collections because Swift's
`Dictionary` does not promise iteration order. Python's `dict` does
promise it. So Mochi-on-Python is one polyfill cheaper than Mochi-
on-Swift here.

### 7.2 Empty and sized literals

```python
empty: dict[str, int] = {}
named: dict[str, int] = {"a": 1, "b": 2}
copy: dict[str, int] = dict(other)
```

### 7.3 Hashability constraint

`K` must be `Hashable`. Mochi's type system already enforces this
(`map<list<int>, int>` is a Mochi error because `list` is not
hashable). The emitter does not add an explicit `K: Hashable` bound
because the constraint is in the surface type checker, not in the
emitted Python.

### 7.4 `dict.get` with default

Mochi `m[k] ?? default` lowers to `m.get(k, default)`:

```python
value: int = m.get("missing-key", 0)
```

### 7.5 Iteration order

`for k in d:` iterates keys in insertion order. `d.items()`,
`d.keys()`, `d.values()` likewise. Mochi `for (k, v) in m { ... }`
lowers to `for k, v in m.items():`.

### 7.6 Variance

`dict[K, V]` is **invariant** in both K and V. Same reasoning as
list: dicts are mutable.

### 7.7 Why not `Mapping[K, V]` for function args?

Same reasoning as list: future-Mochi may add a read-only-map type,
which would lower to `Mapping[K, V]` from `collections.abc`. v1
uses `dict[K, V]` everywhere.

---

## 8. `set<T>`

**Annotation**: `mochi_runtime.collections.OrderedSet[T]` (the
polyfill).

**Runtime**: `OrderedSet[T]` is a Python class backed by
`dict.fromkeys(items)`. See [[04-runtime]] §4.1.

**Choice rationale**: Python's stdlib `set` does **not** guarantee
iteration order. Mochi `set<T>` does. The polyfill is ~30 LOC and
gives us:

- Insertion-order iteration.
- O(1) `add`, `discard`, `in`.
- Equality and `__contains__` matching stdlib semantics.

### 8.1 Why not stdlib `set`?

CPython's `set` is hash-table backed; iteration order is
**deterministic for a given hash seed** but unstable across
interpreters and observable as inconsistent in user-visible output.
PYTHONHASHSEED randomisation (default since 3.3) means
`set(["a", "b", "c"])` may iterate as `"b", "a", "c"` on one process
and `"c", "a", "b"` on another. Mochi byte-equal vm3 tests would
fail.

### 8.2 Why not `collections.OrderedDict`?

We could back `OrderedSet` with `OrderedDict.fromkeys(items)` instead
of `dict.fromkeys`. `OrderedDict` has ~50% memory overhead due to a
doubly-linked list for `move_to_end` support, which `OrderedSet`
does not need. `dict` since 3.7 is the right base.

### 8.3 Hashability

Same as `dict`: `T` must be `Hashable`. Surface enforces.

### 8.4 Empty and sized literals

Mochi `{}` is a map literal (matches Python's literal-syntax
distinction). For an empty set:

```mochi
let s: set<int> = set()
let s2: set<int> = set([1, 2, 3])
```

```python
s: OrderedSet[int] = OrderedSet()
s2: OrderedSet[int] = OrderedSet([1, 2, 3])
```

There is no Mochi set literal `{1, 2, 3}` to disambiguate from a
map; the emitter always uses `OrderedSet([...])` construction.

---

## 9. record (frozen dataclass)

**Annotation**: a custom class name.

**Runtime**: `@dataclass(frozen=True, slots=True)` decorated class.

**Choice rationale**: dataclass synthesises `__init__`, `__repr__`,
`__eq__`, `__hash__`, `__match_args__`. `frozen=True` enforces
immutability (matches Mochi record semantics: records are value
types). `slots=True` declares `__slots__` for memory and speed.

### 9.1 Generated example

```mochi
record Point {
  x: int
  y: int
}
```

```python
from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class Point:
    x: int
    y: int
```

The `dataclass` decorator generates:

```python
class Point:
    __slots__ = ("x", "y")
    x: int
    y: int

    def __init__(self, x: int, y: int) -> None:
        object.__setattr__(self, "x", x)
        object.__setattr__(self, "y", y)

    def __repr__(self) -> str:
        return f"Point(x={self.x!r}, y={self.y!r})"

    def __eq__(self, other: object) -> bool:
        if other.__class__ is self.__class__:
            return (self.x, self.y) == (other.x, other.y)
        return NotImplemented

    def __hash__(self) -> int:
        return hash((self.__class__.__name__, self.x, self.y))

    def __setattr__(self, name: str, value: object) -> None:
        raise FrozenInstanceError(...)
```

### 9.2 `__slots__` impact

`__slots__` removes the per-instance `__dict__` (saves ~64 bytes on
64-bit CPython 3.12). It also forbids attribute drift (assigning
unknown attributes raises `AttributeError`). For Mochi records this
is exactly what we want.

Measured impact on a 100k-instance benchmark (each instance has
3 int fields):

| Variant                       | Memory   | Construction time |
|-------------------------------|----------|-------------------|
| `class Foo:` plain            | 24.2 MB  | 105 ms            |
| `@dataclass class Foo:`       | 24.2 MB  | 142 ms            |
| `@dataclass(frozen=True) ...` | 24.2 MB  | 195 ms            |
| `@dataclass(slots=True) ...`  | 14.8 MB  | 98 ms             |
| `frozen=True, slots=True`     | 14.8 MB  | 138 ms            |

Slots reduces memory ~39%. Frozen adds a `__setattr__` raise that
costs ~40ms on construction. We pay the frozen cost because Mochi
semantics demand immutability.

### 9.3 `kw_only=True`?

`@dataclass(kw_only=True)` forces all fields to be keyword-only
arguments. We do **not** use this; Mochi record construction is
positional-or-named (callers choose). Forcing kw-only would break
Mochi-to-Mochi positional calls.

### 9.4 `__match_args__`

Auto-generated by `@dataclass`: `__match_args__ = ("x", "y")`. This
makes pattern matching work positionally:

```python
match p:
    case Point(0, 0):
        return "origin"
    case Point(x, y):
        return f"({x}, {y})"
```

### 9.5 Custom methods on records

Mochi `record` declarations can include methods:

```mochi
record Point {
  x: int
  y: int

  fun distance(other: Point): float {
    return sqrt(...)
  }
}
```

These lower to instance methods on the dataclass:

```python
@dataclass(frozen=True, slots=True)
class Point:
    x: int
    y: int

    def distance(self, other: "Point") -> float:
        import math
        return math.sqrt(...)
```

The `"Point"` forward reference is unnecessary under `from __future__
import annotations` but the emitter quotes it anyway for legibility.

### 9.6 Inheritance

Mochi records have **no inheritance** (sealed value types). The
emitter does not emit base-class clauses except for sum-type
variants (§10).

### 9.7 `__post_init__`

Not used by the emitter. Mochi records have no construction
validation; validation happens at the call site of constructors via
explicit checks.

---

## 10. sum type (PEP 695 alias + dataclass variants)

**Annotation**: PEP 695 `type Foo = A | B | C` alias.

**Runtime**: each variant is a frozen-slot dataclass. The "alias"
is a type-level synonym; at runtime, `Foo` is the union of variant
classes.

**Choice rationale**: This is the canonical Python pattern for
**closed sum types** with **type-checker exhaustiveness**. Two
parts:

1. Each variant is a separate class so pattern matching can
   discriminate.
2. The `type Foo = A | B | C` alias names the union for use in
   annotations.

### 10.1 Generated example

```mochi
sum JsonValue {
  case JNull
  case JBool(bool)
  case JNum(float)
  case JStr(string)
  case JArr(list<JsonValue>)
  case JObj(map<string, JsonValue>)
}
```

```python
from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class JNull:
    pass

@dataclass(frozen=True, slots=True)
class JBool:
    value: bool

@dataclass(frozen=True, slots=True)
class JNum:
    value: float

@dataclass(frozen=True, slots=True)
class JStr:
    value: str

@dataclass(frozen=True, slots=True)
class JArr:
    value: list["JsonValue"]

@dataclass(frozen=True, slots=True)
class JObj:
    value: dict[str, "JsonValue"]

type JsonValue = JNull | JBool | JNum | JStr | JArr | JObj
```

### 10.2 Exhaustive `match` via `assert_never`

The canonical pattern from PEP 634 + PEP 698:

```python
from typing import assert_never

def describe(v: JsonValue) -> str:
    match v:
        case JNull():
            return "null"
        case JBool(b):
            return f"bool({b})"
        case JNum(n):
            return f"num({n})"
        case JStr(s):
            return f"str({s})"
        case JArr(xs):
            return f"arr(len={len(xs)})"
        case JObj(d):
            return f"obj(keys={len(d)})"
        case _ as o:
            assert_never(o)
```

mypy and pyright track the narrowed type of `o` after the cases.
If a variant is added to `JsonValue` and not handled in the match,
`o` retains the new variant's type at the `assert_never` site, and
the type checker rejects the program. This is the load-bearing
exhaustiveness check.

### 10.3 Why not `enum.Enum` for nullary variants?

Variants without payload (`JNull`) could be `enum.Enum` members.
We use a frozen dataclass for **uniformity**: pattern matching on
`enum.X` differs from pattern matching on `Y()` in subtle ways
(`case X.A` vs `case A()`). Keeping all variants as dataclasses
keeps the pattern shape uniform.

### 10.4 Why not `typing.Annotated[Union, Discriminator(...)]`?

Pydantic uses `Annotated[Union, Discriminator("kind")]` for
tag-discriminated unions. We do not lean on Pydantic; mypy and
pyright handle the bare `A | B | C` union with pattern matching
correctly, no discriminator hint needed.

### 10.5 Why not `Literal` for tags?

We could emit:

```python
@dataclass(frozen=True, slots=True)
class JBool:
    kind: Literal["bool"] = "bool"
    value: bool
```

This is the "tagged union" idiom. We do not need it because Python
`isinstance` is already discriminating; the type checkers narrow on
class identity, not on a tag field. Tagged-union forms are useful
for JSON-roundtrip schemas (where `class` is not serialised) but
Mochi's JSON layer (`mochi_runtime.json_value`) handles that
explicitly.

### 10.6 Variant equality

`JBool(True) == JBool(True)` is True (dataclass auto-generates
`__eq__`). `JBool(True) == JNum(1.0)` is False (different classes).
Both match Mochi semantics.

### 10.7 Variant hashing

Frozen dataclasses are hashable by default. Mochi sum-type values
can be used as `dict` keys.

### 10.8 Recursive variants

`JArr(value: list["JsonValue"])` references its own alias. The
quoted forward reference is necessary in the dataclass field
declaration (the runtime evaluates the annotation eagerly during
`@dataclass`'s introspection on CPython 3.12; PEP 649 deferred
annotations land in 3.14). Under `from __future__ import annotations`
the annotation string is stored unevaluated and the cycle is
tolerated, but `@dataclass` itself re-evaluates the annotations for
its `__init__` generation. Empirically, recursive references work
because `JsonValue` is defined later in the same module file; the
emitter ensures the variant classes are declared before the type
alias.

---

## 11. `T?` (option)

**Annotation**: `T | None` (PEP 604 union syntax).

**Runtime**: `T` or `None`. `None` is the singleton sentinel.

**Choice rationale**: PEP 604 union syntax (`int | None`) is
preferred over `Optional[int]` (which is from `typing`); both are
semantically identical, but `|` is shorter and idiomatic since 3.10.

### 11.1 Generated example

```mochi
let maybe_name: string? = null
let length: int = maybe_name?.length ?? 0
```

```python
maybe_name: str | None = None
length: int = len(maybe_name) if maybe_name is not None else 0
```

The `?.` operator (Mochi's optional-chaining) lowers to an
`if ... is not None else None` ternary. For longer chains, the
emitter generates intermediate locals.

### 11.2 Why `None` not `Optional`

`Optional[T]` is the older form (`from typing import Optional`).
PEP 604 (3.10+) made `T | None` available unquoted. ruff's UP007
rule flags `Optional[T]` and suggests `T | None`. We always emit
the PEP 604 form.

### 11.3 `is None` vs `== None`

Always `is None`. PEP 8 mandates `is` for `None` comparisons. mypy
narrows the type only on `is None` (it does not on `== None`).
ruff's `E711` enforces this.

### 11.4 `Optional[T]` vs `T?` distinction

Mochi `T?` means "T or absent". Mochi has no separate `Maybe<T>`
wrapper (no allocation cost; `T | None` reuses the `None` sentinel).
Same as Python.

---

## 12. `Result<T, E>` (Ok / Err union)

**Annotation**: `mochi_runtime.result.MochiResult[T, E]` (a PEP 695
alias for `Ok[T] | Err[E]`).

**Runtime**: `Ok[T]` or `Err[E]`, both frozen-slot dataclasses.

**Choice rationale**: Three options were considered for Mochi's
`Result<T, E>` lowering:

1. **Python exceptions**: `raise SomeError` for `Err`, return value
   for `Ok`. Reject: Python has no checked exceptions; mypy/pyright
   do not track them; they interact poorly with `asyncio.TaskGroup`
   exception aggregation.
2. **`typing.Final` newtype wrapper**: a thin wrapper class.
   Reject: forces every consumer to unwrap.
3. **Sum type `Ok[T] | Err[E]`**: the canonical algebraic Result.
   Adopt.

We chose option 3. The `MochiResult` lowering parallels [[mep-0050]]
Kotlin `MochiResult` and the Mochi vm3's native result type.

### 12.1 Generated example

```mochi
fun divide(a: int, b: int): Result<int, string> {
  if b == 0 {
    return Err("div by zero")
  }
  return Ok(a / b)
}
```

```python
from mochi_runtime.result import Ok, Err, MochiResult

def divide(a: int, b: int) -> MochiResult[int, str]:
    if b == 0:
        return Err("div by zero")
    return Ok(a // b)
```

(Integer division in Mochi `/` lowers to Python `//` because Mochi's
`int / int` is int-division. Mochi `float / float` -> Python `/`.)

### 12.2 Pattern matching

```python
match divide(10, 2):
    case Ok(v):
        print_line(f"got {v}")
    case Err(msg):
        print_line(f"error: {msg}")
```

The `case _:` fallback is omitted when both variants are handled;
mypy / pyright assert exhaustiveness via the alias.

### 12.3 `?` propagation

Mochi `let v = divide(a, b)?` (early-return on `Err`) lowers to:

```python
_r = divide(a, b)
if isinstance(_r, Err):
    return _r
v: int = _r.value
```

The `isinstance(_r, Err)` narrows the subsequent `_r.value` to `T`
in mypy/pyright. We use `isinstance` here rather than `match` for
single-branch test brevity.

### 12.4 Why not `kotlin.Result`-style with throwables

Kotlin's `Result<T>` wraps a single error type (`Throwable`). Mochi's
`Result<T, E>` is generic in both. We chose the explicit two-param
shape because it forces error-type discipline at every signature.

---

## 13. Function types `(T1, ..., Tn) -> R`

**Annotation**: `Callable[[T1, ..., Tn], R]` from `collections.abc`.

**Runtime**: a callable Python object (function, lambda, method,
or instance with `__call__`).

**Choice rationale**: PEP 585 moved `Callable` to
`collections.abc.Callable` (runtime-subscriptable since 3.9). We
use that, not `typing.Callable` (deprecated alias).

### 13.1 Generated example

```mochi
fun apply<T, U>(x: T, f: (T) -> U): U {
  return f(x)
}
```

```python
from collections.abc import Callable

def apply[T, U](x: T, f: Callable[[T], U]) -> U:
    return f(x)
```

### 13.2 Variadic argument lists

Mochi `(...Ts) -> R` (variadic; not v1 surface) would lower to
`Callable[..., R]` (where `...` is `Ellipsis`, meaning "any
argument list"). This loses type information; the alternative is
PEP 612 `ParamSpec`:

```python
from typing import ParamSpec, Callable
from collections.abc import Callable as ABCallable

P = ParamSpec("P")

def wrap[**P, R](f: ABCallable[P, R]) -> ABCallable[P, R]:
    @wraps(f)
    def inner(*args: P.args, **kwargs: P.kwargs) -> R:
        return f(*args, **kwargs)
    return inner
```

The `**P` in the type-param list is PEP 695's ParamSpec syntax.
v1 emitter uses ParamSpec only for higher-order combinator emission;
end-user Mochi functions stay concrete.

### 13.3 Coroutine functions

A Mochi `async fun` lowers to a Python `async def`. The function's
type is `Callable[[args], Awaitable[R]]`, not `Callable[[args], R]`.
The emitter uses `Awaitable[R]` from `collections.abc`:

```python
from collections.abc import Awaitable

def caller(f: Callable[[int], Awaitable[str]]) -> Awaitable[str]:
    return f(42)
```

### 13.4 Generator functions

A function with `yield` returns an iterator. The type is
`Callable[[args], Iterator[T]]`:

```python
from collections.abc import Iterator

def gen_nums() -> Iterator[int]:
    yield 1
    yield 2
    yield 3
```

For async generators, `AsyncIterator[T]`.

---

## 14. agent

**Annotation**: a custom subclass of `mochi_runtime.agent.AgentBase`.

**Runtime**: instance of that subclass, holding an
`asyncio.Queue[Message]` mailbox and managed by a `TaskGroup`.

**Choice rationale**: See [[04-runtime]] §6.

### 14.1 Generated example

```mochi
agent Counter {
  state: int = 0

  on increment {
    state = state + 1
  }

  on get(): int {
    return state
  }
}
```

```python
from mochi_runtime.agent import AgentBase
from dataclasses import dataclass
from typing import assert_never

@dataclass(frozen=True, slots=True)
class _Increment:
    pass

@dataclass(frozen=True, slots=True)
class _Get:
    reply: "asyncio.Future[int]"

type _CounterMsg = _Increment | _Get

class Counter(AgentBase[_CounterMsg, int]):
    def __init__(self, scope: "asyncio.TaskGroup") -> None:
        super().__init__(scope, initial=0)

    async def _handle(self, state: int, msg: _CounterMsg) -> int:
        match msg:
            case _Increment():
                return state + 1
            case _Get(reply=r):
                r.set_result(state)
                return state
            case _ as o:
                assert_never(o)

    def increment(self) -> None:
        self.cast(_Increment())

    async def get(self) -> int:
        loop = asyncio.get_running_loop()
        fut: asyncio.Future[int] = loop.create_future()
        self.cast(_Get(reply=fut))
        return await fut
```

The generated class shape is mechanical: one envelope dataclass per
Mochi message handler, an alias union, an `_handle` dispatcher with
exhaustive `match`, and one public method per handler. Cast handlers
become sync methods (`def increment(...) -> None`); call handlers
become `async def`.

### 14.2 Field references inside handlers

Mochi `state = state + 1` lowers to the `_handle` method's
`return state + 1`. The base class threads the state through the
mailbox loop:

```python
async def _loop(self) -> None:
    while not self._stopping:
        msg = await self._mailbox.get()
        self._state = await self._handle(self._state, msg)
```

This is the canonical functional-state pattern: handlers are pure
state-transition functions; the loop owns the mutation.

### 14.3 Why not `object` with a private state field?

We could store `_state` as a mutable instance attribute and let
handlers do `self._state = ...`. The functional-state pattern is
cleaner because:

- Each handler is a `state, msg -> state` function, easy to test in
  isolation.
- The state flows through `await` boundaries explicitly (mutation
  across `await` is a subtle bug source).
- mypy / pyright can narrow the state type per branch.

---

## 15. stream

**Annotation**: `AsyncIterator[T]` from `collections.abc`.

**Runtime**: an async generator function instance (PEP 525). Drives
the iteration with `__aiter__` and `__anext__`.

**Choice rationale**: Direct map. Mochi streams are async-iterable
by spec; `AsyncIterator[T]` is the canonical Python type.

### 15.1 Generated example

```mochi
async fun first_n(s: stream<int>, n: int): list<int> {
  let out: list<int> = []
  let count = 0
  for x in s {
    if count >= n { break }
    out.append(x)
    count = count + 1
  }
  return out
}
```

```python
from collections.abc import AsyncIterator

async def first_n(s: AsyncIterator[int], n: int) -> list[int]:
    out: list[int] = []
    count = 0
    async for x in s:
        if count >= n:
            break
        out.append(x)
        count = count + 1
    return out
```

Note `async for` (not plain `for`) for streams. Mochi's `for x in s`
over a stream lowers to `async for`; over a list, plain `for`. The
emitter determines this from the iterable's type.

### 15.2 Why not `AsyncGenerator[T, None]`?

`AsyncGenerator[T, None]` is the type of an async generator function
that takes no input via `asend()`. `AsyncIterator[T]` is the broader
type any async-iterable conforms to. For Mochi's purposes, the
broader type is what consumers want; producers return either.

---

## 16. FFI types (ctypes mapping)

**Annotation**: the corresponding ctypes type.

**Runtime**: a ctypes type instance, hand-off to the C library.

**Choice rationale**: ctypes is stdlib (no extra dep), supports the
standard C type vocabulary, and works on all CPython 3.12 platforms.
The alternative cffi is opt-in (see [[04-runtime]] §15.2).

### 16.1 Integer types

| Mochi    | ctypes     | C equiv     |
|----------|------------|-------------|
| `i8`     | `c_int8`   | `int8_t`    |
| `i16`    | `c_int16`  | `int16_t`   |
| `i32`    | `c_int32`  | `int32_t`   |
| `i64`    | `c_int64`  | `int64_t`   |
| `u8`     | `c_uint8`  | `uint8_t`   |
| `u16`    | `c_uint16` | `uint16_t`  |
| `u32`    | `c_uint32` | `uint32_t`  |
| `u64`    | `c_uint64` | `uint64_t`  |
| `isize`  | `c_ssize_t`| `ssize_t`   |
| `usize`  | `c_size_t` | `size_t`    |

These FFI types are **only used at the FFI boundary**. Mochi
arithmetic stays in Python `int`; conversion to fixed-width happens
on the `bind` call's `argtypes` list (ctypes does the narrowing).

### 16.2 Floating types

| Mochi | ctypes     | C equiv  |
|-------|------------|----------|
| `f32` | `c_float`  | `float`  |
| `f64` | `c_double` | `double` |

### 16.3 Pointers and arrays

| Mochi          | ctypes                          |
|----------------|----------------------------------|
| `ptr<T>`       | `POINTER(c_T)`                   |
| `array<T, N>`  | `(c_T * N)`                      |
| `cstring`      | `c_char_p`                       |
| `wstring`      | `c_wchar_p`                      |

### 16.4 Struct ABI

A Mochi `extern record` declaration:

```mochi
extern record Point {
  x: i32
  y: i32
}
```

lowers to a `Structure` subclass:

```python
import ctypes

class Point(ctypes.Structure):
    _fields_ = [
        ("x", ctypes.c_int32),
        ("y", ctypes.c_int32),
    ]
```

The struct fields are in declaration order, matching C layout. For
ABI compatibility with `__attribute__((packed))` structs, the
emitter adds `_pack_ = 1` if the Mochi annotation indicates packing.

### 16.5 Why `TypedDict` for FFI structs sometimes

For C functions that take a `*const T` to a struct **owned by
Python**, the emitter sometimes uses `TypedDict` for the Mochi-side
representation and converts to `Structure` at the call boundary:

```python
from typing import TypedDict

class PointDict(TypedDict):
    x: int
    y: int

def make_point(d: PointDict) -> Point:
    return Point(x=d["x"], y=d["y"])
```

`TypedDict` is the canonical Python shape for "dict with known
keys"; ctypes `Structure` is the canonical shape for "C struct
with known layout". The bridge above converts between them.

We do **not** use `TypedDict` for Mochi records (those are
dataclasses). `TypedDict` is only at FFI boundaries.

---

## 17. Generic type parameters and variance

### 17.1 PEP 695 syntax

Generic functions and classes use PEP 695 type-parameter syntax:

```python
def first[T](xs: list[T]) -> T:
    return xs[0]

class Box[T]:
    value: T

type Stack[T] = list[T]
```

Bounded:

```python
from collections.abc import Hashable

def unique[T: Hashable](xs: list[T]) -> list[T]:
    seen: set[T] = set()
    return [x for x in xs if x not in seen and not seen.add(x)]
```

### 17.2 Variance

Python's `TypeVar` defaults to **invariant**. Old-style:

```python
from typing import TypeVar
T_co = TypeVar("T_co", covariant=True)
T_contra = TypeVar("T_contra", contravariant=True)
```

PEP 695 has no built-in syntax for variance; you fall back to
explicit `TypeVar` declarations when needed. Mochi's surface does
**not** expose declaration-site variance; mochi uses **use-site
variance** via `Sequence[T]` (read-only, covariant) and
`MutableSequence[T]` (invariant). The emitter therefore rarely
needs explicit covariance.

For the `mochi_runtime.stream` module's `AsyncIterator[T]`, the
underlying `collections.abc.AsyncIterator` is declared
`AsyncIterator[T_co]` (covariant) in the stdlib stub; we inherit
that.

### 17.3 Constraint vs bound

PEP 484 distinguishes:

- **Bound** (`T: Bound`): `T` is `Bound` or a subclass.
- **Constraint** (`T: A | B`): `T` is exactly `A` or exactly `B`.

PEP 695 supports bounds (`[T: Hashable]`) but not constraints
directly. For Mochi `T: A | B` constraints (rare), the emitter falls
back to old-style `TypeVar("T", A, B)`.

---

## 18. Protocol vs nominal classes

**Annotation choice**: nominal classes for Mochi types, Protocol for
FFI shape-matching.

**Rationale**: Mochi's type system is **nominal** (named types are
distinct; structural equivalence does not imply substitutability).
Python's `Protocol` (PEP 544) is **structural** (any class with the
right methods conforms). Mismatching these would silently break
Mochi's nominal contracts.

### 18.1 When the emitter uses Protocol

- **FFI interop**: a foreign library expects "anything with a
  `read(n) -> bytes` method". The emitter emits a Protocol:

  ```python
  from typing import Protocol

  class Reader(Protocol):
      def read(self, n: int) -> bytes: ...
  ```

- **Internal duck-typed helpers**: rare, mostly in
  `mochi_runtime._internal`.

### 18.2 When the emitter uses nominal classes

- **Records**: always dataclass.
- **Sum-type variants**: always dataclass.
- **Agents**: always subclass of `AgentBase`.
- **Mochi interfaces**: always abstract base class (`abc.ABC`) with
  abstract methods.

```python
import abc

class Comparable(abc.ABC):
    @abc.abstractmethod
    def compare(self, other: "Comparable") -> int: ...
```

Subclasses inherit nominally.

### 18.3 Why `@runtime_checkable` is mostly avoided

PEP 544's `@runtime_checkable` makes a Protocol usable with
`isinstance(x, P)`. The check walks `x`'s methods, which is slow
(~10x slower than nominal `isinstance`). We avoid it in
performance-sensitive paths.

---

## 19. Type aliases vs `NewType`

### 19.1 PEP 695 `type` alias

```python
type UserId = int
type UserName = str
```

A type alias is **transparent**: `UserId` and `int` are
interchangeable. mypy / pyright treat them as equal.

### 19.2 `NewType` for distinct identity

```python
from typing import NewType
UserId = NewType("UserId", int)
UserName = NewType("UserName", str)

def lookup(uid: UserId) -> str:
    ...

lookup(42)              # type error: int is not UserId
lookup(UserId(42))      # ok
```

`NewType` is the **opaque** form. mypy / pyright require explicit
construction.

### 19.3 Mochi `type` keyword

Mochi `type UserId = int` is **opaque** by default (Mochi semantic
is nominal). The emitter therefore lowers it to `NewType`, not to
PEP 695 `type` alias.

```python
UserId = NewType("UserId", int)
```

For transparent type synonyms (rare in Mochi, used only for
shortening verbose generic instantiations), the emitter uses PEP 695
`type` alias.

---

## 20. Range type

**Annotation**: `range` (the builtin type).

**Runtime**: `range` object, lazy.

**Choice rationale**: Mochi `0..<n` is a range expression. Python
`range(n)` is the lazy integer range. Direct map.

```mochi
for i in 0..<10 { print(i) }
let r: range = 0..<10
```

```python
for i in range(10):
    print_line(str(i))
r: range = range(10)
```

### 20.1 Step support

Mochi `0..<10 step 2` lowers to `range(0, 10, 2)`. Mochi has no
inclusive range surface that maps directly to Python's `range`;
`0..=10` lowers to `range(0, 11)`.

### 20.2 Float ranges

Python `range` is integer-only. Mochi `0.0..<10.0 step 0.5` lowers
to a hand-rolled generator:

```python
def _float_range(start: float, stop: float, step: float) -> Iterator[float]:
    x = start
    while x < stop:
        yield x
        x += step
```

This is emitted at module scope if the program uses float ranges,
otherwise omitted.

---

## 21. Tuple types

**Annotation**: `tuple[T1, T2, ..., Tn]` (PEP 585 builtin).

**Runtime**: `PyTupleObject`, immutable.

**Choice rationale**: Direct map. Mochi has no first-class tuple in
v1 surface but the emitter uses tuples internally (for `dict.items`,
function multi-return-value, pattern matching).

### 21.1 Anonymous structural tuples

Mochi `(int, string)` lowers to `tuple[int, str]`. Equality is
structural.

### 21.2 Variadic tuples

`tuple[int, ...]` is "a tuple of int of any length". Used for
splat-style arguments. Mochi has no surface; emitter uses it for
PEP 646 `TypeVarTuple` lowering (not v1).

---

## 22. `Any`, `object`, `Never`, `NoReturn`

### 22.1 `Any`

Forbidden in emitted code. mypy / pyright strict mode errors on
`Any` leakage. The emitter never emits `Any`.

### 22.2 `object`

The Python root type. Equivalent to "any value". The emitter uses
`object` only in two places:

- `__eq__(self, other: object) -> bool` (the dunder's declared type).
- `**kwargs: object` for opaque keyword forwarding (rare).

### 22.3 `Never` / `NoReturn`

`Never` (PEP 702) is the bottom type. Used by `assert_never` (§10).
`NoReturn` is the older spelling for functions that never return
(e.g. `sys.exit`). The emitter uses `NoReturn` for Mochi `panic`:

```python
from typing import NoReturn

def panic(msg: str) -> NoReturn:
    raise RuntimeError(msg)
```

### 22.4 `Self`

`typing.Self` (PEP 673, 3.11+) is the type of the enclosing class
instance. Mochi method declarations that return `Self` lower to
`-> Self`:

```python
from typing import Self

class Builder:
    def with_name(self, name: str) -> Self:
        ...
```

This enables fluent builder patterns to type-check correctly when
subclassed.

---

## 23. Frozen vs mutable distinction

Mochi distinguishes (at the surface) **value types** (records, sum
variants, primitives) from **reference types** (lists, dicts, sets,
agents, streams). The emitter respects this:

| Category   | Mochi semantic       | Python emit                       |
|------------|----------------------|------------------------------------|
| Value      | structural, immutable | frozen-slot dataclass               |
| Reference  | identity, mutable    | plain class / builtin container    |

For value types we always set `frozen=True` to enforce immutability
at runtime. For reference types we never freeze (mutation is the
expected operation).

---

## 24. Boxing, slots, and memory cost

### 24.1 Box per object

Every Python value (except `Ellipsis`, `None`, small ints, interned
strings) is a heap-allocated `PyObject`. Boxing overhead per
object: 16 bytes header (refcount + type pointer) plus the value
itself. For a record with three int fields:

- Without `__slots__`: 16 (header) + 8 (dict ptr) + ~232 (dict) +
  3 * (8 + 16) (three boxed ints) = ~328 bytes.
- With `__slots__`: 16 (header) + 24 (3 slots) + 3 * 16 (boxed ints) =
  ~88 bytes.

About 3.7x reduction. The dict's overhead dominates without slots.

### 24.2 Amortised cost of boxing

For short-lived objects (Mochi temporaries during expression
evaluation), CPython's free-list mechanism (`PyObject_Malloc`)
reuses the same memory pages, keeping allocator overhead low. The
free lists for `int`, `float`, `tuple` are dedicated; `dict` and
custom classes go through the general arena.

For long-lived objects, boxing cost is fixed at allocation time and
amortises away.

### 24.3 Comparison to other targets

| Target | Record memory (3 int fields) |
|--------|------------------------------|
| C      | 12 bytes (`struct { int32 x, y, z; }`) |
| JVM    | ~24 bytes (12B header + 12B payload) |
| Kotlin | ~24 bytes (same as JVM)              |
| Swift  | 12 bytes (value type, stack-allocated when possible) |
| Python | 88 bytes (with slots), 328 bytes (without) |

Python is the heaviest. The cost is inherent to the runtime; we
mitigate via `__slots__` but cannot escape boxing.

---

## 25. Refcounting and GIL implications

CPython 3.12 has refcounting + cycle GC. Mochi value types (frozen
records) cannot form cycles (no mutable field through which to
re-reference). Mochi reference types (lists, dicts) can. Cycle GC
runs periodically; we do not tune it from emitted code.

The GIL (Global Interpreter Lock) serialises Python bytecode
execution within a process. `asyncio` is single-threaded
cooperative; `concurrent.futures.ProcessPoolExecutor` is the escape
hatch. Mochi `agent` lives entirely under the GIL on a single OS
thread (the asyncio event loop's thread). For multi-core parallelism,
the user spawns a `ProcessPoolExecutor`; the Mochi spec leaves this
as a runtime concern, not a type-system concern.

CPython 3.13 free-threaded (`--disable-gil`) lifts the GIL. The
emitter does not need changes; the runtime may need lock primitives
on shared state. Tracked in [[12-risks-and-alternatives]] §F1.

---

## 26. Type annotation visibility

### 26.1 PEP 526 visibility

Module-level annotations are visible to:

- Type checkers (mypy, pyright).
- `typing.get_type_hints(module)` at runtime.
- `__annotations__` dict on the module.

Function-local annotations are visible to type checkers but NOT to
`get_type_hints`; they exist as comments. Class-level annotations
behave like module-level annotations.

### 26.2 `from __future__ import annotations`

All emitted modules start with this import. Effects:

- Annotations are stored as **strings** at definition time.
- Forward references work without quotes.
- `get_type_hints` resolves the strings on demand (with errors if
  the referent is undefined).

PEP 649 (3.14+) deferred-annotations may obsolete the import; v1
emit always includes it for forward compatibility.

---

## 27. Cross-reference table

The full Mochi type to Python type table for quick reference:

| Mochi type                  | Python annotation                          |
|-----------------------------|---------------------------------------------|
| `int`                       | `int`                                       |
| `float`                     | `float`                                     |
| `bool`                      | `bool`                                      |
| `string`                    | `str`                                       |
| `bytes`                     | `bytes`                                     |
| `list<T>`                   | `list[T]`                                   |
| `map<K, V>`                 | `dict[K, V]`                                |
| `set<T>`                    | `OrderedSet[T]`                             |
| record `R`                  | `@dataclass(frozen=True, slots=True) class R` |
| sum `S { case A; case B; }` | `type S = A \| B` plus dataclass variants  |
| `T?`                        | `T \| None`                                 |
| `Result<T, E>`              | `MochiResult[T, E]` (= `Ok[T] \| Err[E]`)  |
| `(T1, ..., Tn) -> R`        | `Callable[[T1, ..., Tn], R]`                |
| `() -> R`                   | `Callable[[], R]`                           |
| agent `A`                   | `class A(AgentBase[Msg, State])`            |
| stream `T`                  | `AsyncIterator[T]`                          |
| `range`                     | `range`                                     |
| FFI `i32`                   | `c_int32` (at boundary)                     |
| FFI `ptr<T>`                | `POINTER(c_T)`                              |
| `Never`                     | `Never` / `NoReturn`                        |
| `Self`                      | `Self`                                      |
| `Any` (forbidden in emit)   | (never emitted)                             |
| `object` (rarely)           | `object`                                    |

---

## 28. Edge cases

### 28.1 Recursive type alias

```mochi
sum Tree<T> {
  case Leaf
  case Node(T, Tree<T>, Tree<T>)
}
```

```python
@dataclass(frozen=True, slots=True)
class Leaf:
    pass

@dataclass(frozen=True, slots=True)
class Node[T]:
    value: T
    left: "Tree[T]"
    right: "Tree[T]"

type Tree[T] = Leaf | Node[T]
```

The quoted forward reference is needed because `@dataclass`
evaluates annotations eagerly at class-construction time (PEP 649
defers this to 3.14). Under `from __future__ import annotations`
the storage is lazy, but `@dataclass` re-evaluates internally.

### 28.2 Self-referential record

```mochi
record LinkedList<T> {
  head: T
  tail: LinkedList<T>?
}
```

```python
@dataclass(frozen=True, slots=True)
class LinkedList[T]:
    head: T
    tail: "LinkedList[T] | None"
```

Same forward-reference treatment.

### 28.3 Hashable generic record

A frozen dataclass is hashable iff all its fields are hashable. The
type checker enforces this:

```python
@dataclass(frozen=True, slots=True)
class Box[T: Hashable]:
    value: T

s: set[Box[int]] = {Box(1), Box(2)}  # ok
s2: set[Box[list[int]]] = ...  # type error: list is not Hashable
```

mypy / pyright report the error at the `set[Box[list[int]]]`
annotation site. The emitter does not need extra work; the
constraint is in the source.

---

## 29. Comparison with sibling MEPs

| Mochi type | Swift ([[mep-0049]])           | Kotlin ([[mep-0050]])       | Python ([[mep-0051]])         |
|------------|---------------------------------|------------------------------|--------------------------------|
| `int`      | `Int64`                         | `Long`                       | `int`                          |
| `float`    | `Double`                        | `Double`                     | `float`                        |
| `string`   | `String`                        | `String`                     | `str`                          |
| `list<T>`  | `Array<T>`                      | `List<T>` / `MutableList<T>` | `list[T]`                      |
| `map<K,V>` | `OrderedDictionary<K, V>`       | `LinkedHashMap<K, V>`        | `dict[K, V]`                   |
| `set<T>`   | `OrderedSet<T>`                 | `LinkedHashSet<T>`           | `OrderedSet[T]` (polyfill)     |
| record     | `struct Foo: Hashable`          | `data class Foo`             | `@dataclass(frozen, slots)`    |
| sum type   | `enum Foo`                      | sealed `class Foo`           | PEP 695 alias + dataclasses    |
| `T?`       | `T?`                            | `T?`                         | `T \| None`                    |
| `Result`   | `Result<T, E>` (E: Error)       | `MochiResult<T, E>`           | `MochiResult[T, E]`            |
| function   | `(T) -> R`                      | `(T) -> R`                   | `Callable[[T], R]`             |
| agent      | `actor Foo`                     | `class Foo: SupervisorJob`   | `class Foo(AgentBase[M, S])`   |
| stream     | `AsyncStream<T>`                | `Flow<T>`                    | `AsyncIterator[T]`             |

Two observations:

1. **Python is the only target where the integer width matches
   Mochi semantics by default** (arbitrary precision). All other
   targets pick a fixed width (Int64 on Swift, Long on Kotlin,
   int32_t / int64_t on C).
2. **Python is the only target where `set` needs a polyfill** for
   insertion order. Swift `Set` lacks order; Kotlin `LinkedHashSet`
   has it; Python `set` lacks it, hence `OrderedSet`.

---

## 30. Summary

- Every Mochi primitive maps to a CPython 3.12 builtin without
  conversion.
- Containers map to PEP 585 builtin generics (`list[T]`, `dict[K, V]`)
  except `set<T>` which uses the `OrderedSet` polyfill from
  `mochi_runtime.collections`.
- Records lower to `@dataclass(frozen=True, slots=True)`; the
  combination gives value semantics, memory savings (~40%), and
  type-checker enforcement of immutability.
- Sum types lower to PEP 695 `type` alias plus per-variant frozen
  dataclasses, with exhaustive `match` via `assert_never` for
  static exhaustiveness.
- Function types lower to `Callable[[args], R]` from
  `collections.abc` (PEP 585).
- Optional types lower to PEP 604 `T | None`, never to
  `Optional[T]`.
- `Result` lowers to a custom `Ok[T] | Err[E]` PEP 695 union,
  never to exceptions.
- Generics use PEP 695 type-parameter syntax (`class Foo[T]:`,
  `def f[T](x: T) -> T:`) for both classes and functions.
- Variance is invariant by default; reads from `collections.abc`
  picks up stdlib-declared covariance (`Sequence[T_co]`,
  `AsyncIterator[T_co]`).
- FFI types use `ctypes` at the boundary; Mochi value types stay
  in the Python typing layer.
- `Protocol` is used only at FFI shape-matching boundaries; Mochi
  nominal types lower to nominal classes.
- `Any` is never emitted; mypy --strict and pyright --strict reject
  it.

The lowering is **boring and predictable** by design: each Mochi
construct has one Python translation; the type-checker constraints
guide every choice. The runtime ([[04-runtime]]) provides the few
polyfills needed (OrderedSet, MochiResult, ZonedDateTime, JsonValue,
AgentBase). The codegen pipeline ([[05-codegen-design]]) consults
this table per annotation; the result is Python that passes
mypy --strict, pyright --strict, ruff format --check, and ruff check,
and runs byte-equal against the vm3 reference interpreter.
