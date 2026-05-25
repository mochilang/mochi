---
title: "MEP-51 research note 01, Mochi language surface (Python target)"
description: "Per-feature lowering obligation for the Mochi-to-Python transpiler. Maps every Mochi surface construct to its lowered CPython 3.12+ form, with mypy --strict and pyright --strict gate considerations."
---

# MEP-51 research note 01, Mochi language surface (Python target)

Author: research pass for MEP-51 (Mochi to Python transpiler).
Date: 2026-05-23 (GMT+7).
Sources: `docs/features/*.md`, `docs/index.md`, `docs/common-language-errors.md`,
`mcp/cheatsheet.mochi`, `ROADMAP.md`, `examples/v0.2`-`v0.7`, the normative
security specs `docs/security/threat-model.md` and `docs/security/memory-safety.md`,
PEPs 484, 526, 544, 585, 604, 612, 621, 654, 669, 692, 695, 698, 703,
the CPython 3.12 release notes, the Python typing council notes on
github.com/python/typing, mypy 1.13 and pyright 1.1.380+ release notes,
uv 0.4+ release notes, and the companion MEP-45 note 01 (C target),
MEP-46 note 01 (Erlang/BEAM), MEP-47 note 01 (JVM), MEP-48 note 01
(.NET), MEP-49 note 01 (Swift), MEP-50 note 01 (Kotlin), whose section
structure this note deliberately mirrors so all seven backends can be
diffed line for line.

This note records the user-visible language surface that the Python
target must faithfully reproduce. It is written *from the spec downward*
and ignores the existing Go runtime (vm3), the vm3 bytecode, the C
target under MEP-45, the Erlang/BEAM target under MEP-46, the JVM target
under MEP-47, the .NET target under MEP-48, the Swift target under
MEP-49, the Kotlin target under MEP-50, and any other backend
implementation. The goal is a transpiler design that would be correct
against the *language*, not against the present implementations.

The surface decomposes into the same eight orthogonal sub-languages
identified in the prior notes: (1) the value core, (2) the function and
method core, (3) the collection core, (4) the algebraic-data-type core,
(5) the query DSL, (6) the stream and agent core, (7) the
logic-programming core, and (8) the AI and FFI shells. Each section
below names every form a Mochi program can write, then states a
*lowering obligation* the Python backend must honour.

Where MEP-45 maps Mochi types to C struct plus helper-function pairs,
MEP-46 maps them to BEAM terms (atoms, tagged tuples, maps, binaries,
funs, PIDs), MEP-47 maps them to JVM values directly via bytecode,
MEP-48 maps them to .NET values, MEP-49 maps them to Swift values
(`Int64`, `Double`, structs, enums with associated values, actors,
`AsyncStream`), MEP-50 maps them to Kotlin values (`Long`, `Double`,
`data class`, `sealed interface`, custom actor with `Channel<Message>`,
`Flow<T>`), this note maps them to Python values: arbitrary-precision
`int`, IEEE-754 `float`, `bool`, `str` (PEP 393 variable-width internal
storage in CPython, len in code points), `bytes`, `list[T]`, `dict[K, V]`,
`set[T]` (with `dict.fromkeys` ordering when insertion order matters),
frozen-slots `dataclass` for records, PEP 695 `type` alias plus dataclass
variants for sum types, `T | None` for option types, custom
`MochiResult[T, E]` sealed-by-convention for error results, custom
agent class wrapping `asyncio.Queue[Message]` plus `asyncio.TaskGroup`
supervision, `AsyncIterator[T]` for streams, and `Callable[..., R]` for
function types. The target IR is discussed in note 05 (the default path
emits Python source via `libcst` builders, then a `ast.unparse` /
`black` fixed-point pass for formatting); the runtime is the Python
standard library plus a thin `mochi_runtime` package (see note 04).

Throughout, "Python" means **CPython 3.12.0** (released 2023-10-02) and
later. CPython 3.11 (TaskGroup, exception groups, tomllib) is *not* the
floor because PEP 695 type parameter syntax (`type Foo[T] = ...`,
`class Box[T]:`, `def first[T](xs: list[T]) -> T:`) is 3.12-only and
critical to Mochi's generic lowering. CPython 3.13 (Oct 2024) brings
PEP 703 `--disable-gil` builds and PEP 667 cleaner frame locals; those
are forward notes (see [[12-risks-and-alternatives]]) not gates. PyPy,
Cython, mypyc, Nuitka, GraalPy, Pyodide, MicroPython, and IronPython are
all explicitly out of scope for v1 (see [[02-design-philosophy]] §10 for
the rejection register and [[03-prior-art-transpilers]] for the survey
of why each was considered and dropped).

## 1. Value core

### 1.1 Bindings

Mochi has exactly two binding forms.

- `let name = expr`, immutable. Re-assignment is a **compile-time**
  error, not a runtime panic. Python has no language-level `let`/`const`
  distinction; every name binding is rebindable at the language level.
  We use **PEP 591 `Final`** annotations to declare immutability at the
  type-checker layer. The Mochi-emitted form is:
  ```python
  from typing import Final
  name: Final[int] = expr
  ```
  Both `mypy --strict` and `pyright --strict` enforce `Final` (since
  mypy 0.770 and pyright 1.1.40 respectively); a rebinding of a `Final`
  name is a type error caught at our build gate. At runtime there is
  no enforcement (CPython does not honour `Final`), but the gate
  guarantees no Mochi-emitted code can violate it without a build
  failure. See [[02-design-philosophy]] §3 for the case for the
  type-checker-as-language-guard.

- `var name = expr`, mutable. Re-assignment is unrestricted within the
  variable's lexical scope. Emitted as a plain PEP 526 variable
  annotation:
  ```python
  name: int = expr
  ```
  No `Final`. Subsequent assignments are accepted by both type checkers.

Mochi blocks are expressions in the sense that the last expression is
the block's value. Python statements are not expressions; `if`, `while`,
`for`, and `match` do not yield values. The backend lowers block-valued
constructs in one of three ways: (a) for a one-line conditional, into a
Python conditional expression `a if cond else b`; (b) for a more complex
block, into a `def __block_n() -> T:` helper function returning the
block value, called immediately; (c) for sum-type matches with no side
effects in arms, into a `match` statement with assignment of the result
inside each arm. See note 05 §6 for the full block-lowering table.

A binding may carry an explicit type: `let x: int = 0` becomes
`x: Final[int] = 0`. Note that PEP 484 type comments
(`x = 0  # type: int`) are deprecated since Python 3.6 in favour of
variable annotations (PEP 526); we never emit type comments.

Mochi supports destructuring at `let`:

```mochi
let [a, b] = [1, 2]
let {"name": n, "age": age} = {"name": "Ana", "age": 22}
```

Python 3.10 introduced **structural pattern matching** (PEP 634, 635,
636) via the `match`/`case` statement. We do *not* use it for plain
destructuring assignments because `match` is a statement and we want an
expression-equivalent. Instead we lower destructuring via direct
indexing with explicit length checks:

```python
__tmp: list[int] = [1, 2]
assert len(__tmp) == 2, f"MochiPatternError: arity expected 2, got {len(__tmp)}"
a: Final[int] = __tmp[0]
b: Final[int] = __tmp[1]
```

For map destructuring with explicit keys:

```python
__tmp: dict[str, object] = {"name": "Ana", "age": 22}
n: Final[str] = __tmp["name"]
age: Final[int] = __tmp["age"]
```

The `object` typing for `__tmp` is a deliberate punt: when the
destructured map has heterogeneous value types, Python's `TypedDict`
(PEP 589) is the precise tool but adds emit complexity. For now we
emit `object` plus a `cast(int, ...)` at the use site; see note 06 §7
for the TypedDict-vs-cast tradeoff and the v2 plan to emit TypedDict.

For record types the backend uses keyword access since dataclasses do
not destructure by position:

```python
n: Final[str] = person.name
age: Final[int] = person.age
```

Scoping is lexical and block-based. Inner blocks shadow outer bindings.
Python's scoping is function-local, not block-local: a `for x in ...`
loop variable lives past the loop, `if cond: x = 1` binds `x` in the
enclosing function scope. This is a hazard for Mochi codegen that
assumes block-local shadowing. The lowering pass renames Mochi-level
inner-block bindings to mangled names (`x__1`, `x__2`) when an outer
binding would otherwise be shadowed by a Mochi-level inner block.
See note 05 §11 for the rename strategy.

Reserved-word collisions are handled with a trailing underscore: Mochi
`class` becomes Python `class_`, Mochi `def` becomes Python `def_`,
Mochi `lambda` becomes Python `lambda_`. The full list (35 Python 3.12
keywords plus soft keywords `match`, `case`, `type`) is in §1.6 and
note 06 §2.

### 1.2 Primitive types

Surfaced by the docs and the cheatsheet, with the Python-side
representation:

| Mochi | Width / semantics | Python lowering |
|-------|-------------------|-----------------|
| `int` | 64-bit signed integer (inferred from integer literals) | `int` (arbitrary precision; matches Mochi at lower bound, exceeds at upper bound) |
| `float` | 64-bit IEEE 754 double | `float` (IEEE 754 double) |
| `bool` | `true` / `false` | `bool` (note: `bool` is a subclass of `int` in Python, so `True + 1 == 2`; we never rely on this) |
| `string` | UTF-8 text, indexable as code points, immutable | `str` (PEP 393 variable-width internal: latin-1 / UCS-2 / UCS-4 chosen per string; `len(s)` is code-point count) |
| `bytes` | immutable byte sequence | `bytes` |
| `time` | absolute timestamp | `datetime.datetime` with `tzinfo=datetime.UTC` (3.11+) |
| `duration` | time interval | `datetime.timedelta` |
| `image` (preview) | binary blob | `bytes` wrapped in a `MochiImage` dataclass |

Python's `int` is **arbitrary precision** since Python 3 (PEP 237).
Mochi's `int` is documented as 64-bit signed, so every Mochi program
that fits in 64 bits also fits in Python's `int`. The reverse is not
true: a Python `int` value of `2**100` is legal Python but illegal
Mochi. The backend therefore inserts a runtime guard on FFI boundaries
that ingest Python `int` values (the `mochi_runtime.int_check(x)`
helper raises `MochiValueError` if `x` exceeds the signed-64-bit
range). For pure Mochi code, no guard is needed because Mochi's own
type checker rejects values that would overflow at compile time. See
[[02-design-philosophy]] §16 on the type-wall.

Implicit numeric conversions are **not** allowed (per the type-checker
discipline implied by MEP-4/5/6 referenced from the threat model).
`int + float` is a Mochi type error; the program must `float(x)` first.
Python's `int + float` evaluates to `float` via implicit promotion
(`1 + 1.0 == 2.0`), and the Python runtime does not refuse the
operation. Mypy and pyright catch the mismatch when the operands have
distinct declared types, but the Mochi-emitted code never *attempts*
the mixed expression: the Mochi checker rejects it before lowering.
Both type checkers also flag `int -> float` widening as a type narrowing
(or a `NoReturn`-style narrowing in some cases); we avoid this by
emitting explicit `float(x)` at every Mochi-level coercion point.

Integer overflow in Python **does not occur** because `int` is
arbitrary precision; arithmetic that would overflow in C / Kotlin /
Swift instead allocates a larger integer. Mochi's documented semantic
is two's-complement wrap-around on 64-bit. The default lowering
emits ordinary `+`, `-`, `*` operators and inherits Python's
arbitrary-precision semantics, **breaking strict Mochi compatibility
on overflow**. This is a deliberate choice: most Mochi programs do
not depend on overflow wrap-around (and arguably overflow is a bug
when it does happen). For security-sensitive builds, the
`--strict-int` flag emits `mochi_runtime.add_wrap(a, b)` style helpers
that mask to 64 bits explicitly. Off by default, on for builds that
opt into the audit profile. See note 06 §5.

### 1.3 Operators

Arithmetic `+ - * / %`; comparison `== != < <= > >=`; boolean
`&& || !`; membership `in`; string concatenation overloads `+`.

| Mochi | Python |
|-------|--------|
| `a + b` (int)    | `a + b` (arbitrary precision, no overflow) |
| `a + b` (float)  | `a + b` (IEEE NaN propagates) |
| `a + b` (string) | `a + b` (Python `str + str` returns a fresh `str`; CPython optimises `s = s + t` chains via the in-place refcount=1 optimisation in `ceval.c`) |
| `a + b` (list)   | `a + b` (Python `list + list` returns a fresh `list`; for mutation prefer `xs.extend(ys)`) |
| `a - b` | `a - b` |
| `a * b` | `a * b` |
| `a / b` (float) | `a / b` (true division returns `float` since Python 3) |
| `a / b` (int)   | `a // b` (floor division; Python's `/` on `int` returns `float` per PEP 238, which we don't want) |
| `a % b` | `a % b` (Python's `%` is **floor remainder** by default, matching Mochi's documented semantic; this is the opposite of C and Java) |
| `a == b` (primitive) | `a == b` (Python's `==` calls `__eq__`; for `int`, `float`, `bool`, this is value equality) |
| `a == b` (dataclass) | `a == b` (frozen dataclass synthesises `__eq__` field-by-field) |
| `a != b` | `a != b` |
| `a < b`, `<=`, `>`, `>=` | numeric: native; string: `a < b` uses code-point ordering directly (Python `str < str` compares by code-point sequence, matching Mochi's specified semantic exactly, unlike Kotlin's UTF-16 code-unit order or Java's surrogate-pair-broken order) |
| `a && b` | `a and b` (Python's `and` is short-circuit; returns the first falsy operand or the last operand, not always a bool; we emit `bool(...)` wrapping when the result is consumed as bool) |
| `a \|\| b` | `a or b` |
| `!a` | `not a` |
| `x in xs` (list) | `x in xs` (calls `__contains__`) |
| `x in m` (map) | `x in m` (Python's `in` on a dict tests keys, matching Mochi) |
| `x in s` (set) | `x in s` |

Python's `and` / `or` return one of the operands (not always a `bool`).
For example, `1 and 2 == 2`, `0 or 3 == 3`. Mochi's `&&` / `||` always
return `bool`. The mismatch only matters when the result is bound to a
non-bool variable (which Mochi rejects at type-check time anyway). The
backend never has to coerce because Mochi's type checker already
constrained the result type to `bool` upstream.

The lowering must respect Python's separation between `==` (value
equality via `__eq__`) and `is` (object identity). Mochi has no
reference identity, so the backend never emits `is` in user code
*except* for the canonical idioms `is None`, `is True`, `is False`
(which both PEP 8 and pyright's `--strict` mode require: comparing to
`None` with `==` is a code-smell and pyright will lint it).

### 1.4 Strings as read-only code-point sequences

```mochi
let text = "hello"
print(text[1])     // "e"
for ch in text { ... }
```

Indexing yields a 1-character string (not a code point integer).
Iteration yields 1-character strings in **code-point** order. Python's
`str` is the *cleanest* fit among all seven backends because:

- CPython's `str` is **PEP 393 variable-width** internally: each string
  picks the smallest of latin-1 (1 byte/char), UCS-2 (2 bytes/char), or
  UCS-4 (4 bytes/char) based on the maximum code point. This means
  `len(s)` returns the **code-point count** directly (not UTF-16 code
  units like Kotlin or surrogate-pair-tracked code units like Java),
  matching Mochi's specified semantic.
- `s[i]` returns a single-character `str` containing the `i`-th code
  point. Mochi semantics match exactly.
- Iteration `for ch in s` yields single-character `str` values in
  code-point order. Mochi semantics match exactly.

Concretely:

- `text[i]` lowers to `text[i]`. The Mochi spec says single-character
  string return; Python does exactly this. No runtime helper required.
- `for ch in text` lowers to `for ch in text:`. Identical semantics.
- `len(text)` lowers to `len(text)`. Identical semantics (code-point
  count).

This is the area where the Python target is *cheapest* relative to the
Kotlin and Swift targets: on Kotlin every code-point operation pays a
UTF-16 walk, and on pre-Swift-5.7 Swift the same applies. CPython's
PEP 393 representation gives O(1) random access by code point and O(1)
length. The cost paid by CPython is a one-time allocation of a wider
buffer when a new high code point is added (a rare event for ASCII-
heavy text). See [[02-design-philosophy]] §6 on the PEP 393 cost
analysis.

UTF-8 encoding boundaries (for HTTP, JSON, file I/O) use
`text.encode("utf-8")` and `bytes_obj.decode("utf-8")`. Both are O(n)
one-pass conversions. The CPython 3.12 `str.encode()` was optimised in
3.12 to use SIMD on x86-64 when the string is ASCII-only, reducing
encode time by 30-50% for that case.

### 1.5 Literals

Integer literals; floating literals (`3.14`); boolean (`True`/`False`);
string with C-style escapes; triple-quoted multi-line strings
(`"""..."""`); list `[...]`; map `{key: val, ...}`; set `{a, b, c}`;
record constructor `T { field: val }`.

The set literal `{a, b, c}` is distinguished from the empty/map literal
`{}` by the absence of `:` after the first element. The grammar must
keep these unambiguous; the Python lowering picks the right
constructor accordingly. Note that Python's `{}` is an **empty dict**,
not an empty set; the empty set is `set()`. The backend emits `set()`
for an empty Mochi set.

Lowering forms:

| Mochi | Python |
|-------|--------|
| `42` | `42` (Python's `int` literal, arbitrary precision) |
| `3.14` | `3.14` (Python's `float` literal, IEEE 754 double) |
| `true` / `false` | `True` / `False` (Python capitalises bool literals) |
| `"hello"` | `"hello"` |
| `[1, 2, 3]` | `[1, 2, 3]` (Python list literal, mutable) |
| `"""multi\nline"""` | `"""multi\nline"""` (Python triple-quoted; Mochi's multiline rules match Python's verbatim) |
| `{"a": 1, "b": 2}` | `{"a": 1, "b": 2}` (Python dict literal; insertion order guaranteed since 3.7 per PEP 468) |
| `{1, 2, 3}` (set) | `{1, 2, 3}` (Python set literal; **not** insertion-ordered; see below) |
| `Book { title: "X", pages: 10 }` | `Book(title="X", pages=10)` (dataclass call with keyword arguments) |

Python set literals (and `set()` instances) are **not insertion-
ordered** (unlike `dict`, where insertion order has been guaranteed
since 3.7). Mochi's `set<T>` is documented as insertion-ordered. The
backend handles this by either:

(a) using `dict.fromkeys([1, 2, 3]).keys()` as the set representation
when insertion order matters, which is the canonical Python idiom for
an insertion-ordered set; or

(b) using a thin `MochiOrderedSet[T]` wrapper class in `mochi_runtime`
that internally uses a `dict[T, None]` for ordering plus the `set` ABC
for membership/operators.

We pick (b) for the default lowering because it provides correct
typing (an explicit class with `__contains__`, `__iter__`, `__len__`,
`__or__`, `__and__`, `__sub__`) and avoids the `.keys()` view-vs-set
type confusion that mypy and pyright would otherwise flag. See note
06 §10 for the `MochiOrderedSet` implementation.

Python collection literals create *mutable* containers by default,
matching Mochi semantics. For immutable copies the runtime exposes
`tuple(...)`, `frozenset(...)`, and `types.MappingProxyType(...)`;
Mochi-emitted code uses these only on demand (e.g., for hashable
record fields).

The frozen-slots dataclass form is the default record representation:

```python
from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class Book:
    title: str
    pages: int
```

`slots=True` was added to `dataclasses.dataclass` in Python 3.10; it
sets `__slots__` automatically, reducing memory per instance and
preventing accidental attribute addition. `frozen=True` raises
`FrozenInstanceError` on attribute assignment, matching Mochi's
value-semantic immutability default. See §4 for the full ADT
discussion.

### 1.6 Identifier mangling

Python identifiers may begin with letter or `_` and continue with
letter / digit / `_`. Mochi identifiers are stricter, so every Mochi
identifier is a legal Python identifier *until* it collides with a
Python keyword or soft keyword. Python's reserved words (35 hard
keywords as of 3.12 plus soft keywords `match`, `case`, `type`) are
unambiguous; collisions are resolved with a trailing underscore (the
PEP 8 recommendation):

| Mochi name | Python name (after mangling) |
|------------|------------------------------|
| `class`    | `class_`                     |
| `def`      | `def_`                       |
| `lambda`   | `lambda_`                    |
| `import`   | `import_`                    |
| `from`     | `from_`                      |
| `return`   | `return_`                    |
| `yield`    | `yield_`                     |
| `async`    | `async_`                     |
| `await`    | `await_`                     |
| `match`    | `match_`                     |
| `case`     | `case_`                      |
| `type`     | `type_`                      |

Mochi variables that collide with a Python built-in (`list`, `dict`,
`set`, `tuple`, `int`, `float`, `str`, `bool`, `bytes`, `range`, `len`,
`print`, `id`, `type`, `input`, `open`) are *not* mangled at the
language level, because Python allows shadowing of built-ins (with a
ruff/pyright warning). To preserve the warning-free build, the
backend mangles those too: Mochi `list` becomes Python `list_`, etc.
The full list (Python 3.12 has 152 built-in names per `__builtins__`)
is in note 06 §3.

Mochi package paths `mathutils/extra` produce Python module
`mochi_user.mathutils.extra` for user code (configurable via
`--python-package-prefix`; default `mochi_user`). The `mochi_user`
segment makes the runtime / user distinction visible in stack traces
and in `pyproject.toml` dependency graphs. Each Mochi source file
becomes one Python module; Mochi packages become Python packages with
`__init__.py` files that re-export from the module files.

Mochi record type names become Python `dataclass` names in PascalCase,
unchanged (`Book` becomes `Book`). On collision with a `typing` or
`builtins` symbol (e.g., `Type`, `Iterator`, `Callable`, `Any`), the
backend renames `Type` to `Type_` and emits a file-scope alias only
when the type really is internal.

Mochi sum-type variant constructors become Python dataclass classes
inside a `type Foo = A | B | C` PEP 695 type alias (PascalCase
preserved). Field labels are preserved verbatim. See §4 ADT lowering.

The mangling is deterministic (note 05 §3) and reversible via Python
line comments (`# mochi:source file.mochi:line`) which the emitter
writes for every Mochi source line. Python's `linecache` and
`traceback` modules consult the line comments only via the
`__file__`/`__doc__` mechanism, so debugger stack traces continue to
point at the emitted Python file. A future enhancement (note 10 §15)
will emit Python `co_qualname` / `co_filename` metadata that points
back to the Mochi source file for traceback display.

### 1.7 Optionality

Mochi has no `null` at the language level. Optional values are
expressed via the `Option<T>` sum type. The Python lowering uses
**Python's native `T | None`** representation (PEP 604 union syntax,
since Python 3.10) for Mochi `Option<T>`. This is the same choice the
Swift target (MEP-49) and Kotlin target (MEP-50) made, and the
opposite of the .NET target (MEP-48) which emits its own `Option<T>`
discriminated union to dodge `Nullable<T>` boxing problems.

The decision: the Python target **uses Python's native `T | None` for
Mochi `Option<T>`**. The reasoning:

- Python's `T | None` is the idiomatic Pythonic optional. Every Python
  developer reads it as "Maybe T". Forcing a wrapper `Option[T]` would
  produce non-idiomatic code that fails the "Python code a human
  would write" gate.
- Both type checkers handle `T | None` natively, narrowing via
  `is None` / `is not None` guards. Mochi's `match Option { Some(x) =>
  ..., None => ... }` lowers to Python `if x is not None: ...; else:
  ...` with mypy / pyright narrowing the types correctly.
- The `Optional[T]` alias from `typing` (still available in 3.12) is
  legacy form; `T | None` is the modern equivalent and both checkers
  treat them identically. We always emit `T | None`.
- PEP 604 union syntax requires `from __future__ import annotations`
  for forward-reference safety in pre-3.10 modules, but since we floor
  at 3.12, we use the syntax directly without the `__future__` import
  for unions (see [[02-design-philosophy]] §15 for the
  `from __future__ import annotations` discussion, which we do still
  emit for cyclic-reference safety).
- Mochi `Some(x)` becomes Python `x` (the implicit wrap when
  assigning to `T | None`), Mochi `None` becomes Python `None`.

The Mochi `Result<T, E>` type, by contrast, is **not** mapped to a
single-type `T | E` union, because that conflates success and failure
when `T` and `E` overlap (e.g., `Result<int, int>`). The backend emits
a custom `MochiResult[T, E]` shape, discussed in §4 and §12.

For lowering pattern matches that consume Mochi `Option`:

```mochi
match opt {
  Some(x) => x + 1
  None    => 0
}
```

Lowers to a Python conditional expression or `if`/`else`:

```python
result: int = (opt + 1) if opt is not None else 0
```

For multi-line arms with statements, the lowering uses an `if` /
`else` block. For pure expression arms, the conditional expression
is preferred (it's a single Python expression, type-checks cleanly,
and produces less generated code).

At the FFI boundary, any value coming in from Python code that is
typed as `T | None` is funnelled through Mochi `Option<T>`; values
typed as `T` (non-optional) bypass the wrapper. Both type checkers
enforce this distinction statically through their narrowing analysis,
so no runtime check is required for pure-Python code paths.

## 2. Function and method core

### 2.1 Top-level functions

```mochi
fun add(a: int, b: int): int { return a + b }
```

Lowers to a top-level Python function with PEP 526 annotations:

```python
def add(a: int, b: int) -> int:
    return a + b
```

Every Mochi source file produces one Python module file named after
the source file (`example.mochi` becomes `example.py`) declaring all
top-level functions, top-level `Final` bindings, and any helper
dataclasses at module scope. The module exports the public surface via
`__all__` (the Python idiom for explicit re-export control); the
backend computes `__all__` from the set of Mochi top-level
declarations that are not file-private.

Python modules execute their top level at import time. For Mochi
top-level expressions that have side effects (e.g., `let cache = compute_cache()`),
the backend wraps the computation in a `def __init_module__() -> None:`
that runs at import time. For Mochi top-level pure declarations,
direct assignment is fine.

The reason we use module-level top-level declarations and not a
wrapping class for the module namespace is that Python's modules are
the canonical namespace unit, and they reduce the nesting depth in
generated code by one level compared to the
`class Example: ...` form. This is the Python equivalent of Swift's
"namespacing enum" idiom or Kotlin's top-level declarations.

Mochi `return` is explicit. The Python lowering preserves `return`
directly: `return e;` becomes Python `return e`. Python supports
implicit `return None` at the end of a `def`; we always emit an
explicit `return None` at the end of a void-returning function for
mypy --strict compatibility (mypy 1.13+ flags missing explicit returns
in functions with declared `-> None` return type only weakly, but our
codegen always emits the explicit form).

The docs warn there is **no implicit tail-call optimisation** in
Mochi. CPython does not perform TCO either (Guido has rejected the
feature multiple times; see his 2009 blog post "Tail Recursion
Elimination" for the reasoning). Mochi-emitted Python code that
recurses deeply will hit the default 1000-frame Python recursion
limit and raise `RecursionError`. The backend detects deep recursion
patterns at the Mochi IR level (note 05 §15) and emits a trampoline
helper when the recursion depth can statically exceed the limit.
The trampoline uses an iterative loop and a stack of work items,
preserving Mochi semantics without the Python recursion-limit hazard.

PEP 695 generic functions are supported since Python 3.12:

```mochi
fun first<T>(xs: list<T>): T { return xs[0] }
```

Lowers to:

```python
def first[T](xs: list[T]) -> T:
    return xs[0]
```

The PEP 695 `[T]` syntax is preferred over the older `TypeVar("T")` +
`Generic[T]` form because it (a) requires no `from typing import
TypeVar` import, (b) scopes `T` to the function (avoiding the
type-var-collision hazard in older code), and (c) gives both mypy and
pyright cleaner inference (PEP 695 sealed the variance and constraint
rules definitively, which the older `TypeVar` form had subtle holes
in). The PEP 695 form requires Python 3.12+; we already floor there.

### 2.2 First-class function values

```mochi
let square = fun(x: int): int => x * x
fun apply(f: fun(int): int, value: int): int { return f(value) }
```

Lower to Python callable types and lambdas:

```python
from collections.abc import Callable
from typing import Final

square: Final[Callable[[int], int]] = lambda x: x * x

def apply(f: Callable[[int], int], value: int) -> int:
    return f(value)
```

Python lambda expressions are limited to a single expression body
(no statements). For multi-statement Mochi closures, the lowering
emits a named nested function instead of a lambda:

```python
def apply_complex() -> int:
    def __closure_1(x: int) -> int:
        y = x * 2
        return y + 1
    return __closure_1(5)
```

The `collections.abc.Callable` form is preferred over `typing.Callable`
since Python 3.9 (PEP 585 generics in standard collections). The two
are aliases, but PEP 585 expressed the preference for the
`collections.abc` form. The Mochi codegen always emits the modern
form.

For closures that must be invoked from an async context (i.e., that
may `await`), the function is `async def` and the callable type is
`Callable[..., Awaitable[R]]`:

```python
async def process(x: int) -> int:
    await asyncio.sleep(0.01)
    return x * x
```

The Mochi `async` keyword maps directly to Python's `async def`. The
two type systems agree: calling an `async def` from a non-async
context produces a coroutine object that must be `await`ed; mypy and
pyright flag the missing `await` at type-check time. See [[02-design-philosophy]]
§12 on the coroutine model.

Closures escape freely; captured variables in Python are captured by
**name** (not by value), which is the classic "loop variable
capture" trap. The backend emits Mochi captures explicitly via
default-argument binding when the Mochi semantics require value
capture inside a loop:

```python
# Mochi: for i in range(3) { fns.append(fun() => i) }
fns = []
for i in range(3):
    def __closure_i(i_captured: int = i) -> int:
        return i_captured
    fns.append(__closure_i)
```

Without the `i_captured=i` default, all three closures would return
`2` (the final value of `i`) when called, which is the well-known
Python late-binding bug. The codegen detects loop-local captures via
the Mochi IR and emits the default-argument idiom automatically.
See note 05 §16 for the capture policy.

### 2.3 Methods on type blocks

```mochi
type Circle {
  radius: float
  fun area(): float { return 3.14 * radius * radius }
}
```

A method receives an implicit `self`; field names inside the block are
unqualified. Lowering: the record is a Python `@dataclass(frozen=True,
slots=True)` and the method is an instance method on that class:

```python
from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class Circle:
    radius: float

    def area(self) -> float:
        return 3.14 * self.radius * self.radius
```

Python's `@dataclass` decorator (PEP 557, since Python 3.7) synthesises
`__init__`, `__repr__`, `__eq__`, and (with `frozen=True`)
`__hash__` automatically. With `slots=True` (since 3.10), `__slots__`
is also synthesised. The combination gives a record type that is
memory-efficient (no per-instance `__dict__`), immutable (no field
assignment), hashable (usable as dict key or set member), and
debugger-friendly (`__repr__` shows all fields).

Python methods take an explicit `self` parameter as the first
argument; field access is qualified (`self.radius`, never just
`radius`). The backend rewrites Mochi unqualified field references
inside methods to `self.<field>` during lowering.

For records that need to participate in `total_ordering` (sortable),
the backend emits `@dataclass(order=True, ...)`. For records that need
JSON serialization, the backend emits `mochi_runtime.json.serialize`
calls that introspect the dataclass via `dataclasses.fields(obj)`,
rather than depending on Pydantic or msgspec. See [[02-design-philosophy]]
§7 on the choice of frozen-slots dataclass over Pydantic, NamedTuple,
attrs, and msgspec.

For records with mutable fields (Mochi `var` field), the lowering
removes `frozen=True` from the dataclass decorator and keeps
`slots=True`:

```python
@dataclass(slots=True)
class Counter:
    count: int
```

Mochi's value-semantics contract on records (records are copied by
value when assigned or passed to a function) is preserved by:

(a) the default `frozen=True` immutability for records without `var`
fields, which lets aliasing be safe;

(b) explicit `dataclasses.replace(obj, field=new_val)` calls at every
Mochi mutation site for `var` records, which preserves value
semantics by producing a fresh instance per mutation.

See note 06 §4.

### 2.4 Built-in `print`

Variadic, prints with default formatting and inserts spaces (cheatsheet:
`print("name = ", name, ...)`); newline at end.

Lowers to Python's built-in `print(*args)` directly:

```python
print("name =", name, ...)
```

Python's built-in `print` is already variadic with space separators by
default (`sep=" "`) and newline termination (`end="\n"`). Mochi's
print semantics match Python's exactly, with one wrinkle: Mochi's
list and map `__repr__` should match Mochi's documented format, not
Python's. The lowering uses `mochi_runtime.io.print` instead of bare
`print`:

```python
from mochi_runtime.io import print as mochi_print

mochi_print("name =", name)
```

The `mochi_runtime.io.print` helper formats lists, maps, sets, and
records according to Mochi's `__repr__` conventions (e.g., `[1, 2, 3]`
for a list, `{a: 1, b: 2}` for a map with unquoted keys when the keys
are valid Mochi identifiers, `Book { title: "X", pages: 10 }` for a
record). Python's default `__repr__` would produce slightly different
output (`[1, 2, 3]` for a list, `{'a': 1, 'b': 2}` for a dict with
quoted keys, `Book(title='X', pages=10)` for a dataclass), so the
helper centralises the format.

See note 04 §3 for the `mochi_runtime.io` module.

## 3. Collection core

Three primitive containers, all with structural typing:

- `list<T>`, ordered, growable. Lowers to Python `list[T]` (always
  PEP 585 lowercase form, never `typing.List[T]`).
- `map<K, V>`, keyed lookup, with insertion-order iteration. Lowers
  to Python `dict[K, V]` (insertion order guaranteed since 3.7 per
  PEP 468).
- `set<T>`, unique members, with insertion-order iteration in Mochi
  semantics. Lowers to `MochiOrderedSet[T]` from `mochi_runtime`, **not**
  to Python's built-in `set` (which is not insertion-ordered).

Lowering obligations (full per-type details in note 06 §1):

- `list<T>` is the workhorse. Python `list` is an array-backed
  resizable container with O(1) append (amortised), O(1) random access,
  and O(n) insertion at the head. Element storage is **boxed** for
  every Python object including `int`, `float`, `bool` (Python has no
  unboxed primitive arrays in the language; the `array` module
  provides unboxed numeric arrays as a stdlib option, and NumPy
  provides them as a third-party option). This is the biggest single
  performance disadvantage of the Python target compared to the C, Go,
  and Native-Swift targets: `list[int]` always boxes. For the
  performance-sensitive cases, Mochi exposes `intlist<int>` /
  `floatlist<float>` types that lower to Python `array.array('q', ...)`
  (signed-64 unboxed) / `array.array('d', ...)` (double unboxed),
  giving compact memory and fast iteration when interfacing with C
  extensions. The default `list<T>` lowering uses the boxed form; the
  typed-array form is opt-in.

- `map<K, V>` defaults to Python `dict[K, V]`. Mochi's iteration order
  is insertion order. Python's `dict` has guaranteed insertion-order
  iteration since 3.7 (PEP 468 for kwargs, generalised to all dicts in
  3.7). The default lowering is therefore already insertion-ordered
  without a wrapper. Hash-based key lookup is amortised O(1); ordered
  iteration is O(n).

- `set<T>` is `MochiOrderedSet[T]` (a `dict[T, None]`-backed wrapper
  in `mochi_runtime`). The query layer (§5) needs the *insertion-
  ordered* semantic for `union` / `except` to be deterministic.
  Python's built-in `set` is hash-ordered (which is to say, no order
  contract); using it directly would produce nondeterministic stdout
  on `for x in s` iteration.

- All collections are **value-semantically copied** at language level.
  Python's reference types do not provide this for free (every
  collection is heap-allocated and aliased on assignment), so the
  backend emits explicit defensive copies at function-call boundaries.
  The VM enhancement spec 0951 §1 ("each function call must allocate a
  fresh copy of any list/map literal") is satisfied by emitting
  `arg.copy()` or `list(arg)` / `dict(arg)` / `MochiOrderedSet(arg)`
  at every callsite where a collection is passed to a function. The
  cost is one O(n) copy per call; for hot loops the user can opt into
  the `--no-defensive-copy` flag to disable (at the cost of giving up
  the value-semantics contract). This is the largest performance cost
  of the Python target relative to immutable-by-default targets like
  Swift. See [[02-design-philosophy]] §7.

Mutation operations (`xs.add(x)`, `m[k] = v`) lower to direct Python
mutating method calls (`xs.append(x)`, `m[k] = v`) when the target is
a `var` binding. For `let` bindings, the lowering emits a
copy-and-mutate via the `mochi_runtime.collections` module which
exposes Mochi-shaped helpers (`appended(xs, x)`, `inserting(xs, i, x)`,
`removing(xs, i)`, `updating(m, k, v)`) that take the collection,
return a fresh mutated copy, and rebind. See note 06 §11.

`for x in xs` lowers to a Python `for ... in` loop:

```python
for x in xs:
    ...
```

For maps, `for (k, v) in m` lowers to:

```python
for k, v in m.items():
    ...
```

Python's `dict.items()` returns a `dict_items` view that yields
`(key, value)` tuples in insertion order. The Mochi semantic matches
exactly.

### 3.1 List-of-records

A common Mochi pattern is a `list<Record>` for dataset-shaped data.
The Python lowering uses `list[Record]` directly:

```python
@dataclass(frozen=True, slots=True)
class Person:
    name: str
    age: int

people: Final[list[Person]] = [
    Person(name="Ana", age=22),
    Person(name="Ben", age=30),
]
```

For very large datasets (millions of rows), the Python target offers
two opt-in alternatives: (a) `array.array` of primitive types when all
fields are numeric, and (b) Apache Arrow tables via `pyarrow` when
columnar layout is needed for analytics workloads. Both are opt-in via
`@dataset` annotations on the Mochi record type. See note 08 for the
data pipeline lowering.

## 4. Algebraic data type core

Mochi's sum-of-products data types (`type Tree = Leaf | Node { ... }`)
lower to a combination of PEP 695 `type` alias (3.12+) plus
dataclass-defined variants. There is no Python equivalent of Swift's
`enum` with associated values, Kotlin's `sealed interface`, or Rust's
`enum`. The closest is the union of dataclasses, with exhaustive
matching enforced by the type checker.

```mochi
type Tree =
  | Leaf
  | Node { value: int, left: Tree, right: Tree }
```

Lowers to:

```python
from __future__ import annotations
from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class Leaf:
    pass

@dataclass(frozen=True, slots=True)
class Node:
    value: int
    left: Tree
    right: Tree

type Tree = Leaf | Node
```

The PEP 695 `type Tree = Leaf | Node` declaration (3.12+) creates a
type alias that both mypy and pyright recognise as a discriminated
union. The Python `match` statement (PEP 634, 3.10+) provides
exhaustive matching when the type checker can prove all variants are
covered. mypy 1.13+ and pyright 1.1.380+ both enforce exhaustiveness on
union-of-dataclass matches when the type alias is declared with the
PEP 695 syntax. See [[02-design-philosophy]] §5 on the PEP 695-vs-
Union choice.

The `from __future__ import annotations` import is **mandatory** at
the top of every Mochi-generated module because dataclass field types
that reference other module-level dataclasses (the `left: Tree`,
`right: Tree` self-reference here) would otherwise produce a
`NameError` at class-definition time. The `__future__` import defers
all annotation evaluation to string form, which dataclasses resolve
lazily via `typing.get_type_hints()`. See [[02-design-philosophy]] §15
for the full discussion.

Field labels on the case payload (`value`, `left`, `right`) are
preserved from the Mochi syntax. Construction is `Node(value=42,
left=Leaf(), right=Leaf())`; the dataclass constructor takes keyword
arguments matching the field names.

Pattern matching:

```mochi
match t {
  Leaf => 0
  Node { value, left, right } => value + sum(left) + sum(right)
}
```

Lowers to a Python `match` statement with class patterns:

```python
match t:
    case Leaf():
        result = 0
    case Node(value=value, left=left, right=right):
        result = value + sum_(left) + sum_(right)
```

Python's `match` statement is an expression-equivalent only via the
nested-assignment trick (assigning `result` inside each arm). For
single-expression returns from match, the lowering emits a helper
function that returns from each arm:

```python
def _match_t(t: Tree) -> int:
    match t:
        case Leaf():
            return 0
        case Node(value=value, left=left, right=right):
            return value + sum_(left) + sum_(right)
    raise AssertionError("unreachable")  # for mypy/pyright exhaustiveness
```

The trailing `raise AssertionError("unreachable")` is added when mypy
or pyright cannot prove the match is exhaustive. PEP 695 type aliases
help here: both checkers prove exhaustiveness when the matched value
has type `Tree` and every variant of `Tree` is covered. For
non-PEP-695 alternatives the unreachable guard is required. See note
05 §10.

Class pattern field destructuring uses Python's `case ClassName(field=name)`
syntax. Positional patterns (`case Node(v, l, r)`) require
`__match_args__` to be set on the class; `@dataclass` sets it
automatically since 3.10. Keyword patterns work for all dataclasses
since 3.10. The backend prefers keyword patterns for readability.

Guarded patterns:

```mochi
match shape {
  Circle { radius } if radius > 10.0 => "big"
  Circle { radius }                  => "small"
  _                                  => "other"
}
```

Lower to Python `match` with `if` guards:

```python
match shape:
    case Circle(radius=radius) if radius > 10.0:
        result = "big"
    case Circle(radius=radius):
        result = "small"
    case _:
        result = "other"
```

Python 3.10's PEP 634 supports guard clauses on match arms via the
`case ... if cond:` syntax. The semantics match Mochi exactly: the
arm matches only when both the structural pattern matches and the
guard evaluates truthy. If the guard fails, the next case is tried.

Generic ADTs:

```mochi
type Option<T> = | Some { value: T } | None
type Result<T, E> = | Ok { value: T } | Err { error: E }
```

Mochi `Option<T>` lowers to Python `T | None` (per §1.7), no custom
type emitted. For other generic sums:

```python
from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class Ok[T]:
    value: T

@dataclass(frozen=True, slots=True)
class Err[E]:
    error: E

type MochiResult[T, E] = Ok[T] | Err[E]
```

The PEP 695 `[T]` and `[E]` generic parameters on the dataclasses give
both mypy and pyright clean inference. The PEP 695 `type` alias
combines them into a single union. The backend emits the user sealed
union (always named `MochiResult[T, E]` to avoid collision with
external Python libraries that may name a type `Result`).

For exhaustive matching on `MochiResult`:

```python
match result:
    case Ok(value=v):
        ...
    case Err(error=e):
        ...
```

Both mypy 1.13+ and pyright 1.1.380+ prove exhaustiveness on the PEP
695 alias when both variants are covered, so no `case _:` fallback is
required.

See [[12-risks-and-alternatives]] for the typed-throw interaction and
the rationale for the Result-not-exceptions design.

## 5. Query DSL

Mochi's query DSL (`from x in xs select { ... }`, with `join`, `group
by`, `order by`, `limit`, `offset`, `where`) is the densest sub-language.
Its Python lowering uses Python's **generator expressions** as the
primary IR (for synchronous finite collections) plus
`itertools.groupby`, `sorted`, and the `mochi_runtime.query` helpers
for the operations Python's stdlib does not directly support. For
streaming queries, the lowering uses `AsyncIterator[T]` chains. Full
lowering is in note 08; this section names the surface forms only.

```mochi
let adults =
  from p in people
  where p.age >= 18
  order by p.name
  select { name: p.name, age: p.age }
```

Lowers to:

```python
adults: Final[list[AdultRow]] = sorted(
    (
        AdultRow(name=p.name, age=p.age)
        for p in people
        if p.age >= 18
    ),
    key=lambda r: r.name,
)
```

The `(...)` generator expression keeps intermediate stages lazy.
Python's `sorted(iterable, key=...)` materialises the result. The
materialisation is the natural terminus of the pipeline.

`group by` lowers to a helper from `mochi_runtime.query` (Python's
`itertools.groupby` requires the iterable to be already sorted by the
group key, which is not what Mochi's `group by` means):

```python
from mochi_runtime.query import group_by

by_dept: dict[str, list[Person]] = group_by(people, key=lambda p: p.dept)
```

The `group_by` helper returns a `dict[K, list[V]]` preserving insertion
order (the order in which keys first appeared), matching Mochi's
documented semantic. See note 08.

`join` lowers to a hash-join helper in `mochi_runtime.query` (since
Python's stdlib has no built-in cross-collection join). `limit` /
`offset` are `itertools.islice` (for lazy) or list slicing (for
materialised).

Important: the **Mochi `stream<T>` type and Python's generator
expression are not the same thing**. Generator expressions are
synchronous and (in the lazy sense) on-demand-iterable; Mochi's
`stream<T>` is a *time-evolving* asynchronous publisher (closer to
`AsyncIterator[T]` from `collections.abc`). The query DSL uses
generator expressions for *finite synchronous* collection queries; the
public `stream<T>` type uses `AsyncIterator[T]`. See note 09 for the
agent / stream lowering and [[02-design-philosophy]] §13 for the
type-level distinction.

## 6. Stream and agent core

```mochi
stream Tick = { time: time }

agent ticker {
  every 1s emit Tick { time: now() }
}
```

Streams lower to Python `AsyncIterator[T]` from `collections.abc`
(formerly `typing.AsyncIterator`; the PEP 585 form is preferred). The
implementation is an `async def` generator function that yields
values:

```python
from collections.abc import AsyncIterator
import asyncio

async def ticker() -> AsyncIterator[Tick]:
    while True:
        yield Tick(time=now())
        await asyncio.sleep(1.0)
```

Agents lower to a custom class wrapping an `asyncio.Queue[Message]`
plus an `asyncio.TaskGroup`-supervised receiver task. Python's standard
library has no `actor` builder (asyncio is a coroutine library, not an
actor library). The canonical agent shape since Python 3.11
(`TaskGroup` arrived in 3.11 per PEP 654 family) is a custom class with
a private queue and a TaskGroup-launched receive loop. See [[02-design-philosophy]]
§8.

```python
from __future__ import annotations
import asyncio
from dataclasses import dataclass, replace

@dataclass(frozen=True, slots=True)
class Tick:
    time: float

@dataclass(frozen=True, slots=True)
class Message:
    payload: Tick

class Ticker:
    def __init__(self, tg: asyncio.TaskGroup) -> None:
        self._mailbox: asyncio.Queue[Message] = asyncio.Queue()
        self._task: asyncio.Task[None] = tg.create_task(self._loop())

    async def _loop(self) -> None:
        while True:
            msg = await self._mailbox.get()
            await self._handle(msg)

    def cast(self, msg: Message) -> None:
        self._mailbox.put_nowait(msg)

    async def call(self, msg: Message) -> Tick:
        fut: asyncio.Future[Tick] = asyncio.get_running_loop().create_future()
        msg_with_reply = replace(msg, reply=fut)
        await self._mailbox.put(msg_with_reply)
        return await fut

    def stop(self) -> None:
        self._task.cancel()

    async def _handle(self, msg: Message) -> None:
        # dispatch to per-message handler
        ...
```

Key design points:

- The mailbox is an `asyncio.Queue[Message]` with default-unbounded
  capacity (Python's `asyncio.Queue(maxsize=0)` is unbounded). Mochi's
  `bounded(N)` qualifier lowers to `asyncio.Queue(maxsize=N)` with
  `put()` suspending when full.
- The actor's main loop is `while True: msg = await self._mailbox.get(); ...`.
  When the queue is closed (Python's `asyncio.Queue` doesn't have a
  built-in close primitive; we use `None` as a sentinel or cancel the
  task), the loop terminates. The `Ticker.stop()` method cancels the
  task directly via `self._task.cancel()`.
- `cast(msg)` is `self._mailbox.put_nowait(msg)`, returning immediately
  for fire-and-forget. If the queue is bounded and full, this raises
  `asyncio.QueueFull`; the backend emits a try/except wrapping that
  converts to `MochiQueueFull` per the Mochi error contract.
- `call(msg)` creates a future, attaches it to the message payload,
  enqueues the message, and awaits the future. The receiver fills
  the future when it processes the message. This is the canonical
  request-reply pattern for asyncio actors.
- Spawning child tasks is `tg.create_task(coro)`. The TaskGroup is
  passed in by the caller and represents the supervision scope. The
  TaskGroup's `__aexit__` waits for all children to complete and
  propagates failures as an `ExceptionGroup` (PEP 654).
- Cancellation propagates via `TaskGroup.cancel_scope` (no equivalent
  in Python; cancellation is via `task.cancel()` plus the TaskGroup's
  automatic cancellation of siblings on a single failure). Cooperative
  cancellation checkpoints are at every `await`; long-running loops
  add `await asyncio.sleep(0)` to allow cancellation delivery. See
  the shared-decisions anchor §"Concurrency model".

Supervision *is* built into TaskGroup's PEP 654 ExceptionGroup model
in a one-for-all style: if any child task raises, the TaskGroup
cancels all sibling tasks and raises an `ExceptionGroup` aggregating
all failures. This matches OTP's `one_for_all` strategy. For
`one_for_one` (restart only the failed agent), the lowering uses a
nested TaskGroup per agent with a `try/except` around the
`async with` body:

```python
async with asyncio.TaskGroup() as tg:
    while True:
        try:
            async with asyncio.TaskGroup() as inner:
                inner.create_task(agent._loop())
                await agent_complete
        except* AgentError as eg:
            # log eg.exceptions and restart
            ...
```

The `except*` syntax (PEP 654, 3.11+) catches specific exception
types from an `ExceptionGroup` while propagating others. See note 09
§5 for the supervision tree implementation.

Agents talk via typed message classes (Mochi `stream Tick = {...}`
becomes a Python frozen dataclass, and `ticker ! Tick { time: now() }`
becomes `ticker.cast(Message(payload=Tick(time=now())))`):

```mochi
ticker ! Tick { time: now() }
```

Lowers to:

```python
ticker.cast(Message(payload=Tick(time=now())))
```

Python has no `Sendable`-equivalent annotation or compile-time check.
Structural protection comes from:

- The queue boundary: a value sent through `asyncio.Queue[Message]` is
  captured by the queue and the receiver gets the same reference, so
  the sender must voluntarily not mutate it after sending.
- Mochi's value-semantics contract on records (immutable `frozen=True`
  dataclass by default) makes the sender-after-send mutation
  impossible at the Python level (the dataclass raises
  `FrozenInstanceError` on attribute set).
- For records with mutable fields (Mochi `var` records), the backend
  emits a defensive `dataclasses.replace(msg, ...)` at the send site
  if the message is later mutated by the sender. The default policy is
  to defensively copy on send; the cost is one allocation per send.

See [[02-design-philosophy]] §8 for the Sendability discussion and
the comparison against Trio's nursery model.

The standard library provides higher-level async primitives that the
Mochi stream DSL maps onto: `asyncio.gather`, `asyncio.wait`,
`asyncio.as_completed`, `asyncio.Queue`, `asyncio.Event`,
`asyncio.Condition`, `asyncio.Semaphore`, `asyncio.Lock`. The runtime
depends on no third-party async libraries; only the stdlib `asyncio`
module is required. See note 09 §6.

## 7. Logic programming core

```mochi
fact parent(alice, bob).
fact parent(bob, charlie).

rule ancestor(X, Y) :- parent(X, Y).
rule ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).

query ancestors_of(X) := ancestor(alice, X).
```

The logic core targets a small embedded Datalog engine. The Python
lowering emits a runtime call into `mochi_runtime.datalog.Engine`,
with facts and rules registered at module init time. The engine
implements semi-naive bottom-up evaluation; magic-set transforms are a
v2 concern.

Datalog terms are represented as a frozen-dataclass sum type:

```python
@dataclass(frozen=True, slots=True)
class Atom:
    name: str

@dataclass(frozen=True, slots=True)
class IntTerm:
    value: int

@dataclass(frozen=True, slots=True)
class StringTerm:
    value: str

@dataclass(frozen=True, slots=True)
class Compound:
    head: str
    args: tuple[MochiDatalogTerm, ...]

type MochiDatalogTerm = Atom | IntTerm | StringTerm | Compound
```

The `tuple[MochiDatalogTerm, ...]` typing (immutable, hashable
sequence) is critical for using terms as dict keys in the engine's
index structures. Python's `list` is not hashable; `tuple` is. The
backend always emits `tuple` for Datalog term args.

Predicates are `Callable[[tuple[MochiDatalogTerm, ...]], bool]` lambdas.
Unification is a recursive walk that binds variables in a
`dict[str, MochiDatalogTerm]` substitution map. See note 08 §4 for
the engine internals.

The Python target's Datalog engine has one differentiator: Python's
`functools.lru_cache` (with `maxsize=None` for unbounded caching)
provides zero-cost memoisation of intermediate join results, which
on million-fact workloads can outperform the C and Kotlin engines for
queries with high join repetition. For first-evaluation workloads
(cold cache), the C and Kotlin engines win on raw CPU.

For *very* large fact bases (>10M rows), the Python engine offers an
opt-in Apache Arrow backend that uses `pyarrow.compute` for vectorised
joins. This is a v2 feature; see [[12-risks-and-alternatives]].

## 8. AI and FFI shells

### 8.1 AI shell

```mochi
let summary = ai("summarise this", text)
let result = generate("write a haiku about ", topic)
```

Mochi has two AI builtins: `ai(...)` (synchronous one-shot) and
`generate(...)` (streaming token-by-token). Both lower to async
functions returning the appropriate type.

`ai(...)` lowers to `mochi_runtime.ai.call(prompt: str, *args: object) -> str`
as an `async def`. The body of `call(...)` dispatches based on
provider configuration (env vars at runtime, not codegen choices):

- The default backend uses the **OpenAI Python SDK** (`openai >= 1.50`)
  for OpenAI-compatible endpoints (also covers Azure OpenAI, Mistral
  AI, Together AI, OpenRouter, Groq, and dozens of other vendors that
  expose an OpenAI-compatible API).
- For Anthropic, the backend uses the **Anthropic Python SDK**
  (`anthropic >= 0.40`).
- For local inference via Ollama, the backend uses `httpx` directly
  against the Ollama REST API (no SDK; the API is small enough that a
  thin wrapper is sufficient).
- For Google Gemini, the backend uses the **Google Generative AI
  Python SDK** (`google-generativeai >= 0.8`).
- The provider selection happens at runtime via the
  `mochi_runtime.ai.Provider` protocol; codegen always emits the
  protocol-typed call and lets the runtime pick. See note 04 §10.

`generate(...)` returns `AsyncIterator[MochiToken]` where `MochiToken`
is a small dataclass carrying the token text plus metadata (logprobs,
finish-reason). The implementation wraps the provider's streaming
response in an `async def` generator:

```python
async def generate(prompt: str, *args: object) -> AsyncIterator[MochiToken]:
    async with mochi_runtime.ai.stream(prompt, *args) as stream:
        async for chunk in stream:
            yield MochiToken(text=chunk.text, logprob=chunk.logprob)
```

See note 04 §10.

### 8.2 FFI shell

```mochi
let result = ffi("std/json/parse", raw)
extern fun sqrt(x: float): float = "c:sqrt"
```

The `ffi(...)` builtin lowers to a
`mochi_runtime.ffi.call(path: str, *args: object) -> object`
function that looks up the named function in a module registry. For
Mochi-to-Mochi FFI calls the registry just dispatches to the right
module's top-level function.

The `extern fun` form is the rich case: it declares a foreign function
and lets Mochi code call it directly. The lowering depends on the
target binding type:

For pure-Python external libraries (`extern fun parse(s: string): Json = "py:json.loads"`)
the backend emits a Python function that calls the external library
directly:

```python
import json
from mochi_runtime.ffi import wrap_json

def parse(s: str) -> Json:
    return wrap_json(json.loads(s))
```

For C-extension libraries via `ctypes`
(`extern fun sqrt(x: float): float = "c:sqrt"`), the backend emits
a `ctypes` function pointer:

```python
import ctypes
from ctypes.util import find_library

_libm = ctypes.CDLL(find_library("m"))
_sqrt = _libm.sqrt
_sqrt.argtypes = [ctypes.c_double]
_sqrt.restype = ctypes.c_double

def sqrt(x: float) -> float:
    return _sqrt(x)
```

`ctypes` is part of the Python stdlib and works on every CPython
build. It is slower than `cffi` or a hand-written C extension (each
call pays type-conversion overhead), but it requires no compilation
step at install time. For performance-critical FFI, the user can opt
into **CFFI** by declaring the binding as `"cffi:..."` instead of
`"c:..."`; the lowering emits CFFI's API-level binding code.

For Python C extensions compiled from C source
(`extern fun fast_hash(s: string): int = "cext:fast_hash"`), the
backend generates a `setup.py`-driven C extension stub that the user
must build separately:

```python
# in _fast_hash_cext.pyx (Cython-style, or a hand-written .c file)
from cpython cimport PyUnicode_AsUTF8

cdef extern from "fast_hash.h":
    int fast_hash_c(const char *s)

def fast_hash(s: str) -> int:
    return fast_hash_c(PyUnicode_AsUTF8(s))
```

C extensions are out of scope for v1 codegen (the user writes them
manually if needed); v1 supports `ctypes` and `cffi` only. See note 11
for the full FFI matrix.

For Mochi-as-library exports (Mochi code called by Python consumers),
the emitted Python module is itself the library; the user imports
`from mochi_user.mypkg import myfunc` and calls Mochi code directly.
Python's lack of a foreign-language exposure mechanism (no JNI,
no C-ABI export) means the only consumer of a Mochi-emitted Python
module is another Python program. For C-callable exports from
Python-hosted Mochi code, the user must write a C extension manually
(out of scope for v1).

The Mochi-to-Python FFI surface is one of the load-bearing reasons we
want a Python target at all (see [[02-design-philosophy]] §1): it
unlocks the entire PyPI ecosystem (NumPy, pandas, PyTorch, TensorFlow,
scikit-learn, FastAPI, requests, httpx, BeautifulSoup, sympy, pyarrow,
duckdb, polars, plotly, matplotlib, Jupyter), which is the largest
single-language library ecosystem in the world by package count
(~600k PyPI packages as of 2025-Q4). See note 11.

## 9. Strings, errors, concurrency colouring

### 9.1 Strings

Covered in §1.4 above. The Python target is the *cleanest* of all
seven backends for string handling: PEP 393 variable-width internal
storage gives O(1) random access by code point, O(1) length in code
points, and natural matching of Mochi's specified UTF-8 semantics.
The cost is paid only on the boundary to byte-oriented operations
(UTF-8 encoding for HTTP, file I/O), where the cost is the same as
any other backend.

Cross-boundary string costs:

- Mochi `string` to Python `str`: zero copy (both are the same `str`
  object).
- Python `str` to Mochi `string`: zero copy.
- Mochi `string` to `bytes` (UTF-8 encoded for HTTP, JSON, file I/O):
  one O(n) encoding pass via `s.encode("utf-8")`.
- Mochi `string` to C `char *` via ctypes: one O(n) encoding pass plus
  one allocation via `ctypes.c_char_p(s.encode("utf-8"))`.
- Mochi `string` to NumPy `np.str_`: zero copy when the NumPy dtype is
  UTF-32 (numpy default for `unicode`), one O(n) conversion for
  byte-oriented dtypes.

See [[02-design-philosophy]] §6 for the full string-cost table.

### 9.2 Errors

Mochi's error story is built on the `Result<T, E>` sum type and on
typed `throw e` / `catch e` blocks. Python has **no checked exception
mechanism** (Python exceptions are all unchecked, like Kotlin's), but
Python's culture is heavily exception-based: idiomatic Python uses
`try/except` for error handling, with EAFP ("easier to ask forgiveness
than permission") as a guiding principle.

To preserve Mochi's typed-error semantic, we **do not use exceptions**
for Mochi error reporting; instead, every Mochi function that declares
`throws E` lowers to a Python function returning `MochiResult[T, E]`.
This is a deliberate divergence from idiomatic Python (which would use
exceptions); the rationale is in [[02-design-philosophy]] §11.

Lowering rules:

- Mochi `fun foo(): T throws E { ... }` becomes Python
  `def foo() -> MochiResult[T, E]: ...`. The body returns
  `Ok(value=value)` for the success path and `Err(error=error)` for
  the failure path.
- Mochi `try foo()` becomes Python `foo().get_or_throw()` (which
  rethrows wrapping `Err.error` in a `MochiThrownException(error)` for
  callers that want exception-style propagation), or
  `foo().get_or_none()` for the `T | None` variant.
- Mochi `try foo() catch e => handler` becomes Python:
  ```python
  match foo():
      case Ok(value=v):
          result = v
      case Err(error=e):
          result = handler(e)
  ```
- Mochi `Result<T, E>` becomes Python `MochiResult[T, E]` (the custom
  PEP 695 alias defined above). Mochi `result.ok(x)` becomes `Ok(value=x)`,
  `result.err(e)` becomes `Err(error=e)`.
- Mochi `try?` (optional unwrap of result) becomes Python
  `foo().get_or_none()`, returning `T | None`.
- Mochi `try!` (forced unwrap) becomes Python
  `foo().get_or_throw()`, raising `MochiThrownException` on `Err`.

The `MochiResult[T, E]` PEP 695 alias is defined in `mochi_runtime`:

```python
from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class Ok[T]:
    value: T

    def get_or_throw(self) -> T:
        return self.value

    def get_or_none(self) -> T | None:
        return self.value

@dataclass(frozen=True, slots=True)
class Err[E]:
    error: E

    def get_or_throw(self) -> "object":
        raise MochiThrownException(self.error)

    def get_or_none(self) -> None:
        return None

type MochiResult[T, E] = Ok[T] | Err[E]

class MochiThrownException(Exception):
    def __init__(self, error: object) -> None:
        super().__init__(repr(error))
        self.error = error
```

For interop with Python code that does throw exceptions (the entire
Python stdlib and 99% of PyPI), the backend wraps every FFI call in a
`try/except` that catches the documented exception types and converts
them to `Err` values:

```python
def parse(s: str) -> MochiResult[Json, str]:
    try:
        return Ok(value=wrap_json(json.loads(s)))
    except json.JSONDecodeError as e:
        return Err(error=str(e))
```

The exception types caught at the FFI boundary come from the Mochi
`extern fun` declaration's `throws E` clause, which the user must
write explicitly. See note 06 §9.

### 9.3 Concurrency colouring

Mochi distinguishes synchronous and asynchronous functions (`fun` vs
`async fun`). Python's `async def` modifier and coroutine machinery
maps cleanly:

- Mochi `fun foo(): T { ... }` becomes Python `def foo() -> T: ...`.
- Mochi `async fun foo(): T { ... }` becomes Python
  `async def foo() -> T: ...`.
- Mochi `await foo()` becomes Python `await foo()`.
- Mochi `spawn foo()` (fire-and-forget) becomes Python
  `tg.create_task(foo())` where `tg` is the enclosing `asyncio.TaskGroup`.

Isolation domains:

- An `agent` lowers to a custom class wrapping `asyncio.Queue[Message]`
  plus a TaskGroup-supervised receiver task (see §6). The methods on
  the agent are conceptually isolated to the actor's single-threaded
  receiver loop.
- A `@main` Mochi program (the top-level entry) lowers to a Python
  `async def main() -> None:` plus a `asyncio.run(main())` driver at
  the module's `if __name__ == "__main__":` block.
- Mochi code that needs to run on a specific thread (interop with
  GUI frameworks, blocking I/O) uses `asyncio.run_in_executor` to
  schedule work on a thread pool. There is no `@MainActor` equivalent
  in Python; thread-affinity is managed via executor choice.

Python has no compile-time data-race check (the analogue of Swift 6's
strict-concurrency mode). Structural protection comes from:

- The GIL (Global Interpreter Lock): in CPython 3.12, only one thread
  executes Python bytecode at a time, so concurrent execution within
  a process is serialised at the bytecode level. This eliminates
  many data-race classes by construction (at the cost of throughput).
  PEP 703's `--disable-gil` build (3.13+) removes this guarantee; for
  free-threaded builds, Mochi-emitted code is safe by virtue of the
  immutable-by-default record contract and the queue-based agent
  isolation, but not by GIL-mediated serialisation.
- asyncio's single-threaded event loop: within an asyncio event loop,
  only one coroutine runs at a time (true concurrency only at `await`
  points). This naturally serialises coroutine bodies without
  additional locking.
- Mochi's value-semantics contract: records are immutable by default,
  collections are defensively copied at call boundaries, so the
  natural Mochi style produces code that is already free of data races.

This is the largest semantic gap between MEP-51 (Python) and MEP-49
(Swift): Swift's strict-concurrency catches sharing bugs at compile
time, Python catches none of them at type-check time (mypy and pyright
do not model thread safety). The mitigation is: (a) Mochi's own type
checker rejects sharing patterns at the Mochi level (so the Python
codegen never emits unsafe sharing), (b) the Mochi runtime's
collection wrappers defensively copy at boundaries, and (c) Mochi
agents serialise via queue so cross-agent state is naturally isolated.
See [[02-design-philosophy]] §8 for the Sendability discussion.

Other Python 3.12 features the lowering uses:

- **PEP 695 generic syntax** for functions, classes, and type aliases
  (`def f[T](xs: list[T]) -> T:`, `class Box[T]:`, `type Foo[T] = ...`).
  Mandatory for clean generic codegen.
- **PEP 698 `@override`** decorator. Used on inherited methods (only
  when Mochi traits land; v1 has no method override).
- **PEP 669 `sys.monitoring`** for low-overhead instrumentation.
  Reserved for future profiling and debugging support; not emitted in
  v1 codegen.
- **`tomllib`** (Python 3.11+, stdlib). Used by the Mochi build driver
  for `pyproject.toml` parsing, not by emitted code.
- **`asyncio.TaskGroup`** (3.11+). Used for agent supervision; see §6.
- **PEP 654 ExceptionGroup** and `except*` (3.11+). Used for agent
  supervision failure aggregation; see §6.
- **PEP 604 union types** (`X | Y`, 3.10+). Used everywhere instead of
  `typing.Union[X, Y]`.
- **PEP 585 generic collections** (`list[T]`, `dict[K, V]`, 3.9+).
  Used everywhere instead of `typing.List[T]`.

## 10. What this surface does *not* include

- **Untyped `any`**: Mochi rejects it at the type layer. Python has
  `typing.Any` and the temptation to weaken Mochi's type system to
  allow `any` is real, especially given Python's dynamic-typing
  culture; we resist. Both mypy --strict and pyright --strict warn on
  any `Any` leakage, and the build gate fails if Mochi-emitted code
  contains `Any` outside of explicitly-allowed positions (cast()
  helpers at FFI boundaries).
- **Implicit conversions**: ruled out above. Required to keep C, BEAM,
  JVM, .NET, Swift, Kotlin, and Python behaviours identical.
- **`None` at the language level**: see §1.7. Mochi has no `None`;
  Python's `None` only appears as the `None` variant of `T | None`.
- **Inheritance**: Mochi has no class inheritance (only sealed-union
  ADTs and protocol composition). The Python `class Foo(Bar):` form
  is unused for user code. Internal helpers and FFI bridges to Python
  frameworks (which use inheritance heavily, e.g., FastAPI's
  `BaseModel`, Django's `Model`) are the only places inheritance
  appears.
- **Operator overloading**: Mochi does not let users overload `+` etc.
  Python allows operator overloading via `__add__`, `__sub__`,
  `__mul__`, etc., but the Mochi target never emits them for user
  code. Internal runtime types (the `MochiOrderedSet` wrapper, the
  `MochiResult` types) do define dunder methods, but the user-facing
  surface never exposes them as customisation points.
- **Metaclasses**: not exposed at the Mochi language level. Internally
  the runtime uses `ABCMeta` for protocol abstract methods only.
- **Descriptors / `__get__` / `__set__`**: not exposed at the Mochi
  language level.
- **`__slots__` introspection**: not exposed. Mochi-emitted classes
  use `slots=True` automatically; the user does not write `__slots__`
  directly.
- **`__init_subclass__` / class-creation hooks**: not exposed.
- **Decorators in user code**: not exposed at the Mochi language
  level (Python decorators are powerful but they require dynamic
  dispatch and they break type-checker inference). Internally the
  runtime uses `@dataclass`, `@cache`, `@functools.wraps`.
- **`typing.Protocol`-based structural typing**: not exposed in v1.
  Mochi's type system is nominal (a record of type `Person` is *not*
  the same as another record with the same fields). Python's
  `Protocol` (PEP 544, 3.8+) provides structural typing, which Mochi
  rejects. The Mochi target uses nominal dataclass types and never
  emits `Protocol` definitions for user code. Internal runtime
  abstractions (`AsyncIterator`, `Callable`) are typing's existing
  protocols and are used for type annotations only.
- **`typing.TypedDict`**: deferred. Could be useful for the map-
  destructuring case (§1.1) when fields are known statically; the v1
  lowering uses `dict[str, object]` plus `cast()` instead.
- **`typing.Self`** (PEP 673, 3.11+): not used in v1. Mochi has no
  method that returns `Self`; if traits land in v2, `Self` becomes
  relevant.
- **Walrus operator `:=`** (PEP 572): not used in user-facing Mochi
  code. Internal runtime helpers may use it for inline assignments
  in comprehensions.
- **`f`-strings as DSL**: Mochi has its own string interpolation that
  lowers to Python `f`-strings. The Mochi syntax `"hello {name}"`
  lowers to Python `f"hello {name}"`. We do not expose Python's
  f-string format spec mini-language (`f"{x:>10.2f}"`) to Mochi users
  directly; for formatted output Mochi has its own format builtins.

## 11. Surface-to-Python cheat sheet (cross-reference)

| Mochi form | Python lowering | Note |
|------------|-----------------|------|
| `let x = ...` | `x: Final[T] = ...` | §1.1, note 05 §4 |
| `var x = ...` | `x: T = ...` | §1.1 |
| `int` | `int` (arbitrary precision) | §1.2, note 06 §5 |
| `float` | `float` | §1.2 |
| `bool` | `bool` | §1.2 |
| `string` | `str` (PEP 393 variable-width) | §1.4 |
| `bytes` | `bytes` | §1.2 |
| `list<T>` | `list[T]` | §3, note 06 §10 |
| `map<K,V>` | `dict[K, V]` | §3 |
| `set<T>` | `MochiOrderedSet[T]` (insertion-ordered wrapper) | §3 |
| `record T { ... }` | `@dataclass(frozen=True, slots=True)` class | §2.3 |
| `type T = A \| B` (sum) | PEP 695 `type T = A \| B` with dataclass variants | §4 |
| `Option<T>` | `T \| None` (PEP 604 union) | §1.7 |
| `Result<T, E>` | `MochiResult[T, E]` = `Ok[T] \| Err[E]` PEP 695 alias | §9.2 |
| `match` | PEP 634 `match` statement with class patterns | §4 |
| `fun(...) => ...` | lambda or nested `def` | §2.2 |
| `from ... select ...` | generator expression + sorted + helpers | §5, note 08 |
| `agent ...` | custom class with `asyncio.Queue` + TaskGroup | §6 |
| `stream<T>` | `AsyncIterator[T]` (`async def` generator) | §6 |
| `fact / rule / query` | runtime Datalog engine | §7, note 08 §4 |
| `ai(...)` | `mochi_runtime.ai.call` (OpenAI / Anthropic / Gemini / Ollama dispatch) | §8.1 |
| `generate(...)` | async generator returning `AsyncIterator[MochiToken]` | §8.1 |
| `extern fun ... = "c:..."` | `ctypes` binding | §8.2 |
| `extern fun ... = "py:..."` | direct Python function call | §8.2 |
| `throws E` | return `MochiResult[T, E]` (no exceptions) | §9.2 |
| `async fun` | `async def` | §9.3 |
| `await x` | `await x` | §9.3 |
| `spawn f()` | `tg.create_task(f())` | §9.3 |
| `@ui` | `loop.call_soon_threadsafe(...)` or similar | §9.3 |
| `linear T` | not yet supported (Python has no `~Copyable` equivalent) | §9.3 |
| `borrowed T` | not yet supported | §9.3 |
| identifier `class` | identifier `class_` | §1.6 |
| identifier `def`, `lambda`, `import`, `from`, `match`, `case`, `type` | trailing underscore | §1.6 |
| doc comment `/// ...` | `"""..."""` docstring on the following def / class | §1.1 |

## 12. Doc comments and `__doc__`

Mochi supports doc comments via `///` (triple-slash, Rust-style). The
Python lowering maps doc comments to **PEP 257 docstrings**: a
triple-quoted string literal immediately after the `def` or `class`
header.

```mochi
/// Returns the area of a circle given its radius.
fun area(radius: float): float {
  return 3.14 * radius * radius
}
```

Lowers to:

```python
def area(radius: float) -> float:
    """Returns the area of a circle given its radius."""
    return 3.14 * radius * radius
```

The docstring is accessible at runtime via `area.__doc__`. Python
documentation tools (Sphinx, mkdocs, pydoc) consume `__doc__`
automatically. Module-level Mochi doc comments lower to module-level
docstrings (the first statement of the module). Class-level Mochi doc
comments lower to class-level docstrings (the first statement inside
the class body).

For multi-line doc comments, the lowering uses triple-quoted strings:

```python
def parse(s: str) -> MochiResult[AST, str]:
    """Parse the input string into a Mochi AST.

    Args:
        s: The source string.

    Returns:
        Ok(ast) on success, or Err(message) on parse failure.
    """
    ...
```

The Google docstring style (Args / Returns / Raises) is the default;
Sphinx and pyright both understand it. Other styles (reStructuredText,
NumPy) are configurable via `--python-docstring-style`. See note 06
§13.

## 13. Modules and imports

Mochi modules map to Python packages and modules. A Mochi file
`src/mathutils/extra.mochi` becomes a Python file
`mochi_user/mathutils/extra.py`, with the parent directories getting
`__init__.py` files that re-export from the leaf modules:

```python
# mochi_user/mathutils/__init__.py
from mochi_user.mathutils.extra import *  # re-exports public symbols
from mochi_user.mathutils import extra as extra  # also accessible as submodule
```

Mochi imports map to Python imports. The lowering rules:

| Mochi | Python |
|-------|--------|
| `import "mathutils/extra"` | `from mochi_user.mathutils import extra` |
| `import "mathutils/extra" as me` | `from mochi_user.mathutils import extra as me` |
| `import { add, sub } from "mathutils/extra"` | `from mochi_user.mathutils.extra import add, sub` |
| `import "github.com/foo/bar"` | `from bar import ...` (with the runtime path coming from `pyproject.toml [project.dependencies]`) |

External dependencies (PyPI packages) are declared in the Mochi
project's `mochi.toml` and surfaced into the generated `pyproject.toml`
under `[project.dependencies]`:

```toml
# pyproject.toml (generated)
[project]
name = "myapp"
version = "0.1.0"
requires-python = ">=3.12"
dependencies = [
    "httpx >= 0.27",
    "openai >= 1.50",
    "anthropic >= 0.40",
]
```

The user's Mochi project lock file `mochi.lock` is translated to a
`uv.lock` (the canonical lock file format for the uv build driver,
PEP 751-compatible). The two lock files are kept in sync by the
Mochi build driver. See note 10 for the build system.

## 14. Visibility

Mochi has three visibility levels: `pub` (public), the default
(package-private), and `priv` (file-private). Python has only one
formal visibility level (everything is accessible), with two
conventions:

- A leading underscore (`_name`) marks a name as "private" by
  convention. PEP 8 says: "use one leading underscore only for non-
  public methods and instance variables". Python type checkers and
  linters honour this convention.
- Double leading underscore (`__name`) triggers name mangling for
  class attributes (`__name` becomes `_ClassName__name`), which is
  Python's only enforced privacy mechanism.

The Mochi lowering uses:

- Mochi `pub` becomes Python no-prefix names exported via `__all__`.
- Mochi default (package-private) becomes Python no-prefix names *not*
  in `__all__`. The names are still accessible via direct import, but
  `from module import *` does not pull them.
- Mochi `priv` becomes Python `_name` (single leading underscore).

This is the same pattern other dynamic-language targets adopt (the
Ruby and Lua targets in MEPs not yet drafted would use the same
convention). It is not a hard guarantee at the Python level, but the
combination of `_`-prefix + `__all__` + type-checker enforcement is
the strongest available without resorting to runtime decorators or
attribute-access checks.

## 15. Reflection

Mochi has limited reflection: a Mochi value's type can be queried at
runtime via the `typeof` builtin, and a record's field names can be
enumerated via the `fields_of` builtin. Both lower to Python:

- `typeof(x)` becomes `type(x).__name__` (returns the class name as
  a string).
- `fields_of(record)` becomes `[f.name for f in dataclasses.fields(record)]`.

The Python target does not expose Python's full reflection capabilities
(`getattr`, `setattr`, `hasattr`, `inspect.signature`, `inspect.members`)
to Mochi code, because those would break Mochi's static-typing
guarantees. The two builtins above are the only reflection surface in
v1.

For more advanced reflection (e.g., the Mochi `serialize` builtin that
walks a record's fields and produces a dict), the lowering uses
`dataclasses.asdict(obj)` or a custom walker that handles `MochiResult`
and the sum-type cases.

## 16. Identifier visibility and `__all__`

Each Mochi-generated Python module sets `__all__` explicitly:

```python
__all__ = ["public_fn", "PublicClass", "PublicEnum"]
```

This is the canonical Python way to control `from module import *`
behaviour. The Mochi codegen computes `__all__` from the set of public
top-level declarations.

Names not in `__all__` are still accessible via `from module import
_private_name` if the user knows the name, but they don't appear in
star imports and they are conventionally treated as private. Both
mypy and pyright honour `__all__` for visibility checking.

## 17. Open questions for note 02 (design philosophy)

- **Codegen IR**: libcst (Instagram's concrete syntax tree library) vs
  `ast.unparse` vs raw string emit. (Resolved in note 02 §3 and note
  05 §1: libcst for the source emitter.)
- **Type checker version pinning**: mypy 1.13 vs 1.14 (released
  2024-12) vs pyright 1.1.380 vs 1.1.390. The two checkers' inference
  diverges on PEP 695 type aliases; we pin both for reproducibility.
- **PEP 703 free-threaded compatibility**: emitted code must be safe
  under `--disable-gil` (3.13+). The current lowering is naturally
  safe because of immutable-by-default records and queue-based agent
  isolation, but the gate has not been formalised.
- **Jupyter ipykernel ABI stability**: ipykernel 6.x is the current
  baseline; 7.0 is in development with significant API changes. The
  Mochi kernel implementation pins ipykernel 6.x for v1.
- **PyPI Trusted Publishing OIDC support**: PyPI's Trusted Publishing
  uses GitHub Actions OIDC + sigstore for keyless publishing. The
  build pipeline assumes this; alternative pipelines (GitLab CI,
  CircleCI) require manual token configuration.
- **Wheel reproducibility**: SOURCE_DATE_EPOCH plus sorted wheel
  entries gives byte-identical wheel builds. See [[02-design-philosophy]]
  §16 and note 11 §6.

## 18. Cross-references

- [[02-design-philosophy]] (next note): why each of the choices above
  was made, including the case for CPython 3.12 as the floor and the
  comparison against alternative Mochi-to-Python pathways (PyPy,
  Cython, mypyc, Nuitka, Pyodide).
- [[03-prior-art-transpilers]]: survey of 2to3, Cython, mypyc, Nuitka,
  Numba, Brython, Transcrypt, RustPython, PyPy, Jython, IronPython,
  GraalPy, Hy, Coconut, Mojo, LPython, MicroPython, Codon, pytype,
  pyrefly, plus other source-to-Python transpilers and Python-from-
  other-language pipelines.
- [[04-runtime]]: the `mochi_runtime` Python package layout, including
  the `collections`, `io`, `ai`, `ffi`, `datalog`, `query`, and
  `supervisor` submodules.
- [[05-codegen-design]]: the IR layer that turns this surface into
  emitted Python source via libcst builders.
- [[06-type-lowering]]: the per-type details glossed here, including
  the PEP 695 quirks and the dataclass synthesis details.
- [[07-python-target-portability]]: the CPython version matrix
  (3.12, 3.13, 3.14-dev) and the platform skew (Linux glibc, Linux
  musl via the `manylinux` wheel tags, macOS arm64/x86_64, Windows
  AMD64, sdist-from-source fallback).
- [[08-dataset-pipeline]]: the query DSL lowering in full, including
  the pyarrow/duckdb/polars optional backends and the Datalog engine
  design.
- [[09-agent-streams]]: agent and stream lowering on asyncio Queue
  and TaskGroup, including the supervision tree design and the
  PEP 654 ExceptionGroup integration.
- [[10-build-system]]: uv + hatchling + pyproject.toml integration,
  the generated `pyproject.toml`, and the multi-platform CI matrix.
- [[11-testing-gates]]: the test-suite gates for v0.10 ship; what
  fraction of `examples/v0.2`-`v0.7` must transpile, type-check
  (mypy --strict, pyright --strict), build (wheel + sdist), and run
  on each platform.
- [[12-risks-and-alternatives]]: the risk register and the
  alternatives considered (notably Cython for AOT, mypyc for
  whole-program compilation, and Pyodide for browser deployment).
- [[../0050/01-language-surface]]: the Kotlin-target analogue of this
  note, the closest in spirit since both target a managed dynamic-ish
  runtime with first-class generics, sealed sum types, and
  async/await. The Kotlin and Python lowerings diverge most sharply
  on string encoding (Kotlin UTF-16, Python PEP 393) and on the
  presence/absence of a compile-time data-race check (Kotlin none,
  Python none, neither matches Swift's strict-concurrency).
- [[../0049/01-language-surface]]: the Swift-target analogue, which
  shares the typed-Result error story and the actor-as-class shape.
- [[../0047/01-language-surface]]: the JVM-bytecode-target analogue,
  which shares the GIL-equivalent serialisation property (kind of:
  the JVM has no GIL, but Kotlin/JVM bytecode and Python/CPython
  both run user code on a managed runtime).
- [[../0048/01-language-surface]]: the .NET-target analogue, which
  shares the dynamic-language-feel-from-static-type-system pattern
  via C#'s `dynamic` (which we never use).
- [[../0046/01-language-surface]]: the BEAM-target analogue, whose
  agent / mailbox / supervision design directly inspired the Python
  asyncio.Queue + TaskGroup lowering.
- [[../0045/01-language-surface]]: the C-target analogue, whose
  C-ABI extern story is mirrored via Python `ctypes` bindings.
