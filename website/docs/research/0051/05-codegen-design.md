---
title: "MEP-51 research note 05, Codegen design from aotir IR to Python source"
description: "The codegen pipeline from MEP-45 aotir IR to formatted, type-checked Python 3.12 source, covering phase ordering, AST choice, naming, and source maps."
---

# MEP-51 research note 05, Codegen design from aotir IR to Python source

Author: research pass for [[mep-0051]]. Date: 2026-05-23 14:20 (GMT+7).
Method: structured walk over the CPython `ast` and `compile` modules
(Python 3.12 reference), the libcst project (Instagram), the
`ast.unparse` recipe (PEP 8 round-tripping), `ruff format` v0.6
(Astral, Rust), the [[mep-0050]] Kotlin codegen note, the
[[mep-0049]] Swift codegen note, the [[mep-0045]] aotir IR design
note, and direct reads of `transpiler3/aotir/program.go` and
`transpiler3/c/lower/lower.go` in this repository for IR shape
reference. Cross-referenced PEPs 8, 484, 526, 561, 585, 591, 593,
604, 612, 657, 692, 695, 698, and 723 throughout.

This note specifies the codegen pipeline that turns Mochi `.mochi`
source (already typed and lowered to MEP-45 aotir IR) into formatted
Python 3.12 source files that pass `mypy --strict`, `pyright
--strict`, `ruff check`, and `ruff format --check`. The output is a
**phase-by-phase design** with API contracts, data shapes, and the
choices that distinguish this lowering from a naive
"emit text by walking IR" approach.

The companion notes are: [[04-runtime]] (the `mochi_runtime` package
that the emitted code imports), [[06-type-lowering]] (per-type
mapping decisions, the small numerous choices this note glosses
over), [[10-build-system]] (uv + hatchling assembly into a wheel),
and [[11-testing-gates]] (vm3 byte-equal, mypy, pyright, ruff fixed
points).

---

## 1. Pipeline overview

```
.mochi source
   |
   | (parser, types/check, MEP-45 stages 1-3, shared across targets)
   v
aotir.Program
   |
   |  +-> transpiler3/python/lower/                (this note, phases A-E)
   |  |     |
   |  |     v
   |  |   pyast.Module       (a Python ast.Module surrogate in Go)
   |  |     |
   |  |     v
   |  +-> transpiler3/python/emit/                 (this note, phases F-G)
   |        |
   |        v
   |      python source bytes
   |        |
   |        v
   |      ruff format --stdin                       (phase H)
   |        |
   |        v
   |      formatted bytes
   |        |
   |        v
   |      ruff check --fix --stdin                  (phase I)
   |        |
   |        v
   |      lint-clean bytes
   v
.py file on disk + sourcemap.json sidecar
```

Phases A-E live in `transpiler3/python/lower/`; phases F-G in
`transpiler3/python/emit/`; phases H-I shell out to `ruff` (or call
into it via FFI, see §11). The IR layer (aotir.Program) is the same
shared IR all `transpiler3/<target>/` packages consume. We do not
fork it for Python.

---

## 2. Phase ordering rationale

Eight named phases inside `lower`:

| Phase | Name                            | Input                           | Output                          |
|-------|---------------------------------|---------------------------------|---------------------------------|
| A     | name binding                    | aotir.Program                   | scoped + mangled IR             |
| B     | monomorphisation                | scoped IR                       | mono IR (no type params)        |
| C     | closure conversion              | mono IR                         | closure-free IR                 |
| D     | match-to-decision-tree           | closure-free IR                 | decision-tree-flattened IR      |
| E     | python AST construction         | decision-tree IR                | pyast.Module                    |

Two phases inside `emit`:

| Phase | Name                | Input         | Output             |
|-------|---------------------|---------------|--------------------|
| F     | source maps         | pyast.Module  | annotated pyast    |
| G     | unparse to text     | pyast.Module  | python source bytes |

Two phases that shell to `ruff`:

| Phase | Name                | Input         | Output             |
|-------|---------------------|---------------|--------------------|
| H     | `ruff format`       | bytes         | formatted bytes    |
| I     | `ruff check --fix`  | bytes         | lint-clean bytes   |

The order matters; rearranging breaks invariants. Sections 3-12
cover each phase.

---

## 3. Phase A: name binding and mangling

The Mochi parser binds every identifier to a unique symbol via the
shared types/check pass (MEP-45 stage 2). aotir already carries
fully-qualified symbol names like `mypkg.foo.bar`. Phase A's job is
to map Mochi names to **valid Python identifiers** that survive
`ruff check` and do not collide with Python's reserved words.

### 3.1 Python reserved words and soft keywords

The Python 3.12 lexer treats these as **hard keywords** (cannot be
identifier names): `False`, `None`, `True`, `and`, `as`, `assert`,
`async`, `await`, `break`, `class`, `continue`, `def`, `del`, `elif`,
`else`, `except`, `finally`, `for`, `from`, `global`, `if`, `import`,
`in`, `is`, `lambda`, `nonlocal`, `not`, `or`, `pass`, `raise`,
`return`, `try`, `while`, `with`, `yield`.

Soft keywords (legal as identifiers but reserved in some contexts):
`_` (only in patterns), `case`, `match`, `type`. We treat soft
keywords as **available** identifiers but the emitter rewrites
`match` and `type` as Mochi identifiers to `match_` and `type_` to
avoid surprise when reading emitted code.

### 3.2 Mangling rules

For any Mochi identifier `n`:

1. If `n` is in `HARD_KEYWORDS` ∪ `SOFT_KEYWORDS`: append `_` until
   no longer a keyword. (`class` -> `class_`; never `class__` because
   `class_` is already non-keyword.)
2. If `n` starts with two underscores and ends with two underscores
   (`__foo__`): rename to `_mochi__foo__` to avoid collision with
   Python dunders (`__init__`, `__hash__`, ...). Mochi identifiers
   are not allowed to start with `__` per the Mochi style guide, but
   user-supplied generated names from macros can.
3. If `n` is `self` or `cls` and appears outside a class method
   context: rename to `self_` / `cls_`. Inside methods, the first
   parameter is **always emitted as `self`** regardless of the Mochi
   source name (the Mochi spec requires methods to have an explicit
   receiver name; we override it for Python convention).
4. Module-level identifiers prefixed with `_` keep the prefix (Mochi
   `_foo` -> Python `_foo`, signalling module-private per PEP 8).
5. All other identifiers pass through unchanged.

### 3.3 Symbol uniqueness

After mangling, the emitter rebuilds the per-module symbol table and
asserts: every binding in a single Python scope has a unique name.
If the mangling produced a collision (rare: only via Mochi macro
expansion), append a numeric disambiguator (`foo_2`, `foo_3`).

### 3.4 Dunder collision check

A separate pass walks every class scope and verifies no Mochi field
or method maps to a Python dunder name (`__init__`, `__call__`,
`__hash__`, `__eq__`, `__repr__`, `__match_args__`, `__slots__`).
We allow `__init__` only on dataclass-emitted records (where we
control it), and `__hash__` / `__eq__` only via `@dataclass(eq=True,
frozen=True)`. Manual dunders from Mochi user code are rejected at
this phase with a compiler diagnostic pointing back to the Mochi
source line.

---

## 4. Phase B: monomorphisation

Python 3.12 type parameters (PEP 695) are erased at runtime; they
exist only for type checkers. **We still monomorphise**, for two
reasons:

1. **Speed**. Per-instantiation specialised code is faster than
   `isinstance`-dispatched generics (no `TypeVar.bound` runtime
   checks needed).
2. **Type-checker compatibility**. `pyright` 1.1.380 has known bugs
   on deeply-nested generic dispatch (e.g. `dict[K, list[Result[K,
   E]]]` resolution). Monomorphising the IR before lowering means
   Python sees concrete types, which `pyright` handles reliably.

Phase B walks every generic-callsite in the IR and, per unique type
argument tuple, emits a specialised copy of the function. The
specialisation key is the canonical form of the type argument list;
identical instantiations share a single specialised function.

### 4.1 What stays generic

PEP 695 type-parameter syntax is still emitted on:

- Runtime polymorphic functions where monomorphisation explodes
  (e.g. higher-order combinators that take generic callables; we
  monomorphise the closure but the wrapper stays generic).
- Standard library shapes: `list[T]`, `dict[K, V]`, `Iterator[T]`.
  These are stdlib-provided generic types; we never re-emit them.

### 4.2 Worked example

Mochi:

```mochi
fun pair<A, B>(a: A, b: B): list<A | B> {
  return [a, b]
}

let x = pair(1, "hello")
let y = pair(true, false)
```

Monomorphised IR generates two specialised functions. Phase E
lowers each to a Python `def`:

```python
def pair_int_str(a: int, b: str) -> list[int | str]:
    return [a, b]

def pair_bool_bool(a: bool, b: bool) -> list[bool]:
    return [a, b]

x: list[int | str] = pair_int_str(1, "hello")
y: list[bool] = pair_bool_bool(True, False)
```

Naming convention: `<fn>_<arg1>_<arg2>_..._<argN>` where each `<arg>`
is the canonical type name. For complex types (`list[int]`,
`dict[str, T]`) the canonical form replaces brackets with `_` and
strips spaces (`list[int]` -> `list_int`; `dict[str, T]` -> `dict_str_T`).
Collisions across distinct instantiations are disambiguated with
numeric suffixes.

### 4.3 When monomorphisation is skipped

If a generic function is never called with concrete types (only
ever passed as a higher-order value), we keep it generic. This is
the `Callable[P, R]`-passing case. The Mochi `map` function over
streams is the canonical example: `map_stream<T, U>(s: stream<T>,
f: T -> U): stream<U>`. Phase E emits:

```python
def map_stream[T, U](
    s: AsyncIterator[T], f: Callable[[T], U]
) -> AsyncIterator[U]:
    async def _g() -> AsyncIterator[U]:
        async for x in s:
            yield f(x)
    return _g()
```

The PEP 695 syntax is the in-source type-parameter declaration. The
emitter does not erase it; it knows the function is generic-only.

### 4.4 Bounded generics

Mochi generic constraints (`T: Ord`, `T: Hashable`) lower to
`bound=` on the type parameter:

```python
def sorted_unique[T: Hashable](xs: list[T]) -> list[T]:
    ...
```

PEP 695's syntax for `bound` is `[T: Bound]`. We use it.

---

## 5. Phase C: closure conversion

Python has first-class closure cells (`cell_contents`), so closure
conversion is **almost** trivial: a Python `def` nested in another
`def` captures variables by reference automatically. The phase has
two responsibilities:

### 5.1 Captured-variable identification

Phase C walks every Mochi `\(x) -> body` (lambda) and every nested
`fun` and determines:

- Free variables (not bound in the function body, looked up in
  enclosing scope).
- Whether each free variable is read-only or mutated inside the
  closure.

If a free variable is mutated, the emitter prefixes the assignment
with `nonlocal name` in the nested function:

```python
def outer() -> int:
    counter: int = 0

    def increment() -> None:
        nonlocal counter
        counter += 1

    increment()
    return counter
```

Without `nonlocal`, the inner `counter += 1` creates a fresh local
shadowing the outer binding, and Python raises `UnboundLocalError`
because the `+=` reads the local before the first write. The Mochi
type checker has already verified the capture is legal; Phase C just
adds the keyword.

### 5.2 Lambda lifting (rarely)

If a Mochi closure captures a large state and is passed across an
async boundary, the captured state may end up in the future's
internal closure cell, preventing GC of the surrounding stack frame
even after the closure is callable. We do not lambda-lift by default;
we trust the Mochi user to write tight closures. A future MEP (not
v1) might add a "no-capture" verifier for hot-path closures.

### 5.3 `lambda` vs `def`

Python `lambda` is restricted: single expression, no statements,
limited typing surface. We emit `lambda` only when the Mochi
expression is a single Python expression. For multi-statement
closures, we emit nested `def` with a synthetic name (`_closure_<n>`)
and reference it by name in the surrounding code.

```python
# Mochi: \(x: int) -> int { let y = x * 2; return y + 1 }
def _closure_3(x: int) -> int:
    y: int = x * 2
    return y + 1

# Mochi: \(x: int): int -> x + 1
_closure_4 = lambda x: x + 1  # noqa: E731  (lambda assigned to name)
```

The `noqa: E731` suppresses ruff's lambda-assignment lint; we choose
the lambda form when the Mochi annotation matches a lambda's
expressivity. Otherwise we use the nested `def`.

---

## 6. Phase D: match-to-decision-tree

Mochi's `match` (PEP 634-shaped, predates the PEP, semantically
identical) lowers to Python `match` directly. The exhaustiveness
check is the load-bearing artefact: we want **mypy** and **pyright**
to verify exhaustiveness statically.

### 6.1 The `assert_never` trick

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

`typing.assert_never(o)` has the type signature `(arg: Never) ->
Never`. The type checker:

1. Tracks the type of `o` through each `case`.
2. After all `case`s, `o`'s narrowed type is `Never` if and only if
   all variants are exhausted.
3. If `Never`, the call type-checks. If not, the call is a type
   error: "argument of type X is not assignable to Never".

The result: adding a new variant to `JsonValue` without updating the
`match` raises a mypy / pyright error at the `assert_never` call
site. This is the recommended PEP 634 + PEP 698 pattern (per the
Python typing docs).

### 6.2 Decision-tree flattening

Mochi's source `match` may have nested patterns, guards, and
or-patterns. Phase D flattens these to a Python-friendly shape:

- Or-patterns (`case A | B:`) emit as Python or-patterns directly.
- Guards (`case A if x > 0:`) emit as Python `case A if x > 0:`.
- Nested patterns (`case Foo(bar=Bar(baz=z)):`) emit as Python
  nested patterns. Python 3.10+ accepts these.
- Capture patterns (`case Foo(bar=x):`) emit as Python `case Foo(bar=x):`
  with the implicit binding.

For complex patterns where the Mochi semantic is "decision tree with
sharing" (a single subject inspected once, dispatch to N branches),
we still emit Python `match` because the CPython 3.11+ implementation
already builds a decision tree internally. We do not flatten further.

### 6.3 Patterns that do not lower

A small set of Mochi patterns cannot directly express in Python
`match`:

- **View patterns** (Mochi `case v @ Foo(x) if pred(v):` where `v`
  is the whole subject): Python supports `case Foo(x) as v if
  pred(v):` (the `as v` clause).
- **Type-parameterised patterns** (`case Box<int>(v):`): Python
  match cannot pattern-match on generic instantiations because
  generics are erased. Phase D rewrites these to a guard: `case
  Box(v) if isinstance(v, int):`.

---

## 7. Phase E: Python AST construction

The IR-to-Python-AST conversion is the largest single phase. The
output is a tree of Python AST node surrogates that we **own in Go**
(the host compiler language), not Python `ast.Module` objects. We
do not embed a CPython interpreter; we generate text and let Python
parse it back.

### 7.1 The Go-side AST shape

```go
// transpiler3/python/pyast/nodes.go (sketch)

type Module struct {
    Body    []Stmt
    Imports []ImportSpec  // sorted, deduplicated; ruff isort compatible
    Header  []Comment     // # comments at top of file
}

type Stmt interface { stmt() }

type FunctionDef struct {
    Name       string
    TypeParams []TypeParam   // PEP 695: [T, U: Hashable]
    Args       []Arg
    Returns    Expr          // type annotation, or nil
    Body       []Stmt
    Decorators []Expr        // @override, @dataclass, ...
    Async      bool          // emit `async def` instead of `def`
    SrcLoc     *SourceLoc    // Mochi origin line/col
}

type ClassDef struct {
    Name       string
    TypeParams []TypeParam
    Bases      []Expr
    Keywords   []Keyword     // metaclass=..., etc
    Body       []Stmt
    Decorators []Expr
    SrcLoc     *SourceLoc
}

type Assign struct {
    Targets []Expr
    Value   Expr
    Type    Expr   // PEP 526 annotation, or nil
    SrcLoc  *SourceLoc
}

// ... ~80 more node types
```

The shape mirrors CPython's `ast` module 1:1 (FunctionDef, ClassDef,
Assign, If, For, While, Match, MatchCase, ...) so that the unparser
in phase G can produce output that round-trips through `ast.parse +
ast.unparse` with the standard formatting.

### 7.2 Why a Go-side AST and not text-only?

Three options were considered:

1. **Text concatenation**: walk the IR, emit raw Python text. Simple
   but loses structural information needed for source maps and for
   per-node post-processing (e.g. inserting `from __future__ import
   annotations` exactly once at module top).
2. **Python `ast` over IPC**: shell out to a Python helper that
   builds `ast.Module` objects and calls `ast.unparse`. Robust, but
   adds Python as a build dep of the Mochi compiler itself.
3. **Go-side AST**: in-process Go struct tree, with a Go unparser
   that emits text in the shape `ast.unparse` would emit, then
   `ruff format` cleans up.

We chose option 3. The Go AST is ~400 LOC across the node types;
the unparser is ~600 LOC. `ruff format` is the only external tool
shelled to, after the unparser is done.

### 7.3 Why not libcst?

LibCST (Instagram, Apache-2.0) is a CPython-compatible CST library
that preserves whitespace and comments. It is the right tool for
**modifying existing Python source**; for **generating Python from
scratch** (our case), it adds API surface we do not need.

### 7.4 Per-statement lowering

The lowering tables are mechanical. A sampler:

| Mochi IR                       | Python AST                          |
|--------------------------------|--------------------------------------|
| `let x: T = e`                 | `Assign(target=x, type=T, value=e)` |
| `x = e` (rebinding)            | `Assign(target=x, value=e)` (no type) |
| `if c { ... } else { ... }`    | `If(test=c, body=..., orelse=...)` |
| `while c { ... }`              | `While(test=c, body=...)`           |
| `for x in xs { ... }`          | `For(target=x, iter=xs, body=...)` |
| `match v { ... }`              | `Match(subject=v, cases=...)`       |
| `return e`                     | `Return(value=e)`                   |
| `e1; e2; e3`                   | `Expr(e1), Expr(e2), Expr(e3)`      |
| `break`, `continue`            | `Break`, `Continue`                 |
| `panic("msg")`                 | `Raise(exc=RuntimeError("msg"))`    |
| `assert c, "msg"`              | `Assert(test=c, msg="msg")`         |

### 7.5 Per-expression lowering

| Mochi IR                       | Python AST                          |
|--------------------------------|--------------------------------------|
| `e1 + e2`                      | `BinOp(left=e1, op=Add, right=e2)`  |
| `e1 == e2`                     | `Compare(left=e1, op=Eq, right=e2)` |
| `e1 && e2`                     | `BoolOp(op=And, values=[e1, e2])`   |
| `!e`                           | `UnaryOp(op=Not, operand=e)`        |
| `f(a, b)`                      | `Call(func=f, args=[a, b])`         |
| `xs[i]`                        | `Subscript(value=xs, slice=i)`      |
| `xs[a:b]`                      | `Subscript(value=xs, slice=Slice(lower=a, upper=b))` |
| `obj.field`                    | `Attribute(value=obj, attr="field")` |
| `[a, b, c]`                    | `List(elts=[a, b, c])`              |
| `{k: v, ...}`                  | `Dict(keys=[k], values=[v])`        |
| `\(x) -> e`                    | `Lambda(args=[x], body=e)` (if single-expr) |
| `await e`                      | `Await(value=e)`                    |
| `yield e`                      | `Yield(value=e)`                    |

Lambda gets special-cased to `Lambda` only when its body fits Python
lambda restrictions; otherwise Phase C has already lifted it.

### 7.6 Type annotation lowering

Type annotations are expressions in PEP 526. The lowering produces
the smallest expression that mypy + pyright agree on:

| Mochi type | Python annotation expression          |
|------------|----------------------------------------|
| `int`      | `Name("int")`                          |
| `string`   | `Name("str")`                          |
| `list<T>`  | `Subscript(Name("list"), T-expr)`     |
| `map<K,V>` | `Subscript(Name("dict"), (K-expr, V-expr))` |
| `T?`       | `BinOp(T-expr, BitOr, Constant(None))` |
| `A | B`    | `BinOp(A-expr, BitOr, B-expr)`         |
| function   | `Subscript(Name("Callable"), (args, ret))` |

The `BitOr` operator is PEP 604 union syntax. Available unquoted at
runtime since 3.10. We use it everywhere instead of
`Union[A, B]`. `Callable` is from `collections.abc` (PEP 585).

---

## 8. Module structure

### 8.1 File layout per Mochi module

A Mochi module `mypkg.foo.bar` lowers to a Python module
`mypkg/foo/bar.py`. The standard top-of-file structure:

```python
"""Module docstring from Mochi top-level doc-comment."""

from __future__ import annotations

import asyncio
from collections.abc import AsyncIterator, Callable, Iterable, Iterator
from dataclasses import dataclass
from typing import Final, assert_never

from mochi_runtime import Ok, Err, MochiResult
from mochi_runtime.collections import OrderedSet
from mochi_runtime.io import print_line

# --- module body follows ---

CONST_X: Final[int] = 42

@dataclass(frozen=True, slots=True)
class Point:
    x: int
    y: int

def add(a: Point, b: Point) -> Point:
    return Point(x=a.x + b.x, y=a.y + b.y)
```

Five blocks separated by single blank lines:

1. Module docstring (PEP 257).
2. `from __future__ import annotations` (always, every module).
3. Standard library imports, sorted.
4. Third-party imports (httpx, ...), sorted.
5. First-party imports (`mochi_runtime`, sibling modules), sorted.

Each import group separated by a single blank line. This is the
ruff isort style (`combine-as-imports = true`).

### 8.2 `from __future__ import annotations`

Always emitted. This is PEP 563 (postponed evaluation of annotations)
opt-in. Effects:

- Class annotations and function-signature annotations are stored as
  strings at runtime, not evaluated.
- Forward references work without quoting: `x: Foo` works even if
  `Foo` is defined later in the file.
- Cyclic imports (rare in Mochi-emitted code) become tolerable.
- Runtime introspection of annotations (`get_type_hints`) still
  works; it evaluates the strings on demand.

Why always, not only when needed? Because the emitter does not know
in advance whether a forward reference or cycle exists; emitting
`__future__.annotations` everywhere is the safe default. The cost
(per-import overhead) is negligible.

PEP 649 (deferred annotations, scheduled for 3.14) will subsume this
import. We track this in [[12-risks-and-alternatives]] §F2.

### 8.3 `__init__.py` for packages

A Mochi package `mypkg/foo/` with sub-modules `bar.py` and `baz.py`
gets a synthesised `__init__.py`:

```python
"""Auto-generated by Mochi compiler. Do not edit."""

from __future__ import annotations

from . import bar, baz

__all__ = ["bar", "baz"]
```

If the Mochi package declares specific re-exports, the `__init__.py`
re-exports those names individually:

```python
from .bar import some_func, SomeClass
from .baz import another_func

__all__ = ["some_func", "SomeClass", "another_func"]
```

### 8.4 `__main__.py` for executable modules

A Mochi `package myapp` with a `main` function becomes
`src/myapp/__main__.py`:

```python
"""Entry point for `python -m myapp`."""

from __future__ import annotations

import sys
import asyncio

from . import main as _main


def _run() -> int:
    return asyncio.run(_main.main(sys.argv[1:]))


if __name__ == "__main__":
    sys.exit(_run())
```

If Mochi `main` is sync (not `async fun main`), the `asyncio.run`
wrapping is omitted:

```python
def _run() -> int:
    return _main.main(sys.argv[1:])
```

---

## 9. Function lowering specifics

### 9.1 Sync vs async coloring

Python distinguishes `def` and `async def` at the syntax level
("function coloring", FAQ-3). Mochi's source-level `fun` vs `async
fun` lowers directly:

| Mochi          | Python                |
|----------------|------------------------|
| `fun f(...)` | `def f(...) -> ...:` |
| `async fun f(...)` | `async def f(...) -> ...:` |
| `fun gen() yields T` | `def gen() -> Iterator[T]:` (Python yields make it a generator) |
| `async fun gen() yields T` | `async def gen() -> AsyncIterator[T]:` |

We do **not** synthesise coloring; we do not auto-`async`-ify a sync
function or vice versa. The Mochi semantic-analysis stage has already
verified that an async function is not called without `await` from a
sync context.

### 9.2 Generators

A Mochi function body that uses `yield` lowers to a Python generator
(`def` with a `yield` statement; CPython infers the generator return
from the presence of `yield` anywhere in the body). The return-type
annotation becomes `Iterator[T]` (or `Generator[T, None, None]` if
the Mochi spec uses send/return values, which v1 does not).

For async generators (`async def` with `yield`), the return type
becomes `AsyncIterator[T]`.

### 9.3 Default arguments

Mochi function default values lower to Python defaults. The known
trap: **Python evaluates default values once at function-definition
time**, so `def f(xs=[])` shares one list across all calls. The
emitter detects this and rewrites mutable defaults:

```python
# Mochi: fun f(xs: list<int> = [])
def f(xs: list[int] | None = None) -> ...:
    if xs is None:
        xs = []
    ...
```

The rule applies to `list`, `dict`, `set`, and any user-defined
mutable record type. Immutable defaults (`int`, `str`, `bool`,
`tuple`, frozen dataclasses) pass through unchanged.

`ruff`'s `B006` rule (mutable default argument) confirms this is the
right pattern.

### 9.4 Keyword-only and positional-only arguments

Mochi's function-call surface uses positional **or** named arguments
(callers choose). Python supports the distinction via `*` and `/`
markers in the signature. The emitter does not force the distinction
unless the Mochi function has explicit annotations:

```python
def f(a: int, b: int, /, c: int, *, d: int) -> None:
    ...
```

Without explicit annotations, the emitter emits the simplest
signature (`def f(a, b, c, d)`). Callers in emitted code use
positional arguments only (since the emitter knows the signature),
so the positional/keyword distinction does not matter for Mochi-to-
Mochi calls.

### 9.5 Return type

Every function has a return-type annotation, even `-> None`. mypy
and pyright strict modes require it. The emitter never leaves it
implicit.

```python
def init() -> None:
    print_line("ready")
```

---

## 10. Record and sum-type lowering (cross-reference)

The deep dive is in [[06-type-lowering]] §3 (records) and §6 (sum
types). A summary for completeness of this codegen note:

### 10.1 Records

```python
# Mochi: record Point { x: int, y: int }
@dataclass(frozen=True, slots=True)
class Point:
    x: int
    y: int
```

`frozen=True` makes the record immutable (Mochi semantic). `slots=True`
saves memory and forbids attribute drift. The `__init__`, `__repr__`,
`__eq__`, `__hash__` are synthesised by `dataclass`.

### 10.2 Sum types

```python
# Mochi:
# sum JsonValue {
#   case JNull
#   case JBool(bool)
#   case JNum(float)
#   case JStr(string)
#   case JArr(list<JsonValue>)
#   case JObj(map<string, JsonValue>)
# }

@dataclass(frozen=True, slots=True)
class JNull: pass

@dataclass(frozen=True, slots=True)
class JBool:
    value: bool

# ... etc

type JsonValue = JNull | JBool | JNum | JStr | JArr | JObj
```

Each variant is a separate frozen dataclass. The type alias uses
PEP 695 `type` syntax. Pattern matching is via Python `match`.

---

## 11. Phase F: source maps

Source maps connect emitted Python lines to Mochi source lines.
Three artefacts:

### 11.1 Per-node SourceLoc

Every AST node carries a `SrcLoc *SourceLoc` pointer:

```go
type SourceLoc struct {
    MochiFile string
    MochiLine int
    MochiCol  int
}
```

The lowering passes (A-D) propagate `SrcLoc` from IR to AST. Phase F
walks the AST and:

1. Generates a sidecar `sourcemap.json` (the canonical Mochi source-
   map format, shared with all targets, see [[mep-0045]] §source
   maps).
2. Inserts `# pragma: mochi-src=<file>:<line>` comments at the head
   of every statement, for human-readable trace-back debugging.

### 11.2 Sidecar `sourcemap.json`

Schema (subset):

```json
{
  "version": "1",
  "mochi_file": "mypkg/foo/bar.mochi",
  "python_file": "mypkg/foo/bar.py",
  "mappings": [
    { "py_line": 5,  "mochi_line": 1, "mochi_col": 1 },
    { "py_line": 10, "mochi_line": 3, "mochi_col": 5 },
    ...
  ]
}
```

The `mappings` list is ordered by `py_line`. Tools (the Mochi error
reporter, the future `mochi debug` command) consult it to map Python
tracebacks back to Mochi source.

### 11.3 PEP 657 fine-grained tracebacks

CPython 3.11 added **column-precise traceback locations** (PEP 657).
The `compile` function emits column info into bytecode; tracebacks
show carets pointing at the failing expression. Mochi-emitted code
inherits this automatically because the Python parser-driven compile
step generates correct column info from the emitted source.

For Mochi-original column info (the Mochi expression that produced
the failing Python expression), we use the sidecar `sourcemap.json`
plus a Mochi-aware traceback formatter:

```
Traceback (most recent call last):
  File "myapp/main.py", line 10, in <module>
    result = divide(10, 0)
             ^^^^^^^^^^^^^
  Mochi: myapp/main.mochi:6:12  (let result = divide(10, 0))
ZeroDivisionError: division by zero
```

The Mochi-aware formatter is registered as a sys.excepthook in the
generated `__main__.py`.

### 11.4 `__source_loc__` class attribute

Emitted records and functions carry a class-level
`__source_loc__: SourceLoc` attribute pointing to their Mochi
definition site:

```python
@dataclass(frozen=True, slots=True)
class Point:
    x: int
    y: int

    __source_loc__: ClassVar[SourceLoc] = SourceLoc(
        file="mypkg/geom.mochi", line=3, col=1
    )
```

This is consumed by the Mochi debugger and by IDE tooling (the future
LSP integration). Not load-bearing for compilation; an aid for
tooling.

### 11.5 Doctest integration

Mochi documentation comments above functions become Python docstrings
on the emitted function. If the Mochi doc-comment includes an
`example:` block, it lowers to a doctest-runnable `>>> ` block:

Mochi:
```mochi
/// Add two ints.
///
/// example:
///   add(1, 2) == 3
fun add(a: int, b: int): int { return a + b }
```

Python:
```python
def add(a: int, b: int) -> int:
    """Add two ints.

    >>> add(1, 2)
    3
    """
    return a + b
```

`python -m doctest myapp/main.py` runs the doctest. This gives Mochi
authors a low-cost spec mechanism that pairs with `vm3` byte-equal
tests.

---

## 12. Phase G: unparse to text

The unparser walks the Go-side AST and emits Python source bytes.
The reference for layout is **`ast.unparse(node)`** from CPython 3.12
stdlib; the goal is byte-equal output before phase H runs `ruff
format`. After phase H, the source matches ruff's canonical format
regardless of unparser quirks; phase G correctness only matters for
debuggability when phase H is skipped.

### 12.1 Indentation

4 spaces, no tabs. This is PEP 8 and the ruff default.

### 12.2 Line length

`ruff format` reflows to 100 columns by default. The unparser does
not pre-format to any column width; it emits one statement per line
and lets phase H wrap.

### 12.3 Quotes

The unparser emits double quotes for strings, matching ruff's
`quote-style = "double"`. Raw strings (`r"..."`) where the Mochi
source used a raw literal.

### 12.4 Numbers

- `int`: decimal, no underscores unless Mochi source used them.
- `float`: Python literal that `float(repr(x)) == x` round-trips.
  For example, `0.1` stays `0.1` (not `0.1000000000000000055...`).
- Big ints: emit verbatim, no separators.

### 12.5 String escaping

Standard Python escapes: `\n`, `\t`, `\\`, `\"`. Unicode codepoints
above U+007F emit as literal UTF-8 in the source file (the emitter
writes UTF-8 bytes). Files declare `# -*- coding: utf-8 -*-` only if
phase H injects it; CPython 3 defaults to UTF-8 so the declaration
is unnecessary.

### 12.6 Comments

Mochi source comments emit as Python comments at corresponding
positions. Mochi doc-comments (triple-slash `///`) emit as the
function/class `__doc__` string instead.

### 12.7 Trailing newline

Every file ends with a single `\n`. ruff format enforces this.

---

## 13. Phase H: `ruff format`

ruff format (the formatter) replaces Black. We chose ruff for:

- **Speed**: ruff format is ~30x faster than Black on cold runs.
- **Single binary**: ruff format and ruff check share one Rust
  binary, distributed via `pip install ruff` or `uv tool install
  ruff`. No need for separate Black and isort installs.
- **Black-compatibility**: ruff format is intentionally a Black
  drop-in with rare exceptions documented in `ruff format`'s changelog.

### 13.1 Invocation

We shell to ruff (or call into it via FFI; see §17):

```sh
ruff format --stdin-filename foo.py - < unparsed.py > formatted.py
```

`--stdin-filename` tells ruff the virtual filename for `pyproject.toml`
config-lookup (it climbs from there).

### 13.2 Fixed-point property

After phase H, running `ruff format` again must be a no-op (bytes-
equal output). This is the **formatter fixed point**. The unparser
in phase G is allowed to be sloppy: ruff format normalises.

### 13.3 Failure mode

If ruff format errors out (parse error, malformed syntax), the
compiler reports a Mochi-internal-error pointing to the failing
input file. This is a compiler bug, not a Mochi user error.

---

## 14. Phase I: `ruff check --fix`

ruff check is the linter. Configuration is in
`pyproject.toml` under `[tool.ruff.lint]` (see [[04-runtime]] §20
for the runtime's config; emitted projects use the same shape).

### 14.1 Selected rule sets

- `E` (pycodestyle errors): syntax-level style.
- `F` (pyflakes): undefined names, unused imports, etc.
- `W` (pycodestyle warnings).
- `I` (isort): import order.
- `UP` (pyupgrade): use PEP 585/604/695 syntax.
- `B` (bugbear): common bug patterns.
- `SIM` (simplify): collapsed expressions.
- `RET` (return-statement clarity).
- `ARG` (unused arguments).
- `TCH` (type-checking imports).

### 14.2 Auto-fixable rules

`--fix` auto-applies fixes for `I` (sort imports), `UP` (upgrade
syntax), some `B` and `SIM`. We run `--fix` once; if it changes
anything, the **emitter has a bug** (the lowering should produce
already-clean code). The CI gate is `ruff check --no-fix .` (must
pass without modifications).

### 14.3 Per-file disables

The emitter never emits per-file `# noqa` comments except in two
cases:

- `# noqa: E731` on intentional `lambda` assignments (§5.3).
- `# noqa: F401` on `__init__.py` re-exports that are not used
  inside the file itself (just re-exported for `__all__`).

Both are documented patterns. The vast majority of emitted code has
no noqa comments.

---

## 15. Determinism

The emitter must produce **byte-identical output** for byte-identical
input. Sources of non-determinism we control:

### 15.1 Map iteration

Go `map` iteration is randomised by design. The emitter never
iterates a map directly when building AST; it iterates a sorted list
of keys.

### 15.2 Floating-point reprs

`repr(0.1)` is `"0.1"`, deterministic on all CPython 3.12 builds (PEP
3101 dtoa-based shortest-repr since 3.1). The emitter uses Go's
`strconv.FormatFloat(x, 'g', -1, 64)` which gives the same shortest-
repr semantics.

### 15.3 Import ordering

ruff isort produces deterministic output (alphabetical within each
group). The emitter pre-sorts imports the same way; ruff format
verifies the order.

### 15.4 Hash-set or set iteration

The emitter never iterates a Go `map[T]struct{}` set directly; it
collects to a slice and sorts.

### 15.5 Symbol numbering

Disambiguator suffixes (`foo_2`, `foo_3`) are assigned in
encounter-order during a deterministic AST walk (depth-first, in
source order). This produces stable numbers across runs.

### 15.6 SOURCE_DATE_EPOCH

For wheel-level reproducibility (mtime, zip header), see
[[10-build-system]] §reproducible builds. Not relevant to the emit
phase; relevant to the final wheel.

---

## 16. Per-block lowering details

### 16.1 `if / elif / else`

```python
if cond1:
    body1
elif cond2:
    body2
else:
    body3
```

Mochi `if c { ... } else if c2 { ... } else { ... }` lowers
directly. Python has no separate `else if`; we use `elif`.

### 16.2 `while`

```python
while cond:
    body
```

Mochi `while c { ... }` lowers directly. Python `while...else` (the
"loop completed without break" else) is not used by the emitter.

### 16.3 `for`

```python
for x in iterable:
    body
```

Mochi `for x in xs { ... }` lowers directly. For Mochi
`for i in 0..<n`, the iterable is `range(n)`. For Mochi
`for x, y in pairs`, the iterable returns 2-tuples and Python
destructures.

`for ... else` is not used.

### 16.4 `match`

See §6.

### 16.5 `try / except / finally`

Mochi does not surface exceptions directly (Result is the canonical
error model). However, FFI calls into Python libraries that raise
must be wrapped:

```python
try:
    result = some_python_lib_call()
except SomePythonError as e:
    # convert to MochiResult
    return Err(e)
```

The emitter generates this only at the FFI boundary; see
[[06-type-lowering]] §15.

### 16.6 `with`

For context managers (file handles, locks, asyncio contexts), the
emitter uses Python `with`:

```python
with open("foo.txt") as f:
    contents = f.read()

async with httpx.AsyncClient() as client:
    response = await client.get("https://example.com")
```

Mochi's `defer` lowers to `try/finally` rather than `with` because
`defer` does not bind to a context-manager protocol.

---

## 17. Tooling: ruff in-process vs subprocess

Two options for shelling to ruff:

1. **Subprocess**: spawn `ruff format` / `ruff check` per file.
   Simple, but slow at scale (process spawn overhead is ~10ms on
   Linux, ~30ms on macOS).
2. **Batched subprocess**: run `ruff format .` and `ruff check .`
   once over the whole emitted tree. Amortises spawn cost.
3. **FFI**: ruff exposes a C ABI (via the `ruff_cli` crate); the Go
   compiler could `dlopen` it. Faster but adds a build-time native-
   library dep.

We use option 2 (batched subprocess) for v1. Option 3 is a forward
performance optimisation tracked in [[12-risks-and-alternatives]] §F3.

The compiler emits the entire generated tree to a tmpdir, runs
ruff format and ruff check over the tree, and then copies it to the
final output directory. Any errors abort the compile.

---

## 18. Test fixtures and golden output

The emitter has a fixture suite at `tests/transpiler3/python/`:

```
tests/transpiler3/python/
  fixtures/
    01-hello/
      input.mochi
      expected.py
      expected_stdout.txt
    02-scalars/
      ...
    ...
  golden/
    ...
```

Each fixture has the Mochi input, the expected Python output (byte-
exact match), and the expected stdout when the Python is run. The
test runner:

1. Compiles `input.mochi` to a tmpdir.
2. Diffs the generated `.py` against `expected.py` (byte-equal).
3. Runs the generated `.py` with `python -X dev` and diffs stdout
   against `expected_stdout.txt`.
4. Runs `mypy --strict` and `pyright --strict` on the generated
   `.py`; both must pass with zero errors.
5. Runs `ruff format --check` (no changes) and `ruff check`
   (no issues).

The vm3 byte-equal master gate (compare emitted-Python's stdout to
the Mochi vm3 reference interpreter's stdout) is in [[11-testing-
gates]] §2.

---

## 19. Performance budget

The emit pass is not the compile bottleneck; types/check is. Rough
budget for a 10k-line Mochi project:

- Lower (phases A-E): 50ms.
- Emit unparse (phase G): 30ms.
- ruff format + ruff check: 150ms.
- Total: ~230ms for emit pass.

The competing target is `tsc --noEmit` on equivalent TypeScript:
~500ms. We are faster, despite the extra shell to ruff, because:

- The IR is already typed; we do not re-type-check.
- ruff is Rust, not interpreted.
- The Go emitter is single-binary, no warm-up cost.

---

## 20. Failure modes and diagnostics

When the emitter fails, we report **Mochi-aware** diagnostics:

### 20.1 mypy / pyright fails on emitted code

If mypy reports an error, the error message refers to a Python file
the user did not write. The compiler post-processes the mypy output:

```
$ mochi build --target=python ./
mypy: src/myapp/main.py:5: error: Argument 1 to "f" has type "int"; expected "str"
mochi: myapp/main.mochi:3:7: error: int is not compatible with str (mypy: arg 1 of f)
```

The Mochi-line resolves via the sourcemap.json sidecar. The
secondary mypy message is preserved for users who want the
Python detail.

### 20.2 ruff format fails

ruff format fails only on malformed input (the unparser emitted
something Python cannot parse). This is a compiler bug; we report:

```
mochi: internal error: ruff format rejected the emitted code.
       Please file a bug at https://github.com/mochilang/mochi.
       Mochi source: myapp/main.mochi
       Python output: /tmp/mochi-emit-12345/main.py
```

The tmpdir is preserved with `--keep-emit-tmpdir` for triage.

### 20.3 ruff check fails on auto-fix-applicable rule

If `ruff check --fix` modifies the emitted file, this is a compiler
bug (the emitter should produce already-clean code). We treat it as
a fatal:

```
mochi: internal error: emitted code triggered ruff auto-fix.
       Rule: UP037 (use PEP 695 type-parameter syntax)
       File: src/myapp/main.py
       The Mochi emitter must produce already-clean code.
```

---

## 21. Comparison to sibling MEPs' codegen

| MEP | Output    | Codegen library      | Formatter         |
|-----|-----------|----------------------|-------------------|
| 45  | C         | hand-roll (Go)       | clang-format       |
| 46  | Core Erlang | erlang-syntax (Go)   | erlfmt             |
| 47  | JVM bytecode | ASM (jniwrap)       | -                  |
| 48  | C# source | Roslyn IR (in-proc)  | dotnet format      |
| 49  | Swift     | SwiftSyntax (FFI)    | swift-format       |
| 50  | Kotlin    | KotlinPoet wrapper   | ktlint             |
| 51  | Python    | hand-roll Go pyast   | ruff format        |

The Python target is closest in shape to MEP-50 (Kotlin): both emit
high-level source via a hand-rolled AST and shell to a formatter.
Both have strict type-checker gates (kotlinc + detekt for Kotlin;
mypy + pyright for Python).

The Swift target uses Apple's official SwiftSyntax library for AST
construction; Python's analogue would be CPython's `ast` module, but
embedding a Python interpreter in the Go compiler is more weight
than rolling our own AST.

---

## 22. Future codegen directions

### 22.1 Native CPython AST via embedded Python

If the emitter ever needs round-tripping (read Python -> modify ->
write), embedding Python via cgo + CPython's stable ABI (PEP 384)
would unlock the full `ast.parse` / `ast.unparse` pipeline. We do
not need this for v1. Tracked in [[12-risks-and-alternatives]] §F4.

### 22.2 Cython / mypyc backend

A future MEP could add a Cython or mypyc compile step after emission,
to produce optimised native extensions. This is orthogonal to the
emit pass; it operates on the emitted .py files. Tracked in
[[12-risks-and-alternatives]] §F5.

### 22.3 stubs-only output

For Mochi libraries that ship to non-Mochi Python users, a future
flag `mochi build --target=python-stubs` would emit only `.pyi` stub
files. The codegen pipeline would skip phase G's function bodies
and emit `...` instead. Tracked as v2 candidate.

### 22.4 Pyodide / WASM

Pyodide (CPython compiled to WASM) would let Mochi-emitted Python
run in browsers. The runtime would need to avoid asyncio (Pyodide
has its own event loop bridge). Out of scope for v1; tracked in
[[12-risks-and-alternatives]] §R10.

---

## 23. Open questions and risks

Deferred to [[12-risks-and-alternatives]]:

1. **PEP 695 in mypy 1.10 vs pyright 1.1.380**: some edge cases
   diverge. We pin minimum versions in CI: `mypy>=1.10` and
   `pyright>=1.1.380`. Tracked §R11.
2. **ruff format breaking changes**: ruff 0.6 -> 1.0 may change
   default behaviours. We pin `ruff~=0.6.0`. Tracked §R12.
3. **`from __future__ import annotations` deprecation**: PEP 649
   may make this import unnecessary in 3.14. We will revisit the
   emit pass when 3.14 lands. Tracked §F2.
4. **Async generator cleanup**: PEP 533 (deterministic cleanup of
   asynchronous iterators) is pending. v2 candidate. Tracked §F6.
5. **CPython 3.13 free-threaded**: emitter does not change; runtime
   may need lock primitives. Tracked §F1.

---

## 24. Summary

- Pipeline: aotir IR -> monomorphise -> closure-convert ->
  match-to-decision-tree -> Go-side Python AST -> unparser ->
  `ruff format` -> `ruff check`.
- Eight named phases inside `transpiler3/python/`, four of which
  shell to `ruff`.
- The Go-side AST mirrors CPython's `ast.Module` 1:1; the unparser
  produces text that ruff format normalises.
- Source maps via sidecar `sourcemap.json` plus per-statement
  `# pragma: mochi-src=...` comments and `__source_loc__` class
  attribute.
- Determinism via sorted-key iteration, shortest-repr floats,
  ruff-determined import order, and SOURCE_DATE_EPOCH at wheel
  level.
- Test gates: byte-equal expected output, mypy --strict, pyright
  --strict, ruff format --check, ruff check, vm3 byte-equal stdout.
- Performance: ~230ms for a 10k-line project, faster than `tsc
  --noEmit` on equivalent TypeScript.

The codegen pipeline's design philosophy: **strict, deterministic,
typed**. We do not emit dynamic Python; we emit Python that mypy
strict mode accepts. We do not iterate Go maps; we iterate sorted
slices. We do not invent layout; we let ruff format prescribe it.

The result is Python source that reads as if a careful human typed
it, with PEP 695 generics, PEP 604 unions, dataclasses for records,
and `assert_never` for exhaustive matches. The companion notes
([[06-type-lowering]] for per-type details, [[04-runtime]] for the
library it imports, [[10-build-system]] for how the bytes become a
wheel) complete the picture.
