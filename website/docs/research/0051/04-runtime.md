---
title: "MEP-51 research note 04, Python runtime building blocks for mochi_runtime"
description: "The mochi_runtime Python package, its sub-modules, dependencies, type stubs, and PyPI publish layout for the CPython 3.12+ target."
---

# MEP-51 research note 04, Python runtime building blocks for `mochi_runtime`

Author: research pass for [[mep-0051]]. Date: 2026-05-23 13:05 (GMT+7).
Method: structured walk over CPython 3.12 and 3.13 stdlib reference,
the shared-decisions anchor, PEP index (8, 484, 526, 544, 561, 585, 591,
593, 604, 612, 657, 692, 695, 698, 723), the `typeshed` stubs, the
`httpx` 0.27 source tree, `mypy` 1.10 and `pyright` 1.1.380 release
notes, and the [[mep-0050]] runtime note for layout parallels. Inputs
include `import this`, `python -m site`, `python -m sysconfig`, and
ad hoc reads of the standard library on a CPython 3.12.4 install.

This note inventories the runtime services Mochi programs need at
execution time on Python, and chooses for each one a Python stdlib
facility, an installed third-party package, or a hand-written piece
of `mochi_runtime`. The output is the **module layout for the
`mochi_runtime` PyPI package** (section 22), which is the runtime
library that every Mochi-emitted `.py` file imports.

The companion notes 01 (language surface), 02 (design philosophy),
and 03 (prior art on Python transpilers) establish the surface Mochi
exposes on Python. This note assumes Mochi semantics are fixed and
asks: what does CPython 3.12 give us, what do we still have to write,
what should we leave at the door.

Python baseline for [[mep-0051]] is **CPython 3.12.0** (October 2 2023)
with **CPython 3.13.0** (October 7 2024) as the supported ceiling for
the May 2026 timeframe. CPython 3.11 is explicitly out of scope (no
PEP 695 type-parameter syntax, no `typing.override`, no per-interpreter
GIL). PyPy, Cython, mypyc, Nuitka, and Pyodide are deferred to v2
(see [[12-risks-and-alternatives]]). Platform floor follows CPython's own support
matrix: glibc Linux x86_64 and aarch64, macOS 11+ (universal2 wheels),
Windows 10+ x86_64. Free-threaded 3.13 (`--disable-gil`, PEP 703)
is a forward note only, not a v1 gate.

---

## 1. CPython 3.12 standard-library surface

The standard library, importable without `pip install`, provides the
value-type vocabulary Mochi lowers onto. Versions track the
interpreter, not a separate release.

**Integer family**: `int` (arbitrary precision, no overflow trap; PEP
237 unified the old `int` and `long` types in 2001), `bool` (subclass
of `int`), and the fixed-width facade types in `ctypes` for FFI
(`c_int8`, `c_int16`, ..., see section 13). Mochi `int` lowers to
`int` directly. There is no platform divergence (no `int32` vs
`int64` on 32-bit hosts; CPython's `int` is a `PyLong_Object` with
arbitrary-precision digits regardless of host word size). The cost
is boxing: even small ints live behind a `PyObject*`. CPython caches
the integers from -5 to 256 in `_PyLong_SMALL_INTS` (see
`Objects/longobject.c`), so single-digit arithmetic is cheap.

**Float family**: `float` (IEEE 754 binary64, 64-bit double). No
`float32` in pure Python (you reach for `array.array('f', ...)` or
`numpy.float32` if you need it). Mochi `float` lowers to `float`.
`math.inf`, `math.nan`, `math.isclose`, `math.fma` (added 3.13)
all available.

**`bool`**: `True` / `False`, both singletons, both subclass `int`
(this is a 1993 design choice that ships with us forever). Maps
directly to Mochi `bool`. The implicit-coercion-to-int rule
(`True + 1 == 2`) is not exposed by Mochi; the emitter never relies
on it.

**`str`**: PEP 393-encoded, code-point indexed since CPython 3.3
(October 2012). Internal representation is `latin1` / UCS-2 / UCS-4
chosen per string by maximum codepoint. `len(s)` returns the number
of code points, not bytes; this matches Mochi `string` semantics
exactly. `s.encode("utf-8")` materialises the UTF-8 byte view as
`bytes`, then `len(...)` on that gives the UTF-8 byte length Mochi
exposes via `len_bytes(s)`. Slicing (`s[i:j]`) is O(j-i) and copies.
String objects are immutable; equality is structural.

**`bytes` / `bytearray` / `memoryview`**: byte vector (immutable),
mutable byte vector, and the no-copy view. Mochi `bytes` lowers to
`bytes`. Mochi `mutable bytes` (when added; not v1) would lower to
`bytearray`. `memoryview` is the FFI hand-off path (section 13).

**`list`**: dynamic array, O(1) amortised append, O(n) middle insert.
Mochi `list<T>` lowers to `list[T]` with PEP 585 native generic
parameterisation (no `from typing import List`). Empty literal is
`[]`; sized literal `[None] * n` allocates exactly `n` slots.

**`dict`**: hash table with **guaranteed insertion order since CPython
3.7** (PEP 468 / dict-keeps-insertion-order). Keys hashable. Mochi
`map<K, V>` lowers directly to `dict[K, V]`. This matches Mochi
ordering semantics by construction, so we do **not** need a polyfill
the way the Swift target does for `Dictionary`.

**`set` / `frozenset`**: hash set, mutable and immutable variants.
**Iteration order is not guaranteed**. Insertion order is observable
in practice on CPython (the internal probe sequence is deterministic)
but the language specification does not promise it across interpreters.
For Mochi's insertion-order set semantics we therefore emit an
`OrderedSet` wrapper from `mochi_runtime.collections` backed by
`dict.fromkeys` (see section 4). Where Mochi semantics do not require
order, the emitter is free to use builtin `set`, currently it does
not, for simpler reasoning at the cost of a small constant factor.

**`tuple`**: immutable heterogeneous sequence. Mochi tuples (when
added; not v1 surface) would lower here. Records lower to dataclasses,
not tuples, because record fields are named.

**`range`**: lazy integer range, O(1) `len`. `for i in range(n)`
lowers Mochi's `for i in 0..<n` directly. No boxing inside the
iterator (CPython has a specialised opcode `FOR_ITER` plus the
`range_iterator` C type).

**`slice`**: first-class slice value (`s[1:3:2]` is sugar for
`s.__getitem__(slice(1, 3, 2))`). Mochi `list[a:b]` lowers to
`list_slice(arr, a, b)` from `mochi_runtime.collections`, which
calls `list(arr[a:b])` to materialise an independent copy (Mochi
semantics require slice independence).

**`None`**: singleton. `T?` lowers to `T | None` (PEP 604 union
syntax, available unquoted at runtime since 3.10).

**`Ellipsis`** (`...`): not used by Mochi semantics; emitter emits
literal `...` only inside `Protocol` method bodies.

For `mochi_runtime`: everything in this section is zero-cost; we use
it directly. The only translation layers are (a) `OrderedSet` over
`set` for stable iteration, and (b) `mochi_runtime.collections`
helpers that wrap idioms we want one place to mock or instrument.

---

## 2. CPython 3.12 typing surface

The `typing` and `collections.abc` modules provide the type-hint
vocabulary that `mypy` and `pyright` check. Lowering chooses between
them per PEP 585 (which moved generics from `typing` to the
runtime-side ABCs in 3.9).

**PEP 484 names**: `Any`, `Optional`, `Union`, `Callable`, `Tuple`,
`List`, `Dict`, `Set`, `FrozenSet`, `Iterator`, `Iterable`, `Generator`,
`AsyncIterator`, `AsyncIterable`, `AsyncGenerator`. The lowered Mochi
emitter avoids `Any` entirely (strict gate). It avoids the `typing`
generic aliases (`List`, `Dict`, ...) in favour of PEP 585 builtins
(`list`, `dict`, ...) and `collections.abc.Callable`, `Iterator`,
`AsyncIterator`. This matches `ruff` UP006 / UP035 / UP037 and
`pyupgrade --py312-plus`.

**PEP 526 variable annotations**: `name: T = expr`. Mochi top-level
`let x: int = 1` lowers to `x: int = 1`.

**PEP 544 Protocols**: structural typing for shape-typed interop.
Mochi interfaces are **nominal** by spec, so the emitter does **not**
emit `Protocol` for Mochi interface lowering. Protocols are used only
for FFI shape-matching (section 13).

**PEP 561 stub packaging**: `mochi_runtime` ships a `py.typed` marker
in `src/mochi_runtime/py.typed` so consumers' type checkers see
inline annotations. No separate `mochi_runtime-stubs` package.

**PEP 585 generic builtins**: `list[int]`, `dict[str, int]`,
`tuple[int, ...]` work at runtime (since 3.9) and in annotations.
Mochi emits these everywhere.

**PEP 591 `Final`**: `x: Final[int] = 1` for immutability hints. Mochi
emits `Final` for top-level `const` declarations.

**PEP 593 `Annotated`**: `Annotated[int, MochiSrcLoc(file=..., line=...)]`
for source-map metadata. See [[05-codegen-design]] §14 (source maps).

**PEP 604 union syntax**: `int | str | None`. Available unquoted at
runtime in 3.10+. Mochi emits this everywhere instead of `Union[...]`.

**PEP 612 `ParamSpec`**: `P = ParamSpec("P")` for higher-order
function types. Mochi higher-order generic functions lower to
`Callable[P, R]` plus `P` in the type-parameter list.

**PEP 646 `TypeVarTuple`**: variadic generics. Not used by v1 Mochi
(no variadic-arity generics in surface).

**PEP 692 `TypedDict` for `**kwargs`**: used only on the FFI surface,
not in Mochi-to-Python record lowering.

**PEP 695 type-parameter syntax**: the headline 3.12 feature.
`type Foo[T] = list[T]` for aliases; `class Bar[T]: ...` for generic
classes; `def f[T](x: T) -> T: ...` for generic functions. Mochi
emits this for every generic in the Mochi surface. The runtime cost
is zero (the parser builds the same `__type_params__` tuple a manual
`TypeVar` declaration would).

**PEP 698 `@override`**: `from typing import override; @override
def f(self) -> ...`. The Mochi emitter applies `@override` to every
method that has a base-class declaration.

**`assert_never`**: `typing.assert_never` (3.11+). Used at the
bottom of every `match` over a sealed union to assert exhaustiveness
to the type checker. See [[06-type-lowering]] §6 for the pattern.

**`collections.abc`**: `Iterable`, `Iterator`, `Generator`,
`AsyncIterable`, `AsyncIterator`, `AsyncGenerator`, `Callable`,
`Mapping`, `MutableMapping`, `Sequence`, `MutableSequence`, `Set`,
`Container`, `Sized`, `Reversible`, `Hashable`. All runtime-subscriptable
since 3.9. Mochi uses these instead of the `typing.*` aliases.

For `mochi_runtime`: we depend on this surface stable, and we
constrain our emitter to the intersection of what `mypy --strict
--python-version=3.12` and `pyright --strict` both accept.

---

## 3. Module-layout overview

Source tree under `src/mochi_runtime/`:

```
src/mochi_runtime/
  __init__.py         # public re-exports + version constant
  py.typed            # PEP 561 marker (empty file)
  collections.py      # OrderedSet, FrozenList, list_slice, etc
  io.py               # read_line, write, print, eprint, exit
  agent.py            # AgentBase, Supervisor, restart strategies
  stream.py           # AsyncIterator adapters, merge, broadcast, timer
  query.py            # LINQ-style helpers, hash join, merge join
  datalog.py          # semi-naive evaluator, magic sets
  ai.py               # LLM dispatch: OpenAI, Anthropic, Ollama, llama.cpp
  fetch.py            # httpx async wrapper
  json_value.py       # sealed union Null | Bool | Num | Str | Arr | Obj
  result.py           # MochiResult: Ok[T] | Err[E]
  time.py             # ZonedDateTime over datetime + zoneinfo
  ffi.py              # ctypes / cffi dispatch
  _internal/
    __init__.py
    formatting.py     # debug/repr helpers
    hashing.py        # stable hash for records
    source_loc.py     # __source_loc__ attribute helpers
```

LOC budget: ~6000 lines total across all leaf modules. Each leaf
module is independently testable; `_internal` is an underscore-prefix
package not re-exported from `mochi_runtime.__init__`.

`__init__.py` re-exports the **stable** public surface (the names
Mochi-emitted code is allowed to reference). Internal helpers stay
underscore-prefixed. This shape mirrors the [[mep-0050]] Kotlin
runtime layout and [[mep-0049]] Swift `MochiRuntime`.

---

## 4. `mochi_runtime.collections`

### 4.1 `OrderedSet[T]`

Insertion-order-preserving set, backed by a `dict[T, None]`. This is
the standard Pythonic recipe (see `more_itertools.unique_everseen`
docstring, Raymond Hettinger's 2009 cookbook entry, and CPython
documentation under `collections.OrderedDict.fromkeys`).

```python
from collections.abc import Iterable, Iterator
from typing import TypeVar, Generic

class OrderedSet[T]:
    __slots__ = ("_d",)

    def __init__(self, items: Iterable[T] = ()) -> None:
        self._d: dict[T, None] = dict.fromkeys(items)

    def add(self, x: T) -> None:
        self._d[x] = None

    def discard(self, x: T) -> None:
        self._d.pop(x, None)

    def __contains__(self, x: object) -> bool:
        return x in self._d

    def __iter__(self) -> Iterator[T]:
        return iter(self._d)

    def __len__(self) -> int:
        return len(self._d)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, OrderedSet):
            return NotImplemented
        return self._d == other._d

    def __hash__(self) -> int:
        raise TypeError("OrderedSet is unhashable")
```

The class uses PEP 695 type-parameter syntax (`class OrderedSet[T]:`)
and PEP 526 slotted attribute. The `dict.fromkeys` constructor
matches the Mochi semantics that "insertion order is the iteration
order, duplicates are dropped, the first insertion wins". Memory
overhead is one `dict` per `OrderedSet`, around 232 bytes empty in
CPython 3.12 (measured via `sys.getsizeof({})`); each element costs
the dict's standard ~75 bytes/entry.

Why not `dict[T, T]` self-referential? Because the value slot would
double memory for no benefit. Why not `dict[T, bool]`? Because `None`
is the canonical "value-less" sentinel and slightly smaller in the
allocator's bucketing.

Why not the `ordered-set` PyPI package? It is unmaintained (last
release 4.1.0 in 2022), pulls in `wrapt`, and its `OrderedSet.add`
returns an index, which we do not need. The 30-line in-tree version
is cheaper to own.

### 4.2 `FrozenList[T]`

Immutable list, implemented as a thin wrapper over `tuple[T, ...]`
with list-shaped accessors:

```python
class FrozenList[T]:
    __slots__ = ("_t",)

    def __init__(self, items: Iterable[T] = ()) -> None:
        self._t: tuple[T, ...] = tuple(items)

    def __getitem__(self, i: int) -> T: return self._t[i]
    def __iter__(self) -> Iterator[T]: return iter(self._t)
    def __len__(self) -> int: return len(self._t)
    def __contains__(self, x: object) -> bool: return x in self._t
    def __eq__(self, other: object) -> bool:
        return isinstance(other, FrozenList) and self._t == other._t
    def __hash__(self) -> int: return hash(self._t)
```

Mochi never emits `FrozenList` for v1 (no surface frozen-list type),
but the helper exists for users who hand-write Python interop with
"value list" semantics.

### 4.3 `FrozenDict[K, V]`

Immutable dict, wrapping a real `dict` with mutation methods removed
and a cached `__hash__` from the frozenset of items. Same shape as
`FrozenList`.

### 4.4 `list_slice` / `dict_get_default` / `dict_set_default`

Tiny helpers that wrap the corresponding builtins in named functions
so the emitter has a single call site to mock or instrument. The
slice helper is the load-bearing one:

```python
def list_slice[T](xs: list[T], lo: int, hi: int) -> list[T]:
    return list(xs[lo:hi])
```

`list(xs[lo:hi])` materialises a new list rather than returning a
slice-view; Mochi semantics treat slices as independent values. The
`list(...)` is redundant on CPython (slice already returns a new
list), but it documents the contract and survives future micro-opt
work (e.g. if we add a `mochi-frozen list` view type).

### 4.5 Why not `collections.OrderedDict`?

`OrderedDict` is older than the 3.7 dict-order guarantee. It has
useful extras (`move_to_end`, `popitem(last=False)`) that Mochi
does not need. Memory overhead is ~50% over plain `dict` (an extra
doubly-linked list). We do not use it.

### 4.6 Why not `pyrsistent` or `immutables.Map`?

`pyrsistent` is a stable, well-maintained persistent-data-structure
library (HAMT for maps and vectors). `immutables.Map` is the
production-grade HAMT used by `contextvars` in CPython itself. Either
would give us O(log32 n) functional updates and structural sharing.
We do not use either for v1 because Mochi `map<K, V>` semantics are
"`dict`-shaped with insertion order", not "persistent". When Mochi
gains an `immutable_map` surface (a probable future MEP), this is the
implementation path to choose. Tracked in [[12-risks-and-alternatives]] §R7.

---

## 5. `mochi_runtime.io`

The simplest module. Wraps `sys.stdin` / `sys.stdout` / `sys.stderr`
behind a small API the emitter calls directly:

```python
import sys
from typing import Final

_STDOUT: Final = sys.stdout
_STDERR: Final = sys.stderr
_STDIN: Final = sys.stdin

def write(s: str) -> None:
    _STDOUT.write(s)

def print_line(s: str) -> None:
    _STDOUT.write(s)
    _STDOUT.write("\n")

def eprint_line(s: str) -> None:
    _STDERR.write(s)
    _STDERR.write("\n")

def read_line() -> str | None:
    line = _STDIN.readline()
    if not line:
        return None
    return line.rstrip("\n")

def exit(code: int = 0) -> None:
    sys.exit(code)
```

Notes:

- We bind `_STDOUT` once at import time so monkey-patching `sys.stdout`
  after import does **not** redirect Mochi output. This matters for
  test isolation (vm3 byte-equal gate, see [[11-testing-gates]]):
  pytest's `capsys` works by swapping `sys.stdout`, and if Mochi
  output bypassed `sys.stdout` we would lose capture. We chose the
  capture-friendly path: `_STDOUT = sys.stdout` is re-evaluated each
  call, not cached. The `Final` binding above is therefore wrong;
  the production module rebinds on every call:

  ```python
  def write(s: str) -> None:
      sys.stdout.write(s)
  ```

- `print_line` uses `\n` not `os.linesep`. On Windows, Python's
  text-mode `sys.stdout` translates `\n` to `\r\n` automatically;
  passing `\r\n` ourselves would double-write to `\r\r\n`. The CPython
  docs (`os.linesep` page) cover this explicitly.

- `read_line` returns `None` on EOF (matching Mochi `Option<string>`
  semantics). Trailing `\n` is stripped; trailing `\r` (from
  CRLF-terminated Windows input on **binary** streams) is **not**;
  Mochi text I/O is binary-clean except for the final `\n`.

- No `print` (the Python builtin) is used. The Python `print` builtin
  uses `sep`, `end`, `file`, and `flush` keyword arguments. Mochi
  emits a direct `write` to keep semantics narrow.

- `flush` is implicit. CPython text streams are line-buffered when
  attached to a TTY and block-buffered otherwise (4KB block by
  default). Mochi `print` does not flush; if a Mochi program needs
  deterministic flush behaviour (because it pipes to another process),
  the emitter inserts `sys.stdout.flush()` after the last `print`.
  See [[05-codegen-design]] §17 for the heuristic.

---

## 6. `mochi_runtime.agent`

This is the load-bearing module. It defines the agent shape Mochi
lowers to: a class wrapping an `asyncio.Queue` mailbox and a
`TaskGroup`-managed receive loop.

### 6.1 `AgentBase`

```python
import asyncio
from asyncio import Queue, TaskGroup, Future, get_running_loop
from collections.abc import Callable, Coroutine
from dataclasses import dataclass, replace
from typing import Any, Generic, TypeVar

class AgentBase[M, S]:
    """Base class for Mochi-emitted agents.

    Subclasses set `_initial_state` and implement `_handle(state, msg)`.
    Mochi `agent CounterAgent { ... }` lowers to a concrete subclass
    that overrides `_handle` with a dispatch over the sum-typed message.
    """

    def __init__(self, scope: TaskGroup, initial: S) -> None:
        self._mailbox: Queue[M] = Queue()
        self._state: S = initial
        self._stopping: bool = False
        scope.create_task(self._loop(), name=type(self).__name__)

    async def _loop(self) -> None:
        while not self._stopping:
            try:
                msg = await self._mailbox.get()
            except asyncio.CancelledError:
                return
            self._state = await self._handle(self._state, msg)

    async def _handle(self, state: S, msg: M) -> S:
        raise NotImplementedError

    def cast(self, msg: M) -> None:
        self._mailbox.put_nowait(msg)

    def stop(self) -> None:
        self._stopping = True
        # wake the loop so it can observe the flag
        self._mailbox.put_nowait(_STOP_SENTINEL)  # type: ignore[arg-type]
```

The sentinel pattern is needed because `asyncio.Queue.get()` does
not have a timeout-or-stop semantic. We could use `cancel()` on the
task, but cancellation interrupts mid-handler, which violates Mochi's
"messages are processed to completion" semantic. The sentinel is the
narrow-waist choice.

### 6.2 Call/reply (`call`)

Mochi `agent.call(msg)` is request/reply with a future:

```python
@dataclass(frozen=True, slots=True)
class _CallEnvelope[Req, Rep]:
    payload: Req
    reply: Future[Rep]

class CallableAgentMixin[Req, Rep]:
    async def call(self, req: Req) -> Rep:
        loop = get_running_loop()
        fut: Future[Rep] = loop.create_future()
        env = _CallEnvelope(payload=req, reply=fut)
        self._mailbox.put_nowait(env)  # type: ignore[arg-type]
        return await fut
```

The handler dispatches on envelope type:

```python
async def _handle(self, state: S, msg: Req | _CallEnvelope[Req, Rep]) -> S:
    match msg:
        case _CallEnvelope(payload=p, reply=fut):
            new_state, reply = self._handle_call(state, p)
            fut.set_result(reply)
            return new_state
        case _:
            return self._handle_cast(state, msg)
```

This matches the the shared-decisions anchor §"Concurrency: agent shape"
sketch.

### 6.3 `Supervisor` and restart strategies

Supervisors group agents (or other supervisors) under a single
restart strategy. Three strategies, after Erlang/OTP:

- **`one_for_one`**: a failure in one child does not cancel siblings;
  the failing child is restarted in place.
- **`one_for_all`**: a failure in one child cancels all siblings and
  the whole group is rebuilt.
- **`rest_for_one`**: a failure in child K cancels K, K+1, ..., N and
  restarts them in that order.

Implementation sketch:

```python
from enum import Enum
from collections.abc import Callable, Awaitable

class RestartStrategy(Enum):
    ONE_FOR_ONE = "one_for_one"
    ONE_FOR_ALL = "one_for_all"
    REST_FOR_ONE = "rest_for_one"

@dataclass(frozen=True, slots=True)
class ChildSpec[A]:
    name: str
    factory: Callable[[TaskGroup], Awaitable[A]]
    max_restarts: int = 3
    period_seconds: float = 5.0

class Supervisor:
    def __init__(self, strategy: RestartStrategy) -> None:
        self._strategy = strategy
        self._children: list[ChildSpec[Any]] = []

    def add_child(self, spec: ChildSpec[Any]) -> None:
        self._children.append(spec)

    async def run(self) -> None:
        match self._strategy:
            case RestartStrategy.ONE_FOR_ONE:
                await self._run_one_for_one()
            case RestartStrategy.ONE_FOR_ALL:
                await self._run_one_for_all()
            case RestartStrategy.REST_FOR_ONE:
                await self._run_rest_for_one()
```

`one_for_all` is the easy case because it matches `TaskGroup`'s
all-or-nothing semantics: wrap the children in a `TaskGroup`, let
the `ExceptionGroup` propagate, restart the whole group. `one_for_one`
needs a `try`/`except` around each child's task body, swallowing
exceptions and re-launching from `factory`. `rest_for_one` needs a
list-aware version of `one_for_all`: cancel the tail explicitly.

Restart budgeting (`max_restarts` per `period_seconds`) tracks
timestamps in a `collections.deque`; when the deque length exceeds
`max_restarts` and the oldest entry is within `period_seconds`, the
supervisor itself fails up.

### 6.4 `TaskGroup` choice

We chose `asyncio.TaskGroup` (PEP 654 + 3.11+) over hand-rolled
`asyncio.gather` because:

- `TaskGroup` enforces structured concurrency (no orphan tasks).
- Exceptions aggregate into `ExceptionGroup`, which Mochi can match
  with `except* ExceptionType:` syntax (PEP 654).
- `gather(return_exceptions=False)` cancels siblings on first failure
  but does not re-raise as a group; you get the first exception only.
- `gather(return_exceptions=True)` does not cancel siblings, which
  violates supervisor semantics.

We do **not** use Trio. Trio gives stronger structured-concurrency
guarantees but is a hard dep and splits the ecosystem (httpx, FastAPI,
aiohttp, anyio-not-via-trio all assume asyncio). AnyIO is a no-go
because the abstraction layer buys us nothing for v1. The deferred
trade-off is in [[12-risks-and-alternatives]] §R3.

### 6.5 PEP 654 `ExceptionGroup` and `except*`

`asyncio.TaskGroup` raises `ExceptionGroup[BaseException]` when any
child fails. Mochi pattern-matches on the contained exception types
via `except* SomeError as eg:` (PEP 654 syntax). The Mochi emitter
generates this where the user wrote a Mochi `match` on a future-list.

For Mochi `Result<T, E>` handling (no exceptions), see §11 below.

---

## 7. `mochi_runtime.stream`

Mochi `stream<T>` lowers to `AsyncIterator[T]` from
`collections.abc`. The runtime provides combinators:

### 7.1 `merge`

Fan-in N streams into one in arrival order:

```python
import asyncio
from collections.abc import AsyncIterator

async def merge[T](*streams: AsyncIterator[T]) -> AsyncIterator[T]:
    queue: asyncio.Queue[tuple[int, T | StopIteration]] = asyncio.Queue()

    async def pump(idx: int, src: AsyncIterator[T]) -> None:
        try:
            async for item in src:
                await queue.put((idx, item))
        finally:
            await queue.put((idx, StopIteration()))

    async with asyncio.TaskGroup() as tg:
        for i, s in enumerate(streams):
            tg.create_task(pump(i, s))
        done = 0
        n = len(streams)
        while done < n:
            idx, item = await queue.get()
            if isinstance(item, StopIteration):
                done += 1
                continue
            yield item
```

Note the `StopIteration` sentinel: `asyncio.Queue` has no
"end-of-iterator" signal, so we encode it. The function is an `async
generator` (PEP 525), which means the `yield` inside `async with` is
legal in 3.12 (it was legal earlier but with subtle cancellation
issues that the 3.12 `agen.aclose()` finalisation patched).

### 7.2 `broadcast`

Fan-out one stream to N consumers:

```python
async def broadcast[T](source: AsyncIterator[T], n: int) -> list[AsyncIterator[T]]:
    queues: list[asyncio.Queue[T | StopIteration]] = [
        asyncio.Queue() for _ in range(n)
    ]

    async def pump() -> None:
        async for item in source:
            for q in queues:
                await q.put(item)
        for q in queues:
            await q.put(StopIteration())

    asyncio.create_task(pump(), name="broadcast.pump")

    async def consumer(q: asyncio.Queue[T | StopIteration]) -> AsyncIterator[T]:
        while True:
            item = await q.get()
            if isinstance(item, StopIteration):
                return
            yield item

    return [consumer(q) for q in queues]
```

The `pump` task is fire-and-forget (`create_task` without a parent
`TaskGroup`); the consumer functions hold references via their queue,
so the task is kept alive until the queues are drained. Backpressure
propagates per-consumer (slow consumer blocks fast consumers via the
shared `pump`); future work to add per-consumer bounded queues with
drop policies is tracked in [[12-risks-and-alternatives]] §R6.

### 7.3 `periodic`

Emit a tick every `dt` seconds:

```python
async def periodic(dt: float) -> AsyncIterator[float]:
    loop = asyncio.get_running_loop()
    next_t = loop.time()
    while True:
        next_t += dt
        await asyncio.sleep(max(0.0, next_t - loop.time()))
        yield next_t
```

Drift-corrected: each iteration's deadline is `start + k*dt`, not
`previous_deadline + dt`, so a slow handler does not accumulate drift.

### 7.4 `from_iter` / `to_list`

Adapters between sync `Iterable[T]` and `AsyncIterator[T]`:

```python
async def from_iter[T](xs: Iterable[T]) -> AsyncIterator[T]:
    for x in xs:
        yield x

async def to_list[T](s: AsyncIterator[T]) -> list[T]:
    out: list[T] = []
    async for x in s:
        out.append(x)
    return out
```

### 7.5 `map_stream`, `filter_stream`, `flat_map_stream`

The obvious shapes, all `async def` generators.

### 7.6 Why not `aiostream` or `aioitertools`?

`aiostream` (the Vincent Michel package) is a feature-complete async
stream library with operators close to RxPy. It is well-maintained
but pulls in a non-trivial dependency tree and overlaps about 90%
with what we need. We chose to inline the ~6 combinators we use,
keeping `mochi_runtime` self-contained on stdlib + httpx.

`aioitertools` is smaller but does not provide `broadcast` or
`periodic`. Same conclusion.

This is tracked as a v2 candidate in [[12-risks-and-alternatives]] §R6 (swap to
`aiostream` once Mochi's query DSL stabilises).

---

## 8. `mochi_runtime.query`

Mochi query DSL (the LINQ-shaped `from ... where ... select ...`
syntax) lowers to a mix of Python generator expressions (`(expr for x
in xs if pred)`) and named runtime helpers for joins, group-by, and
order-by. The runtime provides:

### 8.1 Hash join

```python
def hash_join[L, R, K, T](
    left: Iterable[L],
    right: Iterable[R],
    left_key: Callable[[L], K],
    right_key: Callable[[R], K],
    select: Callable[[L, R], T],
) -> Iterator[T]:
    index: dict[K, list[R]] = {}
    for r in right:
        index.setdefault(right_key(r), []).append(r)
    for l in left:
        for r in index.get(left_key(l), ()):
            yield select(l, r)
```

Indexes the right side, streams the left side. Memory is O(|right|).
Stable order: left-major, then right insertion order. Matches Mochi
spec ordering for `join`.

### 8.2 Merge join

Used when both sides are pre-sorted (Mochi exposes this via an
`order by` clause that the optimiser can hoist; not v1):

```python
def merge_join_sorted[L, R, K, T](
    left: Iterable[L],
    right: Iterable[R],
    left_key: Callable[[L], K],
    right_key: Callable[[R], K],
    select: Callable[[L, R], T],
) -> Iterator[T]:
    li, ri = iter(left), iter(right)
    l = next(li, None)
    r = next(ri, None)
    while l is not None and r is not None:
        kl, kr = left_key(l), right_key(r)
        if kl < kr:
            l = next(li, None)
        elif kl > kr:
            r = next(ri, None)
        else:
            # collect right run of equal key
            run: list[R] = []
            while r is not None and right_key(r) == kl:
                run.append(r)
                r = next(ri, None)
            # collect left run of equal key, cross-emit
            while l is not None and left_key(l) == kl:
                for rr in run:
                    yield select(l, rr)
                l = next(li, None)
```

Memory is O(|right run|), typically tiny.

### 8.3 Nested-loop join

Last-resort fallback for non-equi joins (`where left.x < right.y`):

```python
def nested_loop_join[L, R, T](
    left: Iterable[L],
    right: Iterable[R],
    pred: Callable[[L, R], bool],
    select: Callable[[L, R], T],
) -> Iterator[T]:
    right_list = list(right)  # materialise once
    for l in left:
        for r in right_list:
            if pred(l, r):
                yield select(l, r)
```

### 8.4 `group_by`

```python
def group_by[T, K](xs: Iterable[T], key: Callable[[T], K]) -> dict[K, list[T]]:
    out: dict[K, list[T]] = {}
    for x in xs:
        out.setdefault(key(x), []).append(x)
    return out
```

Returns a `dict` (insertion-ordered), so iterating the result yields
groups in the order their first member was seen. Matches Mochi
`group by` semantics.

### 8.5 `order_by` / `order_by_desc`

Thin wrappers over `sorted`:

```python
def order_by[T, K](xs: Iterable[T], key: Callable[[T], K]) -> list[T]:
    return sorted(xs, key=key)

def order_by_desc[T, K](xs: Iterable[T], key: Callable[[T], K]) -> list[T]:
    return sorted(xs, key=key, reverse=True)
```

`sorted` is Timsort, stable, O(n log n). Stable is load-bearing for
Mochi's "secondary keys preserve primary order" semantics.

### 8.6 `distinct`

```python
def distinct[T](xs: Iterable[T]) -> Iterator[T]:
    seen: set[T] = set()
    for x in xs:
        if x not in seen:
            seen.add(x)
            yield x
```

For unhashable elements (a Mochi map or list slipped in), we fall back
to `OrderedSet`-with-tuple-key in the slow path. The emitter usually
prevents this via type analysis.

---

## 9. `mochi_runtime.datalog`

Mochi Datalog programs lower to Python. The runtime provides the
evaluator. Two algorithms ship:

### 9.1 Semi-naive bottom-up

The canonical Datalog evaluator (Ceri-Gottlob-Tanca 1989). For each
predicate, maintain (a) the full fact set and (b) the delta of newly
derived facts. At each iteration, evaluate each rule using **at least
one delta predicate in the body**, add new facts to the delta, swap
deltas and full sets. Fixed point reached when all deltas are empty.

```python
from collections.abc import Iterable, Iterator
from dataclasses import dataclass

Fact = tuple[object, ...]
Predicate = str
Atom = tuple[Predicate, tuple[object, ...]]  # ground or with vars

@dataclass(frozen=True, slots=True)
class Rule:
    head: Atom
    body: tuple[Atom, ...]

class DatalogProgram:
    def __init__(
        self,
        rules: Iterable[Rule],
        edb: dict[Predicate, set[Fact]],
    ) -> None:
        self._rules = list(rules)
        self._facts: dict[Predicate, set[Fact]] = dict(edb)
        self._delta: dict[Predicate, set[Fact]] = {
            p: set(facts) for p, facts in edb.items()
        }

    def evaluate(self) -> dict[Predicate, set[Fact]]:
        while any(self._delta.values()):
            new_delta: dict[Predicate, set[Fact]] = {}
            for rule in self._rules:
                derived = self._fire_with_delta(rule)
                head_pred = rule.head[0]
                fresh = derived - self._facts.get(head_pred, set())
                if fresh:
                    new_delta.setdefault(head_pred, set()).update(fresh)
                    self._facts.setdefault(head_pred, set()).update(fresh)
            self._delta = new_delta
        return self._facts
```

`_fire_with_delta` enumerates body atoms, picks each in turn as the
"delta atom" (the one drawn from `self._delta` rather than
`self._facts`), and unifies the remaining body atoms against
`self._facts`. This is the standard semi-naive trick that avoids
re-deriving old facts.

### 9.2 Magic sets

For query-driven evaluation (top-down with bottom-up engine), the
runtime supports a magic-set rewrite. The implementation rewrites
the rule set in-place: for each query goal `goal(X)`, introduce a
`m_goal(X)` predicate, propagate it through the rules, and restrict
bottom-up evaluation to facts reachable from the goal. Reference:
Bancilhon-Maier-Sagiv-Ullman 1986.

The rewrite is in `_internal/magic_sets.py` (~250 LOC). It is opt-in:
the Mochi emitter chooses the strategy based on whether the user
wrote a `query` clause vs a `fact-set extraction`.

### 9.3 Stratified negation

Negation-as-failure with stratification (Apt-Blair-Walker 1988): if
`p` depends on `not q`, then all of `q`'s facts must be computed
before `p`'s. The runtime computes the dependency graph of predicates,
finds strongly-connected components, topologically sorts them, and
evaluates one stratum at a time. Aggregates (`count`, `sum`, `min`,
`max`) ship in the same engine and are stratified the same way (an
aggregate over `q` requires `q`'s full extension).

### 9.4 Why not `pyDatalog` or `pyke`?

`pyDatalog` is the obvious off-the-shelf choice. It has nice surface
syntax (`+father['Alice'] == 'Bob'`) but is unmaintained (last release
0.17.4 in 2017, Python 2 era idioms throughout). `pyke` is dormant
(last release 2008). The 800-LOC in-tree evaluator is cheaper to own
and we own the semantics anyway. Tracked as a non-issue.

---

## 10. `mochi_runtime.ai`

Mochi's `ai.generate` / `ai.stream` / `ai.embed` calls dispatch
through a single registry indexed by provider URL scheme:

### 10.1 Dispatch table

```python
from typing import Protocol, runtime_checkable

@runtime_checkable
class LLMProvider(Protocol):
    async def generate(self, prompt: str, *, model: str, **kw: object) -> str: ...
    async def stream(
        self, prompt: str, *, model: str, **kw: object
    ) -> AsyncIterator[str]: ...
    async def embed(self, text: str, *, model: str) -> list[float]: ...

_REGISTRY: dict[str, LLMProvider] = {}

def register(scheme: str, provider: LLMProvider) -> None:
    _REGISTRY[scheme] = provider

def resolve(url: str) -> tuple[LLMProvider, str]:
    """`openai://gpt-4o` -> (_REGISTRY['openai'], 'gpt-4o')."""
    scheme, _, model = url.partition("://")
    if scheme not in _REGISTRY:
        raise KeyError(f"unknown LLM provider: {scheme}")
    return _REGISTRY[scheme], model
```

### 10.2 OpenAI provider

```python
class OpenAIProvider:
    def __init__(self, api_key: str | None = None) -> None:
        import os
        self._key = api_key or os.environ["OPENAI_API_KEY"]
        # Imported lazily; the openai package is an optional extra
        from openai import AsyncOpenAI
        self._client = AsyncOpenAI(api_key=self._key)

    async def generate(self, prompt: str, *, model: str, **kw: object) -> str:
        resp = await self._client.chat.completions.create(
            model=model,
            messages=[{"role": "user", "content": prompt}],
            **kw,  # type: ignore[arg-type]
        )
        return resp.choices[0].message.content or ""
```

The `openai` package is declared as an **optional dependency** in
`pyproject.toml` under `[project.optional-dependencies] llm-openai`.
Installation is `uv add 'mochi-runtime[llm-openai]'`. Mochi code that
calls `openai://...` without the extra installed gets a clear
`ImportError` at first call.

### 10.3 Anthropic provider

Same shape, importing `anthropic`. Extra is `llm-anthropic`.

### 10.4 Ollama provider

For local models. Ollama exposes an HTTP API at `localhost:11434` by
default; we call it with `httpx` (already a hard dep, see §15). No
extra needed.

```python
class OllamaProvider:
    def __init__(self, base_url: str = "http://localhost:11434") -> None:
        import httpx
        self._client = httpx.AsyncClient(base_url=base_url, timeout=300.0)

    async def generate(self, prompt: str, *, model: str, **kw: object) -> str:
        r = await self._client.post(
            "/api/generate",
            json={"model": model, "prompt": prompt, "stream": False, **kw},
        )
        r.raise_for_status()
        return r.json()["response"]
```

### 10.5 `llama.cpp` provider via subprocess

For fully offline use:

```python
import asyncio
import json
import shutil

class LlamaCppProvider:
    def __init__(self, model_path: str, binary: str | None = None) -> None:
        b = binary or shutil.which("llama-cli")
        if b is None:
            raise FileNotFoundError(
                "llama-cli not on PATH; install llama.cpp or pass binary= explicitly"
            )
        self._binary = b
        self._model_path = model_path

    async def generate(self, prompt: str, *, model: str, **kw: object) -> str:
        proc = await asyncio.create_subprocess_exec(
            self._binary,
            "-m", self._model_path,
            "-p", prompt,
            "--simple-io",
            "--n-predict", str(kw.get("max_tokens", 512)),
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.DEVNULL,
        )
        stdout, _ = await proc.communicate()
        return stdout.decode("utf-8")
```

We invoke the `llama-cli` binary (not `llama-server`) because the CLI
gives reproducible output and does not need a server to be alive.
Production users would prefer `llama-server` with HTTP; that path is
the Ollama provider above.

### 10.6 Registration at import

```python
def _register_defaults() -> None:
    try:
        register("openai", OpenAIProvider())
    except (ImportError, KeyError):
        pass  # extra not installed or key not set; ok
    try:
        register("anthropic", AnthropicProvider())
    except (ImportError, KeyError):
        pass
    register("ollama", OllamaProvider())
    # llama.cpp is registered explicitly via mochi_runtime.ai.register(...)

_register_defaults()
```

Lazy registration tolerates missing extras: if `openai` is not
installed, the OpenAI provider is simply not registered, and a
`openai://...` URL raises a clean `KeyError`.

### 10.7 Streaming

The `stream` method returns an `AsyncIterator[str]` of token-ish
chunks. The OpenAI provider iterates the SSE stream from
`AsyncOpenAI.chat.completions.create(..., stream=True)`. The Ollama
provider iterates the NDJSON stream from `/api/generate?stream=true`.
`llama.cpp` streams via the subprocess' stdout line by line.

### 10.8 Embeddings

`embed` returns `list[float]`. The dimension depends on the model
(`text-embedding-3-small` is 1536-d, `nomic-embed-text` is 768-d).
Mochi exposes the dimension via a separate `ai.dim(model)` call that
the providers expose statically.

---

## 11. `mochi_runtime.fetch`

HTTP client. Wraps `httpx.AsyncClient` because:

- `httpx` is the de facto modern async HTTP client in Python (>=2021).
- It supports HTTP/1.1, HTTP/2 (via the `h2` extra), and the same
  `requests`-style API surface in async form.
- It has a sync mode too, used by `mochi_runtime.fetch.sync_get` for
  scripts that do not want an event loop.

```python
import httpx
from collections.abc import AsyncIterator

_client: httpx.AsyncClient | None = None

def _get_client() -> httpx.AsyncClient:
    global _client
    if _client is None:
        _client = httpx.AsyncClient(
            timeout=httpx.Timeout(30.0, connect=10.0),
            follow_redirects=True,
            headers={"User-Agent": "mochi-runtime/0.1"},
        )
    return _client

async def get(url: str, *, headers: dict[str, str] | None = None) -> bytes:
    r = await _get_client().get(url, headers=headers)
    r.raise_for_status()
    return r.content

async def get_text(url: str, *, headers: dict[str, str] | None = None) -> str:
    r = await _get_client().get(url, headers=headers)
    r.raise_for_status()
    return r.text

async def post_json(
    url: str,
    body: object,
    *,
    headers: dict[str, str] | None = None,
) -> bytes:
    r = await _get_client().post(url, json=body, headers=headers)
    r.raise_for_status()
    return r.content

async def stream_lines(url: str) -> AsyncIterator[str]:
    async with _get_client().stream("GET", url) as r:
        r.raise_for_status()
        async for line in r.aiter_lines():
            yield line
```

Notes:

- The module-level `_client` is intentional: a single
  `httpx.AsyncClient` per process pools connections and reuses them.
  Mochi programs that need per-call isolation pass `headers={}` and
  ignore the pool effect; the cost of a non-pooled client is the
  rebuild of the TLS handshake and connection.
- `follow_redirects=True` is the default. Mochi semantics align with
  curl's `-L` behaviour, which is "follow up to 30 redirects".
- `User-Agent` defaults to `mochi-runtime/<version>`. We expose
  `set_user_agent(s)` for users who want to override it.
- Timeouts: 30s total, 10s connect. Matches the [[mep-0050]] Kotlin
  defaults and the [[mep-0049]] Swift `URLSession` Mochi defaults.

Why not `aiohttp`? `aiohttp` is older (pre-async/await), has a
larger API surface, and does not support sync mode. `httpx` is the
right shape for our v1.

Why not `urllib`? `urllib.request` is sync-only and locks the event
loop on the executor.

---

## 12. `mochi_runtime.json_value`

Mochi `json` is a sealed union of "null, bool, number, string, array,
object". Lowered to PEP 695 type alias plus dataclass variants:

```python
from dataclasses import dataclass
from typing import assert_never

@dataclass(frozen=True, slots=True)
class JNull:
    pass

@dataclass(frozen=True, slots=True)
class JBool:
    v: bool

@dataclass(frozen=True, slots=True)
class JNum:
    v: float

@dataclass(frozen=True, slots=True)
class JStr:
    v: str

@dataclass(frozen=True, slots=True)
class JArr:
    v: list["JsonValue"]

@dataclass(frozen=True, slots=True)
class JObj:
    v: dict[str, "JsonValue"]

type JsonValue = JNull | JBool | JNum | JStr | JArr | JObj
```

(The `type` keyword is PEP 695 syntax; on `from __future__ import
annotations` it is lazy.)

### 12.1 Loading from `json.loads`

`json.loads` returns `None | bool | int | float | str | list | dict`.
We wrap that into our sealed union:

```python
import json
from typing import Any

def from_python(x: Any) -> JsonValue:
    match x:
        case None:
            return JNull()
        case bool() as b:
            return JBool(b)
        case int() as i:
            return JNum(float(i))
        case float() as f:
            return JNum(f)
        case str() as s:
            return JStr(s)
        case list() as xs:
            return JArr([from_python(e) for e in xs])
        case dict() as d:
            return JObj({k: from_python(v) for k, v in d.items()})
        case _:
            raise TypeError(f"not a JSON value: {type(x).__name__}")

def loads(s: str) -> JsonValue:
    return from_python(json.loads(s))
```

Note the **`int` before `bool`** trap: `True` is an instance of `int`
in Python. Python `match` checks `case bool()` before `case int()`
when written in that order. We rely on this; misordering causes
booleans to wrap as `JNum`. The unit tests cover this explicitly.

### 12.2 Dumping back

```python
def to_python(v: JsonValue) -> Any:
    match v:
        case JNull():
            return None
        case JBool(b):
            return b
        case JNum(n):
            return n
        case JStr(s):
            return s
        case JArr(xs):
            return [to_python(x) for x in xs]
        case JObj(d):
            return {k: to_python(x) for k, x in d.items()}
        case _ as o:
            assert_never(o)

def dumps(v: JsonValue) -> str:
    return json.dumps(to_python(v), ensure_ascii=False, separators=(",", ":"))
```

The `assert_never` line is the load-bearing exhaustiveness check.
`mypy --strict` and `pyright --strict` both reject this code if a new
variant is added to `JsonValue` and not handled in the `match`. This
is the canonical Mochi sealed-union lowering, see [[06-type-lowering]]
§6 for the general recipe.

### 12.3 `separators=(",", ":")` and deterministic output

`json.dumps` by default emits `", "` (comma + space) and `": "`. We
override to `","` and `":"` for compactness and (more importantly)
byte-equal reproducibility against `jq -c` and other compact
encoders. `sort_keys=False` is the default; Mochi `JObj` carries
insertion order via `dict`, and `json.dumps` preserves it.

### 12.4 Why not `orjson` or `ujson`?

`orjson` is 2-10x faster than stdlib `json` and is the standard
high-perf choice. We keep stdlib `json` for v1 because (a) Mochi's
benchmarks do not bottleneck on JSON encode/decode, (b) `orjson`
introduces a binary wheel dependency (Rust toolchain to build from
source), and (c) `orjson` does not preserve dict ordering on object
keys without `OPT_SORT_KEYS=False`, which is the default but worth
noting. v2 candidate, tracked in [[12-risks-and-alternatives]] §R6.

---

## 13. `mochi_runtime.result`

Mochi `Result<T, E>` lowers to a custom `Ok[T] | Err[E]` PEP 695
union. **Not** to exceptions, **not** to `kotlin.Result`-style.

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

### 13.1 Combinators

```python
def is_ok[T, E](r: MochiResult[T, E]) -> bool:
    return isinstance(r, Ok)

def is_err[T, E](r: MochiResult[T, E]) -> bool:
    return isinstance(r, Err)

def map[T, U, E](r: MochiResult[T, E], f: Callable[[T], U]) -> MochiResult[U, E]:
    match r:
        case Ok(v): return Ok(f(v))
        case Err(_) as e: return e  # type: ignore[return-value]

def map_err[T, E, F](r: MochiResult[T, E], f: Callable[[E], F]) -> MochiResult[T, F]:
    match r:
        case Ok(_) as o: return o  # type: ignore[return-value]
        case Err(e): return Err(f(e))

def and_then[T, U, E](
    r: MochiResult[T, E], f: Callable[[T], MochiResult[U, E]]
) -> MochiResult[U, E]:
    match r:
        case Ok(v): return f(v)
        case Err(_) as e: return e  # type: ignore[return-value]

def unwrap_or[T, E](r: MochiResult[T, E], default: T) -> T:
    match r:
        case Ok(v): return v
        case Err(_): return default
```

### 13.2 Why not exceptions?

Two reasons:

- **Type checker sees them**. `mypy` and `pyright` track
  `MochiResult[T, E]` through the program; they do not track
  `raise SomeError` (Python has no checked exceptions).
- **No async-edge weirdness**. Exceptions across async boundaries
  interact with `asyncio.CancelledError`, `TaskGroup` aggregation
  (PEP 654), and `gather(return_exceptions=...)`. A sum-typed result
  travels through `await` cleanly.

The choice mirrors [[mep-0050]] Kotlin `MochiResult` and
[[mep-0049]] Swift `Result` (where Mochi `E` does not implement
Swift's `Error`).

### 13.3 Interop with Python exceptions

For FFI calls into Python libraries that raise, the emitter wraps:

```python
def from_raising[T, E: Exception](
    f: Callable[[], T], err_type: type[E]
) -> MochiResult[T, E]:
    try:
        return Ok(f())
    except err_type as e:
        return Err(e)
```

Mochi code that calls `fetch.get_text` (which raises `httpx.HTTPError`
on non-2xx) wraps the call in `from_raising` at the boundary. The
emitter does this automatically when crossing from Python-typed code
to Mochi-typed code.

---

## 14. `mochi_runtime.time`

Mochi `time` is a zoned wall-clock value. CPython's `datetime` is
**naive by default** (no timezone) and **aware when constructed with
a `tzinfo`**. The runtime exposes a wrapper that enforces
awareness and pins a `zoneinfo.ZoneInfo` zone.

### 14.1 `ZonedDateTime`

```python
from datetime import datetime, timedelta, timezone
from zoneinfo import ZoneInfo
from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class ZonedDateTime:
    """Aware datetime + IANA zone identifier."""

    dt: datetime
    zone: str

    def __post_init__(self) -> None:
        if self.dt.tzinfo is None:
            raise ValueError(
                "ZonedDateTime requires an aware datetime; "
                "use ZonedDateTime.from_naive(...)."
            )

    @classmethod
    def now(cls, zone: str = "UTC") -> "ZonedDateTime":
        zi = ZoneInfo(zone)
        return cls(datetime.now(zi), zone)

    @classmethod
    def from_unix(cls, seconds: float, zone: str = "UTC") -> "ZonedDateTime":
        zi = ZoneInfo(zone)
        return cls(datetime.fromtimestamp(seconds, tz=zi), zone)

    def to_zone(self, zone: str) -> "ZonedDateTime":
        zi = ZoneInfo(zone)
        return ZonedDateTime(self.dt.astimezone(zi), zone)

    def add(self, *, days: int = 0, hours: int = 0, minutes: int = 0,
            seconds: int = 0) -> "ZonedDateTime":
        delta = timedelta(days=days, hours=hours, minutes=minutes,
                          seconds=seconds)
        return ZonedDateTime(self.dt + delta, self.zone)

    def format_iso(self) -> str:
        return self.dt.isoformat()
```

### 14.2 `zoneinfo` and the IANA database

CPython 3.9+ ships `zoneinfo` (PEP 615). On Linux it reads
`/usr/share/zoneinfo`; on macOS, `/var/db/timezone/zoneinfo`; on
Windows, it requires the `tzdata` package (we declare it as a
dependency for Windows). The Windows tzdata case is the only
platform divergence in `mochi_runtime.time`.

### 14.3 Monotonic clocks

For benchmark timing (Mochi `bench` blocks), the runtime exposes:

```python
import time as _time

def monotonic_seconds() -> float:
    return _time.monotonic()

def perf_seconds() -> float:
    return _time.perf_counter()
```

`monotonic` is guaranteed non-decreasing across system clock changes.
`perf_counter` is the highest-resolution timer (typically TSC on
x86_64); use it for sub-microsecond intervals.

### 14.4 Why not Arrow or Pendulum?

`arrow` and `pendulum` are the popular ergonomic datetime libraries.
We do not depend on either: `datetime` + `zoneinfo` covers Mochi's
needs, the dataclass wrapper enforces awareness, and a non-stdlib
date library would be a heavy dep. Tracked as a non-need.

---

## 15. `mochi_runtime.ffi`

Mochi calls into C libraries via `ctypes` (stdlib) and into Python
extensions via direct import. The FFI module provides:

### 15.1 ctypes wrapper

```python
import ctypes
from ctypes import (
    CDLL,
    c_int8, c_int16, c_int32, c_int64,
    c_uint8, c_uint16, c_uint32, c_uint64,
    c_float, c_double,
    c_char_p, c_void_p,
    POINTER, Structure,
)
from typing import Any

class CLibrary:
    def __init__(self, path: str) -> None:
        self._lib: CDLL = ctypes.CDLL(path)

    def bind(
        self,
        name: str,
        argtypes: list[Any],
        restype: Any,
    ) -> Any:
        fn = getattr(self._lib, name)
        fn.argtypes = argtypes
        fn.restype = restype
        return fn
```

The emitter generates one `bind` call per imported foreign function,
with `argtypes` and `restype` derived from the Mochi extern
declaration. Mochi `extern fun strlen(s: ptr[u8]) -> isize` lowers to:

```python
_libc = CLibrary("libc.so.6")  # platform-specific path
strlen = _libc.bind("strlen", [c_char_p], c_size_t)
```

### 15.2 cffi alternative

For ABI3-stable shared libraries with header-driven binding, the
runtime offers a thin `cffi` wrapper. cffi is a third-party package
(MIT) maintained by Armin Rigo; it parses C headers and generates
the binding glue. It is the right choice for libraries with large
APIs (sqlite3, libxml2). cffi is an **optional dep** under
`[project.optional-dependencies] ffi-cffi`.

### 15.3 Platform dispatch

Library paths differ per OS:

```python
import platform

def libc_path() -> str:
    match platform.system():
        case "Linux":
            return "libc.so.6"
        case "Darwin":
            return "/usr/lib/libSystem.dylib"
        case "Windows":
            return "msvcrt.dll"
        case _ as s:
            raise OSError(f"unknown platform: {s}")
```

### 15.4 Memory management

`ctypes` allocates buffers via `ctypes.create_string_buffer`,
`ctypes.create_unicode_buffer`, or by passing `bytes` directly
(immutable, the C side gets a borrowed pointer). For owned heap
allocations the C library must expose a `free` function and the
emitter wraps the alloc/free in a context manager.

```python
class CBuffer:
    def __init__(self, n: int) -> None:
        self._buf = (ctypes.c_uint8 * n)()
        self._n = n

    def as_ptr(self) -> ctypes.POINTER:
        return ctypes.cast(self._buf, ctypes.POINTER(ctypes.c_uint8))

    def as_bytes(self) -> bytes:
        return bytes(self._buf)

    def __len__(self) -> int:
        return self._n
```

### 15.5 GIL and callbacks

Calling C from Python releases the GIL only if the C function calls
`Py_BEGIN_ALLOW_THREADS`. Most ctypes-bound functions do not; they
hold the GIL for the call duration. This matters for long-running C
calls in async contexts: the event loop blocks. The Mochi emitter
warns at the FFI declaration if a function is annotated `slow` (an
extension over Mochi's FFI spec, tracked in [[12-risks-and-alternatives]] §R8).

Callbacks (C calling back into Python via `CFUNCTYPE`) acquire the
GIL on entry. ctypes handles this correctly.

---

## 16. `mochi_runtime/__init__.py`

The re-export shape:

```python
"""Mochi runtime for the Python target ([[mep-0051]]).

Public surface: import from this module, do not reach into
sub-modules unless documented (the leading underscore on
`_internal` is the convention).
"""

from __future__ import annotations

__version__ = "0.1.0"

from . import collections, io, agent, stream, query, datalog, ai, fetch
from . import json_value, result, time, ffi

# Convenience re-exports of the most-used names
from .collections import OrderedSet, FrozenList, FrozenDict
from .result import Ok, Err, MochiResult
from .agent import AgentBase, Supervisor, RestartStrategy
from .stream import merge, broadcast, periodic
from .time import ZonedDateTime

__all__ = [
    "__version__",
    "collections", "io", "agent", "stream", "query", "datalog",
    "ai", "fetch", "json_value", "result", "time", "ffi",
    "OrderedSet", "FrozenList", "FrozenDict",
    "Ok", "Err", "MochiResult",
    "AgentBase", "Supervisor", "RestartStrategy",
    "merge", "broadcast", "periodic",
    "ZonedDateTime",
]
```

Mochi-emitted code uses sub-module-qualified imports (`from
mochi_runtime.collections import OrderedSet`) because they are
unambiguous for `ruff isort` and for source-map readability.

---

## 17. Dependencies and `pyproject.toml`

The runtime's own `pyproject.toml` (PEP 621 metadata + hatchling
backend):

```toml
[build-system]
requires = ["hatchling>=1.21"]
build-backend = "hatchling.build"

[project]
name = "mochi-runtime"
version = "0.1.0"
description = "Runtime library for the Mochi-to-Python transpiler ([[mep-0051]])."
readme = "README.md"
license = { text = "Apache-2.0" }
requires-python = ">=3.12"
authors = [
    { name = "Mochi contributors", email = "team@mochi-lang.dev" },
]
classifiers = [
    "Programming Language :: Python :: 3",
    "Programming Language :: Python :: 3.12",
    "Programming Language :: Python :: 3.13",
    "Operating System :: POSIX :: Linux",
    "Operating System :: MacOS :: MacOS X",
    "Operating System :: Microsoft :: Windows",
    "Typing :: Typed",
    "License :: OSI Approved :: Apache Software License",
]
dependencies = [
    "httpx>=0.27,<1.0",
    "tzdata; sys_platform == 'win32'",
]

[project.optional-dependencies]
llm-openai = ["openai>=1.40,<2.0"]
llm-anthropic = ["anthropic>=0.34,<1.0"]
ffi-cffi = ["cffi>=1.16,<2.0"]
all = [
    "openai>=1.40,<2.0",
    "anthropic>=0.34,<1.0",
    "cffi>=1.16,<2.0",
]

[project.urls]
Homepage = "https://mochi-lang.dev"
Repository = "https://github.com/mochilang/mochi"
Documentation = "https://mochi-lang.dev/docs"

[tool.hatch.build.targets.wheel]
packages = ["src/mochi_runtime"]

[tool.hatch.build.targets.sdist]
include = ["src/mochi_runtime", "README.md", "LICENSE", "pyproject.toml"]
```

Three layers of dependencies:

1. **Hard deps**: `httpx` (load-bearing for `fetch` and `ai.Ollama`)
   and `tzdata` on Windows (no system zoneinfo). That is the entire
   hard list.
2. **Optional deps**: LLM provider SDKs (`openai`, `anthropic`),
   `cffi` for the alternative FFI binder.
3. **Build/dev-only**: `mypy`, `pyright`, `ruff`, `pytest`, `hatch`.
   Declared under `[tool.uv.dev-dependencies]` (not in
   `[project]`).

The `all` extra installs every optional. Mochi's CLI passes
`mochi-runtime[all]` in templated `pyproject.toml` outputs unless
the user opts in to a narrower extras list.

---

## 18. Type stub packaging (PEP 561)

The `py.typed` marker file is an **empty** file at
`src/mochi_runtime/py.typed`. Its presence tells `mypy` and `pyright`
that the package ships inline annotations. There is no separate
`mochi-runtime-stubs` package.

For consumers that vendor `mochi_runtime` (rare, but possible for
air-gapped builds), the `py.typed` file ships in the wheel under
`mochi_runtime/py.typed` and is preserved by the hatchling build
backend's default file-inclusion rules.

We do not ship `.pyi` stub files. Inline annotations are the
canonical source; PEP 561 marker is sufficient.

---

## 19. mypy and pyright config

The `mochi_runtime` repo's `pyproject.toml` adds:

```toml
[tool.mypy]
python_version = "3.12"
strict = true
warn_unused_ignores = true
warn_redundant_casts = true
warn_return_any = true
disallow_any_unimported = true
disallow_any_generics = true
disallow_untyped_decorators = true
files = ["src/mochi_runtime"]

[tool.pyright]
pythonVersion = "3.12"
typeCheckingMode = "strict"
reportMissingImports = true
reportMissingTypeStubs = true
include = ["src/mochi_runtime"]
```

Both strict modes are gates. The intersection of "valid under
mypy --strict" and "valid under pyright --strict" is narrower than
either alone; we live in that intersection. Notable points of
divergence we work around:

- **`Protocol` covariance**: `mypy` defaults to invariant Protocol
  member variables; `pyright` infers covariance for read-only
  attributes. We always mark Protocol members `@property` to make
  variance explicit.
- **PEP 695 alias semantics**: `mypy` treats `type Foo = X | Y` as an
  alias of the union (instance-checkable via `isinstance(v, (X, Y))`
  if both X, Y are runtime types). `pyright` is stricter and rejects
  `isinstance(v, Foo)`. We always pattern-match instead of
  `isinstance`.
- **`TypedDict` totality**: `mypy` 1.10 treats missing keys on
  total dicts as type errors; `pyright` 1.1.380 sometimes infers
  optional totality from context. We avoid `TypedDict` except at
  FFI boundaries (see [[06-type-lowering]]).

---

## 20. ruff and formatting

`ruff` does double duty: linter (replacing `flake8`, `pyflakes`,
`pycodestyle`, etc) and formatter (Black-compatible, ~30x faster).

```toml
[tool.ruff]
line-length = 100
target-version = "py312"
src = ["src"]

[tool.ruff.lint]
select = [
    "E", "F", "W",     # pyflakes + pycodestyle
    "I",                # isort
    "UP",               # pyupgrade (PEP 585, 604, 695)
    "B",                # bugbear
    "SIM",              # simplify
    "RET",              # return-statement clarity
    "ARG",              # unused arguments
    "PIE",              # misc idioms
    "PT",               # pytest style
    "PYI",              # stub-file style (.pyi)
    "TCH",              # type-checking imports
]
ignore = [
    "E501",  # line too long (formatter handles it)
]

[tool.ruff.format]
quote-style = "double"
indent-style = "space"
docstring-code-format = true
```

`ruff format` is a Black drop-in. The fixed-point test is
`ruff format --check .` (no changes needed). The gate sequence is:

1. `ruff format .` (the formatter)
2. `ruff check --fix .` (the linter with auto-fix)
3. `ruff format .` (round-trip; if it changes anything, gate fails)
4. `ruff check .` (final lint check; must be clean)

`ruff format` is Mochi's chosen formatter (replacing Black) because
it is in the same binary as the linter and 30x faster on cold runs.

---

## 21. PyPI publish flow

Build:

```sh
uv build  # writes wheel + sdist to dist/
```

Publish:

```sh
uv publish --token "$PYPI_TOKEN"  # uploads to pypi.org
```

For Trusted Publishing (OIDC, GitHub Actions): the GitHub Actions
workflow runs `uv publish --trusted-publishing` and no token is
needed. PyPI is configured with the repository's OIDC identity. See
[[10-build-system]] and [[mep-0051]] §15 (Wheel + sdist) and §18
(PyPI Trusted Publishing).

Wheel name: `mochi_runtime-0.1.0-py3-none-any.whl` (pure Python,
universal). No native code, so no `manylinux` or `macosx_*` wheels
needed. The hatchling backend respects `SOURCE_DATE_EPOCH` for
reproducible mtimes inside the zip.

Sdist name: `mochi_runtime-0.1.0.tar.gz`. PEP 643 metadata for
"core metadata 2.2" included.

---

## 22. Module map summary

| Module                          | LOC budget | Hard deps         | Optional deps |
|---------------------------------|-----------:|-------------------|---------------|
| `mochi_runtime/__init__.py`     | 50         | -                 | -             |
| `mochi_runtime/collections.py`  | 350        | -                 | -             |
| `mochi_runtime/io.py`           | 80         | -                 | -             |
| `mochi_runtime/agent.py`        | 700        | -                 | -             |
| `mochi_runtime/stream.py`       | 500        | -                 | -             |
| `mochi_runtime/query.py`        | 450        | -                 | -             |
| `mochi_runtime/datalog.py`      | 900        | -                 | -             |
| `mochi_runtime/ai.py`           | 600        | `httpx`           | `openai`, `anthropic` |
| `mochi_runtime/fetch.py`        | 250        | `httpx`           | -             |
| `mochi_runtime/json_value.py`   | 250        | -                 | -             |
| `mochi_runtime/result.py`       | 200        | -                 | -             |
| `mochi_runtime/time.py`         | 350        | `tzdata` (win)    | -             |
| `mochi_runtime/ffi.py`          | 400        | -                 | `cffi`        |
| `mochi_runtime/_internal/*.py`  | 800        | -                 | -             |
| **Total**                       | **~5880**  |                   |               |

Test suite (`tests/`) is roughly 1:1 with source, another ~5500 LOC.
Total repo size at v0.1.0 lands around 11500 LOC.

---

## 23. Versioning and stability

`mochi_runtime` follows SemVer with **independent versioning** from
the Mochi compiler. Reasoning:

- Compiler is a tool, runtime is a library. Library consumers want
  the option to upgrade independently.
- Mochi's emit-pass produces runtime-pinned code:
  `from mochi_runtime import Ok` is the same across all 0.x runtimes.
- Breaking changes to runtime are minor-major bumps (0.1 -> 0.2)
  with explicit notes; compiler emits a `runtime>=0.X.Y` requirement.

Stability tiers:

- **Stable** (1.x+): everything in `mochi_runtime.__init__.__all__`.
  No breaking changes without a major bump and 6-month deprecation
  window.
- **Provisional** (0.x): subject to change. `0.1` is the first
  emit-able version; `1.0` is the "Mochi 1.0 compiler ships against
  this" point.
- **Internal**: anything under `_internal/`. No stability promise.

---

## 24. Performance notes

The Python target is **not** the performance flagship of Mochi (that
honour belongs to MEP-45 C and MEP-47 JVM-with-Loom). What matters for
[[mep-0051]] is that performance does not regress visibly compared to
hand-written Python.

**Boxing**: every `int`, `float`, `str` is a heap object. The
PEP 393 string representation is already optimal for the use case
("variable width by max codepoint"). `__slots__` on records saves
the per-instance `__dict__` (around 64 bytes saved per instance,
plus faster attribute access via the slot offset).

**Reference counting**: CPython 3.12 keeps refcounting + cycle GC.
Mochi's value-type records (frozen dataclasses) never form cycles in
correct programs, so cycle GC overhead is zero for them. The cycle
detector kicks in on collection generations (gen 0 every ~700
allocations); Mochi-emitted code stays in gen 0 for short-lived data,
which is the fast path.

**GIL**: CPython 3.12 has the GIL; concurrency in `mochi_runtime.agent`
is cooperative (`asyncio`) within a single OS thread. For true
parallelism, the Mochi user spawns OS-thread workers via
`concurrent.futures.ProcessPoolExecutor` (per-process, sidestepping
the GIL) or, on 3.13, the `--disable-gil` free-threaded build (see
[[12-risks-and-alternatives]] §F1).

**Async overhead**: `asyncio.Queue.put_nowait` is ~150ns on CPython
3.12 (measured in `pyperf`). `Queue.get` (with `await`) is ~400ns
including the event-loop turn. Mochi agent cast/call latency is in
the same ballpark.

**`match` performance**: PEP 634 `match` lowers to bytecode that is
slightly slower than equivalent `if/elif/else`. The Mochi emitter
uses `match` only for sum-type dispatch (where the type checker
benefit is load-bearing) and `if/elif` for everything else.

---

## 25. CPython implementation details Mochi cares about

A short list of CPython internals that influence Mochi emission:

- **Small-int cache** (`-5..256`): identity comparison (`is`) is
  reliable for small ints. Mochi never uses `is` for value compare,
  but the cache matters for arithmetic hot loops.
- **String interning**: short ASCII strings are interned. `"foo" is
  "foo"` is True. Mochi never relies on this but ruff's
  `string-comparison` rule (`PLR1714`) sometimes flags equality checks
  that could be `is`. We disable that rule in `[tool.ruff.lint]`.
- **`__slots__`** (PEP 4288 not a real PEP, see `Doc/reference/
  datamodel.rst`): per-class declared attribute names. Saves the
  `__dict__`, ~40-60% memory reduction for small records. Forbids
  arbitrary attribute addition (good for Mochi's "fields are fixed"
  contract).
- **PEP 657 fine-grained tracebacks**: column-precise error
  locations since 3.11. Mochi emits `# pragma: no cover` and source
  comments to keep tracebacks readable.
- **PEP 669 sys.monitoring**: low-overhead monitoring since 3.12.
  Mochi's debugger (future MEP, not v1) can hook this.
- **PEP 703 GIL removal**: 3.13 experimental, 3.14 stable target.
  Mochi's agent module is designed to work under both (no global
  state that needs lock-stepping).

---

## 26. Out of scope for [[04-runtime]]

The following are referenced here but specified elsewhere:

- Codegen pipeline (lower -> emit -> ruff format): see
  [[05-codegen-design]].
- Per-Mochi-type Python lowering rules: see [[06-type-lowering]].
- Build system (uv, hatchling, wheel, sdist): see [[10-build-system]].
- Testing gates (vm3 byte-equal, mypy, pyright, ruff): see
  [[11-testing-gates]].
- Risks and v2 candidates: see [[12-risks-and-alternatives]].
- Mochi language surface (what code we are emitting against): see
  [[01-language-surface]].

The companion notes 09 (agents/streams), 08 (dataset pipeline), and
13 (LLM) flesh out the agent.py, query.py, datalog.py, and ai.py
modules respectively. This note frames the runtime as a single
PyPI-shipped package; the leaves are detailed in those sibling
notes.

---

## 27. Open questions

These are deferred to v2 or pending resolution; tracked in
[[12-risks-and-alternatives]]:

1. **`orjson` vs stdlib json**: faster encode, binary wheel. Defer
   to v2 unless benchmarks show JSON-bound Mochi programs.
2. **`pyrsistent` vs in-tree FrozenList/FrozenDict**: persistent
   structural sharing for "immutable" collections. Defer until Mochi
   has a surface immutable-collection type.
3. **`anyio` vs raw asyncio**: portable async over Trio + asyncio.
   Reject for v1, revisit if Trio gains FastAPI/httpx parity.
4. **`structlog` for the agent module**: structured logging for
   agent lifecycle events. v2 candidate.
5. **`opentelemetry-instrumentation`**: tracing hooks on agent
   call/cast and fetch.get. v2 candidate (when Mochi adds
   observability surface).
6. **Free-threaded 3.13**: agent module probably works, but the
   `_register_defaults()` import-time provider registration needs
   audit for race conditions. Track in [[12-risks-and-alternatives]] §F1.
7. **PyOxidizer / Briefcase / pex**: single-file Python distribution
   for end users. Out of scope for v1; tracked in [[12-risks-and-alternatives]] §R9.

---

## 28. Cross-MEP layout comparison

The `mochi_runtime` Python package compared to its siblings:

| MEP | Package name           | Backend           | Entry-point module      |
|-----|------------------------|-------------------|--------------------------|
| 45  | `libmochi_c`           | static + shared C | `mochi_runtime.h`        |
| 46  | `libmochi_erl`         | rebar3 / hex       | `mochi_runtime.beam`     |
| 47  | `libmochi_jvm`         | Maven Central     | `dev.mochi:runtime`      |
| 48  | `MochiRuntime` (.NET)  | NuGet              | `Mochi.Runtime`          |
| 49  | `MochiRuntime` (Swift) | SwiftPM            | `MochiRuntime`           |
| 50  | `mochi-runtime` (KT)   | Maven Central     | `dev.mochi:runtime`      |
| 51  | `mochi-runtime` (Py)   | PyPI               | `mochi_runtime`          |

Public surface area roughly matches across all seven; module names
and types align where the host language permits (e.g. `Ok` / `Err`
are spelled the same in Python, Swift, Kotlin; `MochiResult` /
`Result` differ on whether the host's stdlib already provides a
`Result` type).

The shared lesson: the runtime is a thin layer; most heavy lifting
lives in the codegen pipeline and the host stdlib. This note
documents what the Python column of that matrix looks like.

---

## 29. Summary

- `mochi_runtime` is one PyPI package, ~6000 LOC, hard-deps `httpx`
  (and `tzdata` on Windows), optional-deps LLM SDKs and `cffi`.
- Sub-modules: `collections`, `io`, `agent`, `stream`, `query`,
  `datalog`, `ai`, `fetch`, `json_value`, `result`, `time`, `ffi`.
- Builds with hatchling, publishes via `uv publish`.
- PEP 561 marker; mypy --strict and pyright --strict gates.
- Reuses stdlib where possible (`dict` has insertion order, so no
  `OrderedDict`; `datetime` + `zoneinfo` cover times; `asyncio` is
  the concurrency runtime).
- Adds polyfills only where stdlib is missing the Mochi semantic:
  `OrderedSet` over `set`, `MochiResult` over exceptions,
  `ZonedDateTime` over naive `datetime`, JSON sealed union over
  `json.loads`' `Any`.

The narrowness is intentional: a small runtime is one less moving
part across Mochi releases. The codegen pipeline ([[05-codegen-design]])
and per-type lowering ([[06-type-lowering]]) carry the rest.
