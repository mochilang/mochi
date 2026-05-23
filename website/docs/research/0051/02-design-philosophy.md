---
title: "MEP-51 research note 02, Design philosophy"
description: "The 'why' behind every load-bearing MEP-51 design choice. CPython 3.12 floor, mypy + pyright strict gate, asyncio.Queue + TaskGroup, uv + hatchling, frozen-slots dataclass records, MochiResult union, Jupyter ipykernel."
---

# MEP-51 research note 02, Design philosophy

Author: research pass for MEP-51 (Mochi to Python transpiler).
Date: 2026-05-23 (GMT+7).
Sources: companion notes the shared-decisions anchor, [[01-language-surface]],
[[03-prior-art-transpilers]]; the CPython release notes for 3.10
through 3.13, the Python typing council notes on
github.com/python/typing, the Python Steering Council determinations
on PEP 703 (free-threaded) and PEP 695 (type parameter syntax), the
JetBrains-published PyCharm + mypy interop notes (2024-09), the Astral
team's uv 0.4 launch post (2024-08-20), the Astral team's ruff release
notes, the Pyodide release notes, the mypyc compiler design memos at
github.com/mypyc/mypyc, the Nuitka manual on nuitka.net, the Cython
roadmap on cython.org, the Mojo language design notes from Modular,
the Codon language paper (Shajii et al., 2023), and the MEP-45 / MEP-46
/ MEP-47 / MEP-48 / MEP-49 / MEP-50 design-philosophy notes whose
structure this note mirrors.

This note explains the load-bearing design choices behind MEP-51 and
the constraints they impose. It is the "why" companion to
[[01-language-surface]]'s "what" and [[05-codegen-design]]'s "how".

## 1. Why a Python target

Mochi already has six mature lowering targets: vm3 (the reference
tree-walker), MEP-45 (C, single-binary AOT), MEP-46 (BEAM, supervision
and hot reload), MEP-47 (JVM bytecode direct, Maven Central and Loom),
MEP-48 (.NET, NuGet and NativeAOT), MEP-49 (Swift, Apple platforms plus
Linux/Windows), and MEP-50 (Kotlin source, JVM/Android/Native/JS/Wasm).
Each target picks up an ecosystem Mochi cannot reach from the others.

The Python ecosystem is the remaining unreached pillar, and the
largest single-language library ecosystem in the world. By 2025-Q4 PyPI
hosted approximately 600,000 packages, almost twice the size of npm
(~250,000 packages with first-class type definitions) and an order of
magnitude larger than Maven Central or NuGet. The reason this matters
for Mochi:

- **Data science**: NumPy, pandas, polars, pyarrow, duckdb. Almost
  every published data analysis pipeline in the last decade has been
  Python. A Mochi-to-Python target lets Mochi-typed business logic
  drop into a Jupyter notebook, a pandas DataFrame transformation,
  or a Polars query without rewriting.
- **Machine learning**: PyTorch, TensorFlow, JAX, scikit-learn,
  HuggingFace transformers. These libraries are Python-only. A
  Mochi-typed ML pipeline that needs to call into PyTorch for a model
  training step has no choice but to expose Python interop.
- **Scientific computing**: SciPy, SymPy, AstroPy, BioPython,
  StatsModels. Hundreds of domain-specific libraries that exist
  nowhere else.
- **Web scraping and automation**: BeautifulSoup, Selenium, Playwright,
  Scrapy. Python's culture of "glue code for the messy real world"
  makes it the dominant scripting language for data extraction.
- **Web frameworks**: FastAPI, Django, Flask, Starlette, Litestar. The
  Python web framework story is mature and the FastAPI ecosystem in
  particular is the canonical "modern Python web service" stack since
  2020.
- **DevOps and infrastructure**: Ansible, SaltStack, Fabric, Boto3,
  the AWS CLI, the GCP `gcloud` SDK, the Azure CLI. All Python. A
  Mochi program that orchestrates cloud infrastructure has to call
  Python tooling.
- **Notebook computing**: Jupyter, JupyterLab, VSCode notebooks, Google
  Colab, Databricks notebooks, Mathematica's Wolfram Language
  notebooks-via-Python integration. The Jupyter Protocol (ZMQ-based)
  has become the de-facto interactive computing interface. Mochi
  shipping an ipykernel (see [[10-build-system]] §17) gives Mochi
  immediate presence in the entire notebook ecosystem.

For Mochi to be a credible data-science and ML language, Python interop
is not optional. The choices were: (a) emit Python-callable C
extensions via MEP-45's C path with a Python ctypes wrapper, (b) emit
JVM bytecode and use Jython (dead, abandoned at Python 2.7), (c) emit
Python source. Option (a) works for FFI but produces no Pythonic
debugger experience and breaks Jupyter notebook integration; option
(b) is non-starter (Jython has not had a release since 2020 and only
implements Python 2); option (c) is the path of least resistance and
the only choice that integrates naturally with the existing Python
tooling.

Therefore MEP-51: Mochi compiles to idiomatic, typed CPython 3.12+
source, drops into a uv-managed pyproject.toml project, and produces
PyPI-publishable wheels plus a Jupyter ipykernel for interactive use.

## 2. Why CPython 3.12 as the floor

CPython 3.12.0 shipped on 2023-10-02 and is the first release with
several language and tooling features that materially simplify Mochi's
codegen:

- **PEP 695 type parameter syntax** (`def f[T](xs: list[T]) -> T:`,
  `class Box[T]:`, `type Foo[T] = ...`). 3.12-only. This is the
  critical feature: without PEP 695, Mochi generics lower to
  `TypeVar("T")` plus `Generic[T]` plus class-level type variables,
  which is verbose, error-prone, and produces type-checker inference
  gaps. With PEP 695, generic lowering is trivially correct. See
  [[01-language-surface]] §2.1 and §4.
- **PEP 698 `@override`**. Marks an explicit override of a parent class
  method. Used internally in `mochi_runtime` for the few cases where
  inheritance is unavoidable (e.g., the `MochiOrderedSet` subclassing
  `collections.abc.MutableSet`).
- **PEP 669 sys.monitoring**. Low-overhead instrumentation API.
  Reserved for future profiling support, not used in v1 codegen.
- **Per-interpreter GIL** (PEP 684). Foundation for the free-threaded
  build in 3.13+; in 3.12 the per-interpreter GIL is available via the
  C API but not exposed at the Python level. Mochi-emitted code is
  naturally compatible with both per-interpreter and free-threaded
  modes because of the value-semantics contract.
- **`typing.override`** (PEP 698, exposed in `typing` since 3.12 in
  addition to the `@override` decorator). Used by the runtime.
- **`asyncio.TaskGroup`** (PEP 654, since 3.11). Stable in 3.12. The
  canonical structured-concurrency primitive; see §13.
- **`tomllib`** (since 3.11). Used by the Mochi build driver for
  `pyproject.toml` parsing.
- **f-string formalisation** (PEP 701, 3.12). f-strings are now part of
  the Python grammar (previously they were a separate sub-parser), so
  expression nesting works correctly with arbitrary string literals.
  Mochi's string interpolation lowers to f-strings without escaping
  hazards.
- **Improved error messages**. CPython 3.12 added did-you-mean
  suggestions for attribute errors, syntax errors, and import errors.
  Mochi-emitted code benefits from the improved diagnostics during
  development.

The decision to floor at 3.12 rather than 3.11 LTS trades off two
things: (a) we lose the ability to consume distributions still on
3.11 (Debian stable as of 2026-Q2 still ships 3.11 by default; Ubuntu
24.04 LTS ships 3.12; Alpine 3.20 ships 3.12; RHEL 9 ships 3.11 with
3.12 as an AppStream package). (b) we gain PEP 695 generic syntax,
which is non-trivial to back-port. PEP 695 requires changes to the
parser, the symbol table, and the type-evaluator that cannot be
polyfilled in user code.

CPython 3.13.0 (Oct 2024) and 3.14.0 (Oct 2025) are acceptable as
*upper bounds*. The default toolchain uses 3.12 for codegen and CI
matrix, with 3.13 and 3.14 as rolling secondary gates (warning-only).
3.13 brings PEP 703 `--disable-gil` builds, PEP 667 (clean frame
locals), and improved typing inference. 3.14 brings PEP 649 (deferred
annotations by default), PEP 765 (`return` in `finally` rejected), and
template strings (PEP 750).

Pre-3.12 Python (3.11 and earlier) is explicitly **out of scope**.
Mochi v1 will not emit code that runs on 3.11. The cost of supporting
3.11 (back-porting PEP 695 to TypeVar plus Generic[T], polyfilling
asyncio.TaskGroup on 3.10, etc.) outweighs the user-base benefit
given the rapid distribution adoption of 3.12.

## 3. Why mypy AND pyright strict as compile gates

CPython has no built-in static type checker. Python's PEP 484 type
hints are *advisory* at runtime: they exist in `__annotations__` for
introspection but the Python interpreter does not enforce them. To
enforce types, a separate type checker reads the source and reports
errors.

The Python ecosystem has four production-grade static type checkers:

- **mypy** (originally by Jukka Lehtosalo, 2012-present; now hosted at
  python/mypy, with the typing council oversight). The reference type
  checker; PEP 484 was co-developed with mypy. Written in Python (with
  mypyc compiling hot paths to C). Stable, slow, and the historical
  baseline. Version 1.13.0 (Nov 2024) is the v1 baseline; 1.14 (Dec
  2024) adds PEP 695 improvements.
- **pyright** (Microsoft, 2019-present; now hosted at microsoft/pyright,
  written in TypeScript). The companion type checker for VS Code's
  Pylance extension. Fast (10-50x faster than mypy on large codebases),
  with strong inference. Version 1.1.380 (Sept 2024) is the v1 baseline;
  1.1.390+ improves PEP 695 alias narrowing.
- **pytype** (Google, 2015-present; now hosted at google/pytype, written
  in Python). Google's internal type checker; the only one that does
  *inference* across untyped code (mypy and pyright require annotations,
  pytype infers types from usage). Not adopted as a Mochi gate because
  pytype's PEP 695 support lags (as of 2025-Q4, pytype is still working
  on full 3.12 support).
- **pyrefly** (Meta, 2024-launch; written in Rust, hosted at
  facebook/pyrefly). Meta's newer type checker; intended to replace
  Pyre (Meta's older type checker) over time. Status as of 2025-Q4:
  internal use at Meta, public alpha. Promising but not yet a Mochi
  gate.

The decision: Mochi-emitted code must pass **both `mypy --strict` and
`pyright --strict`**. Two type checkers, not one. The reasoning:

1. **Inference divergence**: mypy and pyright disagree on subtle cases.
   PEP 695 type aliases, `Protocol` subtype checking, `TypedDict`
   totality, generic variance, narrowing on `isinstance`, and tagged
   union exhaustiveness all have inference differences. Passing both
   checkers narrows the emitter to the *intersection* of correct typed
   Python, which is what we want: code that works for every Python
   type-checker user, not just one ecosystem's preferred tool.
2. **User base coverage**: VS Code users (the majority of Python
   developers per the 2025 Python Developer Survey) get pyright by
   default via Pylance. PyCharm users get a Pyright-equivalent
   (JetBrains' type-checker is closer to mypy in style but uses its
   own implementation). CI pipelines often use mypy. Both communities
   exist; emitting code that fails one is a poor experience.
3. **Bug-finding redundancy**: a bug in one checker's inference (and
   both have bugs) does not slip through if the other catches it.

The cost: codegen must work harder. Specifically:

- We use `from __future__ import annotations` at the top of every
  module to avoid forward-reference errors (mypy and pyright both
  handle stringified annotations).
- We never reuse a TypeVar in different scopes (PEP 695 prevents this
  by construction; the TypeVar is scoped to the def/class).
- We always emit explicit return types (not relying on inference for
  return type when both checkers' inference might diverge).
- We always emit `cast()` calls when narrowing an `object` to a known
  type at FFI boundaries, rather than relying on `isinstance`
  narrowing (which mypy and pyright handle slightly differently for
  generic types).
- We avoid `Any`. Every emitted annotation is a concrete type or a
  parameterised generic; `Any` only appears at FFI boundaries with
  explicit `cast()` to a concrete type immediately after.

See [[01-language-surface]] §16 on the "two-checker type wall"
advantage, and [[11-testing-gates]] §3 for the CI gate configuration.

## 4. Why source codegen, not bytecode or C extension

Mochi could in principle emit Python bytecode directly (skipping
`python -c "compile(...)"`) or emit C source for a CPython extension
module (skipping Python entirely for the hot path). Both alternatives
were considered and rejected for v1:

- **Direct bytecode emission**: Python bytecode is **not stable**
  across CPython versions. Every minor release (3.12 -> 3.13 -> 3.14)
  introduces new opcodes, removes old ones, or changes opcode
  semantics. The bytecode format is documented for tooling (the `dis`
  module) but is explicitly *not* an API surface. Emitting bytecode
  would tie Mochi to a specific CPython point release; emitting
  source lets one source file run on every 3.12+ interpreter,
  including future ones.

- **C extension emission (via Cython, mypyc, or hand-written)**:
  produces faster code (5-50x speedup over interpreted Python on
  typed code) but requires a C toolchain at install time, breaks the
  pure-Python wheel story (`py3-none-any` becomes
  `cp312-cp312-manylinux2014_x86_64` etc.), and is incompatible with
  Pyodide (the browser-Python WASM build). Reserved as a v2 opt-in
  (`--target=python-mypyc`); see [[12-risks-and-alternatives]] §C2.

- **AST module emission**: Python's `ast` module can build an AST and
  serialise via `ast.unparse(tree)` (since 3.9). This is the formal
  successor to `astor` (third-party). We could emit AST directly and
  call `ast.unparse`. But `ast.unparse` produces minimal whitespace
  and does not preserve comments; we'd post-process through `black`
  for formatting and `isort` for import ordering, doubling the
  toolchain. **libcst** (Instagram's Concrete Syntax Tree, hosted at
  github.com/Instagram/LibCST) preserves comments and whitespace,
  has a stable API across CPython versions, and is the canonical
  refactoring tool for the Python ecosystem.

The decision: emit Python source via **libcst** builders, then run
**ruff format** (which is Black-compatible, written in Rust by Astral,
and matches Black's output byte-for-byte for the formatting rules
Mochi uses) for final layout. ruff format is faster than Black (~30x
on a 100k-line codebase) and is the canonical Python formatter as of
2025-Q4 per the Python Developer Survey.

Two alternative IRs considered and rejected:

- **astor** (third-party AST-to-source library): superseded by
  `ast.unparse` in 3.9 and `libcst` for richer use cases. Maintained
  but not the canonical tool.
- **Black AST**: Black has its own internal AST representation, but
  it is not a public API. Using Black as a library is fragile.

See note 05 for the full codegen design.

## 5. Why frozen-slots dataclass, not Pydantic / NamedTuple / attrs / msgspec

Mochi records are immutable-by-default product types with named
fields, equality by value, hashable, and ideally memory-efficient.
The Python ecosystem has at least five candidate representations:

- **`@dataclass(frozen=True, slots=True)`** (PEP 557, slots added in
  3.10). Standard library, no third-party dependency. Synthesises
  `__init__`, `__repr__`, `__eq__`, `__hash__` (when frozen), and
  `__slots__`. Type checkers handle dataclass synthesis natively.
- **`typing.NamedTuple`** (PEP 484). A typed tuple with named fields.
  Immutable by construction, hashable, equal by value. But: positional
  iteration (a NamedTuple is a tuple, so `len(point)`, `point[0]`
  work, which is *not* what Mochi semantics want); no `__slots__`
  customisation; no support for default field values until 3.6.1, and
  even then only via class-level assignment which has quirks.
- **`attrs`** (`attrs >= 23.1`, by Hynek Schlawack since 2013). The
  precursor to `dataclasses`; richer feature set (validators,
  converters, post-init hooks, factory defaults). Third-party.
- **`pydantic`** (`pydantic >= 2.7`, by Samuel Colvin and the FastAPI
  ecosystem). Adds runtime validation: every field assignment runs
  type validators (coercing types, raising on failure). Used heavily
  in FastAPI. Third-party, runtime-validating.
- **`msgspec`** (`msgspec >= 0.18`, by Jim Crist-Harif). C-implemented
  struct types with extremely fast serialization. Third-party,
  C-extension.

The decision: **`@dataclass(frozen=True, slots=True)`** is the default.
Reasoning:

1. **Stdlib only**: no third-party dependency. Mochi's runtime should
   be as thin as possible; adding `pydantic` or `attrs` or `msgspec`
   bloats the install footprint and creates version-skew hazards.
2. **Type-checker support**: both mypy and pyright understand
   dataclass synthesis natively (the `@dataclass` decorator is on the
   typing-recognised list since mypy 0.730 and pyright 1.1.50). No
   special plugins required.
3. **Sufficient performance**: `slots=True` gives ~30% memory
   reduction vs `__dict__`-backed classes, and attribute access is
   ~20% faster due to direct slot lookup vs `__dict__` lookup.
   `msgspec` would be ~5x faster for serialization but the cost is a
   C extension dependency.
4. **No runtime validation overhead**: Mochi's type checker enforces
   types at compile time. Adding `pydantic`'s runtime validation is
   wasted work (it duplicates checks Mochi already did) and adds
   measurable overhead (~10-50 ns per field assignment).
5. **NamedTuple's positional-iteration semantics are wrong**: Mochi
   records are not tuples. A user who writes `for x in record:`
   should get a type error from Mochi, not silent iteration over
   fields. Dataclasses raise `TypeError` on `iter(obj)`, which is
   what we want.

For users who *do* want Pydantic (e.g., for FastAPI request models),
the Mochi runtime offers an opt-in `@pydantic_record` annotation that
emits Pydantic `BaseModel` instead of dataclass. Off by default;
emitted only when the user explicitly opts in. See note 06 §4.

For users who *do* want msgspec (e.g., for high-throughput
serialization), similar opt-in. See [[12-risks-and-alternatives]] §C5
for the msgspec roadmap.

## 6. Strings: PEP 393 cleanness vs all-other-targets pain

CPython's `str` is **PEP 393 variable-width** since 3.3: each `str`
object picks the smallest of latin-1 (1 byte/char), UCS-2 (2
bytes/char), or UCS-4 (4 bytes/char) based on the maximum code point.
The benefits for Mochi:

- `len(s)` is O(1) and returns the **code-point count**, matching
  Mochi's specified semantic exactly.
- `s[i]` is O(1) by code point. No surrogate-pair tax (unlike Java /
  Kotlin / JVM-hosted languages), no UTF-8 walk tax (unlike Rust's
  `&str` which is UTF-8 internally and requires a walk for code-point
  indexing).
- Iteration `for ch in s:` walks code points in order. Identical to
  Mochi semantics.

This is the **cleanest** string representation among all the
managed-runtime targets. Comparison:

| Target | Internal | `len()` | `s[i]` cost | Iteration |
|--------|----------|---------|-------------|-----------|
| Mochi (spec) | UTF-8 | code points | O(1) by code point | code points |
| MEP-49 Swift | UTF-8 (since 5.7) | code points (via `count`) | O(n) for first access | code points |
| MEP-50 Kotlin | UTF-16 | UTF-16 code units (wrong!) | UTF-16 walk for high planes | UTF-16 code units (wrong!) |
| MEP-47 JVM | UTF-16 | UTF-16 code units (wrong!) | UTF-16 walk | UTF-16 code units (wrong!) |
| MEP-48 .NET | UTF-16 | UTF-16 code units (wrong!) | UTF-16 walk | UTF-16 code units (wrong!) |
| MEP-46 BEAM | binary or list | code points (binary) | O(1) for binary | code points |
| MEP-45 C | UTF-8 (Mochi runtime) | code points (via cached length) | O(1) with index cache | code points |
| **MEP-51 Python** | **PEP 393 variable** | **code points (O(1))** | **O(1) by code point** | **code points** |

The Kotlin / JVM / .NET targets all pay a UTF-16-to-code-point
adapter cost on every Mochi string operation. The Python target pays
nothing: PEP 393 already chose the right representation in 2013.

The cost paid by CPython is a one-time allocation of a wider buffer
when a string transitions from latin-1 to UCS-2 to UCS-4 due to a
single high code point being added (a rare event for ASCII-heavy
text). This is an internal CPython concern; the user-visible
performance is excellent.

For UTF-8 boundaries (HTTP, JSON, file I/O), `str.encode("utf-8")` is
O(n) with SIMD-optimised paths on x86-64 in CPython 3.12+. The
`bytes.decode("utf-8")` reverse is similarly O(n). These are the same
costs as every other target; we are not advantaged here.

For ASCII-heavy strings (the common case in code, configuration, web
APIs), CPython's PEP 393 latin-1 backing makes `str.encode("utf-8")`
on an ASCII string essentially a memcpy. The benchmark in
[[08-dataset-pipeline]] §11 shows Mochi-on-Python string-heavy
workloads at ~1.2-1.5x the throughput of Mochi-on-Kotlin, mostly
from this PEP 393 advantage.

The downside: Python's `str` has no inline storage. Every string is a
heap-allocated object, with at minimum 24 bytes (CPython 3.12 small-
object pool overhead). For a workload that creates millions of small
strings, the per-string overhead dominates and Mochi-on-C wins by
10-50x. We accept this; Python is not the target for billion-string
workloads.

## 7. Collections: defensive copy and value semantics

Mochi's spec mandates value-semantics for collections: assigning a
list to a new variable or passing it to a function produces an
independent copy. Mutation through one binding does not affect the
other.

Python's collections are reference types: assigning `b = a` makes `b`
and `a` aliases for the same list. Mutating `b` mutates `a`. This is
the opposite of Mochi semantics and requires explicit defensive
copying at every assignment and parameter boundary.

The cost analysis:

- Every function call where a Mochi `list<T>` is passed: O(n) copy
  via `list(arg)` or `arg.copy()`. For million-element lists, this is
  a significant cost.
- Every function call where a Mochi `map<K, V>` is passed: O(n) copy
  via `dict(arg)`.
- Every assignment of a Mochi collection to a new binding: O(n) copy.

The mitigation strategy:

1. **Mochi type-checker tracks ownership**: a Mochi `let xs = [...]`
   followed by no other reference to `xs` is uniquely owned and does
   not need a defensive copy at the next call boundary. The Mochi IR
   carries this ownership information into the lowering pass; the
   pass emits the defensive copy only when the value is aliased. See
   note 05 §17.
2. **Opt-in `--no-defensive-copy` flag**: for users who promise to
   not mutate, the flag disables defensive copying. Off by default
   because it breaks Mochi's documented value-semantics guarantee.
3. **Immutable-by-default records**: a record is `frozen=True` by
   default, so passing a record to a function and the function
   mutating fields is impossible. The defensive copy only applies to
   lists / dicts / sets, not records.
4. **NumPy arrays as escape hatch**: Mochi's `numarray<T>` type lowers
   to NumPy arrays which have well-defined copy-vs-view semantics
   (`arr.copy()` vs slicing). Users who need bulk numeric performance
   opt into NumPy and accept its semantics.

The performance cost (without ownership tracking, in the v0 baseline)
is the largest single overhead of the Python target vs the C and
Native-Swift targets. See [[01-language-surface]] §3 and
[[06-type-lowering]] §11.

For comparison, the Swift target gets value semantics + copy-on-write
for free at the language level: `Array<T>` and `Dictionary<K, V>` are
value types that copy only on mutation, so the worst case is one copy
per mutation rather than one per assignment. Python has no
copy-on-write at the language level, and adding it would require a
custom collection wrapper that intercepts every assignment and
mutation (complex, slow, and breaks interop with stdlib functions).

## 8. asyncio.Queue + TaskGroup, not Trio, not AnyIO, not Pykka, not Thespian, not Ray

Mochi agents need three properties: serial mailbox processing,
isolation from caller threads (or from sibling coroutines on the same
thread), and the ability to send messages asynchronously (cast) and
synchronously (call). The Python ecosystem has many candidate
abstractions:

- **`asyncio.Queue` + `asyncio.TaskGroup`** (stdlib, since 3.11 for
  TaskGroup). The canonical structured-concurrency primitive. Selected.
- **Trio** (by Nathaniel J. Smith, 2017-present, github.com/python-trio/trio).
  An alternative async runtime with stronger structured-concurrency
  guarantees (the "nursery" abstraction predates asyncio's TaskGroup
  by ~5 years and inspired it). Trio's cancellation, supervision, and
  timeout semantics are arguably better than asyncio's. *Rejected
  for v1* because Trio is a hard dependency (the entire async
  ecosystem is asyncio-based: FastAPI, httpx, aiohttp, openai,
  anthropic SDKs all assume asyncio's event loop), and adding Trio
  forces a parallel ecosystem.
- **AnyIO** (by Alex Gronholm, 2018-present, github.com/agronholm/anyio).
  A unified abstraction over asyncio and Trio. Lets users target one
  API and choose the runtime at startup. *Rejected for v1* because
  AnyIO adds a layer of indirection for no v1 benefit; we want
  asyncio's API directly, and Mochi-emitted code calling
  `await asyncio.sleep(1)` is more recognisable than calling
  `await anyio.sleep(1)`.
- **Pykka** (gocept, since 2011). A pre-asyncio actor library
  modelled on Akka. Uses threads (not coroutines) and has not had a
  release since 2019. *Rejected*: not asyncio-based, maintenance
  uncertain.
- **Thespian** (since 2014). Actor framework with multi-process and
  multi-machine support. Heavier than what Mochi needs; Mochi agents
  are in-process by default. *Rejected*: out of scope for v1.
- **Ray** (UC Berkeley RISELab + Anyscale, since 2017). Distributed
  actor framework with strong multi-machine support. Used heavily in
  ML training (Ray Train, Ray Tune, RLlib). *Rejected for v1*: Ray is
  a heavy dependency (~200 MB install, requires Redis or similar),
  and Mochi v1 is in-process. Ray *will* be evaluated as the
  distribution backend for Mochi v2 cross-machine agents.
- **dramatiq, celery, RQ**: task queue libraries, not actor
  frameworks. Different concurrency model. *Rejected*: not what Mochi
  agents need.
- **`multiprocessing.Process` + `multiprocessing.Queue`**: stdlib
  multi-process abstraction. Heavy (process-per-actor is expensive),
  but the queue is robust. *Rejected for v1*: overkill for in-process
  agents.

The decision: **asyncio.Queue + asyncio.TaskGroup is the only
concurrency primitive**. Mochi's `agent` lowers to a custom class
backed by `asyncio.Queue[Message]` + a TaskGroup-supervised receive
loop. Mochi's `stream<T>` lowers to `AsyncIterator[T]` (an
`async def` generator). Mochi's `async fun` lowers to Python
`async def`. Mochi's `spawn f()` lowers to `tg.create_task(f())`.

The actor approach has four concrete wins:

1. **Structured concurrency via TaskGroup**: tasks created in a
   TaskGroup are children of that group; the `async with` exits only
   after all children complete; failures aggregate into an
   `ExceptionGroup` (PEP 654). This is exactly the OTP one-for-all
   supervision behaviour.
2. **Bounded queues with SUSPEND policy**: `asyncio.Queue(maxsize=N)`
   with default behaviour suspends `put()` when full, providing
   backpressure. Matches Mochi's `bounded(N)` qualifier directly.
3. **Cancellation propagation**: cancelling the TaskGroup cancels all
   children. Cancellation delivery is at `await` points (cooperative).
4. **Standard library only**: no third-party dependency. The runtime
   is thin.

The trade-off vs Trio: asyncio's TaskGroup is less strict about
cancellation delivery (you can "swallow" a CancelledError by catching
broad `Exception` and continuing; Trio raises Cancelled until you
specifically rethrow). Mochi-emitted code never catches `Exception`
broadly (the codegen always uses specific exception types), so the
loss of strictness does not bite Mochi-emitted code.

The trade-off vs distributed actor frameworks (Ray, Thespian): Mochi
v1 is in-process. Cross-machine agents are a v2 concern; the natural
v2 lowering is Ray with a thin adapter from `asyncio.Queue` to
`ray.queue`. See [[12-risks-and-alternatives]] §F3.

## 9. Why uv, not pip / Poetry / PDM / Hatch / Conda

The Python packaging ecosystem has historically been fragmented:

- **pip** (since 2008, the canonical installer). Slow (full PyPI
  resolution can take minutes for large dependency graphs), no native
  lock file (requirements.txt is a flat list, not a resolved DAG),
  no virtualenv management (you bring your own). Still works, but
  not great.
- **pip-tools** (since 2014). Adds `pip-compile` for lock file
  generation. Combined with `pip-sync` for installing the locked
  versions. Better than bare pip but still slow.
- **virtualenv** (since 2007). Creates isolated Python environments.
  Required for any non-trivial project; not user-friendly.
- **Poetry** (since 2018). All-in-one project manager: lock file
  (`poetry.lock`), virtualenv (`.venv` per project), dependency
  resolver, build backend (PEP 517 compatible), publish (to PyPI).
  Popular but slow (the resolver is Python and has been a known
  performance bottleneck since 2020).
- **PDM** (since 2020). Similar scope to Poetry, faster resolver,
  PEP 582 `__pypackages__` support (deferred since PEP 582 was
  rejected). Smaller user base.
- **Hatch** (since 2017, by Ofek Lev, now under PyPA auspice). Modern
  project manager with environment matrices, plugin ecosystem,
  hatchling build backend (PEP 517). Active and improving.
- **Conda** (Anaconda, since 2012). Cross-language package manager
  (Python + R + C libraries). Separate package index (conda-forge,
  Anaconda channel). Heavy but the standard for data science. Many
  Python data-science users live in conda environments.
- **uv** (Astral, since 2024-02; v0.4 launched 2024-08-20). Rust-
  written successor to pip + pip-tools + virtualenv + pyenv. Combines
  dependency resolution, lock file management, virtualenv creation,
  Python version management, and build orchestration into one fast
  CLI. Resolution is 10-100x faster than pip; install is 5-50x
  faster (parallelised, cached). Lock file is `uv.lock` (a PEP 751-
  compatible format).

The decision: **uv 0.4+ is the canonical build driver**. Reasoning:

1. **Speed**: 10-100x faster than the alternatives. A fresh `uv sync`
   on a 100-package project is under 5 seconds; the same with pip is
   60-120 seconds. For Mochi's iterative compile-test cycle, the
   speed compounds.
2. **All-in-one**: one binary handles everything. Users do not learn
   five tools.
3. **Reproducibility**: `uv.lock` is deterministic; the resolver
   produces the same answer given the same inputs (unlike pip's
   resolver which has non-determinism in some edge cases).
4. **PyPA-compatible**: emits standard `pyproject.toml`, uses standard
   PEP 517 build backends (hatchling preferred), publishes via
   standard PyPI APIs. No proprietary lock-in.
5. **Active development**: Astral's velocity is high; uv shipped
   weekly through 2024 and 2025.
6. **Backed by Astral's track record with ruff**: ruff (the Python
   linter+formatter) is universally adopted in the Python ecosystem
   by 2025-Q4. The team has demonstrated they can ship.

The build pipeline:

```
mochi.mochi  →  Mochi typecheck + IR
              ↓
            python codegen (libcst + ruff format)
              ↓
            pyproject.toml + src/mochi_user/*.py
              ↓
            uv sync   (install deps into .venv)
              ↓
            mypy --strict + pyright --strict + ruff check  (gates)
              ↓
            uv build  (produce wheel + sdist via hatchling)
              ↓
            uv publish --trusted-publishing  (PyPI OIDC, no token)
```

For Jupyter ipykernel:

```
mochi.mochi  →  ...
              ↓
            mochi build --target=python-ipykernel
              ↓
            ~/.local/share/jupyter/kernels/mochi/kernel.json
            + mochi_kernel/  (ipykernel-based wrapper)
```

See [[10-build-system]] for the full build pipeline.

We did *not* pick Poetry, PDM, or Hatch because:
- Poetry is too slow.
- PDM has too small a user base.
- Hatch is good but uv subsumes it (uv uses hatchling as the *build
  backend* but replaces hatch as the project manager).

We did *not* pick conda because:
- conda's package index is separate from PyPI; Mochi's emitted code
  uses PyPI packages, so conda would require a conda-forge mirror of
  every dependency.
- conda is heavy (~500 MB install).
- conda is optional for users; many Python developers do not use it.
  The data-science subset who do use conda can still install
  Mochi-emitted wheels into a conda environment (conda accepts pip
  installs).

## 10. Rejection register: PyPy / Cython / mypyc / Nuitka / Pyodide / GraalPy / MicroPython / IronPython

The Python ecosystem has many alternative runtimes and compilers.
MEP-51 explicitly rejects all of them for v1, with this rationale per
candidate:

### 10.1 PyPy

- **What it is**: alternative Python implementation in RPython, with
  a tracing JIT. Hosted at pypy.org. Currently at PyPy 7.3.17,
  compatible with Python 3.10 (PyPy lags CPython by ~2 years).
- **Why considered**: faster than CPython for long-running CPU-bound
  workloads (2-10x speedup on benchmarks).
- **Why rejected for v1**: PyPy's Python 3.12 support is not yet
  shipped (planned for PyPy 7.4 in 2026); MEP-51 floors at 3.12 for
  PEP 695 generics. Once PyPy 3.12 ships, Mochi-emitted code should
  run on PyPy unmodified (we use no PyPy-incompatible features like
  ctypes-heavy hot paths or C extension internals). PyPy 3.12 support
  is a v1.1 forward gate.
- **Compatibility risks**: PyPy's GC differs from CPython's reference
  counting; objects with `__del__` are not finalised promptly. Mochi-
  emitted code does not rely on prompt finalisation (no `__del__`
  emitted by codegen), so this should not bite.

### 10.2 Cython

- **What it is**: Python superset that compiles to C extension
  modules. Hosted at cython.org. Mature (since 2007), production-
  grade.
- **Why considered**: 5-50x speedup over interpreted CPython on
  numeric code, with optional static typing via `cdef`.
- **Why rejected for v1**:
  - Cython is a *superset* of Python; Mochi-emitted Python source is
    valid Cython source automatically, but to get the speedup the
    code needs Cython-specific `cdef` annotations. Adding Cython
    codegen is significant additional work.
  - Cython output requires a C toolchain at install time on every
    target platform. Breaks the pure-Python `py3-none-any` wheel.
  - Cython 3.0 (2023-07) introduced new type semantics that mypy and
    pyright do not understand. The two-checker gate would fail.
  - Pyodide compatibility is poor: Cython extensions need to be
    built specifically for the Pyodide wasm target, which Pyodide
    supports but with caveats.
- **v2 plan**: Mochi v2 may offer `--target=python-cython` for hot-
  path acceleration. The Mochi codegen emits `cdef` annotations
  derived from Mochi type information. See [[12-risks-and-alternatives]]
  §C1.

### 10.3 mypyc

- **What it is**: compiles mypy-annotated Python to C extensions.
  Hosted at github.com/mypyc/mypyc; mypyc is what mypy itself uses
  internally to compile its hot paths.
- **Why considered**: zero-source-change AOT compilation given valid
  type annotations. 1.5-15x speedup on typed code. Mochi already
  emits mypy-strict-compatible code.
- **Why rejected for v1**:
  - Same wheel/Pyodide concerns as Cython.
  - mypyc is "stable for internal mypy use" but not advertised as a
    general-purpose compiler (the project page explicitly warns
    against using it for production unless you're prepared to debug).
  - mypyc support for PEP 695 generics is incomplete as of mypyc 1.13
    (Nov 2024).
- **v2 plan**: Mochi v2 may offer `--target=python-mypyc`. The Mochi
  codegen is already mypy-strict compatible, so the additional
  codegen work is minimal. See [[12-risks-and-alternatives]] §C2.

### 10.4 Nuitka

- **What it is**: ahead-of-time Python compiler that produces native
  executables. Hosted at nuitka.net. Mature (since 2014).
- **Why considered**: produces single-binary executables, like Mochi's
  C target (MEP-45). 2-10x speedup on some workloads.
- **Why rejected for v1**:
  - The MEP-45 (C target) already provides single-binary executables
    for Mochi code. Adding Nuitka as a second path would duplicate
    work.
  - Nuitka's compatibility with PEP 695, asyncio.TaskGroup, and other
    3.12 features lags CPython by ~6-12 months.
  - The output binary embeds CPython, so the size advantage over
    bundled-Python distributions (PyInstaller, py2exe) is modest.
- **v2 plan**: not currently planned. Users wanting single-binary
  Python deployment can use Nuitka manually on Mochi-emitted source;
  Mochi does not officially support or test this path.

### 10.5 Pyodide

- **What it is**: CPython compiled to WebAssembly, with NumPy, SciPy,
  pandas, scikit-learn, and others ported to WASM. Hosted at
  pyodide.org. Production at JupyterLite, vscode.dev, and an
  increasing number of browser-Python applications.
- **Why considered**: lets Mochi-emitted Python code run in a browser
  without a backend. Direct competitor to Kotlin/Wasm via MEP-50 and
  C+Emscripten via MEP-45.
- **Why rejected for v1**:
  - Pyodide is *runtime*, not a codegen target. Mochi-emitted Python
    code should run in Pyodide unmodified (we use no Pyodide-
    incompatible features). The "support" we need is verifying it
    works.
  - Pyodide's stdlib is mostly complete but has gaps (some C
    extension modules are not yet ported); the relevant modules for
    Mochi (asyncio, dataclasses, tomllib) are all present.
  - Pyodide's `asyncio` event loop integration with the browser event
    loop is delicate; long-running Mochi agents in Pyodide need
    careful design.
- **v1.1 plan**: ship a `mochi build --target=python-pyodide` flag
  that bundles Mochi-emitted code with a Pyodide HTML harness. See
  [[12-risks-and-alternatives]] §C3.

### 10.6 GraalPy

- **What it is**: Python implementation on Oracle's GraalVM polyglot
  runtime. Hosted at graalvm.org. Reached Python 3.10 parity in 2024.
- **Why considered**: cross-language interop with Java, JavaScript,
  Ruby, R, all in one VM. Could let Mochi-on-Python call into Java
  libraries.
- **Why rejected for v1**: Python 3.12 support is in progress as of
  2025-Q4; not ready. MEP-47 (JVM bytecode) and MEP-50 (Kotlin
  source) already cover the JVM ecosystem.
- **v2 plan**: GraalPy 3.12 will be evaluated when it ships. For now
  out of scope.

### 10.7 MicroPython / CircuitPython

- **What it is**: Python subset implementation for microcontrollers
  (ESP32, RP2040, STM32, etc.). MicroPython since 2014; CircuitPython
  is Adafruit's fork. Both target hundreds-of-kilobyte memory
  budgets.
- **Why considered**: lets Mochi target embedded hardware.
- **Why rejected for v1**: MicroPython supports only a small Python
  subset (no `asyncio.TaskGroup`, no PEP 695 generics, limited
  `dataclasses` support). Mochi-emitted code targeting full Python
  3.12 features would not run on MicroPython. The embedded target
  is better served by MEP-45 (C) which produces direct ARM binaries.
- **v2 plan**: not currently planned.

### 10.8 IronPython

- **What it is**: Python implementation on .NET. IronPython 3.4
  released 2024-04 with Python 3.4 compatibility.
- **Why considered**: cross-language interop with .NET libraries.
- **Why rejected for v1**: IronPython lags CPython by ~8 years.
  Python 3.4 is the floor of IronPython; PEP 695 generics, asyncio
  TaskGroup, frozen dataclasses are all unavailable. MEP-48 (.NET)
  already covers the .NET ecosystem.
- **v2 plan**: not currently planned.

### 10.9 Codon

- **What it is**: high-performance compiler for a Python-like
  language. Originally academic (Shajii et al. MIT-LL, 2023), now
  commercial (Exaloop). Not Python itself; a Python-like language
  with some Python compatibility.
- **Why considered**: significant performance (10-100x over CPython)
  on numeric code; native binaries.
- **Why rejected for v1**: Codon is not Python (it diverges in
  several places, especially around the type system). Mochi-to-Codon
  would be a different codegen path, not a Python target.
- **v2 plan**: not currently planned.

### 10.10 LPython

- **What it is**: LLVM-based ahead-of-time compiler for typed Python.
  By the Lcompilers project (lpython.org). Alpha as of 2024-Q4.
- **Why considered**: AOT compilation similar in spirit to mypyc but
  to LLVM IR rather than C extensions.
- **Why rejected for v1**: alpha-quality, small user base, no
  production deployments.
- **v2 plan**: monitor; may evaluate for v2 if it reaches production
  quality.

### 10.11 Mojo

- **What it is**: Modular's Python-superset systems language. By
  Chris Lattner (creator of LLVM and Swift). Public beta since
  2024-09. Python-compatible syntax with optional typed extensions
  for AI workloads.
- **Why considered**: extremely fast (Mojo claims 35,000x speedup on
  some matrix workloads vs Python). Direct support for SIMD,
  hardware vectorisation. Strong ML-workload focus, aligned with
  Mochi's ML use cases.
- **Why rejected for v1**: Mojo's language is Python-compatible *in
  syntax* but not all Python code is valid Mojo. Mojo's stdlib is
  much smaller than CPython's. Mojo's licence is permissive but
  Modular controls the toolchain. Mojo's compiler is closed-source
  as of 2025-Q4 (the language spec is open, the compiler is not).
  These are significant deployment risks for a Mochi target.
- **v2 plan**: monitor; if Modular open-sources the compiler and Mojo
  stabilises (Mojo 1.0 expected 2026 per Modular's roadmap), Mochi
  may add a Mojo target.

### 10.12 Stackless Python

- **What it is**: a now-defunct CPython fork that added microthreads
  and channels. Last release 2017.
- **Why considered**: green-thread concurrency, conceptually similar
  to coroutines.
- **Why rejected**: defunct. The features Stackless added are now
  available via asyncio.

The rejection list is summarised in [[12-risks-and-alternatives]] §C
for the full risk register.

## 11. MochiResult union vs Python exception model

Python culture is heavily exception-based. The EAFP ("easier to ask
forgiveness than permission") principle, codified in PEP 463 and the
broader Python community, says: "try the thing; catch the exception
if it fails." `try/except` is the canonical error-handling form.

Mochi's error model is the opposite: typed errors are values, not
exceptions. A function `fun parse(s: string): AST throws ParseError`
returns either an AST or a ParseError; both are first-class values,
both are visible in the type signature, and both must be handled
explicitly at the call site.

The lowering must reconcile these two cultures. Three options:

- **Use Python exceptions**: lower `throws E` to a Python `raise E()`
  + `try/except E:` at the call site. Idiomatic Python, but loses
  Mochi's typed-error tracking. The Python type checkers do not track
  which exceptions a function can raise (Python has no `throws`
  declaration). Mochi's compile-time guarantee that "every error case
  is handled" disappears.
- **Use `kotlin.Result`-equivalent custom type**: lower `Result<T, E>`
  to a custom `MochiResult[T, E]` sum type. The user handles success
  vs failure via pattern matching. Non-idiomatic Python (Python
  programmers expect exceptions), but preserves Mochi's typed-error
  guarantee.
- **Hybrid**: emit both, with a `try!` operator that unwraps `Result`
  and raises if it's an `Err`. Lets users mix the two styles based on
  context.

The decision: **MochiResult union** with `try!` for the exception-
escape hatch. Reasoning:

1. **Type-checker support**: both mypy and pyright track
   `MochiResult[T, E]` as a discriminated union via PEP 695. Pattern
   matching on `Ok` / `Err` narrows the type correctly. Exhaustiveness
   is checked at compile time. Python exceptions do not give us any of
   this.
2. **Cross-target consistency**: every other Mochi backend (MEP-45 C,
   MEP-46 BEAM, MEP-47 JVM, MEP-48 .NET, MEP-49 Swift, MEP-50 Kotlin)
   uses either typed throws or `MochiResult`/`Result`-equivalent. The
   Python target stays in the family rather than diverging to
   exceptions.
3. **Composability**: Result values can be passed through chains of
   computation (`map`, `and_then`, `or_else`). Exceptions short-
   circuit the call stack, breaking compositional reasoning.
4. **Async safety**: in `async def` functions, a thrown exception
   crosses `await` boundaries and is caught by the surrounding event
   loop. This can produce surprising behaviour (the exception ends
   up associated with the wrong coroutine). Result values are
   straightforward async-safe.

The cost: idiomatic Python users see `MochiResult` and may find it
unusual. The mitigation:

- The Mochi user guide documents the design choice with examples
  showing the type-checker benefits.
- The `try!` operator lets users opt into exception-style propagation
  when they want it.
- At FFI boundaries that consume Python libraries throwing standard
  exceptions, the codegen wraps the FFI call in a `try/except` that
  converts to `Err`.

See [[01-language-surface]] §9.2 for the lowering details.

## 12. Why we emit `from __future__ import annotations`

PEP 563 (postponed annotations) made `from __future__ import
annotations` available in 3.7, with the intent to make it the default
in some future Python. PEP 649 was accepted in 2024 and will make
deferred annotation evaluation the default in 3.14.

For Mochi-emitted code on 3.12, the import is **mandatory** in every
module. Reasoning:

1. **Forward references**: a class definition that references another
   class defined later in the same module raises `NameError` at
   class-definition time without the future import:

   ```python
   @dataclass(frozen=True)
   class Tree:
       left: Tree  # NameError: Tree not yet defined
       right: Tree
   ```

   With `from __future__ import annotations`, the annotation is a
   string `'Tree'` at class-definition time, resolved lazily later.

2. **Cyclic module imports**: a Mochi module `foo.mochi` that
   imports from `bar.mochi`, and `bar.mochi` that imports from
   `foo.mochi` (both at the top level for type annotations) requires
   the future import to break the cycle. Without it, the imports
   would fail at module-load time.

3. **PEP 695 compatibility**: PEP 695 type aliases work with deferred
   annotations naturally (the alias body is already deferred).

4. **Performance**: with deferred annotations, evaluating a class
   does not pay the cost of evaluating its type annotations. For
   dataclass-heavy modules, this is a measurable speedup at import
   time (~5-15% on large modules).

The cost: at runtime, accessing `cls.__annotations__` returns strings,
not types. To convert back to types, use `typing.get_type_hints(cls)`.
The Mochi runtime helpers (`mochi_runtime.json.serialize`,
`mochi_runtime.dataclasses.fields`) use `get_type_hints` internally.

In 3.14 the future import becomes the default; in 3.13 it is still
opt-in but recommended. Mochi-emitted code uses it on 3.12 and 3.13,
and will continue to emit the import (harmless on 3.14+) for forward
compatibility.

## 13. Coroutines and the asyncio event loop

Python's coroutine machinery (PEP 492, `async def` and `await`)
arrived in Python 3.5 (2015). The asyncio library (PEP 3156) is the
canonical event-loop runtime since 3.4 (2014). The combination is
mature and well-understood.

The decision: Mochi-to-Python uses **asyncio as the only event loop**.
Mochi's `agent` lowers to a custom actor class backed by
`asyncio.Queue[Message]` + a TaskGroup-supervised receive loop (see
[[01-language-surface]] §6). Mochi's `stream<T>` lowers to
`AsyncIterator[T]` (an `async def` generator). Mochi's `async fun`
lowers to `async def`. Mochi's `spawn f()` lowers to
`tg.create_task(f())`.

Alternatives considered and rejected:

- **Trio**: see §8.
- **AnyIO**: see §8.
- **Threading**: Python's `threading` module gives true OS threads
  but the GIL serialises bytecode execution. For I/O-bound work
  asyncio is more efficient (one thread, no GIL contention); for
  CPU-bound work `multiprocessing` or external execution (NumPy,
  Cython extensions) is needed. Mochi agents are mostly I/O-bound;
  asyncio is the right primitive.
- **`concurrent.futures.ThreadPoolExecutor` / `ProcessPoolExecutor`**:
  useful as an offload for blocking calls (asyncio.run_in_executor
  uses them). Not the primary concurrency primitive.

The free-threaded build (PEP 703, 3.13+, opt-in via `--disable-gil`)
removes the GIL, enabling true parallel thread execution. Mochi-
emitted code is safe under free-threaded mode because:

1. Records are immutable by default (`frozen=True`).
2. Collections are defensively copied at function boundaries.
3. Agents serialise via queue (no shared mutable state across agents).
4. We never use module-level mutable state (no module-level lists or
   dicts that agents could mutate).

The combination means free-threaded execution is a transparent
performance win for Mochi-emitted code, with no semantic changes
required. The CI matrix will add free-threaded gates in v1.1.

## 14. Jupyter ipykernel as a secondary target

The Jupyter Protocol (zmq-based, since 2014) lets a language run
inside Jupyter notebooks via a kernel adapter. Python's reference
kernel implementation is ipykernel; ipykernel exposes the Jupyter
protocol over zmq and hosts a Python interpreter.

The decision: Mochi ships a **Mochi-ipykernel** that runs Mochi cells
inside JupyterLab. Implementation: the kernel adapter is a thin
Python module that wraps ipykernel, and each cell is:

1. Parsed as Mochi source.
2. Type-checked at the Mochi level.
3. Lowered to Python source (incrementally, with knowledge of
   previously-defined cells via a persisted Mochi REPL state).
4. The Python source is executed in the ipykernel's Python REPL.
5. Variable bindings from the cell persist for subsequent cells.

The kernel registration is via a kernelspec file under
`~/.local/share/jupyter/kernels/mochi/kernel.json`:

```json
{
  "argv": ["python", "-m", "mochi_kernel", "-f", "{connection_file}"],
  "display_name": "Mochi",
  "language": "mochi"
}
```

The kernel display name is "Mochi" in the Jupyter kernel chooser.
Notebook files (`.ipynb`) store Mochi cells with `"language": "mochi"`
in cell metadata.

Why this matters for Mochi: data science is Mochi's largest target
audience for the Python backend. Data scientists use Jupyter
notebooks (or VS Code's native Jupyter integration, or Google Colab,
or Databricks notebooks). A Mochi-typed data pipeline in a Jupyter
notebook gives interactive exploration with compile-time type
guarantees, which is a workflow no existing Python data science tool
provides.

See [[10-build-system]] §17 for the kernel implementation and
[[11-testing-gates]] §7 for the notebook execution gates.

## 15. The two-checker type wall

The combination of `mypy --strict` and `pyright --strict` as compile
gates produces a *narrower* lowering target than either checker alone.
Specifically, Mochi-emitted code must pass:

- **PEP 484 type hints** (the baseline)
- **PEP 526 variable annotations** (no type comments)
- **PEP 544 protocols** (where used; v1 avoids them)
- **PEP 585 generic collections** (`list[T]` not `typing.List[T]`)
- **PEP 604 union types** (`X | Y` not `typing.Union[X, Y]`)
- **PEP 612 ParamSpec** (where used)
- **PEP 621 pyproject.toml metadata**
- **PEP 654 ExceptionGroup**
- **PEP 669 sys.monitoring** (advisory)
- **PEP 692 TypedDict kwargs** (where used; v1 avoids them)
- **PEP 695 type parameter syntax** (mandatory)
- **PEP 698 @override** (where applicable)

Plus the strict-mode rules:

- No `Any` leakage (every annotation is a concrete type or generic).
- No untyped functions (every `def` has parameter and return
  annotations).
- No implicit `Optional`: `def foo(x: int = None)` is rejected;
  must be `def foo(x: int | None = None)`.
- No implicit re-export (a `from module import name` makes `name`
  internal-only unless explicitly re-exported via `__all__`).
- No comparison with `None` via `==`: must use `is None`.

The codegen produces code that satisfies all of these. The cost is a
more verbose emit; the benefit is the strongest static typing
available in any Python codebase.

The two-checker advantage in detail:

| Feature | mypy strict | pyright strict | Mochi emit |
|---------|-------------|----------------|------------|
| PEP 695 type alias narrowing | strict, sometimes too narrow | strict, slightly broader | emit conservatively to satisfy both |
| `isinstance` narrowing on generic types | bug-prone | better | emit `cast()` to disambiguate |
| `TypedDict` totality | enforces `total=True` defaults | enforces same | emit explicit `total=True` everywhere |
| Protocol structural typing | strict | strict, differs on Self type | avoid Protocol in v1 |
| Async type inference for `async def` | strict | strict, broader | emit explicit return types |
| `Final` enforcement | enforces | enforces | emit `Final` for `let` bindings |

The intersection rule: a feature is used only if both checkers handle
it correctly. The exclusion is conservative; over time as both
checkers converge on PEP 695 semantics, the emit can broaden.

## 16. The `__future__.annotations` and forward compatibility

Beyond §12's per-module mandate, the `from __future__ import
annotations` import is part of Mochi's broader forward-compatibility
strategy.

Python's PEP 563 (postponed annotations) was supposed to become the
default in 3.10, then 3.11, then 3.12. Each time it was deferred
because of backward-compatibility concerns with libraries that
inspected `__annotations__` at runtime (Pydantic v1, the original
`dataclasses` synthesis logic, etc.). PEP 649 (lazy annotations,
accepted in 2024) replaces PEP 563 as the path to default-deferred
annotations; PEP 649 is the default in 3.14.

Mochi-emitted code on 3.12 explicitly imports `from __future__ import
annotations` (the PEP 563 form). On 3.14+ the import becomes a no-op
because deferred annotations are the default. On 3.13 the import is
still meaningful.

Forward-compatibility implications:

- Mochi-emitted code on 3.12 will run on 3.14 unchanged (the
  `__future__` import is allowed and harmless).
- Mochi-emitted code on 3.14 (after a future Mochi codegen update)
  will not need the import.
- The Mochi codegen will continue to emit the import for several
  versions for backward compatibility with 3.12 / 3.13.

## 17. Reproducible builds and wheel determinism

Wheels published to PyPI should be byte-identical across two CI hosts
given the same source. Reproducibility matters for:

- **Supply chain security**: verifying a published wheel matches a
  source release.
- **Sigstore signing**: PyPI's Trusted Publishing flow signs wheels;
  consistent wheels enable signature verification.
- **CI cache hits**: identical inputs producing identical outputs
  enables CI caching.

The reproducibility mechanisms in the Mochi Python pipeline:

1. **`SOURCE_DATE_EPOCH`** (Reproducible Builds spec). Set to a
   deterministic value (the Mochi source commit timestamp) before
   `uv build`. hatchling honours `SOURCE_DATE_EPOCH` to fix wheel
   modification times to the same instant.
2. **Sorted wheel entries**: PEP 427 wheel format mandates entries
   in a specific order; hatchling sorts them deterministically.
3. **Fixed UID/GID**: wheel entries are owned by `0:0` (root),
   matching the Reproducible Builds spec.
4. **No host-specific data**: the wheel `RECORD` file contains only
   filenames and hashes; no paths, no timestamps, no environment
   variables.
5. **Deterministic Python version**: the wheel's `Requires-Python`
   tag is `>=3.12`; the `Python-Version` metadata is the build
   Python's version, which we pin via uv.
6. **ruff format fixed-point**: `ruff format` is deterministic; running
   it twice produces identical output.
7. **libcst output stability**: libcst's `Module.code` property is
   deterministic for a given CST.

The Mochi build gate `TestReproducibility` runs `mochi build` twice
on two CI hosts (one Linux x86_64, one macOS arm64) and verifies the
resulting wheel SHA-256 hashes match. See [[11-testing-gates]] §8.

Sigstore signing: PyPI's Trusted Publishing uses GitHub Actions OIDC
tokens to sign wheels with sigstore at publish time. The
`uv publish --trusted-publishing` flag handles the OIDC exchange and
the sigstore attestation. No PyPI API token is required (it is the
publisher's identity that's verified).

## 18. PEP 695 type aliases vs Union vs explicit subclassing

Mochi sum types (`type Result = Ok | Err`) have three candidate
lowerings:

1. **`Union[Ok, Err]` (PEP 484)**: the original way to express a
   tagged union. Verbose: `from typing import Union; Result = Union[Ok, Err]`.
   Pre-PEP-604 alternative.
2. **`Ok | Err` (PEP 604, 3.10+)**: cleaner syntax, same semantic as
   `Union[Ok, Err]`. The current idiomatic form.
3. **`type Result = Ok | Err` (PEP 695, 3.12+)**: type alias with
   generic-parameter scoping. The current modern form.
4. **Explicit subclassing**: define `class Result(ABC); class Ok(Result); class Err(Result)`.
   Nominal type hierarchy, but breaks `match` exhaustiveness
   checking unless `@final` and `@dataclass` annotations are used.

The decision: **PEP 695 `type Result = Ok | Err`**. Reasoning:

1. **Exhaustiveness**: mypy 1.13+ and pyright 1.1.380+ both prove
   exhaustiveness on PEP 695 aliases when matched in a `match`
   statement. Without the alias, exhaustiveness requires explicit
   `assert_never(x)` at the end (which works but is verbose).
2. **Scoping**: PEP 695 aliases support generic parameters with
   precise scoping. `type Result[T, E] = Ok[T] | Err[E]` is a
   generic type alias that both checkers handle correctly. The
   PEP 484 / PEP 604 alternatives require explicit TypeVar
   declaration which has wider scope.
3. **No third-party**: stdlib only.
4. **Forward compatibility**: PEP 695 is the future of Python typing.

The cost: PEP 695 is 3.12-only. For 3.11 compatibility, the
alternative would be `type Result = Union[Ok, Err]` plus
`from __future__ import annotations`. Since we floor at 3.12, we
take PEP 695 directly.

## 19. Why we never use `typing.Protocol` for user code

Mochi has nominal types: a record of type `Person` is *not*
interchangeable with another record that happens to have the same
fields. Two `Person` instances are equal only if they were constructed
via the `Person` constructor.

Python has both nominal types (classes) and structural types
(`typing.Protocol`, PEP 544). Protocols allow "duck typing with
types": any class with a matching set of methods/fields satisfies the
protocol, without explicit inheritance.

Mochi's nominal-type stance means Mochi-emitted code never uses
`Protocol` for user code. All user types are explicit dataclasses
with nominal identity.

The runtime uses `Protocol`-typed abstractions internally for
duck-typing the `Provider` interface in `mochi_runtime.ai`,
`mochi_runtime.ffi`, and similar plug-in points. These are not
exposed at the user level.

## 20. Cross-references

- [[01-language-surface]]: the user-visible language surface this
  philosophy explains.
- [[03-prior-art-transpilers]]: the survey of Python-from-other-
  language pipelines and Python-runtime alternatives that informed
  this rejection list.
- [[04-runtime]]: the `mochi_runtime` Python package implementation.
- [[05-codegen-design]]: how libcst is driven to produce the emit.
- [[06-type-lowering]]: per-type lowering details.
- [[07-python-target-portability]]: CPython version matrix and
  platform skew.
- [[08-dataset-pipeline]]: pandas / polars / pyarrow / duckdb
  integration paths.
- [[09-agent-streams]]: asyncio.Queue + TaskGroup details.
- [[10-build-system]]: uv + hatchling + pyproject.toml details.
- [[11-testing-gates]]: test gates including mypy --strict and
  pyright --strict.
- [[12-risks-and-alternatives]]: the full risk register and v2
  candidates (Cython, mypyc, Nuitka, Pyodide, Mojo, free-threaded).
- [[../0050/02-design-philosophy]]: the Kotlin-target design-
  philosophy analogue, closest in spirit since both target managed
  runtimes with sealed sum types and async/await.
- [[../0049/02-design-philosophy]]: the Swift analogue, sharing the
  actor-isolation and typed-error design.
- [[../0046/02-design-philosophy]]: the BEAM analogue, whose
  supervision-tree design inspired the TaskGroup-based supervision.
