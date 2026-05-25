---
title: "MEP-51 research note 03, Prior art and source-to-Python transpilers"
description: "Survey of source-to-Python transpilers, type checkers, alternative Python runtimes, and Python-from-other-language pipelines that informed MEP-51. From 2to3 and Cython to Mojo and pyrefly, plus the sibling Mochi backends."
---

# MEP-51 research note 03, Prior art and source-to-Python transpilers

Author: research pass for MEP-51 (Mochi to Python transpiler).
Date: 2026-05-23 (GMT+7).
Sources: CPython release notes for 2to3 and lib2to3; Cython documentation
at cython.org; mypyc internal documentation at github.com/mypyc/mypyc;
Nuitka manual at nuitka.net; Numba documentation at numba.pydata.org;
Brython at brython.info; Transcrypt at transcrypt.org; RustPython at
github.com/RustPython/RustPython; PyPy at pypy.org; Jython at
jython.org (historical); IronPython 3 at ironpython.net; GraalPy at
graalpy.org; Hy at hylang.org; Coconut at coconut-lang.org; Modular's
Mojo announcements at modular.com; LPython at lpython.org;
MicroPython at micropython.org; CircuitPython at circuitpython.org;
Codon at exaloop.io and the original MIT-LL paper (Shajii et al.,
PLDI 2023); pytype at github.com/google/pytype; pyrefly at
github.com/facebook/pyrefly; the Pyodide release notes; the Skulpt
project; the Mochi sibling research bundles (MEP-45, MEP-46, MEP-47,
MEP-48, MEP-49, MEP-50).

This note surveys the prior art that a Mochi-to-Python transpiler must
either build on, learn from, or deliberately diverge from. The survey
is grouped by the *kind* of system: (1) Python-to-Python transformers
(2to3, lib2to3, ruff, libcst), (2) Python-with-types-to-native
compilers (Cython, mypyc, Nuitka, Numba, Codon, LPython), (3)
alternative Python interpreters (PyPy, Jython, IronPython, GraalPy,
MicroPython, CircuitPython, RustPython, Brython, Pyodide, Skulpt,
Transcrypt), (4) Python-superset languages (Hy, Coconut, Mojo),
(5) static type checkers (mypy, pyright, pytype, pyrefly), and
(6) the Mochi sibling backends (MEP-45 to MEP-50). Each entry names
the project, summarises what it does, identifies the specific lesson
MEP-51 takes, and either inherits or rejects the pattern with the
reason.

The single biggest takeaway from this survey: **no widely-adopted
source-to-Python transpiler exists from a typed non-Python language**.
The closest analogues are TypeScript-to-Python (an academic curiosity)
and the C-extension-from-typed-Python tools (Cython, mypyc) which go
the other way. Mochi-to-Python occupies genuinely under-explored
territory. The closest parallels are the Python-on-JVM and Python-on-
WASM runtime projects (Jython, GraalPy, Pyodide, Brython, Skulpt),
which all chose runtime hosting over source emission. MEP-51's choice
to emit typed Python source is therefore a deliberate divergence with
a clear payoff: leverage the entire PyPI ecosystem with Mochi's type
guarantees intact.

## 1. 2to3 and lib2to3, Python 2-to-3 migrator

**What it is.** 2to3 (CPython stdlib, since 3.0 in 2008) is a CLI tool
that ingests Python 2 source and emits Python 3 source. The lib2to3
library powering it has been deprecated since 3.9 (PEP 617) and removed
from CPython 3.13 (replaced by the newer PEG parser). Used heavily
during the 2008-2020 Python 3 migration; obsolete since 2020 when
Python 2 reached end-of-life.

**How it works.** 2to3 uses lib2to3's pattern-matching syntactic
transformer. Each "fixer" matches a Python 2 syntactic pattern and
rewrites it to the Python 3 equivalent. Examples: `print x` becomes
`print(x)`, `dict.iteritems()` becomes `iter(dict.items())`,
`unicode` becomes `str`. The transformer is **source-level** (not
bytecode, not AST), preserving comments and most whitespace.

**Lesson for MEP-51.** Source-level transformation is the right
mental model for any "language A to Python" transpiler. 2to3 showed
that preserving comments, formatting, and source structure matters
for user adoption: developers must be able to read the output and
recognise it as their code. Mochi-to-Python takes the same stance.

**What we take.** The pattern-matching transformer architecture is
the right shape for syntactic rewrites; our IR-to-libcst lowering is
the modern equivalent. The principle that the output should be
human-readable Python code is non-negotiable.

**What we reject.** lib2to3's specific implementation (pattern files
in a custom DSL, the fixer plugin architecture) is dated; libcst is
the modern equivalent with better tooling.

## 2. lib2to3 successors: libcst and ruff

**What lib2to3 left behind.** lib2to3 is deprecated; the Python
ecosystem has moved on to two replacements:

- **libcst** (Instagram, originally Facebook, now maintained at
  github.com/Instagram/LibCST). Concrete Syntax Tree library for
  Python, parses Python 3.0 through 3.13. Used by Pyre (Meta's type
  checker, predecessor to pyrefly), by Instagram's internal code
  modification tooling, and by Bowler (PyCon-era refactoring tool).
- **ruff** (Astral, github.com/astral-sh/ruff). Rust-written linter
  and formatter, now adding fixer infrastructure. ruff has its own
  AST representation and a fast Rust-side parser; the formatter
  shipped in 2023 and is Black-compatible.

**Lesson for MEP-51.** libcst is the canonical Python source emit
tool. ruff is the canonical Python source formatter. The Mochi
codegen uses both:

1. **libcst** for building the CST that represents the Mochi-emitted
   module. CST preserves comments, blank lines, and whitespace
   exactly. Roundtripping (parse + edit + emit) is byte-stable.
2. **ruff format** for the final layout pass. Equivalent to Black
   in output but ~30x faster.

**What we take.** libcst is the source emitter. ruff format is the
formatter. We do not use Black directly because ruff format is
faster and produces byte-identical output for our subset.

**What we reject.** Writing our own Python source emitter from
scratch. Both libcst and ruff are open-source, maintained, and
sufficient.

## 3. Cython, Python superset compiled to C extensions

**What it is.** Cython (cython.org, since 2007, maintained by a
volunteer team including Robert Bradshaw, Stefan Behnel) is a
*superset* of Python that compiles to C. Cython adds typed
declarations (`cdef int x = 0`), native function declarations (`cdef
double mysqrt(double x)`), and direct C-library bindings via `cdef
extern from "header.h"`. The output is a C source file that compiles
to a CPython extension module (a `.so` / `.pyd` file).

Cython is *heavily* used in the scientific Python stack: NumPy's
linalg internals, SciPy, pandas, scikit-learn, lxml, h5py, gevent
all use Cython for hot paths. As of 2025-Q4, Cython 3.0 (released
2023-07) is the stable line; Cython 3.1 (in development) adds Python
3.13 support.

**How it works.** Cython takes a `.pyx` file (Cython syntax), runs a
Cython-specific parser, generates a `.c` file containing CPython
extension code (with PyObject* manipulation, refcount management,
optimised C code where types are known), and hands to a C compiler.

**Lesson for MEP-51.** Cython proved that typed Python can be
compiled to performant native code: the speedup over CPython on
numeric workloads is 5-50x. For Mochi's hot paths, Cython is a
natural v2 target.

**What we take.** The mental model: typed Python annotations are
optimisation hints. Mochi-emitted code is already heavily typed;
adding `cdef` annotations as a future v2 codegen path would unlock
Cython-level performance.

**What we reject for v1.** Cython requires a C toolchain at install
time (breaks `py3-none-any` wheel determinism), adds Cython as a
build dependency (not stdlib), and produces extension modules that
Pyodide handles awkwardly. For v1 we emit pure-Python source. The v2
`--target=python-cython` flag is the planned acceleration path; see
[[02-design-philosophy]] §10.2 and [[12-risks-and-alternatives]] §C1.

**Why not just always use Cython.** The wheel and Pyodide problems
above. Pure-Python wheels are universal; Cython-compiled wheels are
platform-tagged and require build infrastructure per target. The
default codegen targets the universal path; Cython is opt-in.

## 4. mypyc, mypy's internal compile-to-C-ext path

**What it is.** mypyc (github.com/mypyc/mypyc) is a compiler that
takes mypy-annotated Python source and produces a CPython extension
module. mypyc is what mypy itself uses internally to compile its hot
paths: the mypy CLI loads compiled mypyc extensions for the parser,
type checker, and inference engine, which is what makes mypy as fast
as it is (despite being written in Python).

mypyc is "stable for internal mypy use" but the project page warns
against using it for general production. Bug reports are accepted but
the developer effort is mostly on mypy itself.

**How it works.** mypyc consumes a Python source file with full
mypy-compatible annotations. The compiler infers concrete types where
annotations are present, emits C code that manipulates PyObject*
directly (avoiding Python interpreter overhead for the typed paths),
and packages as a CPython extension. The output runs only on CPython
of the specific version it was compiled against.

**Lesson for MEP-51.** mypyc demonstrates that mypy-annotated Python
contains enough type information to compile to native code without
the user adding extra annotations (unlike Cython, which requires
`cdef` keywords). Mochi-emitted Python is already mypy-strict
compatible; if mypyc reaches general-use quality, Mochi-to-mypyc is
nearly a free win.

**What we take.** The principle that "typed Python is enough type
information for AOT compilation". Mochi's emit is already typed; the
v2 `--target=python-mypyc` flag is the planned use of mypyc.

**What we reject for v1.** mypyc is not production-grade for general
use. Its support for PEP 695 generics is incomplete as of mypyc 1.13.
Wheel and Pyodide concerns same as Cython.

## 5. Nuitka, Python AOT-compiler to C++

**What it is.** Nuitka (nuitka.net, by Kay Hayen since 2014) is an
ahead-of-time Python compiler that produces native executables. It
parses Python source, compiles to C++ (with the embedded CPython
runtime), and links to a single binary. Mature; many production
users for shipping Python applications as native executables (similar
in spirit to py2exe and PyInstaller but compiled-to-native rather
than packaged-with-interpreter).

**How it works.** Nuitka's transformer reads Python source, builds
an AST, performs aggressive optimisation passes (constant folding,
dead code elimination, type inference), generates C++ that calls into
the embedded CPython API, and compiles via g++/clang++. The output
binary is typically 30-50% smaller than a PyInstaller bundle and
runs 2-10x faster on some workloads.

**Lesson for MEP-51.** Nuitka shows that whole-program AOT
compilation of Python is feasible. The challenge: Python's dynamic
features (eval, exec, __import__, monkey patching) defeat many
optimisations.

**What we take.** Conceptual confirmation that Python AOT is viable.
The Mochi codegen could in principle target Nuitka in v2.

**What we reject.** Nuitka's lag behind CPython on language features
(PEP 695, asyncio.TaskGroup arrive ~6-12 months after CPython). MEP-45
(C target) already provides single-binary executables for Mochi code
without Nuitka.

## 6. Numba, JIT compiler for numeric Python

**What it is.** Numba (numba.pydata.org, since 2012, by Anaconda)
is a JIT compiler for a subset of Python. The user decorates a
function with `@numba.jit`, and Numba traces the function at first
call, infers types, and compiles to LLVM IR via LLVMlite. The
compiled function replaces the Python implementation for subsequent
calls. Subset: NumPy arrays, scalar numeric types, basic control
flow. Not supported: classes, exception handling, dictionaries with
heterogeneous types.

Heavily used in numeric Python workloads (scientific computing, ML,
financial modeling). The pydata.org ecosystem assumes Numba.

**How it works.** Numba's JIT is type-tracing: the first call to a
Numba-decorated function records the argument types; the function is
specialised and compiled for that type signature. Subsequent calls
with the same types dispatch to the compiled version (cache hit);
different types trigger a new compilation. The output is LLVM IR
that NUMBA's LLVMlite backend lowers to machine code.

**Lesson for MEP-51.** JIT compilation of typed Python is a viable
performance strategy. The cost: Numba's subset is narrow; classes,
dataclasses, async are unsupported. Mochi-emitted code uses all of
those extensively.

**What we take.** Awareness that for *very* hot numeric inner loops,
Numba is the right tool. Mochi users who hit numeric performance
limits can manually decorate Mochi-emitted Python functions with
`@numba.jit` (when the function fits Numba's subset).

**What we reject.** Numba is not a general-purpose codegen target.
Its restrictions (no classes, limited control flow) don't fit
Mochi's surface.

## 7. Brython, Python-in-the-browser interpreter

**What it is.** Brython (brython.info, since 2012, by Pierre Quentel)
is a Python 3 implementation in JavaScript. Brython parses Python
source in the browser, compiles to JavaScript, and runs in the JS
engine. No backend required; pure browser-side Python.

Used by educators (CS curriculum that wants Python in the browser
without a server), some interactive notebook tools, and as a teaching
sandbox.

**How it works.** Brython ships a Python-to-JS compiler in JS itself.
The Python source is parsed in the browser, compiled to JS code that
calls the Brython runtime (which implements Python's data model in
JS classes), and executed.

**Lesson for MEP-51.** Brython proved that Python-in-the-browser is
viable. Today the same use cases are better served by Pyodide
(CPython compiled to WASM); Brython is mostly an educational tool.

**What we take.** Awareness of the browser-Python landscape. Mochi-
to-browser is currently covered by MEP-49 (Swift via SwiftWasm) and
MEP-50 (Kotlin/Wasm); the v1.1 Pyodide target adds a third path.

**What we reject.** Brython's "Python implemented in JS" approach;
Pyodide is the dominant browser-Python today.

## 8. Transcrypt, Python-to-JS source compiler

**What it is.** Transcrypt (transcrypt.org, since 2015, by Jacques de
Hooge) is an ahead-of-time Python-to-JavaScript compiler. Produces
JS source files (not WASM, not embedded Python). Designed for browser
deployment of Python code.

**How it works.** Transcrypt parses Python source via Python's `ast`
module, maps to JS source patterns, emits JS files. The runtime is
a small JS library implementing Python's data model.

**Lesson for MEP-51.** Source-to-source transpilation of Python to
JS is feasible but produces less idiomatic output than runtime
hosting (Pyodide, Brython). Transcrypt's subset compatibility with
CPython is limited (asyncio, recent typing features).

**What we take.** Awareness that some Python users prefer a source-
to-source transpiler over a hosted interpreter. For Mochi, the
equivalent is using Mochi's *own* MEP-49 Swift / MEP-50 Kotlin to
target the browser directly, rather than going through Python first.

**What we reject.** Transcrypt as a target. Two-level transpilation
(Mochi -> Python -> JS) compounds compatibility risks.

## 9. RustPython, Python interpreter written in Rust

**What it is.** RustPython (github.com/RustPython/RustPython, since
2018) is a Python 3 interpreter written in Rust. Goal: a Python
implementation that can be embedded in Rust applications. Status: not
fully CPython-compatible (stdlib coverage is partial as of 2025-Q4),
but actively developed.

**How it works.** RustPython parses Python source via its own
Rust-side parser, builds an AST, and interprets via a tree-walker (no
JIT). The output is in-memory execution; Python source goes in,
results come out.

**Lesson for MEP-51.** Alternative Python interpreters exist beyond
CPython, PyPy, and the discontinued Jython/IronPython lines.
RustPython is the most likely target for embedded use in Rust
applications, which intersects with Mochi's potential Rust target
(not yet planned but on the long-term roadmap).

**What we take.** Awareness. No direct integration in v1.

**What we reject.** RustPython is not yet CPython-compatible enough
for production use of Mochi-emitted code.

## 10. PyPy, Python implementation with tracing JIT

**What it is.** PyPy (pypy.org, since 2007 originally, since 2010 in
its modern form) is an alternative Python implementation written in
RPython (a Python subset). PyPy includes a tracing JIT compiler that
compiles hot loops to native code at runtime. Mature, production-
grade for many workloads.

PyPy compatibility: Python 3.10 as of PyPy 7.3.17 (Oct 2024); Python
3.11 in development; Python 3.12 not yet available (planned 2026).

**How it works.** PyPy's RPython source is compiled to C (via the
RPython compiler bootstrap), producing the PyPy interpreter binary.
At runtime, PyPy interprets Python bytecode; when a loop becomes hot
(thousands of iterations), the tracing JIT records the trace and
compiles it to native code. Subsequent iterations of the loop run
the compiled code, with deoptimisation guards for type changes.

PyPy's performance is 2-10x faster than CPython on long-running
CPU-bound workloads; the JIT overhead means it can be slower on
short-running programs.

**Lesson for MEP-51.** PyPy is the most mature alternative Python
runtime. Mochi-emitted code should run on PyPy unmodified once PyPy
supports Python 3.12.

**What we take.** The PyPy compatibility forward-gate (v1.1, when
PyPy 3.12 ships). Mochi-emitted code uses no PyPy-incompatible
features (we avoid ctypes-heavy hot paths and C extension internals).

**What we reject for v1.** PyPy 3.12 is not yet shipped. The v1
baseline is CPython 3.12+.

## 11. Jython, Python on the JVM (defunct)

**What it is.** Jython (jython.org, originally JPython, since 1997)
is a Python 2 implementation that runs on the JVM. Active development
ceased in 2017; the last Jython release (2.7.4) is from 2024 but is
still Python 2.7. No Python 3 port. Effectively defunct.

**How it works.** Jython parses Python source and compiles to JVM
bytecode. Cross-language interop with Java is direct: Python code
calls Java methods, Java code calls Python objects.

**Lesson for MEP-51.** Python-on-JVM is a viable architecture but
the user demand declined after Python 2's end of life; Jython did
not pivot to Python 3 successfully. GraalPy is now the production
Python-on-JVM-adjacent option.

**What we take.** Awareness. No direct integration.

**What we reject.** Jython for any v1 path.

## 12. IronPython, Python on .NET

**What it is.** IronPython (ironpython.net) is a Python implementation
on .NET. IronPython 2 supported Python 2.7. IronPython 3 (since 2020)
is in active development; the latest release (3.4.2, 2025-Q3)
targets Python 3.4 compatibility.

**How it works.** IronPython compiles Python source to .NET CLR
bytecode. The Python data model is mapped to .NET object model. Full
.NET library access from Python; calls from .NET into Python work
via the DLR (Dynamic Language Runtime, .NET's dynamic-typing
infrastructure).

**Lesson for MEP-51.** Python-on-.NET works but IronPython lags
CPython by 8+ years. MEP-48 (.NET target) handles the .NET ecosystem
directly without going through Python.

**What we take.** Awareness. No direct integration.

**What we reject.** IronPython for any v1 path.

## 13. GraalPy, Python on GraalVM

**What it is.** GraalPy (graalpy.org, by Oracle Labs since 2017,
formerly Graal Python) is a Python implementation on Oracle's
GraalVM polyglot runtime. GraalVM hosts Java, JavaScript, Ruby,
Python, R, and LLVM-bitcode languages in a unified VM with
cross-language interop.

Status as of 2025-Q4: Python 3.10 compatibility, with 3.11 and 3.12
in progress. Production-grade enough that Oracle ships it as part of
GraalVM EE.

**How it works.** GraalPy uses Truffle (GraalVM's language
implementation framework) to define Python's interpreter as a
tree-walking AST visitor. Graal's partial-evaluation JIT then
specialises the interpreter for the actual Python code being run,
producing native code. This is the most advanced Python JIT in
production; benchmarks show 2-5x speedup over CPython, sometimes
matching PyPy.

**Lesson for MEP-51.** GraalPy is the current frontrunner among
JVM-hosted Python implementations. When GraalPy 3.12 ships, Mochi-
emitted code should run on it unmodified.

**What we take.** The GraalPy compatibility forward-gate (v2, when
GraalPy 3.12 ships and matures).

**What we reject for v1.** GraalPy 3.12 is in progress; we floor at
3.12 features but the runtime is CPython.

## 14. Hy, Lisp dialect on the Python AST

**What it is.** Hy (hylang.org, since 2013) is a Lisp dialect that
compiles to Python AST. Hy source is s-expressions; the compiler
translates to Python AST and uses Python's `ast` module + `compile`
+ `exec` to run.

**How it works.** Hy's reader parses s-expressions, the compiler
walks the s-expression tree and builds Python AST nodes, the AST is
compiled to Python bytecode, executed by CPython.

**Lesson for MEP-51.** Hy is the canonical example of "non-Python
source compiled to Python AST + executed by CPython". Mochi-to-Python
is structurally similar but emits Python *source* (for human
readability and toolchain integration) rather than Python AST
directly.

**What we take.** The Python AST is a reasonable IR for compiled
languages targeting CPython. Mochi's libcst-based emit could in
principle emit AST directly; we chose source emit for the human-
readability benefit.

**What we reject.** Hy's AST-direct path; we go through source for
the reasons above.

## 15. Coconut, functional Python superset

**What it is.** Coconut (coconut-lang.org, since 2015, by Evan Hubinger)
is a functional Python superset. Adds pattern matching, pipeline
operators, infix function syntax, partial application, and lazy
evaluation as syntactic sugar on top of Python.

**How it works.** Coconut parses its own syntax, translates to
standard Python source, and hands to CPython. Coconut compiles to
Python 3.8+ compatible source; the output runs on any standard
Python interpreter.

**Lesson for MEP-51.** Coconut is the closest existing project to
Mochi-to-Python in spirit: it transpiles a typed-functional-leaning
language to standard Python source. Coconut's choices on pattern
matching (predating PEP 634) and lazy evaluation (via generators)
inform Mochi's lowering.

**What we take.** The general pattern: parse non-Python source, emit
Python source that any standard Python interpreter runs. Coconut's
output style (idiomatic Python where the Coconut surface allows)
is a good reference.

**What we reject.** Coconut's syntax (s-expression-influenced) is
not Mochi's syntax. Coconut targets a niche audience; Mochi is a
broader-spectrum language.

## 16. Mojo, Modular's Python-superset systems language

**What it is.** Mojo (modular.com/mojo, by Chris Lattner and Modular
since 2023, public beta 2024-09) is a Python-syntax-compatible
systems language with first-class support for SIMD, hardware
vectorisation, and AI workloads. Mojo claims 35,000x speedup over
Python on some matrix multiplication workloads.

**How it works.** Mojo's compiler is closed-source as of 2025-Q4.
The language spec is open; the toolchain is Modular's proprietary
build. Mojo source compiles directly to native code via MLIR
(LLVM's intermediate representation for ML accelerators). Python
compatibility: Mojo source is "almost" Python; many idiomatic
Python programs run in Mojo with minor changes.

**Lesson for MEP-51.** Mojo is the most ambitious "Python superset"
language project. If it succeeds (compiler open-sources, language
stabilises), it becomes a credible AOT-compilation target for
Python-shaped code.

**What we take.** Awareness. Monitor Mojo's roadmap.

**What we reject for v1.** Closed-source compiler. Proprietary
toolchain. Mojo 1.0 not yet shipped. The language is too young to
be a reliable target.

**v2 plan.** If Mojo opens the compiler and stabilises (target 2026
per Modular's roadmap), Mochi may add a Mojo target. Mojo's typed
syntax would be a natural extension of Mochi's typed-Python emit.

## 17. LPython, LLVM-based AOT compiler for typed Python

**What it is.** LPython (lpython.org, by the Lcompilers project since
2022) is an LLVM-based ahead-of-time compiler for typed Python. The
sister project to LFortran (Fortran-to-LLVM); both share the
Lcompilers infrastructure.

**How it works.** LPython parses typed Python source, builds an AST,
runs aggressive type inference, lowers to LLVM IR, compiles to a
native executable. Status: alpha as of 2024-Q4; small subset of
Python supported.

**Lesson for MEP-51.** LPython demonstrates that LLVM-based AOT
compilation of typed Python is feasible. The challenge: the subset
is small (no classes yet, limited stdlib).

**What we take.** Conceptual confirmation. Mochi already has MEP-45
(C target) and could in principle add an LLVM-direct target later.

**What we reject for v1.** Alpha-quality, small subset.

## 18. MicroPython and CircuitPython, embedded Python

**What it is.** MicroPython (micropython.org, since 2013, by Damien
George) is a Python 3 implementation for microcontrollers
(ESP32, RP2040, STM32). Targets hundreds-of-kilobyte memory budgets.
CircuitPython (circuitpython.org) is Adafruit's fork, focused on
educational hardware.

**How it works.** MicroPython's interpreter is a compact CPython-
compatible bytecode VM. Python source is compiled to MicroPython
bytecode at runtime (or AOT via the `mpy-cross` tool). The interpreter
is ~300 KB of code with a ~64 KB minimum memory footprint.

**Lesson for MEP-51.** Python on embedded hardware is viable but
requires a narrow language subset. MicroPython lacks PEP 695 generics,
asyncio.TaskGroup, full dataclasses support, and most of the typing
machinery. Mochi-emitted code targeting full Python 3.12 features
would not run on MicroPython.

**What we take.** Awareness. Embedded Mochi is better served by
MEP-45 (C target) which produces direct ARM binaries.

**What we reject.** MicroPython as a target. The Python subset is
too narrow.

## 19. Pyodide, CPython-in-WASM

**What it is.** Pyodide (pyodide.org, since 2018, originally at
Mozilla, now under Pyodide Project) is CPython compiled to
WebAssembly with NumPy, SciPy, pandas, scikit-learn, and ~100 other
scientific Python packages also ported to WASM. Used in JupyterLite,
vscode.dev, and increasing numbers of browser-Python applications.

As of 2025-Q4, Pyodide is at v0.27 with Python 3.12 support and ~150
packages. Production-grade enough for JupyterLite and similar
in-browser notebook tools.

**How it works.** Pyodide is built by compiling CPython itself with
Emscripten (LLVM-to-WASM toolchain). The resulting WASM module is
~10 MB compressed; loading the full pyodide.js + WASM takes 5-15
seconds on first load (cached afterwards). NumPy and friends are
recompiled with Emscripten for the WASM target.

**Lesson for MEP-51.** Python-in-the-browser is solved by Pyodide for
all practical purposes. Mochi-emitted Python code should run in
Pyodide unmodified (we use no Pyodide-incompatible features).

**What we take.** v1.1 plan: ship `mochi build --target=python-pyodide`
that bundles Mochi-emitted Python source with a Pyodide-loading HTML
harness. The output is a single HTML file plus a JS bundle.

**What we reject for v1.** Pyodide is a runtime, not a codegen target.
Mochi v1 emits Python source; the user can deploy to Pyodide
themselves if they want browser execution. v1.1 ships the
Pyodide-specific bundler.

## 20. Skulpt, browser-Python for educational use

**What it is.** Skulpt (skulpt.org, since 2010) is a Python 2/3
implementation in JavaScript. Used heavily in CS education tools
(Trinket, Runestone Academy, online CS textbooks).

**How it works.** Similar to Brython: Python source is parsed in the
browser, compiled to JavaScript, executed in the JS engine. The
runtime implements Python's data model in JS classes.

**Lesson for MEP-51.** Skulpt and Brython both predate Pyodide;
their use cases (educational browser-Python) survive but are mostly
migrating to Pyodide.

**What we take.** Awareness. No direct integration.

**What we reject.** Skulpt as a target.

## 21. Codon, high-performance typed Python

**What it is.** Codon (codon-lang.org, by Exaloop since 2022; the
original academic paper was Shajii et al., PLDI 2023). High-
performance compiler for a Python-like language. Codon's syntax is
nearly identical to Python's, but Codon is *not* Python: it diverges
on the type system (Codon has Hindley-Milner-style inference, Python
has gradual typing) and on some semantics (Codon's integers are
fixed-width by default).

Codon's performance is impressive: 10-100x speedup over CPython on
many workloads, comparable to C/C++ for numeric code.

**How it works.** Codon parses Python-like source, runs Hindley-
Milner type inference, lowers to LLVM IR, compiles to native code.
The output is a native executable.

**Lesson for MEP-51.** Codon demonstrates that "Python source with
strong inference" can compile to extremely fast native code. The
trade-off: Codon's type system is not Python's, so most existing
Python code does not "just work" on Codon.

**What we take.** Awareness. Codon's design tension (Python syntax,
non-Python semantics) is exactly the tension Mochi resolves by
having its own surface syntax distinct from Python.

**What we reject for v1.** Codon's language is not Python; targeting
Codon would be a separate codegen path, not a Python target.

## 22. pytype, Google's type inference checker

**What it is.** pytype (github.com/google/pytype, since 2015) is
Google's static type checker for Python. The differentiator vs mypy
and pyright: pytype does *inference* across untyped code, deriving
types from usage rather than requiring annotations.

**How it works.** pytype parses Python source, abstract-interprets it
to derive types for all variables, and checks against any
user-provided annotations. The output is a set of warnings and
errors, plus optional `.pyi` stubs containing the inferred types.

**Lesson for MEP-51.** Type inference across untyped code is
valuable, but for Mochi-emitted code (which is always fully typed),
inference is unnecessary. mypy and pyright suffice.

**What we take.** Awareness. pytype is not a Mochi gate.

**What we reject.** pytype as a build gate. mypy and pyright cover
the type-checking requirement; adding pytype would not catch errors
the other two miss for our fully-typed emit.

## 23. pyrefly, Meta's newer type checker

**What it is.** pyrefly (github.com/facebook/pyrefly, launched 2024)
is Meta's new Python type checker, written in Rust. Successor to
Pyre (Meta's older type checker, written in OCaml). Status as of
2025-Q4: internal use at Meta, public alpha. The Rust implementation
is targeted at extreme performance (Pyre was already very fast; pyrefly
aims to be faster).

**How it works.** pyrefly parses Python source via a Rust parser,
performs type inference and checking similar to mypy/pyright, and
reports errors. Designed for Meta-scale monorepos (millions of files).

**Lesson for MEP-51.** Static type checking for Python is converging
on Rust implementations (pyrefly, ruff's type-checker on the roadmap,
pyright via TypeScript on V8). Performance matters at scale.

**What we take.** Awareness. pyrefly is not yet a Mochi gate.

**What we reject for v1.** pyrefly is alpha. We gate on mypy +
pyright. When pyrefly stabilises (target 2026 per Meta's stated
roadmap), Mochi may add it as a third type-check gate.

## 24. The historical "transpile X to Python" zoo

Several niche projects transpile non-Python source languages to
Python. None reached production for any user base:

- **Berp** (Haskell-to-Python, defunct 2014). Translated Haskell to
  Python; abandoned.
- **OCaml-to-Python via ocsigen**: experimental, never released.
- **Clojure-Py** (Clojure on Python, defunct 2014): tried to host
  Clojure on CPython; abandoned.
- **Coffee Script Python (cs.py)**: bidirectional CoffeeScript-Python
  experiment, never reached release.
- **Scala-to-Python via Scala.py**: research project at EPFL, not
  released.
- **Lua-to-Python via Luan-Python**: research, defunct.
- **Tcl-to-Python via tclpy**: research, defunct.

These projects share a common failure mode: they targeted Python
source but lost too much of the source language's semantics in
translation. The lesson for MEP-51: preserve Mochi's surface
semantics exactly, and emit Python that respects them, even if the
emit is verbose.

## 25. Domain-specific source-to-Python tools

A few production tools transpile *specific* source languages to
Python for specific use cases:

- **Solver-to-Python via Z3's Python API**: Z3 SMT solver expressions
  can be expressed in Python via the `z3` library, which is itself a
  Python binding over a C++ library. Not a transpiler per se, but a
  pattern: domain-specific source language exposed through a Python
  API.
- **Apache Beam Python SDK**: distributed-computation graphs are
  expressed in Python, compiled to Apache Beam's IR, executed on
  Dataflow / Spark / Flink. Mochi's query DSL has analogues with
  Beam's pipeline DSL.
- **TensorFlow's `tf.function`**: Python functions decorated with
  `@tf.function` are traced and compiled to TensorFlow graphs. Mochi's
  agent functions could in principle be decorated similarly for
  TF-graph execution.
- **JAX's `jax.jit`**: similar to TF's tf.function, but for JAX
  (XLA-compiled). Numerical functions are traced and compiled.

These are *runtime* compilers (the user writes Python, the framework
traces and compiles at first call). Mochi-to-Python is *ahead-of-
time*: the user writes Mochi, the Mochi compiler emits Python.

## 26. The Mochi sibling backends, what MEP-51 inherits

The seven Mochi backends share a common architecture (MEP-45 aotir
IR feeds a per-target lowering pass). MEP-51 inherits specific
patterns from each:

### 26.1 MEP-45, C target

**What it provides**: single-binary AOT compilation via C, single-
allocator memory model, no GC overhead.

**What MEP-51 inherits**: the **aotir IR** itself. Mochi's lowering
pipeline produces aotir nodes; the Python target reads aotir and
emits Python source. The same IR feeds every other target.

The C target's value-semantics implementation (defensive copies at
function-call boundaries) is the same pattern the Python target uses
(see [[02-design-philosophy]] §7). The Python implementation just
uses `list(arg)` instead of `memcpy`.

The C target's FFI surface (function pointers, struct layouts) is
the model the Python target's ctypes binding follows (see
[[01-language-surface]] §8.2).

### 26.2 MEP-46, BEAM target

**What it provides**: actor model with mailbox-per-process,
supervision trees, hot code reload.

**What MEP-51 inherits**: the **agent + mailbox + supervision tree**
shape. Mochi's `agent` lowers to a class wrapping
`asyncio.Queue[Message]` plus a `TaskGroup` for supervision, which is
the asyncio analogue of BEAM's process + mailbox + supervisor. The
PEP 654 `ExceptionGroup` matches BEAM's "exit signal" propagation;
the one-for-all and one-for-one supervision strategies have the
same names in both targets.

The BEAM target's `Result` shape (Erlang `{ok, T}` / `{error, E}`
tagged tuples) is the same model the Python target's `MochiResult`
uses (see [[02-design-philosophy]] §11).

### 26.3 MEP-47, JVM bytecode target

**What it provides**: direct JVM bytecode emit via ASM-shaped
builder, virtual threads (Loom) for agents.

**What MEP-51 inherits**: the **typed-Result error story**. Both
JVM bytecode (MEP-47) and Python (MEP-51) emit `MochiResult` rather
than exceptions; the lowering rules are nearly identical.

MEP-47's reflection avoidance (no `Class.forName`, no
`Method.invoke`) is mirrored in MEP-51: Mochi-emitted Python uses
`isinstance` and `match` for type tests, never `getattr` /
`hasattr` for dynamic dispatch.

### 26.4 MEP-48, .NET target

**What it provides**: C# source emit, NuGet packaging, NativeAOT
compilation.

**What MEP-51 inherits**: the **package manager integration pattern**.
.NET uses `dotnet add package` / `.csproj`; Python uses `uv add` /
`pyproject.toml`. The Mochi build driver supports both via a uniform
"add dependency" CLI: `mochi build --target=dotnet --add openai` and
`mochi build --target=python --add openai` both result in the
target-appropriate dependency declaration.

### 26.5 MEP-49, Swift target

**What it provides**: Swift source emit, SwiftPM packaging, actors
+ AsyncStream for agents, typed throws for errors.

**What MEP-51 inherits**: the **value-type-semantics philosophy**.
Swift's `struct` and `Array<T>` give value semantics + copy-on-write
for free; the Python target emulates value semantics via defensive
copies (the "expensive" path) plus immutable-by-default records (the
"cheap" path). The conceptual model is shared.

Swift's `actor` + `AsyncStream` for agents is the same architecture
as MEP-51's `asyncio.Queue` + `TaskGroup`. The differences:
- Swift's `actor` enforces sendability at compile time; Python has no
  equivalent check.
- Swift's `AsyncStream` is closer to Python's `AsyncIterator` than
  to `asyncio.Queue`. MEP-51 uses `AsyncIterator` for `stream<T>`
  and `Queue` for agent mailboxes, splitting what Swift unifies in
  `AsyncStream`.

Swift's typed throws (SE-0413) is the closest analogue to
`MochiResult`. MEP-51 emits `MochiResult` rather than Python
exceptions for the same reason MEP-49 emits typed throws (preserve
Mochi's typed-error contract).

### 26.6 MEP-50, Kotlin target

**What it provides**: Kotlin source emit, Gradle build, Kotlin
Multiplatform for cross-target, coroutines + Channel + Flow for
concurrency.

**What MEP-51 inherits**: the **source-emit + post-process formatter
pipeline**. MEP-50 uses KotlinPoet for emit + ktfmt for formatting;
MEP-51 uses libcst for emit + ruff format for formatting. The
two-stage pattern (build CST, run formatter) is shared.

MEP-50's `MochiResult<T, E>` sealed class is the direct analogue of
MEP-51's PEP 695 `MochiResult[T, E]` type alias.

MEP-50's `Channel<Message>` + receiver-loop actor pattern is the
direct analogue of MEP-51's `asyncio.Queue[Message]` +
TaskGroup-supervised loop.

MEP-50's deferral of Compose UI to v2 mirrors MEP-51's deferral of
GUI/web frameworks: Mochi has no `view` syntax yet. When `view`
lands, MEP-51 will lower to a Python web framework (likely FastAPI
for backends and Streamlit for data apps; see
[[12-risks-and-alternatives]] §F2).

### 26.7 Cross-target diff summary

| Backend | Output | Build | Async | Errors | Records |
|---------|--------|-------|-------|--------|---------|
| MEP-45 C | C source | clang/gcc | pthread | -fwrapv int + Result | struct + helper fns |
| MEP-46 BEAM | Core Erlang | rebar3 | actor + supervisor | Result + exit | tagged tuple |
| MEP-47 JVM | direct bytecode | ASM builder | virtual threads (Loom) | Result | final class |
| MEP-48 .NET | C# source | dotnet | Task + Channel | Result | record |
| MEP-49 Swift | Swift source | SwiftPM | actor + AsyncStream | typed throws | struct |
| MEP-50 Kotlin | Kotlin source | Gradle + Kotlin Multiplatform | Channel + SupervisorJob | MochiResult sealed | data class |
| **MEP-51 Python** | **Python source** | **uv + hatchling** | **asyncio.Queue + TaskGroup** | **MochiResult Ok/Err PEP 695 alias** | **frozen-slots dataclass** |

## 27. Lessons for MEP-51 codegen

The survey of prior art produces several actionable rules for the
MEP-51 codegen, captured here as a checklist:

1. **Emit Python source, not bytecode or AST**. Source is the
   stable interchange; libcst is the right tool. Lesson from 2to3
   and Hy.
2. **Run ruff format for layout**. Ruff is the canonical Python
   formatter as of 2025. Lesson from ruff's adoption velocity.
3. **Target CPython 3.12+ for PEP 695**. PEP 695 generic syntax is
   critical for clean codegen. Lesson from mypyc, pyright,
   Coconut.
4. **Strict mypy + strict pyright as gates**. Two checkers catch
   each other's bugs. Lesson from pyright's emergence as a mypy
   competitor.
5. **MochiResult, not exceptions**. Preserve Mochi's typed errors.
   Lesson from MEP-47, MEP-48, MEP-49, MEP-50.
6. **asyncio.Queue + TaskGroup for agents**. Stdlib structured
   concurrency. Lesson from MEP-46 BEAM, Trio's nursery design.
7. **Frozen-slots dataclass for records**. Stdlib, type-checker-
   friendly, memory-efficient. Lesson from PEP 557 + 3.10 slots.
8. **Defensive copy for value semantics**. Python's reference
   semantics require explicit copying. Lesson from MEP-50 Kotlin
   (same problem, same solution).
9. **`from __future__ import annotations` everywhere**. Forward-
   reference safety and import cycle break. Lesson from PEP 563,
   ongoing PEP 649 deferral.
10. **uv + hatchling + pyproject.toml**. Modern Python packaging.
    Lesson from Astral's ruff success and uv 0.4 launch.
11. **Pure-Python wheel (`py3-none-any`)**. Universal compatibility,
    Pyodide-friendly. Lesson from Cython / mypyc / Nuitka wheel
    pain.
12. **Reproducible builds via SOURCE_DATE_EPOCH**. PyPI Trusted
    Publishing requires it for sigstore. Lesson from the reproducible-
    builds.org spec and PyPA's adoption.
13. **Jupyter ipykernel as a secondary target**. Data science is
    the largest Python user base; notebook integration matters.
    Lesson from Pyodide/JupyterLite's success.
14. **Provider-agnostic AI / FFI runtime**. Mochi `ai("...")` and
    `extern fun` dispatch at runtime, not codegen. Lesson from
    LangChain's adapter pattern (without LangChain's bloat).

## 28. What MEP-51 deliberately rejects from prior art

Beyond the explicit rejections in [[02-design-philosophy]] §10
(PyPy, Cython, mypyc, Nuitka, Pyodide, GraalPy, MicroPython,
IronPython, Codon, Mojo, LPython), several specific patterns from
prior art are *not* adopted in v1:

- **Coconut-style functional sugar**: Coconut adds pipeline
  operators, partial application, and lazy evaluation as syntax.
  Mochi's surface is its own; we do not bring Coconut syntax into
  the emitted Python.
- **Hy-style AST-direct emit**: Mochi emits Python *source*, not
  Python AST directly. The source emit gives human readability and
  debugger integration.
- **2to3-style fixer architecture**: 2to3's pattern-based fixers are
  a dated approach; libcst's tree-based transforms are the modern
  equivalent.
- **Numba-style decorator-based JIT**: Numba requires per-function
  decoration. Mochi's whole-program emit is the opposite: every
  emitted function is statically typed and could be JIT'd uniformly.
- **TF-graph-style trace-and-compile**: TensorFlow's `tf.function`
  traces at first call. Mochi's AOT emit is the opposite: all type
  info is statically known at emit time.
- **Brython/Skulpt-style runtime emit**: those tools run a Python
  interpreter in the browser. Mochi-on-browser via Pyodide takes
  the same general approach (CPython in WASM) but is a runtime
  concern, not a codegen choice.
- **Pydantic-style runtime validation**: Pydantic validates types at
  runtime. Mochi's static type checker catches the same errors at
  compile time; runtime validation is redundant work.
- **typing.Protocol-style structural typing**: Mochi is nominal,
  Python supports both; we emit nominal classes. See
  [[02-design-philosophy]] §19.

## 29. Cross-references

- the shared-decisions anchor: the scope anchor for MEP-51.
- [[01-language-surface]]: the Mochi language surface mapped to
  Python lowering.
- [[02-design-philosophy]]: the "why" behind each decision; this
  note is the "what other people tried" companion.
- [[04-runtime]]: the `mochi_runtime` package this prior art
  informed.
- [[05-codegen-design]]: the libcst-based emit, ruff format
  post-process.
- [[06-type-lowering]]: per-type lowering details.
- [[07-python-target-portability]]: CPython 3.12+ portability and
  the platform matrix.
- [[08-dataset-pipeline]]: pandas / polars / pyarrow / duckdb
  integration (Numba and JAX are forward-gates).
- [[09-agent-streams]]: asyncio.Queue + TaskGroup (Trio, AnyIO,
  Ray are alternatives).
- [[10-build-system]]: uv + hatchling + pyproject.toml (Poetry,
  PDM, Hatch are alternatives).
- [[11-testing-gates]]: mypy + pyright strict (pytype, pyrefly
  are alternatives).
- [[12-risks-and-alternatives]]: the v2 candidates (Cython, mypyc,
  Pyodide, Mojo).
- [[../0050/03-prior-art-transpilers]]: the Kotlin-target prior-art
  analogue (J2K, dukat, J2CL, TeaVM, Skip, Scala.js).
- [[../0049/03-prior-art-transpilers]]: the Swift-target prior-art
  analogue.
- [[../0048/03-prior-art-transpilers]]: the .NET-target prior-art
  analogue.
- [[../0047/03-prior-art-transpilers]]: the JVM-target prior-art
  analogue.
- [[../0046/03-prior-art-transpilers]]: the BEAM-target prior-art
  analogue (Elixir, Gleam, Caramel).
- [[../0045/03-prior-art-transpilers]]: the C-target prior-art
  analogue.
