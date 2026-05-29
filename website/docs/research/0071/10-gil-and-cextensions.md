---
title: "10. GIL and C extensions"
sidebar_position: 11
sidebar_label: "10. GIL and C extensions"
description: "The GIL acquisition model, free-threaded CPython 3.13t / 3.14t (PEP 703), the foreign C-extension boundary, fork-safety, sub-interpreters (PEP 684 / 734), import-time side effects in the wild, the Mochi-side concurrency interaction model."
---

# 10. GIL and C extensions

This note covers two related concerns: how the bridge interacts with CPython's GIL, and how it deals with the heterogeneous landscape of C extensions on PyPI. Both are existential concerns for any Python bridge: get the GIL wrong and the bridge hangs or crashes; misjudge a C extension and the host process segfaults.

## The Global Interpreter Lock

The GIL is a per-interpreter lock that CPython acquires before running any Python bytecode or calling into the C API. Threads must hold the GIL to:

- Allocate or deallocate Python objects.
- Call `PyObject_*` C API functions (with rare exceptions like `Py_INCREF`/`Py_DECREF` under specific conditions).
- Execute Python bytecode.

Threads release the GIL during:

- Blocking I/O (the I/O wrappers explicitly release).
- Long-running C extension code that explicitly releases via `Py_BEGIN_ALLOW_THREADS` / `Py_END_ALLOW_THREADS`.
- The asyncio event loop's `select` / `poll` call.

For the Mochi bridge, the relevant rules:

- Any C call into the CPython API must hold the GIL.
- Threads created by Mochi (not by Python) must acquire the GIL before calling in.
- Threads created by Python (e.g., by `threading.Thread` or by asyncio's executor) already hold the GIL when their callbacks run.

## The PyGILState dance

The canonical pattern for foreign threads:

```c
PyGILState_STATE state = PyGILState_Ensure();
// ... call CPython API
PyGILState_Release(state);
```

`PyGILState_Ensure` acquires the GIL and sets up the thread state if this is the first time the thread has called in. `PyGILState_Release` releases the GIL and tears down the thread state if appropriate.

The Mochi wrapper module's C glue uses this pattern. Every entry point from Go into CPython does:

1. `PyGILState_Ensure()` at function entry.
2. The actual work.
3. `PyGILState_Release(state)` at function exit.

The Go-side runtime ensures the Mochi worker goroutine that's making the call is pinned to a single OS thread via `runtime.LockOSThread()` before the entry. This is required because PyGILState assumes a stable thread identity.

## Free-threaded CPython (PEP 703)

CPython 3.13 introduced an experimental "free-threaded" build that removes the GIL. Reference counting uses biased reference counting (Owens & Manson, 2003) so most refcount operations are still single-threaded fast; cross-thread refcount changes use atomics.

The free-threaded build is binary-incompatible with the GIL build. A wheel built for `cp313` will not load on `cp313t` (note the `t` suffix), and vice versa.

The bridge's free-threaded story:

- When `[python].free-threaded = true`, the wrapper module is built for the `t` ABI. The C glue uses:
  - Atomic refcount operations only (no `Py_INCREF` / `Py_DECREF` shortcuts).
  - `PyMutex` for any shared state in the wrapper.
  - No assumptions about GIL serialisation; multiple Mochi threads can be in CPython simultaneously.
- When `[python].free-threaded = false` (default through Phase 16), the wrapper uses GIL-only patterns.

The phase plan defers free-threaded support to Phase 17 because the testing matrix (fixtures × wheel tags × GIL/no-GIL) is large.

## C extensions in the wild: the landscape

PyPI hosts ~500k packages. Roughly 30% ship native code (a `.so`, `.pyd`, or `.dylib`). The native code falls into a few categories:

- **CPython C extensions (manual)**: hand-written CPython C API code. Examples: numpy core, scipy.linalg, lxml. These use the full CPython API (not limited) for performance.
- **CPython C extensions (Cython)**: Cython-compiled to C, then compiled to extension. Examples: scikit-learn, pandas core, msgpack.
- **CPython C extensions (PyO3, pybind11, nanobind)**: Rust or C++ source compiled to CPython extensions. Examples: cryptography (PyO3), uvloop (Cython but with libuv), pendulum (Rust).
- **CFFI extensions**: Python wrappers around C libraries via cffi. Examples: cryptography legacy, paramiko parts.
- **ctypes wrappers**: pure-Python wrappers around shared libraries via ctypes. No native code in the wheel; the wheel is `py3-none-any` and references the system shared library.
- **Native subprocess shells**: pure-Python wrappers that subprocess out to a native binary. Examples: black (subprocesses python -m black), ruff (subprocesses ruff). No CPython interaction beyond plain Python.

The bridge's wrapper module pattern works with all of these. The wrapper imports the package; whatever native code the package contains runs as part of Python's normal import flow.

## The C extension boundary problem

The hazard: a C extension might assume the GIL is held when its functions are called. If the Mochi side calls a function on an object whose method is implemented in C, that C method might assume GIL ownership. The wrapper module's GIL acquisition (via `PyGILState_Ensure`) handles this correctly.

The subtler hazard: a C extension might assume things about the calling thread's state beyond the GIL. Examples:

- **NumPy's signal handling**: NumPy installs SIGFPE handlers on some operations. If Mochi catches SIGFPE for its own reasons, the handlers conflict.
- **PyTorch's CUDA streams**: PyTorch maintains per-thread CUDA stream state. A Mochi worker goroutine that hops between OS threads will see fresh CUDA state on each hop.
- **GIL-released long-running operations**: A C extension that calls `Py_BEGIN_ALLOW_THREADS` to do CPU-intensive work releases the GIL. If Mochi's runtime expects to be in the GIL during that window (because the wrapper acquired it), it sees an inconsistent state.

Mitigations:

- The wrapper's GIL acquisition is at the wrapper-function granularity, not the call granularity. The wrapper acquires, does its setup, calls into Python, and releases. Inside the call, if the Python code releases the GIL for I/O, the bridge does not care because the bridge is not running inside that window.
- The Mochi worker is pinned to one OS thread for the entire duration of a `[python]` import's active lifetime. CUDA-style per-thread state is stable.
- Signal handlers are not intercepted by the bridge; the user's C extensions are free to install their own.

## fork-safety

`os.fork()` on a process holding a GIL produces a child process with the same locked state. Pre-fork, the parent should release the GIL; post-fork, the child reacquires.

CPython's threading model is not fork-safe by default. The standard advice is "don't fork after starting threads."

The Mochi runtime is multi-threaded by default (goroutines on a thread pool). If Mochi imports Python and the user (or a Python dep) calls `os.fork()`, the child process inherits an inconsistent state.

Mitigations:

- The bridge documents the hazard prominently. Users who need `fork()` should keep the Python import surface minimal pre-fork.
- The `multiprocessing` module's `spawn` start method (default on macOS and Windows, optional on Linux) avoids the fork hazard by using `execve` instead of `fork`.
- Future direction: sub-interpreters (PEP 684 / PEP 734) allow per-thread isolated interpreter state, sidestepping the fork hazard entirely. Currently experimental in CPython 3.12+.

## Sub-interpreters (PEP 684 / PEP 734)

Sub-interpreters are isolated Python interpreter instances within one process. PEP 684 added isolated GIL per sub-interpreter; PEP 734 added a stable C API for managing sub-interpreters; both are experimental in CPython 3.12 and stable in 3.13.

The promise: each Mochi worker goroutine could have its own sub-interpreter, with independent module state, no GIL contention between workers.

The reality:

- C extensions must opt in to multi-phase init (PEP 489) to work with sub-interpreters. Many do not, and using them in a sub-interpreter raises `ImportError`.
- The cost of starting a sub-interpreter is ~50ms (importing the entire stdlib). Pooling is required.
- Cross-sub-interpreter object passing is restricted to a small set of immutable types (the "shared data API" of PEP 734).

MEP-71's stance: sub-interpreters are not the default. Phase 17 (free-threaded) is the canonical concurrency improvement; sub-interpreters are a future direction documented in [[12-risks-and-alternatives]] §A8.

## Import-time side effects in the wild

Many Python packages do significant work at import time:

- **NumPy**: imports CPython, allocates type objects, registers fpe handlers, allocates ~50MB of internal arrays. ~30ms cold import.
- **PyTorch**: imports CUDA libraries, allocates GPU contexts, registers ~100 custom type objects. ~200ms cold import.
- **TensorFlow**: similar to PyTorch but heavier; ~500ms cold import.
- **pyarrow**: loads native libraries, registers arrow types. ~100ms.
- **mypy**: imports its own type-checker eagerly. ~150ms.

Side effects at import time include:

- File reads (config files, dist-info metadata).
- Network calls (rare but happens; the bridge denies these in the lock-time sandbox).
- GPU initialisation (PyTorch).
- Signal handler installation.

The bridge's strategy:

- Import the package lazily on first use, not eagerly at Mochi process startup. The cost amortises if the import is needed.
- For the lock-time stubgen sandbox, network is denied; packages that try to fetch are reported as `SkipReason::ImportTimeNetwork`.
- For the runtime, no sandbox is applied; the user's environment is the user's responsibility.

## The bridge's GIL discipline summary

```
┌──────────────────────────────────────────────────────────────────────┐
│ Rule 1: Every Mochi-to-Python entry holds the GIL.                   │
│ Rule 2: Every Python-to-Mochi callback releases the GIL on entry.    │
│ Rule 3: Mochi workers are pinned to one OS thread for the duration   │
│          of any Python active lifetime.                              │
│ Rule 4: Refcount operations on PyObject use the limited-API funcs    │
│          for portability.                                            │
│ Rule 5: Free-threaded mode (Phase 17+) replaces Rule 1 with atomic   │
│          refcount + PyMutex.                                         │
└──────────────────────────────────────────────────────────────────────┘
```

## Cross-references

- [[02-design-philosophy]] §3 for why the wrapper-module pattern handles the GIL.
- [[08-async-bridge]] for the asyncio-loop GIL interaction.
- [[09-abi-stability]] for the free-threaded ABI tag.
- [[12-risks-and-alternatives]] §R5 for the GIL contention risk.
- [PEP 703](https://peps.python.org/pep-0703/) for free-threaded CPython.
- [PEP 684](https://peps.python.org/pep-0684/), [PEP 734](https://peps.python.org/pep-0734/) for sub-interpreters.
- [Python C API: Thread State and the GIL](https://docs.python.org/3/c-api/init.html#thread-state-and-the-global-interpreter-lock).
