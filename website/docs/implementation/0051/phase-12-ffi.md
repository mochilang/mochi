---
title: "Phase 12. FFI (ctypes + CFFI + pure-Python)"
sidebar_position: 17
sidebar_label: "Phase 12. FFI"
description: "MEP-51 Phase 12 -- native FFI via ctypes (basic C ABI) and CFFI 1.17+ (callbacks, opaque pointers); pure-Python FFI via direct import + typed wrapper; PEP 561 py.typed marker and __all__ for @mochi_export; 20 fixtures."
---

# Phase 12. FFI (ctypes + CFFI + pure-Python)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 12](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | -- |
| Landed         | -- |
| Tracking issue | -- |
| Tracking PR    | -- |

## Gate

`TestPhase12FFI`: 20 fixtures green on CPython 3.12.0 and 3.13.0 across x86_64-linux-gnu, aarch64-linux-gnu, aarch64-darwin, and x86_64-windows. Secondary gates: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point. Tertiary gates: byte-equal stdout against vm3 for every fixture; ctypes calls round-trip through `libm` and `libc` test wrappers on Linux/macOS/Windows; CFFI 1.17+ ABI-mode callbacks survive cross-platform invocation; PEP 561 `py.typed` marker present in every emitted wheel.

## Goal-alignment audit

FFI is the connective tissue between Mochi and the PyPI ecosystem. The Python target's load-bearing pitch is "you write Mochi and you get NumPy, pandas, PyTorch, JAX, FastAPI, httpx". Without Phase 12 the user has no way to call any of these from Mochi code; the transpiler ships a beautiful runtime but no door to the outside world. Landing 12 means pure-Python FFI (Mochi calls into any pip-installed package) and native FFI (Mochi calls into `libssl`, `libcrypto`, `libsqlite3`, etc.) both work, both type-check, and both ship a `py.typed` marker so downstream type checkers see the Mochi-emitted package as fully typed.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 12.0 | ctypes basic: `CDLL` + `argtypes` + `restype` for simple C ABI; libm `sqrt`, `pow`, `floor` as the reference set | NOT STARTED | -- |
| 12.1 | CFFI 1.17+ ABI mode for callbacks, opaque pointer types, and headerless declarations | NOT STARTED | -- |
| 12.2 | Pure-Python FFI: `extern fun foo()` from a Python module to `from <module> import foo` plus typed wrapper | NOT STARTED | -- |
| 12.3 | `py.typed` marker shipping + `__all__` generation for `@mochi_export` decorated functions | NOT STARTED | -- |

## Sub-phase 12.0 -- ctypes basic

### Goal-alignment audit (12.0)

`ctypes` is in the stdlib and works on every CPython build without a compilation step. For simple C ABI (scalar arguments, scalar returns, no callbacks, no opaque pointers), it is the right tool. Landing 12.0 lets Mochi call any `libc` or `libm` function and any pure C library with a flat function-pointer API. Without 12.0 the only native interop path would be the heavier CFFI route (12.1), which has a build-time generation cost; 12.0 is the no-compile baseline.

### Decisions made (12.0)

Mochi `extern fun sqrt(x: float) -> float = "c:sqrt"` lowers to:

```python
from __future__ import annotations

import ctypes
from ctypes.util import find_library
from typing import Final


_LIBM_PATH: Final[str | None] = find_library("m")
if _LIBM_PATH is None:
    raise RuntimeError("libm not found")
_LIBM: Final[ctypes.CDLL] = ctypes.CDLL(_LIBM_PATH)

_c_sqrt = _LIBM.sqrt
_c_sqrt.argtypes = [ctypes.c_double]
_c_sqrt.restype = ctypes.c_double


def sqrt(x: float) -> float:
    return float(_c_sqrt(x))
```

Decisions:

- `find_library("m")` is the canonical cross-platform library discovery: on Linux it returns `libm.so.6`, on macOS it returns `libSystem.dylib` (which exports `sqrt`), on Windows it returns `msvcrt.dll`. The IR pass picks `find_library` over hard-coded paths.
- `argtypes` and `restype` are always set; un-typed ctypes calls pass arguments as `int` and lose precision on floats. The IR pass maps Mochi types to ctypes equivalents:
  - `int` to `ctypes.c_int64` (Mochi int is 64-bit signed)
  - `float` to `ctypes.c_double` (Mochi float is IEEE 754 double)
  - `bool` to `ctypes.c_bool`
  - `bytes` to `ctypes.c_char_p`
  - `string` to `ctypes.c_char_p` (after `.encode("utf-8")` at the call site)
- The wrapper function `sqrt` adds the Mochi-side type conversion (`float(...)` to widen the ctypes scalar back to Python `float`). mypy and pyright both narrow `_c_sqrt(x)` to `Any`; the explicit `float(...)` re-narrows.
- The library handle `_LIBM` is module-level and loaded once; subsequent calls reuse the handle.

For multi-library setups (`libssl`, `libcrypto`), each `extern` declaration gets its own handle:

```python
_LIBSSL: Final[ctypes.CDLL] = ctypes.CDLL(find_library("ssl") or "libssl.so.3")
```

The fallback path (`or "libssl.so.3"`) is emitted when the Mochi declaration includes a `@version("3")` annotation; otherwise the IR pass emits a `RuntimeError` if `find_library` returns None.

## Sub-phase 12.1 -- CFFI ABI mode

### Goal-alignment audit (12.1)

ctypes cannot express callbacks (function pointers from Mochi into C), opaque pointer types (a `*sqlite3` that Mochi must not deref), or structured types without per-call boilerplate. CFFI 1.17+ ABI mode handles all three with a single declaration syntax and zero compilation overhead. Without 12.1 the FFI surface excludes any library with callbacks (SQLite hooks, libcurl progress callbacks, libevent handlers) which is most of the production-grade C ecosystem. Landing 12.1 expands the FFI matrix from "scalar in, scalar out" to "the full C surface".

### Decisions made (12.1)

Mochi:

```mochi
extern type SqliteDb = "cffi:sqlite3:opaque"

extern fun sqlite_open(path: string) -> SqliteDb throws SqliteError = "cffi:sqlite3_open"
extern fun sqlite_exec(db: SqliteDb, sql: string, callback: fun(row: list<string>) -> bool) -> int = "cffi:sqlite3_exec"
```

Emit:

```python
from __future__ import annotations

from collections.abc import Callable
from typing import Final

import cffi

from mochi_runtime.result import Err, MochiResult, Ok


_ffi: Final[cffi.FFI] = cffi.FFI()
_ffi.cdef("""
    typedef struct sqlite3 sqlite3;
    int sqlite3_open(const char *filename, sqlite3 **ppDb);
    int sqlite3_exec(
        sqlite3*,
        const char *sql,
        int (*callback)(void*, int, char**, char**),
        void*,
        char **errmsg
    );
""")
_lib: Final = _ffi.dlopen("sqlite3")


class SqliteDb:
    __slots__ = ("_handle",)

    def __init__(self, handle: object) -> None:
        self._handle = handle


def sqlite_open(path: str) -> MochiResult[SqliteDb, SqliteError]:
    pp = _ffi.new("sqlite3 **")
    rc = _lib.sqlite3_open(path.encode("utf-8"), pp)
    if rc != 0:
        return Err(SqliteError(code=rc))
    return Ok(SqliteDb(pp[0]))


def sqlite_exec(
    db: SqliteDb,
    sql: str,
    callback: Callable[[list[str]], bool],
) -> int:
    @_ffi.callback("int(void*, int, char**, char**)")
    def _trampoline(_user: object, argc: int, argv: object, _cols: object) -> int:
        row = [_ffi.string(argv[i]).decode("utf-8") for i in range(argc)]
        return 0 if callback(row) else 1
    return _lib.sqlite3_exec(db._handle, sql.encode("utf-8"), _trampoline, _ffi.NULL, _ffi.NULL)
```

Decisions:

- CFFI ABI mode (`dlopen` + `cdef`) is used, not API mode. API mode requires a compilation step at install time; ABI mode is pure-Python load-time. Mochi v1 picks ABI for the no-compile property.
- Opaque types (`SqliteDb` with `__slots__ = ("_handle",)`) wrap the cffi pointer. mypy sees `SqliteDb` as the public type; the raw `cffi.CData` is hidden inside `_handle`.
- Callbacks are wrapped in `@_ffi.callback("...")` decorator inline at the call site. The trampoline converts Mochi types (Python `list[str]`) to C types (`char**`) and back. The bool return is encoded as 0/1 to match the C convention (`int` return, non-zero = abort).
- `_ffi.string(argv[i]).decode("utf-8")` is the cffi-stdlib pattern for `char*` to `str`; the IR pass emits it for every string return.
- The `cdef` block is the C-header subset that cffi parses; the IR pass generates it from Mochi `extern` declarations. Headerless declarations are supported as long as every type is declared.
- `cffi` is added as a runtime dep of `mochi_runtime`; it ships pre-built wheels for every platform on the support matrix.

## Sub-phase 12.2 -- Pure-Python FFI

### Goal-alignment audit (12.2)

The PyPI ecosystem (NumPy, pandas, PyTorch, FastAPI, httpx, etc.) is the load-bearing reason to have a Python target at all. Pure-Python FFI is how Mochi calls into it. Without 12.2 the user cannot `import numpy` from Mochi code, which defeats the entire pitch. Landing 12.2 makes any pip-installable package callable; the IR pass emits a `from <module> import <symbol>` plus a typed wrapper that converts Mochi types to/from Python types.

### Decisions made (12.2)

Mochi:

```mochi
extern fun np_array(xs: list<float>) -> NdArray = "py:numpy.array"
extern fun np_mean(arr: NdArray) -> float = "py:numpy.mean"
extern type NdArray = "py:numpy.ndarray"
```

Emit:

```python
from __future__ import annotations

import numpy as _numpy
from typing import cast

NdArray = _numpy.ndarray


def np_array(xs: list[float]) -> NdArray:
    return cast(NdArray, _numpy.array(xs, dtype=_numpy.float64))


def np_mean(arr: NdArray) -> float:
    return float(_numpy.mean(arr))
```

Decisions:

- `import numpy as _numpy` uses an underscore prefix so the symbol does not collide with Mochi-level names. The IR pass picks unique aliases.
- Type aliases (`NdArray = _numpy.ndarray`) are bare; PEP 695 `type` is not used because numpy's `ndarray` is not parameterised on the import side and the alias is a runtime reference, not a static-only one.
- `cast(NdArray, ...)` is the explicit re-narrow when the external function's return is `Any` (numpy 2.0+ ships PEP 561 stubs but many ndarray operations still return `Any`). The IR pass injects `cast` whenever the external annotation cannot be narrowed by inference.
- `float(...)` and `int(...)` calls are the safe explicit coercions; without them mypy widens to `Any` for numpy scalar returns.
- Mochi `throws E` for a pure-Python extern is supported via a Mochi-side `try / except` wrapper:

  ```python
  def np_array(xs: list[float]) -> MochiResult[NdArray, ValueError]:
      try:
          return Ok(cast(NdArray, _numpy.array(xs, dtype=_numpy.float64)))
      except ValueError as e:
          return Err(e)
  ```

  The IR pass emits this form only when the Mochi declaration declares `throws ValueError`; otherwise exceptions propagate as panics.
- The wrapper function name is the Mochi-side name (`np_array`); the external symbol can be any path (`numpy.array`).

## Sub-phase 12.3 -- py.typed and @mochi_export

### Goal-alignment audit (12.3)

A wheel without a `py.typed` marker is opaque to type checkers; downstream Python code that imports a Mochi-emitted package would see every public function as `Any`. Without 12.3 the Python target ships fully-typed code that downstream consumers cannot benefit from. Landing 12.3 makes Mochi-emitted wheels first-class typed Python packages: the `py.typed` marker is present, every public function appears in `__all__`, and pyright / mypy treat them as fully typed when imported.

### Decisions made (12.3)

PEP 561 `py.typed` is a zero-byte file at the package root that signals "this package ships inline type annotations". The IR pass emits one per generated package:

```
src/<pkg>/
    __init__.py
    py.typed         # zero-byte marker
    generated/
        __init__.py
        main.py
        ...
```

`__all__` is generated for every emitted module by collecting the names decorated with `@mochi_export` (or, equivalently, the names listed in the Mochi `export { ... }` block). Mochi:

```mochi
export {
    fun process(data: list<int>) -> int
    type Config
}
```

Emit:

```python
from __future__ import annotations

from .generated.main import Config, process

__all__ = ["Config", "process"]
```

Decisions:

- `__all__` is a static list of strings, sorted lexicographically for determinism. The IR pass emits it once per `__init__.py`.
- Private symbols (not in the Mochi export block) are not in `__all__`. They are still accessible (`from pkg.generated.main import _helper`) but `from pkg import *` does not pick them up, and pyright `--strict` treats them as private.
- The `py.typed` marker is emitted by the build driver (`transpiler3/python/build/wheel.go`); it is not a generated source file. Hatchling's `force-include` rule copies it into the wheel.
- The hatch `pyproject.toml` template at the package root is updated to declare `[tool.hatch.build.targets.wheel] packages = ["src/<pkg>"]` and `[tool.hatch.build.targets.wheel.force-include] "src/<pkg>/py.typed" = "<pkg>/py.typed"`.
- For Mochi-emitted packages that re-export FFI bindings, the `py.typed` marker means downstream consumers see the Mochi wrapper as fully typed even if the underlying FFI target is not.

Worked example: a Mochi library that exports a numpy-backed `MeanCalculator` lowers to a package with `py.typed`, `__all__ = ["MeanCalculator"]`, and a pyright-strict-clean wrapper around `numpy.mean`. Downstream Python users `from mochi_user.stats import MeanCalculator` and see the right types in their IDE.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/ffi_ctypes.go` | `extern fun ... = "c:..."` to ctypes `CDLL` + argtypes/restype + wrapper |
| `transpiler3/python/lower/ffi_cffi.go` | `extern fun ... = "cffi:..."` to cffi ABI mode + cdef + callback wrappers |
| `transpiler3/python/lower/ffi_python.go` | `extern fun ... = "py:..."` to `import` + typed wrapper |
| `transpiler3/python/lower/ffi_opaque.go` | `extern type T = "cffi:...:opaque"` to `__slots__` wrapper class |
| `transpiler3/python/emit/py_typed.go` | Zero-byte `py.typed` marker writer |
| `transpiler3/python/emit/all_export.go` | `__all__` list generation from Mochi `export` blocks |
| `transpiler3/python/build/wheel.go` | Hatchling `force-include` rule for `py.typed`; cffi runtime dep |
| `transpiler3/python/build/phase12_test.go` | `TestPhase12FFI`: 20 fixtures + mypy/pyright/ruff gates |
| `tests/transpiler3/python/fixtures/phase12-ffi/` | 20 fixture directories |

## Test set

- `TestPhase12FFI` -- 20 fixtures: ctypes_sqrt, ctypes_pow, ctypes_floor, ctypes_strlen, ctypes_libc_abs, ctypes_libm_sin (6 from 12.0); cffi_sqlite_open, cffi_sqlite_exec, cffi_callback_count, cffi_opaque_handle, cffi_string_return (5 from 12.1); py_numpy_mean, py_numpy_array, py_json_loads, py_pathlib_exists, py_throws_propagate (5 from 12.2); export_single_fn, export_multi_fn, export_type_alias, py_typed_marker_present (4 from 12.3).

## Deferred work

- C-extension compilation (`extern fun ... = "cext:..."`) requiring a `setup.py`-driven build step. Deferred to v2; v1 supports only `c:`, `cffi:`, and `py:` prefixes.
- Mochi-as-library exposure to C callers (no JNI/COM-equivalent on Python). Deferred indefinitely; downstream C consumers must write their own Python C extension to embed CPython.
- Numpy dtype inference from Mochi type annotations (`list<int32>` to `dtype=numpy.int32`). Deferred to Phase 12.4 or v2; v1 emits `dtype=numpy.float64` for floats and `dtype=numpy.int64` for ints, with no narrower variants.
- CFFI API mode (compile-at-install) for performance-critical bindings. Deferred to v2; v1 ABI mode is fast enough for the fixture corpus.
