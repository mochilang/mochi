---
title: "Phase 12. FFI (Python externs)"
sidebar_position: 17
sidebar_label: "Phase 12. FFI"
description: "MEP-51 Phase 12 -- extern python fun lowers to a from <pkg>_externs import import; sidecar <name>_externs.py shipped beside the .mochi; Go FFI, JS FFI, Java FFI, and C extern reject at lower time; 10 fixtures."
---

# Phase 12. FFI (Python externs)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 12](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED (12.0 only; ctypes / CFFI DEFERRED) |
| Started        | 2026-05-29 19:55 (GMT+7) |
| Landed         | 2026-05-29 20:01 (GMT+7) |
| Tracking issue | (filled at ship) |
| Tracking PR    | (filled at ship) |

## Gate

`TestPhase12FFI`: 10 fixtures green on CPython 3.12+ (locally verified against CPython 3.14.5 on Apple Silicon) in `transpiler3/python/build/phase12_test.go`. The corpus covers all four scalar types as both arguments and returns (`int`, `float`, `bool`, `string`), unary and 2-arity signatures, two fixtures stressing whole-number float return via `mochi_runtime.fmt.float_str` (`py_add_floats`, `py_float_div`), one fixture with multiple `extern python fun` declarations from a single sidecar (`py_two_decls`), and one fixture that crosses the scalar boundary by returning `int` from a `float` argument (`py_round`). Each fixture rebuilds from `tests/transpiler3/python/fixtures/phase12-ffi/*.mochi`, copies the sidecar `<name>_externs.py` next to the `.mochi` into the generated `src/<pkg>_externs.py`, runs `python -m mochi_user_<name>`, and byte-compares stdout to the matching `.out`. The tier-1 OS matrix and `mypy --strict` / `pyright --strict` are carried by the cross-host reproducibility workflow introduced in Phase 16.

## Goal-alignment audit

The Python target is a Python target. Asking it to also route Python FFI through the same JSON-stdin subprocess protocol the C target uses (which exists because C cannot call Python natively) would be overengineered: the natural thing for `extern python fun X(...)` on the Python target is a direct Python function call. Phase 12.0 is what turns "the Python target accepts `extern python fun` at all" from false to true. Without 12.0, every Mochi program that declares an external Python function rejects at the Python target with "C extern not supported", which is the gate that blocks Phase 13 (LLM helpers are Python externs against OpenAI / Anthropic SDKs) and Phase 14 (the fetch surface is a Python extern against `httpx`).

The ctypes / CFFI surface originally scoped for Phase 12 (call into a C shared library from Python) is deferred. Mochi's `extern python fun` covers the load-bearing case: native interop with the Python ecosystem (NumPy, requests, scikit, ML SDKs). The ctypes path serves a smaller use case (callout to a C library that does not already have a Python binding) and can ride on top of the same `_externs.py` sidecar shape: the user's sidecar contains the ctypes wrapper, and the generated Mochi code calls it via the same import. Landing 12.0 standing alone is correct precisely because the sidecar pattern composes: a future "12.1 ctypes ergonomics" sub-phase would add a generated `ctypes.CDLL(...)` wrapper at the top of the sidecar without changing the call-site emit.

Go FFI, JS FFI, Java FFI, and C extern decls are all out-of-scope for the Python target by design: a Python program calling Go through cgo or Java through JNI is a deeply niche use case that does not appear in the v1 corpus and would tie the runtime to a specific cross-language bridge (gRPC, py4j, etc.). The lower rejects each with a clear "not supported on Python target" error so the user knows to pick a different target or refactor.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 12.0 | `extern python fun X(...)` to `from mochi_user_<modname>_externs import X` + sidecar copy; CallStmt / CallExpr strip `mochi_py_` prefix; non-Python externs reject at lower time | LANDED 2026-05-29 | (filled at ship) |
| 12.1 | ctypes wrapper generation: `extern c fun X(...)` from a header signature, emit a ctypes prototype in the sidecar | DEFERRED |  -- |
| 12.2 | CFFI 1.17+ wrapper: opaque pointers, callbacks, in/out params | DEFERRED | -- |
| 12.3 | `@mochi_export` decorator + PEP 561 `py.typed` shipping path so other Python code can import the generated user module | DEFERRED to Phase 15 (wheel + sdist) | -- |

## Sub-phase 12.0 -- Python FFI via sidecar import

### Goal-alignment audit (12.0)

The Python extern surface is the load-bearing emit shape for every Python ecosystem integration: every Mochi program that wants to call NumPy, httpx, OpenAI SDK, etc. does so through `extern python fun X`. If the import shape is wrong, every later integration sub-phase has to re-litigate where the implementations live, how they get onto PYTHONPATH, and how the call site reaches them. Landing 12.0 first locks the shape: each program declares its externs, the build copies a sidecar `<modname>_externs.py` to `src/<pkg>_externs.py`, the generated code imports bare names from that sidecar and calls them directly.

### Decisions made (12.0)

**Sidecar lives next to the .mochi source, not embedded in the .mochi.** Mochi v1 has no body for `extern python fun X`; the keyword `extern` declares the signature, not the implementation. The Python target needs the implementation at build time so the import resolves. The convention is: a file named `<moduleName>_externs.py` (one directory up from the package layout, alongside the `.mochi`) contains a Python module exporting one function per `extern python fun` declaration. The build copies it byte-for-byte into `src/<pkgName>_externs.py`; no transformation, no codegen, no JSON shim. The user's Python is the user's Python.

**Top-level sidecar module, not nested inside the user package.** The sidecar lives at `src/mochi_user_<modname>_externs.py`, a sibling of `src/mochi_user_<modname>/`. Two reasons: (1) keeps the generated user package read-only (no user code inside it), so build cache invalidation only needs to track the package contents; (2) lets the user replace the sidecar in production with a wheel-installed version under the same name without touching the generated package. The PYTHONPATH includes `src/`, so both modules are importable side-by-side.

**Missing sidecar is a build error, not a warning.** If a program declares `extern python fun X` but the matching sidecar file does not exist, the Python build fails with `extern python fun declared but sidecar <path> not found`. The alternative (emit a stub `def X(*args): raise NotImplementedError(...)`) defers the error from build time to run time, which is worse: the user finds out at execution that an extern is missing rather than at compile.

**Sidecar import is a single `from ... import name1, name2, ...` line.** All extern names from one program go into one import statement; the lowerer sorts them alphabetically so the emit is deterministic across runs. Splitting into one import per name would bloat the emitted source for programs with many externs (Phase 13 LLM and Phase 14 fetch will each ship 4-6 helpers per program); the single-import form is what `ruff format` produces.

**`mochi_py_<name>` prefix strip at CallStmt and CallExpr.** The C lower mangles every extern python call as `mochi_py_<name>` so the C emitter can pick the right JSON wrapper from the C runtime. The Python lower strips that prefix at both call sites and emits the bare name, matching the `from <sidecar> import <name>` line. The check is `strings.CutPrefix(s.Func, "mochi_py_")`, which is O(1) per call site.

**Sidecar copy uses `copyFile`, not a content-addressed cache key.** The build cache key is over the .mochi source bytes plus the Python toolchain version (`mep51-phase12` marker). Adding the sidecar to the hash would invalidate the cache every time the user edits the externs, which is the wrong default: most extern edits do not change the call shape that the lower emits. The sidecar copy is unconditional and idempotent; if the user wants a cache miss, they edit the .mochi.

**Go, JS, Java FFI, and C extern reject at lower time with an explicit error.** The Mochi C aotir IR carries `prog.GoFuncs`, `prog.JSFuncs`, `prog.JavaFuncs`, and `prog.ExternFuncs`. The Python lower rejects each with a clear "not supported on Python target" error, naming the offending decl. The alternative (silently ignore) would let a program that declares but never calls a Go FFI compile, then fail at run time when something does call it.

### Fixture corpus (10 fixtures)

`tests/transpiler3/python/fixtures/phase12-ffi/`:

| Fixture | Surface | Notes |
|---------|---------|-------|
| `py_add_floats.mochi` | `py_add(x: float, y: float): float` | Float round-trip; `1.5 + 2.5 == 4.0` prints `4` via `float_str` whole-number collapse |
| `py_float_div.mochi` | `py_div(a: float, b: float): float` | Float `/` from Python (true division); `7.5 / 2.5 == 3.0` prints `3` |
| `py_str_lower.mochi` | `py_lower(s: string): string` | String round-trip; exercises `str.lower()` |
| `py_str_upper.mochi` | `py_upper(s: string): string` | String round-trip mirror; pins string round-trip in both case directions |
| `py_str_concat.mochi` | `py_concat(a: string, b: string): string` | Two-arg string return; ensures the import line orders the params correctly |
| `py_int_mul.mochi` | `py_mul(x: int, y: int): int` | Int round-trip; two calls in one program |
| `py_int_sub.mochi` | `py_sub(x: int, y: int): int` | Int round-trip; non-commutative operand order |
| `py_bool_not.mochi` | `py_negate(b: bool): bool` | Bool round-trip; lowercase `true`/`false` print via `Print._format` |
| `py_round.mochi` | `py_round(x: float): int` | Cross-type return: float argument, int return; exercises the `int` print path for an FFI-produced value |
| `py_two_decls.mochi` | `py_double` + `py_inc` | Multiple externs from one sidecar; chained calls; deterministic alphabetic ordering in the emitted `from ... import` line |

Each fixture has a matching `_externs.py` sidecar (the implementation) and a `.out` file with the canonical vm3 stdout. `TestPhase12FFI` walks the directory, runs `runPythonFixture` (which now also copies the sidecar via the `len(prog.PythonFuncs) > 0` branch in `build.go`). All 10 fixtures pass on CPython 3.14.5 (Apple Silicon).

### Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/ffi.go` (new) | `rejectNonPythonExterns`, `registerPythonExterns`, `pythonExternNames`, `stripPythonExternPrefix` |
| `transpiler3/python/lower/lower.go` | `Lower(prog, moduleName)` signature; `pythonExterns` map + emit gating; CallStmt + CallExpr strip `mochi_py_` prefix; emit `from mochi_user_<modname>_externs import ...` |
| `transpiler3/python/build/build.go` | Pass moduleName to lower.Lower; copy `<srcDir>/<moduleName>_externs.py` to `src/<pkgName>_externs.py` when `prog.PythonFuncs != nil`; cache marker bump |
| `transpiler3/python/build/phase12_test.go` (new) | `TestPhase12FFI` walks `phase12-ffi/` |
| `tests/transpiler3/python/fixtures/phase12-ffi/` (new) | 10 `.mochi` + 10 `_externs.py` + 10 `.out`, expanded from the original 5 to cover unary and 2-arity per scalar type, whole-number float collapse, and cross-type return (`float -> int`) |

## Deferred work

- **12.1 ctypes wrapper generation.** `extern c fun X(...)` from a C header signature should emit a ctypes prototype (`lib.X.argtypes = [...]; lib.X.restype = ...`) inside the sidecar. Deferred because no v1 fixtures use C FFI on the Python target and the use case (call into a C library that lacks a Python binding) is rare next to the Python-ecosystem case.
- **12.2 CFFI 1.17+ wrapper.** Opaque pointers, callbacks, in/out params. Same rationale as 12.1: deferred until a real fixture needs it.
- **12.3 `@mochi_export` decorator.** Allows other Python code to import the generated user module via `import mochi_user_<name>` and call exposed functions. Deferred to Phase 15 (wheel + sdist) where the package layout's `__init__.py` re-exports are formalized.
- **Async externs.** `extern python async fun X(...)` would lower to `await X(...)`. Deferred to Phase 11.1 alongside the async colour pass.
- **Typed sidecar verification.** `mypy --strict` checking that the sidecar's signature matches the Mochi declared signature would catch type-skew at build time. Deferred to Phase 15 (when `mypy --strict` becomes a tertiary gate across all phases).
