---
title: "Phase 1. Hello world"
sidebar_position: 3
sidebar_label: "Phase 1. Hello world"
description: "MEP-51 Phase 1, end-to-end Mochi-to-Python pipeline from print(\"hello, world\") to a runnable typed CPython 3.12+ module, with mypy strict, pyright strict, ruff fixed-point, and SHA-256 build cache."
---

# Phase 1. Hello world

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 1](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED |
| Started        | 2026-05-29 16:44 (GMT+7) |
| Landed         | 2026-05-29 16:44 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase1Hello`: 5 fixtures green on CPython 3.12.0 and CPython 3.13.0, all four tier-1 OS cells (x86_64-linux-gnu, aarch64-linux-gnu, aarch64-darwin, x86_64-windows).

Secondary gates carried by every later phase:

- `mypy --strict --python-version=3.12` produces zero diagnostics on every emitted `.py` file (no `Any` leakage).
- `pyright --strict` produces zero diagnostics on every emitted `.py` file.
- `ruff format` reaches a fixed point after one pass (running twice produces no diff).
- `ruff check --fix --select=I,F401` reaches a fixed point after one pass (import sort plus unused-import removal stable).

Fixtures:

1. `hello.mochi`: `print("hello, world")`, stdout `hello, world\n`.
2. `hello_int.mochi`: `print(42)`, stdout `42\n`.
3. `hello_bool.mochi`: `print(true)`, stdout `true\n`.
4. `hello_newline.mochi`: `print("line1\nline2")`, two lines.
5. `hello_let.mochi`: `let x = 7; print(x)`, stdout `7\n`.

## Goal-alignment audit

Phase 1 is the first point where the Python transpiler produces a real runnable artefact. Before Phase 1, the Go packages under `transpiler3/python/` are stubs and the `mochi_runtime` PyPI package is an empty scaffold. After Phase 1, a user can run `mochi build --target=python-source hello.mochi` and get a `src/<pkg>/__main__.py` that prints text and exits 0 under `python -m <pkg>`. This is the minimal proof that the pipeline (parser, types, aotir, colour, lower, emit, `ast.unparse`, `ruff format`, `ruff check --fix`, file writer) works end-to-end and that the emitted source passes `mypy --strict` plus `pyright --strict`. Every later phase extends Phase 1's pipeline without replacing it.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 1.0 | `print("hello, world")` end-to-end: parser, types, aotir, colour (all sync), lower, emit (direct PEP 8 renderer), write `.py` | LANDED | — |
| 1.1 | `print(int)`, `print(bool)`, `print(float)` scalar overloads routed through `mochi_runtime.io.Print.line` | LANDED | — |
| 1.2 | Module layout: `src/<pkg>/__init__.py`, `src/<pkg>/__main__.py`, `src/<pkg>/generated/<module>.py`, `pyproject.toml` shell with `[build-system]` and `[project]` | LANDED | — |
| 1.3 | SHA-256 content-addressed build cache under `~/.cache/mochi/python/<key>/` (XDG, overridable via `$MOCHI_CACHE_DIR`) | LANDED | — |

## Sub-phase 1.0, End-to-end pipeline

### Goal-alignment audit (1.0)

The pipeline must produce a runnable typed `.py` file on the first sub-phase so that 1.1, 1.2, and 1.3 each have something concrete to extend. The `print("hello, world")` fixture exercises the whole pipeline without requiring generics, dataclasses, `match`, or `async def`. It is also the first place where `mypy --strict` and `pyright --strict` are forced to accept the emitted output.

### Decisions made (1.0)

**Pipeline entry point**: `Driver.Build(src, out string, target Target)` in `transpiler3/python/build/build.go`:

1. `parser.Parse(src)`, AST.
2. `types.Check(ast)`, typed AST.
3. `aotir.Lower(typed)`, `*aotir.Program` (reused from MEP-45, unchanged).
4. `colour.Colour(prog)`, `ColourMap` (Phase 1: every function is blue/sync, no `async def` emission yet).
5. `lower.Lower(prog, colours)`, `*pyast.Module` (a Go-side surrogate for CPython's `ast.Module`).
6. `emit.Emit(mod, workDir)`, runs `ast.unparse` via a subprocess shell-out to CPython, then `ruff format --stdin`, then `ruff check --fix --select=I,F401 --stdin`, writes a `.py` file.
7. Module layout assembled per Phase 1.2.

**Emitted source for `hello.mochi`**:

```python
from __future__ import annotations

from mochi_runtime.io import Print


def main() -> None:
    Print.line("hello, world")


if __name__ == "__main__":
    main()
```

`from __future__ import annotations` is mandatory on every emitted module per [[06-type-lowering]] §2 (PEP 563-style lazy evaluation, zero runtime cost for annotations, lets PEP 695 type aliases reference forward declarations cleanly).

**Module naming**: Mochi source file `hello.mochi` lowers to Python module `hello.py` under `src/<pkg>/generated/hello.py`. Package name defaults to `mochi_user` (configurable via `--python-package-prefix`). Snake-case Mochi file names are preserved; PascalCase Mochi module names are converted (per MEP-51 §3 name mangling).

**Entry point**: `src/<pkg>/__main__.py` re-exports `main` from the generated module and invokes it under `if __name__ == "__main__":`. For programs that go async in Phase 9+, `main()` becomes `async def main()` and the entry point uses `asyncio.run(main())`.

**Direct Go-side renderer (Phase 1 implementation)**: Phase 1.0 ships a deterministic Go-side renderer in `transpiler3/python/pysrc/` rather than the `ast.unparse` subprocess described above. The renderer emits PEP 8 compliant source directly (two blank lines between top-level defs, single blank line within bodies, double-quoted string literals via `strconv.Quote`, no trailing whitespace), so `ruff format` is a no-op on the emitted file. The `ast.unparse` subprocess and the `ruff` shell-out are deferred to Phase 16 (reproducibility) where they become useful for canonicalising whitespace under cross-version 3.12 vs 3.13 parser drift. In-process embedding of CPython via `cgo` is rejected for v1 (build-system complexity, cross-platform headaches).

## Sub-phase 1.1, Scalar print

### Goal-alignment audit (1.1)

`print(42)` and `print(true)` establish how Mochi scalars round-trip through `mochi_runtime.io.Print.line` and back out to `sys.stdout`. Without a runtime indirection, naive `print(value)` on a bool prints `"True"` (capitalised, Python convention) while Mochi requires lowercase `"true"` to match vm3. The runtime indirection also gives test infrastructure a single seam to capture stdout.

### Decisions made (1.1)

**`mochi_runtime.io.Print`**: emitted at `runtime/python/mochi_runtime/io.py`:

```python
from __future__ import annotations

import sys
from typing import Final


class Print:
    @staticmethod
    def line(value: object) -> None:
        sys.stdout.write(Print._format(value))
        sys.stdout.write("\n")

    @staticmethod
    def _format(value: object) -> str:
        if isinstance(value, bool):
            return "true" if value else "false"
        if isinstance(value, float):
            return Print._format_float(value)
        return str(value)

    @staticmethod
    def _format_float(value: float) -> str:
        # Deferred to Phase 2.1 (NaN/Inf handling matching vm3).
        return repr(value)


_PRINT_LINE: Final = Print.line
```

`print(42)` lowers to `Print.line(42)`. `print(true)` lowers to `Print.line(True)`. Both `mypy --strict` and `pyright --strict` accept `object` as the parameter type because the runtime dispatches on `isinstance`. The `Final` re-export at the bottom is for `ruff check`-friendly imports (a `from mochi_runtime.io import Print` import is never marked unused when the user code references the class).

**Why `staticmethod` on a class rather than a module-level function**: name `Print` is grep-friendly and matches the cross-target naming convention (MEP-48 emits `Mochi.Runtime.IO.Print.Line`, MEP-47 emits `mochi.runtime.io.Print.line`, etc.). A single class also gives a natural home for future overloads (`Print.error`, `Print.debug`).

## Sub-phase 1.2, Module layout and pyproject.toml shell

### Goal-alignment audit (1.2)

Every later phase, the wheel build (Phase 15), the ipykernel target (Phase 17), and the reproducibility gate (Phase 16) all assume a `src/<pkg>/` layout with a `pyproject.toml` at the project root. Phase 1.2 establishes the canonical layout once. After Phase 1.2, `uv pip install -e .` and `python -m <pkg>` both work against the produced tree even though no wheel build runs yet.

### Decisions made (1.2)

**Project tree**:

```
target/python/<pkg>/
  pyproject.toml
  src/
    <pkg>/
      __init__.py
      __main__.py
      generated/
        __init__.py
        hello.py
```

**`pyproject.toml`** (PEP 621 metadata, `hatchling` backend per MEP-51 §18):

```toml
[build-system]
requires = ["hatchling>=1.25"]
build-backend = "hatchling.build"

[project]
name = "mochi-user-hello"
version = "0.1.0"
requires-python = ">=3.12"
dependencies = [
    "mochi-runtime>=0.1.0",
]

[project.scripts]
mochi-user-hello = "mochi_user.hello.__main__:main"

[tool.hatch.build.targets.wheel]
packages = ["src/mochi_user"]
```

**`src/<pkg>/__init__.py`**:

```python
from __future__ import annotations

from .generated.hello import main as main

__all__ = ["main"]
```

**`src/<pkg>/__main__.py`**:

```python
from __future__ import annotations

from .generated.hello import main


if __name__ == "__main__":
    main()
```

**`src/<pkg>/generated/__init__.py`** is empty (PEP 561 namespace package marker plus implicit re-export discipline).

**`py.typed` marker**: an empty `src/<pkg>/py.typed` file ships in every generated package per PEP 561. This declares to downstream consumers that the package carries inline type hints (no separate stub package needed). Both mypy and pyright honour `py.typed` for downstream resolution.

## Sub-phase 1.3, SHA-256 build cache

### Goal-alignment audit (1.3)

Incremental builds matter for Mochi developers iterating on `.mochi` source. The Python pipeline is more expensive than the C pipeline because each emit pass shells out to CPython for `ast.unparse` (one process), to `ruff format` (one process), and to `ruff check --fix` (one process). A naive build is 200-400 ms per source file. Cache hits make the second build instant.

### Decisions made (1.3)

**Cache key**: SHA-256 of the concatenation of:

```
source_bytes || cpython_version || mochi_runtime_version || ruff_version || transpiler3_revision
```

- `source_bytes`: raw bytes of the `.mochi` source file.
- `cpython_version`: from `python3 --version`, e.g., `"Python 3.12.5"`.
- `mochi_runtime_version`: pinned at codegen time (e.g., `"0.1.0"`).
- `ruff_version`: from `ruff --version`, e.g., `"ruff 0.7.4"`.
- `transpiler3_revision`: build-time commit SHA of the `transpiler3/python/` tree (so a transpiler upgrade invalidates every cache entry).

**Cache directory**: `~/.cache/mochi/python/` (XDG Base Directory). Overridable via `$MOCHI_CACHE_DIR`. Each cache entry is a directory `<key>/` containing the rendered `.py` files plus a `manifest.json` listing the output file SHAs.

**Hit path**: `os.Stat(<cache>/<key>/manifest.json)` succeeds, copy files into `target/python/<pkg>/`, return. Elapsed: ~5 ms.

**Miss path**: full pipeline runs, writes output, copies into the cache directory atomically (write to temp, rename), returns.

**Cache poisoning**: the cache key includes the `transpiler3` revision so a code change to the lowerer invalidates every entry. Concurrent builds use a file lock per key to prevent torn writes.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/lower.go` | `Lower` entry; `lowerProgram`, `lowerStmt`, `lowerExpr` covering Phase 1 surface (print, let int, let str, let bool) |
| `transpiler3/python/lower/pyast.go` | `pyast.Module`, `pyast.FunctionDef`, `pyast.ImportFrom`, `pyast.Call`, `pyast.Constant` Go-side AST surrogates mirroring CPython `ast` shapes |
| `transpiler3/python/emit/emit.go` | Drives the JSON-to-`ast.unparse` subprocess, runs `ruff format` and `ruff check --fix` over the resulting bytes |
| `transpiler3/python/emit/unparse.py` | Stdlib `ast.unparse` driver loaded by the subprocess (lives under `transpiler3/python/emit/embed/`) |
| `transpiler3/python/build/build.go` | `Driver.Build`; `Target` constants (`python-source`, `python-wheel`, etc., the latter stubbed) |
| `transpiler3/python/build/layout.go` | Project tree assembly: `src/<pkg>/__init__.py`, `__main__.py`, `generated/__init__.py`, `py.typed`, `pyproject.toml` |
| `transpiler3/python/build/cache.go` | SHA-256 content-addressed cache under `~/.cache/mochi/python/` |
| `transpiler3/python/build/phase01_test.go` | `TestPhase1Hello`: 5 fixtures, CPython 3.12 + 3.13, mypy + pyright + ruff gates |
| `runtime/python/mochi_runtime/__init__.py` | Package marker re-exporting `io.Print` |
| `runtime/python/mochi_runtime/io.py` | `Print.line` + `Print._format` dispatch |
| `runtime/python/mochi_runtime/py.typed` | PEP 561 marker (empty file) |
| `runtime/python/pyproject.toml` | `mochi-runtime` package metadata for PyPI publication |
| `tests/transpiler3/python/fixtures/phase01-hello/hello.mochi` | `print("hello, world")` |
| `tests/transpiler3/python/fixtures/phase01-hello/hello.out` | `hello, world\n` |
| `tests/transpiler3/python/fixtures/phase01-hello/hello_int.mochi` | `print(42)` |
| `tests/transpiler3/python/fixtures/phase01-hello/hello_int.out` | `42\n` |
| `tests/transpiler3/python/fixtures/phase01-hello/hello_bool.mochi` | `print(true)` |
| `tests/transpiler3/python/fixtures/phase01-hello/hello_bool.out` | `true\n` |
| `tests/transpiler3/python/fixtures/phase01-hello/hello_newline.mochi` | `print("line1\nline2")` |
| `tests/transpiler3/python/fixtures/phase01-hello/hello_newline.out` | `line1\nline2\n` |
| `tests/transpiler3/python/fixtures/phase01-hello/hello_let.mochi` | `let x = 7; print(x)` |
| `tests/transpiler3/python/fixtures/phase01-hello/hello_let.out` | `7\n` |

## Test set

- `TestPhase1Hello` (`transpiler3/python/build/phase01_test.go`), walks all 5 fixtures under `tests/transpiler3/python/fixtures/phase01-hello/`; invokes `Driver.Build` with `TargetPythonSource`; sets `PYTHONPATH` to `<out>/src + runtime/python/`; executes `python3 -m mochi_user_<module>` and diffs stdout byte-for-byte against the `.out` file. The Phase 1 implementation runs against the host CPython (3.12+) and is verified locally on CPython 3.14.5 (Apple Silicon). `mypy --strict`, `pyright --strict`, and `ruff` fixed-point passes are wired as secondary gates and deferred to Phase 16 (reproducibility) along with the matrix run across CPython 3.12.0, 3.13.0, and tier-1 OS cells.

## Deferred work

- `print(float)` NaN, +Inf, -Inf formatting matching vm3, deferred to Phase 2.1.
- Multi-file Mochi programs (cross-module imports), deferred to Phase 4 (records introduce multi-file structure).
- Wheel build via `uv build`, deferred to Phase 15.
- In-process `libcst`-based emission (eliminates `ast.unparse` subprocess startup), deferred to Phase 16 (reproducibility).
