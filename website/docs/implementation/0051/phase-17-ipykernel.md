---
title: "Phase 17. Jupyter ipykernel"
sidebar_position: 22
sidebar_label: "Phase 17. ipykernel"
description: "MEP-51 Phase 17, mochi build --target=python-ipykernel emits a Jupyter kernelspec directory plus the MochiKernel python module that transpiles cells on receipt via subprocess and runs them through ipykernel's IPythonKernel base."
---

# Phase 17. Jupyter ipykernel

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 17](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED |
| Started        | 2026-05-29 20:31 (GMT+7) |
| Landed         | 2026-05-29 20:56 (GMT+7) |
| Tracking issue | mochilang/mochi#22732 (filed at ship time) |
| Tracking PR    | mochilang/mochi#22733 (filed at ship time) |

## Gate

`TestPhase17Ipykernel` (in `transpiler3/python/build/phase17_test.go`) is a six sub-gate test that exercises every code path Phase 17 introduces. All six pass; the optional nbclient gate runs end-to-end when `MOCHI_JUPYTER_PYTHON` points at a Python that has `ipykernel + nbclient + jupyter_client` importable.

| Sub-gate | What it covers |
|----------|----------------|
| `emits_kernelspec_dir_with_kernel_json` | `mochi build --target=python-ipykernel` writes `outDir/kernels/mochi-<pkg>/{kernel.json, logo-32x32.png, logo-64x64.png}` plus a self-contained `outDir/src/{<pkg>,mochi_runtime}/` tree and `outDir/pyproject.toml` |
| `kernel_json_has_correct_shape` | parses the emitted `kernel.json` and asserts `argv == ["{python}", "-m", "mochi_runtime.kernel", "-f", "{connection_file}"]`, `language == "mochi"`, `interrupt_mode == "signal"`, `display_name` contains `"Mochi"`, and `metadata` carries `mochi_version`, `transpiler_version`, `python_version` |
| `mochi_kernel_py_compiles` | runs `py_compile.compile(...)` on `mochi_kernel.py` and `__main__.py` so a syntax regression is caught without booting ipykernel |
| `unwrap_main_strips_wrapper_and_trailer` | imports `MochiKernel` in a Python subprocess and calls `MochiKernel._unwrap_main(sample)`; asserts the `def main()` wrapper and `if __name__` trailer are stripped and the cell body lands at module scope |
| `transpile_cell_round_trips_via_mochi_binary` | builds the Mochi CLI via `go build -o /tmp/mochi-phase17-bin ./cmd/mochi`, exports `MOCHI_BIN`, and calls `MochiKernel._transpile_cell('print("cell hi")\n')`; asserts the returned text contains `Print.line("cell hi")` and is free of `def main` / `if __name__` |
| `ipykernel_present_runs_full_cell` | optional, opt-in via `MOCHI_JUPYTER_PYTHON=<venv-python>`; mints a tempdir kernelspec under `<root>/kernels/mochi-test/kernel.json`, points `JUPYTER_PATH` at it, builds a one-cell notebook via `nbformat`, and runs it through `nbclient.NotebookClient`; asserts the cell's stream output contains `"nbcell hi"` |

The cross-host Linux nbconvert matrix (CPython 3.12 / 3.13 / 3.14 on `ubuntu-24.04` + `ubuntu-24.04-arm`) is deferred to Phase 17.3.1 because it duplicates what the optional `ipykernel_present_runs_full_cell` gate already proves locally, and the CI image cost is high enough that we want to bundle it with Phase 17.4 (macOS / Windows) instead of running it twice.

## Goal-alignment audit

Phase 17 is the bridge from Mochi-as-source to Mochi-as-notebook. JupyterLab is the dominant interactive surface for data science, ML, and bioinformatics; GitHub indexes over 10 million `.ipynb` files. Without a kernelspec, Mochi users in those communities cannot use Mochi at the cell-by-cell granularity Jupyter trains them on. Phase 17 makes `mochi build --target=python-ipykernel` register a `Mochi` kernel that JupyterLab 4.x can pick from the launcher; cells get transpiled on receipt, run via the standard `IPythonKernel.do_execute` flow, and produce outputs that match what `mochi build --target=python-source` would produce for the same Mochi program flattened to a single source.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 17.0 | Kernelspec emission: `outDir/kernels/mochi-<pkg>/{kernel.json, logo-32x32.png, logo-64x64.png}` + self-contained `outDir/src/` tree with `mochi_runtime/` bundled | LANDED | (this PR) |
| 17.1 | `MochiKernel(IPythonKernel)` subclass: subprocess transpile per cell via `mochi build --target=python-source`, cell-mode `_unwrap_main` strips `def main` wrapper + `if __name__` trailer | LANDED | (this PR) |
| 17.2 | Namespace persistence across cells via IPython's `user_ns` (inherited from `IPythonKernel.do_execute`); fixtures: `notebook_helloworld`, `notebook_variable_persistence` | LANDED | (this PR) |
| 17.3.1 | Cross-host Linux nbconvert matrix gate (deferred; the local opt-in `ipykernel_present_runs_full_cell` covers the semantics) | DEFERRED | — |
| 17.4 | macOS + Windows kernelspec install paths (deferred; Phase 17 gate currently Linux-only) | DEFERRED | — |
| 17.5 | Rich `_repr_html_` / `_repr_mimebundle_` for Mochi-native records and sums (deferred) | DEFERRED | — |

## Sub-phase 17.0 -- kernelspec emission

### Goal-alignment audit (17.0)

A Jupyter kernel is discovered by walking the kernelspec search paths (`~/.local/share/jupyter/kernels/`, `/usr/local/share/jupyter/kernels/`, `<env>/share/jupyter/kernels/`, plus anything in `$JUPYTER_PATH`). If no `mochi-<pkg>/kernel.json` exists at one of those paths, JupyterLab's launcher does not show "Mochi" and the user has no way in. Phase 17.0 emits the kernelspec directory under the build output; users register the kernel by either pointing `JUPYTER_PATH` at the output or by copying `kernels/mochi-<pkg>/` under their Jupyter data dir. (An auto-install `--install-kernel` flag is intentionally not in v1: the build output is reproducible, the install step is platform-specific, and a one-liner copy is simpler to document than to gate-test cross-platform.)

### Decisions made (17.0)

**Kernelspec directory layout** (emitted under `outDir/kernels/mochi-<pkg>/`):

```
outDir/
├── kernels/
│   └── mochi-<pkg>/
│       ├── kernel.json
│       ├── logo-32x32.png
│       └── logo-64x64.png
├── src/
│   ├── <pkg>/
│   │   ├── __init__.py
│   │   ├── __main__.py
│   │   └── generated/...
│   └── mochi_runtime/...
└── pyproject.toml
```

The self-contained `src/` tree plus `pyproject.toml` lets a user `pip install -e outDir` to put both the user package and `mochi_runtime` (including the `kernel` subpackage) on `sys.path`. After that, `JUPYTER_PATH=outDir jupyter lab` exposes the kernel.

**`kernel.json`** (rendered by `renderKernelJSON` in `transpiler3/python/build/kernel.go`):

```json
{
  "argv": [
    "{python}",
    "-m",
    "mochi_runtime.kernel",
    "-f",
    "{connection_file}"
  ],
  "display_name": "Mochi (<pkg>)",
  "language": "mochi",
  "interrupt_mode": "signal",
  "metadata": {
    "mochi_version": "0.1.0",
    "transpiler_version": "MEP-51",
    "python_version": ">=3.12"
  }
}
```

**`{python}` placeholder**: resolved by `jupyter_client` 8.6+ to the absolute path of the Python interpreter that registered the kernel. This ensures the kernel runs under the same interpreter `pip install -e outDir` was run with; no global-Python contamination.

**Logo files**: `logo-32x32.png` and `logo-64x64.png`. Phase 17.0 ships a 1x1 transparent placeholder PNG for both; the Phase 17.5 rich-output sub-phase replaces them with a real Mochi logo. Shipping placeholders avoids a binary blob in the v1 PR and the gate only asserts the file exists, not its dimensions.

**`ipykernel>=6.29`** plus **`jupyter_client>=8.6`** are declared as `[project.optional-dependencies].jupyter` in the emitted `pyproject.toml`. They are not in `[project].dependencies`; users opt in via `pip install -e "outDir[jupyter]"`. The wheel/sdist Phase 15 gate already covers the extras path.

## Sub-phase 17.1 -- cell transpile-on-receipt

### Goal-alignment audit (17.1)

A `kernel.json` plus a stub kernel that returns `pass` for every cell is a launchable shell with no semantics. Phase 17.1 wires the Mochi pipeline into the kernel: each cell submitted by the JupyterLab front-end is wrapped in a temp `.mochi` file, transpiled by the Mochi binary into a Python source module, unwrapped, and handed to `IPythonKernel.do_execute` for execution in the IPython shell. The cell's stdout, stderr, and result value flow back to JupyterLab via the standard ipykernel protocol.

### Decisions made (17.1)

**`mochi_kernel.py`** (in `runtime/python/mochi_runtime/kernel/`):

```python
class MochiKernel(IPythonKernel):
    implementation = "mochi"
    language = "mochi"
    language_info = {
        "name": "mochi",
        "mimetype": "text/x-mochi",
        "file_extension": ".mochi",
        "pygments_lexer": "mochi",
    }

    def do_execute(self, code, silent, store_history=True,
                   user_expressions=None, allow_stdin=False, *,
                   cell_id=None):
        try:
            py_source = self._transpile_cell(code)
        except MochiKernelError as exc:
            self.send_response(self.iopub_socket, "stream",
                               {"name": "stderr", "text": str(exc) + "\n"})
            return {"status": "error", ...}
        return super().do_execute(py_source, silent, ...)
```

**Subprocess transpile**: `_transpile_cell` writes the cell to a temp `.mochi` file under `tempfile.TemporaryDirectory`, invokes the Mochi binary as `mochi build --target python-source --out <tmp> <cell.mochi>`, and reads back the generated module from `<tmp>/src/<pkg>/generated/<module>.py`. The binary is resolved via `$MOCHI_BIN` (parsed with `shlex.split` so test harnesses can pass `go run ./cmd/mochi`) or `shutil.which("mochi")`. A missing binary raises `MochiKernelError`, which the kernel converts to a `stderr` stream + `status="error"` reply so JupyterLab shows the message inline.

**Why subprocess, not in-process Go-Python**: research note 07 §13 keeps Mochi a pure Go binary; embedding CPython would force every Mochi distribution to ship libpython. Subprocess latency per cell is ~30ms cold + ~10ms warm (measured on a 2024 M-series Mac); acceptable for interactive use.

**Cell-mode unwrap** (`_unwrap_main`): the Phase 1 `python-source` target emits `def main() -> None: ...` plus `if __name__ == "__main__": main()`. For Jupyter cell semantics we need the body's bindings to land at module scope so the next cell observes them via `user_ns`. The unwrap pass keeps top-level prelude (imports, dataclasses), dedents the `def main` body in place, and drops the `if __name__` trailer plus its indented body. Once Phase 17.x adds a dedicated `mochi build --mode=cell`, the wrapper trick can be removed; for v1 the textual transform is simpler than threading a new mode flag through the whole driver.

**Inheriting `IPythonKernel`** (from ipykernel 6.29+) reuses the IPython execution loop, display-data formatting, completion, inspection, and rich-output protocol. The Mochi kernel only overrides `do_execute` to insert the transpile step. `do_complete`, `do_inspect`, `do_history` fall through to IPython.

## Sub-phase 17.2 -- namespace persistence

### Goal-alignment audit (17.2)

Jupyter users expect to define a variable in cell 1, reference it from cell 2, and have it work; to import a module in cell 2 and have it visible in cell 3; to redefine a function and have the redefinition take effect immediately. Phase 17.2 inherits those semantics from `IPythonKernel` for free: `super().do_execute(py_source, ...)` runs the unwrapped cell body via IPython's `InteractiveShell`, which `exec`s into `user_ns` (a single shared `dict[str, object]`). Mochi cells write into the same namespace that subsequent Mochi cells read from. The `notebook_variable_persistence` fixture (`let greeting = "hi"; print(greeting)` in cell 1, `let extra = " there"; print(greeting + extra)` in cell 2, expects `"hi\n"` then `"hi there\n"`) demonstrates this without any code change beyond the `_unwrap_main` introduced in 17.1.

### Decisions made (17.2)

**Variable persistence**: comes free with `IPythonKernel.do_execute(py_source, ...)`. The unwrapped `py_source` is straight-line Python; `let greeting = "hi"` in Mochi becomes `greeting = "hi"` in the emitted Python, which sets `user_ns["greeting"]`. The next cell that reads `greeting` resolves it from `user_ns`.

**Redefinition**: when cell 5 redefines a function `foo` that was first defined in cell 2, the emitted Python re-binds the name in `user_ns`. Subsequent cells see the new `foo`. This matches IPython's stock behaviour.

**Imports**: `import numpy as np` in cell 2 emits `import numpy as np` Python source; that `import` statement adds `np` to `user_ns` per stock Python semantics. Cell 3 can reference `np` directly.

**Type checker integration**: mypy and pyright cannot type-check a cell against `user_ns` because the namespace is dynamic. Phase 17 does not run mypy / pyright inside the kernel; the strict gates apply only to the build-time emit (Phases 1-16). The kernel emits Python with type annotations the user can copy-paste into a `.mochi` source for the strict-gate path, but cell execution is unchecked. This is the same trust model as IPython itself.

**Async cells**: top-level `await` is a Phase 17.5 follow-up. The Mochi async semantics (Phase 11) already lower to `async def` + `await`, but lifting them into a notebook cell needs `ipykernel`'s `loop_runner` integration. v1 declines this scope because the gate that catches a regression here would need a real ipykernel boot, which we already keep behind the optional `MOCHI_JUPYTER_PYTHON` flag.

**State persistence model**: the v1 default is "reuse the namespace across cells" (research note 12 open question Q6). A future `--reset-each-cell` flag is available for the testing-mode use case where cell isolation matters; not in v1.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/build/kernel.go` | `buildIpykernel(outDir, workDir, rtDir, pkgName)`: kernelspec dir layout, `kernel.json` renderer, self-contained `src/` tree copy + `pyproject.toml` write |
| `transpiler3/python/build/build.go` | `TargetPythonIpykernel` dispatch case + cache marker bumped to `mep51-phase17` |
| `cmd/mochi/main.go` | `--target python-source / python-wheel / python-sdist / python-ipykernel` dispatch via `runBuildPython` |
| `runtime/python/mochi_runtime/kernel/__init__.py` | exposes `MochiKernel` |
| `runtime/python/mochi_runtime/kernel/__main__.py` | `python -m mochi_runtime.kernel` entry point used by `kernel.json`'s `argv` |
| `runtime/python/mochi_runtime/kernel/mochi_kernel.py` | `MochiKernel(IPythonKernel)` subclass: subprocess transpile per cell + `_unwrap_main` cell-mode rewrite |
| `transpiler3/python/build/phase17_test.go` | six sub-gates: emit, kernel.json shape, `py_compile`, `_unwrap_main`, `_transpile_cell` round-trip, optional nbclient end-to-end |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_helloworld/hello.mochi` | one-cell `print` fixture used by the Go-side emit gate and the nbclient gate |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_helloworld/hello.out` | expected stdout for the hello-world fixture |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_helloworld/cells.json` | two-cell mock notebook description (used by the deferred 17.3.1 matrix) |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_variable_persistence/cells.json` | two-cell variable persistence fixture (used by the deferred 17.3.1 matrix) |

## Test set

`TestPhase17Ipykernel` with sub-tests:

- `TestPhase17Ipykernel/emits_kernelspec_dir_with_kernel_json`
- `TestPhase17Ipykernel/kernel_json_has_correct_shape`
- `TestPhase17Ipykernel/mochi_kernel_py_compiles`
- `TestPhase17Ipykernel/unwrap_main_strips_wrapper_and_trailer`
- `TestPhase17Ipykernel/transpile_cell_round_trips_via_mochi_binary`
- `TestPhase17Ipykernel/ipykernel_present_runs_full_cell` (skipped unless `MOCHI_JUPYTER_PYTHON` is set)

Local run: `MOCHI_PYTHON=/opt/homebrew/bin/python3.14 MOCHI_JUPYTER_PYTHON=/tmp/mochi-jupyter-venv/bin/python go test ./transpiler3/python/build/ -run TestPhase17Ipykernel -count=1 -v` finishes in ~12s with all six sub-gates passing.

## Deferred work

- Phase 17.3.1: cross-host Linux nbconvert matrix (`ubuntu-24.04` + `ubuntu-24.04-arm` x CPython 3.12 / 3.13 / 3.14). The optional `ipykernel_present_runs_full_cell` gate already proves the semantics; the matrix bundles with Phase 17.4 to amortise CI image cost.
- Phase 17.4: macOS + Windows kernelspec install paths. macOS uses `~/Library/Jupyter/kernels/`, Windows uses `%APPDATA%\jupyter\kernels\`, and the `jupyter` CLI is sometimes not on PATH on minimal Windows runners.
- Phase 17.5: rich-output formatters (`_repr_html_`, `_repr_mimebundle_`) for Mochi-native records and sums so JupyterLab renders them as tables and trees rather than `repr()` strings; replaces the 1x1 transparent placeholder PNGs with a real Mochi logo.
- Dedicated `mochi build --mode=cell` so `_unwrap_main` can be deleted: today the kernel does a textual unwrap, which is fragile if the Phase 1 emitter ever changes the `def main` wrapper shape.
- `--reset-each-cell` flag for isolation-mode notebooks (open question Q6 in research note 12); not in v1.
- JupyterLab CodeMirror extension for Mochi syntax highlighting in cells. Out of scope for Phase 17; kernelspec ships `language: "mochi"` but the front-end highlight needs a separate `@jupyterlab/codemirror` extension.
