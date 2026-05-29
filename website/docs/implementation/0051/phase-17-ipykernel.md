---
title: "Phase 17. Jupyter ipykernel"
sidebar_position: 22
sidebar_label: "Phase 17. ipykernel"
description: "MEP-51 Phase 17, mochi build --target=python-ipykernel installs a Jupyter kernelspec; each cell is transpiled on receipt and executed in an in-process IPython shell with namespace persistence; nbconvert diff is the gate."
---

# Phase 17. Jupyter ipykernel

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 17](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase17IpykernelPython`: 10 dedicated `.ipynb` fixtures (`notebook_helloworld`, `notebook_variable_persistence`, `notebook_function_redefinition`, `notebook_import_in_cell`, `notebook_error_handling`, `notebook_async_cell`, `notebook_plot_matplotlib`, `notebook_query_dsl`, `notebook_record_definition`, `notebook_multi_cell_workflow`) plus 15 derived from prior fixture categories (records, sums, query DSL, agents) replayed as multi-cell notebooks. Gate: `jupyter nbconvert --to notebook --execute fixtures/<name>.ipynb --output /tmp/actual.ipynb` followed by `jq`-filtered diff of `cells[].outputs` against `expect.ipynb`. Filter strips `execution_count`, cell `id`, and per-cell metadata; preserves `outputs[].text` and `outputs[].data["text/plain"]`. Runs on `ubuntu-24.04` + Python 3.12.7 only (Phase 17 scope; macOS and Windows nbconvert gates deferred).

## Goal-alignment audit

Phase 17 is the bridge from Mochi-as-source to Mochi-as-notebook. JupyterLab is the dominant interactive surface for data science, ML, and bioinformatics; GitHub indexes over 10 million `.ipynb` files. Without a kernelspec, Mochi users in those communities cannot use Mochi at the cell-by-cell granularity Jupyter trains them on. Phase 17 makes `mochi build --target=python-ipykernel --install-kernel` register a `Mochi` kernel that any JupyterLab 4.x user can pick from the launcher; cells get transpiled on receipt, run in an in-process IPython shell, and produce outputs that match what vm3 would produce for the same Mochi program flattened to a single source.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 17.0 | Kernelspec emission: `~/.local/share/jupyter/kernels/mochi-<pkg>/kernel.json` + `mochi_kernel.py` + logos + `jupyter kernelspec install` invocation | NOT STARTED | — |
| 17.1 | Cell transpile-on-receipt: each cell parsed, typed, lowered, executed in in-process IPython shell; `ipykernel 6.29+` kernel base class | NOT STARTED | — |
| 17.2 | State persists across cells via `InteractiveShell.user_ns`; redefinition + import-in-cell semantics | NOT STARTED | — |
| 17.3 | nbconvert execution gate: `jupyter nbconvert --execute` per fixture, `jq`-filtered diff vs `expect.ipynb` | NOT STARTED | — |

## Sub-phase 17.0 -- kernelspec emission

### Goal-alignment audit (17.0)

A Jupyter kernel is discovered by walking the kernelspec search paths (`~/.local/share/jupyter/kernels/`, `/usr/local/share/jupyter/kernels/`, ...). If no `mochi-<pkg>/kernel.json` exists at one of those paths, JupyterLab's launcher does not show "Mochi" and the user has no way in. Phase 17.0 emits the kernelspec directory and shells out to `jupyter kernelspec install` so the kernel actually appears.

### Decisions made (17.0)

**Kernelspec directory layout** (emitted under `dist/kernels/mochi-<pkg>/` and copied to the Jupyter search path on `--install-kernel`):

```
dist/kernels/mochi-<pkg>/
├── kernel.json
├── mochi_kernel.py
├── logo-32x32.png
├── logo-64x64.png
└── logo-svg.svg
```

**`kernel.json`**:

```json
{
  "argv": [
    "{python}",
    "-m",
    "mochi_kernel",
    "-f",
    "{connection_file}"
  ],
  "display_name": "Mochi 0.1 (mochi-example-app)",
  "language": "mochi",
  "interrupt_mode": "signal",
  "metadata": {
    "mochi_version": "0.1.0",
    "transpiler_version": "MEP-51",
    "python_version": ">=3.12"
  }
}
```

**`{python}` placeholder**: resolved by `jupyter_client` 8.6+ to the absolute path of the Python interpreter that registered the kernel. This ensures the kernel runs under the same interpreter `uv venv` produced for the project; no global-Python contamination.

**Install invocation** in `transpiler3/python/build/kernel.go`:

```go
cmd := exec.Command("jupyter", "kernelspec", "install",
    filepath.Join(distDir, "kernels", "mochi-"+pkgName),
    "--user",
    "--name", "mochi-"+pkgName,
    "--replace", // overwrite a stale spec from a prior install
)
```

**`--user`** installs to `~/.local/share/jupyter/kernels/`; no `sudo`. `--replace` is needed so re-running `mochi build --target=python-ipykernel --install-kernel` upgrades a previously-installed kernel without manual cleanup.

**Logo dimensions**: 32x32 and 64x64 PNGs per Jupyter convention, plus an SVG that JupyterLab 4.x prefers for theme-aware rendering. The emitter ships a stock Mochi logo from `transpiler3/python/assets/`.

**`ipykernel>=6.29`** plus **`jupyter_client>=8.6`** are declared as `[project.optional-dependencies].jupyter` in the emitted `pyproject.toml`. They are not in `[project].dependencies`; users opt in via `pip install "<pkg>[jupyter]"`. Phase 15's `wheel_with_extras` fixture covers the extras path.

## Sub-phase 17.1 -- cell transpile-on-receipt

### Goal-alignment audit (17.1)

A `kernel.json` plus a stub kernel that returns `pass` for every cell is a launchable shell with no semantics. Phase 17.1 wires the Mochi pipeline into the kernel: each cell submitted by the JupyterLab front-end is parsed as Mochi, type-checked, lowered through `aotir`, emitted as Python source, and executed in an in-process `IPython.core.interactiveshell.InteractiveShell`. The cell's stdout, stderr, and result value flow back to JupyterLab via the standard ipykernel protocol.

### Decisions made (17.1)

**`mochi_kernel.py`** structure:

```python
"""Mochi Jupyter kernel.

Each cell is transpiled by shelling out to `mochi transpile
--target=python --mode=cell`, then executed in an in-process IPython
shell.  Namespace persists across cells via the shell's user_ns.
"""
from __future__ import annotations

import subprocess
from typing import Any

from ipykernel.ipkernel import IPythonKernel


class MochiKernel(IPythonKernel):
    implementation = "mochi"
    implementation_version = "0.1.0"
    language = "mochi"
    language_version = "0.1.0"
    language_info = {
        "name": "mochi",
        "mimetype": "text/x-mochi",
        "file_extension": ".mochi",
        "pygments_lexer": "mochi",
    }
    banner = "Mochi 0.1 (MEP-51 transpile-to-python)"

    def do_execute(
        self,
        code: str,
        silent: bool,
        store_history: bool = True,
        user_expressions: dict[str, Any] | None = None,
        allow_stdin: bool = False,
        *,
        cell_id: str | None = None,
    ) -> dict[str, Any]:
        py_source = self._transpile_cell(code)
        return super().do_execute(
            py_source,
            silent,
            store_history=store_history,
            user_expressions=user_expressions,
            allow_stdin=allow_stdin,
            cell_id=cell_id,
        )

    @staticmethod
    def _transpile_cell(mochi_source: str) -> str:
        result = subprocess.run(
            ["mochi", "transpile", "--target=python", "--mode=cell", "-"],
            input=mochi_source,
            capture_output=True,
            text=True,
            check=True,
        )
        return result.stdout
```

**`mochi transpile --mode=cell`** is a new CLI mode for the Mochi binary that accepts a single cell of Mochi source on stdin, treats it as a relaxed-grammar fragment (top-level statements allowed, no `main` required, top-level expressions print like the REPL), and writes Python source to stdout. The mode flag is documented in MEP-51 §2 (build driver UX).

**Subprocess vs in-process Mochi**: Phase 17.1 uses subprocess. An in-process Go-Python bridge is faster but requires Mochi to ship a CPython extension module; that contradicts the "Mochi CLI is a Go binary" stance from research note 07 §13. Subprocess latency per cell is ~30ms cold + ~10ms warm; acceptable for interactive use.

**Inheriting `IPythonKernel`** (from ipykernel 6.29) reuses the IPython execution loop, display-data formatting, completion, inspection, and rich-output protocol. The Mochi kernel only overrides `do_execute` to insert the transpile step. `do_complete`, `do_inspect`, `do_history` fall through to IPython.

## Sub-phase 17.2 -- namespace persistence + cell semantics

### Goal-alignment audit (17.2)

Jupyter users expect to define a function in cell 1, call it from cell 5, and have it work; to import numpy in cell 2 and have it visible in cell 3; to redefine a function and have the redefinition take effect immediately. Phase 17.2 wires those semantics through the Mochi-to-Python boundary so cell-by-cell Mochi feels like cell-by-cell Python.

### Decisions made (17.2)

**Namespace**: `IPython.core.interactiveshell.InteractiveShell.user_ns` is a Python `dict[str, object]` that holds every name defined by every cell so far. The Mochi cell transpile pass emits Python that reads from and writes to the same `globals()` (which is `user_ns` inside the shell). No special bridging required; IPython's existing `exec(code, user_ns, user_ns)` does the right thing.

**Redefinition**: when cell 5 redefines a function `foo` that was first defined in cell 2, the emitted Python re-binds the name in `user_ns`. Subsequent cells see the new `foo`. This matches IPython's stock behaviour.

**Imports**: `import numpy as np` in cell 2 emits `import numpy as np` Python source; that `import` statement adds `np` to `user_ns` per stock Python semantics. Cell 3 can reference `np` directly.

**Type checker integration**: mypy and pyright cannot type-check a cell against `user_ns` because the namespace is dynamic. Phase 17 does not run mypy / pyright inside the kernel; the strict gates apply only to the build-time emit (Phases 1-15). The kernel emits Python with type annotations the user can copy-paste into a `.mochi` source for the strict-gate path, but cell execution is unchecked. This is the same trust model as IPython itself.

**Async cells**: ipykernel 6.29+ supports top-level `await` in cells (since IPython 7.0 via `asyncio` event loop integration). When the Mochi cell contains an `await` expression, the emitter wraps the cell body in an `async def __mochi_cell_<n>():` and the kernel's `do_execute` schedules it on the running event loop. The `notebook_async_cell` fixture covers this path.

**State persistence model**: the v1 default is "reuse the namespace across cells" (research note 12 open question Q6). A future `--reset-each-cell` flag is available for the testing-mode use case where cell isolation matters; not in v1.

## Sub-phase 17.3 -- nbconvert execution gate

### Goal-alignment audit (17.3)

The user-visible value of Phase 17 is "Mochi notebooks work in JupyterLab". The way the gate verifies this without spinning up a browser is `jupyter nbconvert --execute`, which runs every cell through the kernel headlessly and writes the result back as an `.ipynb`. Phase 17.3 diffs the executed notebook against a committed `expect.ipynb` after stripping run-specific metadata, giving a deterministic gate.

### Decisions made (17.3)

**Runner pseudocode**:

```bash
for fixture in fixtures/phase17-ipykernel/*/; do
    mochi build --target=python-ipykernel --install-kernel \
        --kernel-name="mochi-test-$$"

    jupyter nbconvert --to notebook --execute \
        --ExecutePreprocessor.kernel_name="mochi-test-$$" \
        --ExecutePreprocessor.timeout=60 \
        --output /tmp/actual.ipynb \
        "$fixture/notebook.ipynb"

    jq -S '{cells: [.cells[] | {
        cell_type,
        source,
        outputs: [.outputs[]? | {output_type, text, data}]
    }]}' "$fixture/expect.ipynb" > /tmp/expect.filtered.json

    jq -S '{cells: [.cells[] | {
        cell_type,
        source,
        outputs: [.outputs[]? | {output_type, text, data}]
    }]}' /tmp/actual.ipynb > /tmp/actual.filtered.json

    diff -u /tmp/expect.filtered.json /tmp/actual.filtered.json

    jupyter kernelspec uninstall -y "mochi-test-$$"
done
```

**`jq` filter** strips `execution_count`, cell `id`, `metadata`, and notebook-level `kernelspec.name` (which embeds the per-PID kernel name from the install step). What remains is the cell source, output type, output text, and output `text/plain` data. This is what users see in JupyterLab; this is what the gate diffs.

**`--ExecutePreprocessor.timeout=60`**: 60-second per-cell budget. Phase 17 fixtures are small (hello world, a couple of comprehensions, an async sleep); 60 seconds is generous and accounts for the cold subprocess transpile on the first cell.

**No matplotlib display gate**: `notebook_plot_matplotlib` fixture is in the corpus but the gate diffs only the cell's `text/plain` output (which says `<Figure size 640x480 with 1 Axes>`); the rendered PNG is not diffed (PNG bytes are non-deterministic across matplotlib versions). Future work could add a perceptual diff.

**`ubuntu-24.04` + Python 3.12.7 only**: macOS and Windows kernelspec install paths differ (`~/Library/Jupyter/kernels/` on macOS, `%APPDATA%\jupyter\kernels\` on Windows), and the `jupyter` CLI is sometimes not on PATH on minimal Windows runners. The Phase 17 gate covers Linux only; Phase 17.4 (deferred) adds macOS and Windows.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/build/kernel.go` | `mochi build --target=python-ipykernel`: kernelspec dir emission + `jupyter kernelspec install` invocation |
| `transpiler3/python/emit/kernel_json.go` | `kernel.json` writer with `{python}` placeholder + display name + metadata |
| `transpiler3/python/emit/cell_mode.go` | Cell-mode lower path: relaxed top-level grammar, no `main` required, top-level expressions print like REPL |
| `runtime/python/mochi_runtime/kernel/mochi_kernel.py` | `MochiKernel(IPythonKernel)` subclass: `do_execute` invokes `mochi transpile --mode=cell` subprocess |
| `runtime/python/mochi_runtime/kernel/__main__.py` | `python -m mochi_kernel` entry point used by `kernel.json`'s `argv` |
| `transpiler3/python/assets/logo-32x32.png` | Kernel launcher icon (32x32) |
| `transpiler3/python/assets/logo-64x64.png` | Kernel launcher icon (64x64) |
| `transpiler3/python/assets/logo-svg.svg` | Theme-aware vector icon |
| `transpiler3/python/build/phase17_test.go` | `TestPhase17IpykernelPython`: 10 dedicated fixtures + 15 derived |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_helloworld/` | `notebook.ipynb` + `expect.ipynb` |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_variable_persistence/` | cell 1 defines `x`, cell 2 prints `x` |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_function_redefinition/` | cell 1 defines `f`, cell 2 redefines, cell 3 uses |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_import_in_cell/` | cell 1 imports a Mochi module, cell 2 calls into it |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_error_handling/` | cell raises, expect.ipynb captures the traceback excerpt |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_async_cell/` | top-level `await` cell |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_plot_matplotlib/` | numpy + matplotlib plot; diff on `text/plain` only |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_query_dsl/` | Mochi from/where/select inside a cell |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_record_definition/` | record declared in cell 1, used in cell 2 |
| `tests/transpiler3/python/fixtures/phase17-ipykernel/notebook_multi_cell_workflow/` | 6-cell ETL workflow that exercises persistence end-to-end |

## Test set

- `TestPhase17IpykernelPython` -- 10 dedicated fixtures + 15 derived. Sub-tests:
  - `TestPhase17IpykernelPython/notebook_helloworld`
  - `TestPhase17IpykernelPython/notebook_variable_persistence`
  - `TestPhase17IpykernelPython/notebook_function_redefinition`
  - `TestPhase17IpykernelPython/notebook_import_in_cell`
  - `TestPhase17IpykernelPython/notebook_error_handling`
  - `TestPhase17IpykernelPython/notebook_async_cell`
  - `TestPhase17IpykernelPython/notebook_plot_matplotlib`
  - `TestPhase17IpykernelPython/notebook_query_dsl`
  - `TestPhase17IpykernelPython/notebook_record_definition`
  - `TestPhase17IpykernelPython/notebook_multi_cell_workflow`
  - `TestPhase17IpykernelPython/derived_*` (15 sub-tests: records, sums, query, agents replayed as 2-3 cell notebooks)
- `TestPhase17KernelspecInstall` -- `mochi build --target=python-ipykernel --install-kernel` followed by `jupyter kernelspec list | grep mochi-test-$$` returning the new kernel.

## Deferred work

- macOS and Windows nbconvert gates (kernelspec install path differences). Tracked as Phase 17.4.
- Rich-output formatters for Mochi-native types (records, sums) so JupyterLab renders them as tables / trees rather than `repr()` strings. Tracked as Phase 17.5; requires registering `_repr_html_` / `_repr_mimebundle_` on the emitted dataclasses.
- `mochi build --target=python-ipykernel --reset-each-cell` flag for isolation-mode notebooks. Not in v1 (open question Q6 in research note 12).
- JupyterLab extension that gives Mochi syntax highlighting in cells via a custom CodeMirror mode. Out of scope; kernelspec ships `language: "mochi"` but front-end highlighting needs a separate `@jupyterlab/codemirror` extension.
- `nbclient` API direct invocation (bypassing the `jupyter` CLI) for faster gate runs. Useful if the gate budget becomes a CI bottleneck.
