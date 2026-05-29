---
title: "11. Pyodide, WASI, and embedded"
sidebar_position: 12
sidebar_label: "11. Pyodide / WASI / embedded"
description: "The Pyodide (wasm32-emscripten) target, WASI Preview 2 (wasm32-wasip2) component model, the no-CPython subset, embedded MEP-53 target compatibility, what Python packages Mochi can consume in the browser and on bare-metal, the MicroPython subset, the firmware story."
---

# 11. Pyodide, WASI, and embedded

This note covers the alternative deployment targets where Python runs alongside Mochi: the browser (via Pyodide), the WASI ecosystem (wasm32-wasip2 component model), and embedded contexts (MicroPython subset, MEP-53 target). Each constrains the bridge differently from the host Linux/macOS/Windows case.

## Pyodide (wasm32-emscripten)

Pyodide is a CPython port to WebAssembly, compiled with Emscripten. It runs the full CPython interpreter (currently 3.13 in Pyodide 0.27+) inside a browser page or Node.js process. The wheel ecosystem includes ~200 pre-built scientific Python packages (numpy, pandas, scipy, scikit-learn, matplotlib, sympy) compiled to wasm32-emscripten.

What Pyodide is good at:

- Pure-Python packages: any pure-Python wheel installs as-is from PyPI.
- Pre-built scientific packages: numpy, scipy, pandas, scikit-learn work out of the box via Pyodide's curated wheel index.
- DOM access: Pyodide exposes JavaScript objects to Python and vice versa, enabling browser scripting.

What Pyodide is not good at:

- Native packages outside Pyodide's curated set: building a wasm32-emscripten wheel for an arbitrary Rust or C extension is currently manual.
- Filesystem-heavy packages: Pyodide's filesystem is in-memory (or backed by IndexedDB); slow.
- Subprocess-using packages: no subprocess in browsers.

The bridge's Pyodide story:

- The Mochi runtime can be compiled to wasm32-emscripten (this is MEP-53's emscripten target).
- The Mochi-side runtime + Pyodide cohabit in the same wasm instance.
- The wrapper module pattern still works: the wrapper is generated as wasm32-emscripten Python that calls into the Mochi runtime via JS interop.
- The bridge surfaces a `SkipReason::PyodideUnavailable` for packages not in Pyodide's curated list when the target is `wasm32-emscripten`.

The phase plan reserves Phase 16 for Pyodide / WASI target support.

## WASI Preview 2: the component model

WASI Preview 2 (released January 2024 stable) introduces the component model: typed interfaces between wasm modules expressed in WIT (WebAssembly Interface Types). The `wasm32-wasip2` target is the canonical WASI Preview 2 target.

CPython on WASI Preview 2 is in active development. The CPython 3.13+ tree has `wasm32-wasi` and `wasm32-wasip2` support upstream. A few caveats:

- The WASI filesystem and clock APIs are exposed; sockets are emerging in Preview 2.
- No threads in Preview 2 (threading is Preview 3 territory).
- No fork or subprocess.
- The C extension ecosystem is essentially empty on WASI; only pure-Python packages and packages with WASI-targeting builds work.

The bridge's WASI story:

- The Mochi runtime can be compiled to wasm32-wasip2 (MEP-53 WASI target).
- A WIT interface defines the wrapper boundary: Mochi exports the host-side functions; Python imports them as a wasm component.
- Pure-Python packages work; native packages require a wasm32-wasip2 wheel which is rare.
- The async bridge runs on a single-threaded asyncio loop (no threading).

The phase plan covers this in Phase 16 alongside Pyodide.

## The no-CPython subset

There are two paths to running Python code without CPython:

- **MicroPython / CircuitPython**: a smaller Python interpreter for microcontrollers. ~200KB binary, ~64KB RAM minimum. Supports a subset of Python 3.x (no GIL, no asyncio, no typing module at runtime, no most stdlib).
- **RustPython**: a CPython-API-compatible Python interpreter written in Rust. ~5MB binary. More compatible than MicroPython but slower than CPython.

The bridge's stance on these:

- **MicroPython**: not a `[python].implementation` value. The Mochi-to-MicroPython story is a transpiler-level concern (MEP-51 emits MicroPython-compatible Python with a `--target=micropython` flag, but that's a separate scope). MEP-71's bridge does not target MicroPython.
- **RustPython**: future direction. `[python].implementation = "rustpython"` would link RustPython instead of CPython. The wrapper module pattern adapts because RustPython implements the CPython C API. Phase 17+ scope.
- **GraalPy** (Oracle's Python on the JVM/Graal): similar to RustPython. `[python].implementation = "graalpy"`. Phase 17+ scope.

## Embedded MEP-53 target

MEP-53 (the Rust transpiler) defines an embedded subset of the Mochi runtime: no GC, no goroutines, no stdlib I/O. The Mochi-to-Rust bridge (MEP-73) extends this to a `no_std` subset of Rust crates.

For MEP-71, the parallel question is: can Mochi consume Python deps in an embedded context? The answer is mostly no:

- Embedded targets typically have no Python interpreter at all. The only Python that runs is MicroPython, and the bridge does not target MicroPython.
- The wrapper module pattern requires libpython linked in, which is impossible on a microcontroller.

The MEP-71 phase plan does not include an embedded subset. Users who need Python-like behaviour on embedded targets should use MEP-51 (Mochi-to-Python transpiler) with the `--target=micropython` flag, which produces MicroPython-compatible source code without linking a Python interpreter.

The closest analog: a Mochi app running on an embedded MEP-53 target that talks to a Python service over a network or serial protocol. This is a deployment pattern, not a bridge feature.

## Pyodide-specific wheel tags

Pyodide uses the wheel tag `cp313-cp313-pyodide_2025_0_wasm32`. The wheel must be built for the exact Pyodide version (because the JS interop ABI changes between minors) and for the exact CPython version.

The bridge's publish path for Pyodide:

- `[python.publish].wheel-tags = ["py3-none-any", "cp313-cp313-pyodide_2025_0_wasm32"]` opts in.
- The Mochi backend builds two wheels: a generic py3-none-any (the default) and a Pyodide-specific wheel using the Pyodide build toolchain.
- The Pyodide wheel can be loaded directly in a browser via Pyodide's `micropip.install()`.

This path is currently experimental and gated on Pyodide's release cadence; it's not in the Phase 0-15 critical path.

## What kind of Python can Mochi consume in each target

| Target | Pure-Python wheels | Native wheels | asyncio | threading | subprocess | Notes |
|--------|---------------------|---------------|---------|-----------|------------|-------|
| linux-x64 | yes | yes | yes | yes | yes | The reference platform. |
| linux-arm64 | yes | yes (manylinux_2_28_aarch64) | yes | yes | yes | Same as x64. |
| macos-x64 | yes | yes (macosx_11_0_x86_64) | yes | yes | yes | Apple Silicon Rosetta works too. |
| macos-arm64 | yes | yes (macosx_11_0_arm64) | yes | yes | yes | The Apple Silicon native target. |
| windows-x64 | yes | yes (win_amd64) | yes | yes | yes | The Windows reference. |
| wasm32-emscripten (Pyodide) | yes | Pyodide curated only | yes (single-threaded) | no | no | Browser/Node.js. |
| wasm32-wasip2 | yes | rare | yes (single-threaded) | no | no | Edge / serverless wasm. |
| embedded MEP-53 | no | no | no | no | no | No CPython available. |

## The Mochi-side runtime mode in each target

The `[python].runtime-mode` value differs by target:

- `embedded` (default on host): libpython is linked into the Mochi binary. The bridge uses `PyImport_AppendInittab` to register the wrapper module, then `Py_Initialize` to start the interpreter.
- `subprocess` (host alternative): the Mochi process spawns a separate `python -m mochi_runtime.subprocess_server` and communicates via JSON-RPC. Slower per call but isolates Python crashes from the Mochi process.
- `pyodide` (wasm32-emscripten): the Mochi runtime is loaded into the same wasm instance as Pyodide; cross-calls use JS interop.
- `wasi` (wasm32-wasip2): the wrapper is a wasm component; calls go through the component-model interface.

Each mode has its own glue layer in `package3/python/runtime/`. The configuration auto-detects the target at build time.

## Cross-references

- [[09-abi-stability]] for the wheel tag computation in each target.
- [[10-gil-and-cextensions]] for the GIL story under Pyodide and WASI.
- [[12-risks-and-alternatives]] for the deferred-target risks.
- [Pyodide docs](https://pyodide.org/).
- [WASI Preview 2 announcement](https://bytecodealliance.org/articles/WASI-0.2).
- [MicroPython](https://micropython.org/).
- [MEP-53](/docs/mep/mep-0053) for the embedded Mochi target.
