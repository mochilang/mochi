// Package pyodide is the MEP-71 Phase 16 surface for WebAssembly Python
// targets. It does three things, all of which the wheel resolver and
// the install orchestrator dial in:
//
//  1. Recognize and rank the WASM platform tags PyPI + the Pyodide
//     distribution publish: `pyodide_<year>_<rev>_wasm32`,
//     `emscripten_<major>_<minor>_<patch>_wasm32`, and the (still
//     experimental) `wasi_p2_wasm32` / `cp3XY_abi3_wasi_p2` shapes.
//     Use ParsePlatformTag to lift one into a Tag with Kind and the
//     vintage fields populated.
//
//  2. Select the right wheel for a given Target from a candidate
//     list using Selector. A Target captures Python ABI tag + the
//     wasm runtime (TargetPyodide vs TargetWASIPreview2) + an
//     optional vintage floor (so installs can pin to >=pyodide_2024_0
//     when a package only fixed a bug in that year's recompile).
//
//  3. Emit a WIT (WebAssembly Interface Type) world describing the
//     Python surface a wheel exports under the WASI Preview 2
//     component model. WIT.Render produces a `world ... { import:
//     ... export: ... }` block the host can compose against. Sub-phase
//     16.1 wires this into the wrapper synthesiser so `extern python`
//     functions emit a real WIT export when the target is WASI.
//
// Sub-phases 16.1 (WIT emit in wrapper), 16.2 (live Pyodide index
// client at pyodide.org/distribution), and 16.3 (mochi pkg install
// --target=pyodide CLI verb) ship separately so the umbrella gate
// stays offline.
package pyodide
