// Package wrapper synthesises the Python-side bridge code that MEP-71 emits
// for each consumed PyPI package. The output is a pair of source files:
//
//   - <pkg>_externs.py: imports the source package and re-exports each
//     translatable item through a thin shape-coercing shim. Async functions
//     get a synchronous run-loop entry; iterators get a list materialisation
//     companion; Optional returns pass through unchanged (Python `None`
//     maps to Mochi `None?`); dataclasses get a `_to_mochi_dict` companion.
//
//   - <pkg>_externs.pyi: PEP 561 stub matching the wrapper. Lets downstream
//     Python tooling (mypy, pyright) type-check the wrapper without re-walking
//     the source package's stub set.
//
// The companion runtime file `_mochi_wrap.py` provides shared helpers used by
// every wrapper (`_to_mochi_dict`, `_materialise_iter`, `_run_async`,
// `_persistent_loop`). It is emitted once per workspace, alongside the
// wrappers.
//
// The wrapper synthesiser is the last lock-time step before Phase 6's Mochi
// extern emitter. It takes the typed ModuleSurface from Phase 3 and the
// translation decisions from Phase 4, and writes the Python side of the
// bridge. See MEP-71 §6 "Build orchestration" and §7 "Async bridge runtime
// hook" for the surface shapes this package emits.
package wrapper
