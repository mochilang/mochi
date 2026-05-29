// Package freethread is the MEP-71 Phase 17 surface for free-threaded
// CPython (PEP 703). Free-threaded builds ship as 3.13t / 3.14t and
// expose:
//
//   - A new ABI tag flavour `cp3XYt` distinguishing the no-GIL build
//     from the legacy `cp3XY` build.
//   - The PyMutex primitive + atomic refcount, so C extensions can
//     stop relying on the GIL to serialise refcount writes.
//   - A per-module sentinel (`Py_GIL_DISABLED` / the
//     `Py_mod_gil = Py_MOD_GIL_NOT_USED` module-init flag) that
//     marks an extension as free-threaded-safe.
//
// Phase 17 ships three offline-deterministic surfaces:
//
//  1. ABI tag parsing: ParseABITag lifts `cp3XY` / `cp3XYt` into a
//     Tag carrying the Python version + a FreeThreaded bool. The
//     wheel resolver branches on this when the target runtime is
//     free-threaded.
//
//  2. A compatibility tier for wheels (SupportLevel): Full when the
//     wheel ships a `cp3XYt`-tagged variant or is pure-Python;
//     Untested when only a `cp3XY` variant is offered; Incompatible
//     when an extension is known to deadlock under free-threaded.
//
//  3. A ModuleMarker reader interface + AuditExtension /
//     AuditWheel that walks compiled extensions and looks for the
//     PEP 703 sentinel. The interface stays mockable so the umbrella
//     gate is offline; sub-phase 17.1 wires a real ELF / Mach-O / PE
//     reader (sharing the SymbolReader contract from phase 13).
//
//   4. A lock-wrapper renderer that emits Python source picking
//     threading.Lock on legacy builds and the PEP 703-aware
//     PyMutex bridge on free-threaded builds. RenderLockShim is what
//     the wrapper synthesiser (phase 5) will call in sub-phase 17.2.
//
// Sub-phases 17.1 (live module-marker reader), 17.2 (wire lock shim
// into wrapper synthesiser), and 17.3 (mochi pkg install --runtime=
// free-threaded CLI verb + per-package compatibility database)
// ship separately so the umbrella gate stays offline.
package freethread
