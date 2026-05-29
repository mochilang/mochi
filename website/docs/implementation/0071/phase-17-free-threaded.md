---
title: "Phase 17. Free-threaded CPython"
sidebar_position: 19
sidebar_label: "Phase 17. Free-threaded"
description: "MEP-71 Phase 17 implementation tracking: cp3XYt ABI tag matcher, PEP 703 module-marker audit, and PyMutex lock shim renderer."
---

# Phase 17. Free-threaded CPython 3.13t / 3.14t

PEP 703 ships CPython without the GIL as 3.13t / 3.14t builds. The
MEP-71 bridge has to:

- tell `cp313` apart from `cp313t` in wheel resolution so we never
  link a GIL-assuming extension into a free-threaded interpreter;
- audit compiled extensions for the PEP 703
  `Py_mod_gil = Py_MOD_GIL_NOT_USED` module-init slot so we can flag
  silent unsafety before the install lands;
- emit lock shims in the wrapper synthesiser that pick
  `threading._PyMutex` on free-threaded builds and the legacy
  `threading.Lock` on GIL builds, with a runtime self-check that
  warns when a GIL-rendered shim ends up imported into a free-threaded
  process.

## Status

`LANDED` (pending PR merge). Phase 17 ships the offline ABI parser +
wheel compatibility classifier + module-marker auditor + lock-shim
renderer. The live ELF / Mach-O / PE marker reader, the wrapper-side
wiring, and the CLI verbs split into sub-phases 17.1 / 17.2 / 17.3.

## Gate

`go test ./package3/python/freethread/... -count=1`

Covers:

- `ParseABITag` happy across `cp312`, `cp313`, `cp313t`, `cp314`,
  `cp314t`, `cp315t` + 8 error paths (non-`cp` interpreter, missing
  version, version too short, non-numeric major / minor, `cp312t`
  rejected because PEP 703 requires Python >= 3.13).
- `ABITag.Compatible` matrix: cp313 -> cp313 OK, cp313 vs cp313t
  rejected both ways, cp313 vs cp314 rejected, pure-Python ("none"
  interpreter) accepted by either side, pp313 rejected by cp313.
- `WheelCompat.Score` matrix: pure -> SupportFull,
  free-threaded extension -> SupportFull, legacy on free-threaded ->
  SupportUntested, legacy on legacy -> SupportFull, deny-list ->
  SupportIncompatible (overrides ABI tag), minor mismatch ->
  SupportIncompatible + error.
- `AuditExtension`: PEP 703 marker yes (FreeThreaded = true, no
  violation), Py_MOD_GIL_USED declared (violation mentions
  Py_MOD_GIL_USED), no marker (violation mentions missing slot),
  reader error propagates, nil reader rejected.
- `AuditWheel`: all extensions safe -> OK, mixed -> violations
  populated.
- `IsExtensionFilename` accepts `.so` / `.dylib` / `.pyd`
  case-insensitively.
- `RenderLockShim` validates (rejects empty name, non-Python
  identifier, empty critical-section entries, duplicate entries) +
  emits the GIL branch with `_LockKind = threading.Lock` +
  `RuntimeWarning` self-check + emits the PyMutex branch with
  `hasattr(threading, '_PyMutex')` fallback.
- `phase17_test.go` umbrella sentinel: numpy free-threaded wheel
  end-to-end + Score + AuditWheel, legacy extension flagged in both
  the resolver and the auditor, denylisted package overrides ABI
  tag, lock shim renders the correct primitive for each strategy.

## Files

```
package3/python/freethread/
  doc.go                # 4-responsibility overview
  abi.go                # ABITag + ParseABITag + Compatible
  support.go            # SupportLevel + WheelClass + WheelCompat.Score
  audit.go              # ModuleMarker + MarkerReader + AuditExtension/AuditWheel
  lock.go               # LockShim + LockStrategy + RenderLockShim (Python source)
  abi_test.go           # 4 tests
  support_test.go       # 8 tests
  audit_test.go         # 11 tests
  lock_test.go          # 6 tests
  phase17_test.go       # 1 umbrella sentinel (4 sub-cases)
```

## Sub-phase decomposition

### 17.1. Live module-marker reader

Today `StaticMarkerReader` is what powers the audit. Sub-phase 17.1
adds an ELF / Mach-O / PE reader sharing the `abi3.SymbolReader`
contract from phase 13. The reader walks the extension's PyInit
section looking for the `Py_mod_gil` slot value (literal
`Py_MOD_GIL_USED = 0` vs `Py_MOD_GIL_NOT_USED = 1`).

### 17.2. Wire RenderLockShim into wrapper synthesiser

The wrapper synthesiser (phase 5) emits `<pkg>_externs.py` +
`_mochi_wrap.py`. Sub-phase 17.2 adds a third file
`_mochi_freethread.py` rendered through `RenderLockShim`, with one
shim per (module, attr) tuple the type-mapping table flagged as
shared-mutable state.

### 17.3. CLI verbs + deny-list config

- `--runtime=free-threaded` -> `WheelCompat.Target.FreeThreaded = true`.
- `--allow-untested-freethread` flips `SupportUntested` from an
  error gate to a warning gate.
- `[python].freethread-deny-list = ["lxml<6", ...]` populates
  `WheelCompat.DenyList`.
- `mochi.lock` gets a `[python].freethread-status` field so the
  resolver does not have to re-classify on every install.

## Fixtures

Phase 17 is selector / auditor / renderer only. Sub-phase 17.2 will
add Python golden files under
`package3/python/freethread/testdata/lock/` to lock in the rendered
shim shape across both strategies.

## Cross-references

- [PEP 703 (Making the GIL Optional in CPython)](https://peps.python.org/pep-0703/)
- [PEP 489 (Multi-phase init)](https://peps.python.org/pep-0489/) for
  the module-init slot machinery the marker rides on.
- [PEP 425 (Compatibility Tags)](https://peps.python.org/pep-0425/)
  for the ABI tag shape `cp3XY[t]`.
- [phase 13](/docs/implementation/0071/phase-13-abi3) for the
  abi3 auditor whose `SymbolReader` interface sub-phase 17.1 reuses.
- [phase 5](/docs/implementation/0071/phase-05-wrapper) for the
  wrapper synthesiser that sub-phase 17.2 will tap into.
