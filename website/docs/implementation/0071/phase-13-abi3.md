---
title: MEP-71 Phase 13 (abi3 wheel slimming + auditwheel)
sidebar_position: 14
sidebar_label: "Phase 13. abi3 + auditwheel"
description: "MEP-71 Phase 13: cp32-abi3 wheel slimming + the auditwheel-equivalent platform tag validator for manylinux / musllinux / macOS / Windows wheel surfaces."
---

# MEP-71 Phase 13. abi3 wheel slimming + auditwheel

Status: **LANDED (pending merge)** as of 2026-05-30 00:51 (GMT+7). Implements two coupled policy layers:

- **abi3 slimming.** `PromoteWheelToABI3` rewrites a wheel filename's interpreter + abi fields from `cp3XX-cp3XX-<platform>` to `cp32-abi3-<platform>` so the wheel resolves on every CPython ≥ 3.2 without rebuilding. The matching `RenderWHEELMarker` emits the PEP 427 `WHEEL` metadata stub with the abi3 `Tag:` line.
- **auditwheel-equivalent platform validation.** `KnownProfiles` declares the closed allow-list for every supported platform tag (manylinux_2_17 / 2_28 / 2_34, musllinux_1_2, macosx_10_15 / 11_0, win_amd64 / arm64). `AuditExtension` reads a `.so` / `.dylib` / `.pyd`'s linked libs through a mockable `SymbolReader` interface and flags every external lib outside the profile's allow-list as a violation. `AuditWheel` aggregates per-extension reports for a wheel's RECORD entries.

The live ELF / Mach-O / PE walker that production callers wire behind `SymbolReader` is sub-phase 13.1; the Py_LIMITED_API discipline check (refusing to promote a wheel to abi3 when its extension references symbols outside the limited API) is sub-phase 13.2; the `mochi pkg audit --wheel <path>` CLI verb is sub-phase 13.3.

## Gate

The umbrella sentinel `TestPhase13ABI3` in `package3/python/abi3/phase13_test.go` is green. The sentinel:

- Promotes `mochi-pkg-0.1.0-cp312-cp312-manylinux_2_28_x86_64.whl` to `mochi-pkg-0.1.0-cp32-abi3-manylinux_2_28_x86_64.whl`, re-parses the result, and asserts `WheelTag.IsABI3()` reports true.
- Renders the `WHEEL` marker for the promoted wheel and asserts the `Tag: cp32-abi3-manylinux_2_28_x86_64` line is present.
- Audits two synthetic extensions (`mochi_pkg/_clean.so` linking only libc + libm, `mochi_pkg/_dirty.so` linking libssl.so.3) plus a non-extension file against the manylinux_2_28_x86_64 profile via a mock `SymbolReader`. Asserts the clean ext passes, the dirty ext violates with `libssl.so.3` named in the violation message, the non-extension file is skipped, and the aggregate `AuditWheel` reports ok=false.
- Audits a macOS extension (`mochi_pkg/_mac.dylib` linking `/usr/lib/libSystem.B.dylib` + a Homebrew lib) against `macosx_11_0_arm64` and asserts the Homebrew lib violates while libSystem passes — cross-platform coverage through the same code path.

Plus 31 unit tests (`go test ./package3/python/abi3/... -count=1`) covering:

- `ParseWheelTag`: 3-field parse OK + rejects wrong field count + rejects empty field; `String` round-trip.
- `WheelTag.IsABI3`: only `abi3` qualifies; `cp312` and `none` do not.
- `ToABI3`: promotes cp312-cp312 -> cp32-abi3; rejects PyPy (`pp310`); rejects non-numeric interpreter (`cpfoo`).
- `SplitWheelFilename`: pulls the trailing 3-field tag off `<dist>-<ver>-<interp>-<abi>-<platform>.whl`; rejects non-`.whl` and rejects too-few-fields inputs.
- `KnownProfiles`: every entry's `Tag` matches its map key and `AllowedLibs` is sorted (binary-search invariant).
- `LookupProfile`: returns the entry for known tags + reports ok=false for future-dated tags.
- `ManylinuxProfile.Allows`: libc.so.6 in manylinux_2_28 yes; libssl.so.3 no; libSystem in macosx_11_0 yes; Homebrew libpng no; KERNEL32.dll in win_amd64 yes; third-party OpenSSL no; musl libc in musllinux yes; glibc libc no.
- `ProfileTags`: sorted slice with cardinality equal to KnownProfiles.
- `AuditExtension`: rejects nil reader + zero profile; happy path on a clean extension; flags disallowed external lib with the right violation message; propagates reader errors.
- `IsExtensionFilename`: case-insensitive `.so` / `.dylib` / `.pyd`; rejects `.py` / `.pyi`.
- `AuditWheel`: aggregates per-extension reports; ok=false when any extension violates; ok=true on a clean wheel; skips non-extension files.
- `RenameWheelFilename`: swaps the abi field in place; requires a non-empty abi.
- `PromoteWheelToABI3`: rewrites cp312-cp312-... -> cp32-abi3-...; rejects PyPy wheels.
- `RenderWHEELMarker`: emits `Wheel-Version: 1.0` + `Generator:` + `Root-Is-Purelib: false` + `Tag: cp32-abi3-<platform>`; defaults Generator to `mochi-pkg` and platform to `any` when called with empty strings.

## Files

- `package3/python/abi3/doc.go` — package overview (abi3 vs interpreter-specific tags, the auditwheel parity goal, planned sub-phases).
- `package3/python/abi3/tag.go` — `WheelTag`, `ParseWheelTag`, `String`, `IsABI3`, `ToABI3`, `SplitWheelFilename`.
- `package3/python/abi3/manylinux.go` — `ManylinuxProfile`, `KnownProfiles` (11 profiles), `LookupProfile`, `ProfileTags`.
- `package3/python/abi3/audit.go` — `LinkedLib`, `SymbolReader` interface, `AuditOptions`, `AuditReport`, `AuditExtension`, `AuditWheel`, `IsExtensionFilename`.
- `package3/python/abi3/slim.go` — `RenameWheelFilename`, `PromoteWheelToABI3`, `RenderWHEELMarker`.
- `package3/python/abi3/tag_test.go` — wheel tag parsing + filename split + abi3 promotion (12 cases).
- `package3/python/abi3/manylinux_test.go` — profile invariants + allow-list coverage across Linux / macOS / Windows / musl (6 cases).
- `package3/python/abi3/audit_test.go` — `AuditExtension` + `AuditWheel` happy + error paths (8 cases).
- `package3/python/abi3/slim_test.go` — filename + WHEEL marker rendering (6 cases).
- `package3/python/abi3/phase13_test.go` — Phase 13 umbrella sentinel.

## Sub-phase decomposition

Phase 13 ships the pure policy layer. Live binary inspection, abi3 discipline checking, and the user-facing CLI verb stay out of the umbrella gate so the suite remains offline + cross-platform.

| Sub-phase | Title | Status | Notes |
|-----------|-------|--------|-------|
| 13 | Tag + profile + audit policy layer (mockable `SymbolReader`) | LANDED (pending merge) | This PR. |
| 13.1 | Live ELF / Mach-O / PE reader behind `SymbolReader` | NOT STARTED | Real binary inspection. Plan: ELF via `debug/elf`, Mach-O via `debug/macho`, PE via `debug/pe`. |
| 13.2 | Py_LIMITED_API discipline check before promoting to abi3 | NOT STARTED | Scans the C extension's symbol references and refuses promotion when any references fall outside the limited API. |
| 13.3 | `mochi pkg audit --wheel <path>` + `mochi pkg slim --abi3` CLI verbs | NOT STARTED | Wires the auditor + slimmer behind the unified `mochi pkg` CLI. |

## Fixtures

Phase 13 is policy-only; the fixture corpus is not exercised. Sub-phase 13.1 will audit the corpus's compiled extensions (`numpy`, `pandas`, `scipy`, `pillow`) against `manylinux_2_28_x86_64` + `macosx_11_0_arm64` + `win_amd64` and assert each violation count matches a golden number per package.

## Skip count

N/A. Phase 13 has no `SkipReport` surface; profile lookup miss / reader error / disallowed external lib all surface as audit failures (returned `AuditReport.Violations`, not skip reports).

## Cross-references

- [MEP-71 spec §8 "abi3 slimming + auditwheel"](/docs/mep/mep-0071) for the normative profile + abi3 promotion rules.
- [Phase 10](./phase-10-python-package-emit) for the wheel emit Phase 13 audits.
- [Phase 17](./phase-17-free-threaded) for the `cp3XYt` abi tag that future free-threaded wheels will use alongside abi3.
- [Phase 18](./phase-18-abi2026) for the abi2026 transition that Phase 13's tag-rewrite helpers will be reused for.
- [PEP 425 (compatibility tags)](https://peps.python.org/pep-0425/), [PEP 600 (manylinux)](https://peps.python.org/pep-0600/), [PEP 656 (musllinux)](https://peps.python.org/pep-0656/), and the [auditwheel docs](https://github.com/pypa/auditwheel) for the upstream policy spec.
