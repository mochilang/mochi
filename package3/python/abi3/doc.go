// Package abi3 implements MEP-71 Phase 13: abi3 wheel slimming + the
// auditwheel-equivalent platform tag validator.
//
// Two responsibilities, both pure policy with the IO mockable:
//
//   - Wheel tag handling per PEP 425 / PEP 600 / PEP 656. ParseWheelTag
//     splits a wheel filename's `{python}-{abi}-{platform}` triple, and
//     ToABI3 promotes a single-version tag (`cp312-cp312-...`) to the
//     limited-API form (`cp32-abi3-...`) when the extension only uses
//     Py_LIMITED_API symbols. The limited-API form ships once and
//     resolves on every CPython ≥ 3.2, which is the slimming win.
//
//   - Platform tag validation, the auditwheel job. KnownProfiles maps
//     every supported tag (manylinux_2_17 / 2_28 / 2_34, musllinux_1_2,
//     macosx_*, win_*) to its allowed-libs list (the libc + libm
//     baseline expected on the target platform). AuditExtension reads
//     the linked libs of a .so / .dylib / .pyd via a SymbolReader
//     interface, classifies each as in-policy or external, and reports
//     violations. The SymbolReader interface is intentionally minimal
//     (just `Read(path) ([]LinkedLib, error)`) so the bridge can swap
//     between the real ELF / Mach-O / PE walker in production and a
//     mock in the gate tests.
//
// Layout:
//
//   - tag.go: WheelTag + ParseWheelTag + ToABI3 + filename helpers.
//   - manylinux.go: ManylinuxProfile + KnownProfiles + LookupProfile.
//   - audit.go: LinkedLib + SymbolReader + AuditReport + AuditExtension.
//   - slim.go: RenameWheelFilename + RenderWHEELMarker for the
//     wheel-side rewrite that downstream packers apply.
//
// Live ELF / Mach-O / PE reading is sub-phase 13.1, not covered here.
// abi3 promotion that walks the C extension's symbol references to
// confirm Py_LIMITED_API discipline is sub-phase 13.2; this phase ships
// the policy layer that the discipline check will feed into.
package abi3
