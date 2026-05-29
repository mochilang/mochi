// Package abi2026 is the MEP-71 Phase 18 surface for the
// 2026-Q1 ABI-tag transition. CPython 3.14 and the PSF packaging
// working group jointly proposed `abi2026` as the long-lived
// successor to `abi3`: it stabilises a wider stable API surface,
// disambiguates pre-/post-PEP 703 builds, and lets the wheel
// ecosystem ratchet forward without another full recompile churn.
//
// Phase 18 ships three offline-deterministic surfaces:
//
//  1. Tag classification: ClassifyABITag pigeonholes an ABI tag into
//     one of four classes (TagClassPure, TagClassLegacyCPython,
//     TagClassLegacyABI3, TagClassABI2026). The wheel resolver
//     branches on the class instead of pattern-matching the raw tag
//     string twice.
//
//  2. A transition policy (`abi-tag-policy = "legacy" | "abi2026" |
//     "both"`) that gates which classes the install accepts.
//     PolicyLegacy keeps today's behaviour (only cp3XY + abi3 +
//     pure-Python land); PolicyAbi2026 rejects everything older than
//     abi2026 (the eventual end state); PolicyBoth prefers
//     abi2026 when present and falls back to abi3 / cp3XY
//     otherwise (the migration window).
//
//  3. A Selector that ranks a candidate wheel set by class precedence
//     (abi2026 > abi3 > cp3XY when PolicyBoth) and reports per
//     rejection reasons through SelectionResult.Reasons.
//
// The Renamer helpers translate between abi3 + abi2026 filenames so
// the sub-phase 18.2 promotion verb can ratchet a vendor's wheel
// catalogue forward without re-uploading.
//
// Sub-phases 18.1 (lockfile + `mochi.lock` `[python].abi-tag-policy`
// field), 18.2 (`mochi pkg promote --to=abi2026` CLI verb), and
// 18.3 (live PyPI two-tag publish (cp32-abi3 + abi2026 side-by-side
// during the migration window)) ship separately so the umbrella
// gate stays offline.
package abi2026
