// Package emit renders an aotir.Program as ISO C23 source.
//
// Design contract: MEP-45 §1 "Pipeline and IR" and §2
// "Lowering rules". See website/docs/mep/mep-0045.md.
//
// Public entry point (introduced incrementally by phase):
//
//	func Emit(prog *aotir.Program) (string, error)
//
// Output target is portable ISO C23, deliberately staying
// inside the subset that Clang 18+, GCC 14+, and MSVC 19.40+
// all accept without warning under -Wall -Wextra. Specifically:
//
//   - No GNU extensions (no nested functions, no statement
//     expressions, no labels as values, no VLAs).
//   - C23 features that ship across all three: typeof,
//     constexpr, [[nodiscard]], unicode escapes, digit
//     separators.
//   - All non-ASCII strings are emitted as \uXXXX or \UXXXXXXXX.
//   - Identifier mangling: every Mochi identifier is escaped to
//     [A-Za-z0-9_] via a reversible scheme so collisions and
//     keyword shadows are impossible (Phase 2).
//
// Reproducibility (MEP-45 Phase 17): output is deterministic.
// Functions and globals are emitted in sorted aotir-identifier
// order. Source paths are stripped via -ffile-prefix-map at
// build time, not here.
//
// Phase 0 ships the package skeleton. Each later phase adds
// emission for the IR shapes its gate requires.
package emit
