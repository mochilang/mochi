// Package emit produces the Mochi-side shim file the bridge synthesises for
// each consumed PyPI package. The shim is a regular Mochi source file
// (`<package>_shim.mochi`) that the user's program treats as an ordinary
// Mochi module through the Phase 7 `import python "<spec>" as <alias>`
// surface: the importer rewrites the import to point at the shim, which in
// turn declares the user-visible Mochi surface via `extern python fun`
// (functions), `type X = { ... }` (records / TypedDict / frozen
// @dataclass), `extern python type` (Protocols and opaque records), and
// `extern python var` (re-exported module-level constants).
//
// The emit step is the Mochi-side counterpart to Phase 5 (Python wrapper
// synthesiser): both consume the same typed Wrapper.Items list, but Phase 5
// renders Python and Phase 6 renders Mochi. The two artefacts are kept in
// 1:1 correspondence: every shim declaration has a wrapper-side counterpart
// the user's Python interpreter will resolve through the sidecar import.
//
// See MEP-71 §3 "Closed type translation table" for the type forms the
// emit produces, and §6 "Build orchestration" for the import-rewrite that
// loads the shim at build time.
package emit
