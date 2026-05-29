// Package lockfile is the MEP-71 Phase 9 implementation of the
// `[[python-package]]` table that lives inside the workspace mochi.lock.
//
// Each entry records:
//
//   - The PyPI distribution name (PEP 503-normalised) plus the user's alias.
//   - The resolution source: registry / index / git / path.
//   - The exact resolved version (for registry / index) or VCS revision
//     (for git) or path (for path).
//   - The Phase 6 shim SHA-256 (`wrapper-sha256`) so `mochi pkg lock --check`
//     can detect surface drift between the recorded lock and the current
//     wrapper output.
//   - A sorted capability list extracted from the wrapper surface (async,
//     dataclass, protocol, callable, awaitable). Phase 15 verifies these
//     against the attestation set.
//
// The package provides both directions of the round-trip plus a Diff /
// Check helper for the `mochi pkg lock --check` mode.
//
// See MEP-71 §5 "Lockfile entry" for the normative schema. The TOML output
// is hand-rendered (no third-party encoder) so the bytes are stable for
// hashing.
package lockfile
