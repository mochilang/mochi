// Package semver implements node-semver-compatible range parsing and SemVer 2.0
// version ordering. Lands in MEP-72 Phase 0.
//
// The package does NOT use golang.org/x/mod/semver because that implements the
// Go-module dialect, which lacks node-semver's union ranges, caret/tilde
// shorthand, and `x`/`X`/`*` wildcards.
//
// See website/docs/implementation/0072/phase-00-skeleton.md.
package semver
