// Package resolve is the MEP-43 Phase 2 binding resolver. Given a Go
// import path, it loads the package via golang.org/x/tools/go/packages
// and produces a typed Mochi binding for every exported symbol. The
// resolver consumes the Phase 1 type bridge (compiler3/ffi/typebridge,
// MEP-44) for all type mapping; it adds no per-package code.
//
// The result is a *PackageBinding tree that the type checker consumes
// directly; no Mochi code looks at *types.Package after this point.
//
// Results are cached on disk under $XDG_CACHE_HOME/mochi/bindings/.
// Cache keys include the user's go.sum hash and the Mochi binary
// version, so dependency upgrades and Mochi upgrades invalidate
// cleanly. The gob wire format carries a leading version byte
// (mirroring MEP-44 §7); a Mochi upgrade that changes the bridge
// format simply bumps the version, causing the loader to refuse
// stale entries.
package resolve
