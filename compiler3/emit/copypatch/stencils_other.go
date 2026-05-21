//go:build !amd64 && !arm64

package copypatch

import "mochi/compiler3/ir"

// archStencils on hosts that are neither amd64 nor arm64 returns an
// empty table. Phase 1's scope (MEP-42 §9) is the five must-have
// targets: x86_64 Linux (Phase 1.0), aarch64 Linux (Phase 1.1),
// x86_64 macOS (Phase 1.2), aarch64 macOS (Phase 1.3), and wasm32
// (Phase 1.4). Hosts outside this matrix get the empty fallback;
// callers must consult `Supported()` before attempting to compile a
// Function on these platforms.
func archStencils() map[ir.OpCode]Stencil {
	return nil
}

// archSupportsBranches reports whether the emitter can lower inter-
// block terminators on the host GOARCH. Always false on hosts
// outside the Phase 1 target matrix because no stencil table exists.
func archSupportsBranches() bool {
	return false
}
