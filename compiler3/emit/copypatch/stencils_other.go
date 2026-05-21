//go:build !amd64

package copypatch

import "mochi/compiler3/ir"

// archStencils on non-amd64 returns an empty table. The Phase 1
// scope (MEP-42 §9 row 1) is x86_64 Linux only; aarch64 stencils
// land in Phase 1.5. Callers must consult NewEmitter().Supported()
// before attempting to compile a Function on these platforms.
func archStencils() map[ir.OpCode]Stencil {
	return nil
}
