//go:build arm64

package copypatch

import (
	"testing"

	"mochi/compiler3/ir"
)

// TestStencilsARM64Coverage pins the exact opcode set the Phase 1.1
// arm64 placeholder table covers: OpConst, OpAddI64, and OpInvalid
// (the ret stencil). Phase 2.1 widens this set to match amd64's
// non-allocating arithmetic + comparison surface; this test fails
// loudly when that widening lands so the umbrella matrix in MEP-42
// §10 stays in sync with the code.
func TestStencilsARM64Coverage(t *testing.T) {
	tab := archStencils()
	want := map[ir.OpCode]bool{
		ir.OpConst:   true,
		ir.OpAddI64:  true,
		ir.OpInvalid: true,
	}
	for op := range want {
		if _, ok := tab[op]; !ok {
			t.Errorf("missing stencil for %s", op)
		}
	}
	for op := range tab {
		if !want[op] {
			t.Errorf("unexpected stencil for %s (phase 1.1 placeholder set is 3 opcodes)", op)
		}
	}
}

// TestArchSupportsBranchesARM64 pins the Phase 1.1 default. The
// emitter gates TermJump and TermBranch emission on this; flipping it
// to true without porting the rel26 / cbz encodings would let the
// emitter produce invalid arm64 bytes. Phase 2.1 flips this to true
// and removes this test (or rewrites it to assert true).
func TestArchSupportsBranchesARM64(t *testing.T) {
	if archSupportsBranches() {
		t.Fatal("archSupportsBranches() = true on arm64 in Phase 1.1; rel26 / cbz encodings land in Phase 2.1")
	}
}
