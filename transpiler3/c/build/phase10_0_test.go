package build

import (
	"runtime"
	"testing"
)

// TestPhase10FFIDirect is the MEP-45 Phase 10.0 gate.
//
// Phase 10.0 adds C-direct FFI to the transpiler3/c pipeline:
//   - `extern fun` declarations lower to extern C function declarations
//     in the generated C prologue.
//   - The driver compiles any .c file co-located with the source
//     (same stem, .mochi -> .c) alongside the generated main.c.
//   - Calls to extern functions lower to CallExpr nodes that emit as
//     name(args...) in C.
//
// This test runs the ffi fixture suite (add_extern, str_len_extern) and
// asserts each binary's stdout matches its expect.txt.
func TestPhase10FFIDirect(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 10 FFI gate skipped on Windows; Phase 11 wires Windows CI")
	}
	runFixtureSuite(t, "ffi")
}
