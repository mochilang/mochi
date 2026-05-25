package build

import (
	"runtime"
	"testing"
)

// TestPhase5FreeFunctionShim is the MEP-45 Phase 5.2 gate for free-function
// closure shim support. It walks every fixture under
// tests/transpiler3/c/fixtures/free_function_shim.
//
// The fixtures probe:
//   - Named int-returning function passed as fun(int): int arg
//   - Named float-returning function passed as fun arg
//   - Named bool-returning function passed as fun arg
//   - Named string-returning function passed as fun arg
//   - Named two-parameter function passed as fun(int,int): int arg
//   - Named function assigned directly to a let binding
//   - Same named function referenced multiple times (shim dedup)
//   - Two different named functions composed via a higher-order function
func TestPhase5FreeFunctionShim(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "free_function_shim")
}
