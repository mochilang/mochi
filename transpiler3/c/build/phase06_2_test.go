package build

import (
	"runtime"
	"testing"
)

// TestPhase6StrConvert is the MEP-45 Phase 6.2 gate for the str(x)
// type-to-string conversion builtin. It walks every fixture under
// tests/transpiler3/c/fixtures/str_convert/ and runs the same
// end-to-end pipeline as all other phase gates.
//
// The fixtures probe:
//   - str(int): positive, negative, zero integers
//   - str(float): shortest-decimal representation (matching Go %g)
//   - str(bool): "true" and "false"
//   - str(string): identity pass-through
//   - str() inside string concatenation
//   - str() result used in a condition
//   - str() inside named functions
//   - str() on list element values in a for loop
func TestPhase6StrConvert(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "str_convert")
}
