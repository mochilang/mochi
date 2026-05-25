package build

import (
	"runtime"
	"testing"
)

// TestPhase6StringOps is the MEP-45 Phase 6.0 gate for string concatenation
// and len(s) on strings. It walks every fixture under
// tests/transpiler3/c/fixtures/string_ops and runs the same end-to-end
// pipeline as all other phase gates.
//
// The fixtures probe:
//   - String concatenation of two string literals
//   - String concatenation through variables
//   - Chained concatenation (a + b + c)
//   - len() on a string literal
//   - len() on a string variable
//   - String concatenation inside a named function
//   - Concatenation followed by len
//   - Concatenation result used in equality comparison
func TestPhase6StringOps(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "string_ops")
}
