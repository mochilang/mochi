package build

import (
	"runtime"
	"testing"
)

// TestPhase5CapturingClosures is the MEP-45 Phase 5.1 gate for capturing
// closure support. It walks every fixture under
// tests/transpiler3/c/fixtures/capturing_closures.
//
// The fixtures probe:
//   - Closure capturing a single int variable
//   - Closure capturing a float variable
//   - Closure capturing a string variable
//   - Closure capturing multiple variables of mixed types
//   - Closure capturing a bool variable
//   - Closure defined inside a named function capturing a parameter
//   - Multiple calls to the same capturing closure
//   - Arithmetic with multiple captured ints
func TestPhase5CapturingClosures(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "capturing_closures")
}
