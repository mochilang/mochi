package build

import (
	"runtime"
	"testing"
)

// TestPhase5Closures is the MEP-45 Phase 5 gate for non-capturing closure
// support. It walks every fixture under tests/transpiler3/c/fixtures/closures
// and runs the same end-to-end pipeline as all other phase gates.
//
// The fixtures probe:
//   - Simple one-arg closure assigned to a let binding and called
//   - Two-argument closure
//   - Closure returning bool
//   - Closure returning string
//   - Closure over float parameters
//   - Closure defined inside a named function (block-local lifting)
//   - Multiple closures of the same and different fun types in one program
//   - Block-body closure (braces + return statement)
//   - Higher-order function accepting a fun-typed parameter
func TestPhase5Closures(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "closures")
}
