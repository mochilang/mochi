package build

import (
	"runtime"
	"testing"
)

// TestPhase4Maranget gates Phase 4.1: the Maranget decision-tree pass in
// lower/match.go. The pass canonicalizes MatchStmt arms (sorts by ascending
// tag, validates no duplicates) before emit. The Phase 4.0 sum-type fixtures
// are the correctness corpus; they must all produce identical output after
// the pass runs.
func TestPhase4Maranget(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("C toolchain not available in Windows CI")
	}
	runFixtureSuite(t, "sum_types")
}
