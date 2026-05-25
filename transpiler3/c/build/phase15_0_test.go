package build

import (
	"runtime"
	"testing"
)

// TestPhase15Datalog is the MEP-45 Phase 15.0 gate for Datalog semi-naive
// evaluation in the AOT C backend. It exercises fact tables, single-rule
// and multi-rule derivation, transitive closure (ancestor / reachability),
// multiple queries in one program, constant-filtered queries, empty result
// sets, and inequality constraints in rule bodies.
//
// All fixtures live under tests/transpiler3/c/fixtures/datalog/.
func TestPhase15Datalog(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "datalog")
}
