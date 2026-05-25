package build

import (
	"runtime"
	"testing"
)

// TestPhase8QueryJoins is the MEP-45 Phase 8.2 gate for join operators in
// the query DSL. It walks every fixture under
// tests/transpiler3/c/fixtures/query_join and runs the same end-to-end
// pipeline as all other phase gates.
//
// Phase 8.2 fixtures:
//   - Inner join on int lists
//   - Inner join with select expression using both vars
//   - Cross join (additional from clause) on int lists
//   - Cross join with mixed types (int x string)
//   - Inner join combined with where filter
//   - Left join (all left rows, matched or not)
//   - Left join with more unmatched rows
//   - Cross join with where filter
func TestPhase8QueryJoins(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "query_join")
}
