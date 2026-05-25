package build

import (
	"runtime"
	"testing"
)

// TestPhase8QueryDSL is the MEP-45 Phase 8.0 gate for the filter+map
// query DSL. It walks every fixture under
// tests/transpiler3/c/fixtures/query and runs the same end-to-end
// pipeline as all other phase gates.
//
// Phase 8.0 fixtures:
//   - Filter-only query (where clause, no transform)
//   - Map-only query (select transform, no filter)
//   - Filter+map combined
//   - String source with contains filter
//   - String source with len transform
//   - Bool source with identity filter
//   - Nested queries (query over a query result)
//   - Queries inside named functions
//
// Phase 8.1 fixtures (order by, skip, take):
//   - order by on int list
//   - order by on string list
//   - take N (first N elements)
//   - skip N (drop first N elements)
//   - skip + take (page window)
//   - order by + take (top N)
//   - order by + skip + take (sorted page)
//   - where + order by (filter then sort)
func TestPhase8QueryDSL(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "query")
}
