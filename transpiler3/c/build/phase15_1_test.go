package build

import (
	"runtime"
	"testing"
)

// TestPhase15MagicSet is the MEP-45 Phase 15.1 gate for goal-directed Datalog
// evaluation via the magic-set transform (Bancilhon et al., PODS 1986).
//
// Magic-set is applied automatically when the query has bound (constant)
// arguments and at least one rule derives the query predicate. The transform
// adds a magic_REL predicate seeded from the query constants, guards each
// rule for REL with magic_REL(bound-vars), and generates propagation rules
// so the magic set grows through left-linear recursion. Right-linear and
// non-recursive rules are handled as the trivial-propagation case.
//
// Fixtures cover: non-recursive rules with neq (ms_sibling), right-linear
// transitive closure (ms_transitive), left-linear reachability where magic
// propagates to new values (ms_left_linear), left-linear two-hop closure
// (ms_two_step), and a DAG ancestor query (ms_ancestor_dag).
//
// All fixtures live under tests/transpiler3/c/fixtures/magic_datalog/.
func TestPhase15MagicSet(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "magic_datalog")
}
