package build

import (
	"runtime"
	"testing"
)

// TestPhase15StratifiedNeg is the MEP-45 Phase 15.2 gate for stratified
// negation in Datalog evaluation.
//
// Stratified negation evaluates the Datalog program in layers (strata).
// Relations used in "not pred(...)" conditions must be in a lower stratum
// than the rule head, ensuring they are fully evaluated before the negation
// check runs. The keyword "not" is new in Phase 15.2; the lower pass computes
// strata via iterative fixpoint (computeDatalogStrata) and emits one
// do-while loop per stratum.
//
// Fixtures cover:
//   neg_orphan:      childless(X) :- person(X), not has_child(X)
//                    (two strata: has_child at 0, childless at 1)
//   neg_complement:  isolated(X) :- node(X), not reachable_from_a(X)
//                    (transitive closure at 0, complement at 1)
//   neg_indirect:    trustworthy(X) :- person(X), not has_blocked_friend(X)
//                    (derived relation at 0, negated complement at 1)
//
// All fixtures live under tests/transpiler3/c/fixtures/datalog/.
func TestPhase15StratifiedNeg(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "datalog")
}
