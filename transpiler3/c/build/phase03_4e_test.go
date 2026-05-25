package build

import (
	"runtime"
	"testing"
)

// TestPhase3MapOfList is the MEP-45 Phase 3.4e gate for map<K,list<V>>:
// maps whose value type is a list of one of the four scalar primitives
// (int / float / bool / string), with K ∈ {int, string}. It walks every
// fixture under tests/transpiler3/c/fixtures/map_of_list and runs the
// same end-to-end pipeline as the other collection gates.
//
// The fixtures probe: literal construction, index (m[k]), len at map and
// list level, has/in check, values() returning list<list<V>>, for-k-in-m
// key iteration, pass/return across function boundaries, and the full
// K x V scalar matrix (8 combinations).
func TestPhase3MapOfList(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "map_of_list")
}
