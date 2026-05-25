package build

import (
	"runtime"
	"testing"
)

// TestPhase3ListOfMap is the MEP-45 Phase 3.4f gate for
// list<map<K,V>>: outer lists whose element is a map with key type
// int or string and scalar value type (int/float/bool/string). It
// walks every fixture under tests/transpiler3/c/fixtures/list_of_map
// and runs the same end-to-end pipeline as the other nested-collection
// gates.
//
// The fixtures probe outer literal construction, double indexing,
// len at the outer level, outer append, nested for-in (induction var
// is a full map<K,V>), the pass/return function boundary, and the
// K x V scalar-primitive matrix.
func TestPhase3ListOfMap(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "list_of_map")
}
