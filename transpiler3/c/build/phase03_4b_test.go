package build

import (
	"runtime"
	"testing"
)

// TestPhase3NestedLists is the MEP-45 Phase 3.4b gate for
// list<list<T>>: outer lists whose element is itself a list of one
// of the four scalar primitives (int / float / bool / string). It
// walks every fixture under tests/transpiler3/c/fixtures/list_of_list
// and runs the same end-to-end pipeline as the scalar list gate.
//
// The fixtures probe outer / inner literal construction, double
// indexing, len at both levels, outer append, nested for-in, jagged
// rows, the pass / return function boundary, and the scalar-primitive
// inner-type matrix.
func TestPhase3NestedLists(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "list_of_list")
}
