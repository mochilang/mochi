package build

import (
	"runtime"
	"testing"
)

// TestPhase3TypedEmptyLiteral is the MEP-45 Phase 3.4c gate for
// typed-empty collection literals: `let xs: list<T> = []` and
// `let m: map<K,V> = {}` where the declared type annotation supplies
// the element / key / value type instead of inference from elements.
// It walks every fixture under tests/transpiler3/c/fixtures/typed_empty_literal
// and runs the same end-to-end pipeline as the other Phase 3 gates.
//
// The fixtures probe: len=0 for all four scalar list element types,
// var + append + for-in accumulation, for-in over empty list, pass /
// return across function boundaries, conditional fill, index after
// append, and empty map<K,V> with len / in-check / pass / return.
func TestPhase3TypedEmptyLiteral(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "typed_empty_literal")
}
