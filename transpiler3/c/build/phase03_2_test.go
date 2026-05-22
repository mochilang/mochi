package build

import (
	"runtime"
	"testing"
)

// TestPhase3Maps is the MEP-45 Phase 3.2 gate for map<K,V>. It walks
// every fixture directory under tests/transpiler3/c/fixtures/maps,
// runs the end-to-end pipeline (parse, type-check, lower, emit, cc,
// link, exec), and diffs the binary's stdout against the fixture's
// expect.txt.
//
// The fixtures cover the eight Phase 3.2 (K,V) instantiations
// (K in int / string, V in int / float / bool / string), literal
// construction, indexed lookup, len, `in` membership test, keys /
// values, key-sorted for-in iteration, maps passed to and returned
// from user functions, reassignment through a var binding, nested
// iteration, and a composition that folds values via the keys list.
func TestPhase3Maps(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "maps")
}
