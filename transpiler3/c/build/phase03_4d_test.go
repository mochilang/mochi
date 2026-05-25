package build

import (
	"runtime"
	"testing"
)

// TestPhase3CollectionEquality is the MEP-45 Phase 3.4d gate for
// collection equality operators: `list<T> == list<T>`, `list<T> != list<T>`,
// `map<K,V> == map<K,V>`, and `map<K,V> != map<K,V>`.
// It walks every fixture under tests/transpiler3/c/fixtures/collection_equality
// and runs the same end-to-end pipeline as the other Phase 3 gates.
//
// The fixtures cover: list equality for all four scalar element types,
// empty-list equality, length-mismatch early exit, list equality in if-
// conditions and function parameters, map equality for all eight scalar
// K/V combos, empty-map equality, key-mismatch detection, and map equality
// as if-conditions and function arguments.
func TestPhase3CollectionEquality(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "collection_equality")
}
