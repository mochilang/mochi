package build

import (
	"runtime"
	"testing"
)

// TestPhase3Lists is the MEP-45 Phase 3.1 gate for list<T>. It walks
// every fixture directory under tests/transpiler3/c/fixtures/lists,
// runs the end-to-end pipeline (parse, type-check, lower, emit, cc,
// link, exec), and diffs the binary's stdout against the fixture's
// expect.txt.
//
// The fixtures cover the scalar element types (int / float / bool /
// string) across every list operation Phase 3.1 lands: literal
// construction, index, len, append, and for-in iteration; plus list
// values passed to and returned from functions, reassigned through a
// var binding, and append composing into a fold.
func TestPhase3Lists(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "lists")
}
