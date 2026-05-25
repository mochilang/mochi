package build

import (
	"runtime"
	"testing"
)

// TestPhase6StringExtra is the MEP-45 Phase 6.3 gate for string case-
// conversion and split/join support. It walks every fixture under
// tests/transpiler3/c/fixtures/string_extra.
//
// The fixtures probe:
//   - upper("hello world") -> "HELLO WORLD"
//   - lower("HELLO WORLD") -> "hello world"
//   - split("a,b,c", ",") -> ["a", "b", "c"] (iterated with for-in)
//   - split("hello world foo", " ") -> len 3
//   - join(["hello", "world", "foo"], ", ") -> "hello, world, foo"
//   - join(["only"], "-") -> "only" (single-element edge case)
//   - upper + lower together
//   - split then join (round-trip)
func TestPhase6StringExtra(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "string_extra")
}
