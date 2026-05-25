package build

import (
	"runtime"
	"testing"
)

// TestPhase2TypeCasts is the MEP-45 Phase 2.5 gate for the int() cast
// builtin and the min()/max() list builtins. It walks every fixture under
// tests/transpiler3/c/fixtures/type_cast/ and runs the same end-to-end
// pipeline as all other phase gates.
//
// The fixtures probe:
//   - int(x): positive float truncation, negative float truncation
//   - int(x): large value truncation (100.999 -> 100)
//   - min(xs) on int list: returns minimum element
//   - max(xs) on int list: returns maximum element
//   - min(xs) on float list: returns minimum float
//   - max(xs) on string list: returns lexicographically largest string
//   - combined: min + max + int() in a single program
func TestPhase2TypeCasts(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "type_cast")
}
