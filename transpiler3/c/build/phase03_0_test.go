package build

import (
	"runtime"
	"testing"
)

// TestPhase3Records is the MEP-45 Phase 3.0 gate for record types.
// It walks every fixture directory under
// tests/transpiler3/c/fixtures/records, runs the end-to-end pipeline
// (parse, type-check, lower, emit, cc, link, exec), and diffs the
// binary's stdout against the fixture's expect.txt.
//
// The fixtures cover: scalar-field records, mixed-type records,
// field arithmetic, record-typed let/var bindings, source-order
// field reordering at the literal, records passed to and returned
// from functions, records used as if/while conditions, and
// structural equality / inequality (a == b / a != b) across both
// numeric and string fields.
func TestPhase3Records(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "records")
}

// TestPhase3Strings is the MEP-45 Phase 3.0 gate for direct string
// equality. Records carry string fields whose pairwise compare uses
// strcmp inside the per-record helper; this suite covers the same
// strcmp lowering invoked directly from `==` / `!=` between two
// string operands (BinEqStr / BinNeStr in the IR).
func TestPhase3Strings(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "strings")
}
