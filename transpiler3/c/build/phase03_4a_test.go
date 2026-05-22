package build

import (
	"runtime"
	"testing"
)

// TestPhase3MonoListRecord is the MEP-45 Phase 3.4a gate for
// list<R>: lists whose element type is a user-defined record. It
// walks every fixture under tests/transpiler3/c/fixtures/list_of_record
// and runs the same end-to-end pipeline as the scalar list gate,
// confirming that the per-record `mochi_list_<R>_*` helpers emitted
// into the TU prologue compose correctly with literal construction,
// indexed access, len, append, for-in iteration, var reassignment,
// and the function call boundary (both pass and return).
func TestPhase3MonoListRecord(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "list_of_record")
}
