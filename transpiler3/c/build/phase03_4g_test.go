package build

import (
	"runtime"
	"testing"
)

// TestPhase3ListSlice is the MEP-45 Phase 3.4g gate for list slice syntax
// xs[start:end]. It walks every fixture under
// tests/transpiler3/c/fixtures/list_slice.
func TestPhase3ListSlice(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "list_slice")
}
