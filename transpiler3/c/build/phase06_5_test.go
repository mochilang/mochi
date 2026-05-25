package build

import (
	"runtime"
	"testing"
)

// TestPhase6FileIO is the MEP-45 Phase 6.5 gate for file I/O builtins.
// It walks every fixture under tests/transpiler3/c/fixtures/file_io and
// runs the same end-to-end pipeline as all other phase gates.
//
// The fixtures probe:
//   - writeFile + readFile round-trip (basic)
//   - writeFile with embedded newlines, readFile round-trip
//   - appendFile: write then append, read back
//   - lines(): split file into list<string>, trailing-newline stripped
//   - lines() on empty file returns empty list (len == 0)
//   - writeFile overwrites existing content
//   - readFile of a longer string, print len
//   - lines() with no trailing newline in file
func TestPhase6FileIO(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("C toolchain not available in Windows CI")
	}
	runFixtureSuite(t, "file_io")
}
