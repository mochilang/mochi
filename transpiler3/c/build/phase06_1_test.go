package build

import (
	"runtime"
	"testing"
)

// TestPhase6StringMethods is the MEP-45 Phase 6.1 gate for string
// indexing, contains, substring, and reverse. It walks every fixture
// under tests/transpiler3/c/fixtures/string_methods and runs the same
// end-to-end pipeline as all other phase gates.
//
// The fixtures probe:
//   - String indexing s[i] (literal index)
//   - String indexing inside a named function
//   - s.contains(sub) -- true result
//   - s.contains(sub) -- false result
//   - substring(s, start, end)
//   - reverse(s)
//   - String index result used in concatenation
//   - Combination of all string methods
func TestPhase6StringMethods(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "string_methods")
}
