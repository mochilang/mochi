package build

import (
	"runtime"
	"testing"
)

// TestPhase4SumTypes is the MEP-45 Phase 4 gate for sum types (union
// declarations) and Maranget-style match statements/expressions. It walks
// every fixture under tests/transpiler3/c/fixtures/sum_types and runs the
// same end-to-end pipeline as all other phase gates.
//
// The fixtures probe:
//   - Basic two-variant union, match-as-expression
//   - All four scalar field types: int, float, bool, string
//   - Unit variants (no fields)
//   - Functions that take a union parameter or return a union value
//   - Match-as-statement (each arm is a print call)
//   - Wildcard (_) arms
//   - Multiple match statements on the same union type in the same scope
//   - Two union types co-existing in one program
//   - Match expression producing string, bool, int, float results
func TestPhase4SumTypes(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "sum_types")
}
