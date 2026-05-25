package build

import (
	"runtime"
	"testing"
)

// TestPhase2MathBuiltins is the MEP-45 Phase 2.6 gate for list containment
// (`val in list<T>`), list sum (`sum(xs)`), abs, floor, and ceil. It walks
// every fixture under tests/transpiler3/c/fixtures/math_builtins/ and runs
// the same end-to-end pipeline as all other phase gates.
//
// The fixtures probe:
//   - list_contains_int:  3 in [1,2,3,4,5] == true, 6 in xs == false
//   - list_contains_str:  string membership with strcmp
//   - list_contains_bool: bool membership (true and false both found)
//   - sum_int:            sum([1,2,3,4,5]) == 15
//   - sum_float:          sum([1.1,2.2,3.3]) with floating-point accumulation
//   - abs_values:         abs(-5)==5, abs(5)==5, abs(-3.14) correct float
//   - floor_ceil:         floor(3.9)==3, ceil(3.1)==4
//   - math_combined:      all four features together
func TestPhase2MathBuiltins(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "math_builtins")
}
