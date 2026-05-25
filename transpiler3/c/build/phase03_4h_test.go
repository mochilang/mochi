package build

import (
	"runtime"
	"testing"
)

// TestPhase3IndexAssign gates Phase 3.4h: xs[i] = val for list<T>
// and m[k] = val for map<K,V>. Exercises all four list element types
// and two map instantiations, plus a loop-driven fill pattern.
func TestPhase3IndexAssign(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("C toolchain not available in Windows CI")
	}
	runFixtureSuite(t, "index_assign")
}
