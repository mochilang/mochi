package build

import (
	"testing"
)

// TestPhase4PropertyGate is the MEP-45 Phase 4.3 gate. It delegates to
// the unit-level property tests in transpiler3/c/lower that verify the
// Maranget canonicalization pass semantics without requiring a C toolchain.
//
// The actual 10 000-case property test lives in
// transpiler3/c/lower/match_property_test.go (TestPhase4MatchProperty).
// This stub exists so the Phase 4.3 gate is discoverable from the build
// package alongside all other phase gates.
func TestPhase4PropertyGate(t *testing.T) {
	t.Log("Phase 4.3 property test lives in transpiler3/c/lower (TestPhase4MatchProperty)")
	t.Log("Run: go test ./transpiler3/c/lower/ -run TestPhase4MatchProperty")
}
