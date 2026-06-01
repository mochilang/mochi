package generics_test

import (
	"testing"

	"mochi/package3/dotnet/generics"
)

// TestPhase12Monomorphise is the MEP-68 Phase 12 gate for generic monomorphisation.
//
// Three input GenericCallSite values (Parse<int64>, Parse<string>, Parse<float64>)
// must produce 3 concrete ConcreteMethodSpec outputs and 0 skips.
func TestPhase12Monomorphise(t *testing.T) {
	sites := []generics.GenericCallSite{
		{Method: "Parse", TypeArgs: []string{"int64"}},
		{Method: "Parse", TypeArgs: []string{"string"}},
		{Method: "Parse", TypeArgs: []string{"float64"}},
	}

	specs, err := generics.MonomorphiseAll(sites)
	if err != nil {
		t.Fatalf("MonomorphiseAll: %v", err)
	}
	if len(specs) != 3 {
		t.Fatalf("expected 3 specs, got %d", len(specs))
	}

	skips := 0
	for _, s := range specs {
		if s.IsSkipped {
			skips++
			t.Errorf("unexpected skip for %v: %s", s.TypeArgs, s.SkipReason)
		}
	}
	if skips != 0 {
		t.Errorf("expected 0 skips, got %d", skips)
	}

	// Verify mangled names.
	wantNames := map[string]bool{
		"Parse_Int64":   false,
		"Parse_String":  false,
		"Parse_Float64": false,
	}
	for _, s := range specs {
		if _, ok := wantNames[s.Name]; ok {
			wantNames[s.Name] = true
		} else {
			t.Errorf("unexpected spec name %q", s.Name)
		}
	}
	for name, found := range wantNames {
		if !found {
			t.Errorf("expected spec name %q not found in output", name)
		}
	}
}

// TestPhase12MonomorphiseSkipsUnsupported verifies that unsupported type args
// produce skipped entries rather than errors.
func TestPhase12MonomorphiseSkipsUnsupported(t *testing.T) {
	sites := []generics.GenericCallSite{
		{Method: "Parse", TypeArgs: []string{"int64"}},
		{Method: "Parse", TypeArgs: []string{"MyCustomType"}}, // unsupported
	}

	specs, err := generics.MonomorphiseAll(sites)
	if err != nil {
		t.Fatalf("MonomorphiseAll: %v", err)
	}
	if len(specs) != 2 {
		t.Fatalf("expected 2 specs (one good + one skipped), got %d", len(specs))
	}

	skippedCount := 0
	for _, s := range specs {
		if s.IsSkipped {
			skippedCount++
		}
	}
	if skippedCount != 1 {
		t.Errorf("expected 1 skip for MyCustomType, got %d", skippedCount)
	}
}
