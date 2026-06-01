package monomorphise_test

import (
	"strings"
	"testing"

	"mochi/package3/swift/monomorphise"
)

// TestPhase12Monomorphise is the MEP-69 Phase 12 gate for Swift generic monomorphisation.
func TestPhase12Monomorphise(t *testing.T) {
	sites := []monomorphise.CallSite{
		{FuncName: "parse", Module: "MyModule", TypeArgs: []string{"int64"}},
		{FuncName: "parse", Module: "MyModule", TypeArgs: []string{"string"}},
		{FuncName: "parse", Module: "MyModule", TypeArgs: []string{"float64"}},
	}

	funcs, err := monomorphise.MonomorphiseAll(sites)
	if err != nil {
		t.Fatalf("MonomorphiseAll: %v", err)
	}
	if len(funcs) != 3 {
		t.Fatalf("expected 3 funcs, got %d", len(funcs))
	}

	skips := 0
	for _, f := range funcs {
		if f.IsSkipped {
			skips++
			t.Errorf("unexpected skip for %v: %s", f.TypeArgs, f.SkipReason)
		}
	}
	if skips != 0 {
		t.Errorf("expected 0 skips, got %d", skips)
	}

	// Verify mangled names.
	for _, f := range funcs {
		if !strings.HasPrefix(f.MochiName, "swift_MyModule_parse_") {
			t.Errorf("unexpected MochiName %q", f.MochiName)
		}
	}
}

func TestPhase12MonomorphiseSkip(t *testing.T) {
	sites := []monomorphise.CallSite{
		{FuncName: "parse", Module: "M", TypeArgs: []string{"int64"}},
		{FuncName: "parse", Module: "M", TypeArgs: []string{"MyCustomType"}},
	}

	funcs, err := monomorphise.MonomorphiseAll(sites)
	if err != nil {
		t.Fatalf("MonomorphiseAll: %v", err)
	}
	if len(funcs) != 2 {
		t.Fatalf("expected 2, got %d", len(funcs))
	}

	skipped := 0
	for _, f := range funcs {
		if f.IsSkipped {
			skipped++
		}
	}
	if skipped != 1 {
		t.Errorf("expected 1 skip for MyCustomType, got %d", skipped)
	}
}
