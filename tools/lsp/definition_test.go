package lsp

import "testing"

func TestDefinition(t *testing.T) {
	src := "fun add(x:int,y:int):int{ return x+y }"
	locs, diags := Definition("test.mochi", src, 0, 2)
	if len(diags) != 0 {
		t.Fatalf("unexpected diagnostics: %v", diags)
	}
	if len(locs) != 1 || locs[0].Range.Start.Line != 0 {
		t.Fatalf("expected definition location, got %v", locs)
	}
}
