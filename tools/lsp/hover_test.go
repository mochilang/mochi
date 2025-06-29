package lsp

import "testing"

func TestHover(t *testing.T) {
	src := "fun add(x:int,y:int):int{ return x+y }"
	hover, diags := Hover("test.mochi", src, 0, 2)
	if len(diags) != 0 {
		t.Fatalf("unexpected diagnostics: %v", diags)
	}
	if hover.Contents == nil {
		t.Fatalf("expected hover text")
	}
}
