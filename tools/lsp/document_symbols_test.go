//go:build slow

package lsp

import "testing"

func TestDocumentSymbols(t *testing.T) {
	src := "fun add(x:int,y:int):int{ return x+y }\nlet v:int = 3"
	syms, diags := DocumentSymbols("test.mochi", src)
	if len(diags) != 0 {
		t.Fatalf("expected no diagnostics, got %v", diags)
	}
	if len(syms) != 2 {
		t.Fatalf("expected 2 symbols, got %d", len(syms))
	}
	if syms[0].Name != "add" || syms[1].Name != "v" {
		t.Fatalf("unexpected symbols: %v", syms)
	}
}
