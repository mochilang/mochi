//go:build slow

package lsp

import "testing"

func TestWorkspaceSymbols(t *testing.T) {
	docs := map[string]string{"file:///test.mochi": "fun add(x:int,y:int):int{ return x+y }"}
	syms := WorkspaceSymbols(docs, "add")
	if len(syms) != 1 || syms[0].Name != "add" {
		t.Fatalf("unexpected symbols: %v", syms)
	}
}
