//go:build slow

package lsp

import "testing"

func TestReferences(t *testing.T) {
	uri := "file:///test.mochi"
	src := "fun add(x:int,y:int):int{ return x+y } let z = add(1,2)"
	locs, _ := References(uri, src, 0, 1)
	if len(locs) < 2 {
		t.Fatalf("expected multiple references, got %v", locs)
	}
}
