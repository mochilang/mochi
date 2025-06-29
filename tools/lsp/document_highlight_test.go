package lsp

import "testing"

func TestDocumentHighlight(t *testing.T) {
	uri := "file:///test.mochi"
	src := "fun add(x:int,y:int):int{ return x+y } let z = add(1,2)"
	highlights, _ := DocumentHighlight(uri, src, 0, 1)
	if len(highlights) < 2 {
		t.Fatalf("expected multiple highlights, got %v", highlights)
	}
}
