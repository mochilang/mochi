package lsp

import "testing"

func TestRename(t *testing.T) {
	docs := map[string]string{"file:///test.mochi": "fun add(x:int,y:int):int{ return x+y } let z = add(1,2)"}
	edit, _ := Rename(docs, "file:///test.mochi", 0, 1, "sum")
	if len(edit.Changes) == 0 {
		t.Fatalf("expected edits, got %v", edit)
	}
	if len(edit.Changes["file:///test.mochi"]) < 2 {
		t.Fatalf("expected multiple edits, got %v", edit.Changes["file:///test.mochi"])
	}
}
