//go:build slow

package lsp

import "testing"

func TestCompletionItems(t *testing.T) {
	items := CompletionItems()
	if len(items) == 0 {
		t.Fatal("expected completions")
	}
}
