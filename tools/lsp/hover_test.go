package lsp

import (
	"strings"
	"testing"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

func TestHover(t *testing.T) {
	src := "fun add(x:int,y:int):int{ return x+y }"
	hover, diags := Hover("test.mochi", src, 0, 2)
	if len(diags) != 0 {
		t.Fatalf("unexpected diagnostics: %v", diags)
	}
	mc, ok := hover.Contents.(protocol.MarkupContent)
	if !ok || !strings.Contains(mc.Value, "fun(int, int): int") {
		t.Fatalf("unexpected hover: %#v", hover)
	}
}
