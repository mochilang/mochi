package lsp

import (
	"testing"

	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

func TestServerHandlers(t *testing.T) {
	srv := NewServer()
	uri := protocol.DocumentUri("file:///test.mochi")
	code := "fun add(x:int,y:int):int{ return x+y }"
	srv.documents[string(uri)] = code

	ctx := &glsp.Context{}
	res, err := srv.documentSymbols(ctx, &protocol.DocumentSymbolParams{TextDocument: protocol.TextDocumentIdentifier{URI: uri}})
	if err != nil {
		t.Fatal(err)
	}
	syms := res.([]protocol.DocumentSymbol)
	if len(syms) != 1 || syms[0].Name != "add" {
		t.Fatalf("unexpected symbols: %v", syms)
	}

	h, err := srv.hover(ctx, &protocol.HoverParams{TextDocumentPositionParams: protocol.TextDocumentPositionParams{TextDocument: protocol.TextDocumentIdentifier{URI: uri}, Position: protocol.Position{Line: 0, Character: 1}}})
	if err != nil {
		t.Fatal(err)
	}
	mc, ok := h.Contents.(protocol.MarkupContent)
	if !ok || mc.Value == "" {
		t.Fatalf("expected hover text")
	}

	res2, err := srv.completion(ctx, &protocol.CompletionParams{})
	if err != nil {
		t.Fatal(err)
	}
	items := res2.([]protocol.CompletionItem)
	if len(items) == 0 {
		t.Fatal("expected completions")
	}

	res3, err := srv.definition(ctx, &protocol.DefinitionParams{TextDocumentPositionParams: protocol.TextDocumentPositionParams{TextDocument: protocol.TextDocumentIdentifier{URI: uri}, Position: protocol.Position{Line: 0, Character: 2}}})
	if err != nil {
		t.Fatal(err)
	}
	loc := res3.(protocol.Location)
	if loc.URI != uri {
		t.Fatalf("unexpected definition location %v", loc)
	}
}
