package lsp

import (
	_ "github.com/tliron/commonlog/simple"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
	"github.com/tliron/glsp/server"
)

const Name = "mochi-language-server"

// Server implements a basic LSP server for Mochi.
type Server struct {
	srv       *server.Server
	documents map[string]string
}

// NewServer creates a new Server instance with handlers registered.
func NewServer() *Server {
	s := &Server{documents: make(map[string]string)}
	handler := protocol.Handler{
		Initialize:                 s.initialize,
		TextDocumentDidOpen:        s.didOpen,
		TextDocumentDidChange:      s.didChange,
		TextDocumentDocumentSymbol: s.documentSymbols,
		TextDocumentHover:          s.hover,
		TextDocumentCompletion:     s.completion,
		TextDocumentDefinition:     s.definition,
	}
	s.srv = server.NewServer(&handler, Name, false)
	return s
}

// RunStdio starts serving LSP requests over stdin/stdout.
func (s *Server) RunStdio() { s.srv.RunStdio() }

func (s *Server) initialize(ctx *glsp.Context, params *protocol.InitializeParams) (any, error) {
	caps := protocol.ServerCapabilities{}
	syncKind := protocol.TextDocumentSyncKindIncremental
	openClose := true
	caps.TextDocumentSync = &protocol.TextDocumentSyncOptions{
		OpenClose: &openClose,
		Change:    &syncKind,
	}
	version := "dev"
	return protocol.InitializeResult{
		Capabilities: caps,
		ServerInfo: &protocol.InitializeResultServerInfo{
			Name:    Name,
			Version: &version,
		},
	}, nil
}

func (s *Server) didOpen(ctx *glsp.Context, params *protocol.DidOpenTextDocumentParams) error {
	uri := string(params.TextDocument.URI)
	text := params.TextDocument.Text
	s.documents[uri] = text
	diags := Analyze(uri, text)
	ctx.Notify(protocol.ServerTextDocumentPublishDiagnostics, protocol.PublishDiagnosticsParams{
		URI:         params.TextDocument.URI,
		Diagnostics: diags,
	})
	return nil
}

func (s *Server) didChange(ctx *glsp.Context, params *protocol.DidChangeTextDocumentParams) error {
	if len(params.ContentChanges) == 0 {
		return nil
	}
	var text string
	for _, change := range params.ContentChanges {
		switch c := change.(type) {
		case protocol.TextDocumentContentChangeEvent:
			text = c.Text
		case protocol.TextDocumentContentChangeEventWhole:
			text = c.Text
		}
	}
	uri := string(params.TextDocument.URI)
	s.documents[uri] = text
	diags := Analyze(uri, text)
	ctx.Notify(protocol.ServerTextDocumentPublishDiagnostics, protocol.PublishDiagnosticsParams{
		URI:         params.TextDocument.URI,
		Diagnostics: diags,
	})
	return nil
}

func (s *Server) documentSymbols(ctx *glsp.Context, params *protocol.DocumentSymbolParams) (any, error) {
	uri := string(params.TextDocument.URI)
	src, ok := s.documents[uri]
	if !ok {
		return nil, nil
	}
	syms, _ := DocumentSymbols(uri, src)
	return syms, nil
}

func (s *Server) hover(ctx *glsp.Context, params *protocol.HoverParams) (*protocol.Hover, error) {
	uri := string(params.TextDocument.URI)
	src, ok := s.documents[uri]
	if !ok {
		return nil, nil
	}
	h, _ := Hover(uri, src, int(params.Position.Line), int(params.Position.Character))
	return &h, nil
}

func (s *Server) completion(ctx *glsp.Context, params *protocol.CompletionParams) (any, error) {
	return CompletionItems(), nil
}

func (s *Server) definition(ctx *glsp.Context, params *protocol.DefinitionParams) (any, error) {
	uri := string(params.TextDocument.URI)
	src, ok := s.documents[uri]
	if !ok {
		return nil, nil
	}
	locs, _ := Definition(uri, src, int(params.Position.Line), int(params.Position.Character))
	if len(locs) == 0 {
		return nil, nil
	}
	if len(locs) == 1 {
		return locs[0], nil
	}
	return locs, nil
}
