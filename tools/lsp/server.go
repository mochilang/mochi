package lsp

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"

	"github.com/alecthomas/participle/v2/lexer"

	"github.com/sourcegraph/jsonrpc2"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"

	"mochi/parser"
)

// Server implements a minimal Language Server Protocol server for Mochi.
type Server struct {
	handler protocol.Handler
	docs    map[protocol.DocumentUri]string
}

// New returns a new Server instance with handlers registered.
func New() *Server {
	s := &Server{docs: make(map[protocol.DocumentUri]string)}
	s.handler = protocol.Handler{
		Initialize:            s.initialize,
		Initialized:           s.initialized,
		Shutdown:              s.shutdown,
		SetTrace:              s.setTrace,
		TextDocumentDidOpen:   s.didOpen,
		TextDocumentDidChange: s.didChange,
		TextDocumentDidSave:   s.didSave,
		TextDocumentHover:     s.hover,
	}
	return s
}

// Serve runs the server on the given connection.
func (s *Server) Serve(ctx context.Context, rwc io.ReadWriteCloser) error {
	conn := newConn(ctx, rwc, s.handler)
	<-conn.DisconnectNotify()
	return nil
}

// RunStdio runs the server using stdin/stdout streams.
func (s *Server) RunStdio() error {
	return s.Serve(context.Background(), stdrwc{})
}

func (s *Server) initialize(ctx *glsp.Context, params *protocol.InitializeParams) (any, error) {
	caps := s.handler.CreateServerCapabilities()
	kind := protocol.TextDocumentSyncKindIncremental
	caps.TextDocumentSync = &protocol.TextDocumentSyncOptions{Change: &kind}
	hoverProvider := true
	caps.HoverProvider = &hoverProvider
	version := "0.1"
	return protocol.InitializeResult{
		Capabilities: caps,
		ServerInfo: &protocol.InitializeResultServerInfo{
			Name:    "mochi-lsp",
			Version: &version,
		},
	}, nil
}

func (s *Server) initialized(ctx *glsp.Context, params *protocol.InitializedParams) error {
	return nil
}

func (s *Server) shutdown(ctx *glsp.Context) error { return nil }
func (s *Server) setTrace(ctx *glsp.Context, params *protocol.SetTraceParams) error {
	protocol.SetTraceValue(params.Value)
	return nil
}

func (s *Server) didOpen(ctx *glsp.Context, params *protocol.DidOpenTextDocumentParams) error {
	uri := params.TextDocument.URI
	s.docs[uri] = params.TextDocument.Text
	s.publishDiagnostics(ctx, uri, params.TextDocument.Text)
	return nil
}

func (s *Server) didChange(ctx *glsp.Context, params *protocol.DidChangeTextDocumentParams) error {
	uri := params.TextDocument.URI
	text := s.docs[uri]
	for _, ch := range params.ContentChanges {
		switch c := ch.(type) {
		case protocol.TextDocumentContentChangeEvent:
			if c.Text != "" {
				text = c.Text
			}
		case protocol.TextDocumentContentChangeEventWhole:
			if c.Text != "" {
				text = c.Text
			}
		}
	}
	s.docs[uri] = text
	s.publishDiagnostics(ctx, uri, text)
	return nil
}

func (s *Server) didSave(ctx *glsp.Context, params *protocol.DidSaveTextDocumentParams) error {
	uri := params.TextDocument.URI
	text := s.docs[uri]
	if params.Text != nil {
		text = *params.Text
	}
	s.publishDiagnostics(ctx, uri, text)
	return nil
}

func (s *Server) hover(ctx *glsp.Context, params *protocol.HoverParams) (*protocol.Hover, error) {
	// Very basic hover: return file URI and position
	msg := fmt.Sprintf("%s:%d:%d", params.TextDocument.URI, params.Position.Line+1, params.Position.Character+1)
	markup := protocol.MarkupContent{Kind: protocol.MarkupKindPlainText, Value: msg}
	return &protocol.Hover{Contents: markup}, nil
}

func (s *Server) publishDiagnostics(ctx *glsp.Context, uri protocol.DocumentUri, text string) {
	var diags []protocol.Diagnostic
	if _, err := parser.ParseString(text); err != nil {
		var posErr interface{ Position() lexer.Position }
		if errors.As(err, &posErr) {
			pos := posErr.Position()
			sev := protocol.DiagnosticSeverityError
			src := "parser"
			diags = append(diags, protocol.Diagnostic{
				Range: protocol.Range{
					Start: protocol.Position{Line: uint32(pos.Line - 1), Character: uint32(pos.Column - 1)},
					End:   protocol.Position{Line: uint32(pos.Line - 1), Character: uint32(pos.Column)},
				},
				Severity: &sev,
				Source:   &src,
				Message:  err.Error(),
			})
		} else {
			sev := protocol.DiagnosticSeverityError
			src := "parser"
			diags = append(diags, protocol.Diagnostic{
				Severity: &sev,
				Source:   &src,
				Message:  err.Error(),
			})
		}
	}
	ctx.Notify(string(protocol.ServerTextDocumentPublishDiagnostics), protocol.PublishDiagnosticsParams{URI: uri, Diagnostics: diags})
}

type pipe struct {
	r io.ReadCloser
	w io.WriteCloser
}

func (p *pipe) Read(b []byte) (int, error)  { return p.r.Read(b) }
func (p *pipe) Write(b []byte) (int, error) { return p.w.Write(b) }
func (p *pipe) Close() error                { p.r.Close(); return p.w.Close() }

type stdrwc struct{}

func (stdrwc) Read(p []byte) (int, error)  { return io.ReadFull(os.Stdin, p) }
func (stdrwc) Write(p []byte) (int, error) { return os.Stdout.Write(p) }
func (stdrwc) Close() error                { return nil }

// newConn creates a JSON-RPC2 connection for the handler.
func newConn(ctx context.Context, rwc io.ReadWriteCloser, handler protocol.Handler) *jsonrpc2.Conn {
	h := jsonrpc2.HandlerWithError(func(ctx context.Context, conn *jsonrpc2.Conn, req *jsonrpc2.Request) (any, error) {
		glspCtx := glsp.Context{
			Method: req.Method,
			Notify: func(method string, params any) { _ = conn.Notify(ctx, method, params) },
			Call:   func(method string, params, result any) { _ = conn.Call(ctx, method, params, result) },
		}
		if req.Params != nil {
			glspCtx.Params = *req.Params
		}
		switch req.Method {
		case "exit":
			handler.Handle(&glspCtx)
			return nil, conn.Close()
		default:
			r, validMethod, validParams, err := handler.Handle(&glspCtx)
			if !validMethod {
				return nil, &jsonrpc2.Error{Code: jsonrpc2.CodeMethodNotFound, Message: fmt.Sprintf("method not supported: %s", req.Method)}
			}
			if !validParams {
				if err != nil {
					return nil, &jsonrpc2.Error{Code: jsonrpc2.CodeInvalidParams, Message: err.Error()}
				}
				return nil, &jsonrpc2.Error{Code: jsonrpc2.CodeInvalidParams}
			}
			if err != nil {
				return nil, &jsonrpc2.Error{Code: jsonrpc2.CodeInvalidRequest, Message: err.Error()}
			}
			return r, nil
		}
	})
	return jsonrpc2.NewConn(ctx, jsonrpc2.NewBufferedStream(rwc, jsonrpc2.VSCodeObjectCodec{}), h)
}
