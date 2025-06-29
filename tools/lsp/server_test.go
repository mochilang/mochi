package lsp_test

import (
	"context"
	"encoding/json"
	"io"
	"sync"
	"testing"
	"time"

	"mochi/tools/lsp"

	"github.com/sourcegraph/jsonrpc2"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// pipe implements io.ReadWriteCloser using two pipe ends.
type pipe struct {
	r io.ReadCloser
	w io.WriteCloser
}

func (p *pipe) Read(b []byte) (int, error)  { return p.r.Read(b) }
func (p *pipe) Write(b []byte) (int, error) { return p.w.Write(b) }
func (p *pipe) Close() error                { p.r.Close(); return p.w.Close() }

func startServer(t *testing.T) (*jsonrpc2.Conn, *[]protocol.PublishDiagnosticsParams) {
	t.Helper()
	sr, cw := io.Pipe() // server reads from sr, client writes to cw
	cr, sw := io.Pipe() // client reads from cr, server writes to sw
	serverPipe := &pipe{r: sr, w: sw}
	clientPipe := &pipe{r: cr, w: cw}

	ls := lsp.New()
	go ls.Serve(context.Background(), serverPipe)

	var mu sync.Mutex
	var diags []protocol.PublishDiagnosticsParams
	conn := jsonrpc2.NewConn(context.Background(), jsonrpc2.NewBufferedStream(clientPipe, jsonrpc2.VSCodeObjectCodec{}),
		jsonrpc2.HandlerWithError(func(ctx context.Context, conn *jsonrpc2.Conn, req *jsonrpc2.Request) (any, error) { return nil, nil }),
		jsonrpc2.OnRecv(func(req *jsonrpc2.Request, resp *jsonrpc2.Response) {
			if req != nil && req.Method == string(protocol.ServerTextDocumentPublishDiagnostics) {
				var p protocol.PublishDiagnosticsParams
				_ = json.Unmarshal(*req.Params, &p)
				mu.Lock()
				diags = append(diags, p)
				mu.Unlock()
			}
		}))
	return conn, &diags
}

func TestLSPInitialize(t *testing.T) {
	conn, _ := startServer(t)
	var result protocol.InitializeResult
	if err := conn.Call(context.Background(), "initialize", &protocol.InitializeParams{}, &result); err != nil {
		t.Fatalf("initialize: %v", err)
	}
	if result.ServerInfo == nil || result.ServerInfo.Name != "mochi-lsp" {
		t.Fatalf("unexpected server info: %#v", result.ServerInfo)
	}
}

func TestLSPDiagnostics(t *testing.T) {
	conn, diags := startServer(t)
	ctx := context.Background()
	var initRes protocol.InitializeResult
	if err := conn.Call(ctx, "initialize", &protocol.InitializeParams{}, &initRes); err != nil {
		t.Fatalf("init: %v", err)
	}
	_ = conn.Notify(ctx, "initialized", &protocol.InitializedParams{})

	uri := protocol.DocumentUri("file:///test.mochi")
	open := protocol.DidOpenTextDocumentParams{TextDocument: protocol.TextDocumentItem{URI: uri, LanguageID: "mochi", Version: 1, Text: "print(\"hi\")"}}
	_ = conn.Notify(ctx, "textDocument/didOpen", open)

	bad := "fun {"
	invalid := protocol.DidChangeTextDocumentParams{
		TextDocument: protocol.VersionedTextDocumentIdentifier{
			TextDocumentIdentifier: protocol.TextDocumentIdentifier{URI: uri},
			Version:                2,
		},
		ContentChanges: []any{protocol.TextDocumentContentChangeEvent{Text: bad}},
	}
	_ = conn.Notify(ctx, "textDocument/didChange", invalid)

	// Allow diagnostics to be processed
	// Wait for at most short time
	for i := 0; i < 100; i++ {
		if len(*diags) >= 2 {
			break
		}
		time.Sleep(10 * time.Millisecond)
	}
	t.Logf("diags: %+v", *diags)
	if len(*diags) < 2 {
		t.Fatal("no diagnostics published")
	}
	if last := (*diags)[len(*diags)-1]; len(last.Diagnostics) == 0 {
		t.Fatalf("expected error diagnostics")
	}
}
