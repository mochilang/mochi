package any2mochi

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os/exec"
	"strings"
	"sync"
	"time"

	jsonrpc2 "github.com/sourcegraph/jsonrpc2"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

type client struct {
	conn  *jsonrpc2.Conn
	cmd   *exec.Cmd
	mu    sync.Mutex
	diags []protocol.Diagnostic
}

// ParseText starts the language server command given by cmdName and
// parses the provided source using the Language Server Protocol.
// It returns the document symbols reported by the server for the file.
// The server must support the textDocument/documentSymbol request.
func ParseText(cmdName string, args []string, langID string, src string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	cmd := exec.CommandContext(ctx, cmdName, args...)
	stdin, err := cmd.StdinPipe()
	if err != nil {
		return nil, nil, err
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, nil, err
	}
	if err := cmd.Start(); err != nil {
		return nil, nil, err
	}

	stream := jsonrpc2.NewBufferedStream(&pipe{r: stdout, w: stdin}, jsonrpc2.VSCodeObjectCodec{})
	c := &client{cmd: cmd}
	conn := jsonrpc2.NewConn(ctx, stream, jsonrpc2.HandlerWithError(func(ctx context.Context, conn *jsonrpc2.Conn, req *jsonrpc2.Request) (interface{}, error) {
		switch req.Method {
		case "textDocument/publishDiagnostics":
			var params protocol.PublishDiagnosticsParams
			if err := json.Unmarshal(*req.Params, &params); err == nil {
				c.mu.Lock()
				c.diags = append(c.diags, params.Diagnostics...)
				c.mu.Unlock()
			}
			return nil, nil
		default:
			return nil, nil
		}
	}))
	c.conn = conn

	if err := c.initialize(ctx); err != nil {
		c.Close()
		return nil, nil, err
	}
	uri := protocol.DocumentUri("file:///input")
	if err := c.conn.Notify(ctx, "textDocument/didOpen", protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{URI: uri, LanguageID: langID, Version: 1, Text: src},
	}); err != nil {
		c.Close()
		return nil, nil, err
	}
	var syms []protocol.DocumentSymbol
	if err := c.conn.Call(ctx, "textDocument/documentSymbol", protocol.DocumentSymbolParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: uri},
	}, &syms); err != nil {
		c.Close()
		return nil, nil, err
	}
	c.Close()
	c.mu.Lock()
	diags := append([]protocol.Diagnostic(nil), c.diags...)
	c.mu.Unlock()
	return syms, diags, nil
}

func (c *client) initialize(ctx context.Context) error {
	hierarchical := true
	params := protocol.InitializeParams{
		Capabilities: protocol.ClientCapabilities{
			TextDocument: &protocol.TextDocumentClientCapabilities{
				DocumentSymbol: &protocol.DocumentSymbolClientCapabilities{
					HierarchicalDocumentSymbolSupport: &hierarchical,
				},
			},
		},
	}
	var res protocol.InitializeResult
	if err := c.conn.Call(ctx, "initialize", params, &res); err != nil {
		return err
	}
	if err := c.conn.Notify(ctx, "initialized", struct{}{}); err != nil {
		return err
	}
	return nil
}

func (c *client) Close() error {
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	c.conn.Call(ctx, "shutdown", nil, nil)
	c.conn.Notify(ctx, "exit", nil)
	c.conn.Close()
	return c.cmd.Wait()
}

type pipe struct {
	r io.ReadCloser
	w io.WriteCloser
}

func (p *pipe) Read(b []byte) (int, error)  { return p.r.Read(b) }
func (p *pipe) Write(b []byte) (int, error) { return p.w.Write(b) }
func (p *pipe) Close() error {
	p.w.Close()
	return p.r.Close()
}

// HoverText opens cmdName as an LSP server and returns the hover information
// for the specified position. Diagnostics produced by the server are also
// returned.
func HoverText(cmdName string, args []string, langID string, src string, pos protocol.Position) (string, []protocol.Diagnostic, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	cmd := exec.CommandContext(ctx, cmdName, args...)
	stdin, err := cmd.StdinPipe()
	if err != nil {
		return "", nil, err
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return "", nil, err
	}
	if err := cmd.Start(); err != nil {
		return "", nil, err
	}

	stream := jsonrpc2.NewBufferedStream(&pipe{r: stdout, w: stdin}, jsonrpc2.VSCodeObjectCodec{})
	c := &client{cmd: cmd}
	conn := jsonrpc2.NewConn(ctx, stream, jsonrpc2.HandlerWithError(func(ctx context.Context, conn *jsonrpc2.Conn, req *jsonrpc2.Request) (interface{}, error) {
		switch req.Method {
		case "textDocument/publishDiagnostics":
			var params protocol.PublishDiagnosticsParams
			if err := json.Unmarshal(*req.Params, &params); err == nil {
				c.mu.Lock()
				c.diags = append(c.diags, params.Diagnostics...)
				c.mu.Unlock()
			}
		}
		return nil, nil
	}))
	c.conn = conn

	if err := c.initialize(ctx); err != nil {
		c.Close()
		return "", nil, err
	}
	uri := protocol.DocumentUri("file:///input")
	if err := c.conn.Notify(ctx, "textDocument/didOpen", protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{URI: uri, LanguageID: langID, Version: 1, Text: src},
	}); err != nil {
		c.Close()
		return "", nil, err
	}

	var hover protocol.Hover
	if err := c.conn.Call(ctx, "textDocument/hover", protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{TextDocument: protocol.TextDocumentIdentifier{URI: uri}, Position: pos},
	}, &hover); err != nil {
		c.Close()
		return "", nil, err
	}

	c.Close()
	c.mu.Lock()
	diags := append([]protocol.Diagnostic(nil), c.diags...)
	c.mu.Unlock()

	return hoverString(hover), diags, nil
}

func hoverString(h protocol.Hover) string {
	switch v := h.Contents.(type) {
	case protocol.MarkupContent:
		return v.Value
	case protocol.MarkedString:
		return markedStringValue(v)
	case []protocol.MarkedString:
		var parts []string
		for _, m := range v {
			parts = append(parts, markedStringValue(m))
		}
		return strings.Join(parts, "\n")
	case string:
		return v
	default:
		return fmt.Sprintf("%v", v)
	}
}

func markedStringValue(m protocol.MarkedString) string {
	b, err := json.Marshal(m)
	if err != nil {
		return fmt.Sprintf("%v", m)
	}
	var s string
	if err := json.Unmarshal(b, &s); err == nil {
		return s
	}
	var obj struct {
		Language string `json:"language"`
		Value    string `json:"value"`
	}
	if err := json.Unmarshal(b, &obj); err == nil {
		return obj.Value
	}
	return string(b)
}
