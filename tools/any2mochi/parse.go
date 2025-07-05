package any2mochi

import (
	"context"
	"encoding/json"
	"io"
	"os/exec"
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
	var res protocol.InitializeResult
	if err := c.conn.Call(ctx, "initialize", protocol.InitializeParams{}, &res); err != nil {
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
