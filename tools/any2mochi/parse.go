package any2mochi

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
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
	return ParseTextWithRoot(cmdName, args, langID, src, "")
}

// ParseTextWithRoot is like ParseText but allows specifying a root directory
// for the language server. Some servers (e.g. fortls) only return symbols when
// initialized with a workspace root.
func ParseTextWithRoot(cmdName string, args []string, langID, src, root string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
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

	tempDir := ""
	if root == "" {
		var err error
		tempDir, err = os.MkdirTemp("", "lsp")
		if err != nil {
			return nil, nil, err
		}
		root = tempDir
	}
	if err := c.initializeWithRoot(ctx, root); err != nil {
		c.Close()
		if tempDir != "" {
			os.RemoveAll(tempDir)
		}
		return nil, nil, err
	}
	absRoot, _ := filepath.Abs(root)
	ext := ""
	switch langID {
	case "fortran":
		ext = ".f90"
	case "c":
		ext = ".c"
	case "cpp":
		ext = ".cpp"
	case "python":
		ext = ".py"
	case "typescript":
		ext = ".ts"
	}
	filePath := filepath.Join(absRoot, "input"+ext)
	if err := os.WriteFile(filePath, []byte(src), 0644); err != nil {
		c.Close()
		if tempDir != "" {
			os.RemoveAll(tempDir)
		}
		return nil, nil, err
	}
	uri := protocol.DocumentUri("file://" + filepath.ToSlash(filePath))
	if err := c.conn.Notify(ctx, "textDocument/didOpen", protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{URI: uri, LanguageID: langID, Version: 1, Text: src},
	}); err != nil {
		c.Close()
		os.Remove(filePath)
		if tempDir != "" {
			os.RemoveAll(tempDir)
		}
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
	os.Remove(filePath)
	if tempDir != "" {
		os.RemoveAll(tempDir)
	}
	c.mu.Lock()
	diags := append([]protocol.Diagnostic(nil), c.diags...)
	c.mu.Unlock()
	return syms, diags, nil
}

func (c *client) initialize(ctx context.Context) error {
	return c.initializeWithRoot(ctx, "")
}

func (c *client) initializeWithRoot(ctx context.Context, root string) error {
	hierarchical := true
	pid := protocol.Integer(os.Getpid())
	uri := protocol.DocumentUri("file:///")
	params := protocol.InitializeParams{
		ProcessID: &pid,
		RootURI:   &uri,
		Capabilities: protocol.ClientCapabilities{
			TextDocument: &protocol.TextDocumentClientCapabilities{
				DocumentSymbol: &protocol.DocumentSymbolClientCapabilities{
					HierarchicalDocumentSymbolSupport: &hierarchical,
				},
			},
		},
	}
	if root != "" {
		abs, _ := filepath.Abs(root)
		uri := protocol.DocumentUri("file://" + filepath.ToSlash(abs))
		params.RootURI = &uri
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

// HoverAt opens a language server and returns hover information for the
// provided position in the source.
func HoverAt(cmdName string, args []string, langID string, src string, pos protocol.Position) (protocol.Hover, error) {
	return HoverAtWithRoot(cmdName, args, langID, src, pos, "")
}

// HoverAtWithRoot is like HoverAt but allows specifying a workspace root.
func HoverAtWithRoot(cmdName string, args []string, langID string, src string, pos protocol.Position, root string) (protocol.Hover, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	cmd := exec.CommandContext(ctx, cmdName, args...)
	stdin, err := cmd.StdinPipe()
	if err != nil {
		return protocol.Hover{}, err
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return protocol.Hover{}, err
	}
	if err := cmd.Start(); err != nil {
		return protocol.Hover{}, err
	}

	stream := jsonrpc2.NewBufferedStream(&pipe{r: stdout, w: stdin}, jsonrpc2.VSCodeObjectCodec{})
	c := &client{cmd: cmd}
	conn := jsonrpc2.NewConn(ctx, stream, jsonrpc2.HandlerWithError(func(ctx context.Context, conn *jsonrpc2.Conn, req *jsonrpc2.Request) (interface{}, error) {
		return nil, nil
	}))
	c.conn = conn

	if err := c.initializeWithRoot(ctx, root); err != nil {
		c.Close()
		return protocol.Hover{}, err
	}
	var uri protocol.DocumentUri
	if root != "" {
		abs, _ := filepath.Abs(root)
		ext := ""
		switch langID {
		case "fortran":
			ext = ".f90"
		case "c":
			ext = ".c"
		case "cpp":
			ext = ".cpp"
		case "python":
			ext = ".py"
		case "typescript":
			ext = ".ts"
		}
		uri = protocol.DocumentUri("file://" + filepath.ToSlash(filepath.Join(abs, "input"+ext)))
	} else {
		uri = protocol.DocumentUri("file:///input")
	}
	if err := c.conn.Notify(ctx, "textDocument/didOpen", protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{URI: uri, LanguageID: langID, Version: 1, Text: src},
	}); err != nil {
		c.Close()
		return protocol.Hover{}, err
	}
	var hover protocol.Hover
	if err := c.conn.Call(ctx, "textDocument/hover", protocol.TextDocumentPositionParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: uri},
		Position:     pos,
	}, &hover); err != nil {
		c.Close()
		return protocol.Hover{}, err
	}
	c.Close()
	return hover, nil
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
	return HoverTextWithRoot(cmdName, args, langID, src, pos, "")
}

// HoverTextWithRoot is like HoverText but allows specifying a root directory.
func HoverTextWithRoot(cmdName string, args []string, langID string, src string, pos protocol.Position, root string) (string, []protocol.Diagnostic, error) {
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

	if err := c.initializeWithRoot(ctx, root); err != nil {
		c.Close()
		return "", nil, err
	}
	var uri protocol.DocumentUri
	if root != "" {
		abs, _ := filepath.Abs(root)
		ext := ""
		switch langID {
		case "fortran":
			ext = ".f90"
		case "c":
			ext = ".c"
		case "cpp":
			ext = ".cpp"
		case "python":
			ext = ".py"
		case "typescript":
			ext = ".ts"
		}
		uri = protocol.DocumentUri("file://" + filepath.ToSlash(filepath.Join(abs, "input"+ext)))
	} else {
		uri = protocol.DocumentUri("file:///input")
	}
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
