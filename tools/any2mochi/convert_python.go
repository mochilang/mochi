package any2mochi

import (
	"context"
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertPython converts Python source code to a minimal Mochi representation
// using the Python language server.
func ConvertPython(src string) ([]byte, error) {
	ls := Servers["python"]
	syms, diags, c, err := parseTextWithClient(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	defer c.Close()
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	var out strings.Builder
	for _, s := range syms {
		writePySymbol(&out, c, s)
	}

	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertPythonFile reads the Python file at path and converts it to Mochi.
func ConvertPythonFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPython(string(data))
}

func writePySymbol(out *strings.Builder, c *client, s protocol.DocumentSymbol) {
	switch s.Kind {
	case protocol.SymbolKindFunction:
		params, ret := pyHoverFunc(c, s.SelectionRange.Start)
		out.WriteString("fun ")
		out.WriteString(s.Name)
		out.WriteByte('(')
		for i, p := range params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteByte(')')
		if ret != "" && ret != "None" {
			out.WriteString(": ")
			out.WriteString(ret)
		}
		out.WriteString(" {}\n")
	case protocol.SymbolKindClass:
		out.WriteString("type ")
		out.WriteString(s.Name)
		out.WriteString(" {\n")
		for _, ch := range s.Children {
			if ch.Kind == protocol.SymbolKindField || ch.Kind == protocol.SymbolKindProperty || ch.Kind == protocol.SymbolKindVariable {
				_, typ := pyHoverVar(c, ch.SelectionRange.Start)
				out.WriteString("  ")
				out.WriteString(ch.Name)
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		}
		out.WriteString("}\n")
	case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
		_, typ := pyHoverVar(c, s.SelectionRange.Start)
		out.WriteString("let ")
		out.WriteString(s.Name)
		if typ != "" {
			out.WriteString(": ")
			out.WriteString(typ)
		}
		out.WriteByte('\n')
	}
}

func pyHoverFunc(c *client, pos protocol.Position) ([]string, string) {
	var hover protocol.Hover
	_ = c.conn.Call(context.Background(), "textDocument/hover", protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///input"},
			Position:     pos,
		},
	}, &hover)
	text := hoverText(hover)
	text = strings.TrimPrefix(text, "(function) ")
	if !strings.HasPrefix(text, "def ") {
		return nil, ""
	}
	text = strings.TrimPrefix(text, "def ")
	ret := ""
	if idx := strings.Index(text, "->"); idx != -1 {
		ret = strings.TrimSpace(text[idx+2:])
		text = strings.TrimSpace(text[:idx])
	}
	if strings.HasSuffix(text, ")") {
		text = text[:len(text)-1]
	}
	open := strings.IndexByte(text, '(')
	nameAndParams := text
	if open != -1 {
		nameAndParams = text[open+1:]
	}
	paramsStr := nameAndParams
	params := []string{}
	for _, p := range strings.Split(paramsStr, ",") {
		p = strings.TrimSpace(p)
		if p == "" || p == "self" {
			continue
		}
		parts := strings.SplitN(p, ":", 2)
		params = append(params, strings.TrimSpace(parts[0]))
	}
	return params, ret
}

func pyHoverVar(c *client, pos protocol.Position) (string, string) {
	var hover protocol.Hover
	_ = c.conn.Call(context.Background(), "textDocument/hover", protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: "file:///input"},
			Position:     pos,
		},
	}, &hover)
	text := hoverText(hover)
	if idx := strings.Index(text, ":"); idx != -1 {
		return strings.TrimSpace(text[:idx]), strings.TrimSpace(text[idx+1:])
	}
	return text, ""
}

func hoverText(h protocol.Hover) string {
	if mc, ok := h.Contents.(protocol.MarkupContent); ok {
		return mc.Value
	}
	if s, ok := h.Contents.(string); ok {
		return s
	}
	return fmt.Sprintf("%v", h.Contents)
}
