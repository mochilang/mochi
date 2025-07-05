package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertSt converts st source code to Mochi using the language server.
func ConvertSt(src string) ([]byte, error) {
	ls := Servers["st"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	var out strings.Builder
	appendStSymbols(&out, syms, src, ls)

	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func appendStSymbols(out *strings.Builder, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		appendStSymbol(out, s, src, ls, parentNone)
	}
}

const (
	parentNone = iota
	parentClass
)

func appendStSymbol(out *strings.Builder, s protocol.DocumentSymbol, src string, ls LanguageServer, parent int) {
	switch s.Kind {
	case protocol.SymbolKindClass:
		fields, rest := splitStFields(s.Children)
		out.WriteString("type ")
		out.WriteString(s.Name)
		if len(fields) == 0 && len(rest) == 0 {
			out.WriteString(" {}\n")
			return
		}
		out.WriteString(" {\n")
		for _, f := range fields {
			out.WriteString("  ")
			out.WriteString(f.Name)
			out.WriteByte('\n')
		}
		for _, c := range rest {
			var b strings.Builder
			appendStSymbol(&b, c, src, ls, parentClass)
			for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
				out.WriteString("  ")
				out.WriteString(line)
				out.WriteByte('\n')
			}
		}
		out.WriteString("}\n")
	case protocol.SymbolKindMethod, protocol.SymbolKindConstructor, protocol.SymbolKindFunction:
		out.WriteString("fun ")
		out.WriteString(cleanStName(s.Name))
		params := extractStParams(s)
		if len(params) == 0 {
			params = getStHoverParams(src, s.SelectionRange.Start, ls)
		}
		out.WriteByte('(')
		for i, p := range params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteString(") {}\n")
		for _, c := range s.Children {
			appendStSymbol(out, c, src, ls, parent)
		}
	case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
		if parent == parentClass {
			out.WriteString(s.Name)
			out.WriteByte('\n')
		} else if parent == parentNone {
			out.WriteString("let ")
			out.WriteString(s.Name)
			out.WriteByte('\n')
		}
	default:
		for _, c := range s.Children {
			appendStSymbol(out, c, src, ls, parent)
		}
	}
}

func splitStFields(syms []protocol.DocumentSymbol) (fields, rest []protocol.DocumentSymbol) {
	for _, s := range syms {
		switch s.Kind {
		case protocol.SymbolKindField, protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			fields = append(fields, s)
		default:
			rest = append(rest, s)
		}
	}
	return
}

func cleanStName(name string) string {
	if i := strings.Index(name, ":"); i != -1 {
		return name[:i]
	}
	return name
}

func extractStParams(sym protocol.DocumentSymbol) []string {
	start := sym.Range.Start.Line
	var params []string
	for _, c := range sym.Children {
		if c.Kind == protocol.SymbolKindVariable && c.Range.Start.Line == start {
			if c.Name != "" {
				params = append(params, c.Name)
			}
		}
	}
	return params
}

func getStHoverParams(src string, pos protocol.Position, ls LanguageServer) []string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return nil
	}
	text := hoverString(hov)
	text = strings.ReplaceAll(text, "\n", " ")
	parts := strings.Split(text, ":")
	var params []string
	for i := 1; i < len(parts); i++ {
		sec := strings.TrimSpace(parts[i])
		if sec == "" {
			continue
		}
		fields := strings.Fields(sec)
		if len(fields) > 0 {
			p := strings.Trim(fields[0], "()|")
			if p != "" {
				params = append(params, p)
			}
		}
	}
	return params
}

// ConvertStFile reads the st file and converts it to Mochi.
func ConvertStFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertSt(string(data))
}
