package any2mochi

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertProlog converts Prolog source code to Mochi. If a language server
// is configured it will be used, otherwise the bundled CLI parser is
// invoked and finally a regex parser is used as fallback.
func ConvertProlog(src string) ([]byte, error) {
	return convertProlog(src, "")
}

func convertProlog(src, root string) ([]byte, error) {
	ls := Servers["prolog"]
	if ls.Command != "" {
		syms, diags, err := EnsureAndParseWithRoot(ls.Command, ls.Args, ls.LangID, src, root)
		if err == nil {
			if len(diags) > 0 {
				return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
			}
			var out strings.Builder
			writePrologSymbols(&out, ls, src, syms, root)
			if out.Len() > 0 {
				return []byte(out.String()), nil
			}
			return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
		}
		// if the server is missing fall back to the CLI parser
		if !strings.Contains(err.Error(), "not found") {
			return nil, err
		}
	}
	return convertPrologFallback(src)
}

// ConvertPrologFile reads the Prolog file and converts it to Mochi.
func ConvertPrologFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return convertProlog(string(data), filepath.Dir(path))
}

func writePrologSymbols(out *strings.Builder, ls LanguageServer, src string, syms []protocol.DocumentSymbol, root string) {
	for _, s := range syms {
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod,
			protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			params := parsePrologSignature(s.Detail)
			if len(params) == 0 {
				if hov, err := EnsureAndHoverWithRoot(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start, root); err == nil {
					if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
						params = parsePrologSignature(&mc.Value)
					}
				}
			}
			out.WriteString("fun ")
			out.WriteString(s.Name)
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
			}
			out.WriteString(") {\n")
			for _, line := range parsePrologBodyFromRange(src, s.Range) {
				out.WriteString(line)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		default:
			if len(s.Children) > 0 {
				writePrologSymbols(out, ls, src, s.Children, root)
			}
		}
	}
}

func parsePrologSignature(detail *string) []string {
	if detail == nil {
		return nil
	}
	d := strings.ReplaceAll(strings.TrimSpace(*detail), "\n", " ")
	if d == "" {
		return nil
	}
	open := strings.Index(d, "(")
	close := strings.LastIndex(d, ")")
	if open == -1 || close == -1 || close < open {
		if slash := strings.LastIndex(d, "/"); slash != -1 {
			arityPart := strings.TrimSpace(d[slash+1:])
			if n := parseInt(arityPart); n > 0 {
				params := make([]string, n)
				for i := 0; i < n; i++ {
					params[i] = fmt.Sprintf("A%d", i)
				}
				return params
			}
		}
		return nil
	}
	list := strings.Split(d[open+1:close], ",")
	var params []string
	for _, p := range list {
		p = strings.TrimSpace(p)
		p = strings.TrimLeft(p, "+-?@")
		if colon := strings.Index(p, ":"); colon != -1 {
			p = strings.TrimSpace(p[:colon])
		}
		if p != "" {
			params = append(params, p)
		}
	}
	return params
}

func parseInt(s string) int {
	n := 0
	for _, r := range s {
		if r < '0' || r > '9' {
			return -1
		}
		n = n*10 + int(r-'0')
	}
	return n
}

func prologOffsetFromPosition(src string, pos protocol.Position) int {
	lines := strings.Split(src, "\n")
	if int(pos.Line) >= len(lines) {
		return len(src)
	}
	off := 0
	for i := 0; i < int(pos.Line); i++ {
		off += len(lines[i]) + 1
	}
	col := int(pos.Character)
	if col > len(lines[int(pos.Line)]) {
		col = len(lines[int(pos.Line)])
	}
	return off + col
}

func parsePrologBodyFromRange(src string, rng protocol.Range) []string {
	start := prologOffsetFromPosition(src, rng.Start)
	end := prologOffsetFromPosition(src, rng.End)
	if start >= end || start < 0 || end > len(src) {
		return nil
	}
	body := strings.TrimSpace(src[start:end])
	if idx := strings.Index(body, ":-"); idx >= 0 {
		body = body[idx+2:]
	}
	body = strings.TrimSpace(body)
	if strings.HasSuffix(body, ".") {
		body = strings.TrimSuffix(body, ".")
	}
	return ParsePrologBody(body)
}

// convertPrologFallback attempts a best effort conversion using regular
// expressions when no language server is available.
func convertPrologFallback(src string) ([]byte, error) {
	funcs := ParseProlog(src)
	if len(funcs) == 0 {
		if strings.Contains(src, "main :-") || strings.Contains(src, "main:-") {
			funcs = append(funcs, PrologFunc{Name: "main", Body: ""})
		} else {
			return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
		}
	}
	var out strings.Builder
	for _, f := range funcs {
		out.WriteString("fun ")
		out.WriteString(f.Name)
		out.WriteByte('(')
		for i, p := range f.Params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteString(") {\n")
		for _, line := range ParsePrologBody(f.Body) {
			out.WriteString(line)
			out.WriteByte('\n')
		}
		out.WriteString("}\n")
	}
	return []byte(out.String()), nil
}
