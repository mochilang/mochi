package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertGo converts Go source code to Mochi using the Go language server.
// It parses the source with gopls and converts discovered symbols into
// minimal Mochi stubs. Any diagnostics reported by the server are returned
// as errors.
func ConvertGo(src string) ([]byte, error) {
	ls := Servers["go"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeGoSymbols(&out, nil, syms)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertGoFile reads the Go file at path and converts it to Mochi.
func ConvertGoFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertGo(string(data))
}

func writeGoSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			params, ret := parseGoDetail(s.Detail)
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
			}
			out.WriteByte(')')
			if ret != "" && ret != "void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
		case protocol.SymbolKindStruct, protocol.SymbolKindClass, protocol.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			out.WriteString("let ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('\n')
		}
		if len(s.Children) > 0 {
			writeGoSymbols(out, nameParts, s.Children)
		}
	}
}

func parseGoDetail(detail *string) ([]string, string) {
	if detail == nil {
		return nil, ""
	}
	d := strings.TrimSpace(*detail)
	if d == "" {
		return nil, ""
	}
	open := strings.Index(d, "(")
	close := strings.LastIndex(d, ")")
	if open == -1 || close == -1 || close < open {
		return nil, strings.TrimSpace(d)
	}
	paramsPart := d[open+1 : close]
	params := []string{}
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		fields := strings.Fields(p)
		if len(fields) > 0 {
			params = append(params, fields[0])
		}
	}
	ret := strings.TrimSpace(d[close+1:])
	return params, ret
}
