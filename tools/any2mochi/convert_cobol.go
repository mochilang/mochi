package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertCobol converts COBOL source code to Mochi using the language server.
func ConvertCobol(src string) ([]byte, error) {
	ls := Servers["cobol"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	var out strings.Builder
	writeCobolSymbols(&out, ls, src, nil, syms)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertCobolFile reads the COBOL file and converts it to Mochi.
func ConvertCobolFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertCobol(string(data))
}

func writeCobolSymbols(out *strings.Builder, ls LanguageServer, src string, prefix []string, syms []protocol.DocumentSymbol) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}

		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			detail := ""
			if s.Detail != nil {
				detail = *s.Detail
			}
			if detail == "" {
				if d, _, err := HoverText(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					detail = d
				}
			}
			params := parseCobolParams(detail)

			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			if len(params) > 0 {
				out.WriteString(strings.Join(params, ", "))
			}
			out.WriteString(") {}\n")

		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			out.WriteString("var ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" = nil\n")

		case protocol.SymbolKindClass, protocol.SymbolKindStruct, protocol.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
		}

		if len(s.Children) > 0 {
			writeCobolSymbols(out, ls, src, nameParts, s.Children)
		}
	}
}

func parseCobolParams(detail string) []string {
	start := strings.Index(detail, "(")
	end := strings.Index(detail, ")")
	if start == -1 || end == -1 || end < start {
		return nil
	}
	list := detail[start+1 : end]
	parts := strings.Split(list, ",")
	params := make([]string, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		fields := strings.Fields(p)
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		name = strings.TrimSuffix(name, ".")
		name = strings.TrimPrefix(name, "using")
		params = append(params, name)
	}
	return params
}
