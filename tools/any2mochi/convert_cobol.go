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
				if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					detail = hoverString(hov)
				}
			}
			params, ret := parseCobolSignature(detail)

			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			if len(params) > 0 {
				names := make([]string, len(params))
				for i, p := range params {
					names[i] = p.name
				}
				out.WriteString(strings.Join(names, ", "))
			}
			out.WriteByte(')')
			if ret != "" && ret != "void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")

		case protocol.SymbolKindVariable, protocol.SymbolKindConstant, protocol.SymbolKindField, protocol.SymbolKindProperty:
			out.WriteString("var ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" = nil\n")

		case protocol.SymbolKindClass, protocol.SymbolKindStruct, protocol.SymbolKindInterface, protocol.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")

		case protocol.SymbolKindNamespace, protocol.SymbolKindPackage, protocol.SymbolKindModule:
			if len(s.Children) > 0 {
				writeCobolSymbols(out, ls, src, nameParts, s.Children)
			}
			continue
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
		name = strings.TrimPrefix(name, "by")
		name = strings.TrimPrefix(name, "value")
		name = strings.TrimPrefix(name, "reference")
		params = append(params, name)
	}
	return params
}

type cobolParam struct{ name string }

func parseCobolSignature(detail string) ([]cobolParam, string) {
	params := parseCobolParams(detail)
	ret := ""
	lower := strings.ToLower(detail)
	if idx := strings.Index(lower, "returning"); idx != -1 {
		r := strings.TrimSpace(detail[idx+len("returning"):])
		r = strings.TrimSuffix(r, ".")
		if f := strings.Fields(r); len(f) > 0 {
			ret = mapCobolType(f[0])
		}
	}
	out := make([]cobolParam, len(params))
	for i, p := range params {
		out[i] = cobolParam{name: p}
	}
	return out, ret
}

func mapCobolType(t string) string {
	t = strings.ToLower(t)
	switch {
	case strings.Contains(t, "9"):
		return "int"
	case strings.Contains(t, "x"), strings.Contains(t, "char"), strings.Contains(t, "string"):
		return "string"
	case strings.Contains(t, "comp-2"), strings.Contains(t, "float"), strings.Contains(t, "decimal"):
		return "float"
	default:
		return ""
	}
}
