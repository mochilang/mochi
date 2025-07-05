package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertOcaml converts ocaml source code to Mochi using the language server.
func ConvertOcaml(src string) ([]byte, error) {
	ls := Servers["ocaml"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeOcamlSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertOcamlFile reads the ocaml file and converts it to Mochi.
func ConvertOcamlFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertOcaml(string(data))
}

func ocamlParamNames(sym protocol.DocumentSymbol) []string {
	var names []string
	start := sym.Range.Start.Line
	for _, c := range sym.Children {
		if c.Kind == protocol.SymbolKindVariable && c.Range.Start.Line == start {
			names = append(names, c.Name)
		}
	}
	return names
}

func writeOcamlSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindNamespace, protocol.SymbolKindModule, protocol.SymbolKindPackage:
			writeOcamlSymbols(out, nameParts, s.Children, src, ls)
		case protocol.SymbolKindFunction:
			names := ocamlParamNames(s)
			types, ret := ocamlHoverSignature(src, s, ls)
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			for i, n := range names {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(n)
				if i < len(types) && types[i] != "" {
					out.WriteString(": ")
					out.WriteString(types[i])
				}
			}
			out.WriteByte(')')
			if ret != "" && ret != "unit" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
		case protocol.SymbolKindStruct, protocol.SymbolKindClass, protocol.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			if len(s.Children) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, c := range s.Children {
					if c.Kind != protocol.SymbolKindField && c.Kind != protocol.SymbolKindVariable {
						continue
					}
					out.WriteString("  ")
					out.WriteString(c.Name)
					typ := ocamlHoverType(src, c, ls)
					if typ != "" && typ != "unit" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				typ := ocamlHoverType(src, s, ls)
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				if typ != "" && typ != "unit" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
			if len(s.Children) > 0 {
				writeOcamlSymbols(out, nameParts, s.Children, src, ls)
			}
		default:
			if len(s.Children) > 0 {
				writeOcamlSymbols(out, nameParts, s.Children, src, ls)
			}
		}
	}
}

func ocamlHoverSignature(src string, sym protocol.DocumentSymbol, ls LanguageServer) ([]string, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	mc, ok := hov.Contents.(protocol.MarkupContent)
	if !ok {
		return nil, ""
	}
	sig := strings.TrimSpace(mc.Value)
	if strings.HasPrefix(sig, "val") {
		if idx := strings.Index(sig, ":"); idx != -1 {
			sig = strings.TrimSpace(sig[idx+1:])
		}
	}
	parts := strings.Split(sig, "->")
	for i := range parts {
		parts[i] = strings.TrimSpace(parts[i])
	}
	if len(parts) == 0 {
		return nil, ""
	}
	ret := mapOcamlType(parts[len(parts)-1])
	paramTypes := make([]string, 0, len(parts)-1)
	for i := 0; i < len(parts)-1; i++ {
		paramTypes = append(paramTypes, mapOcamlType(parts[i]))
	}
	return paramTypes, ret
}

func ocamlHoverType(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
		return mapOcamlType(strings.TrimSpace(mc.Value))
	}
	return ""
}

func mapOcamlType(t string) string {
	switch strings.TrimSpace(t) {
	case "", "unit":
		return ""
	case "int":
		return "int"
	case "float":
		return "float"
	case "string":
		return "string"
	case "bool":
		return "bool"
	default:
		return t
	}
}
