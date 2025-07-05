package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertCs converts cs source code to Mochi using the language server.
func ConvertCs(src string) ([]byte, error) {
	ls := Servers["cs"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeCsSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertCsFile reads the cs file and converts it to Mochi.
func ConvertCsFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertCs(string(data))
}

func writeCsSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindClass, protocol.SymbolKindStruct, protocol.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindField || c.Kind == protocol.SymbolKindProperty {
					fmt.Fprintf(out, "  %s: any\n", c.Name)
				}
			}
			out.WriteString("}\n")
			var rest []protocol.DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != protocol.SymbolKindField && c.Kind != protocol.SymbolKindProperty {
					rest = append(rest, c)
				}
			}
			if len(rest) > 0 {
				writeCsSymbols(out, nameParts, rest, src, ls)
			}
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			params, ret := parseCsSignature(s.Detail)
			if len(params) == 0 && ret == "" {
				if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
						for _, line := range strings.Split(mc.Value, "\n") {
							l := strings.TrimSpace(line)
							if strings.Contains(l, "(") && strings.Contains(l, ")") {
								if p, r := parseCsSignature(&l); len(p) > 0 || r != "" {
									params, ret = p, r
									break
								}
							}
						}
					}
				}
			}
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
			}
			out.WriteByte(')')
			if ret != "" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
			if len(s.Children) > 0 {
				writeCsSymbols(out, nameParts, s.Children, src, ls)
			}
		case protocol.SymbolKindField, protocol.SymbolKindProperty, protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				out.WriteByte('\n')
			}
			if len(s.Children) > 0 {
				writeCsSymbols(out, nameParts, s.Children, src, ls)
			}
		case protocol.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindEnumMember {
					fmt.Fprintf(out, "  %s\n", c.Name)
				}
			}
			out.WriteString("}\n")
			var rest []protocol.DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != protocol.SymbolKindEnumMember {
					rest = append(rest, c)
				}
			}
			if len(rest) > 0 {
				writeCsSymbols(out, nameParts, rest, src, ls)
			}
		default:
			if len(s.Children) > 0 {
				writeCsSymbols(out, nameParts, s.Children, src, ls)
			}
		}
	}
}

func parseCsSignature(detail *string) ([]string, string) {
	if detail == nil {
		return nil, ""
	}
	d := strings.TrimSpace(*detail)
	if d == "" {
		return nil, ""
	}
	open := strings.Index(d, "(")
	close := strings.LastIndex(d, ")")
	if open < 0 || close < open {
		parts := strings.Fields(d)
		if len(parts) > 0 {
			return nil, mapCsType(parts[0])
		}
		return nil, ""
	}
	pre := strings.TrimSpace(d[:open])
	parts := strings.Fields(pre)
	ret := ""
	if len(parts) >= 2 {
		ret = mapCsType(parts[len(parts)-2])
	} else if len(parts) == 1 {
		ret = mapCsType(parts[0])
	}
	paramsPart := strings.TrimSpace(d[open+1 : close])
	params := []string{}
	if paramsPart != "" {
		rawParams := strings.Split(paramsPart, ",")
		for _, p := range rawParams {
			p = strings.TrimSpace(p)
			if p == "" {
				continue
			}
			fields := strings.Fields(p)
			if len(fields) == 0 {
				continue
			}
			name := fields[len(fields)-1]
			if eq := strings.Index(name, "="); eq != -1 {
				name = name[:eq]
			}
			name = strings.Trim(name, "*&[]")
			if name != "" {
				params = append(params, name)
			}
		}
	}
	return params, ret
}

func mapCsType(t string) string {
	for strings.HasSuffix(t, "[]") {
		t = strings.TrimSuffix(t, "[]")
	}
	switch t {
	case "void", "":
		return ""
	case "int", "long", "short", "uint", "ulong", "ushort", "byte", "sbyte":
		return "int"
	case "float", "double", "decimal":
		return "float"
	case "string", "char":
		return "string"
	case "bool":
		return "bool"
	default:
		return ""
	}
}
