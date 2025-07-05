package any2mochi

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertCobol converts COBOL source code to Mochi using the language server.
func ConvertCobol(src string) ([]byte, error) {
	return convertCobol(src, "")
}

func convertCobol(src, root string) ([]byte, error) {
	ls := Servers["cobol"]
	syms, diags, err := EnsureAndParseWithRoot(ls.Command, ls.Args, ls.LangID, src, root)
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
	return convertCobol(string(data), filepath.Dir(path))
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
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.name)
				if p.typ != "" {
					out.WriteString(": ")
					out.WriteString(p.typ)
				}
			}
			out.WriteByte(')')
			if ret != "" && ret != "void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")

		case protocol.SymbolKindVariable, protocol.SymbolKindConstant, protocol.SymbolKindField, protocol.SymbolKindProperty, protocol.SymbolKindEnumMember:
			typ := ""
			if s.Detail != nil {
				typ = parseCobolType(*s.Detail)
			}
			if typ == "" {
				if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					typ = parseCobolType(hoverString(hov))
				}
			}
			out.WriteString("var ")
			out.WriteString(strings.Join(nameParts, "."))
			if typ != "" {
				out.WriteString(": ")
				out.WriteString(typ)
			}
			out.WriteString(" = nil\n")

		case protocol.SymbolKindClass, protocol.SymbolKindStruct, protocol.SymbolKindInterface, protocol.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			var fields []protocol.DocumentSymbol
			for _, c := range s.Children {
				switch c.Kind {
				case protocol.SymbolKindField, protocol.SymbolKindProperty, protocol.SymbolKindVariable:
					fields = append(fields, c)
				}
			}
			if len(fields) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, f := range fields {
					typ := ""
					if f.Detail != nil {
						typ = parseCobolType(*f.Detail)
					}
					if typ == "" {
						if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, f.SelectionRange.Start); err == nil {
							typ = parseCobolType(hoverString(hov))
						}
					}
					out.WriteString("  ")
					out.WriteString(f.Name)
					if typ != "" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
			for _, c := range s.Children {
				switch c.Kind {
				case protocol.SymbolKindField, protocol.SymbolKindProperty, protocol.SymbolKindVariable:
					continue
				default:
					writeCobolSymbols(out, ls, src, nameParts, []protocol.DocumentSymbol{c})
				}
			}
			continue

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

func parseCobolParams(detail string) []cobolParam {
	start := strings.Index(detail, "(")
	end := strings.Index(detail, ")")
	if start == -1 || end == -1 || end < start {
		return nil
	}
	list := detail[start+1 : end]
	parts := strings.Split(list, ",")
	params := make([]cobolParam, 0, len(parts))
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
		params = append(params, cobolParam{name: name, typ: parseCobolType(p)})
	}
	return params
}

type cobolParam struct {
	name string
	typ  string
}

func parseCobolSignature(detail string) ([]cobolParam, string) {
	params := parseCobolParams(detail)
	ret := ""
	lower := strings.ToLower(detail)
	if idx := strings.Index(lower, "returning"); idx != -1 {
		r := strings.TrimSpace(detail[idx+len("returning"):])
		r = strings.TrimSuffix(r, ".")
		if f := strings.Fields(r); len(f) > 0 {
			ret = mapCobolType(f[0])
			if ret == "" {
				ret = parseCobolType(r)
			}
		}
	}
	return params, ret
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
	case strings.Contains(t, "comp-5"), strings.Contains(t, "binary"):
		return "int"
	case strings.Contains(t, "comp-3"):
		return "int"
	case strings.Contains(t, "boolean"), strings.Contains(t, "bool"):
		return "bool"
	default:
		return ""
	}
}

func parseCobolType(detail string) string {
	lower := strings.ToLower(detail)
	if idx := strings.Index(lower, "pic"); idx != -1 {
		rest := strings.TrimSpace(detail[idx+3:])
		if f := strings.Fields(rest); len(f) > 0 {
			if t := mapCobolType(f[0]); t != "" {
				return t
			}
		}
	}
	if idx := strings.Index(lower, "usage is"); idx != -1 {
		rest := strings.TrimSpace(detail[idx+8:])
		if f := strings.Fields(rest); len(f) > 0 {
			if t := mapCobolType(f[0]); t != "" {
				return t
			}
		}
	}
	fields := strings.Fields(detail)
	if len(fields) > 0 {
		if t := mapCobolType(fields[0]); t != "" {
			return t
		}
	}
	return ""
}
