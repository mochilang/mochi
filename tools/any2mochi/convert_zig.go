package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

type zigParam struct {
	name string
	typ  string
}

// ConvertZig converts zig source code to Mochi using the language server.
func ConvertZig(src string) ([]byte, error) {
	ls := Servers["zig"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeZigSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertZigFile reads the zig file and converts it to Mochi.
func ConvertZigFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertZig(string(data))
}

func parseZigParams(list string) []zigParam {
	parts := strings.Split(list, ",")
	params := make([]zigParam, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		name := p
		typ := ""
		if colon := strings.Index(p, ":"); colon != -1 {
			name = strings.TrimSpace(p[:colon])
			typ = strings.TrimSpace(p[colon+1:])
			typ = mapZigType(typ)
		}
		if strings.HasSuffix(name, ":") {
			name = strings.TrimSuffix(name, ":")
		}
		params = append(params, zigParam{name: name, typ: typ})
	}
	return params
}

func parseZigDetail(detail string) ([]zigParam, string) {
	start := strings.Index(detail, "(")
	end := strings.LastIndex(detail, ")")
	if start == -1 || end == -1 || end < start {
		return nil, ""
	}
	paramsPart := detail[start+1 : end]
	retPart := strings.TrimSpace(detail[end+1:])
	params := parseZigParams(paramsPart)
	return params, mapZigType(retPart)
}

func zigHoverSignature(src string, sym protocol.DocumentSymbol, ls LanguageServer) ([]zigParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
		lines := strings.Split(mc.Value, "\n")
		for i := len(lines) - 1; i >= 0; i-- {
			l := strings.TrimSpace(lines[i])
			if strings.HasPrefix(l, "fn") && strings.Contains(l, "(") && strings.Contains(l, ")") {
				return parseZigDetail(l)
			}
		}
	}
	return nil, ""
}

func mapZigType(t string) string {
	t = strings.TrimSpace(t)
	if t == "" || t == "void" {
		return ""
	}
	switch t {
	case "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "usize", "isize":
		return "int"
	case "f16", "f32", "f64":
		return "float"
	case "bool":
		return "bool"
	}
	if strings.HasPrefix(t, "[]const u8") || strings.HasPrefix(t, "[]u8") {
		return "string"
	}
	if strings.HasPrefix(t, "[]") {
		inner := mapZigType(strings.TrimPrefix(t, "[]"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return t
}

func writeZigSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction:
			detail := ""
			if s.Detail != nil {
				detail = *s.Detail
			}
			params, ret := parseZigDetail(detail)
			if len(params) == 0 {
				p, r := zigHoverSignature(src, s, ls)
				if len(p) > 0 {
					params = p
				}
				if r != "" {
					ret = r
				}
			}
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
		case protocol.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			if len(s.Children) == 0 {
				out.WriteString(" {}\n")
				break
			}
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind != protocol.SymbolKindField {
					continue
				}
				out.WriteString("  ")
				out.WriteString(c.Name)
				if c.Detail != nil {
					t := mapZigType(strings.TrimSpace(*c.Detail))
					if t != "" {
						out.WriteString(": ")
						out.WriteString(t)
					}
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			out.WriteString("let ")
			out.WriteString(strings.Join(nameParts, "."))
			if s.Detail != nil {
				t := mapZigType(strings.TrimSpace(*s.Detail))
				if t != "" {
					out.WriteString(": ")
					out.WriteString(t)
				}
			}
			out.WriteByte('\n')
		case protocol.SymbolKindNamespace, protocol.SymbolKindPackage, protocol.SymbolKindModule:
			writeZigSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}
