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

type csParam struct {
	name string
	typ  string
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
					typ := csFieldType(src, c, ls)
					if typ == "" {
						typ = "any"
					}
					fmt.Fprintf(out, "  %s: %s\n", c.Name, typ)
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
			if len(params) == 0 || ret == "" {
				if p, r := csHoverSignature(src, s, ls); len(p) > 0 || r != "" {
					if len(params) == 0 {
						params = p
					}
					if ret == "" {
						ret = r
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
				out.WriteString(p.name)
				if p.typ != "" {
					out.WriteString(": ")
					out.WriteString(p.typ)
				}
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
				if typ := csFieldType(src, s, ls); typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
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

func parseCsSignature(detail *string) ([]csParam, string) {
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
	var params []csParam
	if paramsPart != "" {
		rawParams := splitArgs(paramsPart)
		for _, p := range rawParams {
			typ, name := parseCsParam(p)
			if name != "" {
				params = append(params, csParam{name: name, typ: typ})
			}
		}
	}
	return params, ret
}

func parseCsParam(p string) (string, string) {
	p = strings.TrimSpace(p)
	if p == "" {
		return "", ""
	}
	if eq := strings.Index(p, "="); eq != -1 {
		p = strings.TrimSpace(p[:eq])
	}
	for {
		switch {
		case strings.HasPrefix(p, "ref "):
			p = strings.TrimSpace(strings.TrimPrefix(p, "ref "))
		case strings.HasPrefix(p, "out "):
			p = strings.TrimSpace(strings.TrimPrefix(p, "out "))
		case strings.HasPrefix(p, "in "):
			p = strings.TrimSpace(strings.TrimPrefix(p, "in "))
		case strings.HasPrefix(p, "params "):
			p = strings.TrimSpace(strings.TrimPrefix(p, "params "))
		case strings.HasPrefix(p, "this "):
			p = strings.TrimSpace(strings.TrimPrefix(p, "this "))
		default:
			goto done
		}
	}
done:
	for strings.HasPrefix(p, "[") {
		if idx := strings.Index(p, "]"); idx != -1 {
			p = strings.TrimSpace(p[idx+1:])
		} else {
			break
		}
	}
	typ, name := splitTypeName(p)
	name = strings.Trim(name, "*&[]")
	typ = mapCsType(typ)
	return typ, name
}

func splitTypeName(s string) (string, string) {
	depth := 0
	for i := len(s) - 1; i >= 0; i-- {
		switch s[i] {
		case '>':
			depth++
		case '<':
			depth--
		case ' ':
			if depth == 0 {
				return strings.TrimSpace(s[:i]), strings.TrimSpace(s[i+1:])
			}
		}
	}
	return "", strings.TrimSpace(s)
}

func mapCsType(t string) string {
	t = strings.TrimSpace(t)
	for strings.HasSuffix(t, "[]") {
		inner := mapCsType(strings.TrimSuffix(t, "[]"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if idx := strings.LastIndex(t, "."); idx != -1 {
		t = t[idx+1:]
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
	}
	if strings.HasSuffix(t, ">") {
		if open := strings.Index(t, "<"); open != -1 {
			outer := t[:open]
			inner := t[open+1 : len(t)-1]
			args := splitArgs(inner)
			switch outer {
			case "List", "IEnumerable", "IList", "ICollection", "IReadOnlyList":
				a := "any"
				if len(args) > 0 {
					if at := mapCsType(args[0]); at != "" {
						a = at
					}
				}
				return "list<" + a + ">"
			case "Dictionary", "IDictionary":
				if len(args) == 2 {
					k := mapCsType(args[0])
					if k == "" {
						k = "any"
					}
					v := mapCsType(args[1])
					if v == "" {
						v = "any"
					}
					return "map<" + k + ", " + v + ">"
				}
			case "Nullable":
				if len(args) == 1 {
					return mapCsType(args[0])
				}
			}
		}
	}
	return ""
}

func csHoverSignature(src string, sym protocol.DocumentSymbol, ls LanguageServer) ([]csParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
		for _, line := range strings.Split(mc.Value, "\n") {
			l := strings.TrimSpace(line)
			if strings.Contains(l, "(") && strings.Contains(l, ")") {
				if p, r := parseCsSignature(&l); len(p) > 0 || r != "" {
					return p, r
				}
			}
		}
	}
	return nil, ""
}

func csFieldType(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	if sym.Detail != nil {
		if t := mapCsType(*sym.Detail); t != "" {
			return t
		}
	}
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err == nil {
		if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
			for _, line := range strings.Split(mc.Value, "\n") {
				l := strings.TrimSpace(line)
				fields := strings.Fields(l)
				if len(fields) >= 2 {
					if t := mapCsType(fields[0]); t != "" {
						return t
					}
					if t := mapCsType(fields[len(fields)-1]); t != "" {
						return t
					}
				}
				if idx := strings.Index(l, ":"); idx != -1 {
					if t := mapCsType(strings.TrimSpace(l[idx+1:])); t != "" {
						return t
					}
				}
			}
		}
	}
	defs, err := EnsureAndDefinition(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err == nil && len(defs) > 0 {
		pos := defs[0].Range.Start
		hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
		if err == nil {
			if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
				for _, line := range strings.Split(mc.Value, "\n") {
					l := strings.TrimSpace(line)
					if t := mapCsType(l); t != "" {
						return t
					}
				}
			}
		}
	}
	return ""
}
