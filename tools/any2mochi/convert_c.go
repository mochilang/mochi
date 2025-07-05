package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertC converts c source code to Mochi using the language server.
func ConvertC(src string) ([]byte, error) {
	ls := Servers["c"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	aliasForRange := make(map[protocol.Range]string)
	for _, s := range syms {
		if s.Detail != nil && *s.Detail == "type alias" {
			aliasForRange[s.Range] = s.Name
		}
	}

	var out strings.Builder
	matched := false
	for _, s := range syms {
		switch s.Kind {
		case protocol.SymbolKindFunction:
			matched = true
			ret, params := cHoverSignature(src, s, ls)

			out.WriteString("fun ")
			out.WriteString(s.Name)
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				name := p.name
				if name == "" {
					name = fmt.Sprintf("a%d", i)
				}
				if p.typ != "" {
					out.WriteString(name)
					out.WriteString(": ")
					out.WriteString(p.typ)
				} else {
					out.WriteString(name)
				}
			}
			out.WriteByte(')')
			if ret != "" && ret != "void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			matched = true
			out.WriteString("let ")
			out.WriteString(s.Name)
			typ := ""
			if s.Detail != nil {
				typ = mapCType(*s.Detail)
			}
			if typ == "" {
				typ = cHoverFieldType(src, s, ls)
			}
			if typ != "" {
				out.WriteString(": ")
				out.WriteString(typ)
			}
			out.WriteByte('\n')
		case protocol.SymbolKindStruct, protocol.SymbolKindClass:
			matched = true
			if len(s.Children) == 0 {
				continue
			}
			name := s.Name
			if name == "(anonymous struct)" {
				for r, alias := range aliasForRange {
					if r.Start.Line <= s.Range.Start.Line && r.End.Line >= s.Range.End.Line {
						name = alias
						break
					}
				}
			}
			if name == "" {
				name = s.Name
			}
			out.WriteString("type ")
			out.WriteString(name)
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind != protocol.SymbolKindField {
					continue
				}
				out.WriteString("  ")
				out.WriteString(c.Name)
				typ := ""
				if c.Detail != nil {
					typ = mapCType(*c.Detail)
				}
				if typ == "" {
					typ = cHoverFieldType(src, c, ls)
				}
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case protocol.SymbolKindEnum:
			matched = true
			out.WriteString("type ")
			out.WriteString(s.Name)
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindEnumMember {
					fmt.Fprintf(&out, "  %s\n", c.Name)
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
				// recurse to handle any nested symbols
				syms = append(syms, rest...)
			}
		}
	}

	if !matched {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertCFile reads the c file and converts it to Mochi.
func ConvertCFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertC(string(data))
}

type cParam struct {
	name string
	typ  string
}

func parseCSignature(detail *string) (string, []cParam) {
	if detail == nil {
		return "", nil
	}
	sig := strings.TrimSpace(*detail)
	if sig == "" {
		return "", nil
	}
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	if open < 0 || close < open {
		return mapCType(sig), nil
	}
	header := strings.TrimSpace(sig[:open])
	ret := ""
	if parts := strings.Fields(header); len(parts) > 1 {
		ret = mapCType(strings.Join(parts[:len(parts)-1], " "))
	} else {
		ret = mapCType(header)
	}
	paramsPart := strings.TrimSpace(sig[open+1 : close])
	if paramsPart == "" || paramsPart == "void" {
		return ret, nil
	}
	parts := strings.Split(paramsPart, ",")
	out := make([]cParam, 0, len(parts))
	for _, p := range parts {
		name, typ := parseCParam(strings.TrimSpace(p))
		out = append(out, cParam{name: name, typ: typ})
	}
	return ret, out
}

func parseCParam(p string) (string, string) {
	if p == "" {
		return "", ""
	}
	if p == "..." {
		return "", ""
	}
	fields := strings.Fields(p)
	if len(fields) == 1 {
		return "", mapCType(fields[0])
	}
	name := fields[len(fields)-1]
	typ := strings.Join(fields[:len(fields)-1], " ")
	if strings.ContainsAny(name, "*[]") || strings.Contains(name, "[") {
		// likely part of the type
		return "", mapCType(p)
	}
	return name, mapCType(typ)
}

func mapCType(typ string) string {
	typ = strings.TrimSpace(typ)
	if open := strings.Index(typ, "["); open != -1 && strings.HasSuffix(typ, "]") {
		base := strings.TrimSpace(typ[:open])
		inner := mapCType(base)
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	for strings.HasSuffix(typ, "*") {
		typ = strings.TrimSpace(strings.TrimSuffix(typ, "*"))
	}
	typ = strings.TrimPrefix(typ, "static")
	typ = strings.TrimPrefix(strings.TrimSpace(typ), "const")
	typ = strings.TrimPrefix(strings.TrimSpace(typ), "unsigned")
	typ = strings.TrimPrefix(strings.TrimSpace(typ), "signed")
	typ = strings.TrimPrefix(strings.TrimSpace(typ), "struct")
	typ = strings.TrimSpace(typ)
	switch typ {
	case "", "void":
		return ""
	case "bool":
		return "bool"
	case "int", "size_t", "long", "short":
		return "int"
	case "long long":
		return "int"
	case "float", "double":
		return "float"
	case "char":
		return "string"
	default:
		if strings.HasPrefix(typ, "list_") {
			inner := mapCType(strings.TrimPrefix(typ, "list_"))
			if inner == "" {
				inner = "any"
			}
			return "list<" + inner + ">"
		}
		if strings.HasPrefix(typ, "map_") {
			inner := strings.TrimPrefix(typ, "map_")
			parts := strings.SplitN(inner, "_", 2)
			if len(parts) == 2 {
				k := mapCType(parts[0])
				v := mapCType(parts[1])
				if k == "" {
					k = "any"
				}
				if v == "" {
					v = "any"
				}
				return "map<" + k + ", " + v + ">"
			}
		}
		return typ
	}
}

// cHoverSignature obtains the function signature via hover information.
// It returns the return type and parameter list. If hover data is unavailable
// it falls back to the symbol detail.
func cHoverSignature(src string, sym protocol.DocumentSymbol, ls LanguageServer) (string, []cParam) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err == nil {
		if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
			for _, line := range strings.Split(mc.Value, "\n") {
				l := strings.TrimSpace(line)
				if strings.Contains(l, "(") && strings.Contains(l, ")") {
					return parseCSignature(&l)
				}
			}
		}
	}
	return parseCSignature(sym.Detail)
}

// cHoverFieldType retrieves the type of a symbol using hover information.
func cHoverFieldType(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
		for _, line := range strings.Split(mc.Value, "\n") {
			l := strings.TrimSpace(line)
			if l != "" {
				return mapCType(l)
			}
		}
	}
	return ""
}
