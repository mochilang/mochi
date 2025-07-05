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
			body := cFunctionBody(src, s)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, ln := range body {
					out.WriteString(ln)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
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
			isUnion := false
			if s.Detail != nil && strings.TrimSpace(*s.Detail) == "union" {
				isUnion = true
			}
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
			if isUnion {
				out.WriteString(" union {\n")
			} else {
				out.WriteString(" {\n")
			}
			for _, c := range s.Children {
				if c.Kind != protocol.SymbolKindField {
					continue
				}
				out.WriteString("  ")
				out.WriteString(c.Name)
				raw := ""
				if c.Detail != nil {
					raw = strings.TrimSpace(*c.Detail)
				}
				typ := mapCType(raw)
				if typ == "" {
					typ = cHoverFieldType(src, c, ls)
				}
				if strings.HasPrefix(s.Name, "list_") && c.Name == "data" {
					inner := strings.TrimPrefix(s.Name, "list_")
					if t := mapCType(inner); t != "" {
						typ = "list<" + t + ">"
					}
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

func cFunctionBody(src string, sym protocol.DocumentSymbol) []string {
	lines := strings.Split(src, "\n")
	start := cPosToOffset(lines, sym.Range.Start)
	end := cPosToOffset(lines, sym.Range.End)
	if start >= len(src) || end > len(src) || start >= end {
		return nil
	}
	snippet := src[start:end]
	open := strings.Index(snippet, "{")
	close := strings.LastIndex(snippet, "}")
	if open == -1 || close == -1 || close <= open {
		return nil
	}
	body := snippet[open+1 : close]
	return parseCStatements(body)
}

func parseCStatements(body string) []string {
	var out []string
	indent := 1
	for _, line := range strings.Split(body, "\n") {
		l := strings.TrimSpace(line)
		if l == "" {
			continue
		}
		switch {
		case l == "{":
			out = append(out, strings.Repeat("  ", indent)+"{")
			indent++
		case l == "}":
			indent--
			out = append(out, strings.Repeat("  ", indent)+"}")
		case strings.HasPrefix(l, "for ") || strings.HasPrefix(l, "for("):
			if strings.HasSuffix(l, "{") {
				h := strings.TrimSpace(strings.TrimSuffix(l, "{"))
				out = append(out, strings.Repeat("  ", indent)+h+" {")
				indent++
			} else {
				out = append(out, strings.Repeat("  ", indent)+l)
			}
		case strings.HasPrefix(l, "while ") || strings.HasPrefix(l, "while("):
			if strings.HasSuffix(l, "{") {
				h := strings.TrimSpace(strings.TrimSuffix(l, "{"))
				h = strings.TrimPrefix(h, "while")
				h = strings.TrimSpace(h)
				for strings.HasPrefix(h, "(") && strings.HasSuffix(h, ")") {
					h = strings.TrimPrefix(h, "(")
					h = strings.TrimSuffix(h, ")")
					h = strings.TrimSpace(h)
				}
				out = append(out, strings.Repeat("  ", indent)+"while "+h+" {")
				indent++
			} else {
				out = append(out, strings.Repeat("  ", indent)+l)
			}
		case strings.HasPrefix(l, "if ") || strings.HasPrefix(l, "if("):
			if strings.HasSuffix(l, "{") {
				h := strings.TrimSpace(strings.TrimSuffix(l, "{"))
				h = strings.TrimPrefix(h, "if")
				h = strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(h, "("), ")"))
				out = append(out, strings.Repeat("  ", indent)+"if "+h+" {")
				indent++
			} else {
				out = append(out, strings.Repeat("  ", indent)+l)
			}
		case strings.HasPrefix(l, "else {"):
			indent--
			out = append(out, strings.Repeat("  ", indent)+"else {")
			indent++
		case strings.HasPrefix(l, "return "):
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "return "), ";")
			out = append(out, strings.Repeat("  ", indent)+"return "+expr)
		case l == "continue;":
			out = append(out, strings.Repeat("  ", indent)+"continue")
		case l == "break;":
			out = append(out, strings.Repeat("  ", indent)+"break")
		case strings.HasPrefix(l, "printf("):
			args := strings.TrimSuffix(strings.TrimPrefix(l, "printf("), ");")
			parts := strings.SplitN(args, ",", 2)
			arg := strings.TrimSpace(args)
			if len(parts) == 2 {
				arg = strings.TrimSpace(parts[1])
			}
			out = append(out, strings.Repeat("  ", indent)+"print("+arg+")")
		default:
			if strings.HasSuffix(l, ";") {
				l = strings.TrimSuffix(l, ";")
			}
			switch {
			case strings.HasSuffix(l, "++"):
				v := strings.TrimSpace(strings.TrimSuffix(l, "++"))
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" + 1")
			case strings.HasSuffix(l, "--"):
				v := strings.TrimSpace(strings.TrimSuffix(l, "--"))
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" - 1")
			case strings.Contains(l, "+="):
				parts := strings.SplitN(l, "+=", 2)
				v := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" + "+val)
			case strings.Contains(l, "-="):
				parts := strings.SplitN(l, "-=", 2)
				v := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" - "+val)
			case strings.Contains(l, "*="):
				parts := strings.SplitN(l, "*=", 2)
				v := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" * "+val)
			case strings.Contains(l, "/="):
				parts := strings.SplitN(l, "/=", 2)
				v := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" / "+val)
			case strings.Contains(l, "%="):
				parts := strings.SplitN(l, "%=", 2)
				v := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" % "+val)
			case strings.HasPrefix(l, "int "):
				out = append(out, strings.Repeat("  ", indent)+"var "+strings.TrimSpace(l[4:]))
			case strings.HasPrefix(l, "float "):
				out = append(out, strings.Repeat("  ", indent)+"var "+strings.TrimSpace(l[6:]))
			case strings.HasPrefix(l, "double "):
				out = append(out, strings.Repeat("  ", indent)+"var "+strings.TrimSpace(l[7:]))
			case strings.HasPrefix(l, "char "):
				out = append(out, strings.Repeat("  ", indent)+"var "+strings.TrimSpace(l[5:]))
			default:
				out = append(out, strings.Repeat("  ", indent)+l)
			}
		}
	}
	return out
}

func cPosToOffset(lines []string, pos protocol.Position) int {
	off := 0
	for i := 0; i < int(pos.Line); i++ {
		if i < len(lines) {
			off += len(lines[i]) + 1
		}
	}
	off += int(pos.Character)
	return off
}
