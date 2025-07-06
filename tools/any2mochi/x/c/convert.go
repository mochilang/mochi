package c

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	any2mochi "mochi/tools/any2mochi"
)

// Convert translates C source code to Mochi using the language server.
func Convert(src string) ([]byte, error) {
	if !any2mochi.UseLSP {
		return convertSimple(src)
	}

	ls := any2mochi.Servers["c"]
	syms, diags, err := any2mochi.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	aliasForRange := make(map[any2mochi.Range]string)
	for _, s := range syms {
		if s.Detail != nil && *s.Detail == "type alias" {
			aliasForRange[s.Range] = s.Name
		}
	}

	var out strings.Builder
	matched := false
	for _, s := range syms {
		switch s.Kind {
		case any2mochi.SymbolKindFunction:
			matched = true
			ret, params := hoverSignature(src, s, ls)

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
			body := functionBody(src, s)
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
		case any2mochi.SymbolKindVariable, any2mochi.SymbolKindConstant:
			matched = true
			out.WriteString("let ")
			out.WriteString(s.Name)
			typ := ""
			if s.Detail != nil {
				typ = mapType(*s.Detail)
			}
			if typ == "" {
				typ = hoverFieldType(src, s, ls)
			}
			if typ != "" {
				out.WriteString(": ")
				out.WriteString(typ)
			}
			out.WriteByte('\n')
		case any2mochi.SymbolKindStruct, any2mochi.SymbolKindClass:
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
				if c.Kind != any2mochi.SymbolKindField {
					continue
				}
				out.WriteString("  ")
				out.WriteString(c.Name)
				raw := ""
				if c.Detail != nil {
					raw = strings.TrimSpace(*c.Detail)
				}
				typ := mapType(raw)
				if typ == "" {
					typ = hoverFieldType(src, c, ls)
				}
				if strings.HasPrefix(s.Name, "list_") && c.Name == "data" {
					inner := strings.TrimPrefix(s.Name, "list_")
					if t := mapType(inner); t != "" {
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
		case any2mochi.SymbolKindEnum:
			matched = true
			out.WriteString("type ")
			out.WriteString(s.Name)
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == any2mochi.SymbolKindEnumMember {
					fmt.Fprintf(&out, "  %s\n", c.Name)
				}
			}
			out.WriteString("}\n")
			var rest []any2mochi.DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != any2mochi.SymbolKindEnumMember {
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

// ConvertFile reads the C file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

// convertSimple converts C source code without using a language server.
// It invokes clang to obtain a JSON AST and extracts top-level functions.
func convertSimple(src string) ([]byte, error) {
	funcs, err := parseFileClang(src)
	if err != nil {
		return nil, err
	}
	if len(funcs) == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	var out strings.Builder
	for _, fn := range funcs {
		if fn.name == "main" {
			for _, ln := range fn.body {
				trimmed := strings.TrimSpace(ln)
				if strings.HasPrefix(trimmed, "return ") || trimmed == "return" {
					continue
				}
				out.WriteString(trimmed)
				out.WriteByte('\n')
			}
			continue
		}
		out.WriteString("fun ")
		out.WriteString(fn.name)
		out.WriteByte('(')
		for i, p := range fn.params {
			if i > 0 {
				out.WriteString(", ")
			}
			name := p.name
			if name == "" {
				name = fmt.Sprintf("a%d", i)
			}
			out.WriteString(name)
			if p.typ != "" {
				out.WriteString(": ")
				out.WriteString(p.typ)
			}
		}
		out.WriteByte(')')
		if fn.ret != "" && fn.ret != "void" {
			out.WriteString(": ")
			out.WriteString(fn.ret)
		}
		if len(fn.body) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, ln := range fn.body {
				out.WriteString(ln)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		}
	}
	return []byte(out.String()), nil
}

type param struct {
	name string
	typ  string
}

var castRE = regexp.MustCompile(`\([a-zA-Z_][a-zA-Z0-9_\s]*\*\)`) // matches C casts like (int *)

func stripCasts(s string) string {
	return castRE.ReplaceAllString(s, "")
}

func parseSignature(detail *string) (string, []param) {
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
		return mapType(sig), nil
	}
	header := strings.TrimSpace(sig[:open])
	ret := ""
	if parts := strings.Fields(header); len(parts) > 1 {
		ret = mapType(strings.Join(parts[:len(parts)-1], " "))
	} else {
		ret = mapType(header)
	}
	paramsPart := strings.TrimSpace(sig[open+1 : close])
	if paramsPart == "" || paramsPart == "void" {
		return ret, nil
	}
	parts := strings.Split(paramsPart, ",")
	out := make([]param, 0, len(parts))
	for _, p := range parts {
		name, typ := parseParam(strings.TrimSpace(p))
		out = append(out, param{name: name, typ: typ})
	}
	return ret, out
}

func parseParam(p string) (string, string) {
	if p == "" {
		return "", ""
	}
	if p == "..." {
		return "", ""
	}
	fields := strings.Fields(p)
	if len(fields) == 1 {
		return "", mapType(fields[0])
	}
	name := fields[len(fields)-1]
	typ := strings.Join(fields[:len(fields)-1], " ")
	if strings.ContainsAny(name, "*[]") || strings.Contains(name, "[") {
		// likely part of the type
		return "", mapType(p)
	}
	return name, mapType(typ)
}

func mapType(typ string) string {
	typ = strings.TrimSpace(typ)
	if open := strings.Index(typ, "["); open != -1 && strings.HasSuffix(typ, "]") {
		base := strings.TrimSpace(typ[:open])
		inner := mapType(base)
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
			inner := mapType(strings.TrimPrefix(typ, "list_"))
			if inner == "" {
				inner = "any"
			}
			return "list<" + inner + ">"
		}
		if strings.HasPrefix(typ, "map_") {
			inner := strings.TrimPrefix(typ, "map_")
			parts := strings.SplitN(inner, "_", 2)
			if len(parts) == 2 {
				k := mapType(parts[0])
				v := mapType(parts[1])
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

// hoverSignature obtains the function signature via hover information.
// It returns the return type and parameter list. If hover data is unavailable
// it falls back to the symbol detail.
func hoverSignature(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) (string, []param) {
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err == nil {
		if mc, ok := hov.Contents.(any2mochi.MarkupContent); ok {
			for _, line := range strings.Split(mc.Value, "\n") {
				l := strings.TrimSpace(line)
				if strings.Contains(l, "(") && strings.Contains(l, ")") {
					return parseSignature(&l)
				}
			}
		}
	}
	return parseSignature(sym.Detail)
}

// hoverFieldType retrieves the type of a symbol using hover information.
func hoverFieldType(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) string {
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(any2mochi.MarkupContent); ok {
		for _, line := range strings.Split(mc.Value, "\n") {
			l := strings.TrimSpace(line)
			if l != "" {
				return mapType(l)
			}
		}
	}
	return ""
}

func functionBody(src string, sym any2mochi.DocumentSymbol) []string {
	lines := strings.Split(src, "\n")
	start := posToOffset(lines, sym.Range.Start)
	end := posToOffset(lines, sym.Range.End)
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
	return parseStatements(body)
}

func parseStatements(body string) []string {
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
			l = stripCasts(l)
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

func posToOffset(lines []string, pos any2mochi.Position) int {
	off := 0
	for i := 0; i < int(pos.Line); i++ {
		if i < len(lines) {
			off += len(lines[i]) + 1
		}
	}
	off += int(pos.Character)
	return off
}
