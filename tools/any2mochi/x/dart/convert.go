package dart

import (
	"fmt"
	"os"
	"strings"
	"unicode"

	a2m "mochi/tools/any2mochi"
)

// Convert converts dart source code to Mochi.
func Convert(src string) ([]byte, error) {
	funcs, err := parse(src)
	if err != nil {
		return nil, err
	}
	if len(funcs) == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	var out strings.Builder
	for _, f := range funcs {
		out.WriteString("fun ")
		out.WriteString(f.Name)
		out.WriteByte('(')
		for i, p := range f.Params {
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
		if f.Ret != "" {
			out.WriteString(": ")
			out.WriteString(f.Ret)
		}
		out.WriteString(" {\n")
		for _, line := range f.Body {
			out.WriteString(line)
			out.WriteByte('\n')
		}
		out.WriteString("}\n")
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the dart file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

func parseDetail(detail string) ([]param, string) {
	open := strings.Index(detail, "(")
	close := strings.LastIndex(detail, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := detail[open+1 : close]
	retPart := strings.TrimSpace(detail[close+1:])
	if i := strings.Index(retPart, "→"); i != -1 {
		retPart = strings.TrimSpace(retPart[i+len("→"):])
	} else if i := strings.Index(retPart, "->"); i != -1 {
		retPart = strings.TrimSpace(retPart[i+2:])
	}
	var params []param
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(strings.Trim(p, "{}[]"))
		if p == "" {
			continue
		}
		p = strings.TrimPrefix(p, "required ")
		if eq := strings.Index(p, "="); eq != -1 {
			p = p[:eq]
		}
		fields := strings.FieldsFunc(p, func(r rune) bool { return unicode.IsSpace(r) || r == ':' })
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		if strings.HasPrefix(name, "this.") {
			name = strings.TrimPrefix(name, "this.")
		}
		typ := ""
		if len(fields) > 1 {
			typ = toMochiType(strings.Join(fields[:len(fields)-1], " "))
		}
		params = append(params, param{name: name, typ: typ})
	}
	return params, toMochiType(retPart)
}

func parseHover(h a2m.Hover) ([]param, string) {
	text := hoverString(h)
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if strings.Contains(line, "(") && strings.Contains(line, ")") {
			if p, r := parseDetail(line); len(p) > 0 || r != "" {
				return p, r
			}
		}
	}
	return nil, ""
}

func writeSymbols(out *strings.Builder, prefix []string, syms []a2m.DocumentSymbol, src string, ls a2m.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}

		switch s.Kind {
		case a2m.SymbolKindClass, a2m.SymbolKindInterface, a2m.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == a2m.SymbolKindField || c.Kind == a2m.SymbolKindProperty || c.Kind == a2m.SymbolKindVariable {
					out.WriteString("  ")
					out.WriteString(c.Name)
					if t := fieldType(src, c, ls); t != "" {
						out.WriteString(": ")
						out.WriteString(t)
					}
					out.WriteByte('\n')
				}
			}
			out.WriteString("}\n")
			for _, c := range s.Children {
				if c.Kind == a2m.SymbolKindMethod || c.Kind == a2m.SymbolKindConstructor || c.Kind == a2m.SymbolKindFunction {
					writeSymbols(out, nameParts, []a2m.DocumentSymbol{c}, src, ls)
				}
			}
			continue
		case a2m.SymbolKindVariable, a2m.SymbolKindConstant:
			if len(prefix) == 0 && s.Name != "" {
				out.WriteString("let ")
				out.WriteString(s.Name)
				if t := fieldType(src, s, ls); t != "" {
					out.WriteString(": ")
					out.WriteString(t)
				}
				out.WriteByte('\n')
			}
		case a2m.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == a2m.SymbolKindEnumMember {
					fmt.Fprintf(out, "  %s\n", c.Name)
				}
			}
			out.WriteString("}\n")
			var rest []a2m.DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != a2m.SymbolKindEnumMember {
					rest = append(rest, c)
				}
			}
			if len(rest) > 0 {
				writeSymbols(out, nameParts, rest, src, ls)
			}
			continue
		case a2m.SymbolKindFunction, a2m.SymbolKindMethod, a2m.SymbolKindConstructor:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			detail := ""
			if s.Detail != nil {
				detail = *s.Detail
			}
			params, ret := parseDetail(detail)
			if len(params) == 0 && ret == "" {
				if hov, err := a2m.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					if p, r := parseHover(hov); len(p) > 0 || r != "" {
						params, ret = p, r
					}
				}
			}
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.name)
				if p.typ != "" && p.typ != "any" {
					out.WriteString(": ")
					out.WriteString(p.typ)
				}
			}
			out.WriteByte(')')
			if ret != "" && ret != "any" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			body := parseFunctionBody(src, s)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, line := range body {
					out.WriteString(line)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		}

		if len(s.Children) > 0 {
			writeSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func typeFromDetail(d *string) string {
	if d == nil {
		return ""
	}
	return toMochiType(strings.TrimSpace(*d))
}

func fieldType(src string, sym a2m.DocumentSymbol, ls a2m.LanguageServer) string {
	if t := typeFromDetail(sym.Detail); t != "" && t != "any" {
		return t
	}
	return typeFromHover(src, sym, ls)
}

func typeFromHover(src string, sym a2m.DocumentSymbol, ls a2m.LanguageServer) string {
	hov, err := a2m.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	text := hoverString(hov)
	for _, line := range strings.Split(text, "\n") {
		l := strings.TrimSpace(line)
		if idx := strings.Index(l, ":"); idx != -1 {
			t := strings.TrimSpace(l[idx+1:])
			if t != "" {
				return toMochiType(t)
			}
		}
		if idx := strings.Index(l, "→"); idx != -1 {
			t := strings.TrimSpace(l[idx+len("→"):])
			if t != "" {
				return toMochiType(t)
			}
		} else if idx := strings.Index(l, "->"); idx != -1 {
			t := strings.TrimSpace(l[idx+2:])
			if t != "" {
				return toMochiType(t)
			}
		}
	}
	return ""
}

func splitArgs(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '<':
			depth++
		case '>':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				parts = append(parts, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(s) {
		parts = append(parts, strings.TrimSpace(s[start:]))
	}
	return parts
}

func toMochiType(t string) string {
	t = strings.TrimSpace(t)
	if strings.HasSuffix(t, "?") {
		t = strings.TrimSuffix(t, "?")
	}
	switch t {
	case "", "dynamic", "Object":
		return "any"
	case "int":
		return "int"
	case "double", "num":
		return "float"
	case "bool":
		return "bool"
	case "String":
		return "string"
	case "void":
		return ""
	}
	if strings.HasSuffix(t, ">") {
		if open := strings.Index(t, "<"); open != -1 {
			outer := strings.TrimSpace(t[:open])
			inner := strings.TrimSuffix(t[open+1:], ">")
			args := splitArgs(inner)
			switch outer {
			case "List", "Iterable", "Set":
				a := "any"
				if len(args) > 0 {
					if at := toMochiType(args[0]); at != "" {
						a = at
					}
				}
				return "list<" + a + ">"
			case "Map":
				if len(args) == 2 {
					k := toMochiType(args[0])
					if k == "" {
						k = "any"
					}
					v := toMochiType(args[1])
					if v == "" {
						v = "any"
					}
					return "map<" + k + ", " + v + ">"
				}
			case "Future":
				if len(args) == 1 {
					return toMochiType(args[0])
				}
			}
		}
	}
	return t
}

// parseFunctionBody extracts the body of a Dart function symbol.
// It performs a very lightweight parsing and returns each line in Mochi
// syntax. The implementation is intentionally simple and only handles
// a small subset of statements.
func parseFunctionBody(src string, sym a2m.DocumentSymbol) []string {
	code := extractRangeText(src, sym.Range)
	start := strings.Index(code, "{")
	end := strings.LastIndex(code, "}")
	if start == -1 || end == -1 || end <= start {
		return nil
	}
	body := code[start+1 : end]
	return parseStatements(body)
}
