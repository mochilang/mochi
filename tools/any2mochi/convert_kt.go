package any2mochi

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertKt converts Kotlin source code to Mochi using the language server. The
// converter relies solely on LSP information and does not parse the source
// directly.
func ConvertKt(src string) ([]byte, error) {
	ls := Servers["kt"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		if strings.Contains(err.Error(), "not found") {
			return convertKtFallback(src)
		}
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeKtSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertKtFile reads the kt file and converts it to Mochi.
func ConvertKtFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertKt(string(data))
}

func writeKtSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindClass, protocol.SymbolKindStruct, protocol.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []protocol.DocumentSymbol{}
			methods := []protocol.DocumentSymbol{}
			for _, c := range s.Children {
				switch c.Kind {
				case protocol.SymbolKindField, protocol.SymbolKindProperty:
					fields = append(fields, c)
				case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
					methods = append(methods, c)
				}
			}
			if len(fields) == 0 && len(methods) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, f := range fields {
					out.WriteString("  ")
					out.WriteString(f.Name)
					if typ := ktFieldType(src, f, ls); typ != "" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
				for _, m := range methods {
					var b strings.Builder
					writeKtFunc(&b, m, src, ls, strings.Join(nameParts, ".")+"."+m.Name)
					for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
						out.WriteString("  ")
						out.WriteString(line)
						out.WriteByte('\n')
					}
				}
				out.WriteString("}\n")
			}
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			writeKtFunc(out, s, src, ls, strings.Join(nameParts, "."))
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant, protocol.SymbolKindField, protocol.SymbolKindProperty:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				if typ := ktFieldType(src, s, ls); typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
			if len(s.Children) > 0 {
				writeKtSymbols(out, nameParts, s.Children, src, ls)
			}
		default:
			if len(s.Children) > 0 {
				writeKtSymbols(out, nameParts, s.Children, src, ls)
			}
		}
	}
}

func writeKtFunc(out *strings.Builder, sym protocol.DocumentSymbol, src string, ls LanguageServer, name string) {
	params, ret := parseKtSignature(sym.Detail)
	if len(params) == 0 && ret == "" {
		p, r := ktHoverSignature(src, sym, ls)
		params, ret = p, r
	}
	out.WriteString("fun ")
	out.WriteString(name)
	out.WriteByte('(')
	for i, p := range params {
		if i > 0 {
			out.WriteString(", ")
		}
		out.WriteString(p.name)
		if p.typ != "" {
			out.WriteString(": ")
			out.WriteString(mapKtType(p.typ))
		}
	}
	out.WriteByte(')')
	if ret != "" && ret != "Unit" {
		out.WriteString(": ")
		out.WriteString(mapKtType(ret))
	}
	out.WriteString(" {}\n")
}

func ktHoverSignature(src string, sym protocol.DocumentSymbol, ls LanguageServer) ([]ktParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
		for _, line := range strings.Split(mc.Value, "\n") {
			l := strings.TrimSpace(strings.Trim(line, "`"))
			if strings.HasPrefix(l, "fun ") || strings.HasPrefix(l, "suspend fun ") {
				return parseKtSignature(&l)
			}
		}
	}
	return nil, ""
}

func parseKtSignature(detail *string) ([]ktParam, string) {
	if detail == nil {
		return nil, ""
	}
	sig := strings.TrimSpace(*detail)
	if sig == "" {
		return nil, ""
	}
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := sig[open+1 : close]
	params := parseKtParams(paramsPart)
	rest := strings.TrimSpace(sig[close+1:])
	ret := ""
	if strings.HasPrefix(rest, ":") {
		ret = strings.TrimSpace(rest[1:])
	}
	return params, ret
}

type ktParam struct{ name, typ string }

func parseKtParams(s string) []ktParam {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '<', '(', '[':
			depth++
		case '>', ')', ']':
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
	params := make([]ktParam, 0, len(parts))
	for _, p := range parts {
		if eq := strings.Index(p, "="); eq != -1 {
			p = strings.TrimSpace(p[:eq])
		}
		name := p
		typ := ""
		if colon := strings.Index(p, ":"); colon != -1 {
			name = strings.TrimSpace(p[:colon])
			typ = strings.TrimSpace(p[colon+1:])
		} else if fields := strings.Fields(p); len(fields) > 0 {
			name = fields[len(fields)-1]
		}
		if name != "" {
			params = append(params, ktParam{name: name, typ: typ})
		}
	}
	return params
}

func ktFieldType(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
		for _, line := range strings.Split(mc.Value, "\n") {
			l := strings.TrimSpace(line)
			if idx := strings.Index(l, ":"); idx != -1 {
				typ := strings.TrimSpace(l[idx+1:])
				if typ != "" {
					return mapKtType(typ)
				}
			}
		}
	}
	return ""
}

func mapKtType(t string) string {
	t = strings.TrimSpace(t)
	if strings.HasSuffix(t, "?") {
		t = strings.TrimSuffix(t, "?")
	}
	switch t {
	case "", "Unit", "Nothing":
		return ""
	case "Int", "Long", "Short", "Byte":
		return "int"
	case "Float", "Double":
		return "float"
	case "Boolean":
		return "bool"
	case "String", "Char":
		return "string"
	}
	if strings.HasPrefix(t, "List<") && strings.HasSuffix(t, ">") {
		inner := mapKtType(t[5 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Array<") && strings.HasSuffix(t, ">") {
		inner := mapKtType(t[6 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Map<") && strings.HasSuffix(t, ">") {
		inner := t[4 : len(t)-1]
		parts := splitGeneric(inner)
		key := "any"
		val := "any"
		if len(parts) == 2 {
			if k := mapKtType(parts[0]); k != "" {
				key = k
			}
			if v := mapKtType(parts[1]); v != "" {
				val = v
			}
		}
		return "map<" + key + ", " + val + ">"
	}
	return t
}

func splitGeneric(s string) []string {
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

// convertKtFallback performs a best-effort conversion when the Kotlin language server
// is unavailable. It uses simple regex parsing to translate function declarations
// and a limited set of statements.
func convertKtFallback(src string) ([]byte, error) {
	sc := bufio.NewScanner(strings.NewReader(src))
	var out strings.Builder
	indent := 0
	ind := func() string { return strings.Repeat("  ", indent) }

	funRE := regexp.MustCompile(`^fun\s+([A-Za-z0-9_]+)\s*\(([^)]*)\)`)
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" {
			continue
		}
		switch {
		case funRE.MatchString(line):
			m := funRE.FindStringSubmatch(line)
			name, params := m[1], m[2]
			out.WriteString(ind())
			out.WriteString("fun ")
			out.WriteString(name)
			if params != "" {
				out.WriteByte('(')
				out.WriteString(strings.Join(parseSimpleParams(params), ", "))
				out.WriteByte(')')
			} else {
				out.WriteString("()")
			}
			out.WriteString(" {\n")
			indent++
		case line == "}":
			if indent > 0 {
				indent--
			}
			out.WriteString(ind())
			out.WriteString("}\n")
		case strings.HasPrefix(line, "for (") && strings.Contains(line, " in "):
			r := regexp.MustCompile(`for\s*\(([^ ]+)\s+in\s+([^)]+)\)`)
			if m := r.FindStringSubmatch(line); m != nil {
				out.WriteString(ind())
				iter := strings.TrimSpace(strings.Replace(m[2], "until", "..", 1))
				fmt.Fprintf(&out, "for %s in %s {\n", m[1], iter)
				indent++
				continue
			}
			out.WriteString(ind())
			out.WriteString(line)
			out.WriteByte('\n')
		case strings.HasPrefix(line, "while "):
			cond := strings.TrimPrefix(line, "while ")
			cond = strings.Trim(cond, "(){} ")
			out.WriteString(ind())
			fmt.Fprintf(&out, "while %s {\n", cond)
			indent++
		case strings.HasPrefix(line, "if "):
			cond := strings.TrimPrefix(line, "if ")
			cond = strings.Trim(cond, "(){} ")
			out.WriteString(ind())
			fmt.Fprintf(&out, "if %s {\n", cond)
			indent++
		case line == "else {":
			if indent > 0 {
				indent--
			}
			out.WriteString(ind())
			out.WriteString("} else {\n")
			indent++
		case strings.HasPrefix(line, "val "):
			out.WriteString(ind())
			out.WriteString("let ")
			out.WriteString(strings.TrimSuffix(strings.TrimPrefix(line, "val "), ";"))
			out.WriteByte('\n')
		case strings.HasPrefix(line, "var "):
			out.WriteString(ind())
			out.WriteString("var ")
			out.WriteString(strings.TrimSuffix(strings.TrimPrefix(line, "var "), ";"))
			out.WriteByte('\n')
		case strings.HasPrefix(line, "return "):
			out.WriteString(ind())
			out.WriteString("return ")
			out.WriteString(strings.TrimSuffix(strings.TrimPrefix(line, "return "), ";"))
			out.WriteByte('\n')
		case strings.HasPrefix(line, "println("):
			out.WriteString(ind())
			out.WriteString("print(")
			out.WriteString(strings.TrimSuffix(strings.TrimPrefix(line, "println("), ")"))
			out.WriteString(")\n")
		default:
			out.WriteString(ind())
			out.WriteString(strings.TrimSuffix(line, ";"))
			out.WriteByte('\n')
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func parseSimpleParams(s string) []string {
	var out []string
	for _, p := range strings.Split(s, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if i := strings.Index(p, ":"); i != -1 {
			p = p[:i]
		}
		out = append(out, strings.TrimSpace(p))
	}
	return out
}
