package any2mochi

import (
	"fmt"
	"os"
	"strings"

	cscode "mochi/compile/x/cs"
)

// ConvertCs converts cs source code to Mochi using the language server.
func ConvertCs(src string) ([]byte, error) {
	ls := Servers["cs"]
	// omnisharp requires the dotnet CLI which may not be installed
	_ = cscode.EnsureDotnet()
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

func writeCsSymbols(out *strings.Builder, prefix []string, syms []DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case SymbolKindClass, SymbolKindStruct, SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == SymbolKindField || c.Kind == SymbolKindProperty {
					typ := csFieldType(src, c, ls)
					if typ == "" {
						typ = "any"
					}
					fmt.Fprintf(out, "  %s: %s\n", c.Name, typ)
				}
			}
			out.WriteString("}\n")
			var rest []DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != SymbolKindField && c.Kind != SymbolKindProperty {
					rest = append(rest, c)
				}
			}
			if len(rest) > 0 {
				writeCsSymbols(out, nameParts, rest, src, ls)
			}
		case SymbolKindFunction, SymbolKindMethod, SymbolKindConstructor:
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
			body := convertCsBody(src, s.Range)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, line := range body {
					out.WriteString("  ")
					out.WriteString(line)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
			if len(s.Children) > 0 {
				writeCsSymbols(out, nameParts, s.Children, src, ls)
			}
		case SymbolKindField, SymbolKindProperty, SymbolKindVariable, SymbolKindConstant:
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
		case SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == SymbolKindEnumMember {
					fmt.Fprintf(out, "  %s\n", c.Name)
				}
			}
			out.WriteString("}\n")
			var rest []DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != SymbolKindEnumMember {
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

func csHoverSignature(src string, sym DocumentSymbol, ls LanguageServer) ([]csParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	if mc, ok := hov.Contents.(MarkupContent); ok {
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

func csFieldType(src string, sym DocumentSymbol, ls LanguageServer) string {
	if sym.Detail != nil {
		if t := mapCsType(*sym.Detail); t != "" {
			return t
		}
	}
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err == nil {
		if mc, ok := hov.Contents.(MarkupContent); ok {
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
			if mc, ok := hov.Contents.(MarkupContent); ok {
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

// convertCsBody returns a slice of Mochi statements for the given range.
// It performs very light-weight translation of common C# statements.
func convertCsBody(src string, r Range) []string {
	lines := strings.Split(src, "\n")
	start := int(r.Start.Line)
	end := int(r.End.Line)
	if start >= len(lines) || end >= len(lines) {
		return nil
	}
	bodyLines := lines[start : end+1]
	if len(bodyLines) > 0 {
		bodyLines = bodyLines[1:]
	}
	if len(bodyLines) > 0 {
		bodyLines = bodyLines[:len(bodyLines)-1]
	}
	var out []string
	for _, l := range bodyLines {
		l = strings.TrimSpace(l)
		if l == "" {
			continue
		}
		if strings.HasSuffix(l, ";") {
			l = strings.TrimSuffix(l, ";")
		}
		switch {
		case strings.HasPrefix(l, "Console.WriteLine("):
			l = "print(" + strings.TrimPrefix(strings.TrimSuffix(l, ")"), "Console.WriteLine(") + ")"
		case strings.HasPrefix(l, "return "):
			l = "return " + strings.TrimSpace(strings.TrimPrefix(l, "return "))
		case strings.HasPrefix(l, "for (") && strings.Contains(l, ";") && strings.Contains(l, ")"):
			l = strings.TrimPrefix(l, "for (")
			l = strings.TrimSuffix(l, ") {")
			parts := strings.Split(l, ";")
			if len(parts) >= 2 {
				init := strings.TrimSpace(parts[0])
				cond := strings.TrimSpace(parts[1])
				if strings.HasPrefix(init, "var ") {
					init = strings.TrimPrefix(init, "var ")
				}
				if eq := strings.Index(init, "="); eq != -1 {
					name := strings.TrimSpace(init[:eq])
					startVal := strings.TrimSpace(init[eq+1:])
					startVal = strings.TrimSuffix(startVal, "L")
					endVal := ""
					if idx := strings.Index(cond, "<"); idx != -1 {
						endVal = strings.TrimSpace(cond[idx+1:])
						endVal = strings.TrimSuffix(endVal, "L")
					}
					l = fmt.Sprintf("for %s in %s..%s {", name, startVal, endVal)
				}
			}
		case strings.HasPrefix(l, "while ("):
			l = strings.TrimPrefix(l, "while (")
			l = strings.TrimSuffix(l, ") {")
			l = strings.ReplaceAll(l, "L", "")
			l = "while " + l + " {"
		case strings.HasPrefix(l, "if ("):
			l = strings.TrimPrefix(l, "if (")
			l = strings.TrimSuffix(l, ") {")
			l = strings.ReplaceAll(l, "L", "")
			l = "if " + l + " {"
		case l == "}" || l == "} else {":
			// keep as is
		default:
			for _, t := range []string{"long ", "int ", "float ", "double ", "string ", "bool "} {
				if strings.HasPrefix(l, t) {
					l = strings.TrimPrefix(l, t)
					if strings.HasPrefix(t, "string") {
						l = "var " + l
					} else {
						l = "var " + strings.ReplaceAll(l, "L", "")
					}
					break
				}
			}
			l = strings.ReplaceAll(l, "L", "")
		}
		out = append(out, l)
	}
	return out
}

func splitArgs(s string) []string {
	var parts []string
	var b strings.Builder
	depth := 0
	inStr := false
	for i := 0; i < len(s); i++ {
		ch := s[i]
		if inStr {
			b.WriteByte(ch)
			if ch == '"' {
				inStr = false
			}
			continue
		}
		switch ch {
		case '"':
			inStr = true
			b.WriteByte(ch)
		case '(', '{', '[':
			depth++
			b.WriteByte(ch)
		case ')', '}', ']':
			depth--
			b.WriteByte(ch)
		case ',':
			if depth == 0 {
				parts = append(parts, strings.TrimSpace(b.String()))
				b.Reset()
			} else {
				b.WriteByte(ch)
			}
		default:
			b.WriteByte(ch)
		}
	}
	if b.Len() > 0 {
		parts = append(parts, strings.TrimSpace(b.String()))
	}
	return parts
}
