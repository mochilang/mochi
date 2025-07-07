//go:build archived

package cs

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	cscode "mochi/archived/x/cs"
	any2mochi "mochi/tools/any2mochi"
)

var (
	longLit   = regexp.MustCompile(`\b([0-9]+)L\b`)
	varDeclRE = regexp.MustCompile(`^([A-Za-z0-9_<>\[\]]+)\s+([A-Za-z_][A-Za-z0-9_]*)\s*=`)
)

func stripLong(s string) string { return longLit.ReplaceAllString(s, "$1") }

// Convert converts C# source code to Mochi using the language server.
func Convert(src string) ([]byte, error) {
	if !any2mochi.UseLSP {
		return convertSimple(src)
	}

	ls := any2mochi.Servers["cs"]
	// omnisharp requires the dotnet CLI which may not be installed
	_ = cscode.EnsureDotnet()
	syms, diags, err := any2mochi.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		// fall back to the minimal parser when the language server is unavailable
		return convertSimple(src)
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", diagnostics(src, diags))
	}
	var out strings.Builder
	writeSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the cs file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

// convertSimple converts C# source code without using a language server.
// It relies on a very small built-in parser that understands the subset of
// C# emitted by the Mochi compiler.
func convertSimple(src string) ([]byte, error) {
	ast, err := parseSimple(src)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	for _, t := range ast.Types {
		if t.Doc != "" {
			for _, ln := range strings.Split(t.Doc, "\n") {
				out.WriteString("# ")
				out.WriteString(strings.TrimSpace(ln))
				out.WriteByte('\n')
			}
		}
		out.WriteString("type ")
		out.WriteString(t.Name)
		out.WriteString(" {\n")
		for _, f := range t.Fields {
			if f.Doc != "" {
				for _, ln := range strings.Split(f.Doc, "\n") {
					out.WriteString("  # ")
					out.WriteString(strings.TrimSpace(ln))
					out.WriteByte('\n')
				}
			}
			out.WriteString("  ")
			out.WriteString(f.Name)
			if ft := mapType(f.Type); ft != "" {
				out.WriteString(": ")
				out.WriteString(ft)
			}
			out.WriteByte('\n')
		}
		out.WriteString("}\n")
		for _, fn := range t.Methods {
			if fn.Doc != "" {
				for _, ln := range strings.Split(fn.Doc, "\n") {
					out.WriteString("# ")
					out.WriteString(strings.TrimSpace(ln))
					out.WriteByte('\n')
				}
			}
			out.WriteString("fun ")
			out.WriteString(t.Name)
			out.WriteByte('.')
			out.WriteString(fn.Name)
			out.WriteByte('(')
			for i, p := range fn.Params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.Name)
				if pt := mapType(p.Type); pt != "" {
					out.WriteString(": ")
					out.WriteString(pt)
				}
			}
			out.WriteByte(')')
			if rt := mapType(fn.Ret); rt != "" {
				out.WriteString(": ")
				out.WriteString(rt)
			}
			body := convertBodyLines(fn.Body)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, b := range body {
					out.WriteString("  ")
					out.WriteString(b)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

// convertBodyLines applies a very small set of translations on method body
// lines. It mirrors the logic of convertBody but operates on a slice of
// strings rather than a source range.
func convertBodyLines(body []string) []string {
	var out []string
	for _, ln := range body {
		l := strings.TrimSpace(ln)
		if l == "" {
			continue
		}
		if strings.HasSuffix(l, ";") {
			l = strings.TrimSuffix(l, ";")
		}
		l = stripLong(l)
		switch {
		case strings.HasPrefix(l, "Console.WriteLine("):
			l = "print(" + strings.TrimPrefix(strings.TrimSuffix(l, ")"), "Console.WriteLine(") + ")"
		case strings.HasPrefix(l, "return "):
			l = "return " + strings.TrimSpace(strings.TrimPrefix(l, "return "))
		case strings.HasPrefix(l, "foreach ("):
			inner := strings.TrimPrefix(l, "foreach (")
			inner = strings.TrimSuffix(inner, ") {")
			parts := strings.SplitN(inner, " in ", 2)
			if len(parts) == 2 {
				varName := strings.TrimSpace(parts[0])
				fs := strings.Fields(varName)
				if len(fs) > 1 {
					varName = fs[len(fs)-1]
				}
				iter := strings.TrimSpace(parts[1])
				l = fmt.Sprintf("for %s in %s {", varName, iter)
			}
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
					startVal = stripLong(startVal)
					endVal := ""
					if idx := strings.Index(cond, "<"); idx != -1 {
						endVal = strings.TrimSpace(cond[idx+1:])
						endVal = stripLong(endVal)
					}
					l = fmt.Sprintf("for %s in %s..%s {", name, startVal, endVal)
				}
			}
		case strings.HasPrefix(l, "while ("):
			l = strings.TrimPrefix(l, "while (")
			l = strings.TrimSpace(strings.TrimSuffix(l, "{"))
			if strings.HasSuffix(l, ")") {
				idx := strings.LastIndex(l, ")")
				l = l[:idx]
			}
			l = strings.TrimSpace(l)
			l = stripLong(l)
			l = "while " + l + " {"
		case strings.HasPrefix(l, "if ("):
			l = strings.TrimPrefix(l, "if (")
			l = strings.TrimSuffix(l, ") {")
			l = stripLong(l)
			l = "if " + l + " {"
		case l == "}" || l == "} else {":
			// keep as is
		default:
			if m := varDeclRE.FindStringSubmatch(l); m != nil {
				name := m[2]
				rest := strings.TrimSpace(l[len(m[0]):])
				l = "var " + name + " = " + rest
			} else {
				for _, t := range []string{"long ", "int ", "float ", "double ", "string ", "bool "} {
					if strings.HasPrefix(l, t) {
						l = strings.TrimPrefix(l, t)
						if strings.HasPrefix(t, "string") {
							l = "var " + l
						} else {
							l = "var " + stripLong(l)
						}
						break
					}
				}
				l = stripLong(l)
			}
		}
		out = append(out, l)
	}
	return out
}

type param struct {
	name string
	typ  string
}

func snippet(src string) string {
	return any2mochi.NumberedSnippet(src)
}

func diagnostics(src string, diags []any2mochi.Diagnostic) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, d := range diags {
		start := int(d.Range.Start.Line)
		msg := d.Message
		line := ""
		if start < len(lines) {
			line = strings.TrimSpace(lines[start])
		}
		out.WriteString(fmt.Sprintf("line %d: %s\n  %s\n", start+1, msg, line))
	}
	return strings.TrimSpace(out.String())
}

func writeSymbols(out *strings.Builder, prefix []string, syms []any2mochi.DocumentSymbol, src string, ls any2mochi.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case any2mochi.SymbolKindClass, any2mochi.SymbolKindStruct, any2mochi.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == any2mochi.SymbolKindField || c.Kind == any2mochi.SymbolKindProperty {
					typ := fieldType(src, c, ls)
					if typ == "" {
						typ = "any"
					}
					fmt.Fprintf(out, "  %s: %s\n", c.Name, typ)
				}
			}
			out.WriteString("}\n")
			var rest []any2mochi.DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != any2mochi.SymbolKindField && c.Kind != any2mochi.SymbolKindProperty {
					rest = append(rest, c)
				}
			}
			if len(rest) > 0 {
				writeSymbols(out, nameParts, rest, src, ls)
			}
		case any2mochi.SymbolKindFunction, any2mochi.SymbolKindMethod, any2mochi.SymbolKindConstructor:
			params, ret := parseSignature(s.Detail)
			if len(params) == 0 || ret == "" {
				if p, r := hoverSignature(src, s, ls); len(p) > 0 || r != "" {
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
			body := convertBody(src, s.Range)
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
				writeSymbols(out, nameParts, s.Children, src, ls)
			}
		case any2mochi.SymbolKindField, any2mochi.SymbolKindProperty, any2mochi.SymbolKindVariable, any2mochi.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				if typ := fieldType(src, s, ls); typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
			if len(s.Children) > 0 {
				writeSymbols(out, nameParts, s.Children, src, ls)
			}
		case any2mochi.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == any2mochi.SymbolKindEnumMember {
					fmt.Fprintf(out, "  %s\n", c.Name)
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
				writeSymbols(out, nameParts, rest, src, ls)
			}
		default:
			if len(s.Children) > 0 {
				writeSymbols(out, nameParts, s.Children, src, ls)
			}
		}
	}
}

func parseSignature(detail *string) ([]param, string) {
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
			return nil, mapType(parts[0])
		}
		return nil, ""
	}
	pre := strings.TrimSpace(d[:open])
	parts := strings.Fields(pre)
	ret := ""
	if len(parts) >= 2 {
		ret = mapType(parts[len(parts)-2])
	} else if len(parts) == 1 {
		ret = mapType(parts[0])
	}
	paramsPart := strings.TrimSpace(d[open+1 : close])
	var params []param
	if paramsPart != "" {
		rawParams := splitArgs(paramsPart)
		for _, p := range rawParams {
			typ, name := parseParam(p)
			if name != "" {
				params = append(params, param{name: name, typ: typ})
			}
		}
	}
	return params, ret
}

func parseParam(p string) (string, string) {
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
	typ = mapType(typ)
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

func mapType(t string) string {
	t = strings.TrimSpace(t)
	for strings.HasSuffix(t, "[]") {
		inner := mapType(strings.TrimSuffix(t, "[]"))
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
	case "dynamic", "object":
		return "any"
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
					if at := mapType(args[0]); at != "" {
						a = at
					}
				}
				return "list<" + a + ">"
			case "Dictionary", "IDictionary":
				if len(args) == 2 {
					k := mapType(args[0])
					if k == "" {
						k = "any"
					}
					v := mapType(args[1])
					if v == "" {
						v = "any"
					}
					return "map<" + k + ", " + v + ">"
				}
			case "Nullable":
				if len(args) == 1 {
					return mapType(args[0])
				}
			}
		}
	}
	return ""
}

func hoverSignature(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) ([]param, string) {
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	if mc, ok := hov.Contents.(any2mochi.MarkupContent); ok {
		for _, line := range strings.Split(mc.Value, "\n") {
			l := strings.TrimSpace(line)
			if strings.Contains(l, "(") && strings.Contains(l, ")") {
				if p, r := parseSignature(&l); len(p) > 0 || r != "" {
					return p, r
				}
			}
		}
	}
	return nil, ""
}

func fieldType(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) string {
	if sym.Detail != nil {
		if t := mapType(*sym.Detail); t != "" {
			return t
		}
	}
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err == nil {
		if mc, ok := hov.Contents.(any2mochi.MarkupContent); ok {
			for _, line := range strings.Split(mc.Value, "\n") {
				l := strings.TrimSpace(line)
				fields := strings.Fields(l)
				if len(fields) >= 2 {
					if t := mapType(fields[0]); t != "" {
						return t
					}
					if t := mapType(fields[len(fields)-1]); t != "" {
						return t
					}
				}
				if idx := strings.Index(l, ":"); idx != -1 {
					if t := mapType(strings.TrimSpace(l[idx+1:])); t != "" {
						return t
					}
				}
			}
		}
	}
	defs, err := any2mochi.EnsureAndDefinition(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err == nil && len(defs) > 0 {
		pos := defs[0].Range.Start
		hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
		if err == nil {
			if mc, ok := hov.Contents.(any2mochi.MarkupContent); ok {
				for _, line := range strings.Split(mc.Value, "\n") {
					l := strings.TrimSpace(line)
					if t := mapType(l); t != "" {
						return t
					}
				}
			}
		}
	}
	return ""
}

// convertBody returns a slice of Mochi statements for the given range.
// It performs very light-weight translation of common C# statements.
func convertBody(src string, r any2mochi.Range) []string {
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
		l = stripLong(l)
		switch {
		case strings.HasPrefix(l, "Console.WriteLine("):
			l = "print(" + strings.TrimPrefix(strings.TrimSuffix(l, ")"), "Console.WriteLine(") + ")"
		case strings.HasPrefix(l, "return "):
			l = "return " + strings.TrimSpace(strings.TrimPrefix(l, "return "))
		case strings.HasPrefix(l, "foreach ("):
			inner := strings.TrimPrefix(l, "foreach (")
			inner = strings.TrimSuffix(inner, ") {")
			parts := strings.SplitN(inner, " in ", 2)
			if len(parts) == 2 {
				varName := strings.TrimSpace(parts[0])
				fs := strings.Fields(varName)
				if len(fs) > 1 {
					varName = fs[len(fs)-1]
				}
				iter := strings.TrimSpace(parts[1])
				l = fmt.Sprintf("for %s in %s {", varName, iter)
			}
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
					startVal = stripLong(startVal)
					endVal := ""
					if idx := strings.Index(cond, "<"); idx != -1 {
						endVal = strings.TrimSpace(cond[idx+1:])
						endVal = stripLong(endVal)
					}
					l = fmt.Sprintf("for %s in %s..%s {", name, startVal, endVal)
				}
			}
		case strings.HasPrefix(l, "while ("):
			l = strings.TrimPrefix(l, "while (")
			l = strings.TrimSpace(strings.TrimSuffix(l, "{"))
			if strings.HasSuffix(l, ")") {
				idx := strings.LastIndex(l, ")")
				l = l[:idx]
			}
			l = strings.TrimSpace(l)
			l = stripLong(l)
			l = "while " + l + " {"
		case strings.HasPrefix(l, "if ("):
			l = strings.TrimPrefix(l, "if (")
			l = strings.TrimSuffix(l, ") {")
			l = stripLong(l)
			l = "if " + l + " {"
		case l == "}" || l == "} else {":
			// keep as is
		default:
			if m := varDeclRE.FindStringSubmatch(l); m != nil {
				name := m[2]
				rest := strings.TrimSpace(l[len(m[0]):])
				l = "var " + name + " = " + rest
			} else {
				for _, t := range []string{"long ", "int ", "float ", "double ", "string ", "bool "} {
					if strings.HasPrefix(l, t) {
						l = strings.TrimPrefix(l, t)
						if strings.HasPrefix(t, "string") {
							l = "var " + l
						} else {
							l = "var " + stripLong(l)
						}
						break
					}
				}
				l = stripLong(l)
			}
		}
		out = append(out, l)
	}
	return out
}

func splitArgs(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '[', '(', '<':
			depth++
		case ']', ')', '>':
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
