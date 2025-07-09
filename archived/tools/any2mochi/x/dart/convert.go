//go:build slow

package dart

import (
	"encoding/json"
	"fmt"
	any2mochi "mochi/archived/tools/any2mochi"
	"os"
	"regexp"
	"strings"
	"unicode"
)

// ConvertError provides more context when conversion fails.
type ConvertError struct {
	Line int
	Msg  string
	Snip string
}

func (e *ConvertError) Error() string {
	if e.Line > 0 {
		return fmt.Sprintf("line %d: %s\n%s", e.Line, e.Msg, e.Snip)
	}
	return fmt.Sprintf("%s\n%s", e.Msg, e.Snip)
}

var typedVarRe = regexp.MustCompile(`^(?:final|const)?\s*([A-Za-z_][A-Za-z0-9_<>,\[\]\? ]*)\s+([A-Za-z_][A-Za-z0-9_]*)\s*(=.*)?$`)

// Convert converts Dart source code to Mochi.
func Convert(src string) ([]byte, error) {
	funcs, classes, enums, err := parseCLI(src)
	if err != nil {
		return nil, &ConvertError{Msg: err.Error(), Snip: any2mochi.NumberedSnippet(src)}
	}
	if len(funcs) == 0 {
		return nil, &ConvertError{Msg: "no convertible symbols found", Snip: any2mochi.NumberedSnippet(src)}
	}
	var out strings.Builder
	for _, v := range parseTopLevelVars(src, funcs, classes, enums) {
		out.WriteString(v)
		out.WriteByte('\n')
	}
	for _, e := range enums {
		out.WriteString("type ")
		out.WriteString(e.Name)
		out.WriteString(" {\n")
		for _, m := range e.Members {
			out.WriteString("  ")
			out.WriteString(m)
			out.WriteByte('\n')
		}
		out.WriteString("}\n")
	}
	for _, c := range classes {
		out.WriteString("type ")
		out.WriteString(c.Name)
		out.WriteString(" {\n")
		for _, f := range c.Fields {
			out.WriteString("  ")
			out.WriteString(f.name)
			if f.typ != "" && f.typ != "any" {
				out.WriteString(": ")
				out.WriteString(f.typ)
			}
			out.WriteByte('\n')
		}
		out.WriteString("}\n")
	}
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

// ConvertFile reads the Dart file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

func snippet(src string) string {
	return any2mochi.NumberedSnippet(src)
}

type param struct {
	name string
	typ  string
}

type function struct {
	Name      string
	Params    []param
	Ret       string
	Body      []string
	StartLine int
	EndLine   int
	Doc       string
}

type class struct {
	Name      string
	Fields    []param
	StartLine int
	EndLine   int
	Doc       string
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

func parseHover(h any2mochi.Hover) ([]param, string) {
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

func writeSymbols(out *strings.Builder, prefix []string, syms []any2mochi.DocumentSymbol, src string, ls any2mochi.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}

		switch s.Kind {
		case any2mochi.SymbolKindClass, any2mochi.SymbolKindInterface, any2mochi.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == any2mochi.SymbolKindField || c.Kind == any2mochi.SymbolKindProperty || c.Kind == any2mochi.SymbolKindVariable {
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
				if c.Kind == any2mochi.SymbolKindMethod || c.Kind == any2mochi.SymbolKindConstructor || c.Kind == any2mochi.SymbolKindFunction {
					writeSymbols(out, nameParts, []any2mochi.DocumentSymbol{c}, src, ls)
				}
			}
			continue
		case any2mochi.SymbolKindVariable, any2mochi.SymbolKindConstant:
			if len(prefix) == 0 && s.Name != "" {
				out.WriteString("let ")
				out.WriteString(s.Name)
				if t := fieldType(src, s, ls); t != "" {
					out.WriteString(": ")
					out.WriteString(t)
				}
				out.WriteByte('\n')
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
			continue
		case any2mochi.SymbolKindFunction, any2mochi.SymbolKindMethod, any2mochi.SymbolKindConstructor:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			detail := ""
			if s.Detail != nil {
				detail = *s.Detail
			}
			params, ret := parseDetail(detail)
			if len(params) == 0 && ret == "" {
				if hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
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

func fieldType(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) string {
	if t := typeFromDetail(sym.Detail); t != "" && t != "any" {
		return t
	}
	return typeFromHover(src, sym, ls)
}

func typeFromHover(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) string {
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
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
func parseFunctionBody(src string, sym any2mochi.DocumentSymbol) []string {
	code := extractRangeText(src, sym.Range)
	start := strings.Index(code, "{")
	end := strings.LastIndex(code, "}")
	if start == -1 || end == -1 || end <= start {
		return nil
	}
	body := code[start+1 : end]
	return parseStatements(body)
}

func extractRangeText(src string, r any2mochi.Range) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for i := int(r.Start.Line); i <= int(r.End.Line) && i < len(lines); i++ {
		line := lines[i]
		if i == int(r.Start.Line) && int(r.Start.Character) < len(line) {
			line = line[int(r.Start.Character):]
		}
		if i == int(r.End.Line) && int(r.End.Character) <= len(line) {
			line = line[:int(r.End.Character)]
		}
		out.WriteString(line)
		if i != int(r.End.Line) {
			out.WriteByte('\n')
		}
	}
	return out.String()
}

func parseStatements(body string) []string {
	lines := strings.Split(body, "\n")
	var out []string
	indent := 1
	for _, line := range lines {
		l := strings.TrimSpace(line)
		l = strings.TrimSuffix(l, ";")
		if l == "" {
			continue
		}
		switch {
		case l == "}":
			if indent > 0 {
				indent--
			}
			out = append(out, strings.Repeat("  ", indent)+"}")
		case strings.HasPrefix(l, "if ") && strings.HasSuffix(l, "{"):
			cond := strings.TrimSpace(strings.TrimSuffix(l[3:], "{"))
			out = append(out, strings.Repeat("  ", indent)+"if "+cond+" {")
			indent++
		case strings.HasPrefix(l, "for ") && strings.HasSuffix(l, "{"):
			head := strings.TrimSpace(strings.TrimSuffix(l[4:], "{"))
			rep := regexp.MustCompile(`var (\w+) = ([^;]+); \w+ < ([^;]+); \w+\+\+`)
			if m := rep.FindStringSubmatch(head); m != nil {
				out = append(out, strings.Repeat("  ", indent)+"for "+m[1]+" in "+m[2]+".."+m[3]+" {")
			} else {
				out = append(out, strings.Repeat("  ", indent)+"for "+head+" {")
			}
			indent++
		case !strings.HasPrefix(l, "var ") && !strings.HasPrefix(l, "return ") && typedVarRe.MatchString(l):
			m := typedVarRe.FindStringSubmatch(l)
			typ := toMochiType(strings.TrimSpace(m[1]))
			name := m[2]
			val := strings.TrimSpace(strings.TrimPrefix(m[3], "="))
			stmt := "let " + name
			if typ != "" && typ != "any" {
				stmt += ": " + typ
			}
			if val != "" {
				stmt += " = " + val
			}
			out = append(out, strings.Repeat("  ", indent)+stmt)
		case strings.HasPrefix(l, "var "):
			stmt := strings.TrimSpace(strings.TrimPrefix(l, "var "))
			if strings.Contains(stmt, "=") && strings.Contains(stmt, ".length") {
				parts := strings.SplitN(stmt, "=", 2)
				name := strings.TrimSpace(parts[0])
				rhs := strings.TrimSpace(strings.TrimSuffix(parts[1], ".length"))
				l = "let " + name + " = len(" + rhs + ")"
			} else {
				l = "let " + stmt
			}
			out = append(out, strings.Repeat("  ", indent)+l)
		case strings.HasPrefix(l, "while ") && strings.HasSuffix(l, "{"):
			cond := strings.TrimSpace(strings.TrimSuffix(l[6:], "{"))
			out = append(out, strings.Repeat("  ", indent)+"while "+cond+" {")
			indent++
		case strings.HasPrefix(l, "} else if ") && strings.HasSuffix(l, "{"):
			if indent > 0 {
				indent--
			}
			cond := strings.TrimSpace(strings.TrimSuffix(l[len("} else if "):], "{"))
			out = append(out, strings.Repeat("  ", indent)+"} else if "+cond+" {")
			indent++
		case strings.HasPrefix(l, "else if ") && strings.HasSuffix(l, "{"):
			if indent > 0 {
				indent--
			}
			cond := strings.TrimSpace(strings.TrimSuffix(l[len("else if "):], "{"))
			out = append(out, strings.Repeat("  ", indent)+"else if "+cond+" {")
			indent++
		case l == "} else {":
			if indent > 0 {
				indent--
			}
			out = append(out, strings.Repeat("  ", indent)+"} else {")
			indent++
		case l == "else {":
			if indent > 0 {
				indent--
			}
			out = append(out, strings.Repeat("  ", indent)+"else {")
			indent++
		case strings.HasPrefix(l, "return "):
			expr := strings.TrimSpace(l[len("return "):])
			out = append(out, strings.Repeat("  ", indent)+"return "+convertQuotes(expr))
		default:
			out = append(out, strings.Repeat("  ", indent)+convertQuotes(l))
		}
	}
	return out
}

func hoverString(h any2mochi.Hover) string {
	switch v := h.Contents.(type) {
	case any2mochi.MarkupContent:
		return v.Value
	case any2mochi.MarkedString:
		b, err := json.Marshal(v)
		if err == nil {
			var ms any2mochi.MarkedStringStruct
			if err := json.Unmarshal(b, &ms); err == nil {
				if ms.Value != "" {
					return ms.Value
				}
			}
			var s string
			if err := json.Unmarshal(b, &s); err == nil {
				return s
			}
		}
		return ""
	case []any2mochi.MarkedString:
		parts := make([]string, 0, len(v))
		for _, m := range v {
			parts = append(parts, hoverString(any2mochi.Hover{Contents: m}))
		}
		return strings.Join(parts, "\n")
	case string:
		return v
	default:
		return fmt.Sprint(v)
	}
}

func parseTopLevelVars(src string, funcs []function, classes []class, enums []dartEnum) []string {
	lines := strings.Split(src, "\n")
	skip := make([]bool, len(lines)+1)
	mark := func(start, end int) {
		for i := start; i <= end && i <= len(lines); i++ {
			if i >= 1 {
				skip[i] = true
			}
		}
	}
	for _, f := range funcs {
		mark(f.StartLine, f.EndLine)
	}
	for _, c := range classes {
		mark(c.StartLine, c.EndLine)
	}
	for _, e := range enums {
		mark(e.StartLine, e.EndLine)
	}
	var vars []string
	for i, line := range lines {
		ln := i + 1
		if skip[ln] {
			continue
		}
		l := strings.TrimSpace(strings.TrimSuffix(line, ";"))
		if strings.HasPrefix(l, "var ") {
			vars = append(vars, convertQuotes("let "+strings.TrimSpace(l[4:])))
			continue
		}
		if m := typedVarRe.FindStringSubmatch(l); m != nil {
			typ := toMochiType(strings.TrimSpace(m[1]))
			name := m[2]
			val := strings.TrimSpace(strings.TrimPrefix(m[3], "="))
			stmt := "let " + name
			if typ != "" && typ != "any" {
				stmt += ": " + typ
			}
			if val != "" {
				stmt += " = " + val
			}
			vars = append(vars, convertQuotes(stmt))
		}
	}
	return vars
}

var quoteRe = regexp.MustCompile(`'([^']*)'`)

func convertQuotes(s string) string {
	return quoteRe.ReplaceAllStringFunc(s, func(q string) string {
		return "\"" + strings.Trim(q, "'") + "\""
	})
}
