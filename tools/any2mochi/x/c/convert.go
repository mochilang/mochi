package c

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	parent "mochi/tools/any2mochi"
)

func snippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

func diagnostics(src string, diags []parent.Diagnostic) string {
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

// Convert converts C source code to Mochi using the language server.
func Convert(src string) ([]byte, error) {
	if !parent.UseLSP {
		return convertSimple(src)
	}

	ls := parent.Servers["c"]
	syms, diags, err := parent.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		// If no language server is configured or it failed, fall back to
		// parsing via clang.
		if ls.Command == "" {
			return convertSimple(src)
		}
		if out, cErr := convertSimple(src); cErr == nil {
			return out, nil
		}
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", diagnostics(src, diags))
	}

	aliasForRange := make(map[parent.Range]string)
	for _, s := range syms {
		if s.Detail != nil && *s.Detail == "type alias" {
			aliasForRange[s.Range] = s.Name
		}
	}

	var out strings.Builder
	matched := false
	for _, s := range syms {
		switch s.Kind {
		case parent.SymbolKindFunction:
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
		case parent.SymbolKindVariable, parent.SymbolKindConstant:
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
		case parent.SymbolKindStruct, parent.SymbolKindClass:
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
				if c.Kind != parent.SymbolKindField {
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
		case parent.SymbolKindEnum:
			matched = true
			out.WriteString("type ")
			out.WriteString(s.Name)
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == parent.SymbolKindEnumMember {
					fmt.Fprintf(&out, "  %s\n", c.Name)
				}
			}
			out.WriteString("}\n")
			var rest []parent.DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != parent.SymbolKindEnumMember {
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
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the c file and converts it to Mochi.
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
	structs := parseStructs(src)
	funcs, err := parseClangFile(src)
	if err != nil {
		return nil, fmt.Errorf("%v\n\nsource snippet:\n%s", err, snippet(src))
	}
	if len(funcs) == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	var out strings.Builder
	for _, s := range structs {
		out.WriteString(s)
		out.WriteByte('\n')
	}
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

var structRE = regexp.MustCompile(`(?s)typedef\s+(struct|union)\s*\{([^}]*)\}\s*([A-Za-z_][A-Za-z0-9_]*)\s*;`)

func parseStructs(src string) []string {
	matches := structRE.FindAllStringSubmatch(src, -1)
	var out []string
	for _, m := range matches {
		kind := m[1]
		body := m[2]
		name := strings.TrimSpace(m[3])
		var b strings.Builder
		b.WriteString("type ")
		b.WriteString(name)
		if kind == "union" {
			b.WriteString(" union {")
		} else {
			b.WriteString(" {")
		}
		b.WriteByte('\n')
		for _, line := range strings.Split(body, "\n") {
			line = strings.TrimSpace(line)
			if line == "" {
				continue
			}
			fields := strings.Fields(strings.TrimSuffix(line, ";"))
			if len(fields) < 2 {
				continue
			}
			fieldName := fields[len(fields)-1]
			fieldType := mapType(strings.Join(fields[:len(fields)-1], " "))
			b.WriteString("  ")
			b.WriteString(fieldName)
			if fieldType != "" {
				b.WriteString(": ")
				b.WriteString(fieldType)
			}
			b.WriteByte('\n')
		}
		b.WriteString("}\n")
		out = append(out, b.String())
	}
	return out
}

// hoverSignature obtains the function signature via hover information.
// It returns the return type and parameter list. If hover data is unavailable
// it falls back to the symbol detail.
func hoverSignature(src string, sym parent.DocumentSymbol, ls parent.LanguageServer) (string, []param) {
	hov, err := parent.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err == nil {
		if mc, ok := hov.Contents.(parent.MarkupContent); ok {
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
func hoverFieldType(src string, sym parent.DocumentSymbol, ls parent.LanguageServer) string {
	hov, err := parent.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(parent.MarkupContent); ok {
		for _, line := range strings.Split(mc.Value, "\n") {
			l := strings.TrimSpace(line)
			if l != "" {
				return mapType(l)
			}
		}
	}
	return ""
}

func functionBody(src string, sym parent.DocumentSymbol) []string {
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
			if indent < 0 {
				indent = 0
			}
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
			if indent < 0 {
				indent = 0
			}
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

func posToOffset(lines []string, pos parent.Position) int {
	off := 0
	for i := 0; i < int(pos.Line); i++ {
		if i < len(lines) {
			off += len(lines[i]) + 1
		}
	}
	off += int(pos.Character)
	return off
}
