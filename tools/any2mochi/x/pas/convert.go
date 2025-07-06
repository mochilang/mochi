package pas

import (
	any2mochi "mochi/tools/any2mochi"

	"encoding/json"
	"fmt"
	"os"
	"strings"
)

// Convert converts Pascal source code to Mochi using the language server.
func Convert(src string) ([]byte, error) {
	ls := any2mochi.Servers["pas"]
	syms, diags, err := any2mochi.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		// fall back to very small regex based parser when pasls is missing
		return convertFallback(src)
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

// ConvertFile reads the Pascal file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

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

func diagnostics(src string, diags []any2mochi.Diagnostic) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, d := range diags {
		ln := int(d.Range.Start.Line)
		col := int(d.Range.Start.Character)
		endCol := int(d.Range.End.Character)
		if endCol <= col {
			endCol = col + 1
		}
		msg := d.Message
		out.WriteString(fmt.Sprintf("line %d:%d: %s\n", ln+1, col+1, msg))
		from := ln - 1
		if from < 0 {
			from = 0
		}
		to := ln + 1
		if to >= len(lines) {
			to = len(lines) - 1
		}
		for i := from; i <= to && i < len(lines); i++ {
			prefix := "  "
			if i == ln {
				prefix = "->"
			}
			out.WriteString(fmt.Sprintf("%s %3d| %s\n", prefix, i+1, lines[i]))
			if i == ln {
				marker := strings.Repeat(" ", col) + strings.Repeat("^", endCol-col)
				out.WriteString(fmt.Sprintf("   | %s\n", marker))
			}
		}
	}
	return strings.TrimSpace(out.String())
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

type param struct {
	name string
	typ  string
}

func writeSymbols(out *strings.Builder, prefix []string, syms []any2mochi.DocumentSymbol, src string, ls any2mochi.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case any2mochi.SymbolKindFunction, any2mochi.SymbolKindMethod:
			params, ret := hoverSignature(src, s, ls)
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
			if ret != "" && ret != "void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			body := convertBody(src, s)
			if body == "" {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, line := range strings.Split(body, "\n") {
					if strings.TrimSpace(line) == "" {
						continue
					}
					out.WriteString("  ")
					out.WriteString(line)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case any2mochi.SymbolKindStruct, any2mochi.SymbolKindClass, any2mochi.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []any2mochi.DocumentSymbol{}
			methods := []any2mochi.DocumentSymbol{}
			for _, c := range s.Children {
				switch c.Kind {
				case any2mochi.SymbolKindField, any2mochi.SymbolKindProperty:
					fields = append(fields, c)
				case any2mochi.SymbolKindFunction, any2mochi.SymbolKindMethod:
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
					if typ := fieldType(src, f, ls); typ != "" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
				for _, m := range methods {
					var b strings.Builder
					writeSymbols(&b, []string{m.Name}, []any2mochi.DocumentSymbol{m}, src, ls)
					for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
						out.WriteString("  ")
						out.WriteString(line)
						out.WriteByte('\n')
					}
				}
				out.WriteString("}\n")
			}
		case any2mochi.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == any2mochi.SymbolKindEnumMember {
					out.WriteString("  ")
					out.WriteString(c.Name)
					out.WriteByte('\n')
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
		case any2mochi.SymbolKindVariable, any2mochi.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(s.Name)
				if typ := fieldType(src, s, ls); typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		}
		if len(s.Children) > 0 && s.Kind != any2mochi.SymbolKindStruct && s.Kind != any2mochi.SymbolKindClass && s.Kind != any2mochi.SymbolKindInterface {
			writeSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func hoverSignature(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) ([]param, string) {
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		if sym.Detail != nil {
			return parseSignature(*sym.Detail)
		}
		return nil, ""
	}
	lines := strings.Split(hoverString(hov), "\n")
	for _, l := range lines {
		l = strings.TrimSpace(l)
		lower := strings.ToLower(l)
		if strings.HasPrefix(lower, "function") || strings.HasPrefix(lower, "procedure") {
			return parseSignature(l)
		}
	}
	if sym.Detail != nil {
		return parseSignature(*sym.Detail)
	}
	return nil, ""
}

func fieldType(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) string {
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	lines := strings.Split(hoverString(hov), "\n")
	for _, l := range lines {
		l = strings.TrimSpace(l)
		if idx := strings.Index(l, ":"); idx != -1 {
			t := strings.TrimSpace(l[idx+1:])
			if t != "" {
				return toMochiType(t)
			}
		}
	}
	return ""
}

func parseSignature(sig string) ([]param, string) {
	sig = strings.TrimSpace(sig)
	sig = strings.TrimSuffix(sig, ";")
	lower := strings.ToLower(sig)
	if strings.HasPrefix(lower, "function") {
		sig = strings.TrimSpace(sig[len("function"):])
	} else if strings.HasPrefix(lower, "procedure") {
		sig = strings.TrimSpace(sig[len("procedure"):])
	}
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	paramsPart := ""
	rest := ""
	if open != -1 && close != -1 && close > open {
		paramsPart = sig[open+1 : close]
		rest = strings.TrimSpace(sig[close+1:])
	}
	var params []param
	for _, p := range strings.Split(paramsPart, ";") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		colon := strings.Index(p, ":")
		if colon == -1 {
			continue
		}
		names := strings.Split(p[:colon], ",")
		typ := toMochiType(strings.TrimSpace(p[colon+1:]))
		for _, n := range names {
			n = strings.TrimSpace(n)
			if n == "" {
				continue
			}
			params = append(params, param{name: n, typ: typ})
		}
	}
	ret := ""
	if rest != "" {
		if strings.HasPrefix(rest, ":") {
			rest = strings.TrimSpace(rest[1:])
		}
		ret = toMochiType(strings.TrimSpace(rest))
	}
	return params, ret
}

func toMochiType(t string) string {
	t = strings.ToLower(strings.TrimSpace(t))
	switch t {
	case "", "void":
		return ""
	case "integer", "longint", "shortint", "byte", "smallint":
		return "int"
	case "double", "single", "real", "real64":
		return "float"
	case "boolean", "bool":
		return "bool"
	case "string", "ansistring", "widestring", "shortstring", "pchar", "char":
		return "string"
	}
	if strings.HasPrefix(t, "specialize tarray<") && strings.HasSuffix(t, ">") {
		inner := toMochiType(t[len("specialize tarray<") : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "array of ") {
		inner := toMochiType(strings.TrimPrefix(t, "array of "))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return t
}

func parseFuncHeader(l string) (string, string, string) {
	l = strings.TrimSpace(l)
	l = strings.TrimSuffix(l, ";")
	lower := strings.ToLower(l)
	if strings.HasPrefix(lower, "function") {
		l = strings.TrimSpace(l[len("function"):])
	} else if strings.HasPrefix(lower, "procedure") {
		l = strings.TrimSpace(l[len("procedure"):])
	} else {
		return "", "", ""
	}
	rest := l
	open := strings.Index(rest, "(")
	close := strings.LastIndex(rest, ")")
	if open == -1 || close == -1 || close <= open {
		return "", "", ""
	}
	name := strings.TrimSpace(rest[:open])
	paramsPart := rest[open+1 : close]
	ret := ""
	after := strings.TrimSpace(rest[close+1:])
	if strings.HasPrefix(after, ":") {
		ret = toMochiType(strings.TrimSpace(after[1:]))
	}
	var params []string
	for _, p := range strings.Split(paramsPart, ";") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if idx := strings.Index(p, ":"); idx != -1 {
			namePart := strings.TrimSpace(p[:idx])
			typPart := toMochiType(strings.TrimSpace(p[idx+1:]))
			if typPart != "" {
				params = append(params, namePart+": "+typPart)
			} else {
				params = append(params, namePart)
			}
		}
	}
	return name, strings.Join(params, ", "), ret
}

// convertPasBody extracts the function body for sym from src and converts a few
// basic statements into Mochi equivalents. Only very simple constructs like
// assignments, returns and writeln calls are handled.
func convertBody(src string, sym any2mochi.DocumentSymbol) string {
	lines := strings.Split(src, "\n")
	start := int(sym.Range.Start.Line)
	end := int(sym.Range.End.Line)
	if start >= len(lines) || end >= len(lines) {
		return ""
	}
	bodyLines := lines[start : end+1]
	beginIdx := -1
	endIdx := -1
	for i, l := range bodyLines {
		if strings.TrimSpace(strings.ToLower(l)) == "begin" {
			beginIdx = i + 1
		}
		if strings.TrimSpace(strings.ToLower(l)) == "end;" {
			endIdx = i
			break
		}
	}
	if beginIdx == -1 || endIdx == -1 || beginIdx >= endIdx {
		return ""
	}
	bodyLines = bodyLines[beginIdx:endIdx]
	var out []string
	for _, l := range bodyLines {
		l = strings.TrimSpace(l)
		if l == "" {
			continue
		}
		lower := strings.ToLower(l)
		switch {
		case strings.HasPrefix(lower, "writeln("):
			l = strings.TrimSuffix(strings.TrimPrefix(l, "writeln("), ");")
			out = append(out, "print("+l+")")
		case strings.HasPrefix(lower, "result :="):
			expr := strings.TrimSpace(l[len("result :="):])
			expr = strings.TrimSuffix(expr, ";")
			out = append(out, "return "+expr)
		case lower == "exit;":
			out = append(out, "return")
		case strings.Contains(l, ":="):
			parts := strings.SplitN(l, ":=", 2)
			name := strings.TrimSpace(parts[0])
			expr := strings.TrimSpace(strings.TrimSuffix(parts[1], ";"))
			out = append(out, name+" = "+expr)
		default:
			out = append(out, "// "+l)
		}
	}
	return strings.Join(out, "\n")
}

// convertFallback converts a small subset of Pascal syntax to Mochi when no
// language server is available. It recognises simple type declarations,
// variable definitions and statements inside the main program block.
func convertFallback(src string) ([]byte, error) {
	lines := strings.Split(src, "\n")
	var out []string
	inBody := false
	inFunc := false
	inLoop := false
	inRepeat := false
	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		lower := strings.ToLower(l)
		if !inBody {
			switch {
			case strings.HasPrefix(lower, "type") && strings.Contains(lower, "record"):
				name := ""
				if idx := strings.Index(strings.ToLower(l), "type"); idx != -1 {
					rest := strings.TrimSpace(l[idx+len("type"):])
					eq := strings.Index(rest, "=")
					if eq != -1 {
						name = strings.TrimSpace(rest[:eq])
						rest = strings.TrimSpace(rest[eq+1:])
					}
					if !strings.Contains(strings.ToLower(rest), "record") && i+1 < len(lines) {
						i++
						rest = strings.TrimSpace(lines[i])
					}
				}
				var fields []string
				for j := i + 1; j < len(lines); j++ {
					ln := strings.TrimSpace(lines[j])
					if strings.ToLower(ln) == "end;" {
						i = j
						break
					}
					if idx := strings.Index(ln, ":"); idx != -1 {
						fname := strings.TrimSpace(ln[:idx])
						ftype := toMochiType(strings.TrimSpace(strings.TrimSuffix(ln[idx+1:], ";")))
						if fname != "" {
							if ftype != "" {
								fields = append(fields, fmt.Sprintf("  %s: %s", fname, ftype))
							} else {
								fields = append(fields, "  "+fname)
							}
						}
					}
				}
				if name != "" && len(fields) > 0 {
					out = append(out, "type "+name+" {")
					out = append(out, fields...)
					out = append(out, "}")
				}

			case strings.HasPrefix(lower, "type") && strings.Contains(l, "("):
				name := ""
				if idx := strings.Index(strings.ToLower(l), "type"); idx != -1 {
					rest := strings.TrimSpace(l[idx+len("type"):])
					eq := strings.Index(rest, "=")
					if eq != -1 {
						name = strings.TrimSpace(rest[:eq])
						rest = rest[eq+1:]
					}
					vals := rest
					for !strings.Contains(vals, ")") && i+1 < len(lines) {
						i++
						vals += strings.TrimSpace(lines[i])
					}
					if cIdx := strings.Index(vals, "("); cIdx != -1 {
						vals = vals[cIdx+1:]
					}
					if end := strings.Index(vals, ")"); end != -1 {
						vals = vals[:end]
					}
					var members []string
					for _, part := range strings.Split(vals, ",") {
						v := strings.TrimSpace(strings.TrimSuffix(part, ";"))
						if v != "" {
							members = append(members, v)
						}
					}
					if name != "" && len(members) > 0 {
						out = append(out, "type "+name+" {")
						for _, m := range members {
							out = append(out, "  "+m)
						}
						out = append(out, "}")
					}
				}
			case (strings.HasPrefix(lower, "function") || strings.HasPrefix(lower, "procedure")) && strings.Contains(l, "("):
				name, params, ret := parseFuncHeader(l)
				if strings.HasPrefix(lower, "procedure") {
					ret = ""
				}
				if name != "" {
					funLine := "fun " + name + "(" + params + ")"
					if ret != "" {
						funLine += ": " + ret
					}
					funLine += " {"
					out = append(out, funLine)
					inFunc = true
				}
			case strings.HasPrefix(lower, "var") && strings.Contains(l, ":"):
				rest := strings.TrimSpace(strings.TrimPrefix(l, "var"))
				rest = strings.TrimSuffix(rest, ";")
				if idx := strings.Index(rest, ":"); idx != -1 {
					name := strings.TrimSpace(rest[:idx])
					typ := toMochiType(strings.TrimSpace(rest[idx+1:]))
					if name != "" {
						if typ != "" {
							out = append(out, "let "+name+": "+typ)
						} else {
							out = append(out, "let "+name)
						}
					}
				}
			case lower == "begin":
				inBody = true
			}
			continue
		}
		if lower == "end." || (lower == "end;" && !inLoop && !inFunc) {
			if inFunc {
				out = append(out, "}")
				inFunc = false
			}
			if inLoop {
				out = append(out, "}")
				inLoop = false
			}
			if lower == "end." {
				inBody = false
			}
			continue
		}
		if l == "" {
			continue
		}
		switch {
		case strings.HasPrefix(lower, "for ") && strings.Contains(lower, " in ") && strings.HasSuffix(lower, " do"):
			parts := strings.SplitN(l[len("for "):], " in ", 2)
			varName := strings.TrimSpace(parts[0])
			iter := strings.TrimSpace(strings.TrimSuffix(parts[1], " do"))
			out = append(out, fmt.Sprintf("for %s in %s {", varName, iter))
			if i+1 < len(lines) && strings.TrimSpace(strings.ToLower(lines[i+1])) == "begin" {
				i++
			}
			inLoop = true
		case strings.HasPrefix(lower, "for ") && strings.Contains(lower, ":=") && strings.Contains(lower, " to "):
			varName := strings.TrimSpace(l[len("for "):strings.Index(l, ":=")])
			rest := l[strings.Index(l, ":=")+2:]
			toIdx := strings.Index(strings.ToLower(rest), " to ")
			startExpr := strings.TrimSpace(rest[:toIdx])
			rest = rest[toIdx+4:]
			doIdx := strings.Index(strings.ToLower(rest), " do")
			endExpr := strings.TrimSpace(rest[:doIdx])
			out = append(out, fmt.Sprintf("for %s in %s..%s {", varName, startExpr, endExpr))
			if i+1 < len(lines) && strings.TrimSpace(strings.ToLower(lines[i+1])) == "begin" {
				i++
			}
			inLoop = true
		case strings.HasPrefix(lower, "while ") && strings.Contains(lower, " do"):
			doIdx := strings.LastIndex(lower, " do")
			cond := strings.TrimSpace(l[len("while "):doIdx])
			out = append(out, fmt.Sprintf("while %s {", cond))
			if i+1 < len(lines) && strings.TrimSpace(strings.ToLower(lines[i+1])) == "begin" {
				i++
			}
			inLoop = true
		case lower == "repeat":
			out = append(out, "while true {")
			inLoop = true
			inRepeat = true
		case strings.HasPrefix(lower, "until ") && inRepeat:
			cond := strings.TrimSuffix(strings.TrimSpace(l[len("until "):]), ";")
			out = append(out, fmt.Sprintf("if %s { break }", cond))
			out = append(out, "}")
			inLoop = false
			inRepeat = false
		case lower == "end;" && inLoop:
			out = append(out, "}")
			inLoop = false
		case lower == "begin" && inFunc:
			// skip function begin
			continue
		case strings.HasPrefix(lower, "if ") && strings.HasSuffix(lower, " continue;"):
			cond := strings.TrimPrefix(l, "if ")
			cond = strings.TrimSuffix(cond, "continue;")
			cond = strings.TrimSuffix(strings.TrimSpace(cond), "then")
			cond = strings.TrimSpace(cond)
			if strings.HasPrefix(strings.ToLower(cond), "not ") {
				body := strings.TrimSpace(cond[4:])
				for strings.HasPrefix(body, "(") && strings.HasSuffix(body, ")") {
					body = strings.TrimSuffix(strings.TrimPrefix(body, "("), ")")
					body = strings.TrimSpace(body)
				}
				cond = "!(" + body + ")"
			}
			out = append(out, fmt.Sprintf("if %s { continue }", cond))
		case strings.HasPrefix(lower, "writeln("):
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "writeln("), ");")
			out = append(out, "print("+expr+")")
		case strings.Contains(l, ":="):
			parts := strings.SplitN(l, ":=", 2)
			name := strings.TrimSpace(parts[0])
			expr := strings.TrimSpace(strings.TrimSuffix(parts[1], ";"))
			out = append(out, name+" = "+expr)
		case lower == "exit;" && inFunc:
			out = append(out, "return")
		}
	}
	if len(out) == 0 {
		return nil, fmt.Errorf("convert failure: could not parse Pascal source\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(strings.Join(out, "\n")), nil
}
