package any2mochi

import (
	"fmt"
	"os"
	"strings"
)

// ConvertPas converts pas source code to Mochi using the language server.
func ConvertPas(src string) ([]byte, error) {
	ls := Servers["pas"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		// fall back to very small regex based parser when pasls is missing
		return convertPasFallback(src)
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writePasSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertPasFile reads the pas file and converts it to Mochi.
func ConvertPasFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPas(string(data))
}

type pasParam struct {
	name string
	typ  string
}

func writePasSymbols(out *strings.Builder, prefix []string, syms []DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case SymbolKindFunction, SymbolKindMethod:
			params, ret := pasHoverSignature(src, s, ls)
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
			body := convertPasBody(src, s)
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
		case SymbolKindStruct, SymbolKindClass, SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []DocumentSymbol{}
			methods := []DocumentSymbol{}
			for _, c := range s.Children {
				switch c.Kind {
				case SymbolKindField, SymbolKindProperty:
					fields = append(fields, c)
				case SymbolKindFunction, SymbolKindMethod:
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
					if typ := pasFieldType(src, f, ls); typ != "" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
				for _, m := range methods {
					var b strings.Builder
					writePasSymbols(&b, []string{m.Name}, []DocumentSymbol{m}, src, ls)
					for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
						out.WriteString("  ")
						out.WriteString(line)
						out.WriteByte('\n')
					}
				}
				out.WriteString("}\n")
			}
		case SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == SymbolKindEnumMember {
					out.WriteString("  ")
					out.WriteString(c.Name)
					out.WriteByte('\n')
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
				writePasSymbols(out, nameParts, rest, src, ls)
			}
			continue
		case SymbolKindVariable, SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(s.Name)
				if typ := pasFieldType(src, s, ls); typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		}
		if len(s.Children) > 0 && s.Kind != SymbolKindStruct && s.Kind != SymbolKindClass && s.Kind != SymbolKindInterface {
			writePasSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func pasHoverSignature(src string, sym DocumentSymbol, ls LanguageServer) ([]pasParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		if sym.Detail != nil {
			return parsePasSignature(*sym.Detail)
		}
		return nil, ""
	}
	lines := strings.Split(hoverString(hov), "\n")
	for _, l := range lines {
		l = strings.TrimSpace(l)
		lower := strings.ToLower(l)
		if strings.HasPrefix(lower, "function") || strings.HasPrefix(lower, "procedure") {
			return parsePasSignature(l)
		}
	}
	if sym.Detail != nil {
		return parsePasSignature(*sym.Detail)
	}
	return nil, ""
}

func pasFieldType(src string, sym DocumentSymbol, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	lines := strings.Split(hoverString(hov), "\n")
	for _, l := range lines {
		l = strings.TrimSpace(l)
		if idx := strings.Index(l, ":"); idx != -1 {
			t := strings.TrimSpace(l[idx+1:])
			if t != "" {
				return pasToMochiType(t)
			}
		}
	}
	return ""
}

func parsePasSignature(sig string) ([]pasParam, string) {
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
	var params []pasParam
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
		typ := pasToMochiType(strings.TrimSpace(p[colon+1:]))
		for _, n := range names {
			n = strings.TrimSpace(n)
			if n == "" {
				continue
			}
			params = append(params, pasParam{name: n, typ: typ})
		}
	}
	ret := ""
	if rest != "" {
		if strings.HasPrefix(rest, ":") {
			rest = strings.TrimSpace(rest[1:])
		}
		ret = pasToMochiType(strings.TrimSpace(rest))
	}
	return params, ret
}

func pasToMochiType(t string) string {
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
		inner := pasToMochiType(t[len("specialize tarray<") : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "array of ") {
		inner := pasToMochiType(strings.TrimPrefix(t, "array of "))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return t
}

// convertPasBody extracts the function body for sym from src and converts a few
// basic statements into Mochi equivalents. Only very simple constructs like
// assignments, returns and writeln calls are handled.
func convertPasBody(src string, sym DocumentSymbol) string {
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

// convertPasFallback converts a small subset of Pascal syntax to Mochi when no
// language server is available. It recognises simple type declarations,
// variable definitions and statements inside the main program block.
func convertPasFallback(src string) ([]byte, error) {
	lines := strings.Split(src, "\n")
	var out []string
	inBody := false
	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		lower := strings.ToLower(l)
		if !inBody {
			switch {
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
			case strings.HasPrefix(lower, "var") && strings.Contains(l, ":"):
				rest := strings.TrimSpace(strings.TrimPrefix(l, "var"))
				rest = strings.TrimSuffix(rest, ";")
				if idx := strings.Index(rest, ":"); idx != -1 {
					name := strings.TrimSpace(rest[:idx])
					typ := pasToMochiType(strings.TrimSpace(rest[idx+1:]))
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
		if lower == "end." || lower == "end;" {
			inBody = false
			continue
		}
		if l == "" {
			continue
		}
		switch {
		case strings.HasPrefix(lower, "writeln("):
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "writeln("), ");")
			out = append(out, "print("+expr+")")
		case strings.Contains(l, ":="):
			parts := strings.SplitN(l, ":=", 2)
			name := strings.TrimSpace(parts[0])
			expr := strings.TrimSpace(strings.TrimSuffix(parts[1], ";"))
			out = append(out, name+" = "+expr)
		}
	}
	if len(out) == 0 {
		return nil, fmt.Errorf("convert failure: no convertible content")
	}
	return []byte(strings.Join(out, "\n")), nil
}
