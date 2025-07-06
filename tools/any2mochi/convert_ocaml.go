package any2mochi

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

// ConvertOcaml converts ocaml source code to Mochi using the language server.
func ConvertOcaml(src string) ([]byte, error) {
	ls := Servers["ocaml"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	lines := strings.Split(src, "\n")
	writeOcamlSymbols(&out, nil, syms, lines, src, ls)
	if out.Len() == 0 {
		// fall back to a very small regex based parser for simple scripts
		for _, l := range fallbackOcaml(lines) {
			out.WriteString(l)
			out.WriteByte('\n')
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertOcamlFile reads the ocaml file and converts it to Mochi.
func ConvertOcamlFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertOcaml(string(data))
}

func writeOcamlSymbols(out *strings.Builder, prefix []string, syms []DocumentSymbol, lines []string, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case SymbolKindFunction, SymbolKindMethod:
			names := parseOcamlParamNames(lines, s)
			types, ret := ocamlHoverSignature(src, s, ls)
			body := parseOcamlBody(lines, s)
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			for i, n := range names {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(n)
				if i < len(types) && types[i] != "" {
					out.WriteString(": ")
					out.WriteString(types[i])
				}
			}
			out.WriteByte(')')
			if ret != "" && ret != "unit" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
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
		case SymbolKindVariable, SymbolKindConstant:
			if len(prefix) == 0 {
				typ := ocamlHoverType(src, s, ls)
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				if typ != "" && typ != "unit" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		case SymbolKindStruct, SymbolKindClass, SymbolKindInterface, SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []DocumentSymbol{}
			for _, c := range s.Children {
				if c.Kind == SymbolKindField || c.Kind == SymbolKindProperty {
					fields = append(fields, c)
				}
			}
			if len(fields) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, f := range fields {
					out.WriteString("  ")
					out.WriteString(f.Name)
					if typ := ocamlHoverType(src, f, ls); typ != "" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case SymbolKindModule, SymbolKindNamespace:
			writeOcamlSymbols(out, nameParts, s.Children, lines, src, ls)
		}
		if len(s.Children) > 0 && s.Kind != SymbolKindStruct && s.Kind != SymbolKindClass && s.Kind != SymbolKindInterface && s.Kind != SymbolKindEnum && s.Kind != SymbolKindModule && s.Kind != SymbolKindNamespace && s.Kind != SymbolKindVariable && s.Kind != SymbolKindConstant {
			writeOcamlSymbols(out, nameParts, s.Children, lines, src, ls)
		}
	}
}

func parseOcamlParamNames(lines []string, sym DocumentSymbol) []string {
	start := posToOffset(lines, sym.SelectionRange.End)
	src := strings.Join(lines, "\n")
	rest := src[start:]
	eq := strings.Index(rest, "=")
	if eq == -1 {
		eq = len(rest)
	}
	header := strings.TrimSpace(rest[:eq])
	if idx := strings.LastIndex(header, ":"); idx != -1 {
		if idx > strings.LastIndex(header, ")") {
			header = strings.TrimSpace(header[:idx])
		}
	}
	fields := strings.Fields(header)
	var names []string
	for _, f := range fields {
		if strings.HasPrefix(f, "(") {
			f = strings.TrimPrefix(f, "(")
			if i := strings.Index(f, ")"); i != -1 {
				f = f[:i]
			}
			if i := strings.Index(f, ":"); i != -1 {
				f = f[:i]
			}
			f = strings.TrimSpace(f)
			if f != "" {
				names = append(names, f)
			}
		} else if strings.HasPrefix(f, ":") {
			break
		} else {
			if i := strings.Index(f, ":"); i != -1 {
				f = f[:i]
			}
			names = append(names, f)
		}
	}
	return names
}

func ocamlHoverSignature(src string, sym DocumentSymbol, ls LanguageServer) ([]string, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	mc, ok := hov.Contents.(MarkupContent)
	if !ok {
		return nil, ""
	}
	sig := strings.TrimSpace(mc.Value)
	if strings.HasPrefix(sig, "val") {
		if idx := strings.Index(sig, ":"); idx != -1 {
			sig = strings.TrimSpace(sig[idx+1:])
		}
	}
	parts := strings.Split(sig, "->")
	for i := range parts {
		parts[i] = strings.TrimSpace(parts[i])
	}
	if len(parts) == 0 {
		return nil, ""
	}
	ret := mapOcamlType(parts[len(parts)-1])
	paramTypes := make([]string, 0, len(parts)-1)
	for i := 0; i < len(parts)-1; i++ {
		paramTypes = append(paramTypes, mapOcamlType(parts[i]))
	}
	return paramTypes, ret
}

func ocamlHoverType(src string, sym DocumentSymbol, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(MarkupContent); ok {
		return mapOcamlType(strings.TrimSpace(mc.Value))
	}
	return ""
}

func mapOcamlType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "", "unit":
		return ""
	case "int":
		return "int"
	case "float":
		return "float"
	case "string":
		return "string"
	case "bool":
		return "bool"
	}
	if strings.HasSuffix(t, " list") {
		inner := mapOcamlType(strings.TrimSpace(t[:len(t)-5]))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasSuffix(t, " array") {
		inner := mapOcamlType(strings.TrimSpace(t[:len(t)-6]))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasSuffix(t, " option") {
		return mapOcamlType(strings.TrimSpace(t[:len(t)-7]))
	}
	return t
}

func posToOffset(lines []string, pos Position) int {
	off := 0
	for i := 0; i < int(pos.Line); i++ {
		off += len(lines[i]) + 1
	}
	off += int(pos.Character)
	return off
}

func parseOcamlBody(lines []string, sym DocumentSymbol) []string {
	start := posToOffset(lines, sym.SelectionRange.End)
	end := posToOffset(lines, sym.Range.End)
	src := strings.Join(lines, "\n")
	if start >= len(src) {
		return nil
	}
	if end > len(src) {
		end = len(src)
	}
	body := src[start:end]
	if idx := strings.Index(body, "="); idx != -1 {
		body = body[idx+1:]
	}
	body = strings.TrimSpace(body)
	body = strings.TrimSuffix(body, ";;")
	body = strings.ReplaceAll(body, "print_endline", "print")
	body = strings.ReplaceAll(body, "string_of_int", "str")
	body = strings.ReplaceAll(body, "string_of_float", "str")
	body = strings.ReplaceAll(body, "string_of_bool", "str")
	body = strings.ReplaceAll(body, ":=", "=")
	body = strings.ReplaceAll(body, "!", "")
	body = strings.ReplaceAll(body, ";", "")
	var out []string
	for _, l := range strings.Split(body, "\n") {
		l = strings.TrimSpace(l)
		if l != "" {
			out = append(out, l)
		}
	}
	return out
}

func fallbackOcaml(lines []string) []string {
	var out []string
	reLet := regexp.MustCompile(`^let\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*(.*)`)
	for _, line := range lines {
		t := strings.TrimSpace(line)
		if m := reLet.FindStringSubmatch(t); m != nil {
			expr := strings.TrimSuffix(m[2], ";;")
			expr = strings.ReplaceAll(expr, ";", ",")
			out = append(out, fmt.Sprintf("let %s = %s", m[1], strings.TrimSpace(expr)))
			continue
		}
		if strings.HasPrefix(t, "print_endline") {
			t = strings.ReplaceAll(t, "print_endline", "print")
			t = strings.ReplaceAll(t, "string_of_int", "str")
			t = strings.ReplaceAll(t, "string_of_float", "str")
			t = strings.ReplaceAll(t, "string_of_bool", "str")
			t = strings.TrimSuffix(t, ";;")
			out = append(out, t)
		}
	}
	return out
}
