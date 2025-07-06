package ocaml

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

// Convert converts OCaml source code to Mochi. It prefers using the
// language server when available and falls back to a small parser
// implemented with tree-sitter otherwise.
func Convert(src string) ([]byte, error) {
	if !parent.UseLSP {
		prog, err := parse(src)
		if err != nil {
			return nil, err
		}
		code := formatProgram(prog)
		if len(code) == 0 {
			return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
		}
		return code, nil
	}

	ls := parent.Servers["ocaml"]
	syms, diags, err := parent.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", diagnostics(src, diags))
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
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the OCaml file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

func writeOcamlSymbols(out *strings.Builder, prefix []string, syms []parent.DocumentSymbol, lines []string, src string, ls parent.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case parent.SymbolKindFunction, parent.SymbolKindMethod:
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
		case parent.SymbolKindVariable, parent.SymbolKindConstant:
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
		case parent.SymbolKindStruct, parent.SymbolKindClass, parent.SymbolKindInterface, parent.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []parent.DocumentSymbol{}
			for _, c := range s.Children {
				if c.Kind == parent.SymbolKindField || c.Kind == parent.SymbolKindProperty {
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
		case parent.SymbolKindModule, parent.SymbolKindNamespace:
			writeOcamlSymbols(out, nameParts, s.Children, lines, src, ls)
		}
		if len(s.Children) > 0 && s.Kind != parent.SymbolKindStruct && s.Kind != parent.SymbolKindClass && s.Kind != parent.SymbolKindInterface && s.Kind != parent.SymbolKindEnum && s.Kind != parent.SymbolKindModule && s.Kind != parent.SymbolKindNamespace && s.Kind != parent.SymbolKindVariable && s.Kind != parent.SymbolKindConstant {
			writeOcamlSymbols(out, nameParts, s.Children, lines, src, ls)
		}
	}
}

func parseOcamlParamNames(lines []string, sym parent.DocumentSymbol) []string {
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

func ocamlHoverSignature(src string, sym parent.DocumentSymbol, ls parent.LanguageServer) ([]string, string) {
	hov, err := parent.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	mc, ok := hov.Contents.(parent.MarkupContent)
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

func ocamlHoverType(src string, sym parent.DocumentSymbol, ls parent.LanguageServer) string {
	hov, err := parent.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(parent.MarkupContent); ok {
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

func posToOffset(lines []string, pos parent.Position) int {
	off := 0
	for i := 0; i < int(pos.Line); i++ {
		off += len(lines[i]) + 1
	}
	off += int(pos.Character)
	return off
}

func parseOcamlBody(lines []string, sym parent.DocumentSymbol) []string {
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
