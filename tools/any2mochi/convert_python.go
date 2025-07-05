package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"

	pycode "mochi/compile/py"
)

// ConvertPython converts Python source code to a minimal Mochi representation
// using the Python language server.
func ConvertPython(src string) ([]byte, error) {
	_ = pycode.EnsurePyright()
	ls := Servers["python"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	var out strings.Builder
	writePySymbols(&out, nil, syms, src, ls)

	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertPythonFile reads the Python file at path and converts it to Mochi.
func ConvertPythonFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPython(string(data))
}

func writePySymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			writePyFunc(out, strings.Join(nameParts, "."), s, src, ls)
		case protocol.SymbolKindClass:
			writePyClass(out, nameParts, s, src, ls)
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				if typ := getPyVarType(src, s.SelectionRange.Start, ls); typ != "" && typ != "Unknown" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteString("\n")
			}
		}
	}
}

type pyParam struct {
	name string
	typ  string
}

func writePyFunc(out *strings.Builder, name string, sym protocol.DocumentSymbol, src string, ls LanguageServer) {
	params, ret := getPySignature(src, sym.SelectionRange.Start, ls)
	if len(params) == 0 {
		for _, p := range extractPyParams(sym) {
			params = append(params, pyParam{name: p})
		}
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
			out.WriteString(p.typ)
		}
	}
	out.WriteByte(')')
	if ret != "" && ret != "None" {
		out.WriteString(": ")
		out.WriteString(ret)
	}
	out.WriteString(" {\n")
	for _, line := range extractPyBody(src, sym) {
		out.WriteString("  // ")
		out.WriteString(strings.TrimSpace(line))
		out.WriteByte('\n')
	}
	out.WriteString("}\n")
}

func writePyClass(out *strings.Builder, prefix []string, sym protocol.DocumentSymbol, src string, ls LanguageServer) {
	name := strings.Join(prefix, ".")
	fields := extractPyFields(sym)
	methods := make([]protocol.DocumentSymbol, 0)
	for _, c := range sym.Children {
		switch c.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			methods = append(methods, c)
		}
	}
	out.WriteString("type ")
	out.WriteString(name)
	if len(fields) == 0 && len(methods) == 0 {
		out.WriteString(" {}\n")
		return
	}
	out.WriteString(" {\n")
	for _, f := range fields {
		out.WriteString("  ")
		out.WriteString(f.name)
		if typ := getPyVarType(src, f.sym.SelectionRange.Start, ls); typ != "" && typ != "Unknown" {
			out.WriteString(": ")
			out.WriteString(typ)
		}
		out.WriteByte('\n')
	}
	for _, m := range methods {
		var b strings.Builder
		writePyFunc(&b, m.Name, m, src, ls)
		for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
			out.WriteString("  ")
			out.WriteString(line)
			out.WriteByte('\n')
		}
	}
	out.WriteString("}\n")
}

type pyField struct {
	name string
	sym  protocol.DocumentSymbol
}

func extractPyFields(sym protocol.DocumentSymbol) []pyField {
	var fields []pyField
	for _, c := range sym.Children {
		if c.Kind == protocol.SymbolKindVariable || c.Kind == protocol.SymbolKindConstant {
			fields = append(fields, pyField{name: c.Name, sym: c})
		}
	}
	return fields
}

func extractPyParams(sym protocol.DocumentSymbol) []string {
	start := sym.Range.Start.Line
	var params []string
	for _, c := range sym.Children {
		if c.Kind == protocol.SymbolKindVariable && c.Range.Start.Line == start {
			params = append(params, c.Name)
		}
	}
	return params
}

func getPySignature(src string, pos protocol.Position, ls LanguageServer) ([]pyParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return nil, ""
	}
	mc, ok := hov.Contents.(protocol.MarkupContent)
	if !ok {
		return nil, ""
	}
	return parsePySignature(mc.Value)
}

func parsePySignature(sig string) ([]pyParam, string) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	if i := strings.Index(sig, "def "); i != -1 {
		sig = sig[i+4:]
	}
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := strings.TrimSpace(sig[open+1 : close])
	rest := strings.TrimSpace(sig[close+1:])
	ret := ""
	if strings.HasPrefix(rest, "->") {
		ret = mapPyType(strings.TrimSpace(rest[2:]))
	}
	var params []pyParam
	if paramsPart != "" {
		parts := strings.Split(paramsPart, ",")
		for i, p := range parts {
			p = strings.TrimSpace(p)
			if p == "" || strings.HasPrefix(p, "self") && i == 0 {
				continue
			}
			name := p
			typ := ""
			if colon := strings.Index(p, ":"); colon != -1 {
				name = strings.TrimSpace(p[:colon])
				typ = strings.TrimSpace(p[colon+1:])
				if eq := strings.Index(typ, "="); eq != -1 {
					typ = strings.TrimSpace(typ[:eq])
				}
				typ = mapPyType(typ)
			}
			params = append(params, pyParam{name: name, typ: typ})
		}
	}
	return params, ret
}

func mapPyType(t string) string {
	t = strings.TrimSpace(t)
	if t == "" || t == "None" || t == "Any" || t == "Unknown" {
		return ""
	}
	if strings.Contains(t, "|") {
		var parts []string
		for _, p := range splitPyArgs(strings.ReplaceAll(t, "|", ",")) {
			p = strings.TrimSpace(p)
			if p == "None" || p == "" {
				continue
			}
			if mp := mapPyType(p); mp != "" {
				parts = append(parts, mp)
			}
		}
		if len(parts) == 1 {
			return parts[0]
		}
		if len(parts) > 1 {
			return strings.Join(parts, " | ")
		}
		return ""
	}
	if idx := strings.LastIndex(t, "."); idx != -1 {
		t = t[idx+1:]
	}
	switch t {
	case "int":
		return "int"
	case "float":
		return "float"
	case "str":
		return "string"
	case "bool":
		return "bool"
	}
	if strings.HasSuffix(t, "]") {
		open := strings.Index(t, "[")
		if open != -1 {
			outer := strings.TrimSpace(t[:open])
			inner := t[open+1 : len(t)-1]
			args := splitPyArgs(inner)
			switch outer {
			case "list", "List", "Sequence", "Iterable":
				innerType := "any"
				if len(args) > 0 {
					if a := mapPyType(args[0]); a != "" {
						innerType = a
					}
				}
				return "list<" + innerType + ">"
			case "dict", "Dict", "Mapping":
				key := "any"
				val := "any"
				if len(args) > 0 {
					if k := mapPyType(args[0]); k != "" {
						key = k
					}
				}
				if len(args) > 1 {
					if v := mapPyType(args[1]); v != "" {
						val = v
					}
				}
				return "map<" + key + ", " + val + ">"
			case "set", "Set":
				innerType := "any"
				if len(args) > 0 {
					if a := mapPyType(args[0]); a != "" {
						innerType = a
					}
				}
				return "set<" + innerType + ">"
			case "tuple", "Tuple":
				var mapped []string
				for _, a := range args {
					mt := mapPyType(a)
					if mt == "" {
						mt = "any"
					}
					mapped = append(mapped, mt)
				}
				if len(mapped) > 0 {
					return "tuple<" + strings.Join(mapped, ", ") + ">"
				}
			case "Union":
				var mapped []string
				for _, a := range args {
					mt := mapPyType(a)
					if mt != "" {
						mapped = append(mapped, mt)
					}
				}
				if len(mapped) == 1 {
					return mapped[0]
				}
				if len(mapped) > 1 {
					return strings.Join(mapped, " | ")
				}
				return ""
			case "Optional":
				if len(args) == 1 {
					mt := mapPyType(args[0])
					if mt == "" {
						mt = "any"
					}
					return mt + "?"
				}
			case "Callable":
				if len(args) == 2 {
					argList := strings.Trim(args[0], "[]")
					ret := mapPyType(args[1])
					var argTypes []string
					if strings.TrimSpace(argList) != "" && argList != "..." {
						for _, a := range splitPyArgs(argList) {
							at := mapPyType(a)
							if at == "" {
								at = "any"
							}
							argTypes = append(argTypes, at)
						}
					}
					if ret == "" {
						ret = "any"
					}
					return "fun(" + strings.Join(argTypes, ", ") + "): " + ret
				}
			}
		}
	}
	return t
}

func splitPyArgs(s string) []string {
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

func getPyVarType(src string, pos protocol.Position, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return ""
	}
	mc, ok := hov.Contents.(protocol.MarkupContent)
	if !ok {
		return ""
	}
	return parsePyVarType(mc.Value)
}

func parsePyVarType(hov string) string {
	if i := strings.Index(hov, "\n"); i != -1 {
		hov = hov[:i]
	}
	if colon := strings.Index(hov, ":"); colon != -1 {
		typ := strings.TrimSpace(hov[colon+1:])
		return mapPyType(typ)
	}
	return ""
}

func extractPyBody(src string, sym protocol.DocumentSymbol) []string {
	lines := strings.Split(src, "\n")
	start := int(sym.Range.Start.Line) + 1
	end := int(sym.Range.End.Line)
	if start >= len(lines) || start > end {
		return nil
	}
	if end >= len(lines) {
		end = len(lines) - 1
	}
	body := make([]string, 0, end-start+1)
	indent := 0
	for i := start; i <= end; i++ {
		l := lines[i]
		if strings.TrimSpace(l) == "" {
			body = append(body, "")
			continue
		}
		if indent == 0 {
			for j, r := range l {
				if r != ' ' && r != '\t' {
					indent = j
					break
				}
			}
		}
		if len(l) >= indent {
			l = l[indent:]
		}
		body = append(body, l)
	}
	return body
}
