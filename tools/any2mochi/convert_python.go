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
	out.WriteString(" {}\n")
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
		out.WriteString(f)
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

func extractPyFields(sym protocol.DocumentSymbol) []string {
	var fields []string
	for _, c := range sym.Children {
		if c.Kind == protocol.SymbolKindVariable || c.Kind == protocol.SymbolKindConstant {
			fields = append(fields, c.Name)
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
	switch t {
	case "int":
		return "int"
	case "float":
		return "float"
	case "str":
		return "string"
	case "bool":
		return "bool"
	default:
		return t
	}
}
