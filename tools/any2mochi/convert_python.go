package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"

	pycode "mochi/compile/py"
)

type pyParam struct {
	name string
	typ  string
}

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
	writePySymbols(&out, src, ls, nil, syms)

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

func writePySymbols(out *strings.Builder, src string, ls LanguageServer, prefix []string, syms []protocol.DocumentSymbol) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			writePyFunc(out, src, ls, strings.Join(nameParts, "."), s)
		case protocol.SymbolKindClass:
			writePyClass(out, src, ls, nameParts, s)
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				if typ := getPyVarType(src, ls, s); typ != "" && typ != "None" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteString("\n")
			}
		}
	}
}

func writePyFunc(out *strings.Builder, src string, ls LanguageServer, name string, sym protocol.DocumentSymbol) {
	var params []pyParam
	var ret string
	if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start); err == nil {
		params, ret = parsePyFuncHover(hoverString(hov))
	}
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

func writePyClass(out *strings.Builder, src string, ls LanguageServer, prefix []string, sym protocol.DocumentSymbol) {
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
		out.WriteString(f.Name)
		if typ := getPyVarType(src, ls, f); typ != "" && typ != "None" {
			out.WriteString(": ")
			out.WriteString(typ)
		}
		out.WriteByte('\n')
	}
	for _, m := range methods {
		var b strings.Builder
		writePyFunc(&b, src, ls, m.Name, m)
		for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
			out.WriteString("  ")
			out.WriteString(line)
			out.WriteByte('\n')
		}
	}
	out.WriteString("}\n")
}

func extractPyFields(sym protocol.DocumentSymbol) []protocol.DocumentSymbol {
	var fields []protocol.DocumentSymbol
	for _, c := range sym.Children {
		if c.Kind == protocol.SymbolKindVariable || c.Kind == protocol.SymbolKindConstant {
			fields = append(fields, c)
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

func getPyVarType(src string, ls LanguageServer, sym protocol.DocumentSymbol) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	return parsePyVarHover(hoverString(hov))
}

func parsePyVarHover(hov string) string {
	if idx := strings.Index(hov, ":"); idx != -1 {
		typ := strings.TrimSpace(hov[idx+1:])
		return strings.Split(typ, "\n")[0]
	}
	return ""
}

func parsePyFuncHover(hov string) ([]pyParam, string) {
	if i := strings.Index(hov, "def "); i != -1 {
		hov = hov[i+4:]
	}
	hov = strings.ReplaceAll(hov, "\r", "")
	hov = strings.ReplaceAll(hov, "\n", " ")
	open := strings.Index(hov, "(")
	close := strings.LastIndex(hov, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := hov[open+1 : close]
	tail := strings.TrimSpace(hov[close+1:])
	tail = strings.TrimPrefix(tail, "->")
	ret := strings.TrimSpace(tail)
	var params []pyParam
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		name := p
		typ := ""
		if idx := strings.Index(p, ":"); idx != -1 {
			name = strings.TrimSpace(p[:idx])
			typ = strings.TrimSpace(p[idx+1:])
			if eq := strings.Index(typ, "="); eq != -1 {
				typ = strings.TrimSpace(typ[:eq])
			}
		}
		params = append(params, pyParam{name: name, typ: typ})
	}
	return params, strings.Split(ret, " ")[0]
}
