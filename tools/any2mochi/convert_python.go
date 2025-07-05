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
	writePySymbols(&out, nil, syms)

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

func writePySymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			writePyFunc(out, strings.Join(nameParts, "."), s)
		case protocol.SymbolKindClass:
			writePyClass(out, nameParts, s)
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				out.WriteString("\n")
			}
		}
	}
}

func writePyFunc(out *strings.Builder, name string, sym protocol.DocumentSymbol) {
	params := extractPyParams(sym)
	out.WriteString("fun ")
	out.WriteString(name)
	out.WriteByte('(')
	for i, p := range params {
		if i > 0 {
			out.WriteString(", ")
		}
		out.WriteString(p)
	}
	out.WriteString(") {}\n")
}

func writePyClass(out *strings.Builder, prefix []string, sym protocol.DocumentSymbol) {
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
		writePyFunc(&b, m.Name, m)
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
