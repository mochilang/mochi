package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
	excode "mochi/compile/x/ex"
)

// ConvertEx converts Elixir source code to Mochi using the language server
// for basic parsing. It supports a very small subset of Elixir consisting of
// simple function definitions, assignments and IO.puts statements.
func ConvertEx(src string) ([]byte, error) {
	_ = excode.Ensure()
	ls := Servers["ex"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	lines := strings.Split(src, "\n")
	var out strings.Builder
	foundMain := false
	for _, s := range syms {
		if s.Kind != protocol.SymbolKindFunction {
			continue
		}
		code := convertExFunc(lines, s)
		if code == "" {
			continue
		}
		if s.Name == "main" {
			foundMain = true
		}
		out.WriteString(code)
		out.WriteByte('\n')
	}
	if foundMain {
		out.WriteString("main()\n")
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertExFile reads the Elixir file at path and converts it to Mochi.
func ConvertExFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertEx(string(data))
}

func convertExFunc(lines []string, sym protocol.DocumentSymbol) string {
	start := int(sym.Range.Start.Line)
	end := int(sym.Range.End.Line)
	if start < 0 || end >= len(lines) || start >= end {
		return ""
	}
	seg := lines[start : end+1]
	header := strings.TrimSpace(seg[0])

	name := sym.Name
	params := ""
	prefix := "def " + name + "("
	if strings.HasPrefix(header, prefix) {
		rest := header[len(prefix):]
		if i := strings.Index(rest, ")"); i >= 0 {
			params = strings.TrimSpace(rest[:i])
		}
	}

	var b strings.Builder
	b.WriteString("fun " + name + "(" + params + "): any {\n")

	retPrefix := "throw {:return,"
	inTry := false
	for _, l := range seg[1:] {
		l = strings.TrimSpace(l)
		if l == "end" {
			break
		}
		switch {
		case strings.HasPrefix(l, "try do"):
			inTry = true
		case inTry && strings.HasPrefix(l, retPrefix):
			val := strings.TrimSuffix(strings.TrimSpace(l[len(retPrefix):]), "}")
			b.WriteString("  return " + strings.TrimSpace(val) + "\n")
			inTry = false
		case strings.HasPrefix(l, "IO.puts(") && strings.HasSuffix(l, ")"):
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "IO.puts("), ")")
			b.WriteString("  print(" + strings.TrimSpace(expr) + ")\n")
		case strings.Contains(l, "="):
			parts := strings.SplitN(l, "=", 2)
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])
			b.WriteString("  let " + left + " = " + right + "\n")
		}
	}
	b.WriteString("}")
	return b.String()
}
