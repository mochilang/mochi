package any2mochi

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertEx converts Elixir source code to Mochi using the language server
// for basic parsing. It supports a very small subset of Elixir consisting of
// simple function definitions, assignments and IO.puts statements.
func ConvertEx(src string) ([]byte, error) {
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
	m := regexp.MustCompile(`def\s+` + regexp.QuoteMeta(name) + `\(([^)]*)\)?`).FindStringSubmatch(header)
	if len(m) > 1 {
		params = strings.TrimSpace(m[1])
	}

	var b strings.Builder
	b.WriteString("fun " + name + "(" + params + "): any {\n")

	assignRe := regexp.MustCompile(`^(\w+)\s*=\s*(.+)$`)
	printRe := regexp.MustCompile(`^IO\.puts\((.*)\)$`)
	retRe := regexp.MustCompile(`^throw\s*{\s*:?(?:return|RETURN),\s*(.*)}`)

	inTry := false
	for _, l := range seg[1:] {
		l = strings.TrimSpace(l)
		if l == "end" {
			break
		}
		switch {
		case strings.HasPrefix(l, "try do"):
			inTry = true
		case inTry && retRe.MatchString(l):
			b.WriteString("  return " + retRe.FindStringSubmatch(l)[1] + "\n")
			inTry = false
		case printRe.MatchString(l):
			b.WriteString("  print(" + printRe.FindStringSubmatch(l)[1] + ")\n")
		case assignRe.MatchString(l):
			m := assignRe.FindStringSubmatch(l)
			b.WriteString("  let " + m[1] + " = " + m[2] + "\n")
		}
	}
	b.WriteString("}")
	return b.String()
}
