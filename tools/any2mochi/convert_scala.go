package any2mochi

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertScala converts scala source code to Mochi using the language server.
func ConvertScala(src string) ([]byte, error) {
	ls := Servers["scala"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	lines := strings.Split(strings.ReplaceAll(src, "\r\n", "\n"), "\n")
	var out strings.Builder
	for _, s := range syms {
		if s.Kind != protocol.SymbolKindFunction {
			continue
		}
		code := convertScalaFunc(lines, s)
		if code == "" {
			continue
		}
		out.WriteString(code)
		out.WriteByte('\n')
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(strings.TrimSuffix(out.String(), "\n")), nil
}

// ConvertScalaFile reads the scala file and converts it to Mochi.
func ConvertScalaFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertScala(string(data))
}

func convertScalaFunc(lines []string, sym protocol.DocumentSymbol) string {
	start := int(sym.Range.Start.Line)
	end := int(sym.Range.End.Line)
	if start < 0 || end >= len(lines) || start >= end {
		return ""
	}
	seg := lines[start : end+1]
	header := strings.TrimSpace(seg[0])

	name := sym.Name
	params := ""
	reHeader := regexp.MustCompile(fmt.Sprintf(`def\s+%s\(([^)]*)\)`, regexp.QuoteMeta(name)))
	if m := reHeader.FindStringSubmatch(header); len(m) > 1 {
		parts := strings.Split(m[1], ",")
		for i, p := range parts {
			fields := strings.Fields(strings.TrimSpace(p))
			if len(fields) > 0 {
				if i > 0 {
					params += ", "
				}
				params += fields[0]
			}
		}
	}

	var b strings.Builder
	b.WriteString("fun ")
	b.WriteString(name)
	b.WriteString("(")
	b.WriteString(params)
	b.WriteString(") {\n")
	for _, l := range seg[1:] {
		l = strings.TrimSpace(l)
		if l == "}" {
			break
		}
		switch {
		case strings.HasPrefix(l, "println("):
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "println("), ")")
			b.WriteString("  print(" + convertScalaExpr(expr) + ")\n")
		case strings.HasPrefix(l, "return "):
			expr := strings.TrimSpace(strings.TrimPrefix(l, "return "))
			b.WriteString("  return " + convertScalaExpr(expr) + "\n")
		case strings.HasPrefix(l, "var "):
			b.WriteString("  var " + strings.TrimPrefix(l, "var ") + "\n")
		case strings.HasPrefix(l, "val "):
			if idx := strings.Index(l, "="); idx != -1 {
				name := strings.Fields(strings.TrimSpace(l[4:idx]))[0]
				expr := strings.TrimSpace(l[idx+1:])
				b.WriteString("  let " + name + " = " + convertScalaExpr(expr) + "\n")
			}
		}
	}
	b.WriteString("}")
	return b.String()
}

func convertScalaExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	if strings.HasSuffix(expr, ".toString()") {
		expr = "str(" + strings.TrimSuffix(expr, ".toString()") + ")"
	}
	reArr := regexp.MustCompile(`scala\.collection\.mutable\.ArrayBuffer\((.*)\)`)
	if m := reArr.FindStringSubmatch(expr); m != nil {
		expr = "[" + m[1] + "]"
	}
	if strings.HasSuffix(expr, ".length") {
		expr = "len(" + strings.TrimSuffix(expr, ".length") + ")"
	}
	return expr
}
