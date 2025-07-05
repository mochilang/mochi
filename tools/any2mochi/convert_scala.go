package any2mochi

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertScala converts scala source code to Mochi using the language server.
func ConvertScala(src string) ([]byte, error) {
	return convertScala(src, "")
}

func convertScala(src, root string) ([]byte, error) {
	ls := Servers["scala"]
	syms, diags, err := EnsureAndParseWithRoot(ls.Command, ls.Args, ls.LangID, src, root)
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
		code := convertScalaFunc(lines, s, root)
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
	return convertScala(string(data), filepath.Dir(path))
}

func convertScalaFunc(lines []string, sym protocol.DocumentSymbol, root string) string {
	start := int(sym.Range.Start.Line)
	end := int(sym.Range.End.Line)
	if start < 0 || end >= len(lines) || start >= end {
		return ""
	}
	seg := lines[start : end+1]
	header := strings.TrimSpace(seg[0])

	name := sym.Name
	params, ret := parseScalaSignature(header, name)
	if len(params) == 0 && sym.Detail != nil {
		params, ret = parseScalaSignature(*sym.Detail, name)
	}
	if len(params) == 0 {
		if hov, err := EnsureAndHoverWithRoot(Servers["scala"].Command, Servers["scala"].Args, Servers["scala"].LangID, strings.Join(lines, "\n"), sym.SelectionRange.Start, root); err == nil {
			if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
				for _, l := range strings.Split(mc.Value, "\n") {
					if strings.Contains(l, name+"(") {
						params, ret = parseScalaSignature(l, name)
						break
					}
				}
			}
		}
	}

	var b strings.Builder
	b.WriteString("fun ")
	b.WriteString(name)
	b.WriteByte('(')
	for i, p := range params {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(p)
	}
	b.WriteString(")")
	if ret != "" && ret != "Unit" {
		b.WriteString(": ")
		b.WriteString(ret)
	}
	b.WriteString(" {\n")
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
	prefix := "scala.collection.mutable.ArrayBuffer("
	if strings.HasPrefix(expr, prefix) && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, prefix), ")")
		expr = "[" + inner + "]"
	}
	if strings.HasSuffix(expr, ".length") {
		expr = "len(" + strings.TrimSuffix(expr, ".length") + ")"
	}
	return expr
}

// parseScalaSignature extracts parameter names and return type from a Scala
// function signature string. The provided name is used to locate the opening
// parenthesis when necessary.
func parseScalaSignature(sig, name string) ([]string, string) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	idx := strings.Index(sig, name+"(")
	if idx == -1 {
		idx = strings.Index(sig, "(")
		if idx == -1 {
			return nil, ""
		}
	} else {
		idx += len(name)
	}
	open := strings.Index(sig[idx:], "(")
	if open == -1 {
		return nil, ""
	}
	open += idx
	close := strings.Index(sig[open:], ")")
	if close == -1 {
		return nil, ""
	}
	close += open
	paramsPart := sig[open+1 : close]
	params := []string{}
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if c := strings.Index(p, ":"); c != -1 {
			p = p[:c]
		}
		params = append(params, strings.TrimSpace(p))
	}
	rest := strings.TrimSpace(sig[close+1:])
	ret := ""
	if strings.HasPrefix(rest, ":") {
		rest = strings.TrimSpace(rest[1:])
		if eq := strings.Index(rest, "="); eq != -1 {
			rest = rest[:eq]
		}
		ret = strings.TrimSpace(rest)
	}
	return params, ret
}
