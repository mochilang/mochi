package any2mochi

import (
	"fmt"
	"os"
	"strings"

	excode "mochi/compile/x/ex"
)

// ConvertEx converts Elixir source code to Mochi using the language server
// for basic parsing. It supports a small subset of Elixir consisting of
// simple function definitions. When the language server fails to provide
// usable details the converter falls back to light-weight regex matching of
// the source. Basic control flow such as `if`, `for` and `while` blocks is
// recognized along with assignments and IO.puts statements.
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
		if s.Kind != SymbolKindFunction {
			continue
		}
		params, ret := getExSignature(src, s.SelectionRange.Start, ls)
		code := convertExFunc(lines, s, params, ret)
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

func convertExFunc(lines []string, sym DocumentSymbol, params []string, ret string) string {
	start := int(sym.Range.Start.Line)
	end := int(sym.Range.End.Line)
	if start < 0 || end >= len(lines) || start >= end {
		return ""
	}
	seg := lines[start : end+1]
	header := strings.TrimSpace(seg[0])

	name := sym.Name
	paramStr := ""
	if len(params) == 0 {
		prefix := "def " + name + "("
		if strings.HasPrefix(header, prefix) {
			rest := header[len(prefix):]
			if i := strings.Index(rest, ")"); i >= 0 {
				paramStr = strings.TrimSpace(rest[:i])
			}
		}
	} else {
		paramStr = strings.Join(params, ", ")
	}

	var b strings.Builder
	b.WriteString("fun " + name + "(" + paramStr + ")")
	if ret != "" {
		b.WriteString(": " + ret)
	} else {
		b.WriteString(": any")
	}
	b.WriteString(" {\n")

	retPrefix := "throw {:return,"
	level := 1
	inTry := false
	for _, l := range seg[1:] {
		l = strings.TrimSpace(l)
		if l == "catch {:return, v} -> v end" {
			break
		}
		switch {
		case l == "" || strings.HasPrefix(l, "try do"):
			inTry = true
			continue
		case l == "end":
			if level > 1 {
				level--
				b.WriteString(strings.Repeat("  ", level) + "}\n")
			}
			if !inTry && level == 1 {
				continue
			}
		case l == "else":
			if level > 1 {
				level--
				b.WriteString(strings.Repeat("  ", level) + "} else {\n")
				level++
			}
		case strings.HasPrefix(l, "if ") && strings.HasSuffix(l, " do"):
			cond := strings.TrimSuffix(strings.TrimPrefix(l, "if "), " do")
			b.WriteString(strings.Repeat("  ", level) + "if " + cond + " {\n")
			level++
		case strings.HasPrefix(l, "for ") && strings.Contains(l, "<-") && strings.HasSuffix(l, " do"):
			rest := strings.TrimSuffix(strings.TrimPrefix(l, "for "), " do")
			parts := strings.SplitN(rest, "<-", 2)
			if len(parts) == 2 {
				v := strings.TrimSpace(parts[0])
				coll := strings.TrimSpace(parts[1])
				b.WriteString(strings.Repeat("  ", level) + "for " + v + " in " + coll + " {\n")
				level++
			}
		case strings.HasPrefix(l, "while ") && strings.HasSuffix(l, " do"):
			cond := strings.TrimSuffix(strings.TrimPrefix(l, "while "), " do")
			b.WriteString(strings.Repeat("  ", level) + "while " + cond + " {\n")
			level++
		case strings.HasPrefix(l, retPrefix):
			val := strings.TrimSuffix(strings.TrimSpace(l[len(retPrefix):]), "}")
			b.WriteString(strings.Repeat("  ", level) + "return " + strings.TrimSpace(val) + "\n")
		case l == "throw :break":
			b.WriteString(strings.Repeat("  ", level) + "break\n")
		case l == "throw :continue":
			b.WriteString(strings.Repeat("  ", level) + "continue\n")
		case strings.HasPrefix(l, "IO.puts(Enum.join(Enum.map([") && strings.HasSuffix(l, "], &to_string(&1)), \" \"))"):
			inner := strings.TrimSuffix(strings.TrimPrefix(l, "IO.puts(Enum.join(Enum.map(["), "], &to_string(&1)), \" \"))")
			args := strings.Split(inner, ",")
			for i, a := range args {
				args[i] = strings.TrimSpace(a)
			}
			b.WriteString(strings.Repeat("  ", level) + "print(" + strings.Join(args, ", ") + ")\n")
		case strings.HasPrefix(l, "IO.puts(") && strings.HasSuffix(l, ")"):
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "IO.puts("), ")")
			b.WriteString(strings.Repeat("  ", level) + "print(" + strings.TrimSpace(expr) + ")\n")
		case strings.Contains(l, "="):
			parts := strings.SplitN(l, "=", 2)
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])
			b.WriteString(strings.Repeat("  ", level) + "let " + left + " = " + right + "\n")
		default:
			b.WriteString(strings.Repeat("  ", level) + l + "\n")
		}
	}
	b.WriteString("}")
	return b.String()
}

func getExSignature(src string, pos Position, ls LanguageServer) ([]string, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return nil, ""
	}
	sig := hoverString(hov)
	return parseExSignature(sig)
}

func parseExSignature(sig string) ([]string, string) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	open := strings.Index(sig, "(")
	close := strings.Index(sig, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := strings.TrimSpace(sig[open+1 : close])
	rest := sig[close+1:]
	ret := ""
	if idx := strings.Index(rest, "::"); idx != -1 {
		ret = mapExType(strings.TrimSpace(rest[idx+2:]))
	} else if idx := strings.Index(rest, "->"); idx != -1 {
		ret = mapExType(strings.TrimSpace(rest[idx+2:]))
	}
	var params []string
	if paramsPart != "" {
		parts := strings.Split(paramsPart, ",")
		for _, p := range parts {
			p = strings.TrimSpace(p)
			if p == "" {
				continue
			}
			if i := strings.Index(p, "::"); i != -1 {
				p = strings.TrimSpace(p[:i])
			}
			fields := strings.Fields(p)
			if len(fields) > 0 {
				params = append(params, fields[0])
			}
		}
	}
	return params, ret
}

func mapExType(t string) string {
	switch t {
	case "integer()":
		return "int"
	case "float()":
		return "float"
	case "binary()":
		return "string"
	case "boolean()":
		return "bool"
	default:
		return t
	}
}
