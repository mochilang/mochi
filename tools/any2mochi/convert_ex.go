package any2mochi

import (
	"fmt"
	"os"
	"strings"

	excode "mochi/compile/x/ex"
)

// ConvertEx converts Elixir source code to Mochi using a small regex based
// parser. The language server is currently disabled by default. Only a very
// small subset of Elixir syntax is supported including simple function
// definitions with `if`, `for` and `while` blocks as well as assignments and
// `IO.puts` statements.
func ConvertEx(src string) ([]byte, error) {
	_ = excode.Ensure()

	funcs := parseExFuncs(src)
	if len(funcs) == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}

	var out strings.Builder
	foundMain := false
	for _, f := range funcs {
		code := convertExParsedFunc(f)
		if code == "" {
			continue
		}
		if f.name == "main" {
			foundMain = true
		}
		out.WriteString(code)
		out.WriteByte('\n')
	}
	if foundMain {
		out.WriteString("main()\n")
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

type exFunc struct {
	name   string
	params []string
	body   []string
}

// parseExFuncs extracts top level function definitions using a simple scanner.
func parseExFuncs(src string) []exFunc {
	lines := strings.Split(src, "\n")
	var funcs []exFunc
	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		if !(strings.HasPrefix(l, "def ") || strings.HasPrefix(l, "defp ")) || !strings.HasSuffix(l, " do") {
			continue
		}
		hdr := strings.TrimSuffix(l, " do")
		if strings.HasPrefix(hdr, "defp ") {
			hdr = strings.TrimPrefix(hdr, "defp ")
		} else if strings.HasPrefix(hdr, "def ") {
			hdr = strings.TrimPrefix(hdr, "def ")
		}
		hdr = strings.TrimSpace(hdr)
		name := hdr
		params := []string{}
		if open := strings.Index(hdr, "("); open != -1 {
			close := strings.LastIndex(hdr, ")")
			if close > open {
				name = strings.TrimSpace(hdr[:open])
				paramPart := hdr[open+1 : close]
				if paramPart != "" {
					for _, p := range strings.Split(paramPart, ",") {
						p = strings.TrimSpace(p)
						if p == "" {
							continue
						}
						if idx := strings.IndexAny(p, " :="); idx != -1 {
							p = strings.TrimSpace(p[:idx])
						}
						params = append(params, p)
					}
				}
			}
		}
		var body []string
		depth := 0
		for j := i + 1; j < len(lines); j++ {
			t := strings.TrimSpace(lines[j])
			if t == "end" {
				if depth == 0 {
					i = j
					break
				}
				depth--
				body = append(body, t)
				continue
			}
			if strings.HasSuffix(t, " do") && (strings.HasPrefix(t, "if ") || strings.HasPrefix(t, "for ") || strings.HasPrefix(t, "while ")) {
				depth++
			}
			body = append(body, t)
		}
		funcs = append(funcs, exFunc{name: name, params: params, body: body})
	}
	return funcs
}

func convertExParsedFunc(f exFunc) string {
	var b strings.Builder
	b.WriteString("fun ")
	b.WriteString(f.name)
	b.WriteByte('(')
	b.WriteString(strings.Join(f.params, ", "))
	b.WriteString(")")
	b.WriteString(": any {\n")

	retPrefix := "throw {:return,"
	level := 1
	inTry := false
	for _, line := range f.body {
		l := strings.TrimSpace(line)
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
			cond := strings.TrimSuffix(strings.TrimPrefix(l, "while"), " do")
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
