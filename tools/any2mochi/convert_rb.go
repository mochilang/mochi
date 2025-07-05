package any2mochi

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// rbIndent returns a string of two-space indents for the given level.
func rbIndent(level int) string { return strings.Repeat("  ", level) }

// ConvertRb converts Ruby source code to Mochi. The source is first parsed using
// the configured Ruby language server to ensure it is valid. After successful
// parsing, a very small set of Ruby constructs are translated directly in Go.
func ConvertRb(src string) ([]byte, error) {
	ls := Servers["rb"]
	_, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	return translateRb(src), nil
}

// ConvertRbFile reads the Ruby file at path and converts it to Mochi.
func ConvertRbFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertRb(string(data))
}

func translateRb(src string) []byte {
	var out strings.Builder
	sc := bufio.NewScanner(strings.NewReader(src))
	level := 0
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" {
			continue
		}

		switch {
		case strings.HasPrefix(line, "puts(") && strings.HasSuffix(line, ")"):
			arg := strings.TrimSuffix(strings.TrimPrefix(line, "puts("), ")")
			if strings.HasPrefix(arg, "[") && strings.HasSuffix(arg, "].join(\" \"))") {
				arg = strings.TrimSuffix(strings.TrimPrefix(arg, "["), "].join(\" \"))")
			}
			out.WriteString(rbIndent(level))
			out.WriteString("print(")
			out.WriteString(arg)
			out.WriteString(")\n")
		case strings.HasPrefix(line, "for ") && strings.Contains(line, " in "):
			parts := strings.SplitN(strings.TrimPrefix(line, "for "), " in ", 2)
			if len(parts) == 2 {
				out.WriteString(rbIndent(level))
				out.WriteString("for ")
				out.WriteString(strings.TrimSpace(parts[0]))
				out.WriteString(" in ")
				out.WriteString(strings.TrimSpace(parts[1]))
				out.WriteString(" {\n")
				level++
			}
		case strings.HasPrefix(line, "while "):
			cond := strings.TrimSpace(strings.TrimPrefix(line, "while "))
			out.WriteString(rbIndent(level))
			out.WriteString("while ")
			out.WriteString(cond)
			out.WriteString(" {\n")
			level++
		case line == "end":
			if level > 0 {
				level--
				out.WriteString(rbIndent(level))
				out.WriteString("}\n")
			}
		case strings.HasPrefix(line, "def "):
			rest := strings.TrimSpace(strings.TrimPrefix(line, "def "))
			name := rest
			params := ""
			if open := strings.Index(rest, "("); open != -1 {
				if close := strings.LastIndex(rest, ")"); close > open {
					name = strings.TrimSpace(rest[:open])
					params = strings.TrimSpace(rest[open+1 : close])
				}
			}
			out.WriteString(rbIndent(level))
			out.WriteString("fun ")
			out.WriteString(name)
			out.WriteString("(")
			out.WriteString(params)
			out.WriteString(") {\n")
			level++
		case strings.HasPrefix(line, "return "):
			out.WriteString(rbIndent(level))
			out.WriteString(line)
			out.WriteByte('\n')
		case strings.Contains(line, "="):
			parts := strings.SplitN(line, "=", 2)
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])
			out.WriteString(rbIndent(level))
			out.WriteString("let ")
			out.WriteString(left)
			out.WriteString(" = ")
			out.WriteString(right)
			out.WriteByte('\n')
		}
	}
	if out.Len() == 0 {
		return nil
	}
	return []byte(out.String())
}
