package erlang

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func snippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

// Convert converts erlang source code to Mochi using the language server.
type ConvertError struct {
	Line int
	Msg  string
	Snip string
}

func (e *ConvertError) Error() string {
	if e.Line > 0 {
		return fmt.Sprintf("line %d: %s\n%s", e.Line, e.Msg, e.Snip)
	}
	return fmt.Sprintf("%s\n%s", e.Msg, e.Snip)
}

func Convert(src string) ([]byte, error) {
	funcs, recs, err := parseAST(src)
	if err != nil {
		return nil, formatParseError(src, err)
	}
	var out strings.Builder
	for _, r := range recs {
		out.WriteString("// line ")
		out.WriteString(fmt.Sprint(r.Line))
		out.WriteByte('\n')
		out.WriteString("type ")
		out.WriteString(strings.Title(r.Name))
		out.WriteString(" {\n")
		for _, f := range r.Fields {
			out.WriteString("  ")
			out.WriteString(f)
			out.WriteString(": any\n")
		}
		out.WriteString("}\n")
	}
	hasMain := false
	for _, f := range funcs {
		if f.Name == "main" {
			hasMain = true
		}
		out.WriteString("// line ")
		out.WriteString(fmt.Sprint(f.Line))
		out.WriteByte('\n')
		out.WriteString("fun ")
		if f.Name == "" {
			out.WriteString("fun")
		} else {
			out.WriteString(f.Name)
		}
		out.WriteByte('(')
		for i, p := range f.Params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteByte(')')
		if len(f.Body) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, line := range f.Body {
				if strings.HasPrefix(line, "io:format(") {
					line = "print(" + strings.TrimPrefix(line, "io:format(")
				} else if strings.HasPrefix(line, "io:fwrite(") {
					line = "print(" + strings.TrimPrefix(line, "io:fwrite(")
				}
				out.WriteString("  ")
				out.WriteString(line)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		}
	}
	if hasMain {
		out.WriteString("main()\n")
	}
	if out.Len() == 0 {
		return nil, &ConvertError{Msg: "no convertible symbols found", Snip: snippet(src)}
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the erlang file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

func formatParseError(src string, err error) error {
	msg := err.Error()
	line := 0
	if strings.HasPrefix(msg, "parse error: {") {
		parts := strings.SplitN(msg[len("parse error: "):], ",", 2)
		if len(parts) > 0 {
			n, _ := strconv.Atoi(strings.TrimLeft(strings.TrimSpace(parts[0]), "{"))
			line = n
		}
	}
	lines := strings.Split(src, "\n")
	if line > 0 && line <= len(lines) {
		start := line - 2
		if start < 0 {
			start = 0
		}
		end := line
		if end >= len(lines) {
			end = len(lines) - 1
		}
		var b strings.Builder
		for i := start; i <= end; i++ {
			prefix := "   "
			if i+1 == line {
				prefix = ">>>"
			}
			fmt.Fprintf(&b, "%s %d: %s\n", prefix, i+1, lines[i])
		}
		return &ConvertError{Line: line, Msg: msg, Snip: strings.TrimRight(b.String(), "\n")}
	}
	return &ConvertError{Msg: msg, Snip: snippet(src)}
}
