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
	Line   int
	Column int
	Msg    string
	Snip   string
}

func (e *ConvertError) Error() string {
	if e.Line > 0 {
		if e.Column > 0 {
			return fmt.Sprintf("line %d:%d: %s\n%s", e.Line, e.Column, e.Msg, e.Snip)
		}
		return fmt.Sprintf("line %d: %s\n%s", e.Line, e.Msg, e.Snip)
	}
	return fmt.Sprintf("%s\n%s", e.Msg, e.Snip)
}

func Convert(src string) ([]byte, error) {
	ast, err := parseAST(src)
	if err != nil {
		return nil, formatParseError(src, err)
	}
	var out strings.Builder
	if ast.Module != "" {
		out.WriteString("package ")
		out.WriteString(ast.Module)
		out.WriteString("\n\n")
	}
	for _, r := range ast.Records {
		out.WriteString("// line ")
		out.WriteString(fmt.Sprint(r.Line))
		if r.EndLine > 0 && r.EndLine != r.Line {
			out.WriteString("-")
			out.WriteString(fmt.Sprint(r.EndLine))
		}
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
	for _, f := range ast.Functions {
		if f.Name == "main" {
			hasMain = true
		}
		out.WriteString("// line ")
		out.WriteString(fmt.Sprint(f.Line))
		if f.EndLine > 0 && f.EndLine != f.Line {
			out.WriteString("-")
			out.WriteString(fmt.Sprint(f.EndLine))
		}
		if f.Exported {
			out.WriteString(" (exported)")
		}
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
				parts := strings.Split(line, "\n")
				for _, ln := range parts {
					if strings.HasPrefix(ln, "io:format(") {
						ln = "print(" + strings.TrimPrefix(ln, "io:format(")
					} else if strings.HasPrefix(ln, "io:fwrite(") {
						ln = "print(" + strings.TrimPrefix(ln, "io:fwrite(")
					}
					for _, r := range ast.Records {
						t := strings.Title(r.Name)
						if strings.Contains(ln, "#"+r.Name+"{") {
							ln = strings.ReplaceAll(ln, "#"+r.Name+"{", t+" {")
							if i := strings.Index(ln, t+" {"); i != -1 {
								after := ln[i+len(t)+2:]
								after = strings.ReplaceAll(after, "=", ":")
								ln = ln[:i+len(t)+2] + after
							}
						}
						ln = strings.ReplaceAll(ln, "#"+r.Name+".", ".")
					}
					out.WriteString("  ")
					out.WriteString(strings.TrimSpace(ln))
					out.WriteByte('\n')
				}
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
		end := line + 2
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
			if i+1 == line {
				b.WriteString("    ^\n")
			}
		}
		return &ConvertError{Line: line, Column: 1, Msg: msg, Snip: strings.TrimRight(b.String(), "\n")}
	}
	return &ConvertError{Msg: msg, Snip: snippet(src)}
}
