package php

import (
	"fmt"
	"os"
	"strings"
)

// Convert parses PHP source code and emits minimal Mochi stubs.
func Convert(src string) ([]byte, error) {
	prog, err := Parse(src)
	if err != nil {
		return nil, formatParseError(src, err)
	}
	var out strings.Builder
	for _, c := range prog.Classes {
		out.WriteString("type ")
		out.WriteString(c.Name)
		if len(c.Fields) == 0 && len(c.Methods) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, f := range c.Fields {
				out.WriteString("  ")
				out.WriteString(f)
				out.WriteString(": any\n")
			}
			for _, m := range c.Methods {
				out.WriteString("  fun ")
				out.WriteString(m.Name)
				out.WriteByte('(')
				for i, p := range m.Params {
					if i > 0 {
						out.WriteString(", ")
					}
					out.WriteString(p)
				}
				out.WriteString(") {}\n")
			}
			out.WriteString("}\n")
		}
	}
	for _, f := range prog.Functions {
		out.WriteString("fun ")
		out.WriteString(f.Name)
		out.WriteByte('(')
		for i, p := range f.Params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteString(") {}\n")
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found")
	}
	return []byte(out.String()), nil
}

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

// ConvertFile reads the php file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	out, err := Convert(string(data))
	if err != nil {
		return nil, formatError(path, string(data), err)
	}
	return out, nil
}

func formatParseError(src string, err error) error {
	if e, ok := err.(*ParseError); ok {
		snippet := arrowSnippet(src, e.Line)
		return fmt.Errorf("parse error at line %d: %s\n%s", e.Line, e.Msg, snippet)
	}
	return err
}

func formatError(path, src string, err error) error {
	if e, ok := err.(*ParseError); ok {
		snippet := arrowSnippet(src, e.Line)
		return fmt.Errorf("%s:%d: %s\n%s", path, e.Line, e.Msg, snippet)
	}
	if err.Error() == "no convertible symbols found" {
		return fmt.Errorf("%s: %s\n%s", path, err.Error(), snippet(src))
	}
	return fmt.Errorf("%s: %v", path, err)
}

func arrowSnippet(src string, line int) string {
	lines := strings.Split(src, "\n")
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 1
	if end > len(lines) {
		end = len(lines)
	}
	var b strings.Builder
	for i := start; i < end; i++ {
		prefix := "    "
		if i == line-1 {
			prefix = ">>> "
		}
		fmt.Fprintf(&b, "%d:%s%s\n", i+1, prefix, strings.TrimSpace(lines[i]))
	}
	return b.String()
}
